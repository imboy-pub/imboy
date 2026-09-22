-- 迁移 00000141: Enterprise Webhook 投递账本（FULL-03 / plan-full §3.1、§5、§7）。
--
-- 约束（plan-full §2「禁止重建第二组同义表 / 第二套 delivery worker」）：
--   本迁移**不建新表**。企业投递账本 = 既有 bot_delivery / bot_delivery_attempt
--   （迁移 00000092 / 00000104）+ 本迁移追加的 ownership / version /
--   replay / generation / claim 列与守卫。广州期的 outbox、重试表、死信语义、
--   worker（bot_webhook_delivery_worker）全部原样复用。
--
-- 冻结语义（plan-full §5「endpoint snapshot、claim/retry/terminal state」）：
--   * endpoint snapshot 不可变：入箱时快照的 webhook_url / webhook_host /
--     pinned_ip（迁移 104）在行生命周期内**不可被 UPDATE 改写**——配置变更
--     （改 URL / 轮换 / 停用）只影响**后续**入箱的行，在途投递仍发往快照目标。
--     由 fn_ewh_delivery_guard 的 23514 强制（DB 层，非应用层承诺）。
--   * ownership：ewh_owner_organization_id + ewh_owner_application_id 显式落到
--     行上（不再依赖 bot_id 前缀反查推导），复合 FK 指向
--     enterprise_application (organization_id, id)——跨 Org 引用 23503 拒绝。
--   * version：ewh_ledger_version 由守卫**独占写入**（INSERT=1，每次
--     UPDATE=OLD+1），客户端传值一律被覆盖 → 版本号不可能被回退/伪造。
--   * terminal 不可回退：success / dead 为终态，任何状态变更 23514（重放走
--     **新行** + ewh_replay_of 指向原行，见 uq_ewh_delivery_replay_inflight）。
--   * claim：ewh_claimed_at 记录认领时刻（worker 的租约仍是
--     next_retry_at = NOW() + 60s，见 bot_webhook_delivery_repo）；
--     uq_ewh_delivery_replay_inflight 保证「同一原行同时最多一条在途重放」。
--
-- 命名纪律（FULL-02 真库实测教训：同 schema 函数名全局唯一，CREATE OR REPLACE
--   会静默改写另一个域的同名守卫函数体）：本迁移所有函数/触发器/约束/索引
--   一律用 **ewh** 域前缀；并在本文件内做「迁移前后既有 fn_% 函数体 md5
--   逐条不变」的自证（见 §0 与文末 §6 的 DO 块），使 clobber 在迁移内即 abort。
--
-- 迁移契约：up=可重复执行（IF NOT EXISTS / DROP TRIGGER IF EXISTS + CREATE /
--   CREATE OR REPLACE / pg_constraint 存在性判定）；down=对称回滚（只 DROP
--   本迁移新增的列/约束/索引/函数/触发器，不动 bot_delivery 既有列与 bot 域行为）。
--   禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹（epgsql:squery 多语句）。
--
-- 升级兼容（plan-full §5「先验证 Guangzhou 数据升级兼容」）：既有企业行（GZ 期写入、
--   无 owner 列）在本迁移内用 expand -> backfill 补齐 owner（只填 NULL，不覆盖
--   任何既有值）；owner 的**非空强制**放在 INSERT 守卫里（新行 fail-closed），
--   不放表级 CHECK——否则含历史 NULL 的库无法升级。
--
-- 边界：本迁移不新建 / 不重命名 / 不删除任何投递行；不触碰 bot 域行的既有语义
--   （守卫对非 eapp 行首条语句即放行，见 §5）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- 0) 自证前置：快照本 schema 既有 fn_% 函数体（排除本域 fn_ewh_%）
-- ============================================================
DROP TABLE IF EXISTS ewh_preexisting_fn;
CREATE TEMP TABLE ewh_preexisting_fn AS
SELECT
    p.proname AS name,
    md5(p.prosrc) AS body_md5
FROM pg_proc p
JOIN pg_namespace n ON n.oid = p.pronamespace
WHERE
    n.nspname = 'public'
    AND p.proname LIKE 'fn\_%'
    AND p.proname NOT LIKE 'fn\_ewh\_%';

-- ============================================================
-- 1) bot：端点配置代际计数器（企业 App 的 endpoint 配置版本）
-- ============================================================
-- 非企业 bot 行保持 0（bot 域读写路径完全不感知该列）。
ALTER TABLE bot
    ADD COLUMN IF NOT EXISTS ewh_endpoint_generation integer NOT NULL DEFAULT 0;

-- ============================================================
-- 2) bot_delivery：企业投递账本列（ownership / version / replay / generation）
-- ============================================================
ALTER TABLE bot_delivery
    ADD COLUMN IF NOT EXISTS ewh_owner_organization_id bigint,
    ADD COLUMN IF NOT EXISTS ewh_owner_application_id bigint,
    ADD COLUMN IF NOT EXISTS ewh_replay_of text,
    ADD COLUMN IF NOT EXISTS ewh_endpoint_generation integer NOT NULL DEFAULT 0,
    ADD COLUMN IF NOT EXISTS ewh_ledger_version integer NOT NULL DEFAULT 1,
    ADD COLUMN IF NOT EXISTS ewh_claimed_at timestamptz;

COMMENT ON COLUMN bot_delivery.ewh_owner_organization_id IS
    '企业投递归属机构（eapp 行非空；bot 域行 NULL）；复合 FK 到 enterprise_application';
COMMENT ON COLUMN bot_delivery.ewh_owner_application_id IS
    '企业投递归属 Application（eapp 行非空；bot 域行 NULL）；与 org 列成对';
COMMENT ON COLUMN bot_delivery.ewh_replay_of IS
    '重放来源：原 delivery_id（非重放行 NULL）；在途重放唯一索引的键';
COMMENT ON COLUMN bot_delivery.ewh_endpoint_generation IS
    '入箱时快照的 bot.ewh_endpoint_generation（端点配置代际；只读审计，不可改）';
COMMENT ON COLUMN bot_delivery.ewh_ledger_version IS
    '账本版本（守卫独占写入：INSERT=1，UPDATE=OLD+1；客户端传值被覆盖）';
COMMENT ON COLUMN bot_delivery.ewh_claimed_at IS
    '最近一次 worker 认领时刻（租约见 next_retry_at；bot 域行保持 NULL）';
COMMENT ON COLUMN bot.ewh_endpoint_generation IS
    '企业 Webhook endpoint 配置代际（每次配置写入 +1；bot 域行保持 0）';

-- 归属回填（expand -> backfill）：只填 NULL，不覆盖既有值。
-- 绑定链：bot_delivery.bot_id = 'eapp:<bot.user_id>' -> enterprise_application.principal_user_id。
UPDATE bot_delivery d
SET
    ewh_owner_organization_id = a.organization_id,
    ewh_owner_application_id = a.id
FROM bot b
JOIN enterprise_application a ON a.principal_user_id = b.user_id
WHERE
    d.bot_id = 'eapp:' || b.user_id::text
    AND d.ewh_owner_application_id IS NULL;

-- 幂等加约束（PG 无 ADD CONSTRAINT IF NOT EXISTS；按 conrelid 判定，避免同名约束跨表误判）
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'ck_ewh_delivery_owner_pair' AND conrelid = 'bot_delivery'::regclass
    ) THEN
        ALTER TABLE bot_delivery ADD CONSTRAINT ck_ewh_delivery_owner_pair
            CHECK ((ewh_owner_organization_id IS NULL) = (ewh_owner_application_id IS NULL));
    END IF;

    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'ck_ewh_delivery_ledger_version' AND conrelid = 'bot_delivery'::regclass
    ) THEN
        ALTER TABLE bot_delivery ADD CONSTRAINT ck_ewh_delivery_ledger_version
            CHECK (ewh_ledger_version >= 1);
    END IF;

    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'ck_ewh_delivery_endpoint_generation' AND conrelid = 'bot_delivery'::regclass
    ) THEN
        ALTER TABLE bot_delivery ADD CONSTRAINT ck_ewh_delivery_endpoint_generation
            CHECK (ewh_endpoint_generation >= 0);
    END IF;

    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'ck_ewh_delivery_replay_not_self' AND conrelid = 'bot_delivery'::regclass
    ) THEN
        ALTER TABLE bot_delivery ADD CONSTRAINT ck_ewh_delivery_replay_not_self
            CHECK (ewh_replay_of IS NULL OR ewh_replay_of <> delivery_id);
    END IF;

    -- ownership 复合 FK：跨 Org 引用 23503；NULL 行（bot 域）不受约束（MATCH SIMPLE）。
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'fk_ewh_delivery_owner' AND conrelid = 'bot_delivery'::regclass
    ) THEN
        ALTER TABLE bot_delivery ADD CONSTRAINT fk_ewh_delivery_owner
            FOREIGN KEY (ewh_owner_organization_id, ewh_owner_application_id)
            REFERENCES enterprise_application (organization_id, id) ON DELETE RESTRICT;
    END IF;
END $$;

-- 归属读面 + 死信/成功率统计索引（企业行限定；bot 域行不入索引）
CREATE INDEX IF NOT EXISTS bot_delivery_ewh_owner_idx
    ON bot_delivery (ewh_owner_application_id, status, created_at DESC)
    WHERE ewh_owner_application_id IS NOT NULL;

-- 在途重放唯一：同一原行同时最多一条 pending/retry 重放（并发重放仲裁，23505）
CREATE UNIQUE INDEX IF NOT EXISTS uq_ewh_delivery_replay_inflight
    ON bot_delivery (ewh_replay_of)
    WHERE ewh_replay_of IS NOT NULL AND status IN ('pending', 'retry');

-- ============================================================
-- 3) 账本守卫：快照不可变 / 终态不可回退 / 版本单调 / 归属必填
-- ============================================================
-- 管辖范围**仅 eapp 行**（企业投递）；非 eapp 行首条语句即 RETURN NEW，
-- bot 域行为零变化（含 bot 管理面 dead -> pending 手工重放）。
CREATE OR REPLACE FUNCTION fn_ewh_delivery_guard() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
    allowed text[];
BEGIN
    IF TG_OP = 'INSERT' THEN
        IF NEW.bot_id NOT LIKE 'eapp:%' THEN
            RETURN NEW;
        END IF;
        IF NEW.status <> 'pending' THEN
            RAISE EXCEPTION
                'ewh_delivery_insert_status: enterprise delivery must be born pending (got %)',
                NEW.status
                USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
        END IF;
        IF NEW.ewh_owner_organization_id IS NULL OR NEW.ewh_owner_application_id IS NULL THEN
            RAISE EXCEPTION
                'ewh_delivery_insert_owner_required: enterprise delivery needs org+application owner'
                USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
        END IF;
        -- owner <-> bot_id <-> principal 三元一致：投递行只能属于「其 bot_id 所指
        -- 可信 principal 自己的 Application」——防止一行被写成「用 A 的密钥签发给
        -- B 的端点」（跨 Application 凭证/端点串号，DB 层 fail-closed）。
        IF NOT EXISTS (
            SELECT 1
            FROM enterprise_application a
            JOIN bot b ON b.user_id = a.principal_user_id
            WHERE
                a.organization_id = NEW.ewh_owner_organization_id
                AND a.id = NEW.ewh_owner_application_id
                AND NEW.bot_id = 'eapp:' || b.user_id::text
        ) THEN
            RAISE EXCEPTION
                'ewh_delivery_owner_principal_mismatch: bot_id / owner application / principal disagree'
                USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
        END IF;
        IF NEW.ewh_replay_of IS NOT NULL AND NEW.ewh_replay_of = NEW.delivery_id THEN
            RAISE EXCEPTION 'ewh_delivery_replay_not_self' USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
        END IF;
        -- 版本号由守卫独占写入：客户端传值一律被覆盖。
        NEW.ewh_ledger_version := 1;
        NEW.ewh_claimed_at := NULL;
        RETURN NEW;
    END IF;

    -- UPDATE：非企业行（含 OLD 与 NEW 两侧）一律放行
    IF NEW.bot_id NOT LIKE 'eapp:%' AND OLD.bot_id NOT LIKE 'eapp:%' THEN
        RETURN NEW;
    END IF;

    -- 归属与端点快照不可变（配置变更不得改写在途投递的目标/归属/载荷）
    IF NEW.delivery_id <> OLD.delivery_id
        OR NEW.bot_id <> OLD.bot_id
        OR NEW.event_type <> OLD.event_type
        OR NEW.payload::text <> OLD.payload::text
        OR NEW.idempotency_key <> OLD.idempotency_key
        OR NEW.correlation_id <> OLD.correlation_id
        OR NEW.webhook_url <> OLD.webhook_url
        OR NEW.webhook_host <> OLD.webhook_host
        OR NEW.pinned_ip <> OLD.pinned_ip
        OR NEW.created_at <> OLD.created_at
        OR NEW.ewh_owner_organization_id IS DISTINCT FROM OLD.ewh_owner_organization_id
        OR NEW.ewh_owner_application_id IS DISTINCT FROM OLD.ewh_owner_application_id
        OR NEW.ewh_replay_of IS DISTINCT FROM OLD.ewh_replay_of
        OR NEW.ewh_endpoint_generation <> OLD.ewh_endpoint_generation
    THEN
        RAISE EXCEPTION
            'ewh_delivery_snapshot_immutable: endpoint snapshot / ownership / payload is frozen'
            USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
    END IF;

    -- 终态不可回退 + 合法状态图（pending/retry -> pending|success|retry|dead；
    -- success -> success；dead -> dead）
    allowed := CASE OLD.status
        WHEN 'pending' THEN ARRAY['pending', 'success', 'retry', 'dead']
        WHEN 'retry' THEN ARRAY['pending', 'success', 'retry', 'dead']
        WHEN 'success' THEN ARRAY['success']
        WHEN 'dead' THEN ARRAY['dead']
        ELSE ARRAY[OLD.status]
    END;
    IF NOT (NEW.status = ANY (allowed)) THEN
        RAISE EXCEPTION
            'ewh_delivery_terminal_immutable: illegal transition % -> %', OLD.status, NEW.status
            USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
    END IF;

    -- 终态行为冻结记录：状态、尝试次数、下次重试时间都不得再变
    IF OLD.status IN ('success', 'dead')
        AND (
            NEW.attempt_count <> OLD.attempt_count
            OR NEW.next_retry_at <> OLD.next_retry_at
        )
    THEN
        RAISE EXCEPTION
            'ewh_delivery_terminal_frozen: terminal row is a frozen record'
            USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
    END IF;

    IF NEW.attempt_count < OLD.attempt_count THEN
        RAISE EXCEPTION
            'ewh_delivery_attempt_monotonic: attempt_count must not decrease (% -> %)',
            OLD.attempt_count, NEW.attempt_count
            USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
    END IF;

    NEW.ewh_ledger_version := OLD.ewh_ledger_version + 1;
    RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_ewh_delivery_guard ON bot_delivery;
CREATE TRIGGER trg_ewh_delivery_guard
    BEFORE INSERT OR UPDATE ON bot_delivery
    FOR EACH ROW EXECUTE FUNCTION fn_ewh_delivery_guard();

-- ============================================================
-- 4) 自证后置：既有 fn_% 函数体 md5 逐条不变 + 本域函数名集合封闭
-- ============================================================
DO $$
DECLARE
    offending text;
BEGIN
    IF to_regclass('pg_temp.ewh_preexisting_fn') IS NULL THEN
        RAISE EXCEPTION 'ewh141: self-proof snapshot table missing' USING ERRCODE = '23514',
            CONSTRAINT = 'ewh141_selfproof';
    END IF;

    -- 4.1 既有函数不得消失
    SELECT string_agg(x.name, ',' ORDER BY x.name) INTO offending
    FROM (
        SELECT pre.name
        FROM ewh_preexisting_fn pre
        WHERE NOT EXISTS (
            SELECT 1 FROM pg_proc p
            JOIN pg_namespace n ON n.oid = p.pronamespace
            WHERE n.nspname = 'public' AND p.proname = pre.name
        )
    ) x;
    IF offending IS NOT NULL THEN
        RAISE EXCEPTION 'ewh141: pre-existing fn_%% dropped: %', offending
            USING ERRCODE = '23514',
            CONSTRAINT = 'ewh141_selfproof';
    END IF;

    -- 4.2 既有函数体不得被改写（md5 逐条比对）
    SELECT string_agg(x.name, ',' ORDER BY x.name) INTO offending
    FROM (
        SELECT pre.name
        FROM ewh_preexisting_fn pre
        JOIN pg_proc p ON p.proname = pre.name
        JOIN pg_namespace n ON n.oid = p.pronamespace
        WHERE
            n.nspname = 'public'
            AND md5(p.prosrc) <> pre.body_md5
    ) x;
    IF offending IS NOT NULL THEN
        RAISE EXCEPTION 'ewh141: pre-existing fn_%% body rewritten: %', offending
            USING ERRCODE = '23514',
            CONSTRAINT = 'ewh141_selfproof';
    END IF;

    -- 4.3 本域函数名集合封闭（只允许本迁移声明的 fn_ewh_*）
    SELECT string_agg(p.proname, ',' ORDER BY p.proname) INTO offending
    FROM pg_proc p
    JOIN pg_namespace n ON n.oid = p.pronamespace
    WHERE
        n.nspname = 'public'
        AND p.proname LIKE 'fn\_ewh\_%'
        AND p.proname <> 'fn_ewh_delivery_guard';
    IF offending IS NOT NULL THEN
        RAISE EXCEPTION 'ewh141: unexpected fn_ewh_* function(s): %', offending
            USING ERRCODE = '23514',
            CONSTRAINT = 'ewh141_selfproof';
    END IF;
END $$;

DROP TABLE IF EXISTS ewh_preexisting_fn;
