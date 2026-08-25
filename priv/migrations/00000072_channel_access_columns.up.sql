-- 00000072_channel_access_columns.up.sql
-- Step 10：频道正交访问模型 expand/backfill/cleanup 迁移
-- 设计契约：docs/architecture/adr-channel-access-and-payment-2026-08.md §8.2 / §8.2.1（2026-08-25 修订）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹，
--           文件内 COMMIT 会提前提交外层事务，其后的失败将无法回滚前置 DDL。
--
-- 设计决策：删除 legacy type 列，正交三字段为唯一权威源。
--   旧 type 列在回填后删除，不再保留兼容桥架。
--   Step 11 应用代码必须使用 visibility/access_type/join_policy 显式写入。
--   无触发器：不存在双向投影，单源事实 = 零歧义。

-- ============================================================
-- Phase 0: type=2 盘点强制门
-- ============================================================
-- 存量 type=2（历史付费频道）→ C3(public/paid/purchase) 的回填基于「假设均公开」。
-- ADR §8.5 铁律：禁止猜测覆盖，须逐条人工盘点。本门把人工盘点从流程约束升级为 DB 强制约束：
--   存在 type=2 存量行 且 无审计确认行 → RAISE EXCEPTION，整个迁移回滚；
--   空库 / 无 type=2 行 → 自动放行（新部署无需盘点）。
-- 运维流程：在 psql 中执行 SELECT id, name, type FROM channel WHERE type=2 逐条核对可见性
--           （人工 dry-run），确认后 INSERT INTO channel_access_type2_audit(type2_count, approved_by)
--           VALUES(N, '<operator>') 写入审计行，再跑本迁移。
CREATE TABLE IF NOT EXISTS channel_access_type2_audit (
    audited_at  timestamptz NOT NULL DEFAULT now(),
    type2_count integer     NOT NULL,
    approved_by text        NOT NULL
);

DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM channel WHERE type = 2)
       AND NOT EXISTS (SELECT 1 FROM channel_access_type2_audit) THEN
        RAISE EXCEPTION '00000072 中止：存在 % 个 type=2 历史付费频道未盘点确认。'
            '请先执行 SELECT id, name, type FROM channel WHERE type=2 逐条核对可见性，'
            '确认后 INSERT INTO channel_access_type2_audit(type2_count, approved_by)'
            ' VALUES(<盘点数量>, ''<操作人>'') 写入审计行，再重跑本迁移。'
            '禁止跳过盘点直接回填（ADR §8.5 不猜测覆盖）。',
            (SELECT count(*) FROM channel WHERE type = 2);
    END IF;
END $$;

-- ============================================================
-- Phase 0.5: 非法 type abort 守卫（ADR §8.2.1 验收 B）
-- ============================================================
-- channel.type 在建表迁移 00000003 中无 CHECK，可为任意 smallint。
-- 回填仅命中 type IN (0,1,2)；非法行若放行将被隐式保留 DEFAULT (0,0,0)=C1 公开免费，
-- 且 type 列即将被删除，非法值将永久丢失。此处显式 abort，事务回滚含已执行 DDL。
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM channel WHERE type NOT IN (0, 1, 2)) THEN
        RAISE EXCEPTION '00000072 中止：发现非法 channel.type 值，需人工清理后重跑: %',
            (SELECT string_agg(type::text || ':' || cnt::text, ', ')
               FROM (SELECT type, count(*) AS cnt FROM channel
                      WHERE type NOT IN (0, 1, 2) GROUP BY type) t);
    END IF;
END $$;

-- ============================================================
-- Phase 1: expand — 新增三个正交字段
-- ============================================================
-- PG fast add column（常量默认值，元数据级，不重写表、不持长锁）。
ALTER TABLE channel ADD COLUMN IF NOT EXISTS visibility  smallint NOT NULL DEFAULT 0
    CHECK (visibility = ANY (ARRAY[0, 1]));
ALTER TABLE channel ADD COLUMN IF NOT EXISTS access_type smallint NOT NULL DEFAULT 0
    CHECK (access_type = ANY (ARRAY[0, 1]));
ALTER TABLE channel ADD COLUMN IF NOT EXISTS join_policy smallint NOT NULL DEFAULT 0
    CHECK (join_policy = ANY (ARRAY[0, 1, 2, 3]));

COMMENT ON COLUMN channel.visibility  IS '0 public / 1 private（谁能发现）';
COMMENT ON COLUMN channel.access_type IS '0 free / 1 paid（是否付费）';
COMMENT ON COLUMN channel.join_policy IS '0 open / 1 invite / 2 approval(fail-closed 未实现) / 3 purchase';

-- 纵深防御：DB 层禁止最危险矛盾组合「免费+购买」（ADR §8.3.1 可选防线）；
-- 其余矛盾/未定义组合由应用层 channel_access_policy（Step 11）创建时拒绝。
ALTER TABLE channel DROP CONSTRAINT IF EXISTS chk_channel_no_free_purchase;
ALTER TABLE channel ADD CONSTRAINT chk_channel_no_free_purchase
    CHECK (NOT (access_type = 0 AND join_policy = 3));

-- Step 11 discovery SQL 将按 visibility=0 过滤（ADR §8.4），提前建部分索引避免 seq scan。
CREATE INDEX IF NOT EXISTS i_channel_visibility_active
    ON channel (visibility) WHERE status = 1;

-- ============================================================
-- Phase 2: backfill — type → 新字段（幂等：仅命中仍为默认态的行）
-- ============================================================
-- 幂等守卫（AND 三字段 = 0）：重复执行不覆盖已漂移/已人工修正的数据（ADR §8.2.1）。
UPDATE channel SET visibility = 0, access_type = 0, join_policy = 0
 WHERE type = 0 AND visibility = 0 AND access_type = 0 AND join_policy = 0;

UPDATE channel SET visibility = 1, access_type = 0, join_policy = 1
 WHERE type = 1 AND visibility = 0 AND access_type = 0 AND join_policy = 0;

UPDATE channel SET visibility = 0, access_type = 1, join_policy = 3
 WHERE type = 2 AND visibility = 0 AND access_type = 0 AND join_policy = 0;

-- ============================================================
-- Phase 3: cleanup — 删除 legacy type 列
-- ============================================================
-- type 列不再是权威源。应用层（Step 11）完全使用三正交字段，不再读写 type。
-- API 响应中需要 type 字段的，由应用层 compute_type() 从三字段计算。
-- 无触发器：不需要双向投影，单源事实 = 零歧义。
ALTER TABLE channel DROP COLUMN IF EXISTS type;