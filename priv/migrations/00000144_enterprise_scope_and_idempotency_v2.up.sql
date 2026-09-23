-- 迁移 00000144: enterprise scope 枚举扩到 14 值 + internal 幂等 response 快照
-- （V2.1 / plan §7 Scope Matrix、§11 Idempotency Contract、§16 Schema Impact：
--   确定项恰为「4 scope CHECK」与「幂等 response_body/completion 约束」）。
--
-- 本迁移做的事（全部幂等，可重放）：
--   1) ck_eags_scope_fixed 从 10 值扩到 14 值：新增 4 个只读 scope
--      groups:read / workspaces:read / projects:read / channels:read
--      （INT-18/24..31）。无行回填——枚举扩展只放宽 CHECK，既有行天然满足；
--      生产 Grant 不会因迁移自动获得新授权（§5.2：禁止自动回填权限）。
--      新枚举与 enterprise_internal_scope:all/0 逐字一致（顺序同 plan §7）。
--   2) enterprise_internal_idempotency 加 response_body text（可空）：
--      §11 Replay 要求**字节精确**重放首次响应体。列类型刻意用 text 而非
--      plan §9 字面写的 jsonb：PG jsonb 存的是解析态、输出时按其规范重排
--      （键序/空白均会变，如 '{"v":2}' 回读为 '{"v": 2}'），replay 会漂移，
--      使 §21 IDEM-01「exact status/body replay」不可达成；text 保序字节，
--      JSON 合法性由 ck_eii_completion 内的 `response_body::jsonb IS NOT NULL`
--      谓词强制（非法 JSON 在 CHECK 求值时即报错拒绝）。旧行回填 '{}'，
--      新完成行由应用层在同一事务写入完整 JSON 快照。
--   3) ck_eii_completion：完成态一致性——
--      (response_code IS NULL) = (response_body IS NULL)，且 response_code
--      （非空时）在 100..599、response_body（非空时）是合法 JSON。对应
--      §11 Completion「业务写/audit/response_code/response_body 同一 DB
--      事务提交」的表侧钉子：不存在「有码无体」（重放空体）或
--      「有体无码」（无 status 的体）行。
--
-- 旧行回填说明（不静默丢数据）：
--   既有完成行（response_code 非空）在 136 期没有响应体。它们全部早于
--   本迁移执行时刻 ≥ 24h（幂等 TTL=24h，136 上线远早于本迁移），即均已
--   过期——下一次同 key 请求会走 record_tx 的原子重置分支作为新请求执行，
--   不会重放旧行。回填 '{}' 只为满足完成态配对形态（空 JSON 对象），
--   不伪造任何业务语义。
--
-- 过期原子重置（§11 TTL）不需要 DDL：由应用层 repo 在同一事务内
--   INSERT DO NOTHING → SELECT FOR UPDATE → 条件 UPDATE 全量重置实现
--   （src/repo/enterprise_internal_idempotency_repo.erl record_tx/7）。
--   i_eii_expires 索引已存在（136），清理任务只是运维兜底，正确性不依赖。
--   故此处刻意不造触发器——把重置放进触发器会让「读判定」与「写重置」
--   的边界漂移到 DB 层，且无法与业务事务的 BEGIN/ROLLBACK 边界对齐。
--
-- 命名纪律（FULL-02 实证）：不新建函数/触发器，只改列/约束/注释；
--   对象名 ck_eags_scope_fixed / response_body / ck_eii_completion 均属
--   本表域。所有变更用 DROP-IF-EXISTS-再建 / IF NOT EXISTS 保证幂等。
--
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- 1) Grant scope 固定枚举：10 -> 14（无行回填）
-- ============================================================
ALTER TABLE enterprise_application_grant_scope
    DROP CONSTRAINT IF EXISTS ck_eags_scope_fixed;

ALTER TABLE enterprise_application_grant_scope
    ADD CONSTRAINT ck_eags_scope_fixed CHECK (scope = ANY (ARRAY[
        'application:read'::text,
        'identities:read'::text,
        'identities:write'::text,
        'groups:read'::text,
        'groups:write'::text,
        'workspaces:read'::text,
        'projects:read'::text,
        'channels:read'::text,
        'files:write'::text,
        'messages:send'::text,
        'messages:send_as_human'::text,
        'friend_requests:create'::text,
        'webhooks:manage'::text,
        'sso:exchange'::text
    ]));

COMMENT ON CONSTRAINT ck_eags_scope_fixed ON enterprise_application_grant_scope IS
    'Grant 固定 scope 枚举（V2.1 §7 冻结 14 值；与 enterprise_internal_scope:all/0 逐字一致，无 wildcard）';

-- ============================================================
-- 2) 幂等 response 快照：response_body text（可空，兼容旧行；字节保序，
--    类型选择理由见文件头——jsonb 重排会破坏 §11 字节精确重放）
-- ============================================================
ALTER TABLE enterprise_internal_idempotency
    ADD COLUMN IF NOT EXISTS response_body text;

-- 旧行回填：仅完成行（response_code 非空）需要——它们均已过 24h TTL，
-- 原子重置分支会全量刷新；'{}' 只满足配对形态（见文件头说明）。
UPDATE enterprise_internal_idempotency
SET response_body = '{}'
WHERE response_code IS NOT NULL
  AND response_body IS NULL;

-- ============================================================
-- 3) 完成态一致性 CHECK（§11 Completion 的表侧钉子）
--    JSON 合法性经 `response_body::jsonb IS NOT NULL` 谓词强制
--    （非法 JSON 在 CHECK 求值时抛 22P02，写入即失败——fail-closed）。
-- ============================================================
ALTER TABLE enterprise_internal_idempotency
    DROP CONSTRAINT IF EXISTS ck_eii_completion;

ALTER TABLE enterprise_internal_idempotency
    ADD CONSTRAINT ck_eii_completion CHECK (
        (response_code IS NULL) = (response_body IS NULL)
        AND (response_code IS NULL OR (response_code >= 100 AND response_code <= 599))
        AND (response_body IS NULL OR response_body::jsonb IS NOT NULL)
    );

COMMENT ON COLUMN enterprise_internal_idempotency.response_body IS
    '首次响应完整 JSON 文本快照（V2.1 §11：replay 字节精确重放 status+body；text 保序，与 response_code 同事务回填、同空/同非空）';
COMMENT ON COLUMN enterprise_internal_idempotency.response_code IS
    '已存储响应码（claim 前可空；100..599，与 response_body 成对出现）';
COMMENT ON TABLE enterprise_internal_idempotency IS
    '/api/internal/v1 幂等记录（V2.1 §11：同 key 同 payload TTL 内精确重放 status+body+Idempotent-Replayed 头；同 key 异 payload 409 idempotency_conflict；过期行同事务原子重置）';
