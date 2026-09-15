-- 迁移 00000121: 保留 Hold 的「作用域引用」只对 active 行成立（EB-03R §2.4 M1 / D1 修复）。
-- 计划契约：EB-03R M1-a..M1-e（released hold 不得永久阻断到期 bounded purge）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 缺陷（R0 实测，不是推测）：00000117 的 `fk_erh_message` 是
--   (organization_id, workspace_id, scope_message_id) → enterprise_message(...)
--   `ON DELETE RESTRICT` 的**无条件**复合 FK。hold 行 append-only 不可删、
--   scope_message_id 不可改，因此**已被 release 的 hold 行仍然永久阻断**其
--   作用域消息的物理删除（`enterprise_business_db_it.sh` 的 D1 段实测 23001）。
--
-- 修复方向（Q3 裁决原文：「把『阻断 purge』的条件从『存在引用行』改为
-- 『存在 active 行』」）：把引用列换成**派生列**——
--   active_scope_message_id := CASE WHEN released_at IS NULL THEN scope_message_id END
-- 该列 STORED 生成、随 release 自动变 NULL，因此：
--   * active 行（released_at IS NULL）→ 派生列 = 原 id → FK 生效 →
--     既做**插入期引用完整性校验**，也由 DB 行锁 + RI 触发器**序列化**并发删除；
--   * released 行 → 派生列 = NULL → 复合 FK 按 MATCH SIMPLE 不再校验 →
--     既不删除历史行、也不丢原 resource id，更不再阻断 purge。
--
-- M1-a：不出现 `ON DELETE CASCADE`（本迁移只 DROP 旧 FK、ADD RESTRICT 复合 FK）。
-- M1-b：不出现 `ON DELETE SET NULL`；本迁移**不改** `scope_message_id` 的可空性
--       （该列自 EB-01 起即为 nullable，message 作用域的非空由 ck_erh_scope_shape 强制）。
-- M1-c：released 行保留在表内，`scope_message_id` 逐字不变（派生列不影响该列取值）。
-- M1-d：active hold 的阻断由 DB 层保障——复合 FK 的 KEY SHARE 行锁使并发
--       DELETE 阻塞/跳过，RI 触发器在 READ COMMITTED 下用最新快照复核；
--       不依赖应用层「先查后删」。
-- M1-e：只通过本迁移（00000121）修复，00000114..00000120 逐字节不变。
--
-- 边界声明：这批准的是**本地数据模型语义**，**不构成**真实 Legal Hold、法规或
-- 生产合规结论。真实 hold 的创建/释放属需担责操作。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: 去掉无条件引用（released 行不再阻断）
-- ============================================================
ALTER TABLE enterprise_retention_hold DROP CONSTRAINT IF EXISTS fk_erh_message;

COMMENT ON COLUMN enterprise_retention_hold.scope_message_id IS
    '作用域消息 id（原 resource id，审计可追）：append-only 不可改；非 message 作用域时为 NULL。release 后该列逐字保留（不置 NULL、不随消息删除而丢失）';

-- ============================================================
-- Phase 2: active-only 派生引用列 + 复合 FK（DB 级 active 守卫）
-- ============================================================
ALTER TABLE enterprise_retention_hold
    ADD COLUMN IF NOT EXISTS active_scope_message_id bigint
    GENERATED ALWAYS AS (CASE WHEN released_at IS NULL THEN scope_message_id END) STORED;

COMMENT ON COLUMN enterprise_retention_hold.active_scope_message_id IS
    'STORE 派生列（非调用方可写）：active（released_at IS NULL）时等于 scope_message_id，released 后为 NULL。仅用于把 fk_erh_active_message 的阻断范围收窄到 active 行——released 行因 MATCH SIMPLE 的 NULL 语义不再参与引用校验，从而既不丢历史 id、也不阻断到期 purge';

ALTER TABLE enterprise_retention_hold
    DROP CONSTRAINT IF EXISTS fk_erh_active_message;
ALTER TABLE enterprise_retention_hold
    ADD CONSTRAINT fk_erh_active_message
    FOREIGN KEY (organization_id, workspace_id, active_scope_message_id)
    REFERENCES enterprise_message (organization_id, workspace_id, id)
    ON DELETE RESTRICT;

COMMENT ON CONSTRAINT fk_erh_active_message ON enterprise_retention_hold IS
    'active-only 复合引用：只在 hold 生效中（released_at IS NULL）时把 scope_message_id 绑定到 enterprise_message，提供 (a) 插入期引用完整性 (b) KEY SHARE 行锁 + RI 最新快照复核的并发序列化；release 后派生列为 NULL，引用自动失效（released hold 不再永久阻断 bounded purge）';

-- active_only 索引支撑：purge 候选/复核按 (Org, Workspace, message) 命中 active hold。
CREATE INDEX IF NOT EXISTS i_erh_active_scope_message ON enterprise_retention_hold
    USING btree (organization_id, workspace_id, active_scope_message_id)
    WHERE active_scope_message_id IS NOT NULL;
