-- 00000215_talk_record_cleanup_t20.up.sql
-- 面聊记录 T20：删除/留存/清理闭环支撑索引。
-- 契约：BACKEND-API.md §10.3（删除行映射）/§10.4（治理与删除状态拆分）/
--       §11.3（删除后 owner 白名单）
-- 迁移契约：禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹；down 成对
--       可逆（纯索引，无数据依赖）。
--
-- 设计决策：
--   * i_tr_cleanup_pending —— 软删后未完成清理的行索引（pending/failed）：
--     purge 重试入口（cleanup 运维函数）与删除状态扫描的支撑；完成态
--     （complete/blocked_*/retained_*）不进索引——重试只关心未完成且
--     可推进的行。
--   * i_trau_action —— 删除/清理审计按动作检索（action='delete'/
--     'purge_retry'）的支撑；治理元数据不随业务删除（§10.4），审计行
--     只增不删。
SET LOCAL lock_timeout = '5s';

CREATE INDEX IF NOT EXISTS i_tr_cleanup_pending
    ON talk_record (cleanup_status, deleted_at)
    WHERE deleted_at IS NOT NULL
      AND cleanup_status IN ('pending', 'failed');

COMMENT ON INDEX i_tr_cleanup_pending IS
    '删除后未完成清理的行（pending/failed）：purge 协调/重试与删除状态扫描支撑';

CREATE INDEX IF NOT EXISTS i_trau_action
    ON talk_record_audit (action, created_at DESC);

COMMENT ON INDEX i_trau_action IS
    '删除/清理/授权等审计按动作检索支撑（治理元数据不随业务删除）';
