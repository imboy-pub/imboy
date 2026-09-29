-- 迁移 00000154_customer_service_seat_console_list_index.up.sql: 坐席控制台
-- 列表索引与实际查询对齐（REVIEW-3 F-5）。
-- 计划契约：seat-console-embed round3 车道 r3-f5（索引-查询对齐新迁移）。
-- 迁移契约：up=可重复执行（IF EXISTS / IF NOT EXISTS 幂等），down=对称回滚。
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 背景（REVIEW-3 F-5，P3 正确性/一致性）：
--   SQL_LIST_CONSOLES_PAGE（cs_pg_seat_console.erl）按
--     WHERE organization_id = $1 AND workspace_id = $2
--       AND ($3::bigint = 0 OR id < $3)
--     ORDER BY id DESC LIMIT $4
--   做游标分页，无 status 谓词；153 遗留的 i_cssc_org_ws_status
--   (organization_id, workspace_id, status) 第三列无用，且不提供 id 排序。
--   本迁移替换为 i_cssc_org_ws_id (organization_id, workspace_id, id DESC)，
--   前缀过滤 + 尾列直接供 ORDER BY id DESC 的反向扫描（无 Sort 节点）。
--
-- 惯例依据：
--   * 尾列 DESC 的 keyset 索引是仓内既有口径（145 i_ws_org_created、
--     150 bot_delivery_ewh_keyset_idx 均为 (..., id DESC) 形态）；
--   * 不用 CREATE INDEX CONCURRENTLY——erlang_migrate 外层单事务包裹
--     （145 同款注释：140/141 均为事务内 CREATE INDEX）；
--   * 本表行数极小，属正确性/一致性修正而非性能急救。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP INDEX IF EXISTS i_cssc_org_ws_status;

CREATE INDEX IF NOT EXISTS i_cssc_org_ws_id ON customer_service_seat_console
    USING btree (organization_id, workspace_id, id DESC);

COMMENT ON INDEX i_cssc_org_ws_id IS
    '坐席控制台列表 keyset 访问路径（REVIEW-3 F-5 对齐 SQL_LIST_CONSOLES_PAGE）：
    (org, ws) 前缀过滤 + id DESC 尾列供游标分页反向扫描，无 status 谓词';
