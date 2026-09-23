-- 迁移 00000145: enterprise internal 只读面三个热点索引
-- （V2.1 F2 索引裁决 / B2-EXPLAIN 证据：index-decisions.json，verdict=ADD_INDEX
--   共 3 条；A0 已分配号 145）。
--
-- 证据来源（scratch 库 imboy_eov21_929b7d41，scaled fixture）：
--   EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) before/after 候选索引对比，
--   全堆路径（seq/bitmap-of-near-all-rows）+ 阈值行数（>=10k）才 ADD：
--
--   1) i_ws_org_created —— INT-24 GET /workspaces（148x）：
--      before: BitmapAnd(i_workspace_organization_id + i_workspace_status)
--      取 24924 行 + top-N Sort，35.5ms（hit 50298）；
--      after : Index Scan act=51，0.24ms（hit 106）。
--      i_workspace_organization_id(organization_id) 是本索引严格前缀
--      （dedupe 机会，本迁移不做 REPLACE，保守 ADD）。
--
--   2) i_group_ws_created —— INT-26 GET /groups（204x，partial 索引）：
--      before: Hash Join 链 + Bitmap Heap 28501 行 + Seq Scan origin 30001
--      + top-N Sort，56.5ms（hit 57706 read 516）；
--      after : Index Scan 探测 58 行 -> nested loop，0.277ms（hit 444）。
--      谓词 scope='workspace' AND status=1 只收 workspace 域企业群；
--      i_group_scope_ws(workspace_id, created_at DESC) 是 workspace 维
--      访问路径，与本 org 维索引不重复。
--
--   3) i_gm_grp_created —— INT-27 GET /groups/{gid}/members（155x，partial）：
--      before: Seq Scan group_member 98000 行 + Gather Merge Sort，19.5ms；
--      after : Index Scan act=51，0.126ms（hit 2）。
--      谓词 status=1（活跃成员）；idx_group_member_role(group_id,role) 不
--      覆盖排序，uk_gid_uid 只覆盖等值——member 热分页的写放大可接受。
--
-- 判为 KEEP（不建）的 7 条见 verifiers/F2/index-decisions.json（INT-18/25/
-- 28/29/30/31 既有索引 plan 亚毫秒，无 seq 扫描；证据不足 = KEEP）。
--
-- 索引名沿用仓库 i_<语义缩写> 惯例（证据里 idx_cand_* 是测量用临时名，
-- 测后已从 scratch 删除，不入库）。
--
-- 注意：不用 CREATE INDEX CONCURRENTLY——erlang_migrate 外层单事务包裹
-- （与既有 1xx-14x 索引迁移同口径：140/141 均为事务内 CREATE INDEX）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- 1) INT-24：org 全域 workspace 列表 (organization_id, created_at DESC, id DESC)
-- ============================================================
CREATE INDEX IF NOT EXISTS i_ws_org_created
    ON workspace USING btree (organization_id, created_at DESC, id DESC);

COMMENT ON INDEX i_ws_org_created IS
    'INT-24 GET /workspaces org 全域列表 keyset 访问路径（F2 EXPLAIN 148x；i_workspace_organization_id 的 (organization_id) 前缀可后续去重）';

-- ============================================================
-- 2) INT-26：org 域企业群列表 (created_at DESC, id DESC) WHERE scope='workspace' AND status=1
-- ============================================================
CREATE INDEX IF NOT EXISTS i_group_ws_created
    ON public."group" USING btree (created_at DESC, id DESC)
    WHERE scope = 'workspace' AND status = 1;

COMMENT ON INDEX i_group_ws_created IS
    'INT-26 GET /groups org 域企业群 keyset 访问路径（F2 EXPLAIN 204x；partial 只收 workspace scope + 活跃群）';

-- ============================================================
-- 3) INT-27：群活跃成员分页 (group_id, created_at, id) WHERE status=1
-- ============================================================
CREATE INDEX IF NOT EXISTS i_gm_grp_created
    ON group_member USING btree (group_id, created_at, id)
    WHERE status = 1;

COMMENT ON INDEX i_gm_grp_created IS
    'INT-27 GET /groups/{gid}/members 活跃成员升序分页访问路径（F2 EXPLAIN 155x；partial 只收 status=1）';
