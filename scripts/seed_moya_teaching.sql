-- ============================================================================
-- seed_moya_teaching.sql
-- 墨芽教学域「最小可用造数」：机构 → 工作区 → 班级 → 老师 → 学员 → 家长
--
-- 为什么需要这个脚本
-- ------------------
-- imboy 后端目前**没有任何创建教学数据的接口**：
--   class_staff / learner / class_enrollment / guardian_learner / class_profile
-- 这五张表在 src/ 下只有 SELECT，INSERT 只出现在 test/ 的集成测试夹具里；
-- 43 个 adm_* 模块中只有 adm_organization_handler 与组织相关，无教学端点；
-- admin 前端搜 class_staff / learner / guardian_learner 命中数均为 0。
--
-- 后果：小程序 GET /api/v1/moya/contexts 恒返回空列表，登录后只能落到
-- pages/no-identity（「还没有可用的身份」），且**在管理后台怎么点都点不出来**
-- ——因为「把某人变成老师」这一跳的代码不存在。
--
-- 本脚本用一次幂等写入补齐这条链路，让真实账号能在小程序里看到老师端/家长端。
-- [注意] 这是权宜手段（一次性数据），不是产品能力。产品化需要新增教学管理端点。
--
-- 用法
-- ----
--   PGPASSWORD=<pw> psql -h <host> -p <port> -U <user> -d <db> \
--     -v owner_uid=<机构所有者 uid> \
--     -v org_name='<机构名>' \
--     -v class_title='<班级名>' \
--     -v teacher_uids='{<老师uid1>,<老师uid2>}' \
--     -v learner_names='{<学员名1>}' \
--     -v guardian_uids='{<家长uid1>}' \
--     -f scripts/seed_moya_teaching.sql
--
--   参数说明
--     owner_uid      机构所有者（必填）。会经触发器自动成为 organization_member(owner)。
--     org_name       机构名（必填）。同名同 owner 的机构已存在则复用。
--     class_title    班级名（必填）。同工作区内同名群已存在则复用。
--     teacher_uids   教学角色 uid 列表（必填，≥1）。**第 1 个是 manager（班主任）**，
--                    其余为 teacher。manager 才有绑定学员的权限（moya_learner_bind_logic）。
--     learner_names  学员显示名列表（必填，≥1）。与 guardian_uids 一一对应。
--     guardian_uids  家长 uid 列表，与 learner_names 等长；写 0 表示该学员暂无家长账号。
--     rollback=1     只验证不落库（末尾 ROLLBACK），用于上线前预演。
--
-- 幂等性
-- ------
-- organization / workspace / "group" 按名字复用（已存在则不新建）；
-- 关系表（class_profile / class_staff / class_enrollment / guardian_learner）
-- 靠主键 ON CONFLICT 去重。**重复执行不会产生重复行**。
--
-- 实测记录（本地库 imboy_test_v1，2026-09-24）
-- ------------------------------------------
--   1. 造数前 class_staff 相关行数 = 0；
--   2. rollback=1 预演通过（全部语句 OK，末尾 ROLLBACK 无残留）；
--   3. 正式提交后复核查询返回预期行数：2 个老师各 1 行、1 个家长 1 行；
--   4. 连跑两次提交 → 业务表行数不变（1/1/2/1/1/1），幂等成立；
--   5. 服务端真实代码路径复核（RPC 调 moya_context_logic:contexts/2，
--      schema=organization，与小程序 ?schema_version=2 一致）：
--        老师 uid → 返回 teacher 上下文；
--        家长 uid → 返回 guardian 上下文（can_submit/can_view_review 均为 true）；
--        未造数的对照 uid → {ok, #{contexts => []}}（反向对照，证明非恒真）；
--   6. 清理后回到全 0。
--
-- 回滚（按需手工执行，注意先备份）
-- --------------------------------
--   以下顺序按外键依赖排列，可整段执行（已在本地库实测）：
--     BEGIN;
--     DELETE FROM guardian_learner WHERE learner_id IN
--       (SELECT id FROM learner WHERE display_name = ANY (ARRAY['<学员名>']));
--     DELETE FROM class_enrollment WHERE learner_id IN
--       (SELECT id FROM learner WHERE display_name = ANY (ARRAY['<学员名>']));
--     DELETE FROM learner WHERE display_name = ANY (ARRAY['<学员名>']);
--     DELETE FROM class_staff WHERE group_id IN
--       (SELECT id FROM "group" WHERE title = '<班级名>');
--     DELETE FROM class_profile WHERE group_id IN
--       (SELECT id FROM "group" WHERE title = '<班级名>');
--     DELETE FROM "group" WHERE title = '<班级名>';
--     DELETE FROM organization_default_workspace
--       WHERE organization_id IN (SELECT id FROM organization WHERE name = '<机构名>');
--     DELETE FROM workspace WHERE name = '<机构名>·默认工作区';
--     DELETE FROM organization WHERE name = '<机构名>';
--     COMMIT;
--   注意：workspace.organization_id 与 learner.organization_id 都是
--   ON DELETE RESTRICT ⇒ 必须先删 learner 与 workspace，再删 organization。
-- ============================================================================

\set ON_ERROR_STOP on

-- ---------------------------------------------------------------- 参数校验
\if :{?owner_uid}
\else
  DO $seed_guard$ BEGIN RAISE EXCEPTION '[seed_moya_teaching] 缺少必填参数 owner_uid（机构所有者的 user id）'; END $seed_guard$;
\endif
\if :{?org_name}
\else
  DO $seed_guard$ BEGIN RAISE EXCEPTION '[seed_moya_teaching] 缺少必填参数 org_name（机构名）'; END $seed_guard$;
\endif
\if :{?class_title}
\else
  DO $seed_guard$ BEGIN RAISE EXCEPTION '[seed_moya_teaching] 缺少必填参数 class_title（班级名）'; END $seed_guard$;
\endif
\if :{?teacher_uids}
\else
  DO $seed_guard$ BEGIN RAISE EXCEPTION '[seed_moya_teaching] 缺少必填参数 teacher_uids（形如 {111,222} 的 uid 数组，第 1 个为班主任）'; END $seed_guard$;
\endif
\if :{?learner_names}
\else
  DO $seed_guard$ BEGIN RAISE EXCEPTION '[seed_moya_teaching] 缺少必填参数 learner_names（形如 {小雨} 的学员名数组）'; END $seed_guard$;
\endif
\if :{?guardian_uids}
\else
  \set guardian_uids '{0}'
\endif

-- ------------------------------------------------------------------ TSID 生成
-- 严格复刻 src/lib/elib_tsid.erl 的位布局：
--   [sign 1][timestamp 42][node 10][sequence 11]
--   id = (自 2025-01-01 起的毫秒数 << 21) | (node << 11) | seq
--
-- 为什么 node 固定 0：imboy 默认配置 tsid_dc_id=1 / tsid_node_id=1 / tsid_dc_bits=3
-- ⇒ NodeBits=7，线上 CombinedNode = (1 << 7) | 1 = 129。
-- 本脚本用 node=0，与线上节点**位面不重叠**，因此生成的 ID 恒不与线上
-- 真实生成的 ID 冲突（node 位不同即不同 ID）。时间戳取 clock_timestamp()，
-- 不硬编码常量，保证 ID 落在合理时间区间内、可被 elib_tsid:parse/1 正常解析。
CREATE OR REPLACE FUNCTION pg_temp.seed_tsid(p_seq bigint) RETURNS bigint
LANGUAGE sql AS $$
  SELECT ((((extract(epoch FROM clock_timestamp()) * 1000)::bigint - 1735689600000) << 21)
          | (p_seq & 2047))
$$;

CREATE TEMP TABLE IF NOT EXISTS _moya_seed (k text PRIMARY KEY, v bigint);

BEGIN;

-- ------------------------------------------------------------- 1) 机构
INSERT INTO organization (id, name, owner_id, status, branding, settings)
SELECT pg_temp.seed_tsid(1), :'org_name', :owner_uid, 'active', '{}'::jsonb, '{}'::jsonb
WHERE NOT EXISTS (
    SELECT 1 FROM organization WHERE name = :'org_name' AND owner_id = :owner_uid
);

INSERT INTO _moya_seed (k, v)
SELECT 'org', id FROM organization
 WHERE name = :'org_name' AND owner_id = :owner_uid
 ORDER BY id LIMIT 1;

-- ------------------------------------------------------------- 2) 工作区
-- 教学班级必须挂在**已归属机构**的工作区下：判定链是
-- class_staff → group → workspace → organization，任何一环为空都不构成教学身份。
INSERT INTO workspace (id, name, logo, owner_id, status, type, organization_id, branding)
SELECT pg_temp.seed_tsid(2),
       :'org_name' || '·默认工作区',
       '',
       :owner_uid,
       'active',
       'project',
       (SELECT v FROM _moya_seed WHERE k = 'org'),
       '{}'::jsonb
WHERE NOT EXISTS (
    SELECT 1 FROM workspace
     WHERE organization_id = (SELECT v FROM _moya_seed WHERE k = 'org')
       AND name = :'org_name' || '·默认工作区'
);

INSERT INTO _moya_seed (k, v)
SELECT 'ws', id FROM workspace
 WHERE organization_id = (SELECT v FROM _moya_seed WHERE k = 'org')
   AND name = :'org_name' || '·默认工作区'
 ORDER BY id LIMIT 1;

-- 默认工作区关系（organization_default_workspace，PK = organization_id）
INSERT INTO organization_default_workspace (organization_id, workspace_id)
SELECT (SELECT v FROM _moya_seed WHERE k = 'org'), (SELECT v FROM _moya_seed WHERE k = 'ws')
ON CONFLICT (organization_id) DO NOTHING;

-- ------------------------------------------------------------- 3) 班级群
-- scope 必须 'workspace' 且 workspace_id 非空（chk_group_scope_xor）；
-- type = 2（私有群）；e2ee_mode = 0；status = 1（启用）。
INSERT INTO "group" (id, title, owner_uid, creator_uid, scope, workspace_id,
                     status, type, e2ee_mode, member_max, member_count)
SELECT pg_temp.seed_tsid(3),
       :'class_title',
       :owner_uid,
       :owner_uid,
       'workspace',
       (SELECT v FROM _moya_seed WHERE k = 'ws'),
       1, 2, 0, 1000, 1
WHERE NOT EXISTS (
    SELECT 1 FROM "group"
     WHERE title = :'class_title'
       AND workspace_id = (SELECT v FROM _moya_seed WHERE k = 'ws')
);

INSERT INTO _moya_seed (k, v)
SELECT 'grp', id FROM "group"
 WHERE title = :'class_title'
   AND workspace_id = (SELECT v FROM _moya_seed WHERE k = 'ws')
 ORDER BY id LIMIT 1;

-- 班级教学档案（1:1 扩展；course_type: hard_pen 硬笔 | brush 毛笔 | mixed 混合）
INSERT INTO class_profile (group_id, course_type, term, status)
SELECT (SELECT v FROM _moya_seed WHERE k = 'grp'), 'hard_pen', NULL, 'active'
ON CONFLICT (group_id) DO NOTHING;

-- ------------------------------------------------------------- 4) 老师（核心）
-- class_staff 是「谁是老师」的唯一真源。表注释明确：
--   「聊天管理角色≠教学角色」「不从 Group 管理员推断」
-- 所以把某人设成组织 admin 或群管理员都不会让他成为老师，必须写在这里。
INSERT INTO class_staff (group_id, user_id, role, status)
SELECT (SELECT v FROM _moya_seed WHERE k = 'grp'),
       t.uid,
       CASE WHEN t.ord = 1 THEN 'manager' ELSE 'teacher' END,
       'active'
  FROM unnest(:'teacher_uids'::bigint[]) WITH ORDINALITY AS t(uid, ord)
ON CONFLICT (group_id, user_id)
DO UPDATE SET role = EXCLUDED.role, status = 'active', updated_at = now();

-- ------------------------------------------------------------- 5) 学员档案
INSERT INTO learner (id, organization_id, display_name, status)
SELECT pg_temp.seed_tsid(100 + t.ord),
       (SELECT v FROM _moya_seed WHERE k = 'org'),
       t.nm,
       'active'
  FROM unnest(:'learner_names'::text[]) WITH ORDINALITY AS t(nm, ord)
 WHERE NOT EXISTS (
    SELECT 1 FROM learner
     WHERE organization_id = (SELECT v FROM _moya_seed WHERE k = 'org')
       AND display_name = t.nm
 );

-- ------------------------------------------------------------- 6) 学员入班
-- 触发器 trg_class_enrollment_org_consistency（DEFERRABLE）在提交时校验
-- learner.organization_id == group→workspace→organization_id，不一致即 23514。
INSERT INTO class_enrollment (group_id, learner_id, status)
SELECT (SELECT v FROM _moya_seed WHERE k = 'grp'), l.id, 'active'
  FROM learner l
 WHERE l.organization_id = (SELECT v FROM _moya_seed WHERE k = 'org')
   AND l.display_name = ANY (:'learner_names'::text[])
ON CONFLICT (group_id, learner_id) DO NOTHING;

-- ------------------------------------------------------------- 7) 家长绑定
-- guardian_learner 是「家长能不能替孩子提交 / 看回评」的权限真源。
-- 同机构内一个 user 只能绑一个 learner（uk_learner_org_user 部分唯一索引）。
INSERT INTO guardian_learner (guardian_uid, learner_id, relation,
                              can_submit, can_view_review, status)
SELECT g.uid, l.id, 'guardian', true, true, 'active'
  FROM unnest(:'learner_names'::text[], :'guardian_uids'::bigint[]) AS g(nm, uid)
  JOIN learner l
    ON l.organization_id = (SELECT v FROM _moya_seed WHERE k = 'org')
   AND l.display_name = g.nm
 WHERE g.uid > 0
ON CONFLICT (guardian_uid, learner_id) DO NOTHING;

-- ------------------------------------------------------------- 结果回读
\echo ''
\echo '=== 造数结果 ==='
SELECT (SELECT v FROM _moya_seed WHERE k = 'org') AS organization_id,
       (SELECT v FROM _moya_seed WHERE k = 'ws')  AS workspace_id,
       (SELECT v FROM _moya_seed WHERE k = 'grp') AS group_id;

\echo '--- 老师（class_staff）---'
SELECT cs.user_id, cs.role, cs.status, g.title AS class_title, o.name AS org_name
  FROM class_staff cs
  JOIN "group" g     ON g.id = cs.group_id
  JOIN workspace w   ON w.id = g.workspace_id
  JOIN organization o ON o.id = w.organization_id
 WHERE cs.group_id = (SELECT v FROM _moya_seed WHERE k = 'grp')
 ORDER BY cs.role, cs.user_id;

\echo '--- 学员 / 入班 / 家长 ---'
SELECT l.id AS learner_id, l.display_name,
       (SELECT count(*) FROM class_enrollment e
         WHERE e.learner_id = l.id AND e.status = 'active') AS in_class,
       (SELECT count(*) FROM guardian_learner gl
         WHERE gl.learner_id = l.id AND gl.status = 'active') AS guardians
  FROM learner l
 WHERE l.organization_id = (SELECT v FROM _moya_seed WHERE k = 'org')
   AND l.display_name = ANY (:'learner_names'::text[])
 ORDER BY l.id;

\echo ''
\echo '=== 教学身份判定复核（与 src/repo/moya_context_repo.erl 的 SQL 同构）==='
\echo '--- staff_contexts：老师应各返回 1 行 ---'
SELECT cs.user_id, cs.role, g.title AS group_title, o.name AS org_name
  FROM class_staff cs
  JOIN "group" g     ON g.id = cs.group_id
  JOIN workspace w   ON w.id = g.workspace_id
  JOIN organization o ON o.id = w.organization_id
 WHERE cs.user_id = ANY (:'teacher_uids'::bigint[]) AND cs.status = 'active'
 ORDER BY cs.user_id;

\echo '--- guardian_contexts：有 uid 的家长应返回行 ---'
SELECT gl.guardian_uid, l.display_name, l.organization_id, o.name AS org_name
  FROM guardian_learner gl
  JOIN learner l     ON l.id = gl.learner_id AND l.status = 'active'
  JOIN organization o ON o.id = l.organization_id
 WHERE gl.guardian_uid = ANY (:'guardian_uids'::bigint[]) AND gl.status = 'active'
 ORDER BY gl.guardian_uid;

-- ------------------------------------------------------------- 结束
\if :{?rollback}
  \echo ''
  \echo '[注意] rollback=1：本次改动已全部回滚，数据库未发生变更。'
  ROLLBACK;
\else
  COMMIT;
  \echo ''
  \echo '[完成] 已提交。下一步：用老师/家长的微信登录小程序，首屏应直接进入'
  \echo '   老师端（4 个 tab：待点评/作业/班级/我的）或家长端（3 个 tab）。'
\endif
