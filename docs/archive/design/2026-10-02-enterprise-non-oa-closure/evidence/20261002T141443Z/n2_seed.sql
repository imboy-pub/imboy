-- N2/N3 合成数据 seed（隔离测试库 imboy_test_v1，仅合成账号，无真实用户数据）
-- 幂等：按名称/账号查重。TSID 用 node=0 位面（seed_tsid 惯例，与线上节点不重叠）。
-- 用法：docker exec -i imboy_pg18 psql -U imboy_user -d imboy_test_v1 < n2_seed.sql

CREATE OR REPLACE FUNCTION pg_temp.seed_tsid(p_seq bigint) RETURNS bigint
LANGUAGE sql AS $$
  SELECT ((((extract(epoch FROM clock_timestamp()) * 1000)::bigint - 1735689600000) << 21)
          | (p_seq & 2047))
$$;

CREATE TEMP TABLE IF NOT EXISTS _nonoa_seed (k text PRIMARY KEY, v bigint);

BEGIN;

-- ============ 0) uid 解析（ctl 创建的合成账号） ============
CREATE TEMP TABLE _nonoa_uids AS
SELECT
  (SELECT id FROM public."user" WHERE account='nonoa_admin_1002' AND status>=0 LIMIT 1)  AS admin_uid,
  (SELECT id FROM public."user" WHERE account='nonoa_member_1002' AND status>=0 LIMIT 1) AS member_uid,
  (SELECT id FROM public."user" WHERE account='nonoa_b_1002' AND status>=0 LIMIT 1)      AS b_uid,
  (SELECT id FROM public."user" WHERE account='nonoa_ws_1002' AND status>=0 LIMIT 1)     AS ws_uid;

-- ============ 1) 企业 A：非OA验收企业A（owner=admin） ============
INSERT INTO organization (id, name, owner_id, status, branding, settings)
SELECT pg_temp.seed_tsid(101), '非OA验收企业A', admin_uid, 'active', '{}'::jsonb, '{}'::jsonb
FROM _nonoa_uids
WHERE NOT EXISTS (SELECT 1 FROM organization WHERE name='非OA验收企业A' AND owner_id=(SELECT admin_uid FROM _nonoa_uids));

INSERT INTO _nonoa_seed (k,v)
SELECT 'orgA', id FROM organization WHERE name='非OA验收企业A'
ORDER BY id LIMIT 1;

-- 企业 A 默认工作区
INSERT INTO workspace (id, name, logo, owner_id, status, type, organization_id, branding)
SELECT pg_temp.seed_tsid(102), '非OA验收企业A·默认工作区', '', (SELECT admin_uid FROM _nonoa_uids),
       'active', 'project', (SELECT v FROM _nonoa_seed WHERE k='orgA'), '{}'::jsonb
WHERE NOT EXISTS (SELECT 1 FROM workspace WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA')
                  AND name='非OA验收企业A·默认工作区');

INSERT INTO _nonoa_seed (k,v)
SELECT 'wsA', id FROM workspace
WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND name='非OA验收企业A·默认工作区'
ORDER BY id LIMIT 1;

INSERT INTO organization_default_workspace (organization_id, workspace_id)
SELECT (SELECT v FROM _nonoa_seed WHERE k='orgA'), (SELECT v FROM _nonoa_seed WHERE k='wsA')
ON CONFLICT (organization_id) DO NOTHING;

-- 企业 A 成员：owner=admin；member=普通成员；ws=普通成员（部门管理员见部门成员表）
INSERT INTO organization_member (organization_id, user_id, role, invited_by, joined_at, status)
SELECT (SELECT v FROM _nonoa_seed WHERE k='orgA'), admin_uid, 'owner', NULL, now(), 'active'
FROM _nonoa_uids
ON CONFLICT (organization_id, user_id) DO NOTHING;

INSERT INTO organization_member (organization_id, user_id, role, invited_by, joined_at, status)
SELECT (SELECT v FROM _nonoa_seed WHERE k='orgA'), member_uid, 'member', (SELECT admin_uid FROM _nonoa_uids), now(), 'active'
FROM _nonoa_uids
ON CONFLICT (organization_id, user_id) DO NOTHING;

INSERT INTO organization_member (organization_id, user_id, role, invited_by, joined_at, status)
SELECT (SELECT v FROM _nonoa_seed WHERE k='orgA'), ws_uid, 'member', (SELECT admin_uid FROM _nonoa_uids), now(), 'active'
FROM _nonoa_uids
ON CONFLICT (organization_id, user_id) DO NOTHING;

-- ============ 2) 企业 A 部门树（8 主部门 + 52 填充 = 60 根下部门，>50 默认页触发翻页；三层深） ============
-- 主部门
INSERT INTO organization_department (id, organization_id, parent_id, name, status, created_by_user_id)
SELECT pg_temp.seed_tsid(110+n), (SELECT v FROM _nonoa_seed WHERE k='orgA'), NULL, d.name, 'active', (SELECT admin_uid FROM _nonoa_uids)
FROM (VALUES (1,'销售中心'),(2,'技术中心'),(3,'综合职能部'),(4,'市场部'),
             (5,'财务部'),(6,'人力资源部'),(7,'法务部'),(8,'行政部')) AS d(n,name)
WHERE NOT EXISTS (SELECT 1 FROM organization_department
                  WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND name=d.name);

-- 填充部门（宽树/翻页）
INSERT INTO organization_department (id, organization_id, parent_id, name, status, created_by_user_id)
SELECT pg_temp.seed_tsid(200+n), (SELECT v FROM _nonoa_seed WHERE k='orgA'), NULL,
       '专项测试组' || lpad(n::text, 2, '0'), 'active', (SELECT admin_uid FROM _nonoa_uids)
FROM generate_series(1,52) AS n
WHERE NOT EXISTS (SELECT 1 FROM organization_department
                  WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA')
                    AND name='专项测试组' || lpad(n::text, 2, '0'));

-- 二级：销售中心下
INSERT INTO organization_department (id, organization_id, parent_id, name, status, created_by_user_id)
SELECT pg_temp.seed_tsid(300+n), (SELECT v FROM _nonoa_seed WHERE k='orgA'),
       (SELECT id FROM organization_department
        WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND name='销售中心'
        ORDER BY id LIMIT 1),
       d.name, 'active', (SELECT admin_uid FROM _nonoa_uids)
FROM (VALUES (1,'广州销售组'),(2,'深圳销售组'),(3,'佛山销售组')) AS d(n,name)
WHERE NOT EXISTS (SELECT 1 FROM organization_department dd
                  JOIN organization_department p ON dd.parent_id=p.id AND p.name='销售中心'
                  WHERE dd.organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND dd.name=d.name);

-- 二级：技术中心下
INSERT INTO organization_department (id, organization_id, parent_id, name, status, created_by_user_id)
SELECT pg_temp.seed_tsid(310+n), (SELECT v FROM _nonoa_seed WHERE k='orgA'),
       (SELECT id FROM organization_department
        WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND name='技术中心'
        ORDER BY id LIMIT 1),
       d.name, 'active', (SELECT admin_uid FROM _nonoa_uids)
FROM (VALUES (1,'平台架构组'),(2,'质量保障组')) AS d(n,name)
WHERE NOT EXISTS (SELECT 1 FROM organization_department dd
                  JOIN organization_department p ON dd.parent_id=p.id AND p.name='技术中心'
                  WHERE dd.organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND dd.name=d.name);

-- 三级：平台架构组下
INSERT INTO organization_department (id, organization_id, parent_id, name, status, created_by_user_id)
SELECT pg_temp.seed_tsid(320), (SELECT v FROM _nonoa_seed WHERE k='orgA'),
       (SELECT dd.id FROM organization_department dd
        JOIN organization_department p ON dd.parent_id=p.id AND p.name='技术中心'
        WHERE dd.organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND dd.name='平台架构组'
        ORDER BY dd.id LIMIT 1),
       '存储小组', 'active', (SELECT admin_uid FROM _nonoa_uids)
WHERE NOT EXISTS (SELECT 1 FROM organization_department dd
                  JOIN organization_department p ON dd.parent_id=p.id AND p.name='平台架构组'
                  WHERE dd.organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND dd.name='存储小组');

-- 部门成员：member→广州销售组；ws→技术中心(部门管理员)
INSERT INTO organization_department_member (organization_id, department_id, user_id, is_admin, added_by_user_id)
SELECT (SELECT v FROM _nonoa_seed WHERE k='orgA'),
       (SELECT dd.id FROM organization_department dd
        JOIN organization_department p ON dd.parent_id=p.id AND p.name='销售中心'
        WHERE dd.organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND dd.name='广州销售组'
        ORDER BY dd.id LIMIT 1),
       (SELECT member_uid FROM _nonoa_uids), false, (SELECT admin_uid FROM _nonoa_uids)
WHERE NOT EXISTS (SELECT 1 FROM organization_department_member
                  WHERE user_id=(SELECT member_uid FROM _nonoa_uids)
                    AND department_id IN (SELECT dd.id FROM organization_department dd
                        JOIN organization_department p ON dd.parent_id=p.id AND p.name='销售中心'
                        WHERE dd.name='广州销售组'));

INSERT INTO organization_department_member (organization_id, department_id, user_id, is_admin, added_by_user_id)
SELECT (SELECT v FROM _nonoa_seed WHERE k='orgA'),
       (SELECT id FROM organization_department
        WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND name='技术中心'
        ORDER BY id LIMIT 1),
       (SELECT ws_uid FROM _nonoa_uids), true, (SELECT admin_uid FROM _nonoa_uids)
WHERE NOT EXISTS (SELECT 1 FROM organization_department_member
                  WHERE user_id=(SELECT ws_uid FROM _nonoa_uids)
                    AND department_id=(SELECT id FROM organization_department
                        WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND name='技术中心'
                        ORDER BY id LIMIT 1));

-- ============ 3) 企业 B：非OA验收企业B（owner=b，单部门） ============
INSERT INTO organization (id, name, owner_id, status, branding, settings)
SELECT pg_temp.seed_tsid(401), '非OA验收企业B', b_uid, 'active', '{}'::jsonb, '{}'::jsonb
FROM _nonoa_uids
WHERE NOT EXISTS (SELECT 1 FROM organization WHERE name='非OA验收企业B' AND owner_id=(SELECT b_uid FROM _nonoa_uids));

INSERT INTO _nonoa_seed (k,v)
SELECT 'orgB', id FROM organization WHERE name='非OA验收企业B'
ORDER BY id LIMIT 1;

INSERT INTO workspace (id, name, logo, owner_id, status, type, organization_id, branding)
SELECT pg_temp.seed_tsid(402), '非OA验收企业B·默认工作区', '', (SELECT b_uid FROM _nonoa_uids),
       'active', 'project', (SELECT v FROM _nonoa_seed WHERE k='orgB'), '{}'::jsonb
WHERE NOT EXISTS (SELECT 1 FROM workspace WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgB')
                  AND name='非OA验收企业B·默认工作区');

INSERT INTO _nonoa_seed (k,v)
SELECT 'wsB', id FROM workspace
WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgB') AND name='非OA验收企业B·默认工作区'
ORDER BY id LIMIT 1;

INSERT INTO organization_default_workspace (organization_id, workspace_id)
SELECT (SELECT v FROM _nonoa_seed WHERE k='orgB'), (SELECT v FROM _nonoa_seed WHERE k='wsB')
ON CONFLICT (organization_id) DO NOTHING;

INSERT INTO organization_member (organization_id, user_id, role, invited_by, joined_at, status)
SELECT (SELECT v FROM _nonoa_seed WHERE k='orgB'), b_uid, 'owner', NULL, now(), 'active'
FROM _nonoa_uids
ON CONFLICT (organization_id, user_id) DO NOTHING;

INSERT INTO organization_department (id, organization_id, parent_id, name, status, created_by_user_id)
SELECT pg_temp.seed_tsid(410), (SELECT v FROM _nonoa_seed WHERE k='orgB'), NULL, '乙端产品部', 'active', (SELECT b_uid FROM _nonoa_uids)
WHERE NOT EXISTS (SELECT 1 FROM organization_department
                  WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgB') AND name='乙端产品部');

COMMIT;

-- ============ 汇报 ============
SELECT 'orgA' AS k, v FROM _nonoa_seed WHERE k='orgA'
UNION ALL SELECT 'wsA', v FROM _nonoa_seed WHERE k='wsA'
UNION ALL SELECT 'orgB', v FROM _nonoa_seed WHERE k='orgB';
SELECT 'orgA_root_deps' AS metric, count(*) FROM organization_department
WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA') AND parent_id IS NULL;
SELECT 'orgA_all_deps' AS metric, count(*) FROM organization_department
WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA');
SELECT 'orgA_members' AS metric, count(*) FROM organization_member
WHERE organization_id=(SELECT v FROM _nonoa_seed WHERE k='orgA');
