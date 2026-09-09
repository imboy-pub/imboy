-- STEP-08 教学 ACL/上下文 SQL 真库验证（moya_mig_test@4323，BEGIN...ROLLBACK 不留数据）
-- 验证 teaching_context_repo 所用 SQL 文本与 00000095-97 最终 schema 的兼容性：
--   submission_scope / guardian_contexts / staff_contexts / owner_contexts /
--   guardian_relation / staff_relation / org_owner_uid / learner_org / group_org
\set ON_ERROR_STOP on
BEGIN;

-- ===== 数据准备（与 eunit fixture 同构）=====
INSERT INTO "user" (id, password, account, reg_ip, reg_cosv) VALUES
  (980001, 'x', 't98_teacher',  '127.0.0.1', 'x'),  -- A1 老师
  (980002, 'x', 't98_parent',   '127.0.0.1', 'x'),  -- L1 监护人
  (980004, 'x', 't98_owner',    '127.0.0.1', 'x'),  -- A 机构 Owner（非 staff）
  (980005, 'x', 't98_grpadmin', '127.0.0.1', 'x'),  -- 仅群管理员
  (980006, 'x', 't98_multi',    '127.0.0.1', 'x');  -- G2 老师 + L1 监护人

INSERT INTO organization (id, name, owner_id) VALUES
  (981000, '机构A', 980004), (981900, '机构B', 980001);

INSERT INTO workspace (id, name, owner_id, organization_id) VALUES
  (982001, 'A-校区', 980004, 981000), (982900, 'B-校区', 980001, 981900);

INSERT INTO "group" (id, owner_uid, creator_uid, scope, workspace_id, title) VALUES
  (983001, 980004, 980004, 'workspace', 982001, 'A1-硬笔班'),
  (983002, 980004, 980004, 'workspace', 982001, 'A2-硬笔班'),
  (983901, 980001, 980001, 'workspace', 982900, 'B1-班');

INSERT INTO learner (id, organization_id, display_name) VALUES
  (984001, 981000, '大宝'), (984002, 981000, '二宝'), (984901, 981900, 'B学员');

INSERT INTO class_enrollment (group_id, learner_id) VALUES
  (983001, 984001), (983001, 984002), (983002, 984001), (983901, 984901);
SET CONSTRAINTS ALL IMMEDIATE;  -- enrollment 机构一致性触发器检查点

INSERT INTO class_staff (group_id, user_id, role) VALUES
  (983001, 980001, 'teacher'),   -- TEACHER_A @ A1
  (983002, 980006, 'manager');   -- MULTI @ A2
-- 注意：980005（仅群管理员）不插 class_staff —— T4 关键
-- workspace_member 前置（群成员 ⊆ 工作区成员子集约束）
INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, status) VALUES
  (982001, 980001, 'member', 980004, 'active'),
  (982001, 980002, 'member', 980004, 'active'),
  (982001, 980004, 'owner',  980004, 'active'),
  (982001, 980005, 'member', 980004, 'active'),
  (982001, 980006, 'member', 980004, 'active'),
  (982900, 980001, 'owner',  980001, 'active');
-- group_member（群管理员）插入以还原"仅 Group 管理员"前提（role/status 均为 smallint）
INSERT INTO group_member (id, group_id, user_id, alias, role, status, is_join) VALUES
  (985005, 983001, 980005, '群管理员', 2, 1, true);

INSERT INTO guardian_learner (guardian_uid, learner_id, can_submit, can_view_review) VALUES
  (980002, 984001, true, true),   -- GUARDIAN_B → L1
  (980006, 984001, true, true);   -- MULTI → L1（多身份）

INSERT INTO group_task (id, group_id, task_id, title, creator_id) VALUES
  (985001, 983001, 'task98_hash_001', '横竖练习', 980001),
  (985901, 983901, 'task98_hash_901', 'B班作业', 980001);

INSERT INTO group_task_assignment (id, task_id, user_id, learner_id) VALUES
  (986001, 'task98_hash_001', 980002, 984001),  -- L1 @ A1
  (986901, 'task98_hash_901', 980001, 984901);  -- B 学员 @ B1
SET CONSTRAINTS ALL IMMEDIATE;  -- gta 机构一致性触发器检查点

INSERT INTO homework_submission (id, assignment_id, learner_id, submitted_by, attempt_no) VALUES
  (987001, 986001, 984001, 980002, 1),   -- SUB_A1（org A）
  (987901, 986901, 984901, 980001, 1);   -- SUB_B1（org B）
SET CONSTRAINTS ALL IMMEDIATE;  -- submission 学员一致性触发器检查点

-- ===== Q1 submission_scope（teaching_context_repo SQL 原文，$1 内联）=====
SELECT hs.id AS submission_id, hs.assignment_id, hs.learner_id,
       hs.status AS submission_status, hs.attempt_no,
       a.task_id, a.learner_id AS assignment_learner_id,
       gt.group_id, w.organization_id AS org_id
FROM public.homework_submission hs
JOIN public.group_task_assignment a ON a.id = hs.assignment_id
JOIN public.group_task gt ON gt.task_id = a.task_id
JOIN public."group" g ON g.id = gt.group_id
JOIN public.workspace w ON w.id = g.workspace_id
WHERE hs.id = 987001;

-- ===== Q2 guardian_contexts（980006：L1 监护 + 入班 A1）=====
SELECT gl.learner_id, gl.can_submit, gl.can_view_review, gl.relation,
       l.display_name, l.organization_id,
       e.group_id, g.title AS group_title,
       w.id AS workspace_id, w.name AS workspace_name,
       o.id AS org_id, o.name AS org_name
FROM public.guardian_learner gl
JOIN public.learner l ON l.id = gl.learner_id AND l.status = 'active'
LEFT JOIN public.class_enrollment e
 ON e.learner_id = gl.learner_id AND e.status = 'active'
LEFT JOIN public."group" g ON g.id = e.group_id
LEFT JOIN public.workspace w ON w.id = g.workspace_id
LEFT JOIN public.organization o ON o.id = w.organization_id
WHERE gl.guardian_uid = 980006 AND gl.status = 'active'
ORDER BY gl.learner_id, e.group_id;

-- ===== Q3 staff_contexts（980006：A2 manager）=====
SELECT cs.group_id, cs.role, g.title AS group_title,
       w.id AS workspace_id, w.name AS workspace_name,
       o.id AS org_id, o.name AS org_name
FROM public.class_staff cs
JOIN public."group" g ON g.id = cs.group_id
JOIN public.workspace w ON w.id = g.workspace_id
JOIN public.organization o ON o.id = w.organization_id
WHERE cs.user_id = 980006 AND cs.status = 'active'
ORDER BY cs.group_id;

-- ===== Q4 owner_contexts（980004）=====
SELECT id AS org_id, name AS org_name FROM public.organization
WHERE owner_id = 980004 AND status = 'active' ORDER BY id;

-- ===== Q5-Q9 ACL 关系查询（deny-by-default 语义断言）=====
-- Q5 staff_relation：仅群管理员 980005 在 A1 班无行（T4 依据）
SELECT count(*) AS grpadmin_staff_rows FROM public.class_staff
WHERE user_id = 980005 AND group_id = 983001;
-- Q6 org_owner_uid：A 机构 Owner = 980004
SELECT owner_id FROM public.organization WHERE id = 981000 LIMIT 1;
-- Q7 learner_org：L1 → org A
SELECT organization_id FROM public.learner WHERE id = 984001 LIMIT 1;
-- Q8 group_org：A1 班 → org A（经 workspace）
SELECT w.organization_id AS org_id
FROM public."group" g
JOIN public.workspace w ON w.id = g.workspace_id
WHERE g.id = 983001 LIMIT 1;
-- Q9 guardian_relation：980005 对 L1 无监护行（T3/T4 依据）
SELECT count(*) AS grpadmin_guardian_rows FROM public.guardian_learner
WHERE guardian_uid = 980005 AND learner_id = 984001;

ROLLBACK;
