# STEP-06 证据 — 约束行为测试

脚本：`/tmp/moya_mig/step6_behavior_test.sql`（输出存 `/tmp/moya_mig/step6_test_output.txt`）

数据拓扑：机构A(961000)→ws 962001/962002 两个校区；机构B(961900)→ws 962900；无机构历史 ws 962910；personal 群 963999；学员 A1/A2（机构A）、B1/B2（机构B）。

| # | 验收项 | 用例 | 期望 | 结果 |
|---|---|---|---|---|
| T1 | DB-LEARNER-01 | 机构A学员同时入机构A两个不同 workspace 的班级（跨 Workspace 同机构） | 成功（deferred 触发器 SET CONSTRAINTS ALL IMMEDIATE 检查点通过） | PASS |
| T2 | DB-LEARNER-01 | 机构A学员入机构B班级（跨机构 enrollment） | check_violation 拒绝，且校验错误消息含 `trg_class_enrollment_org_consistency` | PASS |
| T3 | DB-LEARNER-01 | personal 群（无 workspace）接收学员入班 | check_violation 拒绝（教学班必须挂 workspace 群） | PASS |
| T4 | DB-LEARNER-01 | workspace 群但 workspace.organization_id IS NULL | check_violation 拒绝（fail-closed：解析不出机构即拒） | PASS |
| T5 | DB-GUARDIAN-01 | 一名家长(960002)绑定两个学员，can_view_review 按孩子细分（true/false） | 成功 | PASS |
| T6 | DB-GUARDIAN-01 | 一名学员(964001)绑定第二监护人(960003, relation=other, can_submit=false) | 成功 | PASS |
| T7 | DB-GUARDIAN-01 | 重复插入 (guardian_uid=960002, learner_id=964001) | unique_violation 拒绝 | PASS |
| T8 | DB-BIND-01 | user_id IS NULL 的未登录学员档案长期存在（A 机构 2 个 + B 机构 1 个并存） | 成功（部分唯一索引不约束 NULL） | PASS |
| T9 | DB-BIND-01 | 同一 user(960004) 在机构A绑第二个 learner | unique_violation（uk_learner_org_user）拒绝 | PASS |
| T10 | DB-BIND-01 | 同一 user(960004) 跨机构（A 的 964001 + B 的 964901）各绑一个 learner | 成功 | PASS |
| T11 | — | 已有 enrollment 的 learner 变更 organization_id | check_violation（trg_learner_org_change_guard）拒绝 | PASS |
| T12 | — | class_staff 重复 (group_id,user_id)；非法角色 'principal' | unique_violation / check_violation 拒绝 | PASS |
| T13 | — | class_profile 非法 course_type 'ink'；正例 hard_pen+term | 违规拒绝 / 正例成功 | PASS |

## 实现说明（机构一致性方案选型）

- **复合 FK 不可行**：group 无 organization_id 列（经 workspace 间接归属），被引用唯一键不在单表上。
- **采用可延迟约束触发器**（仓库先例 00000077 `trg_group_member_ws_subset` 同款）：
  `trg_class_enrollment_org_consistency`（DEFERRABLE INITIALLY DEFERRED）在提交时校验
  `learner.organization_id == (SELECT w.organization_id FROM "group" g JOIN workspace w ON w.id=g.workspace_id WHERE g.id=NEW.group_id)`；
  解析不出机构（personal 群 / workspace 无机构）一律拒绝。
- **补充 trg_learner_org_change_guard**（立即触发）：已有 enrollment 的 learner 禁止换机构，防止事后篡改使历史 enrollment 失效。

## 踩坑记录

- deferred 触发器在子事务（PL/pgSQL EXCEPTION 块）内不会自动触发检查，需 `SET CONSTRAINTS ALL IMMEDIATE` 显式建立检查点，否则跨机构拒绝只会在最外层 COMMIT 时暴露、无法被测试块捕获。
- T11 初版用已绑定 user 的学员做机构变更，先撞 uk_learner_org_user（unique_violation）而非 guard；改为用未绑定账号的学员 964902 后命中预期约束。

## 验收结论

- DB-LEARNER-01 PASS：同机构跨 Workspace 入班通过（T1）；跨机构 enrollment 提交时被数据库拒绝（T2），personal 群/无机构 workspace 亦拒绝（T3/T4）。
- DB-GUARDIAN-01 PASS：一家长多学员（T5）、一学员多监护人（T6）、重复关系拒绝（T7）。
- DB-BIND-01 PASS：未登录档案长期存在（T8）；同机构重复绑定拒绝（T9）；同用户跨机构独立绑定通过（T10）。
