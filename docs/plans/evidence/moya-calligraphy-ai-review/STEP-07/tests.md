# STEP-07 证据 — 约束行为测试

脚本：`/tmp/moya_mig/step7_behavior_test.sql`（输出 `/tmp/moya_mig/step7_test_output.txt`）
数据拓扑：机构A/机构B 各自 workspace+班级群；group_task.task_id 为 varchar HashID 字符串；学员大宝/二宝（机构A，同一家长 970002）+ B机构学员。

| # | 验收项 | 用例 | 期望 | 结果 |
|---|---|---|---|---|
| T1 | DB-COMPAT-01 | 普通群作业（learner_id NULL）重复 (task_id,user_id) | unique_violation 且**约束名为 `group_task_assignment_task_id_user_id_key`**（错误契约保留） | PASS |
| T2 | DB-ASSIGN-01 | 同一家长(970002)替两个孩子(974001/974002)接收同一 task（同 task_id+user_id 两行不同 learner_id） | 成功（机构一致性触发器检查点通过） | PASS |
| T3 | DB-ASSIGN-01 | 同一 learner 重复教学 assignment | unique_violation（uk_group_task_assignment_task_learner） | PASS |
| T4 | DB-ASSIGN-01 | B机构学员收 A 机构班级 task 的教学 assignment | check_violation（trg_gta_learner_org_consistency）提交时拒绝 | PASS |
| T5 | DB-SUBMIT-01 | 同 assignment attempt 1/2 共存；重复 attempt_no | 成功 / unique_violation（uk_homework_submission_attempt） | PASS |
| T6 | DB-SUBMIT-01 | 旧 attempt 行内容/状态未被新提交触碰 | 断言成立 | PASS |
| T7 | — | submission.learner_id ≠ assignment.learner_id | check_violation（trg_homework_submission_learner_consistency） | PASS |
| T8 | — | submission 挂普通作业 assignment（learner_id NULL） | check_violation 拒绝（教学提交必须挂教学 assignment） | PASS |
| T9 | DB-REVIEW-01 | published 缺 reviewer_uid | check_violation | PASS |
| T10 | DB-REVIEW-01 | published 缺 published_at | check_violation | PASS |
| T11 | DB-REVIEW-01 | published 无任何反馈内容（文本/视频全空） | check_violation | PASS |
| T12 | DB-REVIEW-01 | 合法 published（reviewer+published_at+focus_problem） | 成功 | PASS |
| T13 | DB-REVIEW-01 | 同 submission 第二个 published / 多个 draft 共存 | unique_violation（uk_tr_published_per_submission）/ 成功 | PASS |
| T14 | — | AI 草稿：failed 不占坑 + 新 queued 成功；queued 后第二个有效草稿拒绝；succeeded 仍占坑 | 均符合（uk_crd_active_per_submission） | PASS |
| T15 | — | AI succeeded 缺 completed_at | check_violation（ck_crd_completed） | PASS |
| T16 | — | submission_asset 重复 (submission_id,attachment_id) / 非法 kind 'audio' | unique_violation / check_violation | PASS |
| T17 | — | FK 链：删 assignment 级联删 submission；删有提交历史的 learner 被拒 | CASCADE 生效 / restrict_violation | PASS |
| down | — | 有教学数据时 down 00000097 | 显式报错（清晰消息），数据保留不删 | PASS |
| eunit | DB-COMPAT-01 | 现有 group_task_repo_tests + group_task_assignment_repo_tests + 2 个既有迁移测试套件 | All 53 passed | PASS |

## 验收结论

- DB-SUBMIT-01 PASS：多 attempt 有序共存（T5）、旧 attempt 不可触碰（T6）、attempt_no 唯一兜底。
- DB-ASSIGN-01 PASS：一家长两孩子同 task（T2）；同 learner 不重复（T3）；跨机构教学 assignment 数据库拒绝（T4）。
- DB-REVIEW-01 PASS：AI 草稿与老师回评分表；published 必须有 reviewer/published_at/至少一种内容（T9-T12）；每 submission 至多一个已发布回评（T13）与一个有效 AI 草稿（T14）。
- DB-COMPAT-01 PASS：learner_id 可空兼容普通群作业（T1 + 行为不变）；同名约束错误契约保留（T1）；现有 group_task eunit 全部通过（53/53）。
