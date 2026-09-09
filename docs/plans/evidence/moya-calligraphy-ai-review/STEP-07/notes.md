# STEP-07 备注

## 产物

- `priv/migrations/00000097_homework_review_loop.up.sql`
- `priv/migrations/00000097_homework_review_loop.down.sql`
- `test/repo/moya_teaching_migration_tests.erl`（SQL 结构断言 eunit，12 用例）

## 最终表结构（给 Step 8/9/10/11 的依赖说明）

### group_task_assignment（扩展，expand-first）
- 新增可空列：`learner_id bigint`（FK→learner RESTRICT）、`submitted_by bigint`（FK→"user" SET NULL）
- 唯一性改造：
  - `group_task_assignment_task_id_user_id_key` **部分唯一索引** `(task_id,user_id) WHERE learner_id IS NULL`（沿用原约束名=保留错误契约；ON CONFLICT 写法需改为 `ON CONFLICT (task_id,user_id) WHERE learner_id IS NULL`）
  - `uk_group_task_assignment_task_learner` **部分唯一索引** `(task_id,learner_id) WHERE learner_id IS NOT NULL`
- 新索引：`i_group_task_assignment_learner (learner_id) WHERE learner_id IS NOT NULL`
- 触发器：`trg_gta_learner_org_consistency`（DEFERRABLE；learner_id 非空时校验 task→group→workspace→organization == learner.organization_id）
- **注意**：assignment.task_id 是 varchar(40) HashID 字符串（引用 group_task.task_id），不是 bigint FK。

### homework_submission
- `id bigint PK`；`assignment_id FK→group_task_assignment(id) ON DELETE CASCADE`
- `learner_id bigint NOT NULL FK→learner(id) ON DELETE RESTRICT`（触发器强制 == assignment.learner_id）
- `submitted_by bigint NULL FK→"user"(id) SET NULL`；`attempt_no int CHECK(>0)`；`status`（CHECK: submitted|withdrawn）
- `UNIQUE uk_homework_submission_attempt (assignment_id, attempt_no)`（attempt_no 由应用层 MAX+1 分配）
- 索引：`i_homework_submission_queue (status, submitted_at DESC)`
- 触发器：`trg_homework_submission_learner_consistency`（DEFERRABLE）

### submission_asset
- `id PK`；`submission_id FK→homework_submission CASCADE`；`attachment_id FK→attachment RESTRICT`
- `kind`（CHECK: practice_video|final_photo）；`sort_order int`；`created_by FK→"user" SET NULL`
- `UNIQUE uk_submission_asset_attachment (submission_id, attachment_id)`
- 索引：`i_submission_asset_submission (submission_id, kind, sort_order)`
- ⚠️ attachment 表现状（后续迁移已演进）：唯一列是 `file_hash256`+`path`（md5 已不存在），造数需给两者不同值。

### calligraphy_review_draft
- `id PK`；`submission_id FK→homework_submission CASCADE`；`ai_task_id varchar(64) NULL`
- `status`（CHECK: queued|running|succeeded|failed）；`model_profile/prompt_version/rubric_version/input_digest`
- `result_json jsonb`（只存结构化结果）；`error_code`；`created_at`；`completed_at`（CHECK：succeeded|failed 必填）
- 部分唯一索引 `uk_crd_active_per_submission (submission_id) WHERE status IN ('queued','running','succeeded')`
  —— **语义**：failed 可重试新行；succeeded 后不可重跑，需新 attempt 新 submission（Step 11 Worker 注意）
- 索引：`i_crd_status_created (status, created_at)`

### teacher_review
- `id PK`；`submission_id FK→homework_submission CASCADE`
- `reviewer_uid bigint NULL FK→"user" SET NULL`（published 时 CHECK 强制非空）
- `positive_point/focus_problem/practice_action/comment text NOT NULL DEFAULT ''`
- `video_attachment_id FK→attachment RESTRICT`；`rework_required bool`
- `status`（CHECK: draft|published|discarded）；`published_at`
- 发布完整性 CHECK `ck_teacher_review_published`：published ⇒ reviewer_uid 非空 AND published_at 非空 AND 至少一种内容非空
- 部分唯一索引 `uk_tr_published_per_submission (submission_id) WHERE status='published'`
- 索引：`i_teacher_review_submission (submission_id, status)`、`i_teacher_review_reviewer (reviewer_uid) WHERE reviewer_uid IS NOT NULL`

## 关键设计决策

1. **同名部分唯一索引保留错误契约**：现有 eunit（group_task_assignment_repo_tests:292）断言约束名 `group_task_assignment_task_id_user_id_key`；普通作业分支索引直接沿用该名，DB 层与应用层错误契约均不变。
2. **用户删除流程兼容**：user_deletion_executor 物理删除该用户的 assignment 行 → submission CASCADE 跟随删除（与现状语义一致）；所有引用 "user" 的新列可空 SET NULL，不阻塞删除事务。
3. **儿童证据 fail-closed**：learner（有 submission 时）与 attachment 被 RESTRICT 保护，物理删除须走显式清理流程（Step 10 删除传播 / §9.3 删除请求）。
4. **down 数据保护**：教学数据存在时 down 显式失败并提示人工清理，绝不静默删行。

## 风险与给后续泳道的注意点

- **ON CONFLICT 语法变化**：任何对 assignment 的 `ON CONFLICT ON CONSTRAINT group_task_assignment_task_id_user_id_key` 写法在新结构下失效（约束已变索引）；当前代码库无此写法（已 grep 确认），Step 9 写 Repo 时须用部分索引推断语法或先查后插。
- 触发器均为 DEFERRABLE INITIALLY DEFERRED：应用事务在 COMMIT 时才收到机构/学员一致性错误；如需即时反馈，事务内 `SET CONSTRAINTS ALL IMMEDIATE`（Step 9 可选）。
- scratch 库 `moya_mig_test`（4323）保留为 00000001→00000097 全量最终态，可供 Step 8/9/17 直接复用做集成测试基线；不需要时可 `DROP DATABASE moya_mig_test`。
