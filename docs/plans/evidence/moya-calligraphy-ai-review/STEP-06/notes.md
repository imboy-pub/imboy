# STEP-06 备注

## 产物

- `priv/migrations/00000096_teaching_identity.up.sql`
- `priv/migrations/00000096_teaching_identity.down.sql`

## 最终表结构（给 Step 8/9/16 的依赖说明）

### class_profile（Group 1:1 教学扩展）
- `group_id bigint PK, FK→"group"(id) ON DELETE CASCADE`
- `course_type text NOT NULL DEFAULT 'hard_pen'`（CHECK: hard_pen|brush|mixed）
- `term varchar(100) NULL`；`status text 'active'`（CHECK: active|archived）；`created_at/updated_at`

### class_staff（复合 PK）
- `PK (group_id, user_id)`；`group_id FK→"group" CASCADE`；`user_id FK→"user" CASCADE`
- `role text 'teacher'`（CHECK: manager|teacher|assistant）；`status 'active'`（CHECK: active|removed）
- 索引 `i_class_staff_user_status (user_id, status)`

### learner
- `id bigint PK (TSID)`；`organization_id bigint NOT NULL FK→organization(id) ON DELETE RESTRICT`
- `display_name varchar(100) NOT NULL`；`birth_year int NULL`（CHECK 1900..2100）
- `user_id bigint NULL FK→"user"(id) ON DELETE SET NULL`（未来账号绑定）
- `account_bound_at timestamptz NULL`；`account_bound_by bigint NULL FK→"user" SET NULL`
- `status 'active'`（CHECK: active|archived）
- 部分唯一索引 `uk_learner_org_user UNIQUE (organization_id, user_id) WHERE user_id IS NOT NULL`
- 索引 `i_learner_org_status (organization_id, status)`
- 触发器 `trg_learner_org_change_guard`（有 enrollment 时禁改 organization_id）

### class_enrollment（复合 PK）
- `PK (group_id, learner_id)`；`group_id FK→"group" CASCADE`；`learner_id FK→learner(id) CASCADE`
- `status 'active'`（CHECK: active|removed）；`joined_at timestamptz NOT NULL DEFAULT now()`
- 索引 `i_class_enrollment_learner (learner_id)`
- 约束触发器 `trg_class_enrollment_org_consistency`（DEFERRABLE INITIALLY DEFERRED，
  函数 `fn_class_enrollment_org_check`，ERRCODE 23514，提交时校验
  learner.organization_id == group→workspace→organization_id；机构解析为 NULL 即拒绝）

### guardian_learner（复合 PK）
- `PK (guardian_uid, learner_id)`；`guardian_uid FK→"user" CASCADE`；`learner_id FK→learner CASCADE`
- `relation 'guardian'`（CHECK: guardian|other）；`can_submit bool true`；`can_view_review bool true`
- `status 'active'`（CHECK: active|removed）
- 索引 `i_guardian_learner_guardian (guardian_uid, status)`、`i_guardian_learner_learner (learner_id, status)`

## 关键设计决策

1. 教学班级准入规则（数据库层）：必须是 `scope='workspace'` 且 `workspace.organization_id IS NOT NULL` 的群才能接收 enrollment；跨机构一律拒绝。Step 8 的 ACL 可直接依赖该约束做 fail-closed 兜底。
2. guardian_learner 是提交/查看权限真源（计划 §6.3），can_submit/can_view_review 布尔按孩子细分；class_staff.role=assistant 无发布权由 Step 9 业务层实现。
3. 软删除语义统一：class_staff/class_enrollment/guardian_learner 用 status=removed（软删），重新生效可 UPDATE 回 active；learner/class_profile 用 active|archived。

## 风险

- deferred 触发器在长事务里到 COMMIT 才报错，应用层如需即时反馈可在事务内 `SET CONSTRAINTS ALL IMMEDIATE` 或由 Logic 层预检（错误消息带 HINT 定位）。
- group→workspace→organization 三跳查询在 enrollment 热路径上每行执行一次；班级规模（<100 学员）下无压力，如未来量级上升可在 class_profile 上冗余 organization_id（另立迁移）。
