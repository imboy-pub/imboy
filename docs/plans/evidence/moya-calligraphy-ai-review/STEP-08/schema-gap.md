# STEP-08 — Schema Compatibility Audit（Step 9 开工前）

> **状态更新（R3）**：本审计已被 Agent C 的 `00000098_submission_idempotency_withdraw`
> 迁移取代——G1/G2/G4 全部落库（幂等列、部分唯一约束、双向互斥触发器 + withdraw 审计
> CHECK），G3 的 FOR UPDATE 串行化点配方见 `STEP-08-DB/notes.md`。本文保留作为审计
> 记录；Step 9 Repo 按 STEP-08-DB 配方实现。

> 审计对象：`priv/migrations/00000097_homework_review_loop.up.sql`（Agent C 冻结版）
> 对照契约：`STEP-04/openapi/moya-teaching.yaml`、`STEP-04/state-machines.md`、
> `STEP-04/schemas/submission-create-request.json`。
> 结论先行：**Coordinator 点名的 4 项疑似缺口全部属实**，另发现 2 项新缺口（G5/G6）。
> 本文件只报告不修复；修复方案建议归 DATABASE Agent 以 00000098+ 统一落地。

## 逐条核实

### G1（确认）homework_submission 缺幂等列

- 事实：`CREATE TABLE homework_submission` 仅有
  `id/assignment_id/learner_id/submitted_by/attempt_no/status/submitted_at/created_at`。
  **无 idempotency_key、request_digest、withdrawn_at、withdrawn_by**。
- 契约要求：STEP-04（createSubmission：Idempotency-Key 头；`(uid, assignment_id,
  idempotency_key)` 去重；withdrawn 需时间与操作人审计；withdrawn_at 进入
  SubmissionSummary schema）。
- 无列的后果：
  1. 幂等记录只能放应用侧缓存/新表（计划铁律禁 Redis；epgsql 池 + depcache 不持久、
     重启即丢重放保护，IDEMP-01 在服务重启窗口内不成立）。
  2. 撤回操作无法区分"刚撤回"与"提交后立即撤回"，审计（§9.3 儿童数据操作留痕）缺字段。
  3. withdrawn_by 缺失 → "老师和 Owner 不能替家长撤回"只有代码守卫，无 DB 证据。

### G2（确认）(submitted_by, assignment_id, idempotency_key) 有效唯一约束缺失

- 事实：表上唯一索引仅 `uk_homework_submission_attempt (assignment_id, attempt_no)`。
- 契约要求：幂等重试返回**同一** submission。没有 DB 唯一约束，两个并发同 key 请求
  会各自 MAX+1 得到不同 attempt_no、双插入成功——应用层"先查后插"存在竞态，
  IDEMP-01（不新增 submission/attempt）无法在并发下保证。
- 需要形式（建议）：部分唯一索引
  `UNIQUE (submitted_by, assignment_id, idempotency_key) WHERE idempotency_key IS NOT NULL`
  （可空列 + 部分索引，兼容无幂等键的历史/后台路径）。

### G3（确认）attempt_no MAX+1 分配缺行锁/串行化

- 事实：COMMENT 明言"应用层 MAX+1 分配，UNIQUE 兜底"。UNIQUE 只防撞号（第二个事务
  报错），不防"同 attempt 被两个并发事务争抢后一个失败重试"——失败重试在幂等键缺失
  （G1）时会产生第三次提交。
- 应用层可行方案（G1/G2 修复前）：事务内
  `SELECT ... FOR UPDATE` 对 assignment 行加锁（UPDATE 顺序化）或
  `LOCK TABLE homework_submission IN EXCLUSIVE MODE`（过重，不推荐）。前者可行但每个
  提交都串行在 assignment 行上——班级规模下单 assignment 并发提交极低，性能可接受；
  **但正确性仍依赖 G2 的唯一约束兜底**，纯应用层方案只能降概率不能归零。
- 结论：应用层 FOR UPDATE 是必要补充，不足以替代 G1/G2。

### G4（确认）撤回与发布并发——当前 schema 不足以表达"只能成功一方"

- 场景：监护人撤回（status→withdrawn）与老师发布（teacher_review→published）并发。
  当前防线：`uk_tr_published_per_submission`（一个已发布回评）+ 应用层条件更新。
  缺口：
  1. 撤回守卫"无已发布回评才可撤"需要读 teacher_review 再 UPDATE homework_submission，
     两个语句之间发布可插入——无锁即竞态（撤回了一个已发布的 submission，
     违反状态机 §2"submission.submitted → published 存在 ⇒ 不可 withdrawn"）。
  2. 反向：发布条件更新查 submission.status='submitted' 后、UPDATE teacher_review 前，
     撤回可发生 → 已发布回评挂在 withdrawn submission 上。
- 应用层可行方案：两个操作都先 `SELECT ... FOR UPDATE` 锁 homework_submission 行
  （行锁串行化撤回/发布对），可行且开销可忽略；**但**"已发布回评不可撤回证据"
  目前仅靠行锁纪律，无 DB 约束（如触发器：存在 published review 时禁止
  status→withdrawn）。建议 00000098 加触发器使不变量在 DB 层 fail-closed。

## 新发现（Coordinator 未点名）

### G5 assignment 缺"教学作业开放提交"判定字段的一致性

- `assignment_scope` 返回 `task_status`（group_task.status 为 **integer**，repo 语义
  1=进行中 等，见 group_task_repo），STEP-04 错误码 5442（ERR_ASSIGNMENT_CLOSED）
  需要明确的 int→可提交判定。schema 无缺口，但 Step 9 必须复用既有 group_task
  status 语义（勿发明新枚举）——登记为契约↔实现对齐点，非迁移需求。

### G6 homework_submission 无 updated_at / withdrawn 专用时间语义

- 撤回将复用 status 变更，但表无 `updated_at`（仅 created_at/submitted_at）。
  撤回时间审计依赖 G1 的 withdrawn_at；若 00000098 不加 withdrawn_at，则无任何
  字段可承载"何时撤回"。与 G1 合并处理即可。

## 修复建议汇总（给 DATABASE Agent，00000098 建议）

```sql
-- 建议（示意，最终以 DATABASE Agent 为准）：
ALTER TABLE homework_submission
  ADD COLUMN IF NOT EXISTS idempotency_key varchar(64),
  ADD COLUMN IF NOT EXISTS request_digest varchar(128),
  ADD COLUMN IF NOT EXISTS withdrawn_at timestamptz,
  ADD COLUMN IF NOT EXISTS withdrawn_by bigint REFERENCES "user"(id) ON DELETE SET NULL;

CREATE UNIQUE INDEX IF NOT EXISTS uk_hs_idempotency
  ON homework_submission (submitted_by, assignment_id, idempotency_key)
  WHERE idempotency_key IS NOT NULL;

-- 撤回×发布互斥的 DB 层 fail-closed（触发器）：
-- homework_submission.status: submitted→withdrawn 时若存在 status='published'
-- 的 teacher_review 则 RAISE（ERRCODE 23514）。
```

应用层配套（Step 9 实现，无需迁移）：
- 提交事务：`SELECT ... FOR UPDATE` assignment 行 → MAX(attempt_no)+1 → INSERT
  （ON CONFLICT 目标含幂等唯一索引时捕获重放）→ `SET CONSTRAINTS ALL IMMEDIATE`。
- 撤回/发布事务：先 `SELECT ... FOR UPDATE` homework_submission 行再条件更新。

## 影响面判定

- G1–G4 不修复则 **IDEMP-01 / STATE-01 / FLOW-01 的并发用例无法诚实通过**；
  仅靠应用层 FOR UPDATE 可覆盖单进程正确性，多节点/重启窗口不成立。
- G5/G6 不阻塞 Step 9 开工（对齐语义即可），G1–G4 建议**先修复再开 Step 9**。
