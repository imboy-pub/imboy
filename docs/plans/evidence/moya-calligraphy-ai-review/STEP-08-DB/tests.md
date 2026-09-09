# STEP-08-DB 证据 — 行为测试明细（00000098）

## 基础约束测试（step8db_behavior_test.sql，BEGIN..ROLLBACK）

| # | 用例 | 期望 | 结果 |
|---|---|---|---|
| T1 | 同 key 同载荷重放：`ON CONFLICT (submitted_by,assignment_id,idempotency_key) WHERE idempotency_key IS NOT NULL DO NOTHING` 后回读 | 仅 1 行且返回原行 id（不新增 attempt） | PASS |
| T2a | 同 key 不同载荷裸 INSERT | unique_violation 且约束名 `uk_homework_submission_idempotency` | PASS |
| T2b | 应用层配方：DO NOTHING 后回读 digest 比对，不同则 raise | `IDEMPOTENCY_CONFLICT_5460` 路径触发（自捕获断言） | PASS |
| T3a-d | withdrawn 缺 withdrawn_at / 缺 withdrawn_by / submitted 态携带 withdrawn_at / 携带 withdrawn_by | 全部 check_violation（ck_homework_submission_withdraw_audit） | PASS |
| T4 | 撤回正例：submitted→withdrawn 全审计字段（条件 UPDATE 含 NOT EXISTS published 守卫） | 生效，status=withdrawn | PASS |
| T5 | withdrawn_by=888888（不存在用户） | foreign_key_violation（fk_hs_withdrawn_by） | PASS |
| T6 | 无幂等键两行（idempotency_key NULL） | 不受部分唯一索引约束，均可插入 | PASS |

## 并发测试（两个独立 psql 会话，真交错）

### T6' 并发取号（attempt 分配）
配方：`BEGIN; SELECT id FROM group_task_assignment WHERE id=$aid FOR UPDATE; INSERT ... attempt_no=(SELECT COALESCE(MAX(attempt_no),0)+1 ...); COMMIT;`
- 会话1 先持锁，会话2 延迟 0.4s 启动 → 阻塞在 FOR UPDATE → 依次取号
- 结果：s1_exit=0 s2_exit=0；新增两行 attempt_no=2,3 互不重复，无死锁；`uk_homework_submission_attempt` 兜底未触发 PASS

### T7 撤回/发布互斥（**发现并修复了一个真实竞态**）

| 场景 | 交错 | 结果 |
|---|---|---|
| A 撤回先赢 | 撤回持 submission 行锁 2s；发布 0.4s 后启动 | 发布 DO 块 FOR UPDATE 后见 withdrawn → `PUBLISH_BLOCKED` 事务失败（exit 3）；终态 withdrawn+draft PASS |
| B 发布先赢 + naive 撤回 | 发布持 FOR UPDATE 锁 2s 后 COMMIT；naive 单语句撤回 0.4s 后启动 | **初版无 backstop 时：撤回穿透成功，出现 withdrawn+published 非法并存（竞态实证）**；加触发器后：撤回被 `trg_homework_submission_withdraw_guard` 拦截（check_violation「撤回互斥」，exit 3）；终态 submitted+published PASS |
| C 发布先赢 + lock-first 撤回 | 同 B 交错，撤回先 `SELECT ... FOR UPDATE` 再条件 UPDATE | 锁等待后 NOT EXISTS 用新鲜快照 → `UPDATE 0` 干净拒绝（exit 0 无异常）；终态 submitted+published、published_at 非空 PASS |

### 竞态根因与修复（重要，给 Step 9 的教训）

- **根因**：发布事务只 `FOR UPDATE` 锁 submission 行而未修改该行时，撤回方的单语句守卫 `UPDATE ... WHERE status='submitted' AND NOT EXISTS (published review)` 在 READ COMMITTED 下按**语句起始快照**评估子查询——看不到发布事务随后 COMMIT 的 published review → 守卫穿透。
- **修复（DB 层，00000098 内）**：双向 backstop 约束触发器（`DEFERRABLE INITIALLY IMMEDIATE`，语句结束时以**新鲜语句快照**复查）：
  - `trg_homework_submission_withdraw_guard`：withdrawn 前查 published review，存在即拒（ERRCODE 23514）；
  - `trg_teacher_review_publish_guard`：published 前查 submission 状态，withdrawn 即拒。
- **应用层正确配方**：任何"条件守卫"必须**先 `SELECT ... FOR UPDATE` 锁行，再发条件 UPDATE**（两条语句），保证第二条语句的快照在锁获得之后取得。触发器是无视应用配方的兜底，两层都要。

## 验收对照（R2 任务书验证项）

1. 空库链 up/down/up：PASS（叠加在 00000001→97 全量态上）
2. 同 key 同载荷重放返回原行：PASS（T1）；同 key 不同载荷冲突拒绝（5460 语义）：PASS（T2a/T2b）
3. 并发不同 key 同 assignment 两 attempt 各自成功不重复：PASS（T6'，含 FOR UPDATE 取号配方）
4. 撤回与发布并发一方失败：PASS（T7 A/B/C 三场景；B 场景含竞态发现→修复闭环实证）
5. make compile 0：PASS；eunit 64/64：PASS；既有 group_task 套件无回归：PASS
