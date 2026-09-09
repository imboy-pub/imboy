# STEP-08-DB 备注 — 00000098 最终结构与 Agent B（Step 9 Repo）应用层配方

## 产物

- `priv/migrations/00000098_submission_idempotency_withdraw.up.sql`
- `priv/migrations/00000098_submission_idempotency_withdraw.down.sql`
- `test/repo/moya_teaching_migration_tests.erl`（+4 用例：幂等列/部分唯一/CHECK/互斥触发器）

## homework_submission 新增结构

```sql
idempotency_key varchar(128) NULL   -- 客户端幂等键
request_digest  varchar(128) NULL   -- sha256 hex；同 key 不同 digest 由应用层判 5460
withdrawn_at    timestamptz  NULL   -- status=withdrawn 必填
withdrawn_by    bigint      NULL    -- FK→"user"(id) ON DELETE SET NULL；status=withdrawn 必填

uk_homework_submission_idempotency UNIQUE (submitted_by, assignment_id, idempotency_key)
    WHERE idempotency_key IS NOT NULL
ck_homework_submission_withdraw_audit CHECK (
    (status='submitted' AND withdrawn_at IS NULL AND withdrawn_by IS NULL)
    OR (status='withdrawn' AND withdrawn_at IS NOT NULL AND withdrawn_by IS NOT NULL
                                  AND submitted_at IS NOT NULL))
fk_hs_withdrawn_by → "user"(id) ON DELETE SET NULL   -- 与 learner.user_id/submitted_by 注销语义一致
trg_homework_submission_withdraw_guard  -- 语句末新鲜快照：有 published review 禁 withdrawn
trg_teacher_review_publish_guard        -- 语句末新鲜快照：submission withdrawn 禁 publish review
```

## Agent B 可照抄配方（Step 9 Repo/DS 层）

### 1. 幂等提交（重放返回原行 / 同 key 不同载荷判 5460）

```sql
-- 单语句完成"插入或回读"（READ COMMITTED 安全）：
WITH ins AS (
    INSERT INTO homework_submission
        (id, assignment_id, learner_id, submitted_by, attempt_no,
         idempotency_key, request_digest)
    VALUES ($id, $aid, $lid, $uid, $next, $key, $digest)
    ON CONFLICT (submitted_by, assignment_id, idempotency_key)
        WHERE idempotency_key IS NOT NULL      -- 必须带索引谓词，否则推断不出部分唯一索引
    DO NOTHING
    RETURNING id, attempt_no, request_digest
)
SELECT * FROM ins
UNION ALL
SELECT id, attempt_no, request_digest FROM homework_submission
 WHERE submitted_by=$uid AND assignment_id=$aid AND idempotency_key=$key
   AND NOT EXISTS (SELECT 1 FROM ins);
-- Erlang 侧：返回行的 request_digest ≠ 本次 $digest → {error, <<"5460">>} 幂等键冲突；
--            相等 → 返回该行（同一 submission，不新增 attempt）。
```

### 2. attempt 并发取号（FOR UPDATE 串行化点在 assignment 行）

```sql
BEGIN;
SELECT id FROM group_task_assignment WHERE id = $aid FOR UPDATE;   -- ① 先锁：并发在此排队
SELECT COALESCE(MAX(attempt_no),0)+1 FROM homework_submission      -- ② 锁后取号（新鲜快照）
 WHERE assignment_id = $aid;                                       --    应用层用该值插入
INSERT INTO homework_submission (... attempt_no = $next ...)       -- ③ uk_homework_submission_attempt 兜底
COMMIT;
-- 备选：pg_advisory_xact_lock(hashtext('hs_attempt:' || $aid::text)) 锁粒度更小（00000083 先例）
```

### 3. 撤回/发布互斥（**先锁行，再条件更新**——单语句守卫有 READ COMMITTED 快照洞）

```sql
-- 撤回（家长侧）：
BEGIN;
SELECT status FROM homework_submission WHERE id = $sid FOR UPDATE;  -- ① 串行化点
UPDATE homework_submission
   SET status='withdrawn', withdrawn_at=now(), withdrawn_by=$uid
 WHERE id=$sid AND status='submitted'
   AND NOT EXISTS (SELECT 1 FROM teacher_review
                    WHERE submission_id=$sid AND status='published');  -- ② 锁后守卫（0 行=拒绝）
COMMIT;

-- 发布（老师侧，draft→published 一次性条件更新）：
BEGIN;
SELECT status FROM homework_submission WHERE id = $sid FOR UPDATE;  -- ① 同一串行化点
UPDATE teacher_review
   SET status='published', published_at=now(), reviewer_uid=$teacher
 WHERE submission_id=$sid AND status='draft'
   AND (SELECT status FROM homework_submission WHERE id=$sid) <> 'withdrawn';
COMMIT;
-- 0 行更新=重复发布或已撤回：回读已发布行返回幂等结果。
-- DB 兜底：即使忘了 ①，trg_*_guard 双向触发器会以 check_violation(23514) 拒绝非法方。
```

### 4. 错误分类（epgsql/SQLSTATE）

| SQLSTATE | PG 条件名 | 场景 | 应用映射建议 |
|---|---|---|---|
| 23505 | unique_violation | `uk_homework_submission_idempotency` / `uk_homework_submission_attempt` / `uk_tr_published_per_submission` | 回读判定幂等成功或 5460/409 |
| 23514 | check_violation | withdraw_audit CHECK / `trg_*_guard` 互斥（CONSTRAINT 名在消息里） | 409 状态冲突；按 CONSTRAINT 名细分 |
| 23001 | restrict_violation | RESTRICT FK（删有 submission 的 learner / 删被引用 attachment） | 409 资源被引用 |
| 23503 | foreign_key_violation | 普通 FK（如 withdrawn_by 不存在） | 400 参数错误 |

### 5. attachment 表现状提醒

唯一列是 **file_hash256 + path**（md5 已被历史迁移移除）；造数/关联 submission_asset 时两者都要给不同值。业务表只存 attachment_id，禁止持久化 presigned URL。

## 已知交互（审计 fail-closed）

- 已撤回 submission 的 withdrawn_by 用户被物理删除时：FK SET NULL 会违反 withdraw_audit CHECK → 用户删除事务失败。这是**故意的审计 fail-closed**（撤回审计完整性优先）；user_deletion 流程需先匿名化/改派（同 00000097 teacher_review.reviewer_uid 的同类交互）。建议 Step 16 绑定/解绑设计时统一处理"审计用户匿名化"策略（如 sentinel uid 0 而非 NULL）。

## 环境收尾

- 并发测试数据已清理（step8db_conc_cleanup.sql），moya_mig_test 回到 00000098 全量干净态，可继续作为集成测试基线。
- 历史迁移 00000001..00000094 未动；00000095-97 未动（00000098 是独立新增修复迁移）。
