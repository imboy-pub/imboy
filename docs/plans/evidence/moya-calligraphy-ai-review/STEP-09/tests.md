# STEP-09 证据 — 测试结果明细

## 真库集成测试 teaching_flow_integration_tests（6/6 PASS）

| 用例 | 验收 | 断言链 | 结果 |
|---|---|---|---|
| flow01_test_ | **FLOW-01** | 幂等提交（配方①②）→ AI 草稿 queued 占位 → 老师草稿（status=draft）→ lock-first 发布（配方③：status=published、published_at 非空、reviewer_uid=老师）→ 家长可读唯一已发布回评 → 发布后撤回被互斥拒绝（already_reviewed） | PASS |
| idemp01_test_ | **IDEMP-01** | 同 key 同 digest → 返回同一 submission_id（created=false）；同 key 异 digest → `{error, idempotency_conflict}`（5460 路径）；异 key → attempt=2；submission 总数=2、attachment 关系=4（重放/冲突两次重试零新增）、attempt 序列 [1,2] | PASS |
| state01_publish_without_draft | **STATE-01** | 无草稿发布 → `{error, no_draft}`（5480） | PASS |
| state01_double_publish | **STATE-01** | 发布后重复发布 → `{ok, already_published, _}` 幂等返回；published 行数恒 1（uk_tr_published_per_submission） | PASS |
| state01_withdraw_then_publish | **STATE-01** | 撤回（lock-first）→ withdrawn_by 审计列落库（00000098 CHECK）→ 撤回后发布 `{error, withdrawn}`（5482）→ 重复撤回 `{error, not_submitted}`（5444） | PASS |
| state01_ai_failed_not_blocking | **STATE-01** | AI 草稿置 failed(timeout) 后老师仍可人工发布（D-10 降级） | PASS |

测试路径说明：直连 epgsql（scratch 库），驱动 Repo 的 `_tx` 函数——与生产
`elib_pg:with_tx` 同一代码路径（仅连接来源不同）；每用例 BEGIN/ROLLBACK，
跑后残留检查 = 0 行。

## 回归（全部 ok）

- teaching_acl_tests（13，Step 8）、teaching_auth_logic_tests（8，Step 8）
- 受波及既有：auth_middleware_tests、billing_route_tests、auth_ds_tests

## 覆盖声明

- 已实现并验证：无 AI 人工闭环主链（列表→详情→幂等提交→队列→草稿→发布→
  家长读已发布；withdrawn 即时出队列由队列 SQL 恒过滤 status='submitted' 保证，
  在 state01_withdraw_then_publish 的撤回路径 + queue SQL 双重实现）。
- ACL 拒绝矩阵对 Step 9 端点的复用由 teaching_acl（Step 8 已测）保证；
  Handler 层 HTTP 形态（:id 路由绑定、Idempotency-Key 头、envelope 错误码）
  未做 Cowboy 级测试（薄适配，Step 17 E2E 范围）——如实标注。
