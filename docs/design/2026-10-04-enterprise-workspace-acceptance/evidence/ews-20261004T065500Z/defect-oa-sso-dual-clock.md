# 缺陷卡 EWS-BE-D2：OA SSO code 单次消费约束跨双时钟竞态（main 既有）

- 发现：全量 eunit R6 + 2 次单跑确定性失败（`enterprise_internal_foundation_pg_tests:106 sso_code_issue_consume_single_use`）
- 严重级：**P1（企业域 INT-14 OA SSO 错误合同破坏）**
- 机械证明为 main 既有：本 run 候选 `git diff 0be001e3 -- src/ test/` = 0 行；引入点为 main 提交 `baecf5ae`（2026-10-01 fix(oa): enforce code expiry after acquiring lock）。

## 机制（三层证据）

1. **代码**：`enterprise_oa_sso_code_repo:issue_tx` 用 `elib_dt:now()`（应用/VM 时钟）写 `created_at`；`consume_locked`（baecf5ae 起）用 DB `clock_timestamp()` 写 `consumed_at`。
2. **约束**：`ck_eosc_consumed_after_created CHECK (consumed_at IS NULL OR consumed_at >= created_at)` 跨越两个时钟源求值。
3. **SQL 硬证据**（run 库 BEGIN...ROLLBACK 复现，与测试错误逐字吻合）：
   `created_at = clock_timestamp() + interval '20 milliseconds'` 后立即 `UPDATE SET consumed_at = clock_timestamp()` →
   `ERROR: new row for relation "enterprise_oa_sso_code" violates check constraint "ck_eosc_consumed_after_created"`

## 表现

应用时钟领先 DB（毫秒级即可）时，「签发→立即消费」路径（测试形态；生产 OA exchange 紧跟签发同样常见）以 23514 check_violation 崩溃，吞掉类型化错误合同：`consume_tx/3` 应返回 `{error, redirect_mismatch}`，实际返回原始 epgsql 错误。本机 PG 实例崩溃恢复后进入确定性领先窗口——完整解释 R5 绿 / R6+单跑红的"非确定性"。

## 建议修复（交后端 owner）

单一时钟源：`issue_tx` 的 `created_at` 改 DB 端默认/CURRENT_TIMESTAMP；或 `consume_locked` 改 `SET consumed_at = GREATEST(clock_timestamp(), created_at)`（保留过期 CAS 语义）。修复后本测试自然转绿。

## 证据

- evidence/full-eunit/r6.log、/tmp/ews_arb_foundation*.log（归档）
- 上文 SQL 复现输出（13:18:03 会话）
