# IMBoy T-02 Retention and Backup Enforcement Checklist

> 实施计划 Task T-02。前置：T-01 保留策略注册表（已完成）。
> 状态：**首批已实施（2026-09-07）**——DB 凭证清理执行器 + 备份恢复删除重放 runbook。

## Goal（计划原文要点）

证明 DB/Garage/Loki/备份四域的到期执行：bounded batches、locks、metrics、dry-run、retry、备份/vendor 传播墓碑；无静默吞错。验收=证据记录行数与失败（无 PII）、过期未删有告警、备份恢复 runbook 含删除重放。

## 本批交付（narrow scope：sessions_tokens 域 + 备份域）

### 1. 过期凭证清理执行器（DB 域）

- **新 worker** `src/lib/credential_retention_worker.erl`（gen_server，挂 imboy_sup，对齐 olm_otk_cleanup_worker 模式）：
  - **默认禁用**（`credential_retention_enabled=false`），运维 sys.config 显式启用；
  - **bounded batches**：单轮按 LIMIT(默认 5000) 分批删，批间循环直到不足一批；
  - **dry-run**：`credential_retention_dry_run=true` 只统计不删除（count_expired）；
  - 配置：interval（默认每日）、verification_code_days（过期宽限默认 1 天）；
  - 日志只记行数（无 PII）；单轮失败 WARN 不 crash，tick 自动重排；
  - 多节点安全：删除按 age 幂等，行锁串行化，重复扫描无副作用。
- **清理对象**：`verification_code`（id=手机号/邮箱，**PII**；validity_at 过期；NULL 行按 created_at 起算）——T-01 注册表 sessions_tokens 类 event-driven→delete 口径的执行层。
- **DS/Repo 接口**：`verification_code_ds/repo:purge_expired/2`（分批 DELETE…IN(SELECT…LIMIT)、count_expired/1（dry-run 统计））。
- **fake clock 测试**：cutoff 换算用 `elib_dt:millisecond()` 注入（worker 测试 mock 固定毫秒并回转断言 3 天窗口），days/limit/dry-run 四组配置矩阵 + 失败不崩 + 未知消息，**7/7**。

### 2. 备份恢复删除重放 runbook（备份域）

`docs/guides/operations/deployment/BACKUP-RESTORE.md` 新增「恢复后删除重放（Deletion Replay）— 合规必做」：

- 原理：T0 备份恢复会复活 T0 后删除的账号——墓碑表 `user_deletion_job`（D-03，UNIQUE user_id 幸存设计）驱动重放；
- 操作：对账 SQL（completed 且 finished_at > 恢复点 但 user 行存在）→ 重置 pending（SQL 给定）→ worker SKIP LOCKED 自动认领重放（或 `user_deletion_logic:cleanup_now()` 手动触发）→ 复查对账 0 行；
- 演练验收：季度演练第 4 步固定为「删除重放验证」，报告存档对账前后对比；
- 边界：库与备份同失（墓碑丢失）时以商店/客服删除请求外部记录为准人工补建——`user_deletion_job` 全量行 retain 不可时间清理。

### 3. 其余两域现状（不重复建设的部分）

- **Loki**：`retention_enabled: true` + 180d（T-01 已实证一致），compactor 自动删除，无需自建 job；
- **Garage**：账号删除的附件删除入队已在 D-03 执行器（collect_attachment_keys_tx）；孤儿对象生命周期列为后续（attachment GC 独立小批）；
- **metrics 埋点**：worker 预留 TODO（删除行数/耗时/失败计数/末次成功时间），对齐 olm_otk 模板的同款预留——本批日志计数即可支撑证据，elib_metric 接线下批统一做。

## 测试证据（2026-09-07）

| 门 | 结果 |
|---|---|
| credential_retention_worker_tests（新） | **7/7**：disabled 门 / 短批单轮 / 满批循环 3 批 / dry-run 只统计不删 / 失败不崩 / 未知消息 / fake clock cutoff 3 天窗口回转 |
| 既有回归 | imboy_sup 编译绿；ERLC_EXCLUDE=agent_task_repo（并行会话 WIP） |

## 已知边界与后续

- worker 默认禁用：**上线需运维在 sys.config 显式启用**（credential_retention_enabled=true）；
- login_attempt（爆破防护记录）清理未做——属 audit_security_moderation 类，等 owner 翻转 T-01 口径后同款 worker 复用（或并入本 worker 增一个表步骤）；
- Garage 孤儿对象 GC、metrics 接线 elib_metric、vendor 传播墓碑——后续批次。
