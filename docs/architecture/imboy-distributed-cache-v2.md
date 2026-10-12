# IMBoy 分布式缓存 V2（fenced cache sync）架构文档

> w0 计划 §15.3「长期保留文档」要求的架构篇。描述**当前已实现并经
> CACHE_V221_BACKEND_CLOSEOUT_V1 收尾验证**的 V2 缓存同步子系统。
> 权威协议规格：`docs/architecture/cache-v2.2.1/closeout-v1/protocol-budget-frozen.md`；
> 收尾结论与台账：同目录 `closeout-report-v1.md` / `ledger-v2.json`。
> 最终候选：`f1aa1ce016eaa6e4db18e52e6ee6fba51cdb745303c057aaeff650c9b990020c`
> （= sha256(git archive 2873dbfd)，产品代码零漂移）。

---

## 1. 定位与设计约束

- **不引入新中间件**：无 Redis、无 Mnesia；保留 depcache 做本地缓存，复用 syn 3.4.2
  （pg scope `imboy_cache`，group `dsync_handler`）做成员管理与消息广播。
- **渐进启用**：V2 行为仅对 `cache_fenced_namespaces` 显式列出的 namespace 生效；
  默认空表 = 全部走 legacy depcache 行为，QR 客户端 wire contract 不受影响。
- **失败封闭（fail-closed）**：replicate 复制授权要求「注册表登记 `{replicate, false}`」
  **且**「生产白名单命中」两个条件同时成立；未登记键一律拒绝（not_authorized）。
- **分代栅栏**：所有跨节点 SET/DELETE/失效以 revision（seq+boot id incarnation）定序，
  旧化身重放与跳号均有确定语义。

## 2. 模块图

| 模块 | 职责 |
|---|---|
| `src/lib/imboy_cache.erl` | depcache 门面（源自 z_depcache）；`put/4`、`put_delete/1`、`invalidate/1` 对上提供 local/replicate/invalidate 三种行为；同步本身委托独立 gen_server |
| `src/lib/imboy_cache_policy.erl` | namespace 判定、fenced 开关（`cache_fenced_namespaces`）、replicate 授权（`replicate_allowed/1`）、预算（`b_max/p_max/p_max_bytes`）、config_hash 输入 |
| `src/lib/imboy_cache_sync.erl` | 同步核心 gen_server：revision 状态机、r1/r2 版本握手、缺口补偿、恢复（`RECOVER_ROUND_MS=5000`、`RECOVER_MAX_RETRY=3`、对账间隔 60s、时钟偏移容限 5s） |
| `src/lib/imboy_cache_sync_proto.erl` | 协议帧编解码与纯函数校验：envelope（epoch、config_hash/registry_hash 各 16B）、`check_incarnation`（首见旧化身返回 changed 触发补偿）、`version_request_frame` |
| `src/lib/imboy_cache_sync_snapshot.erl` | C04 快照轮：Round 令牌、分页发送（每页 ≤Pmax 项且 ≤Pmax_bytes）、页摘要 phash2、接收端 Bmax 缓冲、闭环判定（全对端 done + 无 expected_more + 截止期内） |

共享 ETS：`imboy_cache_meta`（世代、peer counter、恢复状态）。

## 3. 关键机制

### 3.1 版本握手（r1 → r2）

- **r2（现行）**：`version_request_frame(FromPid)` 发 3 元组
  `{cache_version_request, FromPid, FromNode}`，对端**直连回** 9 元组 version_report
  （携带 sender pid）；请求方据此 pid 直发 `{cache_snapshot_request, self(), Round}`，
  消除 syn 视图滞后导致的恢复饥饿。
- **r1 兼容**：2 元组旧帧仍被接受，经 syn 视图查找回信；peer counter 在两条路径都会
  持久化（`{peer_counter, FromNode}` → meta ETS），供冷启动水位恢复。
- **incarnation**：节点重启产生新 boot id；对端首见旧化身事件按 **changed** 处理
  （补偿信号），不再静默忽略。

### 3.2 缺口补偿与 fenced 世代

seq 跳号（如 1→3）触发缺口检测；对 `cache_fenced_namespaces` 内的键，接收端立即
推进本地 generation（`imboy_cache_sync:generation/1` 可观测），保证读侧栅栏不放大
缺口窗口。

### 3.3 快照（C04）

- 预算：`p_max=256` 项/页、`p_max_bytes=4 MiB`/页、接收缓冲 `b_max=2048` 项
  （`imboy_cache_policy` 可覆写）。
- 闭环 = 全部对端 done **且** 无 expected_more **且** 截止期内；任一页失序/摘要不符/
  缓冲溢出 → 本轮 abort（计数 `snapshot_abort_total`，见告警）。
- `take_page` 保证每页真实攒批（页满或发完才切页），杜绝单条页导致的轮次饥饿。

### 3.4 授权与配置指纹

- 注册表：基线登记 + `cache_registry_overrides`（`#{Key => {replicate, false}}`）。
- `replicate_allowed(Key)` = 注册表 `{replicate, false}` ∧ `whitelist()` 命中 namespace。
- config_hash = phash2(`budget_digest()` + whitelist + fenced_namespaces)；集群间
  config_hash/registry_hash 不一致会拒帧并计数（`ImBoyCacheSyncRejected` 观测点）。

## 4. 可观测性

`/metrics`（`src/imboy_router.erl`，仅内网，nginx 对外 403）暴露：

| 指标 | 语义 |
|---|---|
| `imboy_cache_sync_status` | 2=ok / 1=recovering / 0=degraded / -1=进程不存在 |
| `imboy_cache_sync_epoch` | 当前 epoch |
| `imboy_cache_sync_queue_depth` | 队列深度（-1=进程不存在） |
| `imboy_cache_sync_peers` | syn 成员数（`syn:members(imboy_cache, dsync_handler)`） |
| `imboy_cache_sync_counter` | 本节点同步计数 |
| `imboy_cache_sync_peer_counter{peer}` | 每对端已见计数（水位） |

Prometheus 告警（`deploy/prometheus/rules/imboy-alerts.yml`）：
`ImBoyCacheSyncRejected`、`ImBoyCacheSyncUnknownEvents`、`ImBoyCacheSyncStuckRecovering`
（status==1 持续 5m，critical）、`ImBoyCacheSyncDegraded`（status<=0，critical）、
`ImBoyCacheSyncSnapshotAborting`（10m 内 abort>2，warning）。
Grafana 面板：`deploy/grafana/dashboards/imboy-cache-sync.json`。

## 5. 配置键参考

| 键 | 默认 | 说明 |
|---|---|---|
| `dsync_enabled` | false | 同步 gen_server 总开关 |
| `cache_fenced_namespaces` | `[]` | 启用 V2 fenced 行为的 namespace；空=全 legacy |
| `cache_registry_overrides` | `#{}` | 键级注册表覆写（`{replicate, false}` 才可复制） |
| `cache_replication` | `#{whitelist => []}` | 生产复制白名单；**默认空=不复制** |
| `b_max` / `p_max` / `p_max_bytes` | 2048 / 256 / 4 MiB | 快照预算（经 `rep_cfg` 覆写） |

## 6. 测试与验证工具

| 工具 | 用途 |
|---|---|
| `test/lib/imboy_cache_sync_tests.erl` | 29 个 EUnit（含 C04 闭环/缺页/溢出 abort、C05 缺口补偿、授权栅栏） |
| `scripts/cache_v21_dcheck.erl` | 三节点故障注入 dcheck（C9a–f；`main(["<out.json>"])`，`DCHECK_ONLY=<子串>` 单场景） |
| `scripts/cache_v21_mixed_dcheck.erl` | 混合故障轮（M7–M10：摘要不符/epoch 漂移/旧化身重放/r2 帧） |
| `scripts/cache_v2_bench.erl` | V2 全链路压测（`CACHE_V2_OUTPUT`/`CACHE_V2_OPERATIONS`/`CANDIDATE_SHA256` 环境变量约定） |
| `scripts/check_cache_closeout_v1.py` | 证据机械复算校验器（只读；`--self-test` 自证 6 负例全拒） |

## 7. 文档地图

- 协议与预算（权威、冻结）：`closeout-v1/protocol-budget-frozen.md`
- 运维手册（告警处置/诊断/回滚入口）：`docs/guides/operations/cache-cluster.md`
- 收尾报告与三结论：`closeout-v1/closeout-report-v1.md`
- 验收台账（96 项）：`closeout-v1/ledger-v2.json`
- 回滚步骤：`closeout-v1/rollback-v1.md`（注意：G-QR 单特性回滚须**先**执行 down 迁移再回退代码）
- 原计划冻结快照：`cache-v2.2.1/w0/plan-v2.2.1.md`（SHA256 见任务档案）。其 §15.3
  所列 `docs/plans/2026-10-10-distributed-cache-v2.2.md` 即本快照的原工作路径
  （`docs/plans/` 为仓内刻意不跟踪目录，见 .gitignore:163），内容即该快照本身。
