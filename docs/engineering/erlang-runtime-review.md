# IMBoy Erlang Runtime Engineering Audit — 主报告

> 版本 1.0 | 2026-11-23 | 第一轮：只审查，未修改任何业务代码
> 审查依据：[Learning-Erlang-from-Claude](https://github.com/ayarodionov/Learning-Erlang-from-Claude)（15 章，对照 OTP 29.1.1 校验）× IMBoy 实际代码证据 × OTP 官方文档复核
> 状态文件：`erlang-runtime-audit-status.json` · 续接入口：`RESUME.md`

> ⚠️ **R0 勘误（2026-11-23 追加，下文原文保留未改）**：P1-1 的结论方向成立（docker/dev 路径存在 VM 进程悬崖），但机制与数字有误——`websocket_options/0` 的两个 `infinity` 是流级死配置，从未生效；真实机制是 cowboy 在 WS 升级时调用 `ranch:remove_connection/1`，任何 `max_connections` 都约束不了已建立的 WebSocket；`+P 20480` 实际取整为 32768；蓝绿（1048576）与 native-shell（262144/65536）路径的 VM 上限并非"未覆盖"。完整勘误见 `r0-capacity-remediation-design.md` §11；后续工作入口见 `RESUME-R0.md`。

---

## 1. 基线（Phase A 实测）

| 项 | 值 | 证据 |
|---|---|---|
| 仓库/分支/提交 | `imboy` @ `main` = `a5c74219f`（领先 origin/main 56，behind 0） | `git log`/`git rev-list` |
| 未提交修改 | 仅 1 个未跟踪目录 `docs/research/`（既有研究产物，未触碰） | `git status --porcelain` |
| 本地 OTP/ERTS | OTP 29，ERTS 17.0.2（CI 矩阵 OTP 29+28，dialyzer 固定 28） | `erl -noshell -eval`；`.github/workflows/nightly.yml:61` |
| Cowboy / Ranch | 2.19.0 / 2.3.0 | `deps/cowboy/Makefile`、`deps/ranch/Makefile` |
| 代码规模 | src/ 1098 个 .erl；约 700 业务模块（.app modules 列表） | `find src`；`ebin/imboy.app` |
| 测试规模 | test/ 1467 个 .erl；meck 用于 688 文件；CT `*_SUITE` 19 个；PBT 8 文件（test/pbt/） | Phase D 盘点 |
| 既有可观测性 | recon/redbug/observer_cli 在 DEPS；OTel 栈 + Uptrace；Prometheus 风格 /metrics；elib_metric（counter/histogram/gauge ETS） | `Makefile:59-60`；`src/api/metrics_handler.erl` |
| 既有 Bench | `scripts/bench_websocket.sh`（握手延迟分布+保持）、`scripts/cache_*_bench.erl` ×5、`scripts/smoke/` 套件、`imboy_msg_store_batch_seconds` 生产直方图 | Phase F 盘点 |

教程版本对齐说明：教程按 OTP 29.1.1 校验，本项目运行 OTP 28/29，教程涉及的版本敏感行为（OTP 24+ alias、OTP 25+ global 分区处置等）均低于本项目下限，可直接适用。

## 2. 方法

1. 精读全部 15 章（重点 01/02/03/04/05/06/07/08/09/10/13/14），提取各章 Review Checklist 与 "Plausible but wrong" 检索模式。
2. 以 Ch.14 红旗检索表对 src/（排除 *_tests）逐条扫描，全部命中点回到源码逐个定性（真实缺陷 / 潜在风险 / 优化建议 / 暂不适用）。
3. 承重结论核对官方文档（约束 7）：
   - `+P` 默认值 = **1,048,576**（[erl_cmd 官方文档](https://www.erlang.org/doc/apps/erts/erl_cmd.html) 已核）；
   - gen_server `terminate/2` 需 trap_exit 才在 shutdown 信号下运行（[gen_server 官方文档](https://www.erlang.org/doc/apps/stdlib/gen_server.html) 已核：*"If the gen_server process does not trap exits, the gen_server process terminates immediately"*）；
   - Cowboy WS `terminate/3` 在回调崩溃时**会被调用**——依据本项目 vendored `deps/cowboy/src/cowboy_websocket.erl:647-648`（`handler_terminate(State, HandlerState, {crash, Class, Reason})`）及 `terminate_reason()` 类型含 `{crash, ...}`（:46-49）。**教程未覆盖此点，若按教程 Ch.1 §4.1 类推会得出错误结论**——本次审查以实际版本源码为准修正。
4. 第一轮**零业务代码修改**；本目录 10 份文档是唯一产物，均可独立审查/回滚（`git clean docs/engineering/` 即全量撤销）。

## 3. 风险台账（按严重度排序）

完整九要素（文件/函数、证据、机制、触发条件、实际影响、复现方法、修复建议、回归测试、预估工作量）见各专项报告。此处为收敛索引。

### P1（可能造成大范围服务不可用）

| # | 风险 | 证据锚点 | 专项 |
|---|---|---|---|
| P1-1 | **VM 进程/端口上限与"无限连接"组合成容量悬崖**：`config/vm.args` 设 `+P 20480`、`+Q 20480`（OTP 默认 1,048,576 / 65,536），而 WS 监听 `max_connections => infinity`（`src/api/websocket_handler.erl:118-119`）；release 模板实证同值（`_rel/imboy/releases/1.0.0-alpha.75/vm.args` 仅 name/cookie 不同）。连接数逼近 ~2 万时进程表/端口表打满，spawn 全局失败：新连接被拒、supervisor 无法重启子进程、定时器失效——无削峰、无告警、无优雅降级 | perf-§2 | performance |

### P2（明显性能退化 / 恢复失败 / 可观测性缺口）

| # | 风险 | 证据锚点 | 专项 |
|---|---|---|---|
| P2-1 | **无 `erlang:system_monitor`**：long_gc / long_schedule / large_heap / busy_port / long message queue 全无主动哨兵；BEAM 内病理只能靠事后 shell 诊断 | src/ 零调用 | observability |
| P2-2 | **metrics 面缺 BEAM 关键维度**：已有 process_count / memory(total,processes,ets) / ws_connections / http_requests / msg_store batch 直方图；缺 run_queue、scheduler utilization、binary memory、atom_count、MQL top-N、ETS 分表内存、supervisor 重启计数、日志 drop 计数（教程 Ch.9 §4.6/4.7 逐项） | metrics_handler | observability |
| P2-3 | **重启风暴观测缺口与 intensity 50/60 不配套**：顶层 50 次/60s（imboy_sup.erl:404，有 CI-00 实测论证）意味着 worker 陷入 crash loop 时节点"永续僵尸态"而非崩溃退出——设计意图成立，但缺"重启率"指标与告警来兜住该权衡的另一面 | imboy_sup.erl:399-404 | process |
| P2-4 | **`start/2` 同步跑 DB 迁移**（imboy_app.erl:65 `maybe_migrate`，默认 `auto_migrate=true`）：DB 不可达 = 启动失败 = 节点 crash-loop（permanent + -heart + 编排重启）。已有蓝绿开关（`auto_migrate=false`）与 fail-fast 论证注释；风险集中于"新部署默认路径 + DB 慢启动"场景 | imboy_app.erl:256-273 | process |
| P2-5 | **`+S 2` 双调度器**：vm.args 固定 2 个调度器；若部署机核数 >2，CPU 并行度被钳制（消息吞吐上限、GC 并行度）。若为防与 PG/Garage 争核则缺注释论证，且无 scheduler utilization 指标可佐证（与 P2-2 连带） | config/vm.args | performance |
| P2-6 | **每帧 `hibernate` 的 CPU 成本未实测**：WS 每个回调返回均 hibernate（websocket_handler 全部 return 子句）——内存纪律满分（Ch.3 §4.4 解药），但高吞吐连接上每帧 fullsweep GC 是 CPU/内存权衡，无 bench 数据背书（bench_websocket.sh 只测握手+保持，不测消息吞吐） | websocket_handler | performance |

### P3（可维护性 / 理论性 / 暂不适用）

| # | 项 | 定性 |
|---|---|---|
| P3-1 | `message_ds.erl:140` 网关分块裸 spawn：`remote_group_batch` 内部全路径 catch + unknown 语义正确（Ch.2 §4.5 纪律满分），残余风险仅为 catch 范围外异常跳过剩余 targets → 客户端走离线补拉，无消息丢失，仅延迟 | 残余风险 |
| P3-2 | `group_file_ds.erl:298` 下载计数 spawn 无 catch：计数丢失可接受，一行 catch 可消除 | 微修 |
| P3-3 | `user_server` worker_loop（user_server.erl:246-257）无 catch-all 子句：单 sender 前提当前成立，脆弱但理论性 | 理论 |
| P3-4 | `agent_run_pg.erl:103/652/654` 对 DB 值 `binary_to_atom`：DB 枚举有界；同仓其余处已显式拒绝外部输入造原子 | 理论 |
| P3-5 | WS 崩溃孤儿 ETS 行：cowboy 2.19 已实证 terminate-on-crash（见 §2.3），孤儿仅在 kill 级事件产生；imboy_syn 注释已接受该残留 | 已接受 |
| P3-6 | lager + logger 双栈并存：sys.config 以 kernel logger_level=info 统一控级，drop 计数无指标（并入 P2-2） | 观察 |
| P3-7 | 多节点相关（Ch.6 分布式、TLS dist、global 合并策略）：当前单节点 + `inet_dist_use_interface {127,0.0.1}` 锁回环，教程 Ch.6 多数检查项**暂不适用**；未来多节点时按 checklist-§6 重审 | 暂不适用 |

### 强项（审查中确认的正面实践，防止误判为缺陷）

- **WS 层**：每帧 hibernate、dispatch 兜底子句全覆盖、ack 幂等 + "持久化成功才停重试"（websocket_handler.erl:1133 注释）、心跳重校验会话、v2 未知帧回 ERROR 不静默。
- **在线态自愈**：syn 注册制，WS 进程死亡自动摘除；terminate/3 兜底清投递预算行（且 cowboy 2.19 崩溃路径也会调 terminate，已核源码）。
- **背压/预算**：user_server 分片队列有 `?MAX_SHARD_QUEUE` 上限 + 过载丢弃日志（Ch.5 §4.7 合规）；BE-14 投递预算门以 `message_queue_len` 为真源的免记账设计 + bulk/direct 双通道预留。
- **超时纪律**：手写 receive 全部 ref/monitor 打标 + deadline + after-0 drain（talk_record_model_exec、barrel_mcp_session 等教科书式）；`remote_group_batch` 注释明确 "timeout 不能证明未送达"（Ch.2 §4.5）。
- **Binary/原子**：binary 前插零命中；`++` 无循环热点；gpb 生成代码自带 `binary:copy` 切片防御；`binary_to_atom` 全部白名单前置（cs_identity_assertion.erl:133-141、eb_offboarding_app.erl:243 已实证 guard）。
- **ETS 归属**：六张关键表由 application master 显式建表并注释论证（imboy_app.erl:79-93）；DELIVERY_BUDGET_TAB 启动即建（imboy_syn:init ← imboy_app.erl:69）。
- **监督树**：子 supervisor 均正确 `type => supervisor + shutdown => infinity`；PluginLoader/MCP 系 `transient` 语义精确；pooler 池独立于主树的决策有实测论证注释（imboy_sup.erl:65-71）。
- **工程化**：erlang.mk 自动生成完整 applications 列表（Ch.7 §4.1 结构性免疫）；启动期 fail-fast 校验链（drain/探针预算/附件 registry/limiter 迁移）；僵尸监听器拒绝上线（imboy_app.erl:156-158 注释）。

## 4. 与教程各章的总体符合度

| 章 | 符合度 | 主要证据 |
|---|---|---|
| 01 监督树 | 高 | 子树类型/关闭值正确；intensity 放宽有实测论证；缺重启观测（P2-3） |
| 02 进程/邮箱 | 高 | receive 卫生极佳；elib_async:async/1 裸 spawn 无生产调用方（仅测试） |
| 03 Binary/内存 | 高 | 红旗模式零命中；hibernate 全覆盖；81 处 binary:part 为残余观察面 |
| 04 ETS | 高 | owner 决策明确且注释论证；update_counter 原子计数广泛使用；tab2list 仅 12 处小表 |
| 05 gen_server/statem | 高 | 30 个 gen_server 定位清晰；慢工作异步化（user_server 分片）；crude timeout 仅 1 处 |
| 06 分布式 | N/A（单节点） | 回环锁定；未来多节点重审 |
| 07 应用/Release | 高（含 1 个 P2） | applications 自动完整；start/2 重（P2-4）；release 有 prodready_e2e 验证车道 |
| 08 测试 | 高 | PBT/故障注入/CT/真机 E2E 齐备；缺口=投递状态机的状态ful属性测试 |
| 09 可观测性 | 中 | 有 metrics/OTel/sasl；缺 system_monitor、BEAM 维度指标、drop 计数（P2-1/2） |
| 10 调度器/性能 | 中 | bench 仅覆盖连接容量（P2-6）；+S 2 未论证（P2-5） |
| 13 gen_statem 深度 | N/A（仅 2 处 gen_statem） | barrel_mcp_registry / imboy_plugin_lifecycle 逐条过 checklist 无红旗 |
| 14 速查表 | — | 已逐条扫描，见 erlang-review-checklist.md |

## 5. 修复优先级与后续路径（待授权，不在本轮实施）

1. **P1-1**：vm.args 容量对齐（+P/+Q 提升 + cowboy max_connections 设界）→ bench_websocket.sh CONNS 阶梯验证。工作量 S。
2. **P2-1/2/3（同批）**：system_monitor worker + metrics_handler 扩维度 + supervisor 重启计数 → Grafana 面板 + 告警规则。工作量 M。
3. **P2-4**：迁移路径 runbook 明确化（auto_migrate=false 为生产默认）或迁移超时上限。工作量 S。
4. **P2-5/6**：先测后改——scheduler utilization 采样 + 活跃消息吞吐 bench 补齐，再决定 +S 与 hibernate 策略。工作量 M（bench 先行）。
5. P3 微修（catch 两处、worker_loop 兜底）随下次触碰相关模块时捎带。

## 6. 验收口径

以实际发现并验证的工程风险为准（本轮：1×P1、6×P2、7×P3、8 项强项确认），不以文档数量或重构量为完成标准。所有结论均有 file:line 证据可复查；承重 OTP 行为结论已对照官方文档或 vendored 源码复核。
