# 进程与监督树健康审查（Phase B）

> 2026-11-23 · 审查范围：监督树结构、restart 策略、worker 生命周期、未受监督 spawn、mailbox 模式、阻塞回调
> 依据教程 Ch.01 Supervision Trees / Ch.02 Processes and Mailboxes / Ch.05 gen_server

---

## 1. 监督树实况

### 1.1 结构（全部 4 个 supervisor 实测）

| Supervisor | 策略 | intensity/period | 子进程 | 备注 |
|---|---|---|---|---|
| `imboy_sup`（src/imboy_sup.erl:404） | one_for_one | **50/60** | 22+ 常驻 worker + 2 子 sup（条件 feature 门控） | 放宽有 CI-00 run10 实测论证（kill 风暴 10 连杀不拖垮全树） |
| `msg_store_sup`（src/ds/msg_store_sup.erl:36-39） | one_for_one | 10/60 | msg_store_ds + msg_store_worker | 均永久 worker |
| `imboy_plugin_sup`（:36-37） | one_for_one | 5/5(默认) | 4 生产插件子树 | |
| `imboy_plugin_generic_sup`（:37-38） | one_for_one | 5/5 | 动态插件 worker | |

**正确项**：MsgWriteQueueSup 与 PluginSup 均 `type => supervisor, shutdown => infinity`（imboy_sup.erl:136-143/298-305）；PluginLoader/MCP 三件套/AiAgentRuntime 用 `transient` 且注释论证精确（"崩溃正常退出不重启" 语义）。

**findings**：

#### PB-1（P2-3）重启风暴观测缺口与 50/60 放宽不配套
- **文件/函数**：`src/imboy_sup.erl:399-404`
- **证据**：注释自述"单子进程崩溃风暴不应带崩整个 IM 服务"；intensity=50/60。
- **机制**（教程 Ch.1 §2）：intensity/period 是爆发量+持续率（50/60 ≈ 每 1.2s 一次重启可永续）。顶层 supervisor 永不因重启超限退出。
- **触发条件**：任一常驻 worker 陷入 crash loop（如 DB schema 漂移导致 msg_burn 每轮必崩）。
- **实际影响**：节点不退出、/readyz 可能仍 ready（readiness 只查 whereis 存在性 + 少量 call，见 imboy_app.erl:209-237），形成"永续僵尸 worker + CPU 空转 + 日志洪泛"的慢性病；外部无信号。
- **复现**：本地起服后对某 worker 循环 `exit(whereis(msg_burn_logic), kill)`——supervisor 每 ~80ms 重启一次（CI-00 实测值），60s 内 50 次不触顶，持续 1 小时无告警。
- **修复建议**：logger handler 计数 supervisor crash/progress 报告 → metrics 导出 → Grafana 重启率告警（如 >5/min 持续 5min）。
- **回归测试**：kill 风暴 eunit 已有（CI-00 run10 形态）可复用为告警阈值验证。
- **工作量**：S（1 个 logger handler + 1 条告警规则）。

#### PB-2（P2-4）start/2 同步迁移的启动脆弱面
- **文件/函数**：`src/imboy_app.erl:65`（`maybe_migrate/0`，:256-273）；`src/imboy_sup.erl:71-72`（sup init 内 `ensure_all_started(pooler)` + `new_pool`）。
- **证据**：`auto_migrate` 默认 true；`imboy_migrate:migrate()` 失败即 raise；注释明确 fail-fast 论证（"半迁移 schema 继续服务会把故障推迟到运行期"）。
- **机制**（教程 Ch.7 §4.4）：start/2 阻塞 = 全应用阻塞；permanent app + `-heart` + 容器编排 = DB 慢则节点 crash-loop。
- **触发条件**：新部署（默认 auto_migrate=true）+ PG 慢启动/网络抖动；或蓝绿 runbook 未执行的环境误开默认值。
- **实际影响**：启动窗口内 DB 不可达 → 节点反复重启；窗口外的运行期不受影响。
- **复现**：`IMBOYENV=local make run` 且 pg_conf 指向不通端口 → 观察 app start 每次在 migrate 处失败。
- **修复建议**（低侵入）：生产 sys.config 明确 `auto_migrate=false` 为交付默认（install.sh 侧固化），保留 dev 默认 true；可选 migrate 超时上限。结构性方案（迁移移入受监督 worker）需属主决策，风险大于收益不建议本轮动。
- **回归测试**：现有 migrate 失败拒启测试已覆盖 fail-fast 分支；补一条"auto_migrate=false 时 DB 不通仍可 boot（迁移延后）"的装配测试。
- **工作量**：S（runbook+默认值）；M（若选结构性方案）。

### 1.2 未受监督 spawn（15 处全量定性）

| 位点 | 定性 |
|---|---|
| `src/lib/attachment_edge_proxy_handler.erl:262` | ✅ watchdog：monitor owner + stop 消息 + 30s deadline kill，教科书式 |
| `src/ds/message_ds.erl:140` | **P3-1**：分块异步投递；`remote_group_batch`（:172-186）全路径 catch + unknown 语义正确；残余=catch 外异常（take_group_chunk/reroute）跳过剩余 targets → 客户端离线补拉兜底，无消息丢失仅延迟。微修=spawn 体套 try→`send_unexecuted_group_targets` |
| `src/ds/group_file_ds.erl:298` | **P3-2**：下载计数 fire-and-forget，无 catch 崩溃仅计数丢失；一行 catch 可消除 |
| `src/lib/elib_async.erl:36`（async/1） | ✅ 无生产调用方（仅测试）；async/2 spawn_monitor+超时 kill；async_retry 内部 catch+日志 |
| `src/mcp/barrel_mcp_registry.erl:288`、`barrel_mcp_protocol.erl:239`、`imboy_hird.erl:137`、`organization_deletion_preflight.erl:147`、`elib_async.erl` 其余 | 抽查均为内部 catch 或一次性通知形态，无状态承载 |

### 1.3 阻塞回调与嵌套 call

- 粗糙超时 `{noreply,_,Timeout}`：仅 1 处（imboy_cache_sync_recover.erl:82，重试节奏正确）✅
- 回调内阻塞等待：0（WS after-0 drain 不阻塞；user_server 慢工作全部走分片 worker）
- 嵌套 `gen_server:call`：15 个"同时定义 handle_call 且调用 gen_server:call"的候选模块，抽查（imboy_cache_sync、user_server、msg_burn_logic、group_event_subscription、barrel_mcp_tasks）均为**跨服务器单向调用**，无 A↔B 互调证据。**遗留**：静态调用图未穷尽（二轮动作，见 checklist §3.1）。

## 2. WS 连接进程生命周期（核心路径）

```
cowboy ranch accept → websocket_handler:init/2（throttle_ws 门 + subprotocol + auth）
  → websocket_init → user_logic:online(Uid,DType,self(),DID)
      ├─ imboy_syn:join（syn 注册：进程死→注册自动消失 = 崩溃自愈）
      └─ user_server:cast_online（异步分片：离线消息检查等慢工作）
  → 每帧 hibernate；CLIENT_ACK 持久化成功才 CONFIRM（websocket_handler.erl:1132-1155）
  → terminate/3：drop_delivery_budget + user_logic:offline
```

- **崩溃路径清理已证实**：vendored cowboy 2.19 `cowboy_websocket.erl:647-648` 在回调崩溃时仍调用 `handler_terminate(..., {crash, Class, Reason})`（`terminate_reason()` 类型 :46-49 含 `{crash,...}`）。**注意**：这与"教程式类推"（crash 不调 terminate）相反，本结论以实际 vendored 源码为准。残余孤儿仅在 `exit(Pid, kill)` 级事件（OOM killer/heart 强杀）产生，imboy_syn.erl:309-310 注释已接受（"孤儿行无害且可被 reset 清理"）→ P3-5。
- **背压**：BE-14 投递预算门（imboy_syn.erl:353-377）以 `message_queue_len` 为真源判定 allow/defer，bulk/direct 双通道预留 headroom，超预算不入队、轻通知引导客户端拉取——符合教程 Ch.5 §4.7"high-volume producers have backpressure"且免记账回路。
- **user_server 分片队列**（user_server.erl:205-227）：`?MAX_SHARD_QUEUE` 有界 + 过载丢弃 ERROR 日志 + current 任务 DOWN 后 replay 重放——Ch.5 §4.1（慢工作出回调）与 Ch.1（重启恢复）双重合规。
- **P3-3**：worker_loop（user_server.erl:246-257）receive 仅 `{run,..}|stop` 两子句无兜底；当前唯一 sender 为 owner gen_server，理论脆弱性成立但无现实入口。微修=加 `_ ->` 日志丢弃子句。

## 3. 手写 receive 卫生（15 文件全查）

| 形态 | 判定 |
|---|---|
| ref/monitor 打标 + deadline（talk_record_model_exec.erl:29-40、barrel_mcp_session.erl:946-1018、group_event_subscription.erl:98-105、user_server stop_workers :184-194） | ✅ 教科书式（教程 Ch.2 §3 "reference trick"） |
| after-0 非阻塞 drain（msg_forward_logic.erl:619-624、group_collab_message_logic.erl:25-29、bot_logic.erl:248-255） | ✅ 同步栅栏用法，不阻塞 |
| Port 收割（attachment_worker_port.erl:260-272） | ✅ exit_status 全形态 + 'EXIT' + Budget 超时 port_close，且注释修正了旧 3-tuple 误匹配坑 |
| httpc 流式 collect（attachment_artifact_reader.erl:166-204） | ✅ stream_next 逐块 + drain 兜底 |
| `receive after 1`（message_ds.erl:429-431） | ✅ 让出调度拍，非同步等待 |

## 4. 消息重复与超时语义

- `remote_group_batch`（message_ds.erl:172-186）：erpc 超时/异常 → `unknown`（不可证未送达）→ 走 `send_unexecuted_group_targets` **重路由而非盲重发**，且注释明确 "Connection loss or timeout cannot prove that admission was never attempted"——教程 Ch.2 §4.5（timeout=未知结果）+ Ch.6 §4.2（ack+幂等）双合规。
- ACK 幂等：`complete_client_ack` 先持久化后停重试；WebRTC 复用 MsgId 场景显式只回 CONFIRM 不写去重缓存（websocket_handler.erl:826-830 注释）——重复投递过滤与信令时序的权衡有明确论证。
- ACK 重试链：`websocket_delivery_guard:retry`（websocket_delivery_guard.erl:124+）携带原 binary、每跳重新鉴权（"retries still carry the original binary, so a later retry must authorize again"）——授权不缓存于重试路径。

## 5. Phase B 结论

监督树与进程纪律整体**显著高于教程基线**：全部"plausible but wrong"形态（4.1-4.7 系列）均未在热路径出现，且多处实现（BE-14 预算门、unknown 语义、分片 replay）展现出对教程同款机制的更深应用。真实缺口集中在**观测配套**（PB-1）与**启动脆弱面**（PB-2），均为小工作量修复，无结构性重构必要。
