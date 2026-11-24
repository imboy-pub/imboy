# R0 容量治理设计（VM 上限 × WebSocket 连接上限）— 评审稿

> 版本 1.1（实施稿）| 2026-11-24 | 评审门已过：属主批准 D0–D8（附录 §13），阶段 E–G 实施中
> 任务：P1-1 修复 / IMBoy Runtime Risk Remediation R0
> 状态文件：`r0-capacity-status.json` · 续接入口：`RESUME-R0.md` · 检查点：`r0-capacity-checkpoint.md`
> 分支/worktree：`fix/r0-capacity-limits-20261123` @ `_task_worktrees/R0_CAPACITY_20261123/imboy`（基于 main `a5c74219f`）

---

## 0. 先读这一节：任务书的一处前提不成立

任务书 C3 的思路是“把 `max_connections => infinity` 改成有界值”。阶段 B 的源码核验与隔离实验表明这条路**单独做无效**：

1. `websocket_options/0` 里的 `num_acceptors => infinity, max_connections => infinity`（`src/api/websocket_handler.erl:118-119`）位于 **websocket 流级选项**（`{cowboy_websocket, Req, State, Opts}` 的第 4 元素，见 `src/ds/websocket_ds.erl:240`）。ranch 2.3 只从 **TransportOpts**（`cowboy:start_clear/3` 第 2 参数）读取这两个键（`deps/ranch/src/ranch_listener_sup.erl:24`、`ranch_acceptors_sup.erl:31`），cowboy 源码中不存在这两个键。它们从未生效；若真被放进 TransportOpts，`num_acceptors => infinity` 会让监听器以 `{bad_option, num_acceptors}` 启动失败（`deps/ranch/src/ranch.erl:153-154`）。
2. **即使设置了有界的 `max_connections`，也约束不了已建立的 WebSocket**：cowboy 在每条 WS 升级时主动调用 `ranch:remove_connection(Ref)`（`deps/cowboy/src/cowboy_websocket.erl:325-332`，源码留有 `@todo We should have an option to disable this behavior`）。ranch 文档对该 API 的说明是“该连接不再计入连接数限制……适用于空闲的被动连接”。
3. 实验证据（隔离裸 cowboy 2.19.0 / ranch 2.3.0 / OTP 29.0.2，复用主检出已编译 beam，仅回环）：
   - 名义上限 `max_connections=100 × num_conns_sups=3 = 300`，尝试 380 条 → **380 条全部建立**；
   - imboy 真实形态（隐式 10×1024=10,240）全尺度 → **16,201 条建立**（超名义上限 58%）。

**结论**：`max_connections` 只能约束 **HTTP 阶段**（握手中的连接 + 普通 API 请求）。要给 WebSocket 设上限，必须在**应用层**做准入（见 §5）。这超出“只改配置”的预期，属于需要属主明确批准的范围调整（决策点 D0）。

## 1. 当前配置证据（阶段 B1/B3 实测）

### 1.1 VM 参数的 5 个来源、3 套数值

| # | 部署入口 | 证据（文件:行） | `+P` 请求值 → 实际 | `+Q` 请求值 → 实际 | `+S` | 其他 |
|---|---|---|---|---|---|---|
| E1 | Docker 镜像（社区版/商务版 compose、demo） | `Dockerfile:66` `make rel` → `relx.config:32` `{vm_args, "config/vm.args"}` → `config/vm.args:10-12` | 20480 → **32768** | 20480 → **32768** | 2 | `+K true +A 1024 +MSe true` |
| E2 | 蓝绿部署（`imboy-deploy.sh api`） | `scripts/lib/blue_green_deploy.sh:1286-1304` 安装期整体重写 release 内 vm.args | 1048576 | 1048576 | 4 | `+A 256 +MSe true +sbwt/+sbwtdcpu/+sbwtdio none +swt very_low +stbt db +zdbbl 81920` |
| E3 | native-shell 原生交付包 | `deploy/native-shell/lib/render.sh:232-239` → `/etc/imboy/vm.args`（systemd `VMARGS_PATH`） | 262144 | 65536 | 缺省（核数 / CPU quota） | `+K true +A 16` |
| E4 | 本地 `make run` | `scripts/start_node.sh:82-93` | 20480 → 32768 | 20480 → 32768 | 2 | `+MSe true` |
| E5 | dev_mode release（prodready_e2e 等） | `relxlocal.config:8` / `relxdev.config:8` → `config/vm_local.args:7-9` | 20480 → 32768 | 20480 → 32768 | 2 | `+MSe true` |
| — | compose 环境变量 | `deploy/docker-compose.prod.yml:195`、`community.yml:226` `ERL_MAX_PORTS: "1048576"`；`demo.yml` `"262144"` | — | **无效**（被 E1 的 `+Q` 覆盖） | — | 见 §1.3 |

“请求值 → 实际”：OTP 会把 `+P`/`+Q` **向上取整到 2 的幂**（实测 20480→32768、1025→2048、5000→8192；官方文档原文：“The actual maximum chosen may be much larger than the Number passed … often, but not always, chooses a value that is a power of 2”）。

### 1.2 连接层事实

| 事实 | 值 | 证据 |
|---|---|---|
| imboy 监听器有效 TransportOpts | 仅 `connection_type => supervisor`；`max_connections`/`num_acceptors`/`num_conns_sups` 全为 ranch 缺省 | 实验启动日志；`src/imboy_app.erl:456-466` 只传 `[{port, Port}]` |
| HTTP 阶段隐式上限 | 10 conns_sup × 1024 = **10,240 / listener** | `ranch.erl:136-138`、`ranch_conns_sup_sup.erl:34-35`；文档 “Maximum number of active connections per connection supervisor. Soft limit.” |
| 已建立 WS 是否计入 | **否** | `cowboy_websocket.erl:331`；实验 |
| 每条已建立 WS | **1 进程 + 1 端口**，约 **9 KB** | 380 条 → `cowboy_clear:381`、`port_count 383`；16,201 条 → 16301 进程 / 16204 端口 / 193 MB（空闲 36–50 MB） |
| 握手期间 | 额外 1 个瞬态 request process（`cowboy_stream_h`） | `cowboy_http.erl:1399-1428` 协议接管 |
| 裸 cowboy 空闲基线 | 84–100 进程 / 3 端口 | 实验各轮 stats0（真实 imboy 基线待阶段 F 实测） |

### 1.3 覆盖机制实测（OTP 29.0.2）

| 组合 | 生效值 | 含义 |
|---|---|---|
| `args_file(+P 5000,+Q 4096)` + `ERL_FLAGS="+Q 8192 +P 16384"` | 16384 / 8192 | **`ERL_FLAGS` 覆盖 args_file（vm.args）** |
| 同 args_file + `ERL_AFLAGS=…` | 8192 / 4096 | `ERL_AFLAGS` 被 args_file 覆盖 |
| `ERL_MAX_PORTS=777` | port_limit 1024 | 是仿真器层**未文档化**默认值（`erts/emulator/beam/erl_init.c:1335`；erl_cmd 文档中不存在），优先级最低 |
| `ERL_MAX_PORTS=777` + args_file `+Q 4096` | 4096 | 显式 `+Q` 覆盖之 → compose 的 `ERL_MAX_PORTS` 是死配置 |
| `ERL_MAX_PROCESSES=5000` | 无效 | 该变量不存在 → `IMBOY_MAX_PROCESSES` 必须经真实标志注入 |
| `VMARGS_PATH` 指向缺失文件 | 启动失败（exit 1） | 启动脚本要求恰好一个 `-name`，fail-fast |

### 1.4 `+P` 打满时的真实形态（缩尺实验 `+P 1024`，基线 84）

| 阶段 | 现象 |
|---|---|
| 突发 1000 条 | 938 条建立，62 条被拒，`process_count` 触顶 |
| 滴灌 100 条（模拟自然增长） | 只再挤进 2 条，其余 98 条收到 **HTTP 500**（非 101） |
| 满表期间 | **普通 HTTP API 20/20 全部 500**；已建立 WS ping/pong 20/20 正常 |
| 日志 | 163 次失败 → 2,378 行 `Too many processes` / `error:system_limit` 错误报告 |
| 拆除后 | 进程数回落、API 恢复（无泄漏） |

即：撞线时受害的不只是新连接——**所有 REST API（含登录）一起 500**，且无显式拒绝信号。

### 1.5 相邻缺陷（与 R0 同路径，阶段 B 新发现）

- **WS 连接数指标失真**：`imboy_ws_connections_current` 取 `ranch:info(imboy_listener).active_connections`（`src/api/metrics_handler.erl:128-136`），该值不含任何已建立的 WS → 面板“WebSocket 在线连接”结构性接近 0，告警 `ImBoyWSConnectionsDrop`（`deploy/prometheus/rules/imboy-alerts.yml:289-297`，`> 10` 才触发）不可能按设计触发。
- **docker 路径 cookie 为仓库占位值**：镜像内 vm.args 的 `-setcookie imboy_local_dev_only`（`config/vm.args:5`，注释自称“生产构建必须用随机值覆盖”，但 `Dockerfile` 未覆盖；`deploy/install.sh:605-612` 要求 `IMBOY_CTL_COOKIE` 与之“一致”）。分发接口已绑回环，暴露面有限；属 W6-A02 范畴，**不在 R0 处理**，仅上报。

## 2. 生产核验状态

| 级别 | 状态 | 说明 |
|---|---|---|
| REPOSITORY_CONFIRMED | ✅ 全部 5 个来源 | 表格 1.1 均为 tracked 文件静态追踪 |
| RELEASE_CONFIRMED | ⚠️ 部分 | 本地 release 产物 `_rel/imboy/releases/1.0.0-alpha.75/vm.args` 与 `config/vm_local.args` 逐字一致，证实 relx 原样打包；但该产物是 dev-mode 旧构建，**Docker 镜像产物未在本机检视**（本机无 backend 镜像）→ E1 的镜像级确认安排在阶段 G（构建镜像后核对 `/opt/imboy/releases/*/vm.args`） |
| PRODUCTION_CONFIRMED | ❌ | 未获得授权的生产只读诊断入口；本任务未尝试连接生产 |
| **PRODUCTION_NOT_VERIFIED** | **当前结论** | 不得把仓库配置等同于生产实际配置 |

请属主在生产节点执行以下只读命令并回填（任一即可）：

```bash
# 宿主机：实际 release 的 vm.args、fd 上限、核数、CPU quota
grep -E '^\+' /path/to/releases/*/vm.args
cat /proc/$(pgrep -f beam.smp | head -1)/limits | grep -E 'open files|processes'
nproc; cat /sys/fs/cgroup/cpu.max 2>/dev/null
```

```erlang
%% bin/imboy eval（只读）
io:format("~0p~n", [[
  {process_limit, erlang:system_info(process_limit)},
  {port_limit, erlang:system_info(port_limit)},
  {schedulers_online, erlang:system_info(schedulers_online)},
  {dirty_cpu, erlang:system_info(dirty_cpu_schedulers_online)},
  {dirty_io, erlang:system_info(dirty_io_schedulers_online)},
  {process_count, erlang:system_info(process_count)},
  {port_count, erlang:system_info(port_count)},
  {memory_total, erlang:memory(total)},
  {ranch_max_connections, ranch:get_max_connections(imboy_listener)},
  {ws_connections_syn, imboy_syn:count()},
  {erl_flags, os:getenv("ERL_FLAGS")}]]).
```

## 3. Process / Port 资源模型（B3）

### 3.1 每连接占用（实测）

| 对象 | 进程 | 端口 | 说明 |
|---|---|---|---|
| 已建立 WebSocket | 1 | 1 | 连接进程原地接管 WS 循环（`cowboy_websocket:takeover/7`） |
| HTTP 阶段连接（握手中 / API 请求） | ≤2（连接进程 + request process） | 1 | 请求结束后释放 |
| TLS 监听器上的连接 | 同上 | 同上（同一 socket 端口） | 未单独实测握手 CPU |

### 3.2 其余占用

| 类别 | 进程 | 端口 | 依据 |
|---|---|---|---|
| acceptor | `num_acceptors`（缺省 10）/ listener | — | `ranch_acceptors_sup.erl:31` |
| conns_sup | `num_conns_sups`（缺省 = num_acceptors = 10）/ listener | — | `ranch_conns_sup_sup.erl:34-35` |
| OTP 系统进程 + 监督树常驻 worker + 库进程 | 裸 cowboy 实测 84–100；真实 imboy 估计 300–800 | 裸 3；真实估计 30–100（DB 池、日志文件、dist） | 真实值阶段 F 开测第一步实测并回填 |
| 定时任务 / 动态 worker | 定时器不占进程；`user_server` 分片 worker、`elib_async` 等瞬态进程 | — | 第一轮审计 Phase B |
| 故障恢复 | 顶层 supervisor 50 次重启/60s；重启瞬间新旧进程并存 | — | `src/imboy_sup.erl:404` |

### 3.3 预算公式

```
P_need = B_p + W×1 + N_L×H×2          P_limit ≥ next_pow2(1.25 × P_need)
Q_need = B_q + W×1 + N_L×H×1          Q_limit ≥ next_pow2(1.25 × Q_need)
约束：W（含准入超调 ≤ N_L×H）必须先于 P_limit / Q_limit 触顶
```

- `W` = WS 准入上限；`H` = 每 listener 的 HTTP 阶段上限（= max_connections × num_conns_sups）；`N_L` = 共享 VM 预算的 listener 数（http/tls 模式 1；http_tls 模式 2）
- `B_p = 4096`、`B_q = 4096`：非 WS 业务预留（≥ 真实基线估计值的 5 倍）
- `×1.25`：故障恢复余量

**为什么不能把 `+P`/`+Q` 当作最大 WS 数**：每个 VM 进程位还要留给 HTTP 阶段连接（含握手风暴）、瞬态 worker、supervisor 重启；并且取整后实际值与请求值不同。

### 3.4 各项安全预算（按推荐档 C2）

| 预算项 | 值 |
|---|---|
| VM Process 安全预算 `P_limit` | **65,536**（need 44,576，实际余量 32%） |
| VM Port 安全预算 `Q_limit` | **65,536**（need 34,336） |
| WebSocket 连接安全预算 `W` | **20,000**（软上限；最坏超调 ≤ 10,240 条在途握手） |
| 非 WebSocket 业务预留 | 4,096 进程 / 4,096 端口 |
| 故障恢复余量 | need 的 25%（满载时 ≥ 11,136 个空闲进程位） |

### 3.5 当前各路径的悬崖位置（推算）

| 路径 | 实际 `+P` | WS 悬崖（裸 cowboy 基线口径） |
|---|---|---|
| E1 / E4 / E5（docker、本地、dev） | 32,768 | ≈ 32,600 条后 REST API 开始 500 |
| E3 native-shell | 262,144 | 无进程悬崖（端口 65,536 先于进程；受内存约束） |
| E2 蓝绿 | 1,048,576 | 无 VM 悬崖（受内存/fd 约束） |

## 4. `+P` / `+Q` / `+S` 候选参数（C1 / C2）

### 4.1 `+P` / `+Q`（取 `H` = 10,240、`N_L` = 1）

| 档 | `W` | `P_need` | `Q_need` | 派生 `+P` | 派生 `+Q` | 现有 docker `+P`(32768) 是否够 | 内存估算（9 KB × W） |
|---|---|---|---|---|---|---|---|
| C1 保守 | 8,192 | 32,768 | 22,528 | 65,536 | 32,768 | 不够（need×1.25=40,960） | ≈ 74 MB |
| **C2 推荐** | **20,000** | 44,576 | 34,336 | **65,536** | **65,536** | 不够（55,720） | ≈ 180 MB |
| C3 扩展 | 50,000 | 74,576 | 64,336 | 131,072 | 131,072 | 不够（93,220） | ≈ 450 MB |

- 取 2 的幂是为了让“请求值 = 实际值”，避免再出现 20480→32768 的隐性偏差。
- 进程表/端口表预分配代价可忽略：实测空闲内存 `+P 1024`→37 MB、`+P 20480`→36 MB、缺省 1048576→52 MB。
- “不无限提高”：65,536 = 当前悬崖值的 2 倍且由公式导出，非 OTP 上限。
- 适用范围：E1/E4/E5 改为派生值；E2（1048576）与 E3（262144/65536）已 ≥ 派生值，**本批不改数值**，由“容量下限一致性测试”（§9 T1）守住。

### 4.2 `+S`（与 `+P`/`+Q` 同批评审）

事实：
- 官方文档：`+S` 缺省 = 逻辑处理器数；“If the emulator detects that it is subject to a CPU quota, the default value for SchedulersOnline will be limited accordingly.”
- `+SDcpu` 缺省 = 常规调度器线程数（即 `+S 2` 同时把 dirty CPU 调度器钳到 2）；`+SDio` 缺省 10。
- 现状：E1/E4/E5 = 2；E2 = 4；E3 = 缺省。无任何注释说明 2 的依据。
- 资源信息缺口：compose 只有内存上限（8192M），**无 CPU 限制** → BEAM 看到宿主机全部核心；交付机与蓝绿宿主机核数未知。**无法据此给出最优值**，以下为候选与验证计划。

| 候选 | 内容 | 影响与风险 |
|---|---|---|
| **S-B 推荐（本批）** | 不改各路径默认 `+S`；docker 路径新增可注入 `IMBOY_MAX_SCHEDULERS`（1..1024，校验后生成 `+S N:N`） | 零行为变化；把决策权交给有资源数据的交付方 |
| S-A | 移除仓库默认里的 `+S 2`，交给 ERTS 缺省（核数 / CPU quota） | 多核宿主机上并行度上升；compose 栈与 PG/Garage/LiveKit 同机，可能争核 → 必须有吞吐与 CPU 数据后再定 |
| S-C | 默认改 `+S 4`（对齐蓝绿） | 无数据支撑的机械改动，不推荐 |

验证计划（决定 S-A/S-C 的前置）：阶段 F 在隔离环境对比 `+S 2` / 缺省(本机 12) / `+S 4` 三档，在 (a) 2 万连接风暴 (b) 活跃消息吞吐两场景下记录握手 p50/p95/p99、API p99、`msacc` 调度器利用率、同机 PG 容器 CPU 受限情况；另请属主提供 §2 的生产核数与 cpu.max。

## 5. 连接上限候选（C3）

### 5.1 WebSocket 上限：应用层准入（新增，需批准）

| 项 | 方案 |
|---|---|
| 配置 | `{imboy, max_ws_connections}`（整数），环境变量 `IMBOY_MAX_WS_CONNECTIONS`（沿用 `src/lib/imboy_env.erl` 既有 `IMBOY_<KEY>` 约定，含整数解析） |
| 默认 | 20,000（C2） |
| 校验 | 启动期 fail-fast（同 `validate_drain_config` 风格）：非整数 / 负数 / 超上界 → 拒启，**不静默回退为无限制** |
| 闸口位置 | `websocket_handler:init/2` 两个 surface，**在鉴权与 DB 查询之前**（与现有 429 限流同一位置，`websocket_handler.erl:69-74`） |
| 拒绝方式 | HTTP **503** + `Retry-After: 5`（不升级、不建会话状态） |
| 计数来源 | 复用既有 syn 在线表计数 `imboy_syn:count()`（`src/lib/imboy_syn.erl:120-130`，O(1) `ets:info/2`，syn 监控自动清理死进程，`kill` 也不泄漏）。若阶段 E 核实存在不经 `user_logic:online/4` 注册的长连接 surface，则改用专用 `pg` 组计数 |
| 超调 | 并发握手在 join 之前都可通过闸口 → 最坏超调 ≤ 在途握手数（≤ `H`），仍在 §3 预算内 |
| 客户端行为 | 已核实：握手失败走 `lib/service/websocket.dart:587-602` `_handleConnectionFailure` → 指数退避（1s 起、上限 2min、抖动 0.3），不会紧凑重连；仅 401 触发登出，503 不触发 |
| 可观测 | 新增计数 `imboy_ws_admission_rejected_total` + 限速 WARN；修复 `imboy_ws_connections_current` 语义（§5.3）；新增 80% 水位告警 |

### 5.2 ranch 监听器选项：把隐式值显式化

| 候选 | 内容 | 评价 |
|---|---|---|
| **H-A 推荐** | TransportOpts 改为 map 并显式钉住当前有效值：`max_connections => 1024, num_conns_sups => 10`（= 10,240/listener） | 零行为变化，只是让有效值可见、可测、有注释 |
| H-B | 收紧到 2,048/listener（`max_connections => 512, num_conns_sups => 4`） | 降低握手风暴下最坏占用并缩小 WS 准入超调窗口；但属调优，改变风暴期行为 → 建议待阶段 F 风暴数据后再定 |

同时删除 `websocket_options/0` 里的两个死键（`num_acceptors`/`max_connections => infinity`），并由测试 T3 防回归。

### 5.3 Listener 清单

- `http` / `tls` 模式：1 个 listener（`imboy_listener` / `imboy_listener_tls`）；`http_tls` 模式：2 个，共享同一 VM 预算 → WS 准入是**节点级**（全局），`H` 按 listener 各计。模型对 `N_L=2` 仍成立（need 增加 10,240）。
- 同 listener 同时承载 WS 与 REST API：HTTP 阶段预算被二者共享（现状如此）。

## 6. 参数注入方案（C4）

### 6.1 Docker / install.sh 路径（本批实现）

`.env` 是该路径的唯一真源（`install.sh:197-218` 的 `set_var` / `ensure_value` 幂等约定）。

| 变量 | 默认值 | 校验 | 去向 |
|---|---|---|---|
| `IMBOY_MAX_PROCESSES` | 65536 | 整数 ∈ [1024, 134217727] | 由 install.sh 汇总为 `ERL_FLAGS="+P … +Q … [+S N:N]"` |
| `IMBOY_MAX_PORTS` | 65536 | 同上 | 同上 |
| `IMBOY_MAX_SCHEDULERS` | 空（沿用镜像 vm.args 的 `+S`） | 空 或 整数 ∈ [1, 1024] | 同上 |
| `IMBOY_MAX_WS_CONNECTIONS` | 20000 | 整数 ∈ [1, 1000000]，后端启动期再校验 | compose 直传 backend 环境变量 |

- compose（prod / community / demo）：`imboy_backend.environment` 新增 `ERL_FLAGS: ${ERL_FLAGS:-}` 与 `IMBOY_MAX_WS_CONNECTIONS: ${IMBOY_MAX_WS_CONNECTIONS:-20000}`；删除死配置 `ERL_MAX_PORTS`。
- 为什么用 `ERL_FLAGS` 而不是 `VMARGS_PATH`：前者无需在宿主机维护一份会漂移的 vm.args 副本，且实测覆盖 args_file；后者需要镜像外挂载可写文件并在每次升级时重新生成。`ERL_MAX_PORTS` 是未文档化变量且会被覆盖，不采用。
- 幂等：`ERL_FLAGS` 每次运行由三个 `IMBOY_MAX_*` **确定性重算**；三个 `IMBOY_MAX_*` 仅在 `.env` 缺失时写入默认值，交付方已填的值不被覆盖。
- 非法值：任一校验失败 → `die`，不落 `.env`；`.env` 里若已有手工 `ERL_FLAGS` → 拒绝（避免双真源）。
- 旧安装兼容：已有 `.env` 缺这些键 → 下次 `install.sh` 补默认值；不经 `install.sh` 直接 `docker compose up -d` 的环境 → `ERL_FLAGS` 为空，使用新镜像内置的 vm.args 默认值（同为派生值），两条路径结果一致。
- 一致性核验：后端启动时输出一行 INFO `capacity: process_limit=… port_limit=… schedulers_online=… max_ws_connections=…`；install.sh 起服后读取该行并与期望值比对，不符即 `die`。
- 不写敏感信息：`ERL_FLAGS` 只含数字。

### 6.2 不做动态注入的入口（说明原因）

| 入口 | 处理 | 原因 |
|---|---|---|
| E2 蓝绿 | 数值不改；纳入容量下限一致性测试 | 已高于派生值；是 SaaS 生产部署脚本，改动需走其发布验收 |
| E3 native-shell | 不改 | 数值已达标；该包带 `CHECKSUMS.sha256` 与独立验收（`deploy/native-shell/tests/run.sh`），改 `render.sh` 等于重新交付 |
| E4 / E5 本地 | 直接改默认值 | 开发路径，无注入需求 |

## 7. 向后兼容性

- `+P`/`+Q` 仅上调（32768 → 65536 实际值）；`+S` 不变（S-B）。
- 新增 WS 软上限 20,000：现有部署若单节点并发 WS 超过 2 万才开始拒绝（未发现此类部署；可经 `IMBOY_MAX_WS_CONNECTIONS` 调整）。
- HTTP 阶段上限（H-A）为显式化，行为不变。
- API 面无变化；WS 拒绝发生在升级前，客户端按既有失败路径退避。
- 指标 `imboy_ws_connections_current` 语义修正后会从“接近 0”跳变为真实连接数：`ImBoyWSConnectionsDrop` 告警恢复有效；面板阈值需同步复核。

## 8. 风险分析

| 风险 | 等级 | 缓解 |
|---|---|---|
| WS 准入闸口在热路径引入缺陷（误拒 / 计数错） | 中 | 单元 + CT 覆盖；闸口仅一次 O(1) 读；阶段 F 阶梯验证 |
| 存在未注册到 syn 的长连接 surface → 漏计 | 中 | 阶段 E 先核实；必要时改专用 `pg` 组 |
| 准入超调（并发握手） | 低 | 有界（≤ H），已计入预算 |
| 提升上限后内存/CPU 被更多连接占用 | 低 | 9 KB/连接；水位告警 80%；`+S` 保守 |
| `ERL_FLAGS` 被误用带入危险标志 | 低 | install.sh 只生成四个白名单标志；compose 不接受自由文本 |
| 压测机限制导致无法验证 >16k | 中 | 已实测本机单客户端节点上限约 16.2k（`connect_failed` 4,799）→ 阶段 F 用多客户端节点或 Linux 压测机；不外推 |
| 指标语义变化影响既有面板 | 低 | 同批更新面板与告警规则并留存对照 |
| 与在途车道冲突 | 低 | 7 个 worktree 在 R0 文件上全部干净；独立 worktree 开发 |

## 9. 测试计划

### 9.1 配置与单元测试（复用 EUnit / CT / 既有 shell 测试，不新建框架）

| # | 测试 | 类型 |
|---|---|---|
| T1 | 容量下限一致性：解析 5 个来源的 `+P/+Q`，断言均为 2 的幂且 ≥ 派生下限；`config/vm.args` 恰为派生值 | EUnit |
| T2 | 监听器 TransportOpts：`max_connections`/`num_conns_sups` 为正整数、无 `infinity`；在端口 0 实起 listener 回读有效值 | EUnit |
| T3 | `websocket_options/0` 不含 `num_acceptors`/`max_connections` | EUnit |
| T4 | `IMBOY_MAX_WS_CONNECTIONS` 覆盖与非法值拒启（非整数 / 负数 / 超界） | EUnit |
| T5 | WS 准入：未满 → 正常升级；已满 → 503 + `Retry-After`；计数器递增 | EUnit / CT（沿用 `websocket_connection_flow_SUITE` 的独立 listener 模式） |
| T6 | `imboy_ws_connections_current` 等于 syn 在线计数 | EUnit |
| T7 | install.sh：默认值写入、非法值拒绝、重复执行结果一致、`ERL_FLAGS` 汇总正确 | shell 测试 |
| T8 | Release 一致性：`make rel` 后 `releases/<vsn>/vm.args` 与 `config/vm.args` 标志一致；带 `ERL_FLAGS` 启动后 `erlang:system_info/1` 读数符合预期 | shell 测试 |

### 9.2 容量阶梯（阶段 F，隔离本地环境，禁止生产）

复用 `scripts/bench_websocket.sh`（保留握手延迟与空闲保持能力），新增阶梯与观测：

| 档 | 连接数 | 验证点 |
|---|---|---|
| R1 | 2,000 / 5,000 / 10,000 | 安全容量内稳定；采集每档基线 |
| R2 | 接近安全容量（18,000） | 无拒绝；已建立连接不受影响 |
| R3 | 超安全容量（22,000） | 超出部分得到 **503**；服务不崩；已建立连接正常 |
| R4 | 释放 5,000 后重连 | 释放的配额可被重新占用 |
| R5 | 满载时 supervisor 重启 | worker 可正常重启；REST API 正常 |
| R6 | （可选）`+S` 三档对比 | 为 S-A/S-C 决策供数 |

每档记录：实际建立数、握手成功率、握手 p50/p95/p99、拒绝数、`process_count`、`port_count`、RSS、BEAM 内存、CPU、调度器利用率、最大邮箱长度、错误日志条数、supervisor 重启次数。

## 10. 回滚方案

| 逻辑单元 | 单独回滚方式 |
|---|---|
| VM 参数默认值（E1/E4/E5） | `git revert` 对应提交并重建镜像 / release；运行期临时回退：`.env` 设 `IMBOY_MAX_PROCESSES=20480 IMBOY_MAX_PORTS=20480` 后重跑 install.sh（经 `ERL_FLAGS` 生效，无需重建） |
| 监听器显式选项 + 删除死键 | `git revert`；行为本就与现状等价 |
| WS 准入 | `git revert`；或把 `IMBOY_MAX_WS_CONNECTIONS` 调高（不提供“无限制”取值） |
| install.sh 注入 + compose | `git revert`；已写入 `.env` 的 `ERL_FLAGS` 需手工删除一行 |
| 指标与告警 | `git revert` 面板 / 规则文件 |
| 文档 | `git revert` 文档提交 |

- 全程不使用 `git reset --hard` / `git clean`；worktree 内按逻辑单元拆分提交，便于逐个 revert。

## 11. 对第一轮审计的勘误

| 第一轮表述 | 实际情况 | 依据 |
|---|---|---|
| P1-1：“`max_connections => infinity` 使 ranch 层不设上限” | 该键是流级死配置；真实机制是 cowboy 在 WS 升级时调用 `ranch:remove_connection/1` | §0 |
| P1-1：“悬崖在 ~1.9 万连接” | `+P 20480` 实际取整为 32,768 → docker/dev 路径约 3.26 万 | §1.1、§3.5 |
| P1-1：“release 模板实证同值 → 部署管线未覆盖” | 仅对 Docker 镜像路径成立；蓝绿重写为 1048576、native-shell 为 262144/65536 | §1.1 |
| 性能专项：“`+Q` 默认 65,536 … 压缩 3.2 倍” | 缺省值随 fd 上限上调（本机为 1048576）；显式 `+Q 20480` 实际 32,768 | §1.1、§1.3 |
| 性能专项建议“`websocket_options/0` 设 18000” | 该位置不生效；且任何值都约束不了已建立的 WS | §0、§5 |
| 性能专项建议“`+S 4:2` 起步” | 无资源数据支撑，改为候选 + 验证计划 | §4.2 |
| §0/§3.1 “连接进程原地接管（takeover/7）” | **不成立于本仓形态**：ranch `connection_type=supervisor` 下升级接管**替换进程**——init/2 进程在 101 后 ~1s 内退出，真实 ws 循环在另一个幸存进程（隔离节点实测 `is_process_alive=false` vs syn 成员存活）。准入执行点因此移到 `websocket_init/1`（幸存进程原子认领），init/2 保留软闸（实施提交 6825219c2） | 阶段 E6 实测 |
| §9.2 R1–R3 大档位（10k/12k/超限）预期在本机可测 | **本机 rig 对真实 imboy 的服务端可持续稳态 ≈1.6–1.8k 并发**（recorder 三轮复现）；bench 的 held/connected 在 ≥3k 是半开幻觉。超限行为改由 cap=1/5/1500 直测证实；20,000 绝对值本机不可验证，不外推 | 阶段 F |

未发现第一轮“误报”之外的 P1 级问题；原审计文档全部保留，勘误以追加方式标注。

## 12. 需要属主批准的决策点

| # | 决策 | 推荐 |
|---|---|---|
| D0 | 同意 R0 范围调整：新增**应用层 WS 准入**（`websocket_handler:init/2` 热路径改动），而非仅改配置 | 同意（否则无法给 WS 设上限） |
| D1 | WS 上限 `W`：C1 8,192 / **C2 20,000** / C3 50,000 / 其他 | C2 |
| D2 | docker/dev 默认 `+P/+Q` 采用派生值 **65536/65536**；蓝绿与 native-shell 本批不改数值 | 同意 |
| D3 | `+S`：**S-B 保持现状 + 可注入** / S-A 交给 ERTS 缺省 / S-C 固定 4 | S-B，S-A 待阶段 F 数据后另议 |
| D4 | HTTP 阶段上限：**H-A 显式钉住 1024×10** / H-B 收紧到 2,048 | H-A |
| D5 | WS 拒绝方式：**HTTP 503 + Retry-After（升级前）** / 升级后以 1013 关闭 | 503 |
| D6 | 同批修复 WS 连接数指标 + 新增水位告警 | 同意（否则容量不可观测） |
| D7 | 同批删除 compose 死配置 `ERL_MAX_PORTS` | 同意 |
| D8 | docker 路径 cookie 占位值（W6-A02）另立任务处理 | 另立，不并入 R0 |

**批准前不进入阶段 E。**

---

## 13. 批准记录（2026-11-24）

属主已正式批准 D0–D8 并授权进入阶段 E–G（批准原文要点摘录）：

| 决策 | 批准内容（属主原文） |
|---|---|
| D0 | 批准新增应用层 WebSocket 准入控制 |
| D1 | 单节点 WebSocket 准入上限 20,000 |
| D2 | Docker/dev 路径 +P 65536、+Q 65536 |
| D3 | +S 暂时保持 2，支持部署参数注入，不擅自改变默认值 |
| D4 | 显式保持 HTTP 阶段 max_connections 1024 × 10 的现有行为 |
| D5 | WebSocket 升级前通过 HTTP 503 拒绝，提供 Retry-After |
| D6 | 修复 WS 连接数指标，使用真实在线注册数量，并增加 80% 水位告警 |
| D7 | 删除 compose 中已被显式 +Q 覆盖的 ERL_MAX_PORTS 死配置 |
| D8 | Docker Cookie 安全问题独立立项，不纳入 R0 |

授权范围：修改 R0 worktree 业务代码/配置；WS 准入逻辑；VM 参数与 Docker 部署参数注入；WS 指标修复；相关测试与容量压测；部署/容量/回滚文档；完成后创建 PR 但不得合并。禁止：推送前未经批准合并、生产部署、越出 R0 的改动。

### 13.1 属主强制约束对实施的映射（全部落实）

| 属主约束 | 实施落点 |
|---|---|
| 20,000 是单节点总准入上限（非 per-listener） | `ws_admission` 全局命名 ETS 表，双 surface/双 listener 共享 |
| 并发连接建立不会超额准入 | `admit/0` = `ets:update_counter` 原子自增 + 超限回滚（阈值式被 OTP 29.0.2 实证 badarg 弃用；2-op 形态 EUnit 并发竞态守恒） |
| WS 握手失败释放名额 | `websocket_handler:init/2` 全部非升级返回路径显式 `release/0`（throttle 429 / 子协议 40x / auth 401/412） |
| 正常关闭释放 | `terminate/3` `release/0`（take 行配对，幂等） |
| 异常退出可靠清理 | `sweep/0` 每 5s `is_process_alive` 全表扫描 + take 配对递减 |
| 重启后计数恢复 | VM 重启表随进程消亡重建为 0 = 真实在线状态；运行期损坏由 sweep 对账（静置复核后修复）修复，负计数夹取 |
| 不允许计数泄漏导致永久拒绝 | 上述 sweep 对账 + guarded decrement |
| 优先复用 syn | syn 仍是在线状态真源（metrics 换源 `imboy_syn:count()`）；准入计数是容量预算（与 syn 语义不同：syn 含 `ai_agent_runtime` 的 agent 成员，无法做原子条件加入——属主约束"并发不超额准入"必须用原子原语，设计 §5.1 的超调容忍条款被属主更严约束取代，已记录） |
| （实施勘误）准入执行点 | 属主约束"升级前 503"保留为**软闸**（init/2 读精确 count 满载 shed）；原子认领移至 `websocket_init/1` 幸存进程（cowboy supervisor 模式替换 init 进程，实测见 §11 勘误）；竞争窗口兜底 = 升级后 1013 Try Again Later 关闭。已提交名额仍永不超上限 |
| 503 在升级前 + Retry-After + 确认 Cowboy 语义 | `init/2` 闸口在 throttle 之前；`{ok, replied_Req, State}` 直接 reply 形态为既有生产路径（429 同款），503 body `{"code":503,"msg":"ws_capacity_full"}`，`Retry-After: 5` |
| 确认 Flutter 重连行为 | 沿用阶段 B 结论：`_handleConnectionFailure` 指数退避（1s 起/2min 上限/抖动 0.3），仅 401 登出 |
| 指标修复 + 80% 水位告警 | `imboy_ws_connections_current` ← `imboy_syn:count()`；新增 `imboy_ws_admission_current/limit/rejected_total`；告警 `ImBoyWSAdmissionHigh`（admission 比 > 0.8，对注入上限自适应） |
| VM 参数 + 不允许 Release/运行参数不一致 | `config/vm.args`/`start_node.sh` = 65536（= 实际值，无取整偏差）；install.sh 经 `ERL_FLAGS` 注入时确定性重算单一真源；启动期 INFO 行输出 process_limit/port_limit/max_ws_connections 供一致性核验 |
| 压测只在隔离环境 | 阶段 F 于本机隔离 worktree release 节点执行，禁止生产 |

