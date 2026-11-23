# 性能审查（Phase F）

> 2026-11-23 · 原则：结合既有 Bench 区分空闲连接容量与活跃消息吞吐；先识别瓶颈面，优化前后需实测数据（工作约束 5）
> 依据教程 Ch.10 Schedulers and Performance / Ch.02 §4.6 / Ch.03 §4

> ⚠️ **R0 勘误（2026-11-23 追加，下文原文保留未改）**：§2 的证据链有三处不成立——(1) `websocket_options/0` 的 `max_connections => infinity` 位于流级选项，ranch 从未读取；(2) 真实机制是 cowboy 在 WS 升级时调用 `ranch:remove_connection/1`（`deps/cowboy/src/cowboy_websocket.erl:325-332`），故 §2 修复建议第 2 条"在 `websocket_options/0` 设 18000"在任何位置都不起作用，WS 上限必须由应用层准入实现；(3) `+P 20480` 实际为 32768，悬崖在约 3.26 万而非 ~1.9 万，且蓝绿 / native-shell 路径的 vm.args 由各自脚本重写。`+S 4:2` 建议缺乏资源数据支撑，改为候选 + 验证计划。详见 `r0-capacity-remediation-design.md`。

---

## 1. 既有性能资产盘点

| 资产 | 覆盖面 | 局限 |
|---|---|---|
| `scripts/bench_websocket.sh` | 空闲连接容量：N 条**带鉴权真实 WS**，握手延迟分布（percentiles）+ 保持稳定性（默认 100 连接/10s，可 CONNS/HOLD 调参） | **不含消息收发**——活跃吞吐为零 |
| `scripts/smoke/c2c_ws_smoke.*` / `ws_c2g_send.py` | 少量消息功能面冒烟 | 非压测（无速率控制/统计） |
| `scripts/cache_*_bench.erl` ×5 | depache 缓存微基准（digest/stress/v2/w0） | 组件级 |
| `imboy_msg_store_batch_seconds` 直方图 | 生产在环：落库批次时延真实分布 | 覆盖单一环节 |
| RTC `lk load-test` | 真实 Opus 媒体面压测（项目记忆） | 独立车道 |
| cowboy metrics | HTTP 层 QPS/状态码分布 | 非 WS |

**结论：`空闲连接容量` 有 bench，`活跃消息吞吐` 无 bench——这正是任务要求区分的两面，后者缺位。**

## 2. P1-1 容量悬崖（本轮最高严重度发现）

### 全要素记录

- **文件/函数**：`config/vm.args:12-13`（`+P 20480`、`+Q 20480`）× `src/api/websocket_handler.erl:116-127`（`websocket_options/0` 的 `max_connections => infinity`、`num_acceptors => infinity`）。
- **证据**：
  1. release 模板实证出厂值：`_rel/imboy/releases/1.0.0-alpha.75/vm.args` 与 `config/vm.args` diff 仅 name/cookie 两行，`+P/+Q/+S` 完全一致——即部署管线未覆盖；
  2. 官方文档已核：`+P` 默认 **1,048,576**、`+Q` 默认 **65,536**（erl_cmd）——本项目分别压缩 51× / 3.2×；
  3. deploy/install.sh 只注入 cookie/name（:605-612 注释），无容量参数注入通道。
- **机制**（教程 Ch.2 §2 + Ch.10）：每 WS 连接 ≥1 进程 + 1 port（socket）；cowboy `max_connections => infinity` 表示 ranch 层不设上限。进程表/端口表是**全局硬顶**：打满后 `erlang:spawn` 全节点 badarg——不止新连接失败，**supervisor 重启子进程、定时器、user_server 分片 worker 全部无法派生**，故障从"过载"升级为"全节点机能瘫痪"。
- **触发条件**：连接数爬升至 ~1.9 万（20480 − 系统基线进程/端口占用）；或任何进程泄漏叠加。无削峰（throttle_ws 是握手速率闸，非并发数闸）。
- **实际影响**：大范围服务不可用且**不可自愈**（连接不释放则进程表不释放）。
- **复现**（本地/压测环境）：`CONNS=22000 bash scripts/bench_websocket.sh`——预计 ~2 万连接后握手批量失败；伴随节点日志出现 spawn 相关 badarg。⚠️ 仅限本地压测环境（脚本头部自带的警告）。
- **修复建议**（可独立审查回滚）：
  1. `config/vm.args`：`+P 262144`（或按交付容量定值）+ `+Q 65536`——单行配置改动；
  2. `websocket_options/0`：`max_connections` 设有限值（如 18000，留系统余量），ranch 超限时 cowboy 拒绝握手（客户端可感知重试，优于节点瘫痪）；
  3. install.sh 增加 `IMBOY_MAX_PROCESSES` 注入通道（与 cookie 同模式），交付文档同步容量公式。
- **回归测试**：bench_websocket.sh CONNS 阶梯（如 5k/10k/18k/21k）作为容量基线脚本；eunit 层断言 websocket_options 不含 infinity（防回归棘轮，项目已有同款文化）。
- **预估工作量**：S（配置+设界）+ S（bench 阶梯化）。

### 联动核验（P2-5）

- `+S 2`（vm.args:14）：2 调度器对多核机是并行度钳制。在修复 P1-1 提升 +P 之前，+S 2 反而"意外兜底"了进程失控的调度面；**修 +P 时必须同批评审 +S**（否则 20 万进程压 2 个调度器会把问题转化为调度饥饿）。建议按部署机核数 `+S 4:2` 起步并用 scheduler utilization 指标（OBS-2）实证后再调。
- `+A 1024` 异步线程池充足；`+K true` 内核轮询正确（高连接前提）；`+MSe true`（共享 erts allocator）为容器内存友好配置，与高连接数场景匹配。

## 3. 热点/瓶颈识别（代码面定性，待 bench 实证）

### 3.1 热点进程面（教程 Ch.10 §3"只有进程间可并行"）

| 候选热点 | 现状评估 |
|---|---|
| WS 连接进程 | 天然 per-connection 并行 ✅；每帧 hibernate（见 3.2） |
| msg_store_worker（单写者） | 批拉模型 + batch_seconds 直方图；**单进程串行**——吞吐天花板=单进程批处理速率。当前无数据判断是否饱和；若饱和，按 group/key 分片是设计内扩展位（msg_store_sup 加 worker） |
| user_server（分片 ×?WORKER_COUNT） | 有界队列+丢弃，过载显式日志 ✅ |
| elib_metric（单 gen_server） | cast 计数聚合，高量下 mailbox 风险低（无 reply 等待） |

### 3.2 每帧 hibernate 的 CPU 成本（P2-6）

- **证据**：websocket_handler 全部 return 子句带 `hibernate`。
- **机制**：hibernate = fullsweep GC + 堆收缩。空载/低频连接是纯收益（内存紧、binary 句柄即释放）；高吞吐连接（如群聊扇出端、bot）每帧 fullsweep 的 CPU 成本随堆尺寸放大（2MB 帧处理期堆峰值不小）。
- **教程观点**（Ch.13 actions 表）：hibernate "costly; not after every event"——但 cowboy WS 场景属该指导的经典例外（长闲连接为主）。
- **判定**：**不是缺陷，是未实证的权衡**。行动项=活跃吞吐 bench（§4）分 CONNECT-ONLY / LOW-RATE / HIGH-RATE 三档测 p99 帧延迟与 scheduler utilization，数据说话后再决定是否对高活动连接改用 `message_queue_data, off_heap` + 周期性 GC 策略。
- **工作量**：bench M；若数据支持调整，改动 S（return 子句宏化）。

### 3.3 消息复制面

- 入站：jsone/imboy_pb 解码一次 → 路由 map 传递（同进程堆内指针，无复制）✅
- 出站：JSON 预编码 binary → refc 共享投递（跨进程零拷贝）✅；protobuf 客户端侧一次 decode→re-encode（websocket_handler.erl:1057-1106）——每消息每协议转换一次，属必要成本，兜底路径有 fallback 日志 ✅
- 跨节点：GROUP_BATCH 64KB 切块 + external_size 预算 ✅

## 4. 补齐活跃消息吞吐 bench 的最小方案（不动现有 bench）

```
scripts/bench_ws_throughput.sh（新增，复用 c2c_ws_smoke.py 骨架）
  维度：
    - 发送端 M 个连接 × 每连接 r msg/s（可参数化 1/10/100）
    - 消息类型：text JSON 与 v2 binary 两协议
    - 采集：端到端 p50/p95/p99（client 时间戳→SERVER_ACK/回执）、
      服务端 imboy_msg_store_batch_seconds 与 budget counters（/metrics 拉取）、
      scheduler utilization（依赖 OBS-2 落地；先用手动 recon 采样过渡）
  验收口径：优化前后同环境同参数三次取中位（教程 Ch.10 §3 基准纪律：
    秒级时长、独立进程、CPU 时间与墙钟双记）
```

- **工作量**：M。**前置**：无（可先跑，scheduler 维度后补）。

## 5. 调度器与系统面现状

- 无 NIF → 无调度器阻塞面（教程 Ch.10 §"1ms rule" 不适用，hird/textguard 均进程外）。
- 容器 CPU% 观测陷阱（教程 Ch.10 §4.6 spin 假负载）：deploy 栈以 OS CPU 为告警源时注意；`+sbwt` 未调（默认），低负载节点 CPU 读数偏高属预期——建议告警规则改挂 run_queue 指标（OBS-2 联动）。

## 6. Phase F 结论

唯一 P1 是**配置面容量悬崖**（+P/+Q × infinity 组合），修复成本 S 级且完全独立可回滚；其修复必须与 +S 同批评审。性能热点面无明显结构性错误（并行度、复制面、批处理设计均健康），真正缺的是**活跃吞吐基准数据**——在拿到 §4 的 bench 数据前，任何进一步优化（含 hibernate 策略）都不应动手（教程 Ch.10 §4.2"optimising by guessing"反面教材防线）。
