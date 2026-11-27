# 生产可观测性审查（Phase E）

> 2026-11-23 · 原则：结合既有 Recon/OTel/metrics 能力盘点，不重复开发已有诊断功能（工作约束 3）
> 依据教程 Ch.09 Observability

---

## 1. 既有可观测性资产（先盘点，避免重复建设）

| 类别 | 资产 | 证据 |
|---|---|---|
| 指标 | `metrics_handler`：Prometheus 风格输出 erlang_vm_process_count、erlang_vm_memory_bytes_total{kind=total/processes/ets}、ws 连接数、imboy_http_requests_total{method,status}（cowboy metrics_callback 产出，B-26）；`elib_metric`（ETS counter/histogram/gauge gen_server） | src/api/metrics_handler.erl:101-162、imboy_app.erl:127-136 |
| 生产直方图 | `imboy_msg_store_batch_seconds`（落库批次时延）、BE-14 预算 counters oracle（delivered+deferred==尝试数，imboy_syn.erl:353-377/476-498） | msg_store_worker.erl:228、imboy_syn.erl |
| 追踪 | OpenTelemetry 栈 + Uptrace（可选 DSN，内部异常自吞不阻断启动） | imboy_app.erl:66-68 |
| 日志 | lager（sys.config 段）+ kernel logger_level=info 统一控级；sasl error_logger 落盘（log/sasl，104MB×255 滚动）；elib_log:safe_log 级别门控先于格式化（Ch.9 §4.1 合规） | config/sys.config.example:786-796、src/lib/elib_log.erl:110-120 |
| 交互诊断 | recon / redbug / observer_cli 在 DEPS——供远程 shell 事故诊断（正确用法，非代码内轮询） | Makefile:59 |
| 编排 | 社区监控（Prometheus、Grafana 和指标采集器） | 仓根 `docker-compose.yml` 的 `monitoring` profile |
| 探针 | /readyz（readiness 分级 starting/degraded/ready + 依赖 whereis + limiter + cache_sync 状态）、/healthz | imboy_app.erl:205-245 |

**结论：外围观测栈完整度高；缺口集中在 BEAM 内部维度。**

## 2. 对照教程 Ch.9 checklist

| 检查项 | 状态 |
|---|---|
| ?LOG_* 宏 + 结构化报告 | ✅（自研宏 + 级别门控；报告带 module/line/pid metadata） |
| 日志不落整包载荷 | ✅（websocket_handler.erl:897-899 注释："protobuf 消息整帧含明文 payload，默认级别不输出"；WARN 级只带 byte_size 不带内容——Ch.9 §4.2 合规） |
| formatter depth/chars_limit | ⚠️ 未见显式设置（sys.config lager 段无 depth 限制）——大 term 若进 error 级日志无截断兜底 |
| 重要事件计数化 | ✅（budget counters / http requests / batch histogram） |
| 生产 trace 限率/限进程 | ✅（代码内零 dbg 调用；redbug/recon 人工诊断模式） |
| message_queue_len 读法 | ✅（imboy_syn.erl:359 且作为预算门真源；全仓无 process_info(messages)） |
| sys: 仅调试 | ✅（零 sys:get_state 代码内调用） |
| BEAM 指标导出 + system_monitor | ❌ **两项缺口（见 §3）** |

## 3. 缺口明细

### 3.1 OBS-1（P2-1）无 erlang:system_monitor 哨兵

- **文件/函数**：src/ 全量 grep `system_monitor` = 0 命中。
- **机制**（教程 Ch.9 §2/§4.7）：`erlang:system_monitor/2` 是 BEAM 内病理（long_gc / long_schedule / large_heap / busy_port / busy_dist_port，OTP 25+ 还有 `long_msgq`）的**唯一主动推送哨兵**；没有它，上述事件只能在事故后靠 recon 手工取证。
- **触发条件/影响**：大帧处理触发长 GC、2MB 帧 × 慢客户端引发大堆/长队列——正是本项目消息面最可能的病理形态——当前全部静默。
- **复现**：向本地节点发 2MB 帧 × 100 并让接收端休眠 → `erlang:statistics(garbage_collection)` 可见 GC 时间攀升，但无任何日志/指标产生。
- **修复建议**：新增轻量 worker（挂 imboy_sup，transient）安装 `erlang:system_monitor(self(), [{long_gc, 100}, {long_schedule, 100}, {large_heap, 33554432}, {long_msgq, 8192}])`，handle_info 落 WARN 结构化报告 + elib_metric 计数（阈值可 env 覆写，默认保守避免噪音）。
- **回归测试**：eunit 内人为制造 large_heap（spawn 大列表）断言 WARN 产出。
- **工作量**：S（约 60 行 + 测试）。

### 3.2 OBS-2（P2-2）metrics 面缺 BEAM 关键维度

- **文件/函数**：src/api/metrics_handler.erl:101-104（现仅 process_count + memory 三项）。
- **缺口清单**（教程 Ch.9 §4.7 逐项）：
  1. `erlang:statistics(total_run_queue_lengths)` —— 调度器过载的首要信号；
  2. scheduler utilization（`scheduler:utilization/1` 采样，runtime_tools）—— 与 +S 2 决策（perf P2-5）互为前提；
  3. binary memory（`erlang:memory(binary)`）—— Ch.3 病理（binary 保留）的直接观测面；
  4. `system_info(atom_count)` —— 上限耗尽预警；
  5. MQL top-N（`recon:proc_count(message_queue_len, 10)` 低频采样导出，或至少 max MQL）；
  6. ETS 分表内存（`ets:info(T, memory)` 遍历，低频）；
  7. supervisor 重启计数（logger handler 计 supervisor 报告，见 process PB-1）；
  8. lager/logger drop 计数（Ch.9 §4.6：事故尖峰恰好是 drop 高发窗，缺此即"日志缺失被当无事发生"）。
- **修复建议**：metrics_handler 内加低频采样节流（如 5s 缓存，避免每 scrape 全量遍历 ETS）；1/3/4 为一行级改动先行，5/6/7/8 次之；Grafana 面板配套。
- **回归测试**：metrics 输出快照测试（既有 metrics_handler 测试扩展字段断言）。
- **工作量**：M（分两批）。

### 3.3 OBS-3（P3）lager 格式化无 depth 兜底

- lager formatter 未配 `depth`/`size` 限制；error 级携带大 term（如异常参数）时全量格式化。
- 建议：sys.config lager 段加 formatter depth（低风险纯配置）；与 OBS-2 的 drop 计数同批。

## 4. 已有的"超出教程"实践（保护名单，勿重复建设）

- BE-14 预算门的 **counters-as-oracle**（delivered+deferred==尝试数的不变式指标化）—— 教程 Ch.9 §4.6"重要事件计数化"的高阶实现。
- /readyz 的**分级降级语义**（degraded ≠ down，LB 可按级别摘流）—— 比二值 healthz 更精细。
- safe_log 的"调阈值不炸调用点"设计（级别过滤显式化，注释论证）。
- metrics 字段命名的 Grafana 兼容映射（metrics_handler.erl:449-458 的扁平 key 翻译层）。

## 5. Phase E 结论

外围栈（Prometheus/Grafana/告警/OTel/sasl 落盘/readyz 分级）完整，交互诊断工具（recon/redbug/observer_cli）就位且使用姿势正确。真实缺口两处且同根：**BEAM 内部维度零覆盖**（system_monitor 哨兵 + 8 项指标维度），合计 M 工作量即可把"事故后手工取证"升级为"病理主动告警"。修复不与任何既有能力重复。
