# IMBoy × Learning-Erlang-from-Claude 审查核对表

> 2026-11-23 · 依据教程 Ch.14 速查表 + 各章 Review Checklist 逐条对照 src/（排除 *_tests）
> 图例：✅ 合规有据 · ⚠️ 部分合规/有条件 · ❌ 缺口 · N/A 暂不适用（单节点/无 NIF/无 Elixir）

## 1. 红旗检索表命中结果（教程 Ch.14 §1 逐条）

| 检索模式 | 命中 | 定性 |
|---|---|---|
| `timer:sleep`（测试同步） | test/ 242 处 | ⚠️ 既有体系内大量存在；多数位于 deadline 轮询循环（有界非裸睡），不重构（工作约束 3），列为测试卫生观察项 |
| `list_to_atom` / `binary_to_atom` | src/ 6 处实质调用 | ✅ 全部白名单前置或 DB 有界枚举（cs_identity_assertion.erl:133-141、eb_offboarding_app.erl:243-257 已核 guard；agent_run_pg P3-4） |
| `os:cmd` 拼接注入 | 仅 ensure_tool_path 改 PATH | ✅ 无用户输入拼接 |
| 裸 `spawn(` 无 link/monitor/supervisor | 15 处非测试 | ⚠️ 逐一核查：edge_proxy watchdog 正确 monitor；message_ds/group_file_ds 为 P3-1/P3-2；elib_async:async/1 **零生产调用方**（仅测试用） |
| `++` 在 fold/递归中 | 无循环热点 | ✅ 命中均为启动期路由表一次拼接 |
| `<<X/binary, Acc/binary>>` 前插 | 0 | ✅ |
| `ets:tab2list` / 无键 `match_object` | 12 处 | ✅ 均为小表（路由名表、sync 元数据、测试 stub） |
| `ets:lookup`+`ets:insert` 同键读改写 | 未见竞态形态 | ✅ 投递预算用 `ets:update_counter`/`select_replace`（imboy_syn.erl:399/445） |
| `persistent_term:put` 非启动期 | 17 处 | ✅ 启动期/稀有事件（license、插件 manifest、regex 缓存、adm 配置更新）；无每请求写 |
| `{noreply, State, Timeout}` | 1 处 | ✅ imboy_cache_sync_recover.erl:82 重试 500ms，语义正确 |
| behaviour 回调内 `receive` | 0（WS guard 的 after-0 drain 非阻塞等待） | ✅ |
| `{{timeout, Name}, ...}` 无 cancel | 未见 | ✅ |
| `handle_call` 内 `gen_server:call` | 15 个模块候选 | ⚠️ 抽查为跨服务器调用（非互调环）；无同服务器回调内自调；无证据的死锁环。遗留：未逐一穷尽 15 模块的每条边（第二轮可做静态调用图） |
| `catch exit:{timeout,_}` 后重试 | 未见盲重试 | ✅ remote_group_batch 显式 unknown 语义（message_ds.erl:172-186 注释） |
| `noconnection` 接管领导权 | N/A 单节点 | — |
| 匿名 fun 跨节点 rpc | 无 | ✅ erpc:cast 用 MFA（imboy_syn.erl:228） |
| `application:set_env` 运行期控制 | lifecycle/draining 状态标记 | ✅ 有意设计（imboy_app.erl:32/161/169），非配置通道；启动校验函数防御了运行期注入（imboy_app.erl:296-299 注释） |
| `ensure_all_started(App)` 生产 boot | pooler（sup init 内） | ⚠️ imboy_sup.erl:71，有 2026-11-13 实测论证注释；imboy 主 app 由 release 以 permanent 启动 |
| `process_info(Pid, messages)` | 0 | ✅ 全部用 message_queue_len（imboy_syn.erl:359） |
| `sys:get_state` 当 API | 0 | ✅ |
| `dbg:p(all, ...)` | 0 | ✅ redbug/recon 在 deps 供人工诊断 |
| `logger:debug(io_lib:format(...))` 先格式化 | 0 | ✅ elib_log:safe_log 级别门控在格式化之前（elib_log.erl:110-120）；?DEBUG_LOG 单参形式传 term 非格式串 |
| NIF 调度标志 | N/A 无自研 NIF | —（textguard 走独立服务，hird vendored） |

## 2. 各章 Checklist 符合度

### Ch.1 监督树
- ✅ 子顺序=依赖顺序：one_for_one 下启动顺序有注释论证（PluginLoader→PluginSup，imboy_sup.erl:293-297）
- ✅ 子 supervisor 全部 `type => supervisor`（imboy_sup.erl:136-143/298-305；msg_store_sup 无子 sup）
- ✅ per-connection 进程不在监督树内（cowboy 自管，属 ranch conns_sup 临时子）——符合 Ch.1 §4.5 精神
- ✅ intensity/period 分层差异化（50/60 顶层、10/60 msg_store、5/5 插件），顶层放宽有 CI-00 run10 实测（imboy_sup.erl:399-404）
- ❌→P2-3 重启率无指标无告警（checklist 第 8 条之外的配套缺口）
- ✅ 需清理的 worker trap_exit（user_server.erl:55）；cowboy conn 进程生命周期由 cowboy 管理

### Ch.2 进程与邮箱
- ✅ 手写 receive 全部有 after/deadline 或属 after-0 drain；catch-all 仅 user_server worker_loop 缺（P3-3，单 sender 前提）
- ✅ 请求/应答全部 make_ref/monitor 打标（barrel_mcp_session.erl:946-1018、talk_record_model_exec.erl:29-38）
- ✅ 大载荷不复制分发：网关分块按 Budget 切块、消息 refc binary 共享（message_ds GROUP_BATCH_*）
- ⚠️ 高流量进程 message_queue_data 未显式设置（默认 on_heap）——与每帧 hibernate 组合后内存风险受控（P2-6 联动验证）

### Ch.3 Binary 与内存
- ✅ 热路径 binary/iodata；json/protobuf 编解码直接产出 binary
- ✅ append 优化未被破坏（无前插、无多版本累积器）
- ✅ gpb 生成解码器内置 binary:copy（textguard_pb.erl:605 等）
- ⚠️ 81 处 binary:part 中存入长生命周期状态者为残余观察面（二轮可用 binary:referenced_byte_size 抽查）

### Ch.4 ETS
- ✅ 每表 owner 明确：六表 application master 持有（imboy_app.erl:79-93 含防短命进程持表的攻击面注释）、login_attempt_ds 表随其 gen_server、DELIVERY_BUDGET_TAB 启动建（imboy_syn:init）
- ✅ 读改写全部 update_counter/update_element/select_replace
- ✅ 遍历安全：budget 表操作全部单键；无 unsafe first/next 手工遍历
- ✅ 对象小（计数/槽位/元数据行）；无 2MB 巨对象模式
- ✅ persistent_term 仅存近乎不变数据；并发选项按表用途设置（budget 表 read/write_concurrency）

### Ch.5 gen_server / gen_statem
- ✅ 长命进程全 behaviour 化；无手写长命循环（worker_loop 为受管短命任务执行器）
- ✅ 慢工作不占回调：user_server 分片异步（cast_online→分片队列）；ACK 持久化在连接进程内属 per-connection 并行而非共享服务器阻塞
- ✅ 定时器用 send_after/start_timer（msg_burn 首跑延迟 30s；moderation_sweep send_after）
- ✅ 高量生产者有背压：user_server 队列上限+丢弃日志；WS 投递预算门（BE-14）
- ⚠️ handle_call 内跨服务器 call 静态图未穷尽（15 模块候选，抽查无环）

### Ch.6 分布式
- N/A 单节点（dist 锁 127.0.0.1）；跨节点投递已按 "unknown 语义" 实现（erpc + Deadline + 兜底重路由）——未来多节点时按教程 §5 checklist 重审 zdbbl/TLS/global

### Ch.7 应用与 Release
- ✅ applications 完整（erlang.mk 自动生成，ebin/imboy.app 实证 46 项）
- ✅ release 形态启动验证存在（prodready_e2e 车道 + install.sh）
- ⚠️ start/2 含迁移等重活（P2-4，有 fail-fast 论证与蓝绿开关）
- ✅ 手写循环无（除受管 worker_loop 一次性任务）；热加载路径未依赖局部递归

### Ch.8 测试
- ✅ 无共享注册名泄漏（quality 门既有 silent-name 清理，记忆 50 条 eunit 修复链）
- ✅ PBT 存在（test/pbt/ 8 文件：tsid/cjson/dt/manifest/oracle_faults）
- ✅ 故障注入 66 文件（kill/meck:exit）；监督恢复有测（CI-00 run10 即 kill 风暴测试）
- ⚠️ 缺口：WS 投递重试/预算门这类状态机无模型化属性测试（见 testing-gap §5）
- ⚠️ timer:sleep 242 处（体系内既有，不重构）

### Ch.9 可观测性
- ✅ 结构化报告与级别门控（safe_log）；日志不落整包载荷（P2-1 注释 "protobuf 整帧日志禁产"）
- ✅ 邮箱长度用 message_queue_len（且作为预算门真源）
- ❌ system_monitor 未安装（P2-1）
- ❌ BEAM 维度指标与 drop 计数（P2-2）
- ✅ 重要事件有计数（elib_metric + http metrics + budget counters oracle）

### Ch.10 调度器与性能
- ✅ CPU 工作分散（per-connection 进程、分片 worker、批拉 writer）
- ✅ 生产直方图（imboy_msg_store_batch_seconds）
- ❌ 活跃消息吞吐 bench 缺位（P2-6）；scheduler utilization 无采样（P2-5）
- ✅ 无 NIF（调度器阻塞面=0，纯 Erlang）

### Ch.13 gen_statem 深度
- N/A 仅 barrel_mcp_registry、imboy_plugin_lifecycle 两处；抽查无 "data 应升 state"、无 catch-all postpone、无回调内 receive

### Ch.11/12（NIF/Elixir）
- N/A：无自研 NIF、无 Elixir 混合

## 3. 二轮建议动作（如授权）

1. 静态调用图穷尽 15 个 gen_server 候选模块的 call 边，闭环 Ch.5 §4.6 检查。
2. binary:part 存储点抽样跑 referenced_byte_size 诊断脚本（只读）。
3. metrics/system_monitor 扩展实施（P2-1/2/3 合并批次）。
