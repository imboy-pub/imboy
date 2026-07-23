# IMBoy 架构演进路线（Architecture Roadmap · 依赖/状态驱动，非日历）

> Staff Engineer 视角 · 基于 `docs/review/` 全量评审 · 基线三仓 `1.0.0-alpha.15` · 日期 2026-07-22
> **三条铁律**：① 不推倒重来 ② 不破坏兼容 ③ 渐进升级
> 本文是总纲；专项见 `performance-roadmap.md` / `security-roadmap.md` / `testing-roadmap.md` / `engineering-roadmap.md`，时间线见 `2026-roadmap.md`。任务编号跨文档唯一。

---

## 0. 兼容性宪法（所有任务必须遵守）

评审确认协议主链已经多轮真机 QA 沉淀，任何演进都不得让存量客户端离线。据此定四条不可违反的施工规范：

1. **协议只增不改**：新字段/新 type 只加不删；proto 字段号永不复用；枚举旧值永不改语义。
2. **双路兼容窗口**：下行编码改动一律"新格式为主 + 旧格式兜底"，客户端"pb 先试 + JSON 兜底"双路解码（评审已证 `websocket.dart:726-744` 天然兼容），保留 ≥2 个发布周期。
3. **数据迁移双写过渡**：schema 演进先加列/加表双写，灰度读切换，确认后再停旧写；迁移遵循现有 strict 乱序 + advisory lock + 单文件事务机制。
4. **开关先行**：行为变更走 `imboy_feature` / `sys.config` flag，默认保持旧行为，灰度放量后翻默认。

---

## 1. 两年演进主线（一句话）

> **从"约定驱动的成熟骨架"演进为"机制驱动的可规模化平台"——不重写任何一层，而是把评审发现的每一处"正确范本 + 未推广"收口成默认路径、把每一道软门收紧成硬门。**

评审的头号结论是：IMBoy 正确性依赖约定/注释/纪律而非机制,但库内到处已有正确范本(`agent_rate_limiter` 并发 ETS 表 write_concurrency、`recharge_order_repo:271` 钱包守卫、`webrtc_ws_logic` ACK 预编码、`check_module_boundaries`+xref=0 门禁)。**演进 = 推广范本 + 机制化,而非发明新架构。**

---

## 2. 阶段划分与优先级映射

> 阶段=波次(Wave)，**无日历日期**；进入条件是"上一波闸门绿"，出口标志绿即达成、解锁下一波。执行序列详见 `2026-roadmap.md`。

| 优先级 | 阶段/波次 | 进入条件 | 主题 | 出口标志（闸门）|
|---|---|---|---|---|
| **P0** | Wave 0 稳定化 | 无（立即可启动）| 清发布阻断项，达到可售最小安全线 | 5 个阻断项清零，可对外私有化交付 |
| **P1** | Wave 1 GA 硬化 | Wave 0 闸门绿 | 协议/可靠性/权限收口，软门变硬门 | 1.0 GA，SDK 端到端通，CI 全硬门 |
| **P2** | Wave 2 规模化 | Wave 1 闸门绿 | 并发热路径重构、集群正确性、E2EE 收敛 | 单节点吞吐提升一个量级，支持集群 |
| **P3** | Wave 3 平台化 | Wave 2 闸门绿 | IM+Agent 平台职责分离、多租户成熟、可观测闭环 | 平台级 SLA，租户隔离达标 |

---

## 3. 架构级任务（ARCH-xx，跨领域结构性）

> 单点性能/安全/测试任务见对应专项文档。此处只列改变架构形态的任务。

### ARCH-01 · 鉴权属性声明式化（消灭横切真相源分裂）
- **目标**：把 open/option/免签豁免从 4 处平行 path 字符串前缀，收敛为路由 `Opts` 上的声明式属性，中间件统一消费。
- **原因**：评审 P0-1（`auth_middleware.erl:34` 前缀死代码）、P1-A2（setup 401）、billing 越权同源于一次路由前缀硬切；`required_feature` 已有声明先例却无人消费（`imboy_router.erl:857-867`）。
- **收益**：消除整类"改路由忘改鉴权"静默失效;新增端点鉴权属性与路由同处定义,不可能漏配。
- **风险**：中；触及全站鉴权入口。缓解：先补集成测试覆盖现有豁免矩阵作为回归网，再重构，行为不变。
- **影响范围**：`src/api/auth_middleware*.erl`、`src/adm/adm_auth_middleware.erl`、`imboy_router.erl`。
- **修改模块**：鉴权中间件 3 个 + 路由表。
- **工作量**：L（相对 effort，非工期）。**PR 数**：4–6（①豁免矩阵测试 ②P0-1 前缀急修 ③Opts 声明层 ④各中间件切换 ⑤删旧 path 维护 ⑥文档）。
- **验收**：所有豁免/开放/签名端点由路由 Opts 单一声明；集成测试覆盖支付回调、webhook、setup、passport 签名门；`grep` 无平行 path 前缀维护残留。

### ARCH-02 · WS 同步回执路径与投递管道对齐（协议腐蚀单点根治）
- **目标**：把 `ws_reply(protobuf, v2, Msg)` 的有损 protobuf 白名单转换，改为与异步投递管道一致的"v2 帧 + JSON payload"。
- **原因**：评审证明这是协议唯一系统性腐蚀源——不在 MsgDirection 枚举的 type 归零、不在 `to_pb_map` 的字段（in_reply_to/error/code）静默蒸发；直接导致 P1-P1（C2S ACK 丢 type）、P1-P5（C2G_ERROR 蒸发）。webrtc 已用此法修过一次（`webrtc_ws_logic.erl:44-56`）。
- **收益**：一次消除 C2S ACK、C2G 错误、in_reply_to 缺失、及未来所有新 type 的整类缺陷；客户端双路解码天然兼容,零客户端改动。
- **风险**：低；改动约一个函数，客户端已双路兼容。
- **影响范围**：`src/api/websocket_handler.erl:814-818`、`imboy_codec.erl`。
- **修改模块**：`websocket_handler`、`imboy_codec`。
- **工作量**：S（≤3 pd）。**PR 数**：1–2。
- **验收**："非快乐路径 × v2 编码"矩阵测试全绿；C2S 出站确认、C2G 拒发错误在 v2 连接可达客户端。

> 范本措辞更正：`agent_rate_limiter` 实为**单 named_table + write/read_concurrency**（`agent_rate_limiter.erl:84`），非"分片 ETS"。它对 ACK 定时器仍是正确范本（并发 ETS 表优于 depcache 单 gen_server 串行），但下文凡称"分片 ETS"应读作"**并发 ETS 表（write_concurrency）**"。

### ARCH-03 · 协议契约 CI 门禁（proto / OpenAPI / ws_url 三 diff）
- **目标**：protobuf 三端同源、OpenAPI 全覆盖、ws_url 契约进 CI 硬门。
- **原因**：P1-P4（app 幻影枚举）、P1-P6（默认 ws_url 404）、OpenAPI 仅覆盖 ~130/278 路由；proto 双拷贝当前一致但无门禁。
- **收益**：协议漂移在 PR 阶段拦截,杜绝"生成物与 proto 分叉""配置指向不存在路由"。
- **风险**：低。
- **影响范围**：三仓 CI + proto/OpenAPI。
- **修改模块**：`.github/workflows`、`regen_protobuf.sh`。
- **工作量**：M（相对 effort，非工期）。**PR 数**：3（后端 proto diff / app regen diff / OpenAPI 覆盖门）。
- **验收**：`regen_protobuf.sh && git diff --exit-code` 进 app CI；proto 双拷贝 diff 进后端 CI；ws_url 指向真实路由由 preflight 校验。

### ARCH-04 · SDK 契约对齐 + 端到端冒烟（对外集成面复活）
- **目标**：修 imboy-sdk-js 全部契约漂移，加最小 E2E 冒烟作发版门禁。
- **原因**：评审证 SDK 从未跑通端到端——握手/登录/确认/端点四处全断（P1-P3、协议 #3–6）；SDK 是对外售卖面。
- **收益**：SDK 可用于第三方集成,售卖面成立。
- **风险**：低（独立仓,不影响主链）。
- **影响范围**：`imboy-sdk-js/`。
- **修改模块**：`passport.ts`、`websocket.ts`、`e2ee.ts`。
- **工作量**：M。**PR 数**：3–4。
- **验收**：登录→握手→C2C→SERVER_ACK→CLIENT_ACK→CONFIRM 全链路 E2E 绿,进发版门禁。

### ARCH-05 · Flutter 运行时架构收敛（三套并存 → 单一模式）
- **目标**：把 Riverpod 图外手写单例（WS/Retry/Message）、`lib/modules/` DDD 试点（38 文件）、传统 page/service 三套运行时，渐进收敛为单一约定。
- **原因**：评审指出三套并存中间态是 Flutter 侧最大结构债；巨型 Notifier（ChatNotifier 13+ 字段）与手写单例交织是历史 bug 密度最高区。
- **收益**：降低新人认知负荷,压制状态管理类复发 bug。
- **风险**：中；核心运行时重构。缓解:选一个方向（推荐把手写单例纳入 Riverpod 图）,逐模块迁移,每步真机验收。
- **影响范围**：`imboyapp/lib/` 消息与连接核心。
- **修改模块**：WS/Retry/Message 运行时、ChatNotifier。
- **工作量**：XL（>1 月，分期）。**PR 数**：8–12（逐模块）。
- **验收**：确定单一运行时约定并文档化；`lib/modules/` 试点或转正或下线,不再三套并存。

### ARCH-06 · 后端 IM + Agent 平台职责分离
- **目标**：把顶层 `imboy_sup`（实测 18 child spec，混 MCP/AI Agent/插件三套扩展）按子系统拆分监督子树，明确故障隔离边界。
- **原因**：后端已从纯 IM 演进为平台；顶层 supervisor 职责过重，扩展子系统崩溃影响面不清。
- **收益**：故障隔离,扩展子系统可独立重启/降级,不拖累 IM 主链。
- **风险**：中；改监督树结构。缓解:one_for_one 下逐个下沉为子 supervisor,行为等价。
- **影响范围**：`imboy_sup.erl`、MCP/AI/插件子系统。
- **工作量**：L。**PR 数**：4–6。
- **验收**：IM 主链、Agent/MCP、插件三子树独立监督；杀死 Agent 子树不影响消息收发（测试验证）。

### ARCH-07 · 执行已签字的 Olm-only cutover（RSA 下线）
> **2026-07-22 更正**：本项原写"为三代共存制定收敛 ADR、Olm C2C 未来启用（P2/P3）"，方向错误。事实是 **cutover 已由用户定夺并签字**：Olm 单聊 + Megolm 群，收端**永不回退 RSA**，RSA 仅 decrypt-only 读历史。后端 B.0–B.3.3 已提交，**迁移 42–46 已落盘**（`00000042_olm_prekeys` … `00000046_compliance_key_drop_private`，实测最大迁移号 47）。故本项从"制定 ADR"改为"**执行已定方案、完成 RSA 下线**"，时序提前到 P0/P1。
- **目标**：执行 Olm-only cutover 剩余环节——启用客户端 Olm C2C（`useOlmForC2C` 当前 false，`chat_network_service.dart:562`）、补 proto `E2EEMeta` 的 olm 子对象（P1-P7）、完成 RSA decrypt-only 收尾与 `private_key_encrypted` 下线（迁移 46 已 DROP）。
- **原因**：方案已签字、迁移已落盘、后端已提交；剩客户端启用与协议对齐。
- **收益**：密钥体系统一，RSA 写路径下线，协议分叉收敛。
- **风险**：高；收端不回退 RSA 是硬约束（旧客户端不兼容，alpha.15 已接受）。缓解：保留 RSA decrypt-only 读历史，无 Olm 设备跳过；不把 RSA fallback 冒充 Olm PASS（真机验收）。
- **影响范围**：E2EE 客户端（启用开关）+ proto E2EEMeta + RSA 写路径下线。
- **工作量**：L（大部分已完成，剩启用+对齐+收尾）。**PR 数**：客户端启用/协议/收尾约 4–6。
- **验收**：Olm C2C 真机验收 PASS（非模拟器、非 RSA fallback 冒充）；proto E2EEMeta 含 olm 字段；RSA 仅 decrypt-only。

---

## 4. 兼容性风险总控

| 演进动作 | 兼容策略 | 回滚方式 |
|---|---|---|
| 协议编码（ARCH-02） | 客户端双路解码,服务端只改回执编码 | 单函数 revert |
| 鉴权重构（ARCH-01） | 豁免矩阵测试锁定行为,重构保持等价 | flag 切回旧中间件 |
| schema 演进 | 加列双写→灰度读→停旧写 | 迁移 down 脚本 |
| 运行时收敛（ARCH-05） | 逐模块迁移,每步真机 | 保留旧路径至下版 |
| Olm 切换（ARCH-07） | 新消息切、RSA decrypt-only 读历史 | flag 回退（收端不回退,见 E2EE 记忆约束） |

---

## 5. 任务总表（跨文档索引）

| 编号 | 任务 | 优先级 | 工作量 | 专项文档 |
|---|---|---|---|---|
| ARCH-01 | 鉴权声明式化 | P0/P1 | L | 本文 + security |
| ARCH-02 | WS 回执路径对齐 | P0 | S | 本文 |
| ARCH-03 | 协议 CI 门禁 | P1 | M | 本文 + testing |
| ARCH-04 | SDK 对齐+冒烟 | P1 | M | 本文 + testing |
| ARCH-05 | Flutter 运行时收敛 | P2 | XL | 本文 |
| ARCH-06 | 平台职责分离 | P3 | L | 本文 |
| ARCH-07 | E2EE 收敛 | P2/P3 | XL | 本文 + security |
| PERF-01…08 | 并发热路径 | P0–P2 | — | performance |
| SEC-01…08 | 安全硬化 | P0–P2 | — | security |
| TEST-01…07 | 测试机制化 | P1–P2 | — | testing |
| ENG-01…08 | 工程效能 | P1–P3 | — | engineering |

发布阻断五清单（P0，必须先于一切）：ARCH-02、PERF-01（user_server）、PERF-02（depcache）、SEC-01（计费越权）、以及法务 AGPL 裁决 + 钱包冻结资金（见 security/performance）。
