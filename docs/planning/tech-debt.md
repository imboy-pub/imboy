# IMBoy 技术债务与优化路线（Tech Debt & Roadmap）

> Fact-based Review 汇总 · 基线三仓 `1.0.0-alpha.15` · 日期 2026-07-22
> 本文把 9 份评审的发现收敛为**可执行路线**。风险逐条明细见 `risk-report.md`；此处按"根因 → 批次"组织，供排期。

---

## 一、贯穿全库的头号根因：机制缺位（4+ agent 独立收敛）

后端、Flutter、Admin、数据库、测试五个方向的评审**各自独立**得出同一结论：

> **IMBoy 的正确性大量依赖约定 / 注释 / 纪律，而非 schema 约束 / 类型 / lint / CI 硬门。**

同一模式的多处佐证（均引自各专项文档）：

| 领域 | "约定"现状 | 应有的"机制" |
|---|---|---|
| Flutter autoDispose 陷阱 | 只在注释里防御，67 个 Notifier 复发面 | custom_lint 规则 |
| Flutter 裸 URL / 800 行红线 / token 化 | 仅写在 CLAUDE.md | lint 门禁 |
| Flutter DDL schema | 常量 + 2 个 .sql 镜像 + CLAUDE.md 四处手工同步且已矛盾 | 单一真源生成 + CI 校验 |
| Admin TSID / 分页 / zod | 约定执行度高但靠自觉，旁路未收口 | lint / 全局拦截 |
| 后端鉴权豁免 | open/option/免签以 4 处平行 path 字符串维护 | 声明到路由 Opts（`required_feature` 已有先例但没人消费） |
| 数据库钱包不变量 | 只写注释，未下沉 schema | 表级 CHECK |
| 数据库注入防线 | raw 逃生门靠约定 | 结构封死 + 断言 |
| 后端分层边界 | ✅ **已机制化**（`check_module_boundaries.sh`，全仓仅 1 处破窗） | — 成功样板 |
| 后端 xref | ✅ **已机制化**（xref=0 硬门） | — 成功样板 |

**乐观信号——"正确范本 + 未推广"并存态**：代码库内部到处有对的做法，只是没推广。`agent_rate_limiter`（并发 ETS 表 write_concurrency 正确范本，`agent_rate_limiter.erl:84` 单 named_table+write_concurrency 非分片，对照误用 depcache 的 P0-3）、`recharge_order_repo:271`（钱包正确守卫，对照 `wallet_repo` 的 P1-D1）、`webrtc_ws_logic`（ACK JSON 预编码正确范本，对照 C2S 的 P1-P1）、`check_module_boundaries` + xref=0（门禁成功样板）。**说明债务是"未收口"而非"不会做"——把已有范本 + 已有 ratchet 框架推广即可，成本可控。**

**次级根因——横切变更无单一真相源**：2026-07 一次路由前缀硬切，独立引发 3 个 P0/P1（P0-1 auth 前缀、P1-A2 setup 401、以及 billing 归属）。鉴权属性散落 4 处平行维护，任一遗漏静默失效。

---

## 二、执行路线（按批次，每批可独立交付）

### 批次 0 — 发布阻断项（P0，发售/上线前必清）

| 项 | 动作 | 证据 |
|---|---|---|
| P0-1 支付回调死代码 | 修 `auth_middleware:execute` 分支为 `/api/v1/` 前缀（或统一委派），恢复 `auth_middleware_api_v1` 生效；加一条集成测试覆盖支付回调免签路径 | `src/api/auth_middleware.erl:34` |
| P0-4 AGPL 法务 | 产品决策三选一（开源本体 / 购 vodozemac 商业授权 / 换非 AGPL 绑定），在发布里程碑前定夺 | `imboyapp/pubspec.yaml:221-222` |
| P1-A1 计费越权 | billing 9 端点补 `current_uid` 归属校验（照 owner_uid 红线范式），`invoice_pay` 优先 | `src/api/billing_handler.erl:70-253` |
| P1-A2 首启 401 | `adm_auth_middleware` 增加 setup 分支 / 查 open/0 | `src/adm/adm_auth_middleware.erl:19-44` |
| P1-D1 钱包冻结资金 | 借记守卫补 `frozen`/`status` 校验（照 `recharge_order_repo:271`），加表级 CHECK `frozen<=balance` | `src/repo/wallet_repo.erl:117` |

### 批次 1 — 把已有 ratchet 收紧为硬门（性价比最高，直接压制头号根因）

- 后端 full-eunit 按 `backend-ci.yml` 注释里的自定计划从 continue-on-error 收紧为阻塞门；dialyzer 同理
- 三仓补覆盖率阈值门（当前仅 `test -f lcov.info`）
- admin Playwright E2E 接入 CI（当前零引用）
- protobuf regen diff 进 CI：`regen_protobuf.sh + git diff --exit-code`（app 端）+ proto 双拷贝 diff（后端）
- SDK 契约冒烟作发版门禁（P1-P3 的 5 项漂移靠联测即可拦）

### 批次 2 — 机制化关键约定（新增 lint / schema / 断言，防复发）

- Flutter custom_lint：autoDispose 显式化（P1-F1）、裸 URL 禁令、800 行红线、token 化
- Flutter DDL 单一真源：以常量生成 .sql 副本 + CI 校验；无脚本降级显式失败（P1-F3、P1-C3）
- 数据库：钱包不变量下沉表级 CHECK（P1-D1 兜底）、`elib_pg_sql` raw 逃生门加正则断言/删死代码（P1-D3）、连接级 `statement_timeout`（P1-D2）
- 后端鉴权属性声明到路由 Opts，消灭 4 处平行 path 维护（消除 P0-1 类复发）

### 批次 3 — 并发热路径重构（P0-2/P0-3 + 性能 P1）

- P0-3 depcache 误用：ACK 定时器/标志改并发 ETS 表 write_concurrency 或存 WS 进程 State（范本 `agent_rate_limiter.erl:84`，非分片）
- P0-2 user_server 单进程：上下线拆出 DB 写与 fanout，离线检查移出单进程、改按阈值查询而非拉 5000 行
- P1-PF1 C2G 扇出异步化；P1-PF2 投递管道去 JSON 中间格式；P1-PF3 msg_store_worker 扩 worker
- P1-D2 连接池 sleep 盲重试改真超时

### 批次 4 — 可靠性与集群正确性

- P1-C1 syn 远端 Pid + start_timer 集群崩溃（单节点当前无感，集群化前必修）
- P1-C2 message_retry 前 100 条截断改全量/分页
- P1-P1/P1-P2 ACK 链路：C2S type 丢失 + 大小写敏感双修，统一同步回复为 JSON 预编码

### 批次 5 — 可维护性与清理

- P1-Q2 消息主链路巨型文件拆分（`chat_page.dart` 2234 行等 12+ 文件）
- P1-Q1 阅后即焚静默吞错补日志/上报（`chat_burn_service.dart` 7 处）
- P2 系列：JWT 吊销通道、密码 KDF 升级、密钥拆分、imboy_cache Pid 修复、liveRoom 死资产决策
- P3 文档漂移批量修正（archive_enabled、Vue→React、ADR 补齐 E2EE/支付/LiveKit/MCP）

---

## 三、结构性债务（非单点，需专题）

1. **E2EE 三代方案共存**（RSA / Olm / Megolm，~10 张表）：数据库与客户端都背着历史包袱，`useOlmForC2C=false` 硬门控使 Olm C2C 实际未启用（`chat_network_service.dart:562`）。收敛方向已定（Olm-only cutover，见 roadmap ARCH-07），此处为历史记录。
2. **后端从纯 IM 演进为 "IM + Agent 平台"**：顶层 `imboy_sup` 挂 18 child spec，混入 MCP/AI Agent/插件三套扩展；职责边界与故障隔离需重审。
3. **TimescaleDB 是消息域地基也是最大隐性负债**：队列 1 年 / timeline 30 天 / msg_store 永久的生命周期链条依赖 `msg_archive_enabled`，配置与文档已漂移，单事务内 create_hypertable 有版本耦合。
4. **Flutter 三套运行时并存中间态**：Riverpod 图外手写单例（WS/Retry/Message）+ `lib/modules/` DDD 试点（仅 38 文件）+ 传统 page/service，迁移未完成。
5. **imboy-sdk-js 事实性失联**：5 项契约漂移说明 SDK 按文档记忆手写、从未对后端联测，作为对外集成入口风险高。

---

## 四、一句话总结

> IMBoy 骨架成熟（后端分层纪律是真金、消息 QoS 与 E2EE 设计扎实、约束即文档密度高、可观测栈完整），核心问题不是"不会做"而是"没收口"：正确范本与 ratchet 框架都已存在，最高性价比的动作是**把软门收紧成硬门、把注释约定升级为 lint/schema/CI 机制**。发布前先清 5 个阻断项（P0-1 支付回调、P0-4 AGPL、P1-A1 计费越权、P1-A2 首启 401、P1-D1 钱包冻结资金）。

---

## 五、工程质量维度补充（2026-07-25 自 `reference/engineering/technical-debt.md` 并入）

> 原"工程视角债务笔记"的独有内容；与上文批次不重复的部分。

- **巨型文件清单**（均超 800 行规范）：`imboy_pb.erl` 6018（生成物，可接受）、`adm_channel_handler.erl` 1044、`barrel_mcp_session.erl` 1027、`imboy_router.erl` 977、`moment_logic.erl` 962、`msg_c2c_logic.erl` 937、`websocket_handler.erl` 925；Flutter `chat_page.dart` 2234 + 12+ 个 >800 行。拆分见批次 5。
- **工作区根级卫生**：非 git 仓库的工作区根散落 `REPAIR_PLAN_v2`、`CLEANUP_PLAN.md`、`spikes/`、`releases/` 等，归属需梳理（根 CLAUDE.md 已警示根级只允许 AI/工具配置）。
- **坏死工作流**：`integration_test.yml` 路径失效。
- **翻案防误报**（排查死代码时勿再误报）：`msg_rate_logic` 已接线、`textStream` 已复活（Phase 2）。
- **增量改进编号**（原 ENG 系列，供排期引用）：ENG-01 补 custom_lint / 边界脚本；ENG-03 巨型文件逐个拆分、每步真机验证；ENG-04 liveRoom 冻结确认后下线；ENG-06 文档漂移批量修正。
