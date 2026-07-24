# imboy WebSocket 协议语义清晰度审查报告

> 审查日期：2026-07-02
> 审查性质：只读盘点，不含代码改动
> 审查范围：`src/lib/imboy_frame.erl`、`src/lib/imboy_ws_action_registry.erl`、`src/api/websocket_handler.erl`、`src/logic/websocket_logic.erl`、`src/logic/webrtc_ws_logic.erl`、`docs/reference/ws-protocol-contract.md`、`docs/reference/websocket-api-2.md`；延伸取证：`message_router_logic.erl`、`msg_c2s_logic.erl`、`msg_s2c_logic.erl`、`msg_ack_logic.erl`、`elib_retry_config.erl`、`include/imboy_frame.hrl`、`imboy_pb.erl`、`message_policy.erl`
> 核心问题：「这条 WS 通道到底是不是 RPC」是否在协议层说清楚了？

---

## 总体结论

**「这条通道是不是 RPC」在协议层没有说清楚，且答案是"三种语义都有，全靠隐式约定区分"。** 客户端仅凭一个入站帧，无法从帧头或任何显式字段判断它是 ①对我某个请求的响应、②服务端单向推送、还是 ③需要我回执的投递。区分依据是三元组 `(type, action, id是否匹配本地pending)` 的隐式约定，其中 `type` 字段同时承载了方向、消息类别、响应标记三种职责（`C2C_SERVER_ACK`、`CLIENT_ACK_CONFIRM`、`webrtc_offer` 都是 4 向枚举之外的越界值）。v2 帧头的 `Type` 只区分到 C2C/C2G/C2S/S2C 粒度，`RPC_REQ/RPC_RSP`（0x80/0x81）已定义但从未接入——协议层为 RPC 预留了位置，实际 RPC 语义却散落在业务 JSON 里。

当前通道上实际存在的三类语义及其（隐式）表达方式：

| 语义 | 实例 | 客户端如何识别 | 显式标注 |
|------|------|----------------|----------|
| (a) RPC 请求-响应 | `C2S to=sync` → `sync_resp`；发消息 → `C2C_SERVER_ACK`；`CLIENT_ACK` → `CLIENT_ACK_CONFIRM` | 响应复用请求的 `id`，靠本地 pending 表匹配 | ❌ 无 `reply_to`/`request_id` 字段 |
| (b) 单向事件推送 | `S2C action=online/offline/app_upgrade` | `type=S2C` + action 白名单 | ❌ 与 (c) 无法区分 |
| (c) 需 ACK 的投递 | C2C/C2G/S2C 消息投递（服务端按 `elib_retry_config` 重投直到 CLIENT_ACK） | 无任何帧级标记，靠客户端硬编码"哪些 type 要 ACK" | ❌ `FRAME_FLAG_ACK` 存在但业务帧恒 0 |

---

## P0 — 会导致 ACK 闭环断裂 / 状态错乱

### P0-1 v2 连接下，同步响应丢失帧头，CLIENT_ACK_CONFIRM 整条 RPC 闭环失效

- **现象**：`ws_reply/2` 硬编码 `framing = none`（`src/api/websocket_handler.erl:770-771`），而 CLIENT_ACK 的确认/错误响应全部走这条路：`handle_client_ack` 的 `ws_reply(Protocol, AckConfirmMsg)`（`websocket_handler.erl:367,376,387`）、`handle_protobuf_client_ack`（`:433,441,451`）、`handle_protobuf_message_decoded` 的路由 reply（`:730,747,755`）。v2 连接的 `Protocol=protobuf`，于是 `CLIENT_ACK_CONFIRM` 以**裸 protobuf 二进制（无 9 字节帧头）**下发。
- **触发场景**：Dart 客户端在 `imboy.v2` 子协议下发 `MSG_C2S + "CLIENT_ACK,..."`（文档确认这是现行路径，`websocket-api-2.md:546`）→ 服务端回裸 protobuf → 客户端 `_handleV2Binary` 解帧 `bad_magic` → FormatException 只记日志丢弃（`websocket-api-2.md:503`）→ AckManager 永远收不到 confirm → 按 `ackConfirmRetryIntervals` 重发 4 次后放弃。每条下行消息的 ACK 都要多跑 4 轮重试，pending 表持续堆积。
- **对照**：正确写法就在同文件——`dispatch_v2_frame` 的限流响应用了 `ws_reply(protobuf, v2, ...)`（`:285`），异步路径 `websocket_info({reply, Msg})` 也正确取了 `Framing`（`:517-520`）。只有同步 reply 系统性漏了。
- **改进方向**：删掉 `ws_reply/2`，所有调用点强制传 `maps:get(framing, State, none)`。一处根因，全部调用点受益。
- **待核实**：Dart 端是否存在"帧解析失败回退裸 protobuf"的容错路径（需查 `lib/service/websocket.dart` 或真机抓包）；若有，降级为 P2。

### P0-2 客户端发 `type=S2C` + 未注册 action → function_clause → 被误报为 `invalid_json`

- **现象**：`msg_s2c_logic:s2c/4` 只有 6 个 action 子句（`C2C_DEL_EVERYONE`/`C2G_DEL_FOR_ME`/`C2G_DEL_EVERYONE`/`store_shard`/`shard_stored`/`e2ee_key_changed_ack`，`src/logic/msg_s2c_logic.erl:19-237`），**没有兜底子句**。`message_router_logic.erl:73-79` 把任何客户端上行的 S2C 直接转给它。
- **触发场景**：客户端（或旧版本客户端）发 `{"type":"S2C","action":"任何其他值"}` → function_clause 崩溃 → 被 `handle_json_message` 的 catch 兜住（`websocket_handler.erl:502-507`）→ 客户端收到 `action=invalid_json`。消息明明是合法 JSON，错误码却指向"JSON 非法"，客户端按文档建议会"提示检查客户端版本"（`websocket-api-2.md:851`）——完全错误的排障方向。
- **改进方向**：`msg_s2c_logic:s2c/4` 加兜底子句返回 `unknown_action`，与 `route_action` 的兜底（`message_router_logic.erl:147-148`）语义对齐。

---

## P1 — 协议语义缺口 / 体验问题

### P1-1 WebRTC 信令成功路径零响应，失败才有响应（响应语义不对称）

- **现象**：`webrtc_ws_logic:event/4` 成功时 `send_next` 后返回 `ok`——发送方**收不到任何确认**（`src/logic/webrtc_ws_logic.erl:37-41`）；失败时（拉黑/非好友）却返回 S2C reply（`:42-47`）。同一个请求，"有响应"本身成了失败信号。
- **触发场景**：客户端若对 webrtc 帧套用 `sendWithAck` 语义等确认 → 永远超时 → 按 `messageSendRetryIntervals` 重发 offer → 对端收到重复 SDP。这与此前"跨网络通话 SDP glare / `_createAnswer` 双触发"的未解嫌疑直接吻合。
- **改进方向**：webrtc 事件也回 SERVER_ACK（与 C2C 对齐），或在契约文档明文规定"webrtc_* 为 fire-and-forget，客户端禁止重试"。**待核实**：Flutter 端 webrtc 发送是否走 MessageRetry。

### P1-2 CLIENT_ACK 的方向白名单与文档声称的客户端行为冲突（WEBRTC 方向是黑洞）

- **现象**：`validate_ack_params` 只接受 `C2C/C2G/S2C/C2S`（`websocket_handler.erl:643-647`），`process_ack_type` 同样无 WEBRTC 子句（`:872-884`）。而 `websocket-api-2.md:537` 白纸黑字："AckManager 实际发送 4 种方向的 ACK —— C2C / C2G / S2C / **WEBRTC**"。
- **触发场景**：客户端发 `CLIENT_ACK,WEBRTC,...` → `invalid_type` → `CLIENT_ACK_ERROR` → 客户端重试 4 次全败。webrtc 消息 `MsLi=[0]` 不重投，故无重复投递，但 ACK 通道对整类消息失效，staging 清理（`msg_ack_logic.erl:38`）不发生。
- **改进方向**：要么后端白名单加 WEBRTC（映射到 no-op 或 s2c 清理），要么客户端停发。**待核实**：`ack_manager.dart:261` 现行代码是否仍发 WEBRTC 方向。

### P1-3 帧级 NACK/ERROR 已定义、从未使用——所有协议错误都静默丢弃

- **现象**：`FRAME_TYPE_NACK/CLOSE/ERROR` 常量存在（`include/imboy_frame.hrl:57-59`），但 v2 帧解码失败（`websocket_handler.erl:233-236`）、未知帧类型（`:289-291`）、payload 双路解码失败（`:314-321`）全部只记 warn 后丢弃，不向对端回任何帧。收到 NACK 也只记日志（`:264-266`）。
- **触发场景**：新版客户端向旧版服务端发 `MSG_TYPING`(0x25) 等"已占位未实现"类型 → 静默丢弃 → 客户端只能靠超时感知，且无法区分"服务端不支持"与"网络丢失"。
- **改进方向**：未知类型/解码失败回 `FRAME_TYPE_ERROR`（payload 带 reason code），这正是 ERROR 帧存在的意义。

### P1-4 `FRAME_FLAG_ACK` 是装饰位：设了也没人理

- **现象**：`needs_ack/1` 访问器（`src/lib/imboy_frame.erl:196-198`）在 `src/` 中零调用（仅 `test/lib/imboy_frame_tests.erl:224,231` 引用）。`dispatch_v2_frame` 除 ACK 帧的 DIR 位外完全忽略 Flags（`websocket_handler.erl:239` 注释自认）。心跳 ping 设 ACK 位（`imboy_frame.erl:214`），服务端回的是 PONG（按 Type 分派，与 flag 无关）——即"该 ACK 却没走 ACK"的固化实例。
- **触发场景**：任何客户端对业务帧设 ACK 位期待帧级回执 → 永远等不到。
- **改进方向**：要么实现（收到 ACK 位帧即回 `FRAME_TYPE_ACK`），要么在契约文档把该位标记为"仅心跳 ping 装饰性使用，不得依赖"。

### P1-5 帧版本号不校验：v3 帧会被当 v2 处理

- **现象**：`imboy_frame:decode/1` 把 `Ver:8` 绑定为自由变量、不做任何校验（`imboy_frame.erl:111-127`），任意版本号的帧都成功解码。编码侧写死版本 2（`:98`）。
- **触发场景**：未来协议升级到 v3（哪怕只是 Flags 语义变更），旧服务端会按 v2 语义误解析新帧，无任何告警——升级/降级协商规则在帧层不存在，只靠子协议字符串 `imboy.v2` 兜底。
- **改进方向**：`decode` 对 `Ver =/= 2` 返回 `{error, unsupported_version}`；未来版本协商继续走子协议字符串（`imboy.v3`），帧内版本号作为守护断言。

### P1-6 RPC 响应的方向标注系统性错位

- **现象**：服务端→客户端的响应，`type` 字段五花八门：
  - `sync_resp` 响应标 `type=C2S`（`src/logic/msg_c2s_logic.erl:30-35`）——服务端下行却标"客户端到服务端"；
  - `C2C_SERVER_ACK`（`src/domain/message_policy.erl:70`）、`C2S_SERVER_ACK`（`msg_c2s_logic.erl:122`）、`CLIENT_ACK_CONFIRM`（`websocket_handler.erl:359-361`，且 type 与 action 同值）都是 4 向枚举之外的值；
  - proto 枚举干脆叫 `MsgDirection` 却混入 `WEBRTC_OFFER`、`CLIENT_ACK` 等非方向值（`src/imboy_pb.erl:54`）；
  - webrtc 信令原文转发，接收方收到 `type=webrtc_offer`——既非 S2C 也非 C2C。
- **触发场景**：客户端分派器必须为每个越界 type 值写特判；新增一种响应就多一个魔法值，这正是"通道是不是 RPC 没说清楚"在字段层的投影。
- **改进方向**：见文末统一建议——引入 `reply_to` 字段，把"这是响应"从 type 越界值里解放出来，`type` 回归纯方向。

### P1-7 v2 业务 payload 靠内容嗅探区分 JSON/protobuf，帧头无格式位

- **现象**：`dispatch_v2_business_payload` 先试 JSON、失败再试 protobuf（`websocket_handler.erl:306-322`）。帧头 Flags 有 CMP/ENC 位，唯独没有 payload 格式位。
- **触发场景**：理论上一段恰好解析为 JSON map 的 protobuf 字节会被误路由（概率极低）；更实际的代价是每帧都可能白付一次 JSON 解析失败的开销，且"两种格式共存"的契约只存在于代码注释（`:300-302`）里。
- **改进方向**：低优先级。若保留双格式，占用一个保留 flag 位标注格式即可（客户端不设则维持嗅探，向后兼容）。

### P1-8 `e2ee_key_changed_ack` 返回裸 map，违反自家消息结构契约

- **现象**：`msg_s2c_logic.erl:233-237` 返回 `#{<<"status">> => ..., <<"uid">> => ...}`——无 `id`/`type`/`action`/`server_ts`，四个必需顶层字段（`websocket-api-2.md:625-636`）全缺。
- **触发场景**：客户端分派器按 type/action 路由，这条消息无法归类，大概率静默落入 unknown 分支。
- **改进方向**：套用 `ws_validation_error`/`assemble_s2c` 同款信封。

---

## P2 — 文档一致性

**结论：以 `ws-protocol-contract.md`（2026-06-23）+ 代码为准；`websocket-api-2.md`（2026-04-10）有三处已被后续代码演进推翻，但它是唯一记录行为语义（分派规则、ACK 辨析、错误处理）的文档，不能废弃，需修订。**

| # | 冲突点 | websocket-api-2.md | 代码 / contract 真值 |
|---|--------|--------------------|---------------------|
| 1 | Flags bit4-3 | "保留，必须填 0"（`:376`） | ACK 方向位 DIR（`imboy_frame.hrl:33,39-47`；contract `:35`） |
| 2 | v2 ACK 帧方向处理 | "`msg_direction` 硬编码为 C2C"（`:537,545`） | 已从 flags 读方向（`websocket_handler.erl:246-248`，即 2026-06-23 修的 bug） |
| 3 | 服务端重试间隔 | C2C `[0,5000,7000,11000,17000]`、C2G `[0,3500,3500,3000,5000]`（`:919-923`） | C2C `[0,3000]`、C2G `[0]`（`src/lib/elib_retry_config.erl:22-27`；contract §5.1 一致） |
| 4 | FRAME_TYPE_ACK 方向 | "服务端→客户端（预留）"（`:386`） | 服务端实现的是**客户端→服务端入站**处理（`websocket_handler.erl:246-263`），下行从未发过 ACK 帧 |
| 5 | 文档位置 | `imboy/CLAUDE.md` 引用 `docs/reference/websocket-api-2.md` | 该路径不存在，文件实际在 `docs/analysis/`（已 `ls` 证实） |
| 6 | 根 CLAUDE.md | "未确认重试 2s/5s/7s/11s" | 无任何类型是这个序列（最接近的 c2s 是 `[0,5s,7s,11s]`） |

另有一处**命名债**：「消息结构 v2.0」与「imboy.v2 帧协议」双 "v2" 并存，`websocket-api-2.md:7` 自己都要加注区分，而 contract 文档未提及此歧义。

---

## ACK 二义性专项（传输层 ACK vs 业务层 ACK 的边界）

代码里共有**五个不同的 "ACK" 概念**，边界如下（前三者容易混淆，后两者语义独立）：

| 概念 | 载体 | 语义 | 处理入口 |
|------|------|------|----------|
| 帧级 ACK | `FRAME_TYPE_ACK`(0x03) | 传输层"帧已收到"（客户端实际不发，见 `websocket-api-2.md:535-541` 五条理由） | `dispatch_v2_frame`（`websocket_handler.erl:246`）→ 适配成业务 CLIENT_ACK |
| CLIENT_ACK | 文本 `CLIENT_ACK,type,msgid,did` 或 protobuf | 投递回执"消息已收到"，驱动服务端停止重投 + 清理 staging | `handle_client_ack` → `websocket_logic:cancel_timer` + `msg_ack_logic:client_ack`（`msg_ack_logic.erl:21-40`） |
| CLIENT_ACK_CONFIRM | 服务端响应 | ACK 的 ACK（ACK 本身是一次 RPC） | `websocket_handler.erl:358-367` |
| SERVER_ACK | `C2C_SERVER_ACK` 等 | 发送回执"服务端已受理"，驱动客户端停止重发 | `message_policy:build_server_ack`（`message_policy.erl:66-73`） |
| message_read / *_ack action | 业务 action | 已读回执/撤回确认等端到端业务语义 | `imboy_ws_action_registry` 查表分派（`imboy_ws_action_registry.erl:48-59`） |

边界本身划分是清晰的（CLIENT_ACK=已收到，message_read=已读，两者不复用表达），问题在于：①帧级 ACK 与业务 CLIENT_ACK 在服务端被适配进同一条管道，且帧级 ACK 无法携带 `did`，多设备场景语义降级；②`FRAME_FLAG_ACK`（needs-ack 请求位）与 `FRAME_TYPE_ACK`（回执帧）共用 "ACK" 命名但互不相干（前者无实现，见 P1-4）。

---

## 统一建议：「在帧头显式标注语义类型」值不值得？

**不值得动帧头，值得动消息信封。** 理由：

1. 帧头被三端 26+26 个字节级 fixture 锁死（`websocket-api-2.md:538` 明令禁改），改帧头 = 三端同步升级 + 版本协商，成本与收益不成比例；
2. 语义歧义的根源不在帧层，在消息结构层——`type` 一个字段背了方向、类别、响应标记三份职责。帧头加位解决不了 `C2C_SERVER_ACK` 这种越界值。

### 推荐的最小改造路径（按优先级，本轮未动代码）

1. **修 bug，不是改协议**：`ws_reply/2` 丢 framing（P0-1）是实现缺陷，统一走带 State framing 的封装即可修复整个 v2 响应闭环。
2. **消息信封加一个可选字段 `reply_to`**（值 = 被响应消息的 `id`）：所有响应类消息（`*_SERVER_ACK`/`CLIENT_ACK_CONFIRM`/`sync_resp`/校验错误）填上它。客户端从此凭 `reply_to` 是否存在即可区分 (a) RPC 响应与 (b)(c) 推送——纯加性，旧客户端忽略该字段零破坏，长期可让 type 回归纯方向枚举。
3. **兜底补齐**：`msg_s2c_logic` 加 catch-all（P0-2）、未知 v2 帧类型回 ERROR 帧（P1-3）、`decode` 校验版本（P1-5）。
4. **文档收敛**：contract 文档补一节「语义类型总表」（哪些交互是 RPC、哪些是推送、哪些要 ACK、各自的响应/超时契约），并修订 `websocket-api-2.md` 的 6 处过时点、修正 CLAUDE.md 的失效引用。RPC_REQ/RSP(0x80/0x81) 若近期无落地计划，建议在文档明确标记 deprecated，避免后来者以为"RPC 该走那里"。

### 待核实清单（超出本轮只读范围，需查客户端仓 / 真机）

1. Dart `_handleV2Binary` 有无裸 protobuf 回退（影响 P0-1 定级）；
2. `ack_manager.dart` 是否仍发 WEBRTC 方向 ACK（P1-2）；
3. Flutter webrtc 发送是否挂 MessageRetry（P1-1 与 SDP glare 的关联）。
