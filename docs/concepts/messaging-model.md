# 消息模型（Messaging Model）

> Purpose：定义 IMBoy 消息的四条通道、权威顺序、投递确认与离线补投机制。
> 协议字节级规范见 [WebSocket 协议](../reference/ws-protocol-contract.md)；企业会话/客服会话不走本模型，见各自概念文档。

## Concept：四条消息通道

| 通道 | 表 | WS type | 语义 |
|---|---|---|---|
| 单聊（C2C） | `msg_c2c` | `c2c` | 一对一；E2EE 载荷存于 `e2ee` jsonb 列 |
| 群聊（C2G） | `msg_c2g` | `c2g` | 群扇出；`mentions` 支持 @ 提及 |
| 智能体（C2S） | `msg_c2s` | `c2s` | 客户端 ↔ 智能体会话 |
| 服务端推送（S2C） | `msg_s2c` | `s2c` | 系统通知 |

会话视图由 `conversation` 表聚合（单聊/群聊统一列表），客户端本地 SQLite 同构镜像（`msg_c2c`/`msg_c2g`/`msg_c2s`/`msg_s2c` + FTS5 虚表）。

## Current

**权威顺序与存储**

- 群消息的**服务端权威序**是 `conv_seq`（`msg_store` + `msg_store_seq`），客户端时间戳不构成顺序依据。
- `msg_c2c`/`msg_c2g` 是 TimescaleDB hypertable（7 天 chunk、压缩、保留策略）；`msg_store` 按会话键 30 天 chunk，是游标分页与离线补投的统一读取面。
- 历史拉取：`GET /api/v1/msg/history` 以 conv_seq 游标分页。

**投递确认（Delivery Ack）**

- `msg_delivery` 按 `(msg_kind, msg_id, to_uid, to_did)` 记录设备级 ACK：行存在即该设备已确认；全部活跃设备确认后主行删除。
- WebSocket 层：v2 二进制帧 ack/nack + JSON 文本 `CLIENT_ACK,...` 双轨；发送侧 `ack_retry_cache` 重试。

**离线与补投**

- 在线投递经 `syn` presence 路由；离线消息由 16 个 worker 异步扇出（`user_server` offline 池）。
- 群消息离线时间线 `msg_c2g_timeline`；幂等与授权由 `msg_c2g_request_ledger`（请求哈希账本）+ `msg_c2g_recipient_snapshot`（收件人快照，防退群重入取回旧世代消息）保证。

**消息操作（REST + WS action 双入口）**

- 撤回/编辑/已读/输入状态：REST 端点 + WS action（`message_revoke`、`message_edit`、`message_read`、`message_input`，注册于 `imboy_ws_action_registry`）。
- 转发/置顶/提及/回应/主题（`msg_forward/pinned/mention/reaction/topic`）为 REST 面。
- 阅后即焚 `msg_burn_logic`；限流 `msg_rate_logic`。

**E2EE 消息的边界**

- 服务器只见密文与元数据；Megolm 房间密钥经 WS action `e2ee_room_key` **不透明中转**（c2c/c2g 两路注册），群历史可解密性由成员世代（`group_member_generation`）界定——详见[端到端加密](./e2ee.md)。
- 全文搜索（FTS）排除 E2EE 密文（迁移 33）。

## Contract

1. `msg_type` 内容类型清单（text/image/voice/video/file/location/quote/redPacket/transfer/...）三端共用，客户端对未知类型必须渲染为 `unsupported` 而非崩溃。
2. WS action 命名为 `snake_case`（如 `message_revoke`），不是点分式——新增 action 必须注册进 `imboy_ws_action_registry`，未注册 action 回 `unknown_action`。
3. 消息 ID 格式与幂等：客户端 `msg_id`（varchar 40）+ 服务端 TSID 主键；C2G 请求幂等以 request ledger 为准。

## Constraints

- 单聊密钥有意不做云端备份 → 换设备后历史单聊不可解密是**设计行为**（解密失败占位与引导见客户端），不是缺陷。
- `msg_store`/timeline 有保留期，超期数据走归档策略，客户端不应假设历史无限可拉。

## References

- 协议：`docs/reference/ws-protocol-contract.md`、`api/proto/`（字节级真源）
- 迁移：05/06/07/08/19/111
- 客户端：imboyapp `lib/service/websocket.dart`、`lib/service/message_s2c.dart`（S2C action 真源）
