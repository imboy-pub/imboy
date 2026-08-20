# WebSocket 协议契约（三端镜像基准）

> 定位：三端（Erlang 后端 / Flutter / JS SDK）协议常量的**单一对照基准**，防止漂移。
> 本文是速查契约表，不重复 API 用法（见 [websocket-api-2.md](./websocket-api-2.md)）
> 与帧格式设计（见 `../../../.claude/plans/imboy-frame-protocol.md`）。
>
> 最后更新：2026-08-20

---

## 1. V2 二进制帧格式

```
[Magic:2 = 0x4942 "IB"] [Ver:1 = 2] [Flags:1] [Type:1] [PayloadLen:4 BE] [Payload:N]
```

头部固定 **9 字节**，PayloadLen 为 big-endian uint32，最大载荷 **16 MiB**。

## 2. 帧类型枚举（三端必须一致）

| 区间 | 名称 | 值 |
|------|------|----|
| 控制帧 | heartbeatPing / heartbeatPong / ack / nack / close / error | 0x01 / 0x02 / 0x03 / 0x04 / 0x05 / 0x06 |
| 握手 | handshakeHello / handshakeAuth / handshakeOk | 0x10 / 0x11 / 0x12 |
| 业务 | msgC2C / msgC2G / msgC2S / msgS2C / msgSync / msgTyping / msgRead / msgRecall | 0x20 / 0x21 / 0x22 / 0x23 / 0x24 / 0x25 / 0x26 / 0x27 |
| RPC | rpcReq / rpcRsp（**deprecated**，无落地计划，RPC 语义由信封 `in_reply_to` 承担，见 §9） | 0x80 / 0x81 |

> 2026-07-02 起：`error`(0x06) 已实现——服务端对协议错误（帧解码失败 / 未知帧类型 /
> payload 双路解码失败）回 ERROR 帧，负载为 UTF-8 原因文本（`unsupported_version` /
> `unsupported_frame_type:N` / `payload_decode_failed` / `bad_magic`）；同时
> `imboy_frame:decode` 拒收 `Ver ≠ 2`（`{error, unsupported_version}`，版本协商仍走
> 子协议字符串，帧内版本号为守护断言）。旧客户端对未知下行类型按现有丢弃逻辑处理。

## 3. Flags 位定义

| 位 | 名称 | 含义 |
|----|------|------|
| bit7 | CMP | 载荷 zstd 压缩 |
| bit6 | ENC | 载荷 E2EE（预留） |
| bit5 | ACK | 需要 ACK |
| bit4-3 | DIR | ACK 帧方向：c2c=0 / c2g=1 / s2c=2 / c2s=3 |
| bit2-0 | PRI | 优先级 0-7 |

## 4. 字节序列固化（跨端测试向量）

三端编码必须产出完全相同的字节，守护测试已锁定：

```
heartbeatPing seq=7:
  49 42 02 27 01 00 00 00 02 00 07

ack msgId=0x1234567890ABCDEF (默认方向 C2C):
  49 42 02 00 03 00 00 00 08 12 34 56 78 90 AB CD EF
```

## 5. 重试策略（两套语义，勿混淆）

### 5.1 服务端投递重试 — `elib_retry_config.erl`（按消息类型）

服务端「投递消息给接收端 → 等接收端 CLIENT_ACK → 超时重投」的节奏：

| 类型 | 间隔 (ms) |
|------|-----------|
| c2c | `[0, 3000]` |
| c2g | `[0]` |
| c2s | `[0, 5000, 7000, 11000]` |
| s2c | `[0, 1500, 1500, 3000, 5000, 7000]` |
| pull | `[8000, 10000, 20000]` |
| notice | `[0, 5000, 10000]` |

### 5.2 客户端侧重试 — `retry_policy.dart`（Flutter）/ SDK

客户端「发出后等服务端确认」的重试，**独立于服务端投递重试**，不镜像：

| 语义 | 常量 | 间隔 (ms) | 使用方 |
|------|------|-----------|--------|
| 发消息等服务端确认 | `messageSendRetryIntervals` | `[3000, 5000, 10000, 20000]` | Flutter `MessageRetry`、SDK `sendWithAck`（`ACK_RETRY_INTERVALS_MS`） |
| 发 ACK 等服务端 confirm | `ackConfirmRetryIntervals` | `[3000, 5000, 10000, 15000]` | Flutter `AckManager` |

> 历史教训：`ack_manager.dart` 曾误注「服务端统一 2s/5s/7s/11s」、SDK 曾误注「3s/5s/10s 最多 3 次」，
> 实际服务端按类型不同、客户端按语义不同。本次（2026-06-23）已统一到上表。

## 6. WS action 路由（`message_router_logic.erl`）

| 类型 | 支持的 action |
|------|---------------|
| C2C | `message_revoke` / `message_revoke_ack` / `message_edit` / `message_edit_ack` / `message_read` / `message_read_ack` |
| C2G | `message_revoke` / `message_revoke_ack` / `message_edit` / `message_edit_ack` |
| S2C | `pull_offline_msg` / `apply_friend` / `apply_friend_confirm` / `message_revoke_ack` / `message_read_sync` / `channel_*`（`channel_message` / `channel_message_edited` / `channel_message_revoked` / `channel_message_deleted` / `channel_updated` 等，发射点见 `src/logic/channel_logic_notify.erl`）等（完整清单见 Flutter `message_s2c.dart` `switchS2C`） |

> 2026-08-20 核对：历史版本此处列过的 `user_muted`、`c2c_revoke` 在后端 `src/` **无发射点**
> （grep 零命中，撤回实际下发的 action 是 `message_revoke_ack`），不属于现行契约，
> 客户端不应依赖；已从上表移除。

## 7. 三端实现指针

| 端 | 帧编解码 | 重试策略 | 测试 |
|----|---------|---------|------|
| 后端 Erlang | `src/lib/imboy_frame.erl` + `include/imboy_frame.hrl` | `src/lib/elib_retry_config.erl` | `test/lib/elib_retry_config_tests.erl`、imboy_frame 相关 |
| Flutter | `lib/service/protocol/imboy_frame.dart` | `lib/service/retry_policy.dart` | `test/service/imboy_frame_test.dart`、`retry_policy_test.dart` |
| JS SDK | `src/protocol/imboy-frame.ts` | `src/websocket.ts` (`ACK_RETRY_INTERVALS_MS`) | `test/imboy-frame.test.ts`、`test/ack_retry.test.ts` |

## 8. 变更协议

修改任一协议常量前，必须同时更新三端实现 + 三端守护测试，并更新本表的「最后更新」日期。

## 9. 语义类型总表（RPC vs 推送 vs 回执，2026-07-02 新增）

`type` 字段历史上背了方向、类别、响应标记三份职责。自 2026-07-02 起，**响应类消息以可选
顶层字段 `in_reply_to`（= 被响应请求的 `id`）显式标注**；客户端凭其存在即可判定
"这是对我某个请求的响应"，无需为越界 type 值写特判。纯加性，旧客户端忽略零破坏。

> ⚠️ 命名辨析：`in_reply_to`（下行响应，binary=请求 id）≠ `reply_to`（上行引用回复，
> map `#{msg_id, from_id}`）。两者永不混用。
> ⚠️ 生效范围：protobuf 下行经 `to_pb_map` 只保留 schema 字段，`in_reply_to` 当前仅
> JSON 通路生效；proto 增字段需三端同步（待办）。

| 交互 | 请求方 | 响应/推送 | 带 `in_reply_to` | 超时/重试契约 |
|------|--------|-----------|------------------|---------------|
| 发消息（RPC 式） | 客户端 C2C/C2G/C2S 消息 | `C2C_SERVER_ACK` / `C2G_SERVER_ACK` / `C2S_SERVER_ACK` | ✅ | 客户端按 §5.2 `messageSendRetryIntervals` 重发；服务端幂等短路（stage duplicate 只补 ACK） |
| 增量同步（RPC 式） | C2S `sync` | `sync_resp` | ✅ | 客户端自行超时重试 |
| 投递回执 | 客户端 `CLIENT_ACK,type,msgid,did` | `CLIENT_ACK_CONFIRM`（失败为 `CLIENT_ACK_ERROR`） | ✅（ERROR 在请求 id 可知时带） | 客户端按 §5.2 `ackConfirmRetryIntervals` 重发 ACK；**消费语义 per-device**：服务端按 (msg_id, uid, did) 写送达标记（`msg_delivery`），全部活跃设备确认后才清主行 |
| 校验/限流错误 | 任意非法请求 | `ws_validation_error`（S2C + action=错误码） | ✅（请求 id 可知时） | 一次性，无重试 |
| 服务端推送 | —（服务端发起） | C2C/C2G/S2C 下行消息 | ❌（无 in_reply_to 即推送） | 服务端按 §5.1 投递重试，等 CLIENT_ACK |
| 帧层协议错误 | 任意非法帧 | `FRAME_TYPE_ERROR`(0x06) 帧 | —（帧层，无信封） | 一次性，连接保持 |

### 2026-07-02 新增下行 action

| action | 触发 | payload | 消费方 |
|--------|------|---------|--------|
| `message_read_sync` | C2C 已读落库后，同步给**阅读者本人**（含其离线设备，save 落 msg_s2c） | `msg_id` / `peer` / `read_at` | 客户端更新对应会话未读数；阅读设备自身收到时按已读状态幂等忽略；旧客户端按未知 action 忽略 |

### 2026-08-20 补登：channel 下行通知 action（发射点 `src/logic/channel_logic_notify.erl`）

| action | 触发 | payload | 消费方 |
|--------|------|---------|--------|
| `channel_message_edited` | 频道消息编辑成功后通知全部订阅者（save 落 msg_s2c，离线可拉） | `channel_id` / `message_id` / `content` / `edited_at`（⚠️ `edited_at` 为 `elib_dt:now()` 生成的 **RFC3339 微秒字符串**，非整数毫秒，见 api-format.md 时间戳格式一节） | 客户端按 `message_id` 原位更新消息内容与编辑标记 |
| `channel_message_revoked` | 频道消息撤回后通知全部订阅者 | `channel_id` / `message_id` / `revoked_by` / `revoked_at` | 客户端按 `message_id` 将消息置为撤回态 |
| `channel_message_deleted` | 频道消息删除后通知全部订阅者 | `channel_id` / `message_id` | 客户端按 `message_id` 移除消息 |

### REST 离线接口的设备维度（2026-07-02）

`GET /offline` 与 `POST /offline_ack` 新增**可选** `did` 参数：携带时 C2C/S2C 按设备
过滤/标记（配合 `msg_delivery`）；缺省保持按 uid 的旧语义（旧客户端零破坏，但多端
场景存在丢消息风险，客户端应尽快带 `did`）。
