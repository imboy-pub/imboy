# ImBoy WebSocket API 规范 v2.0

> Last Updated: 2026-04-10  
> Status: 长期协议契约文档  
> Scope: WebSocket 连接、消息结构、错误约定与迁移说明  
> Source of truth: `src/imboy_router.erl` + `src/api/websocket_handler.erl` + `src/logic/websocket_logic.erl` + `src/ds/message_ds.erl` + `src/ds/websocket_ds.erl` + `src/lib/imboy_codec.erl` + `src/lib/imboy_frame.erl` + `include/imboy_frame.hrl`  
> Note: 本文中的 `v2.0` 指消息结构版本，`imboy.v2` 指分层二进制帧协议版本，二者相互独立。  
> Related docs: `docs/api/rest-api.md`, `docs/api/e2ee_server_persisted_shard_contract_v1.md`, `docs/operations/security.md`, `proto/imboy.proto`, `.claude/plans/imboy-frame-protocol.md`

## 变更记录 (Changelog)

| 日期 | 说明 |
|------|------|
| 2026-04-10 | 引入 `imboy.v2` 分层二进制帧协议：新增 9 字节 frame header、帧类型枚举、Flags 位图、心跳 ping/pong、frame 层 ACK/NACK；`Sec-WebSocket-Protocol` 新增 `imboy.v2` 优先项；前后端双层心跳（WS 传输层 + IMBoy frame 层）；保留现有 `imboy-protobuf` / `imboy-json` / `text` 子协议回退路径。详见 [v2 二进制帧协议（imboy.v2）](#v2-二进制帧协议imboyv2)。 |
| 2025-01-19 | 消息结构 v2.0：`msg_type` / `action` / `e2ee` 字段从 payload 提升到顶层。 |
| 2025-01-06 | 初版发布。 |

## 目录

- [概述](#概述)
- [连接管理](#连接管理)
- [传输协议](#传输协议)
- [v2 二进制帧协议（imboy.v2）](#v2-二进制帧协议imboyv2)
- [消息格式规范](#消息格式规范)
- [消息类型定义](#消息类型定义)
- [消息流程](#消息流程)
- [错误处理](#错误处理)
- [安全规范](#安全规范)
- [扩展指南](#扩展指南)
- [最佳实践](#最佳实践)
- [数据库结构](#数据库结构)
- [迁移指南](#迁移指南)

---

## 概述

### 设计原则

ImBoy WebSocket API v2.0 遵循以下设计原则：

1. **扁平化结构**: 消息关键字段提升到顶层，减少嵌套层级
2. **类型明确**: 通过顶层字段直接判断消息类型和操作
3. **职责分离**: S2C 消息与非 S2C 消息使用不同的字段组合
4. **向后不兼容**: v2.0 与 v1.0 不兼容，需要前后端同步升级
5. **简洁高效**: 减少冗余字段，优化消息大小

### v2.0 主要变更

| 变更项 | v1.0 | v2.0 | 说明 |
|--------|------|------|------|
| `msg_type` 位置 | payload 内 | 顶层 | 直接判断消息类型 |
| `action` 位置 | payload 内 | 顶层 | 直接判断操作类型 |
| `e2ee` 位置 | payload 内 | 顶层 | 加密元数据独立管理 |
| S2C 消息 | `msg_type` 在 payload | `action` 在顶层 | S2C 使用 action 字段 |
| 非 S2C 消息 | `action` 在 payload | `msg_type` 在顶层 | 非 S2C 使用 msg_type 字段 |

### 消息流向

```
┌─────────────┐                    ┌─────────────┐                    ┌─────────────┐
│   Client    │◄──────────────────►│   Server    │◄──────────────────►│  Database   │
│   (APP)     │   WebSocket JSON    │  (Imboy)    │    PostgreSQL      │             │
└─────────────┘                    └─────────────┘                    └─────────────┘
       │                                   │                                   │
       │  1. 发送消息 (v2.0 格式)           │                                   │
       ├──────────────────────────────────►│                                   │
       │  2. 服务端确认 (SERVER_ACK)        │                                   │
       │◄──────────────────────────────────┤                                   │
       │                                   │  3. 存储到数据库                   │
       │                                   ├──────────────────────────────────►│
       │                                   │  4. 查询接收方在线状态             │
       │                                   ├──────────────────────────────────►│
       │  5. 投递消息 (v2.0 格式, 如果在线) │                                   │
       │◄──────────────────────────────────┤                                   │
       │  6. 客户端确认 (CLIENT_ACK)       │                                   │
       ├──────────────────────────────────►│                                   │
       │                                   │  7. 清理定时器和离线消息           │
       │                                   ├──────────────────────────────────►│
```

### 核心概念

#### 消息类型 (Type)

- **C2C** (Client to Client): 单聊消息
- **C2G** (Client to Group): 群聊消息
- **C2S** (Client to Server): 客户端请求（如 AI 机器人）
- **S2C** (Server to Client): 服务端通知（如系统消息）

#### 消息子类型 (msg_type) - 仅非 S2C 消息

- `text`: 文本消息
- `image`: 图片消息
- `voice`: 语音消息
- `video`: 视频消息
- `file`: 文件消息
- `location`: 位置消息
- `custom`: 自定义消息

#### 操作类型 (action) - 仅 S2C 消息

- `pull_offline_msg`: 拉取离线消息
- `please_refresh_token`: Token 刷新请求
- `logged_another_device`: 异地登录通知
- `online`: 用户上线通知
- `offline`: 用户离线通知
- `apply_friend`: 好友申请通知
- `apply_friend_confirm`: 好友申请确认
- `in_denylist`: 对方已拉黑
- `not_a_friend`: 非好友关系
- `device_force_offline`: 被其他设备强制下线
- `app_upgrade`: 应用升级通知

#### 消息确认 (Acknowledgment)

- **SERVER_ACK**: 服务端确认收到消息
- **CLIENT_ACK**: 客户端确认接收消息

---

## 连接管理

### 连接端点

```
ws://host:port/ws
wss://host:port/ws
```

### 连接建立

#### 握手请求 (HTTP)

```http
GET /ws HTTP/1.1
Host: example.com
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
Sec-WebSocket-Version: 13
Authorization: Bearer <jwt_token>
User-Agent: Imboy/2.0.0
X-Device-ID: <device_id>
X-Device-Type: ios|android|web|desktop
```

#### 握手响应

```http
HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
```

### 认证方式

#### Token 认证

在握手时通过 `Authorization` header 传递 JWT Token：

```http
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

#### Token 刷新消息

Token 过期时，服务端发送刷新消息（v2.0 格式）：

```json
{
  "id": "s2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "S2C",
  "from": "",
  "to": "<user_id>",
  "action": "please_refresh_token",
  "payload": {},// "Token 已过期，请在 8 秒内刷新"
  "server_ts": 1736141700000
}
```

### 心跳机制

**WebSocket 传输层心跳**（所有子协议通用）:

- 间隔：120 秒
- 由 `IOWebSocketChannel.pingInterval` 驱动，使用 WebSocket 协议原生 ping/pong
- 作用：NAT 保活、底层 keepalive

**IMBoy frame 层心跳**（仅 `imboy.v2` 子协议下启用）:

- 间隔：120 秒
- 帧类型：`FRAME_TYPE_HEARTBEAT_PING` (`0x01`) / `FRAME_TYPE_HEARTBEAT_PONG` (`0x02`)
- Payload：`uint16` big-endian `Seq`，pong 原样回显
- 详见 [v2 二进制帧协议 / 心跳协议](#心跳协议)

### 连接断开

#### 正常断开

```http
HTTP/1.1 1000 (Normal Closure)
```

#### 异常断开处理

| 状态码 | 说明 | 处理方式 |
|--------|------|----------|
| 1000 | 正常关闭 | 无需处理 |
| 1001 | 端点离开 | 自动重连 |
| 1008 | 策略违反 | 检查认证状态 |

#### 重连策略

```
第 1 次断开 → 立即重连
第 2 次断开 → 1 秒后重连
第 3 次断开 → 2 秒后重连
第 4 次断开 → 5 秒后重连
第 5 次断开 → 10 秒后重连
后续断开 → 指数退避，最大 60 秒
```

---

## 传输协议

### 双协议支持

ImBoy WebSocket 支持 JSON 和 Protocol Buffers 两种传输协议，通过 WebSocket 子协议协商选择。

#### 子协议协商

客户端在握手时通过 `Sec-WebSocket-Protocol` 请求头声明支持的协议：

```http
Sec-WebSocket-Protocol: imboy.v2, imboy-protobuf, imboy-json, text
```

服务端按优先级选择：`imboy.v2` > `imboy-protobuf` > `imboy-json` > `text`。

选择逻辑实现在 `src/ds/websocket_ds.erl:select_subprotocol/1`，由 `src/api/websocket_handler.erl:negotiate_protocol/1` 将选中的子协议转换为连接级状态 `#{protocol => json | protobuf, framing => none | v2}`。

| 子协议 | 传输格式 | 帧类型 | framing | 说明 |
|--------|----------|--------|---------|------|
| `imboy.v2` | 分层二进制帧 + 内嵌 JSON / Protobuf payload | binary frame | `v2` | **推荐**。引入 9 字节 frame header，详见 [v2 二进制帧协议（imboy.v2）](#v2-二进制帧协议imboyv2) |
| `imboy-protobuf` | Protocol Buffers | binary frame | `none` | 裸 protobuf，无 frame header |
| `imboy-json` | JSON | text frame | `none` | 默认，开发调试友好 |
| `text` | JSON | text frame | `none` | 向后兼容 |

#### 协议差异

| 特性 | JSON | Protocol Buffers |
|------|------|------------------|
| 编码速度 | ~1.3 us/op | ~1.4 us/op |
| 解码速度 | ~1.4 us/op | ~0.4 us/op (3.5x 更快) |
| 消息体积 | 基准 | 约 -67% |
| 调试友好性 | 高（可读文本） | 低（二进制） |

#### Proto 定义

消息结构定义在 `proto/imboy.proto`，核心消息类型：

```protobuf
message IMBoyMessage {
  string id = 1;
  MsgDirection type = 2;    // C2C, C2G, S2C, C2S, CLIENT_ACK, etc.
  sint64 from = 3;
  sint64 to = 4;
  ContentType msg_type = 5;
  string action = 6;
  bytes payload = 7;
  int64 server_ts = 8;
  int64 created_at = 9;
  E2EEMeta e2ee = 10;
}
```

#### CLIENT_ACK 协议

**JSON 格式**（向后兼容）：
```
CLIENT_ACK,{Type},{MsgId},{DID}
```

**Protobuf 格式**：
```protobuf
IMBoyMessage {
  type: CLIENT_ACK
  payload: PayloadClientAck {
    msg_direction: C2C    // 原始消息类型
    msg_id: "msg-123"     // 被确认的消息 ID
    did: "device-456"     // 设备 ID
  }
}
```

**服务端响应** — CLIENT_ACK_CONFIRM：
```protobuf
IMBoyMessage {
  id: "msg-123"
  type: CLIENT_ACK_CONFIRM
  action: "CLIENT_ACK_CONFIRM"
  server_ts: 1710000000000
}
```

#### 连接级错误

连接级错误（认证失败、协议不支持等）始终使用 JSON text frame 响应，不受客户端协议选择影响。

---

## v2 二进制帧协议（imboy.v2）

> 引入时间：2026-04-10  
> Source of truth: `include/imboy_frame.hrl`、`src/lib/imboy_frame.erl`、`src/api/websocket_handler.erl`、`lib/service/protocol/imboy_frame.dart`、`lib/service/websocket.dart`  
> 适用范围：子协议协商选中 `imboy.v2` 后，所有上下行 WebSocket **binary frame** 的载荷布局；`imboy-protobuf` / `imboy-json` / `text` 子协议不受影响，保持原样。

### 动机与目标

消息结构 v2.0 把 `msg_type` / `action` / `e2ee` 提升到 JSON 顶层后，传输层仍然是"WebSocket text frame + JSON 字符串"或"WebSocket binary frame + 裸 protobuf 字节"。在继续演进过程中遇到若干痛点：

- 心跳、ACK、业务消息共用同一个 WS frame，无法在传输层直接区分优先级；
- 控制信令（ping/pong/ack/nack）必须 decode 整个 JSON/Protobuf 才能识别，CPU 与延迟都被拉高；
- 未来要做压缩（zstd）、端到端加密（E2EE）、需要 ACK 的可靠投递时，缺少帧级 flag 位；
- TCP 直连（非 WebSocket）模式缺少通用的 framing 规范。

`imboy.v2` 在不放弃现有 JSON / Protobuf 数据契约的前提下，为每个 WebSocket binary frame 新增一个固定 9 字节头部，承载类型、标志、长度。头部独立于 payload 内部格式，使控制帧和业务帧在同一个物理通道里清晰分流，并为未来的压缩 / 加密 / 可靠投递预留 flag 位。

设计目标：

1. **零破坏**：旧客户端可继续使用 `imboy-protobuf` / `imboy-json` / `text` 子协议，路径保持与升级前一致。
2. **跨语言字节一致**：同一份 `.hrl` / `.dart` 常量，Erlang 与 Dart 编码结果字节级相同。
3. **控制面与数据面解耦**：ping/pong/ack/nack 是独立帧类型，不占用业务 JSON。
4. **面向 TCP 友好**：header 结构同时适用于 WebSocket binary frame 与未来的 TCP 字节流。

### 帧结构

固定 9 字节帧头 + 可变长 payload：

```text
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+---------------+---------------+---------------+---------------+
|         Magic (0x4942)        |    Version    |     Flags     |
+---------------+---------------+---------------+---------------+
|     Type      |                 PayloadLen (BE)               |
+---------------+-----------------------------------------------+
|                        Payload (N bytes)                      |
+---------------------------------------------------------------+
```

| 偏移 | 长度 | 字段 | 值 / 说明 |
|------|------|------|------|
| 0 | 2 | `Magic` | 固定 `0x4942`（ASCII `"IB"`），用于快速对齐与错包识别 |
| 2 | 1 | `Version` | 当前固定为 `2`（`?IMBOY_FRAME_VERSION`） |
| 3 | 1 | `Flags` | 位图，见 [Flags 位图](#flags-位图) |
| 4 | 1 | `Type` | 帧类型枚举，见 [帧类型枚举](#帧类型枚举) |
| 5 | 4 | `PayloadLen` | big-endian uint32，payload 字节数，上限 `16 * 1024 * 1024`（`?IMBOY_FRAME_MAX_PAYLOAD`） |
| 9 | N | `Payload` | 可变长字节，内容由 `Type` 决定 |

- 头部常量定义：`include/imboy_frame.hrl`，`?IMBOY_FRAME_HEADER_SIZE = 9`。
- 最大载荷：16 MiB；超出则 Erlang 端 `imboy_frame:encode/3` 抛 `frame_too_large`，解码端返回 `{error, frame_too_large}`，对端应直接关闭连接。
- 所有多字节整数一律 **big-endian**，`ByteData.setUint32(..., Endian.big)` / `32/big-unsigned` 双端显式声明。

### Flags 位图

| 位 | 常量 | 掩码 | 语义 | 状态 |
|----|------|------|------|------|
| bit7 | `FRAME_FLAG_CMP` / `FrameFlags.cmp` | `0x80` | payload 是否经过 zstd 压缩 | 保留，当前双端均不设置 |
| bit6 | `FRAME_FLAG_ENC` / `FrameFlags.enc` | `0x40` | payload 是否为帧级 E2EE 密文 | 保留，业务层 E2EE 仍走 JSON `e2ee` 字段 |
| bit5 | `FRAME_FLAG_ACK` / `FrameFlags.ack` | `0x20` | 对端是否需要回 frame 层 ACK | 心跳 ping 使用；业务帧当前不使用 |
| bit4-3 | — | `0x18` | 保留 | 必须填 0 |
| bit2-0 | `FRAME_FLAG_PRI_MASK` / `FrameFlags.priorityMask` | `0x07` | 优先级 0-7，数值越大越优先 | 心跳使用 7，业务帧当前填 0 |

Erlang 访问器：`imboy_frame:is_compressed/1`、`is_encrypted/1`、`needs_ack/1`、`priority/1`。Dart 等价访问器：`ImboyFrame.isCompressed` / `isEncrypted` / `needsAck` / `priority`。

### 帧类型枚举

| 类别 | Type | 常量 | 方向 | Payload 规范 |
|------|------|------|------|--------------|
| 控制 | `0x01` | `FRAME_TYPE_HEARTBEAT_PING` | 双向 | `uint16` big-endian `Seq`，uint16 回绕 |
| 控制 | `0x02` | `FRAME_TYPE_HEARTBEAT_PONG` | 双向 | `uint16` big-endian，回显对应 ping 的 `Seq` |
| 控制 | `0x03` | `FRAME_TYPE_ACK` | 服务端→客户端 | `uint64` big-endian `MsgId`（**frame 层** ACK，见后文辨析） |
| 控制 | `0x04` | `FRAME_TYPE_NACK` | 服务端→客户端 | `uint64` big-endian `MsgId` |
| 控制 | `0x05` | `FRAME_TYPE_CLOSE` | _预留_ | _未实现_ |
| 控制 | `0x06` | `FRAME_TYPE_ERROR` | _预留_ | _未实现_ |
| 握手 | `0x10`–`0x12` | `FRAME_TYPE_HANDSHAKE_HELLO` / `AUTH` / `OK` | _预留_ | _未实现，握手仍走 HTTP upgrade + Authorization header_ |
| 业务 | `0x20` | `FRAME_TYPE_MSG_C2C` | 双向 | UTF-8 JSON 字符串 或 Protobuf `IMBoyMessage` 字节，详见 [上行帧分派规则](#上行帧分派规则) |
| 业务 | `0x21` | `FRAME_TYPE_MSG_C2G` | 双向 | 同上 |
| 业务 | `0x22` | `FRAME_TYPE_MSG_C2S` | 双向 | 同上；额外兼容 `CLIENT_ACK,...` 纯文本载荷 |
| 业务 | `0x23` | `FRAME_TYPE_MSG_S2C` | 服务端→客户端 | 同上；投递管道默认封装到该类型 |
| 业务 | `0x24`–`0x27` | `MSG_SYNC` / `MSG_TYPING` / `MSG_READ` / `MSG_RECALL` | _预留_ | _常量已定义，业务分派暂未接入_ |
| 扩展 | `0x80`–`0x81` | `FRAME_TYPE_RPC_REQ` / `RPC_RSP` | _预留_ | _未实现_ |

> 标注为 **预留** 的类型仅定义了数值常量，尚未接入业务分派路径；实现路线见下文的「路线图」。

### 子协议协商流程

```
Client                                             Server
  |                                                  |
  |  GET /ws HTTP/1.1                                |
  |  Upgrade: websocket                              |
  |  Sec-WebSocket-Protocol:                         |
  |    imboy.v2, imboy-protobuf, imboy-json, text    |
  |------------------------------------------------->|
  |                                                  |
  |                             websocket_ds:select_subprotocol/1
  |                             (imboy.v2 > imboy-protobuf > imboy-json > text)
  |                                                  |
  |  HTTP/1.1 101 Switching Protocols                |
  |  Sec-WebSocket-Protocol: imboy.v2                |
  |<-------------------------------------------------|
  |                                                  |
  |   websocket_handler:negotiate_protocol/1         |
  |   State = #{protocol => protobuf, framing => v2} |
```

服务端关键实现：

- `src/ds/websocket_ds.erl:select_subprotocol/1` — 按优先级表 `[<<"imboy.v2">>, <<"imboy-protobuf">>, <<"imboy-json">>, <<"text">>]` 与客户端声明求交集。
- `src/api/websocket_handler.erl:negotiate_protocol/1`（约 690 行） — 把选中子协议翻译成 `{Protocol :: json | protobuf, Framing :: none | v2}` 存入连接 State。
  - `imboy.v2` → `{protobuf, v2}`
  - `imboy-protobuf` → `{protobuf, none}`
  - `imboy-json` / `text` → `{json, none}`
- `State#{protocol := ..., framing := ...}` 后续驱动所有上下行编解码分支。

前端关键实现：

- `lib/service/websocket.dart` 握手时声明 `protocols: ['imboy.v2', 'imboy-protobuf', 'imboy-json', 'text', 'sip']`（约 271 行）。
- 连接建立后通过 `WebSocketChannel.protocol` 读取服务端选中的子协议，由 `_detectFraming` 设置 `_framing = FramingMode.v2 | none`（约 362 行）。

### 上行帧分派规则

`imboy.v2` 选中后，上行 WebSocket frame 必须是 **binary**。Erlang 端入口：

```
websocket_handle({binary, Bin}, State)
  -> handle_v2_binary/2          %% websocket_handler.erl ~190 行
  -> imboy_frame:decode/1
  -> dispatch_v2_frame/3         %% websocket_handler.erl ~202 行
```

分派规则（`dispatch_v2_frame/3` 实现）：

| Frame Type | 分支 | 行为 |
|------------|------|------|
| `HEARTBEAT_PING` (`0x01`) | payload 为 `<<Seq:16>>` | 立即以 `imboy_frame:heartbeat_pong(Seq)` 回复 binary frame |
| `HEARTBEAT_PING` | payload 畸形 | 记 `v2_heartbeat_ping_bad_payload` warn，静默丢弃 |
| `ACK` (`0x03`) | `<<MsgIdInt:64>>` | 转换为 `protobuf PayloadClientAck { msg_id, did, msg_direction=C2C }`，交 `handle_protobuf_client_ack/3` 复用现有 ACK 流水线 |
| `NACK` (`0x04`) | `<<MsgIdInt:64>>` | 记 `v2_nack_received` warn，当前无业务动作 |
| `MSG_C2S` (`0x22`) + payload 以 `"CLIENT_ACK,"` 开头 | 走 `handle_client_ack/2` | **不受** `msg_per_user` throttle 限制；等价于旧 `{text, <<"CLIENT_ACK,...">>}` 路径 |
| `MSG_C2C` / `MSG_C2G` / `MSG_C2S` 业务 payload | 进入 `throttle:check(msg_per_user, Uid)` | 通过后进入 `dispatch_v2_business_payload/3` 宽容解码 |
| 其他 Type | 记 `v2_frame_unsupported_type` warn | 静默丢弃 |

业务 payload 的宽容解码（`dispatch_v2_business_payload/3` + `try_decode_json_payload/1`，约 249–284 行）：

| 情况 | 判定 | 后续流水线 |
|------|------|-----------|
| ① UTF-8 JSON 文本 | `jsone:decode/2` 成功且得到 Map | `handle_json_message/2`：`decode_websocket_message → convert_v1_to_v2 → validate_message → message_router_logic:route` |
| ② Protobuf `IMBoyMessage` | ① 失败后 `imboy_codec:decode(protobuf, Payload)` 得到非空 Map | `handle_protobuf_message_decoded/2`，复用既有 protobuf 处理路径 |
| ③ `CLIENT_ACK,<type>,<msg_id>,<did>` 文本 | 仅在 `MSG_C2S` 分支匹配前缀 | `handle_client_ack/2`，旁路 throttle |
| ④ 非法 / 空载荷 / decode 抛异常 | — | 记录 `v2_msg_decode_failed` / `v2_msg_decode_empty` warn，连接**保持**不关闭 |

> **当前 Dart 客户端选择情况 ①**：`_encodeV2BusinessFrame/1`（`lib/service/websocket.dart` 约 373 行）直接把 JSON 字符串作为 UTF-8 字节包进 frame payload；`type` 字段读出后映射到 `FrameType.msgC2C` / `msgC2G` / `msgC2S` / `msgS2C`，解析失败或 `CLIENT_ACK,` 前缀时回退到 `FrameType.msgC2S`。

### 下行帧封装规则

`websocket_handler.erl` 里的下行路径分两条：

1. **业务消息直接回写**：`websocket_info({reply, Msg :: map()}, State)` → `ws_reply/3`（约 707 行）
   - 当 `Framing = v2` 时：`imboy_codec:encode(protobuf, Msg)` → `imboy_codec:wrap_v2_frame(FrameType, 0, Encoded)` → 返回 `{binary, Frame}`
   - 否则走旧 `ws_reply/2`，保留 `{text, Json}` 或 `{binary, ProtobufBytes}` 行为

2. **投递管道（`send_next` / 定时重试）**：`encode_delivery_frame/2` → `encode_delivery_frame_v2/1`（约 768 行）
   - 输入是已经预编码的 JSON binary
   - 先 `jsone:decode` 还原为 Map，再 protobuf 重新编码，最后 `wrap_v2_frame`
   - 失败回退为 `{text, OriginalJson}`，并记 `encode_delivery_frame_v2_fallback` warn

**Type 映射**（`msg_to_v2_frame_type/1`，约 719 行）：

| 消息顶层 `type` | Frame Type |
|-----------------|-----------|
| `"C2C"` | `FRAME_TYPE_MSG_C2C` (`0x20`) |
| `"C2G"` | `FRAME_TYPE_MSG_C2G` (`0x21`) |
| `"C2S"` | `FRAME_TYPE_MSG_C2S` (`0x22`) |
| `"S2C"` | `FRAME_TYPE_MSG_S2C` (`0x23`) |
| 缺失 / 其他 | `FRAME_TYPE_MSG_S2C`（默认） |

Flags 当前统一填 `0`（无压缩、无加密、不要求 ACK、优先级 0）。

**前端下行接收**：`WebSocketService._handleV2Binary`（`lib/service/websocket.dart` 约 421 行）：

- `HEARTBEAT_PONG` → 仅打印日志
- `HEARTBEAT_PING` → 回 `ImboyFrame.heartbeatPong(seq)`
- `ACK` → 读 `uint64` msgId，调用 `AckManager.to.ackConfirmed(msgId.toString())`
- `NACK` → 记日志
- `MSG_C2C` / `MSG_C2G` / `MSG_C2S` / `MSG_S2C` → `utf8.decode(frame.payload)` 得到 JSON 字符串后递归喂给 `_onMessage`，复用现有 JSON 分派
- 未知 Type → 记日志丢弃
- `FormatException`（bad magic / frame too large）→ 捕获后仅记日志，不关闭连接

### 心跳协议

`imboy.v2` 启用**双层心跳**：

| 层 | 发送方 | 周期 | 实现 | 作用 |
|----|--------|------|------|------|
| WebSocket 传输层 | 客户端 | 120s | `IOWebSocketChannel(..., pingInterval: Duration(seconds: 120))` | 保活 NAT、触发底层 WebSocket ping/pong，浏览器/OS 级 keepalive |
| IMBoy frame 层 | 客户端 | 120s | `Timer.periodic(_pingInterval, _sendV2Heartbeat)`（`lib/service/websocket.dart` 约 303 行） | 携带业务 `seq`，使服务端能在应用层感知对端存活、为未来 RTT / 掉线检测预留通道 |

Seq 语义：

- 客户端维护 `_v2PingSeq`，每次发送后 `(seq + 1) & 0xFFFF` 回绕，uint16 big-endian。
- 服务端收到 `HEARTBEAT_PING` 立即回 `HEARTBEAT_PONG`，payload 原样回显 `Seq`。
- 客户端收到的 `HEARTBEAT_PONG` 当前仅打印日志，未与原始 ping 做强匹配。

服务端同时在 `dispatch_v2_frame/3` 保留了对 `HEARTBEAT_PING` 的直接响应，当未来服务端主动发起 ping 时客户端也会立即回 pong（`_handleV2Binary` 已实现）。

### Frame 层 ACK vs 业务层 CLIENT_ACK

这是本次升级最容易混淆的一点，必须严格区分：

| 维度 | Frame 层 ACK / NACK | 业务层 CLIENT_ACK |
|------|---------------------|-------------------|
| 帧 Type | `FRAME_TYPE_ACK` (`0x03`) / `FRAME_TYPE_NACK` (`0x04`) | `FRAME_TYPE_MSG_C2S` (`0x22`) |
| Payload | `uint64` big-endian `MsgId` | UTF-8 文本 `"CLIENT_ACK,<type>,<msg_id>,<did>"` |
| 方向 | 仅用于**服务端下发**确认（预留） | **客户端→服务端** 主动确认收到下行消息 |
| Dart 客户端是否发送 | **否** | **是**（`AckManager` 正常发送） |
| TSID 兼容性 | `uint64` 字节级可承载 TSID（64-bit 有符号正数），但 Dart Web 平台 `int` 为 double，超过 2^53 丢精度 | 文本格式直接承载 TSID 十进制字符串，跨平台无歧义 |
| Throttle | — | 被特判为不受 `msg_per_user` 限制 |

**为什么 Dart 客户端不使用 frame 层 ACK？**（2026-04-10 事实复核后的权威结论）

1. **`msg_direction` 信息无处安放（决定性原因）**：`AckManager` 实际发送 4 种方向的 ACK —— `C2C` / `C2G` / `S2C` / `WEBRTC`（见 `lib/service/websocket.dart:521-532` 和 `lib/service/ack_manager.dart:261`）。frame 层 ACK payload 固定 8 字节 `uint64`，无法携带 direction；服务端 `handle_protobuf_client_ack` 需要 `msg_direction` 路由到正确的清理管道（`msg_c2c` / `msg_c2g` 等）。现行 `dispatch_v2_frame(?FRAME_TYPE_ACK, ...)` 分支不得已把 `msg_direction` **硬编码为 `C2C`**（`websocket_handler.erl:215`）。若 Dart 全量切到 frame ack，`C2G` / `S2C` / `WEBRTC` 的 ACK 将被错误路由，破坏重试闭环。
2. **扩展 payload 不可行**：升级约束明令禁止修改 `imboy_frame.erl/dart` 及其 26+26 个跨语言字节 fixture 测试，因此无法在 frame ack payload 追加 direction 字段。
3. **Web 平台精度风险**：TSID 是 64-bit 有符号正数（首 bit=0，最大 ~9.2e18），字节级可无损填进 `uint64`；但 Dart 在 Web / JS 平台 `int` 编译为 `double`，`int.parse("1838294017982464")` 超过 `Number.MAX_SAFE_INTEGER`（2^53）会丢精度，`ImboyFrame.ack(int)` 下的 `setUint64` 在 dart2js 无法可靠承载全量 TSID。文本 CLIENT_ACK 路径天然规避此问题。
4. **`AckManager` 重构成本不对等**：`_pendingAcks` 是 `Map<String, _PendingAck>`，`_PendingAck.content` 存储预格式化的 `"CLIENT_ACK,type,msgId,did"` 字符串，RTT 统计 / 重试上限事件 / 测试 mock 均基于 String key。切到 int64 涉及 30+ 处改动，而回报仅是每 ACK 省 ~30 字节文本开销，性价比极低。
5. **`did` 字段必须保留**：业务层 CLIENT_ACK 携带 `did`（设备 ID）用于多端同 `uid` 场景下精准清理 in-flight 消息；frame 层 8 字节 payload 完全放不下。

因此当前版本的选择是：

- **下行**：frame 层 ACK 通道保留给服务端未来的纯数字 ID / RPC 场景；现阶段若触发 `dispatch_v2_frame(?FRAME_TYPE_ACK, ...)`（例如未来服务端向客户端推送 frame ACK），服务端会把 `uint64` 适配成 `PayloadClientAck { msg_id = integer_to_binary(...), did = State.did, msg_direction = C2C }` 交给 `handle_protobuf_client_ack/3`，复用同一套 ACK 流水线。
- **上行**：Dart 客户端仍然发送 `msg_c2s` 帧 + `"CLIENT_ACK,..."` 文本载荷；服务端在 `dispatch_v2_frame` 里对这个前缀做特判，旁路 throttle。

### 跨语言字节样例

以下字节序列由 `test/service/imboy_frame_test.dart`「跨语言兼容性」fixture 固化，Erlang `imboy_frame:heartbeat_ping(7)` / `imboy_frame:ack(16#1234567890ABCDEF)` 输出必须字节级一致。

**示例 1：`heartbeat_ping(seq = 7)`**

```text
49 42         ; Magic "IB" = 0x4942
02            ; Version = 2
27            ; Flags = FRAME_FLAG_ACK (0x20) | PRI=7 (0x07) = 0x27
01            ; Type = FRAME_TYPE_HEARTBEAT_PING
00 00 00 02   ; PayloadLen = 2
00 07         ; Payload = uint16 big-endian seq = 7
```

总长 11 字节（9 字节 header + 2 字节 payload）。

**示例 2：`ack(msg_id = 0x1234567890ABCDEF)`**

```text
49 42                       ; Magic "IB"
02                          ; Version = 2
00                          ; Flags = 0 (无 CMP / ENC / ACK，优先级 0)
03                          ; Type = FRAME_TYPE_ACK
00 00 00 08                 ; PayloadLen = 8
12 34 56 78 90 AB CD EF     ; Payload = uint64 big-endian msg_id
```

总长 17 字节。

Erlang 与 Dart 构造同一帧时必须输出完全相同的字节序列，任何偏差都会在 fixture 测试里被捕获。

### 兼容性与回退

| 客户端声明 | 服务端选中 | 上行 | 下行 | 备注 |
|------------|-----------|------|------|------|
| `imboy.v2, imboy-protobuf, imboy-json, text` | `imboy.v2` | v2 二进制帧 | v2 二进制帧 | 推荐路径 |
| `imboy-protobuf, imboy-json, text` | `imboy-protobuf` | WS binary + 裸 protobuf | WS binary + 裸 protobuf | 旧版 Dart 或其他 protobuf 客户端 |
| `imboy-json, text` 或未声明 | `imboy-json` | WS text + JSON | WS text + JSON | 默认 JSON 通道 |
| `text` | `text` | WS text + JSON | WS text + JSON | 向后兼容 |

- 同一个 `websocket_handler` 进程根据 `State.framing` 字段决定上下行路径，不同连接可各自选择不同子协议互不影响。
- 连接级错误（认证失败、协议不支持）**仍使用 JSON text frame 响应**，不受 framing 影响。
- 服务端 `encode_delivery_frame_v2/1` 在 protobuf 编码失败时会自动回退为 `{text, OriginalJson}`，保证消息至少能以 JSON 形式送达；同时记录 warn 日志供排查。

### 错误处理

| 错误 | 触发点 | 服务端行为 | 客户端行为 |
|------|--------|-----------|-----------|
| `bad_magic` | `imboy_frame:decode/1` 匹配不到 `0x4942` | 记录 `v2_frame_decode_failed` warn，当前**保持连接**并丢弃本帧 | Dart 端 `tryDecode` 抛 `FormatException`，`_handleV2Binary` 捕获后只记日志，不关闭连接 |
| `frame_too_large` | `PayloadLen > 16 MiB` | 同上 | 同上 |
| v2 帧类型未注册 | `dispatch_v2_frame` fall-through | 记录 `v2_frame_unsupported_type` warn，丢弃 | 记录 `v2 未知 frame type` 日志，丢弃 |
| 业务 payload 无法解码为 JSON 或 Protobuf | `dispatch_v2_business_payload` 两路均失败 | 记录 `v2_msg_decode_failed` / `v2_msg_decode_empty` warn，丢弃 | N/A |
| 业务消息校验失败 | `message_ds:validate_message/1` | 返回 `invalid_message` / `invalid_json` S2C 错误 | 走原有 S2C 错误处理分支 |
| Rate limit | `throttle:check(msg_per_user, Uid)` | 返回 `rate_limited` 校验错误，格式沿用 `ws_validation_error/3` | 走原有错误处理分支 |

> **设计选择**：当前出现帧层错误时不关闭连接。原因是 WebSocket binary frame 本身已经是有边界的消息单元，错包往往是客户端临时 bug 或对端版本漂移，直接断开会放大故障面。未来若要迁移到 TCP 字节流传输，需要引入重同步（resync）策略并按 `.claude/plans/imboy-frame-protocol.md` 重新评估。

### 路线图

以下能力在帧结构里已有占位，但当前版本**尚未实现**：

- *`FRAME_FLAG_CMP` + zstd 压缩*：flag 位已保留，编解码函数未做压缩/解压。
- *`FRAME_FLAG_ENC` 帧级 E2EE*：业务层 E2EE 仍走 JSON `e2ee` 字段，frame 层加密留作未来 TCP 直连场景。
- *握手帧 `HANDSHAKE_HELLO` / `AUTH` / `OK`*：常量已定义，当前握手仍依赖 HTTP upgrade + `Authorization` header。
- *`MSG_SYNC` / `MSG_TYPING` / `MSG_READ` / `MSG_RECALL`*：枚举值已占位，`dispatch_v2_frame` 尚未实现分派；暂仍通过业务 JSON 顶层 `type` / `msg_type` 表达。
- *`RPC_REQ` / `RPC_RSP`*：为未来纯 RPC 调用预留。
- *TCP 直连 framing*：header 设计已兼容字节流，但 `decode_stream/2` 的 `bad_magic` 语义目前是"立即关闭"而非"resync"，TCP 场景需要额外工作。

---

## 消息格式规范

### 标准消息结构

#### 顶层字段

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `id` | binary | ✅ | 消息唯一标识符，格式：`<type>.<tsid>.<timestamp>.<random>` |
| `type` | binary | ✅ | 消息类型：`C2C`\|`C2G`\|`C2S`\|`S2C` |
| `from` | binary | ✅ | 发送方 ID (TSID integer)，S2C 消息可能为空 |
| `to` | binary | ✅ | 接收方 ID (TSID integer) |
| `payload` | map | ✅ | 消息载荷，仅包含展示相关内容 |
| `created_at` | binary | ⚪ | 客户端创建时间 (RFC3339 格式)，可选 |
| `server_ts` | integer | ✅ | 服务端时间戳 (毫秒，UTC+0) |
| `action` | binary | 🔄 | **S2C 消息专用**：包含指令值；非 S2C 消息该字段不存在或为空字符串 `""` |
| `msg_type` | binary | 🔄 | 包含消息类型；S2C 消息该字段暂时没用上，以后扩展多媒体消息可以用上该字段 `""` |
| `e2ee` | map | 🔄 | **非 S2C 消息加密时**：包含加密元数据；未加密或 S2C 消息该字段不存在 `""` |

> **🔄 字段说明**：标注 🔄 的字段根据消息类型决定是否包含。为保持解析一致性，实现时可以选择：
> - 根据消息类型决定是否包含该字段（减少消息大小）
>

#### 字段使用规则

| 消息类型 | `action` | `msg_type` | `e2ee` |
|---------|----------|------------|--------|
| **S2C** | ✅ **包含指令值** | ✅ **包含消息类型** | ❌ 不存在或空字符串 `""` |
| **C2C** | ❌ 不存在或空字符串 | ✅ **包含消息类型** | 🔸 加密时包含，否则不存在或空字符串 `""` |
| **C2G** | ❌ 不存在或空字符串 | ✅ **包含消息类型** | 🔸 加密时包含，否则不存在或空字符串 `""` |
| **C2S** | ❌ 不存在或空字符串 | ✅ **包含消息类型** | 🔸 加密时包含，否则不存在或空字符串 `""` |

#### Payload 结构（简化版）

**非加密消息**：
```json
{
  "content": "Hello World",
  "url": "...",
  "width": 1024,
  "height": 768
}
```

**加密消息**：payload 直接是 Base64 编码的密文字符串
```json
"base64_encoded_ciphertext_with_tag"
```

#### E2EE 字段结构

当启用端到端加密时，`e2ee` 字段包含加密元数据：

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `e2ee` | boolean | ✅ | 是否启用端到端加密；始终为 `true` |
| `e2ee_ver` | integer | ✅ | E2EE 协议版本；当前为 `1` |
| `e2ee_suite` | binary | ✅ | 算法套件标识，如：`RSA-OAEP-256+AES-256-GCM` |
| `nonce` | binary | ✅ | Base64 编码的 AES-GCM nonce（建议 12 bytes） |
| `keys` | list(map) | ✅ | 接收方设备密钥包列表 |

`keys` 元素结构：

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `did` | binary | ✅ | 接收方设备 ID |
| `kid` | binary | ⚪ | 接收方设备公钥版本/标识 |
| `wrap_alg` | binary | ✅ | 对称密钥包裹算法，如：`RSA-OAEP-256` |
| `ek` | binary | ✅ | Base64 编码的 wrapped key |

### 消息示例

#### C2C 文本消息（非加密）

```json
{
  "id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2C",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "msg_type": "text",
  "e2ee": {"e2ee": false},
  "payload": {
    "content": "Hello World"
  },
  "created_at": "2025-01-06T12:35:00Z",
  "server_ts": 1736141700000
}
```

#### C2C 文本消息（加密）

```json
{
  "id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2C",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "msg_type": "text",
  "e2ee": {
    "e2ee": true,
    "e2ee_ver": 1,
    "e2ee_suite": "RSA-OAEP-256+AES-256-GCM",
    "nonce": "b64_nonce_12bytes",
    "keys": [
      {
        "did": "deviceA",
        "kid": "k1",
        "wrap_alg": "RSA-OAEP-256",
        "ek": "b64_wrapped_aes_key_for_deviceA"
      }
    ]
  },
  "payload": "b64_ciphertext_with_tag",
  "created_at": "2025-01-06T12:35:00Z",
  "server_ts": 1736141700000
}
```

#### C2G 群聊消息

```json
{
  "id": "c2g.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2G",
  "from": "XyZ9aBcDeF",
  "to": "GrOuPiD123",
  "msg_type": "text",
  "e2ee": "",
  "payload": {
    "content": "Hello Group"
  },
  "server_ts": 1736141700000
}
```

#### S2C 系统消息

```json
{
  "id": "s2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "S2C",
  "from": "",
  "to": "XyZ9aBcDeF",
  "action": "pull_offline_msg",
  "payload": {},
  "server_ts": 1736141700000
}
```

#### C2S AI 请求

```json
{
  "id": "c2s.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2S",
  "from": "XyZ9aBcDeF",
  "to": "bot_qian_fan",
  "msg_type": "text",
  "e2ee": "",
  "payload": {
    "text": "你好，请帮我写一首诗"
  },
  "server_ts": 1736141700000
}
```

---

## 消息类型定义

### 基础消息类型 (msg_type)

| msg_type | 说明 | Payload 示例 | 消息UI |
|----------|------|-------------|-------------------|
| `text` | 文本消息 | `{"text": "Hello"}` | flyer_chat_text_message |
| `textStream` | 文本流消息 | `{"text": "Hello", "index": 0, "is_end": false, "stream_id": "abc123"}` | flyer_chat_text_stream_message |
| `image` | 图片消息 | `{"uri": "https://cdn.example.com/image.jpg", "size": 102400, "width": 1920, "height": 1080}` | flyer_chat_image_message |
| `imageMulti` | 多图片消息 | `{"images": [{"uri": "...", "size": 102400, "width": 1920, "height": 1080}], "total": 3}` | ImageMultiMessageBuilder |
| `file` | 文件消息 | `{"uri": "https://cdn.example.com/doc.pdf", "name": "report.pdf", "size": 1024000, "mime_type": "application/pdf"}` | flyer_chat_file_message |
| `location` | 位置消息 | `{"latitude": 39.9042, "longitude": 116.4074, "title": "北京市朝阳区", "address": "朝阳区建国路88号"}` | LocationMessageBuilder |
| `voice` | 语音消息 | `{"uri": "https://cdn.example.com/voice.mp3", "duration_ms": 15000, "size": 51200}` | flyer_chat_audio_message |
| `video` | 视频消息 | `{"uri": "https://cdn.example.com/video.mp4", "duration_ms": 60000, "size": 5120000, "width": 1920, "height": 1080}` | flyer_chat_video_message |
| `system` | 系统消息 | `{}` | flyer_chat_system_message |
| `quote` | 引用消息 | `{"quote_msg_id": "msg100", "quote_text": "原始消息内容", "text": "回复内容"}` | QuoteMessageBuilder |
| `revoked` | 撤回消息 | `{}` (通过 status=30 或 31 识别) | RevokedMessageBuilder |
| `visitCard` | 个人名片消息 | `{"uid": "user123", "title": "张三", "avatar": "https://cdn.example.com/avatar.jpg"}` | VisitCardMessageBuilder |
| `webrtcAudio` | WebRTC 音频消息 | `{"call_type": "offer", "sdp": "..."}` | WebRTCMessageBuilder |
| `webrtcVideo` | WebRTC 视频消息 | `{"call_type": "offer", "sdp": "..."}` | WebRTCMessageBuilder |
| `custom` | 自定义消息 | 通过 `custom_type` 区分子类型 | 根据子类型选择 |
| `unsupported` | 不支持的消息类型 | 自定义字段 | UnsupportedMessageBuilder |

> **注意**：优先使用 flutter_chat_ui 库的 `flyer_chat_*` 消息组件，没有的则使用 `lib/component/chat/` 里的自定义 builder。

### C2C - 单聊消息

参考 基础消息类型 (msg_type)

### C2G - 群聊消息

#### 基础消息类型

参考 基础消息类型 (msg_type)

#### 群组特殊 S2C 消息

| action | 说明 | 触发条件 |
|--------|------|----------|
| `group_member_join` | 成员加入群组 | 新成员加入时发送给所有成员 |
| `group_member_leave` | 成员退出群组 | 成员退出时发送给所有成员 |
| `group_dissolve` | 群组解散 | 群主解散群组时发送 |

### S2C - 服务端通知

#### 系统消息 action 类型

| action | 说明 | 处理建议 |
|--------|------|----------|
| `pull_offline_msg` | 拉取离线消息 | 客户端立即拉取离线消息 |
| `please_refresh_token` | Token 刷新请求 | 客户端 8 秒内刷新 Token |
| `logged_another_device` | 异地登录通知 | 提示用户并强制下线 |
| `online` | 用户上线通知 | 更新好友在线状态 |
| `offline` | 用户离线通知 | 更新好友离线状态 |
| `apply_friend` | 好友申请通知 | 弹出好友申请对话框 |
| `apply_friend_confirm` | 好友申请确认 | 通知申请结果 |
| `in_denylist` | 对方已拉黑 | 显示拉黑提示 |
| `not_a_friend` | 非好友关系 | 显示非好友提示 |
| `not_group_member` | 非群成员 | 显示非群成员提示 |
| `permission_denied` | 权限不足 | 显示权限不足提示 |
| `device_force_offline` | 被强制下线 | 显示强制下线提示 |
| `app_upgrade` | 应用升级通知 | 显示升级对话框 |
| `invalid_message` | 客户端消息校验失败 | 记录日志并提示重发 |
| `invalid_json` | 客户端消息 JSON 非法 | 记录日志并提示检查客户端版本 |

### C2S - 客户端请求

#### AI 机器人

| to 值 | 说明 | Payload 示例 |
|--------|------|-------------|
| `bot_qian_fan` | 千帆 AI 对话 | `{"text": "...", "topic_id": 0}` |

---

## 消息流程

### 发送消息流程

```
┌──────────┐                    ┌──────────┐                    ┌──────────┐
│  Client  │                    │  Server  │                    │ Receiver │
└──────────┘                    └──────────┘                    └──────────┘
     │                               │                               │
     │  1. 发送消息 (Msg, v2.0格式)   │                               │
     ├───────────────────────────────►│                               │
     │                               │  2. 解析、验证、存储             │
     │                               ├───────────────────────────────►│
     │  3. 返回 SERVER_ACK           │                               │
     │◄───────────────────────────────┤                               │
     │                               │  4. 判断接收方在线状态           │
     │                               ├───────────────────────────────►│
     │                               │                               │
     │                               │  5. 如果在线，投递消息           │
     │                               │───────────────────────────────►│
     │                               │                               │
     │                               │  6. 接收方返回 CLIENT_ACK      │
     │                               │◄───────────────────────────────┤
     │                               │  7. 清理定时器和离线消息         │
```

### 消息确认机制

#### SERVER_ACK (服务端确认)

**格式**: 与原始消息相同，type 变更为 `*_SERVER_ACK`

```json
{
  "id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2C_SERVER_ACK",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "e2ee": "",
  "payload": {},
  "server_ts": 1736141700000
}
```

#### CLIENT_ACK (客户端确认)

**格式**: `CLIENT_ACK,<type>,<msg_id>,<device_id>`

```
CLIENT_ACK,C2C,c2c.x9j8.5ia0V5.Kr3aUs.F,device123
```

### 重试机制

#### 重试策略

| 消息类型 | 重试间隔 (毫秒) |
|---------|---------------|
| C2C 单聊 | `[0, 5000, 7000, 11000, 17000]` |
| C2G 群聊 | `[0, 3500, 3500, 3000, 5000]` |
| S2C 系统消息 | `[0, 1500, 1500, 3000, 5000, 7000]` |

---

## 错误处理

### 通用约定

- 通用业务错误码、envelope 语义与字段口径以 `docs/standards/error-codes.md`、`docs/api/rest-api.md` 为准。
- 本节仅补充 WebSocket 场景下的连接期 / 消息期错误表现与客户端处理建议。

### 顶层错误消息

```json
{
  "code": 401,
  "payload": {
    "title": "认证失败",
    "content": "Token 无效或已过期"
  },
  "server_ts": 1736141700000
}
```

适用场景：

- 握手认证失败；
- Token 缺失、无效或已过期；
- 请求在进入业务处理前就被拒绝。

### S2C 错误消息

```json
{
  "id": "s2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "S2C",
  "from": "",
  "to": "XyZ9aBcDeF",
  "action": "in_denylist",
  "payload": {
    "content": "对方已将您加入黑名单"
  },
  "server_ts": 1736141700000
}
```

适用场景：

- 连接已建立，但业务消息被服务端拒绝；
- 服务端希望通过统一 `S2C.action` 通知客户端调整 UI 或触发补偿逻辑。

### WebSocket 场景补充约定

| 场景 | 表现 | 客户端处理建议 |
|------|------|----------------|
| Token 无效 / 缺失 | 顶层 `401` 或 `please_refresh_token` | 刷新 Token，必要时重连 |
| 黑名单 / 权限拒绝 | `S2C.action` 如 `in_denylist`、`permission_denied` | 给出提示，不做本地伪成功 |
| 客户端消息校验失败 | `invalid_message` | 记录日志并提示重发 |
| 客户端 JSON 非法 | `invalid_json` | 记录日志并提示检查客户端版本 |
| 资源不存在 | 顶层 `404` 或业务型 `S2C.action` | 提示目标已失效并刷新会话状态 |
| 服务端内部错误 | 顶层 `500` | 指数退避重试，避免立即洪泛重发 |

---

## 安全规范

WebSocket 的通用安全基线以 `docs/operations/security.md` 为准；本节只保留协议特有约束。

### 认证

- 连接建立前必须携带有效登录态；
- 认证凭证失效后，客户端应优先刷新 Token，而不是盲目无限重连；
- `Device Token` / `did` 只能辅助识别设备，不替代用户认证。

### 传输安全

- 生产环境强制使用 `wss://`；
- 开发环境如使用 `ws://`，必须在内网或受控测试环境下进行；
- 任何“口令 RSA 包装”或“初始化数据加密”都不能替代 TLS。

### E2EE 约束

- `E2EE` 为可选能力，不是所有版本默认开启；
- 字段结构以本文“消息格式规范 / E2EE 字段结构”和前文消息示例为准；
- 服务端负责鉴权、路由、存储与转发，不应将可解密明文作为默认承诺；
- 多设备场景下应按设备分发 wrapped key，而不是复用单份接收密钥。

### TSID 与资源标识

- TSID 降低了直接枚举风险，但不是权限控制；
- 实际资源访问权限以后端鉴权和业务校验为准；
- TSID 规范参考 `src/lib/CLAUDE.md` 中 elib_tsid 章节。

---
## 扩展指南

### 添加新消息类型

#### 1. 定义 msg_type

在服务端代码中定义：

```erlang
-define(MSG_TYPE_NEW_TYPE, <<"new_type">>).
```

#### 2. 实现处理逻辑

```erlang
handle_new_type_msg(Uid, ToId, Payload) ->
    % 业务逻辑
    ok.
```

#### 3. 客户端处理

```dart
// 在消息处理代码中添加 case 分支
case msgType {
  'new_type':
    // 处理新消息类型
}
```

### 添加新 S2C action

#### 1. 定义 action

```erlang
-define(ACTION_NEW_ACTION, <<"new_action">>).
```

#### 2. 实现处理逻辑

```erlang
handle_new_action(Uid, Payload) ->
    % 业务逻辑
    ok.
```

#### 3. 客户端处理

```dart
// 在 MessageS2CService.switchS2C 中添加 case
case action {
  'new_action':
    // 处理新 action
}
```

---

## 最佳实践

### 客户端实现

#### 消息解析

```dart
// 解析 v2.0 消息
Map<String, dynamic> parseV2Message(String jsonStr) {
  final data = jsonDecode(jsonStr);

  final type = data['type'] as String;
  final action = data['action'] as String? ?? '';
  final msgType = data['msg_type'] as String? ?? '';

  if (type == 'S2C') {
    // S2C 消息：使用 action 字段
    final action = data['action'];
    // 处理 S2C 消息
  } else {
    // 非 S2C 消息：使用 msg_type 字段
    final msgType = data['msg_type'];
    // 处理普通消息
  }

  return data;
}
```

#### E2EE 解密

```dart
// 解密 E2EE 消息
Future<Map<String, dynamic>> decryptE2EEMessage(
  Map<String, dynamic> data,
) async {
  final e2ee = data['e2ee'] as Map<String, dynamic>?;
  if (e2ee == null || e2ee.isEmpty) {
    // 非加密消息，直接返回 payload
    return data['payload'] as Map<String, dynamic>;
  }

  // 加密消息，解密 ciphertext
  final decrypted = await E2EEService.decrypt(
    ciphertext: data['payload'],
    nonce: e2ee['nonce'],
    keys: e2ee['keys'],
  );

  return jsonDecode(decrypted);
}
```

### 服务端实现

#### 消息验证

```erlang
validate_v2_message(Msg) ->
    Type = maps:get(<<"type">>, Msg),
    Action = maps:get(<<"action">>, Msg, <<>>),
    MsgType = maps:get(<<"msg_type">>, Msg, <<>>),

    % 根据类型验证字段
    case Type of
        <<"S2C">> ->
            % S2C 消息必须有 action
            case Action of
                <<>> -> {error, missing_action};
                _ -> {ok, Msg}
            end;
        _ ->
            % 非 S2C 消息必须有 msg_type
            case MsgType of
                <<>> -> {error, missing_msg_type};
                _ -> {ok, Msg}
            end
    end.
```

#### 生成 v2.0 消息

##### 概述

生成 v2.0 消息时，核心原则是**根据 `type` 字段区分消息类型**：

- **S2C 消息**：使用 `action` 字段，`msg_type` 预留
- **非 S2C 消息**（C2C/C2G/C2S）：使用 `msg_type` 字段，`action` 为空

##### C2C/C2G 消息构建

**构建步骤**：

1. 生成消息 ID：`<type>.<tsid>.<timestamp>.<random>`
2. 设置 `type`：`"C2C"` 或 `"C2G"`
3. 设置 `from` 和 `to`：TSID 用户/群组 ID
4. 设置 `msg_type`：消息子类型（text/image/voice/video/file/location/custom）
5. 设置 `action`：空字符串 `""`
6. 设置 `e2ee`：加密时包含，否则空字符串 `""`
7. 设置 `payload`：根据 `msg_type` 填充内容
8. 设置 `created_at`：（可选）RFC3339 格式时间戳

**Dart 代码示例**：

```dart
Map<String, dynamic> buildC2CMessage({
  required String from,
  required String to,
  required String msgType,
  required dynamic payload,  // 非 E2EE: Map, E2EE: String
  bool isE2EE = false,
  Map<String, dynamic>? e2eeData,
}) {
  final now = DateTime.now().toUtc();
  final id = 'c2c.${from}.${now.millisecondsSinceEpoch}.${randomId()}';

  final message = <String, dynamic>{
    'id': id,
    'type': 'C2C',
    'from': from,
    'to': to,
    'msg_type': msgType,
    'action': '',  // 非 S2C 消息 action 为空
    'e2ee': isE2EE ? e2eeData : '',  // 加密时包含，否则为空
    'payload': payload,
    'created_at': now.toIso8601String(),
    'server_ts': 0,  // 服务端填充
  };

  return message;
}
```

**使用示例**：

```dart
// 发送文本消息
final message = buildC2CMessage(
  from: 'XyZ9aBcDeF',
  to: 'GhI8jKlMnO',
  msgType: 'text',
  payload: {'content': 'Hello World'},
);

// 发送加密文本消息
final e2eeResult = await buildE2EEData(
  plaintext: 'Secret Message',
  recipients: [deviceA, deviceB],
);
final encryptedMessage = buildC2CMessage(
  from: 'XyZ9aBcDeF',
  to: 'GhI8jKlMnO',
  msgType: 'text',
  isE2EE: true,
  e2eeData: e2eeResult['e2ee'],  // e2ee 元数据
  payload: e2eeResult['ciphertext'],  // 密文直接作为 payload
);
```

##### S2C 消息构建

**构建步骤**：

1. 生成消息 ID：`s2c.<tsid>.<timestamp>.<random>`
2. 设置 `type`：`"S2C"`
3. 设置 `from`：空字符串 `""`
4. 设置 `to`：接收方用户 ID
5. 设置 `action`：系统指令（pull_offline_msg/please_refresh_token 等）
6. 设置 `msg_type`：预留，可为空字符串 `""`
7. 设置 `e2ee`：空字符串 `""`（S2C 消息不加密）
8. 设置 `payload`：根据 `action` 填充内容

**Dart 代码示例**：

```dart
Map<String, dynamic> buildS2CMessage({
  required String to,
  required String action,
  Map<String, dynamic> payload = const {},
}) {
  final now = DateTime.now().toUtc();
  final id = 's2c.${to}.${now.millisecondsSinceEpoch}.${randomId()}';

  return <String, dynamic>{
    'id': id,
    'type': 'S2C',
    'from': '',  // S2C 消息 from 为空
    'to': to,
    'action': action,  // S2C 消息使用 action
    'msg_type': '',  // 预留字段
    'e2ee': '',  // S2C 消息不加密
    'payload': payload,
    'server_ts': now.millisecondsSinceEpoch,
  };
}
```

**使用示例**：

```dart
// 构建拉取离线消息通知
final pullMsg = buildS2CMessage(
  to: 'XyZ9aBcDeF',
  action: 'pull_offline_msg',
  payload: {'count': 5},
);

// 构建 Token 刷新请求
final refreshMsg = buildS2CMessage(
  to: 'XyZ9aBcDeF',
  action: 'please_refresh_token',
);
```

##### E2EE 消息特殊处理

**加密流程**：

1. 生成一次性对称密钥（AES-256-GCM）
2. 使用接收方公钥加密对称密钥（RSA-OAEP-256）
3. 使用对称密钥加密消息内容
4. 组装 `e2ee` 元数据字段
5. 将密文放入 `payload`

**e2ee 字段结构**（仅包含元数据）：

```json
{
  "e2ee": true,
  "e2ee_ver": 1,
  "e2ee_suite": "RSA-OAEP-256+AES-256-GCM",
  "nonce": "base64_encoded_nonce",
  "keys": [
    {
      "did": "deviceA",
      "kid": "key_v1",
      "wrap_alg": "RSA-OAEP-256",
      "ek": "base64_encoded_wrapped_key"
    }
  ]
}
```

**payload 结构**（加密消息）：

```json
"base64_encoded_ciphertext"
```

**Dart 代码示例**：

```dart
/// 构建 E2EE 元数据
/// 返回：{ e2ee: Map<String, dynamic>, ciphertext: String }
Future<Map<String, dynamic>> buildE2EEData({
  required String plaintext,
  required List<RecipientDevice> recipients,
}) async {
  // 1. 生成一次性对称密钥和 nonce
  final aesKey = generateRandomBytes(32);  // AES-256
  final nonce = generateRandomBytes(12);   // 推荐长度

  // 2. 加密明文
  final ciphertext = await encryptAESGCM(
    plaintext: plaintext,
    key: aesKey,
    nonce: nonce,
  );

  // 3. 为每个接收方设备包装密钥
  final keys = <Map<String, dynamic>>[];
  for (final recipient in recipients) {
    final wrappedKey = await wrapAESKey(
      aesKey: aesKey,
      publicKey: recipient.publicKey,
    );
    keys.add({
      'did': recipient.deviceId,
      'kid': recipient.keyId,
      'wrap_alg': 'RSA-OAEP-256',
      'ek': base64Encode(wrappedKey),
    });
  }

  // 4. 组装 e2ee 元数据和密文
  return {
    'e2ee': {
      'e2ee': true,
      'e2ee_ver': 1,
      'e2ee_suite': 'RSA-OAEP-256+AES-256-GCM',
      'nonce': base64Encode(nonce),
      'keys': keys,
    },
    'ciphertext': base64Encode(ciphertext),
  };
}
```

**对比**：非加密 vs 加密消息

| 字段 | 非加密消息 | 加密消息 |
|------|-----------|----------|
| `payload` | Map 对象（包含实际内容） | String（Base64 密文） |
| `e2ee` | 空字符串 `""` | 包含加密元数据（不含密文） |

##### 完整示例：消息生成器类

**工具类封装**：

```dart
class V2MessageBuilder {
  /// 构建 C2C/C2G 消息
  static Map<String, dynamic> buildChatMessage({
    required String type,      // 'C2C' 或 'C2G'
    required String from,
    required String to,
    required String msgType,   // 'text', 'image', 等
    required Map<String, dynamic> payload,
    String? createdAt,
  }) {
    final now = DateTime.now().toUtc();
    final id = '${type.toLowerCase()}.${from}.${now.millisecondsSinceEpoch}.${randomId()}';

    return {
      'id': id,
      'type': type,
      'from': from,
      'to': to,
      'msg_type': msgType,
      'action': '',
      'e2ee': '',
      'payload': payload,
      if (createdAt != null) 'created_at': createdAt,
      'server_ts': 0,
    };
  }

  /// 构建 S2C 消息
  static Map<String, dynamic> buildS2CMessage({
    required String to,
    required String action,
    Map<String, dynamic> payload = const {},
  }) {
    final now = DateTime.now().toUtc();
    final id = 's2c.${to}.${now.millisecondsSinceEpoch}.${randomId()}';

    return {
      'id': id,
      'type': 'S2C',
      'from': '',
      'to': to,
      'action': action,
      'msg_type': '',
      'e2ee': '',
      'payload': payload,
      'server_ts': now.millisecondsSinceEpoch,
    };
  }

  /// 构建 E2EE 消息
  static Future<Map<String, dynamic>> buildE2EEMessage({
    required String type,
    required String from,
    required String to,
    required String msgType,
    required String plaintext,
    required List<RecipientDevice> recipients,
  }) async {
    final result = await buildE2EEData(
      plaintext: plaintext,
      recipients: recipients,
    );

    return buildChatMessage(
      type: type,
      from: from,
      to: to,
      msgType: msgType,
      payload: result['ciphertext'],  // 密文直接作为 payload
    )..['e2ee'] = result['e2ee'];  // 元数据放入 e2ee
  }
}
```

**使用示例**：

```dart
// 1. 发送普通 C2C 消息
final normalMsg = V2MessageBuilder.buildChatMessage(
  type: 'C2C',
  from: 'XyZ9aBcDeF',
  to: 'GhI8jKlMnO',
  msgType: 'text',
  payload: {'content': 'Hello World'},
);

// 2. 发送 C2G 群聊消息
final groupMsg = V2MessageBuilder.buildChatMessage(
  type: 'C2G',
  from: 'XyZ9aBcDeF',
  to: 'GrOuPiD123',
  msgType: 'image',
  payload: {'url': 'https://...', 'width': 1024, 'height': 768},
);

// 3. 发送 E2EE 消息
final encryptedMsg = await V2MessageBuilder.buildE2EEMessage(
  type: 'C2C',
  from: 'XyZ9aBcDeF',
  to: 'GhI8jKlMnO',
  msgType: 'text',
  plaintext: 'Secret Message',
  recipients: [deviceA, deviceB],
);

// 4. 服务端构建 S2C 消息
final s2cMsg = V2MessageBuilder.buildS2CMessage(
  to: 'XyZ9aBcDeF',
  action: 'pull_offline_msg',
  payload: {'count': 5},
);
```

##### 构建验证清单

发送消息前，请验证：

- [ ] 消息 ID 格式正确：`<type>.<tsid>.<timestamp>.<random>`
- [ ] `type` 字段值有效：`C2C`/`C2G`/`C2S`/`S2C`
- [ ] S2C 消息：`action` 非空，`msg_type` 可为空
- [ ] 非 S2C 消息：`msg_type` 非空，`action` 为空
- [ ] E2EE 消息：`e2ee` 包含完整元数据，`payload` 为空
- [ ] 非 E2EE 消息：`e2ee` 为空字符串，`payload` 包含内容
- [ ] `from` 和 `to` 使用 TSID（S2C 的 `from` 除外）

---

## 迁移指南

### v1.0 → v2.0 迁移

#### 前端迁移步骤

1. **更新消息解析逻辑**
   - 将 `msg_type`、`action`、`e2ee` 从 payload 中读取改为从顶层读取
   - 根据 `type` 字段判断使用 `action` 还是 `msg_type`

2. **更新消息发送逻辑**
   - 发送消息时将 `msg_type`、`action`、`e2ee` 放到顶层
   - 根据 `type` 设置正确的字段

3. **更新数据库模型**
   - 修改 MessageModel 以适配新格式
   - 数据迁移脚本（可选）

#### 后端迁移步骤

1. **更新消息编码逻辑**
   - 将 `msg_type`、`action`、`e2ee` 从 payload 移到顶层
   - 根据 `type` 设置对应字段

2. **更新消息解码逻辑**
   - 从顶层读取 `msg_type`、`action`、`e2ee`
   - 字段验证逻辑调整

3. **更新数据库存储**
   - 调整消息表结构（如需要）

#### 新增功能 (v2.0.1+)

##### 消息验证功能

后端新增 `message_ds:validate_message/1` 函数，用于验证 v2.0 消息格式：

```erlang
%% 验证消息格式
case message_ds:validate_message(Msg) of
    {ok, ValidatedMsg} ->
        process_message(ValidatedMsg);
    {error, Reason} ->
        handle_error(Reason)
end.
```

**验证规则**：
- 必填字段：`id`、`type`、`from`、`to`
- S2C 消息：必须有 `action` 字段，不支持 `e2ee`
- 非 S2C 消息：必须有 `msg_type` 字段

##### 版本转换功能

后端新增 `message_ds:convert_v1_to_v2/1` 函数，自动将 v1.0 格式转换为 v2.0：

```erlang
%% 自动转换 v1.0 消息
V2Msg = message_ds:convert_v1_to_v2(V1Msg).
```

**转换逻辑**：
- 检测顶层是否有 `msg_type` 或 `action`
- 如果没有，从 `payload` 中提取到顶层
- 已是 v2.0 格式则直接返回

##### 消息监控指标

后端新增 `elib_metric:log_message_format/2` 函数，记录消息统计：

```erlang
%% 记录接收消息
ok = elib_metric:log_message_format(<<"in">>, IncomingMsg).

%% 记录发送消息
ok = elib_metric:log_message_format(<<"out">>, OutgoingMsg).
```

**统计指标**：
- 消息类型分布 (C2C/C2G/C2S/S2C)
- 消息内容类型分布 (text/image/voice/etc.)
- Action 分布 (S2C 消息)
- E2EE 使用率
- 消息方向 (in/out)

**查询指标**：
```erlang
%% 获取所有指标
Metrics = elib_metric:get_all_metrics().
%% 返回: #{counters => #{...}, histograms => #{...}}
```

#### 兼容性说明

v2.0 与 v1.0 **不兼容**，需要前后端**同步升级**。

建议部署流程：
1. 先部署服务端（支持 v1.0 和 v2.0 双格式解析）
2. 逐步更新客户端
3. 完成后移除 v1.0 支持

---

## 附录

### 版本历史

| 版本 | 日期 | 说明 |
|------|------|------|
| imboy.v2 frame | 2026-04-10 | 引入分层二进制帧协议（9 字节 header + payload），新增 `imboy.v2` 子协议、帧类型枚举、Flags 位图、frame 层心跳与 ACK/NACK；保留 JSON / Protobuf 旧子协议作为回退 |
| 2.0.0 | 2025-01-19 | 重构消息结构，字段提升到顶层 |
| 1.0.0 | 2025-01-06 | 初始版本 |

### 相关文档

- `docs/api/rest-api.md`
- `docs/standards/error-codes.md`
- ~~`docs/standards/hashid-encoding.md`~~ （已删除，TSID 迁移后不再使用 hashids）
- `docs/operations/security.md`
- `docs/api/e2ee_server_persisted_shard_contract_v1.md`
- `CLAUDE.md`

---

**文档维护**: 请在更新 WebSocket API 时同步更新此文档。
