# Imboy WebSocket API 规范

> **版本**: 1.0.0
> **协议**: WebSocket (RFC 6455)
> **编码**: UTF-8
> **消息格式**: JSON

---

## 目录

- [概述](#概述)
- [连接管理](#连接管理)
- [消息格式规范](#消息格式规范)
- [消息类型定义](#消息类型定义)
- [消息流程](#消息流程)
- [错误处理](#错误处理)
- [安全规范](#安全规范)
- [扩展指南](#扩展指南)
- [最佳实践](#最佳实践)

---

## 概述

### 设计原则

Imboy WebSocket API 遵循以下设计原则：

1. **简单性**: 消息结构清晰，易于理解和实现
2. **可靠性**: 通过 ACK 机制和重试策略保证消息投递
3. **可扩展性**: 支持自定义消息类型和 payload 结构
4. **安全性**: HashID 混淆、Token 认证、加密传输
5. **向后兼容**: 新字段默认可选，旧客户端可忽略未知字段

### 消息流向

```
┌─────────────┐                    ┌─────────────┐                    ┌─────────────┐
│   Client    │◄──────────────────►│   Server    │◄──────────────────►│  Database   │
│   (APP)     │   WebSocket JSON    │  (Imboy)    │    PostgreSQL      │             │
└─────────────┘                    └─────────────┘                    └─────────────┘
       │                                   │                                   │
       │  1. 发送消息                        │                                   │
       ├──────────────────────────────────►│                                   │
       │  2. 服务端确认 (SERVER_ACK)        │                                   │
       │◄──────────────────────────────────┤                                   │
       │                                   │  3. 存储到数据库                   │
       │                                   ├──────────────────────────────────►│
       │                                   │  4. 查询接收方在线状态             │
       │                                   ├──────────────────────────────────►│
       │  5. 投递消息 (如果在线)           │                                   │
       │◄──────────────────────────────────┤                                   │
       │  6. 客户端确认 (CLIENT_ACK)       │                                   │
       ├──────────────────────────────────►│                                   │
       │                                   │  7. 清理定时器和离线消息           │
       │                                   ├──────────────────────────────────►│
```

### 核心概念

#### 消息类型 (Message Type)

- **C2C** (Client to Client): 单聊消息
- **C2G** (Client to Group): 群聊消息
- **C2S** (Client to Server): 客户端请求（如 AI 机器人）
- **S2C** (Server to Client): 服务端通知（如系统消息）

#### 消息确认 (Acknowledgment)

- **SERVER_ACK**: 服务端确认收到消息
- **CLIENT_ACK**: 客户端确认接收消息
- **重试机制**: 未确认消息自动重试

#### 消息操作 (Action)

- **普通消息**: 直接发送文本、图片等
- **message_revoke**: 撤销已发送的消息
- **message_edit**: 编辑已发送的消息

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
User-Agent: Imboy/1.0.0
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

#### 1. Token 认证 (推荐)

在握手时通过 `Authorization` header 传递 JWT Token：

```http
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

#### 2. 查询参数认证 (兼容)

Token 过期时，服务端仍接受连接，但会发送刷新 Token 消息：

```json
{
  "type": "S2C",
  "from": "",
  "to": "<user_id>",
  "payload": {
    "msg_type": "token_refresh_required",
    "content": "Token 已过期，请在 8 秒内刷新"
  }
}
```

### 心跳机制

**服务端心跳**:

- 间隔：30 秒
- 格式：`PING`
- 响应：`PONG`

**客户端心跳**:

- 间隔：建议 60 秒
- 格式：`PING`
- 响应：`PONG`

**示例**:

```
客户端 → 服务端: PING
服务端 → 客户端: PONG

服务端 → 客户端: PING
客户端 → 服务端: PONG
```

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
| 1002 | 协议错误 | 检查协议版本 |
| 1003 | 不支持的数据类型 | 检查消息格式 |
| 1006 | 异常关闭 | 立即重连 |
| 1008 | 策略违反 | 检查认证状态 |
| 1010 | 缺少扩展 | 更新客户端版本 |

#### 重连策略

```
第 1 次断开 → 立即重连
第 2 次断开 → 1 秒后重连
第 3 次断开 → 2 秒后重连
第 4 次断开 → 5 秒后重连
第 5 次断开 → 10 秒后重连
后续断开 → 指数退避，最大 60 秒
```

### 多设备管理

#### 设备类型

- `ios`: iOS 设备
- `android`: Android 设备
- `web`: Web 浏览器
- `desktop`: 桌面客户端

#### 设备互斥

同一用户在不同设备登录时，旧设备会收到通知：

```json
{
  "id": "msg_id",
  "type": "S2C",
  "from": "",
  "to": "<user_id>",
  "payload": {
    "msg_type": "logged_another_device",
    "content": "账号在其他设备登录"
  },
  "server_ts": 1672531200000
}
```

---

## 消息格式规范

### 标准消息结构

#### 顶层字段

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `id` | binary | ✅ | 消息唯一标识符，格式：`<type>.<hashid>.<timestamp>.<random>` |
| `type` | binary | ✅ | 消息类型：`C2C`\|`C2G`\|`C2S`\|`S2C` |
| `from` | binary | ✅ | 发送方 ID (HashID 编码)，S2C 消息可能为空 |
| `to` | binary | ✅ | 接收方 ID (HashID 编码) |
| `payload` | map | ✅ | 消息载荷，详见 [Payload 结构](#payload-结构) |
| `created_at` | binary | ⚪ | 客户端创建时间 (RFC3339 格式)，可选 |
| `server_ts` | integer | ✅ | 服务端时间戳 (毫秒，UTC+0) |

#### 示例

```json
{
  "id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2C",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "payload": {
    "msg_type": "text",
    "content": "Hello World"
  },
  "created_at": "2025-01-06T12:35:00Z",
  "server_ts": 1736141700000
}
```

### Payload 结构

#### 通用字段

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `msg_type` | binary | ✅ | 消息子类型，详见 [消息类型定义](#消息类型定义) |
| `content` | binary | ⚪ | 消息内容，根据 `msg_type` 不同而不同 |
| `action` | binary | ⚪ | 操作类型，用于撤销/编辑等操作 |
| `custom_type` | binary | ⚪ | 向后兼容字段，优先使用 `action` |

#### 元数据字段

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `original_msg_id` | binary | ⚪ | 原始消息 ID，用于撤销/编辑操作 |
| `edited_at` | integer | ⚪ | 编辑时间戳 (毫秒) |
| `revoked_at` | integer | ⚪ | 撤销时间戳 (毫秒) |

#### Payload 字段职责说明

##### msg_type 字段

**职责**：定义消息内容的类型，与用户看到的内容形式直接相关

**可选值**：
- `text`: 文本消息
- `image`: 图片消息
- `voice`: 语音消息
- `video`: 视频消息
- `file`: 文件消息
- `location`: 位置消息
- `custom`: 自定义消息

##### action 字段

**职责**：定义对消息的操作行为，与消息的生命周期管理相关

**撤销操作类型**：
- `message_revoke`: 消息撤销请求（根据 Msg.type 区分单聊/群聊）
- `message_revoke_ack`: 消息撤销确认（根据 Msg.type 区分单聊/群聊）

**编辑操作类型**：
- `message_edit`: 消息编辑请求（根据 Msg.type 区分单聊/群聊）
- `message_edit_ack`: 消息编辑确认（根据 Msg.type 区分单聊/群聊）

**普通消息**：
- `null` 或不包含此字段：表示普通消息（无特殊操作）

**注意**：action 类型通过 Msg.type 字段来区分是单聊(C2C)还是群聊(C2G)操作，简化了 action 类型的定义。采用 `message_` 前缀的命名规则，提高语义清晰度。

##### 消息处理优先级

1. 优先检查 `action` 字段
2. 如果 `action` 为空，则按普通消息处理
3. 向后兼容：如果 `custom_type` 存在且 `action` 为空，可使用 `custom_type` 作为 `action`

#### 示例

**普通消息**:
```json
{
  "msg_type": "text",
  "content": "Hello World"
}
```

**撤销请求**:
```json
{
  "msg_type": "custom",
  "action": "message_revoke",
  "content": "",
  "original_msg_id": "c2c.x9j8.5ia0V5.Kr3aUs.F"
}
```

**撤销确认**:
```json
{
  "msg_type": "custom",
  "action": "message_revoke_ack",
  "content": "",
  "original_msg_id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "revoked_at": 1736141700000
}
```

---

## 消息类型定义

### C2C - 单聊消息

#### 基础消息

| msg_type | 说明 | Payload 示例 |
|----------|------|-------------|
| `text` | 文本消息 | `{"content": "Hello"}` |
| `image` | 图片消息 | `{"url": "...", "width": 1024, "height": 768}` |
| `voice` | 语音消息 | `{"url": "...", "duration": 30}` |
| `video` | 视频消息 | `{"url": "...", "duration": 60, "thumb": "..."}` |
| `file` | 文件消息 | `{"url": "...", "name": "doc.pdf", "size": 102400}` |
| `location` | 位置消息 | `{"latitude": 39.9, "longitude": 116.4, "address": "..."}` |
| `custom` | 自定义消息 | `{"custom_type": "...", "data": {...}}` |

#### 操作消息

| action | 说明 | Payload 必需字段 |
|--------|------|-----------------|
| `message_revoke` | 撤销消息 | `original_msg_id` |
| `message_revoke_ack` | 撤销确认 | `original_msg_id`, `revoked_at` |
| `message_edit` | 编辑消息 | `original_msg_id`, `content` |
| `message_edit_ack` | 编辑确认 | `original_msg_id`, `content`, `edited_at` |

#### 完整示例

**发送文本消息**:
```json
{
  "id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2C",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "payload": {
    "msg_type": "text",
    "content": "Hello World"
  },
  "server_ts": 1736141700000
}
```

**撤销消息**:
```json
{
  "id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2C",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "payload": {
    "msg_type": "custom",
    "action": "message_revoke",
    "content": "",
    "original_msg_id": "c2c.x9j8.5ia0V5.Kr3aUs.F"
  },
  "server_ts": 1736141700000
}
```

### C2G - 群聊消息

#### 基础消息

与 C2C 相同，支持 `text`、`image`、`voice`、`video`、`file`、`location`、`custom`

#### 操作消息

与 C2C 相同，支持 `message_revoke`、`message_edit` 等

#### 群组特殊消息

| msg_type | 说明 | 触发条件 |
|----------|------|----------|
| `group_member_join` | 成员加入群组 | 新成员加入时发送给所有成员 |
| `group_member_leave` | 成员退出群组 | 成员退出时发送给所有成员 |
| `group_dissolve` | 群组解散 | 群主解散群组时发送 |
| `group_member_alias` | 修改群昵称 | 成员修改群昵称时发送 |

#### 完整示例

**发送群聊消息**:
```json
{
  "id": "c2g.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2G",
  "from": "XyZ9aBcDeF",
  "to": "GrOuPiD123",
  "payload": {
    "msg_type": "text",
    "content": "Hello Group"
  },
  "server_ts": 1736141700000
}
```

### C2S - 客户端请求

#### AI 机器人

| to 值 | 说明 | Payload 示例 |
|--------|------|-------------|
| `bot_qian_fan` | 千帆 AI 对话 | `{"msg_type": "text", "text": "...", "topic_id": 0}` |

#### 完整示例

```json
{
  "id": "c2s.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2S",
  "from": "XyZ9aBcDeF",
  "to": "bot_qian_fan",
  "payload": {
    "msg_type": "text",
    "text": "你好，请帮我写一首诗",
    "topic_id": 0
  },
  "server_ts": 1736141700000
}
```

### S2C - 服务端通知

#### 系统消息

| msg_type | 说明 | 处理建议 |
|----------|------|----------|
| `pull_offline_msg` | 拉取离线消息 | 客户端立即拉取离线消息 |
| `token_refresh_required` | Token 刷新请求 | 客户端 8 秒内刷新 Token |
| `logged_another_device` | 异地登录通知 | 提示用户并强制下线 |
| `online` | 用户上线通知 | 更新好友在线状态 |
| `offline` | 用户离线通知 | 更新好友离线状态 |
| `hide` | 用户隐身通知 | 更新好友隐身状态 |
| `apply_friend` | 好友申请通知 | 弹出好友申请对话框 |
| `apply_friend_confirm` | 好友申请确认 | 通知申请结果 |
| `user_cancel` | 用户注销通知 | 显示好友注销提示 |
| `in_denylist` | 对方已拉黑 | 显示拉黑提示 |
| `not_a_friend` | 非好友关系 | 显示非好友提示 |
| `not_group_member` | 非群成员 | 显示非群成员提示 |
| `permission_denied` | 权限不足 | 显示权限不足提示 |
| `internal_error` | 内部错误 | 显示系统错误提示 |

#### 完整示例

**Token 刷新请求**:
```json
{
  "id": "s2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "S2C",
  "from": "",
  "to": "XyZ9aBcDeF",
  "payload": {
    "msg_type": "token_refresh_required",
    "content": "Token 已过期，请在 8 秒内刷新"
  },
  "server_ts": 1736141700000
}
```

**拉取离线消息**:
```json
{
  "id": "s2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "S2C",
  "from": "",
  "to": "XyZ9aBcDeF",
  "payload": {
    "msg_type": "pull_offline_msg",
    "content": "您有新的离线消息"
  },
  "server_ts": 1736141700000
}
```

**好友申请**:
```json
{
  "id": "s2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "S2C",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "payload": {
    "msg_type": "apply_friend",
    "content": "",
    "from": {
      "source": "qrcode",
      "msg": "我是 nick leeyi",
      "remark": "leeyi101",
      "role": "all",
      "donotlookhim": false,
      "donotlethimlook": true
    },
    "to": {}
  },
  "server_ts": 1736141700000
}
```

---

## 消息流程

### 发送消息流程

```
┌──────────┐                    ┌──────────┐                    ┌──────────┐
│  Client  │                    │  Server  │                    │ Receiver │
└──────────┘                    └──────────┘                    └──────────┘
     │                               │                               │
     │  1. 发送消息 (Msg)              │                               │
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

#### 时间说明

| 步骤 | 时间消耗 | 说明 |
|------|---------|------|
| 1 → 2 | < 10ms | 网络延迟 |
| 2 → 3 | < 50ms | 服务端处理（验证、存储） |
| 3 → 1 | < 10ms | 网络延迟 |
| 总延迟 | < 70ms | 客户端感知延迟 |

### 接收消息流程

```
┌──────────┐                    ┌──────────┐                    ┌──────────┐
│ Receiver │                    │  Server  │                    │  Sender  │
└──────────┘                    └──────────┘                    └──────────┘
     │                               │                               │
     │  1. 接收消息 (Msg)              │                               │
     │◄───────────────────────────────┤                               │
     │                               │                               │
     │  2. 解析并显示消息              │                               │
     │  3. 返回 CLIENT_ACK            │                               │
     ├───────────────────────────────►│                               │
     │                               │  4. 清理定时器和离线消息         │
     │                               ├───────────────────────────────►│
     │                               │  5. 通知发送方消息已送达         │
```

### 消息确认机制

#### SERVER_ACK (服务端确认)

**格式**: 与原始消息相同，添加 `ack` 字段

```json
{
  "id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2C_SERVER_ACK",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "payload": {
    "msg_type": "custom",
    "action": "server_ack"
  },
  "server_ts": 1736141700000
}
```

**返回时机**: 客户端发送消息后立即返回

#### CLIENT_ACK (客户端确认)

**格式**: `CLIENT_ACK,<type>,<msg_id>,<device_id>`

```
CLIENT_ACK,C2C,c2c.x9j8.5ia0V5.Kr3aUs.F,device123
```

**服务端处理代码 (Erlang)**:

```erlang
% 客户端确认消息处理
websocket_handle({text, <<"CLIENT_ACK", Tail/binary>>}, State) ->
    [Type, MsgId, DID] = binary:split(Tail, <<",">>, [global]),
    ?LOG(["CLIENT_ACK", Tail]),
    CurrentUid = proplists:get_value(current_uid, State),
    case Type of
        <<"C2C">> ->
            websocket_logic:c2c_client_ack(MsgId, CurrentUid, DID),
            {ok, State, hibernate};
        <<"S2C">> ->
            websocket_logic:s2c_client_ack(MsgId, CurrentUid, DID),
            {ok, State, hibernate}
    end;
```

**返回时机**: 客户端接收并显示消息后返回

### 时间戳生成

**ServerTs 字段生成 (Erlang)**:

```erlang
% 获取服务器当前毫秒时间戳 (UTC+0)
Ts = imboy_dt:milliseconds(),
% 返回值示例: 1736141700000
```

### 重试机制

#### 重试策略

| 消息类型 | 重试间隔 (毫秒) |
|---------|---------------|
| C2C 单聊 | `[0, 5000, 7000, 11000, 17000]` |
| C2G 群聊 | `[0, 3500, 3500, 3000, 5000]` |
| S2C 系统消息 | `[0, 1500, 1500, 3000, 5000, 7000]` |

#### 重试流程

```
第 1 次投递 (0ms)
   ├─ 成功 → 结束
   └─ 失败 → 继续重试
       ↓
第 2 次投递 (5s)
   ├─ 成功 → 结束
   └─ 失败 → 继续重试
       ↓
第 3 次投递 (7s)
   ├─ 成功 → 结束
   └─ 失败 → 继续重试
       ↓
第 4 次投递 (11s)
   ├─ 成功 → 结束
   └─ 失败 → 存储为离线消息
```

#### 离线消息存储

**触发条件**: 4 次投递全部失败

**存储位置**: 数据库 `msg_c2c` / `msg_c2g` 表

**拉取方式**: 客户端上线后通过 HTTP API 拉取

```
GET /msg/offline?type=C2C&limit=50
```

---

## 错误处理

### 错误响应格式

#### 顶层错误消息

```json
{
  "type": "error",
  "code": 401,
  "payload": {
    "title": "认证失败",
    "content": "Token 无效或已过期"
  },
  "server_ts": 1736141700000
}
```

#### S2C 错误消息

```json
{
  "id": "s2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "S2C",
  "from": "",
  "to": "XyZ9aBcDeF",
  "payload": {
    "msg_type": "in_denylist",
    "content": "对方已将您加入黑名单"
  },
  "server_ts": 1736141700000
}
```

### 错误码定义

#### 顶层错误码

| code | 说明 | payload 要求 |
|------|------|-------------|
| 1 | 无需弹窗错误 | 可为空，记录日志后忽略 |
| 2 | 带 title 弹窗 | 必须包含 `title`, `content` |
| 3 | 无 title 弹窗 | 必须包含 `content` |
| 401 | 认证失败 | Token 无效或缺失 |
| 403 | 权限不足 | 无权限执行操作 |
| 404 | 资源不存在 | 用户/群组不存在 |
| 500 | 服务器错误 | 内部错误 |
| 705 | Token 过期 | 需要刷新 Token |
| 706 | Token 无效 | 需要重新登录 |
| 707 | 签名错误 | 需要更新客户端版本 |

#### S2C 消息错误码

| msg_type | 说明 | 处理建议 |
|----------|------|----------|
| `in_denylist` | 对方已拉黑 | 显示拉黑提示 |
| `not_a_friend` | 非好友关系 | 显示非好友提示 |
| `not_group_member` | 非群成员 | 显示非群成员提示 |
| `permission_denied` | 权限不足 | 显示权限不足提示 |
| `internal_error` | 内部错误 | 显示系统错误提示 |
| `c2s_unsupported` | 不支持的 C2S 消息 | 显示不支持提示 |

### 错误处理建议

#### 客户端

1. **静默处理**: `code = 1` 的错误只需记录日志
2. **用户提示**: `code = 2, 3` 的错误需要弹窗提示
3. **认证错误**: `code = 705, 706` 需要重新登录
4. **签名错误**: `code = 707` 需要强制更新

#### 服务端

1. **参数验证**: 验证所有必需字段
2. **权限检查**: 检查好友关系、群成员身份
3. **错误日志**: 记录所有错误信息
4. **友好提示**: 返回客户端可理解的错误消息

---

## 安全规范

### 认证

#### Token 类型

- **JWT Token**: 用于用户认证
- **Refresh Token**: 用于刷新访问令牌
- **Device Token**: 用于设备唯一标识

#### Token 刷新流程

```
┌──────────┐                    ┌──────────┐
│  Client  │                    │  Server  │
└──────────┘                    └──────────┘
     │                               │
     │  1. 收到 Token 过期消息        │
     │◄───────────────────────────────┤
     │                               │
     │  2. 使用 Refresh Token 刷新  │
     ├───────────────────────────────►│
     │                               │
     │  3. 获取新 Token              │
     │◄───────────────────────────────┤
     │                               │
     │  4. 继续使用连接               │
```

### 加密

#### 传输层加密

- **生产环境**: 强制使用 `wss://` (WebSocket over TLS)
- **开发环境**: 可使用 `ws://`，但需明示风险

#### 内容加密 (可选)

对于敏感消息，可使用端到端加密：

```erlang
% 客户端加密
EncryptedPayload = imboy_cipher:encrypt(Payload, SharedKey).

% 服务端转发 (不解密)
msg_ds:send_next(ToUid, #{<<"payload">> => EncryptedPayload}).

% 客户端解密
DecryptedPayload = imboy_cipher:decrypt(EncryptedPayload, SharedKey).
```

### 限流

#### 连接限流

- **单用户连接数**: 最多 5 个并发连接
- **IP 连接数**: 同一 IP 最多 100 个并发连接

#### 消息发送限流

- **单聊消息**: 每秒最多 10 条
- **群聊消息**: 每秒最多 5 条
- **系统消息**: 每秒最多 20 条

#### 违规处理

```
第 1 次违规 → 警告
第 2 次违规 → 暂时禁用 1 分钟
第 3 次违规 → 暂时禁用 10 分钟
第 4 次违规 → 永久封禁
```

### HashID 混淆

#### 目的

- 防止遍历攻击
- 保护用户隐私
- 隐藏真实 ID

#### 编码示例

```erlang
% 原始 ID
UserId = 12345

% 编码为 HashID
HashId = imboy_hashids:encode(UserId)  % "XyZ9aBcDeF"

% 解码为原始 ID
DecodedId = imboy_hashids:decode(HashId)  % 12345
```

#### 注意事项

- **所有客户端可见的 ID 必须编码**
- **数据库操作必须使用原始 ID**
- **消息 ID 使用特殊格式**: `<type>.<hashid>.<timestamp>.<random>`

---

## 扩展指南

### 添加新消息类型

#### 1. 定义 msg_type

```erlang
% 在 src/logic/msg_xxx_logic.erl 中定义
-define(MSG_TYPE_NEW_TYPE, <<"new_type">>).
```

#### 2. 实现处理逻辑

```erlang
% 在 Logic 层添加处理函数
handle_new_type_msg(Uid, ToId, Payload) ->
    % 业务逻辑
    ok.
```

#### 3. 更新路由

```erlang
% 在 src/api/websocket_handler.erl 中添加路由
case Payload of
    #{<<"msg_type">> := <<"new_type">>} ->
        msg_xxx_logic:handle_new_type_msg(Uid, ToId, Payload);
    _ ->
        handle_unknown_message(Payload)
end.
```

### 添加新 action

#### 1. 定义 action

```erlang
-define(ACTION_PIN_MESSAGE, <<"message_pin">>).
-define(ACTION_PIN_ACK, <<"message_pin_ack">>).
```

#### 2. 实现处理逻辑

```erlang
% 处理置顶消息请求
handle_message_action(pin, Uid, ToId, Payload) ->
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload),
    % 业务逻辑
    {ok, AckMsg} = assemble_pin_ack(OriginalMsgId),
    msg_ds:send_next(ToId, AckMsg).
```

#### 3. 客户端示例

```json
{
  "id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2C",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "payload": {
    "msg_type": "custom",
    "action": "message_pin",
    "content": "",
    "original_msg_id": "c2c.x9j8.5ia0V5.Kr3aUs.F"
  },
  "server_ts": 1736141700000
}
```

### 添加新 C2S Bot

#### 1. 定义 Bot ID

```erlang
-define(BOT_NEW_SERVICE, <<"bot_new_service">>).
```

#### 2. 实现处理逻辑

```erlang
% 在 src/logic/msg_c2s_logic.erl 中添加
c2s_to_new_service(Uid, Payload) ->
    Text = maps:get(<<"text">>, Payload),
    % 调用外部服务 API
    Response = call_external_api(Text),
    {ok, assemble_bot_response(Response)}.
```

#### 3. 客户端示例

```json
{
  "id": "c2s.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2S",
  "from": "XyZ9aBcDeF",
  "to": "bot_new_service",
  "payload": {
    "msg_type": "text",
    "text": "你好，请帮我翻译这段文字",
    "topic_id": 0
  },
  "server_ts": 1736141700000
}
```

### Payload 扩展

#### 添加新字段

```json
{
  "msg_type": "text",
  "content": "Hello",
  "new_field": "new_value"
}
```

**注意事项**:
- 新字段默认可选
- 旧客户端会忽略未知字段
- 避免修改现有字段的类型

#### 向后兼容

```erlang
% 服务端处理新字段
Payload = maps:get(<<"payload">>, Msg),
NewField = maps:get(<<"new_field">>, Payload, <<"default_value">>),
```

```javascript
// 客户端检查新字段
if (msg.payload.new_field !== undefined) {
  // 处理新字段
}
```

---

## 最佳实践

### 客户端实现

#### 1. 消息队列

```javascript
class MessageQueue {
  constructor() {
    this.pending = new Map(); // msg_id -> callback
    this.retry = new Map();   // msg_id -> retry_count
  }

  send(msg) {
    this.pending.set(msg.id, {
      msg,
      timestamp: Date.now(),
      retryCount: 0
    });
    ws.send(JSON.stringify(msg));
  }

  ack(msgId) {
    const pending = this.pending.get(msgId);
    if (pending) {
      this.pending.delete(msgId);
      console.log(`Message ${msgId} acknowledged`);
    }
  }

  retry(msgId) {
    const pending = this.pending.get(msgId);
    if (pending && pending.retryCount < 5) {
      pending.retryCount++;
      this.send(pending.msg);
    }
  }
}
```

#### 2. 消息去重

```javascript
class MessageDedup {
  constructor() {
    this.seen = new Set();
  }

  isDuplicate(msgId) {
    if (this.seen.has(msgId)) {
      return true;
    }
    this.seen.add(msgId);
    // 清理旧消息 ID (保留最近 10000 条)
    if (this.seen.size > 10000) {
      const first = this.seen.values().next().value;
      this.seen.delete(first);
    }
    return false;
  }
}
```

#### 3. 离线消息同步

```javascript
async function syncOfflineMessages() {
  const lastMsgId = localStorage.getItem('last_msg_id');
  const response = await fetch(`/msg/offline?since=${lastMsgId}&limit=50`);
  const messages = await response.json();

  for (const msg of messages) {
    if (!dedup.isDuplicate(msg.id)) {
      displayMessage(msg);
      sendClientAck(msg.id, msg.type);
      localStorage.setItem('last_msg_id', msg.id);
    }
  }
}
```

### 服务端实现

#### 1. 消息验证

```erlang
validate_message(Msg) ->
    Required = [<<"id">>, <<"type">>, <<"from">>, <<"to">>, <<"payload">>],
    case lists:all(fun(K) -> maps:is_key(K, Msg) end, Required) of
        true ->
            {ok, Msg};
        false ->
            {error, missing_required_fields}
    end.
```

#### 2. 限流实现

```erlang
% 使用 bucket 算法
check_rate_limit(Uid) ->
    Key = {rate_limit, Uid},
    case imboy_cache:get(Key) of
        {ok, Count} when Count >= ?MAX_MSG_PER_SECOND ->
            {error, rate_limit_exceeded};
        {ok, Count} ->
            imboy_cache:set(Key, Count + 1, 1000),
            ok;
        {error, not_found} ->
            imboy_cache:set(Key, 1, 1000),
            ok
    end.
```

#### 3. 消息投递优化

```erlang
% 批量投递群聊消息
broadcast_to_group(GroupId, Msg, MemberUids) ->
    % 使用 spawn_batch 并发投递
    imboy_func:spawn_batch(
        fun(Uid) -> msg_ds:send_next(Uid, Msg) end,
        MemberUids,
        100  % 每批 100 个并发
    ).
```

### 性能优化

#### 1. 消息压缩

对于大型 payload，使用压缩：

```erlang
% 服务端压缩
Compressed = zlib:compress(jsx:encode(Payload)),
Msg = #{<<"payload">> => base64:encode(Compressed)}.
```

```javascript
// 客户端解压
const compressed = atob(msg.payload);
const decompressed = pako.inflate(compressed, { to: 'string' });
const payload = JSON.parse(decompressed);
```

#### 2. 心跳优化

```erlang
% 动态调整心跳间隔
calculate_heartbeat_interval(NetworkQuality) ->
    case NetworkQuality of
        excellent -> 60000;  % 60 秒
        good -> 45000;         % 45 秒
        fair -> 30000;         % 30 秒
        poor -> 15000          % 15 秒
    end.
```

#### 3. 消息批量处理

```erlang
% 批量写入数据库
bulk_write_messages(Msgs) ->
    {ok, _} = imboy_pg:query(
        <<"INSERT INTO msg_c2c (from_id, to_id, payload) VALUES ",
          "(?1, ?2, ?3), (?4, ?5, ?6), ...">>,
        lists:flatten([format_msg(M) || M <- Msgs])
    ).
```

### 监控与日志

#### 关键指标

- **连接数**: 当前活跃连接数
- **消息量**: 每秒消息吞吐量
- **延迟**: 消息端到端延迟
- **重试率**: 消息重试比例
- **错误率**: 消息错误比例

#### 日志记录

```erlang
% 记录消息发送
?LOG_INFO([
    {uid, Uid},
    {to_id, ToId},
    {msg_type, MsgType},
    {action, "send_message"},
    {status, success}
]).
```

---

## 附录

### 相关文档

- **消息确认机制**: [libraries/message-ack.md](../libraries/message-ack.md)
- **类型规范**: [standards/type-specification.md](../standards/type-specification.md)
- **API 格式规范**: [standards/api-format.md](../standards/api-format.md)
- **主文档**: [CLAUDE.md](../CLAUDE.md)

### 核心文件

| 文件 | 职责 |
|------|------|
| `src/api/websocket_handler.erl` | WebSocket 连接处理 |
| `src/logic/websocket_logic.erl` | ACK 定时器管理 |
| `src/logic/msg_c2c_logic.erl` | 单聊消息逻辑 |
| `src/logic/msg_c2g_logic.erl` | 群聊消息逻辑 |
| `src/logic/msg_s2c_logic.erl` | 系统消息逻辑 |
| `src/logic/msg_c2s_logic.erl` | 客户端请求逻辑 |
| `src/ds/message_ds.erl` | 消息投递服务 |

### 版本历史

| 版本 | 日期 | 说明 |
|------|------|------|
| 1.0.0 | 2025-01-06 | 初始版本 |

---

**文档维护**: 请在更新 WebSocket API 时同步更新此文档。
