# ImBoy WebSocket API 规范 v2.0

> **版本**: 2.0.0
> **协议**: WebSocket (RFC 6455)
> **编码**: UTF-8
> **消息格式**: JSON
> **变更说明**: 重构消息结构，将 msg_type/action/e2ee 提升到顶层字段

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

**服务端心跳**:

- 间隔：120 秒
- 格式：`ping`
- 响应：`pong`

**客户端心跳**:

- 间隔：建议 128 秒
- 格式：`ping`
- 响应：`pong`

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

## 消息格式规范

### 标准消息结构

#### 顶层字段

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `id` | binary | ✅ | 消息唯一标识符，格式：`<type>.<hashid>.<timestamp>.<random>` |
| `type` | binary | ✅ | 消息类型：`C2C`\|`C2G`\|`C2S`\|`S2C` |
| `from` | binary | ✅ | 发送方 ID (HashID 编码)，S2C 消息可能为空 |
| `to` | binary | ✅ | 接收方 ID (HashID 编码) |
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

### C2C - 单聊消息

#### 基础消息类型 (msg_type)

| msg_type | 说明 | Payload 示例 |
|----------|------|-------------|
| `text` | 文本消息 | `{"content": "Hello"}` |
| `image` | 图片消息 | `{"url": "...", "width": 1024, "height": 768}` |
| `voice` | 语音消息 | `{"url": "...", "duration": 30}` |
| `video` | 视频消息 | `{"url": "...", "duration": 60, "thumb": "..."}` |
| `file` | 文件消息 | `{"url": "...", "name": "doc.pdf", "size": 102400}` |
| `location` | 位置消息 | `{"latitude": 39.9, "longitude": 116.4, "address": "..."}` |
| `custom` | 自定义消息 | 自定义字段 |

### C2G - 群聊消息

#### 基础消息类型

与 C2C 相同，支持 `text`、`image`、`voice`、`video`、`file`、`location`、`custom`

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

### 错误响应格式

#### 顶层错误消息

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

#### S2C 错误消息

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

### 错误码定义

| code | 说明 | payload 要求 |
|------|------|-------------|
| 0 | 业务成功 | 可为空，根据业务需求结构不限不同 |
| 1 | 无需弹窗错误 | 可为空，记录日志后忽略 |
| 2 | 带 title 弹窗 | 必须包含 `title`, `content` |
| 3 | 无 title 弹窗 | 必须包含 `content` |
| 401 | 认证失败 | Token 无效或缺失 |
| 403 | 权限不足 | 无权限执行操作 |
| 404 | 资源不存在 | 用户/群组不存在 |
| 500 | 服务器错误 | 内部错误 |

---

## 安全规范

### 认证

#### Token 类型

- **JWT Token**: 用于用户认证
- **Refresh Token**: 用于刷新访问令牌
- **Device Token**: 用于设备唯一标识

### 加密

#### 传输层加密

- **生产环境**: 强制使用 `wss://` (WebSocket over TLS)
- **开发环境**: 可使用 `ws://`，但需明示风险

#### 内容加密 (E2EE)

对于敏感消息，可启用端到端加密（E2EE）：

- **密钥模型**: 发送方为每条消息生成一次性对称密钥
- **服务端职责**: 仅做鉴权、路由、存储与转发；不解密 `ciphertext`
- **多设备支持**: 每个接收设备一份独立的 wrapped key
- **e2ee**: 只存储元数据，ciphertext 存储在 payload

E2EE 消息示例（C2C 文本）：

```json
{
  "id": "c2c.x9j8.5ia0V5.Kr3aUs.F",
  "type": "C2C",
  "from": "XyZ9aBcDeF",
  "to": "GhI8jKlMnO",
  "msg_type": "text",
  "action": "",
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
        "ek": "b64_wrapped_aes_key"
      },
      {
        "did": "deviceB",
        "kid": "k3",
        "wrap_alg": "RSA-OAEP-256",
        "ek": "b64_wrapped_aes_key"
      }
    ]
  },
  "payload": "base64_ciphertext_with_tag",
  "server_ts": 1736141700000
}
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
HashId = elib_hashids:encode(UserId)  % "XyZ9aBcDeF"

% 解码为原始 ID
DecodedId = elib_hashids:decode(HashId)  % 12345
```

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

1. 生成消息 ID：`<type>.<hashid>.<timestamp>.<random>`
2. 设置 `type`：`"C2C"` 或 `"C2G"`
3. 设置 `from` 和 `to`：HashID 编码的用户/群组 ID
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
  final id = 'c2c.${hashid(from)}.${now.millisecondsSinceEpoch}.${randomId()}';

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

1. 生成消息 ID：`s2c.<hashid>.<timestamp>.<random>`
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
  final id = 's2c.${hashid(to)}.${now.millisecondsSinceEpoch}.${randomId()}';

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
    final id = '${type.toLowerCase()}.${hashid(from)}.${now.millisecondsSinceEpoch}.${randomId()}';

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
    final id = 's2c.${hashid(to)}.${now.millisecondsSinceEpoch}.${randomId()}';

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

- [ ] 消息 ID 格式正确：`<type>.<hashid>.<timestamp>.<random>`
- [ ] `type` 字段值有效：`C2C`/`C2G`/`C2S`/`S2C`
- [ ] S2C 消息：`action` 非空，`msg_type` 可为空
- [ ] 非 S2C 消息：`msg_type` 非空，`action` 为空
- [ ] E2EE 消息：`e2ee` 包含完整元数据，`payload` 为空
- [ ] 非 E2EE 消息：`e2ee` 为空字符串，`payload` 包含内容
- [ ] `from` 和 `to` 使用 HashID 编码（S2C 的 `from` 除外）

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
| 2.0.0 | 2025-01-19 | 重构消息结构，字段提升到顶层 |
| 1.0.0 | 2025-01-06 | 初始版本 |

### 相关文档

- **v1.0 API**: [websocket-api.md](./websocket-api.md)
- **数据库迁移**: [websocket-api-2-database-migration.md](./websocket-api-2-database-migration.md)
- **代码迁移计划**: [websocket-api-2-migration-plan.md](./websocket-api-2-migration-plan.md)
- **主文档**: [CLAUDE.md](../CLAUDE.md)

---

**文档维护**: 请在更新 WebSocket API 时同步更新此文档。
