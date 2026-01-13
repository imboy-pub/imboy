# API 格式规范

> **协议**: HTTP/REST + WebSocket
> **编码**: UTF-8
> **消息格式**: JSON

---

## HTTP API 响应格式

### 标准响应结构

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {}
}
```

### 字段说明

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `code` | integer | ✅ | 响应状态码 |
| `msg` | binary | ✅ | 响应消息 |
| `payload` | map | ⚪ | 响应数据，可选 |

### 状态码分类

| code | 类型 | 说明 |
|------|------|------|
| `0` | 成功 | 操作成功 |
| `4xx` | 客户端错误 | 参数、认证、资源等错误 |
| `5xx` | 服务端错误 | 服务器内部问题 |
| `9xx` | 业务错误 | IM 业务特定错误 |

### 成功响应

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "id": "XyZ9aBcDeF",
    "name": "张三",
    "avatar": "https://example.com/avatar.jpg"
  }
}
```

### 错误响应

```json
{
  "code": 404,
  "msg": "用户不存在",
  "payload": {}
}
```

## WebSocket 消息格式

> **详细规范**: 请参阅 [websocket-api.md](../api/websocket-api.md) - 完整的 WebSocket API 规范文档

### 基础消息结构

```json
{
  "id": "msg_id",
  "type": "C2C",
  "from": "encoded_user_id",
  "to": "encoded_user_id",
  "payload": {
    "msg_type": "text",
    "text": "message content"
  },
  "created_at": 1650118822382,
  "server_ts": 1650118823376
}
```

### 字段说明

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `id` | binary | ✅ | 消息唯一标识符，格式：`<type>.<hashid>.<timestamp>.<random>` |
| `type` | binary | ✅ | 消息类型：`C2C`\|`C2G`\|`C2S`\|`S2C` |
| `from` | binary | ✅ | 发送方 ID (HashID 编码) |
| `to` | binary | ✅ | 接收方 ID (HashID 编码) |
| `payload` | map | ✅ | 消息载荷 |
| `created_at` | binary | ⚪ | 客户端创建时间 (RFC3339 格式)，可选 |
| `server_ts` | integer | ✅ | 服务端时间戳 (毫秒，UTC+0) |

### 消息类型

| 类型 | 说明 |
|------|------|
| **C2C** | 单聊消息 |
| **C2G** | 群聊消息 |
| **C2S** | 客户端请求（如 AI 机器人） |
| **S2C** | 服务端通知（如系统消息） |

## 响应处理建议

### 客户端处理流程

```javascript
// 伪代码
handleResponse(response) {
    if (response.code === 0) {
        // 成功
        onSuccess(response.payload);
    } else if (response.code >= 400 && response.code < 500) {
        // 客户端错误
        handleClientError(response.code, response.msg);
    } else if (response.code >= 500 && response.code < 600) {
        // 服务端错误
        handleServerError(response.code, response.msg);
    } else if (response.code >= 900 && response.code < 1000) {
        // 业务错误
        handleBusinessError(response.code, response.msg);
    } else {
        // 未知错误
        handleUnknownError(response);
    }
}
```

### 错误提示策略

| code | 弹窗类型 | 说明 |
|------|---------|------|
| 0 | 无弹窗 | 成功，正常处理 |
| 4xx | Toast/提示 | 客户端错误，轻提示 |
| 5xx | Alert | 服务端错误，需要用户知晓 |
| 9xx | Toast | 业务错误，轻提示 |

## 数据编码规范

### ID 字段编码

所有 ID 字段必须使用 HashID 编码：

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "id": "XyZ9aBcDeF",
    "uid": "XyZ9aBcDeF",
    "from": "XyZ9aBcDeF",
    "to": "GhI8jKlMnO"
  }
}
```

**详细规范**: [hashid-encoding.md](./hashid-encoding.md)

### 时间戳格式

- **server_ts**: 毫秒时间戳 (integer)，UTC+0
- **created_at**: RFC3339 格式 (binary)

```json
{
  "server_ts": 1736141700000,
  "created_at": "2025-01-06T12:35:00Z"
}
```

### 字符串编码

所有包含中文的字符串必须使用 UTF-8 编码：

```erlang
% Erlang 示例
Msg = <<"操作成功"/utf8>>,
```

**详细规范**: [utf8-encoding.md](./utf8-encoding.md)

## 错误码使用

### 错误码定义

所有错误码定义在 `include/error_code.hrl` 中：

```erlang
-define(ERR_OK, 0).
-define(ERR_BAD_REQUEST, 400).
-define(ERR_UNAUTHORIZED, 401).
-define(ERR_NOT_FOUND, 404).
-define(ERR_USER_NOT_FOUND, 940).
```

### 错误响应示例

```erlang
% 使用宏定义
elib_response:error(Req, <<"用户不存在"/utf8>>, ?ERR_USER_NOT_FOUND).

% 使用辅助函数
elib_response:error(Req, error_msg(?ERR_USER_NOT_FOUND), ?ERR_USER_NOT_FOUND).
```

**详细规范**: [error-codes.md](./error-codes.md)

## 分页格式

### 请求参数

```
GET /api/messages?page=1&limit=20
```

### 响应格式

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "list": [...],
    "total": 100,
    "page": 1,
    "limit": 20,
    "has_more": true
  }
}
```

### 字段说明

| 字段 | 类型 | 说明 |
|------|------|------|
| `list` | array | 数据列表 |
| `total` | integer | 总记录数 |
| `page` | integer | 当前页码 |
| `limit` | integer | 每页数量 |
| `has_more` | boolean | 是否有更多数据 |

## 批量操作格式

### 批量请求

```json
{
  "ids": ["XyZ9aBcDeF", "GhI8jKlMnO", "AbCdEfGhIj"]
}
```

### 批量响应

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "success": ["XyZ9aBcDeF", "GhI8jKlMnO"],
    "failed": [
      {
        "id": "AbCdEfGhIj",
        "error": "用户不存在"
      }
    ]
  }
}
```

## 相关文档

- **UTF-8 编码规范**: [utf8-encoding.md](./utf8-encoding.md)
- **错误码规范**: [error-codes.md](./error-codes.md)
- **HashID 编码规范**: [hashid-encoding.md](./hashid-encoding.md)
- **WebSocket API**: [websocket-api.md](../api/websocket-api.md)
- **主文档**: [CLAUDE.md](../../CLAUDE.md)
