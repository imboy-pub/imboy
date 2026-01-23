# Logic 层文档 - 业务逻辑层

[根目录](../CLAUDE.md) > **src/logic**

> **最后更新**: 2026-01-20 08:48:18 CST
> **模块数量**: 26 个
> **职责**: 处理业务逻辑，调用 DS 层进行数据操作，实现核心业务功能

---

## 模块职责

Logic 层是 Imboy 系统的业务逻辑层，负责：
- 实现核心业务逻辑
- 调用 DS 层进行数据操作
- 处理消息投递与确认
- 用户状态管理
- 好友关系管理
- 群组管理
- 消息路由与分发

---

## 入口与启动

Logic 模块由 Handler 层或 DS 层调用：

```erlang
% Handler 调用 Logic
{ok, Result} = user_logic:profile(Uid).

% Logic 调用 DS
{ok, User} = user_ds:find_by_uid(Uid).
```

---

## 对外接口

### 用户相关 Logic

| Logic | 说明 |
|-------|------|
| `user_logic.erl` | 用户信息管理 |
| `user_server.erl` | 用户进程管理 |
| `user_device_logic.erl` | 设备管理 |
| `user_collect_logic.erl` | 收藏管理 |
| `user_denylist_logic.erl` | 黑名单管理 |
| `user_tag_logic.erl` | 用户标签 |
| `user_tag_relation_logic.erl` | 标签关系 |

### 认证相关 Logic

| Logic | 说明 |
|-------|------|
| `auth_logic.erl` | 认证逻辑 |
| `passport_logic.erl` | 登录注册 |

### 好友相关 Logic

| Logic | 说明 |
|-------|------|
| `friend_logic.erl` | 好友管理 |
| `friend_category_logic.erl` | 好友分组 |

### 群组相关 Logic

| Logic | 说明 |
|-------|------|
| `group_logic.erl` | 群组管理 |
| `group_member_logic.erl` | 群成员管理 |
| `group_notice_logic.erl` | 群公告 |

### 消息相关 Logic

| Logic | 说明 |
|-------|------|
| `msg_c2c_logic.erl` | 单聊消息逻辑 |
| `msg_c2g_logic.erl` | 群聊消息逻辑 |
| `msg_c2s_logic.erl` | 客户端请求逻辑 |
| `msg_s2c_logic.erl` | 系统消息逻辑 |
| `msg_ack_logic.erl` | 消息确认逻辑 |
| `message_router_logic.erl` | 消息路由器 |

---

## WebSocket API v2.0 消息格式

### C2C 消息格式（单聊）

客户端发送的 WebSocket 消息必须包含以下字段：

```erlang
# WebSocket API v2.0 - C2C 消息格式
#{
  <<"id">> => MsgId,                    % binary: 消息ID
  <<"type">> => <<"C2C">>,             % binary: 消息类型
  <<"from">> => FromId,                % binary: 发送者ID (hashids编码)
  <<"to">> => ToId,                    % binary: 接收者ID (hashids编码) ⚠️ 注意：不是 to_id
  <<"msg_type">> => MsgType,           % binary: 消息类型 (text/image/file等)
  <<"action">> => Action,              % binary: 动作类型 (可选)
  <<"e2ee">> => E2EEMap,               % map: E2EE元数据 (可选, 加密消息时必须有)
  <<"payload">> => Payload,            % binary|string: 消息内容 (加密时为密文base64)
  <<"created_at">> => CreatedAt        % integer: 创建时间戳(毫秒)
}
```

### 关键字段说明

| 字段 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `<<"to">>` | binary | 接收者ID (hashids编码) - **推荐使用** | `<<"gdwqa5">>` |
| `<<"from">>` | binary | 发送者ID (hashids编码) - **推荐使用** | `<<"p25vd5">>` |
| `<<"e2ee">>` | map | E2EE 元数据，必须是 **Map** 不是 JSON 字符串 | `#{<<"e2ee">> => true, ...}` |
| `<<"payload">>` | binary|string | 消息内容（明文时为 JSON 字符串，加密时为 base64 密文） | 见下方示例 |

> **注意**：`message_ds:decode_websocket_message/1` 已支持字段兼容性，同时接受 `to`/`from`（推荐）和 `to_id`/`from_id`（兼容）两种格式。

### E2EE 加密消息格式

当 `<<"msg_type">> = <<"e2ee">>` 时：

```erlang
# payload: base64(nonce).base64(ciphertext)
# e2ee: E2EE 元数据 (Map，不是 JSON 字符串!)
#{
  <<"e2ee">> => true,
  <<"e2ee_ver">> => 1,
  <<"e2ee_suite">> => <<"RSA-OAEP-256+AES-256-GCM">>,
  <<"nonce">> => Base64Nonce,
  <<"keys">> => [
    #{
      <<"did">> => DeviceId,
      <<"kid">> => KeyId,
      <<"wrap_alg">> => <<"RSA-OAEP-256">>,
      <<"ek">> => Base64EncryptedKey
    }
  ]
}
```

### 常见错误

#### 错误1: {badkey,<<"to">>}

**原因**: 客户端发送了 `<<"to_id">>` 而不是 `<<"to">>`

**解决方案**: 客户端使用 `to` 而不是 `to_id`

```dart
// ❌ 错误
{ 'to_id': 'gdwqa5' }

// ✅ 正确
{ 'to': 'gdwqa5' }
```

#### 错误2: e2ee 字段是 JSON 字符串

**原因**: 客户端将 e2ee Map 序列化为 JSON 字符串

**解决方案**: 客户端直接发送 Map，让 WebSocket 库自动编码

```dart
// ❌ 错误
'e2ee': json.encode(e2eeMap)  // 变成 JSON 字符串

// ✅ 正确
'e2ee': e2eeMap  // 直接发送 Map
```

### 其他 Logic

| Logic | 说明 |
|-------|------|
| `websocket_logic.erl` | WebSocket 业务逻辑 |
| `e2ee_logic.erl` | 端到端加密 |
| `location_logic.erl` | 位置服务 |
| `fts_logic.erl` | 全文搜索 |

### 管理后台 Logic

| Logic | 说明 |
|-------|------|
| `adm_passport_logic.erl` | 管理员认证 |
| `adm_app_version_logic.erl` | 版本管理 |
| `adm_user_logic.erl` | 用户管理 |

---

## 关键依赖与配置

### 依赖的 DS 模块

| Logic | 依赖的 DS |
|-------|-----------|
| `user_logic` | `user_ds`, `user_setting_ds` |
| `friend_logic` | `friend_ds`, `friend_category_ds` |
| `group_logic` | `group_ds` |
| `msg_c2c_logic` | `msg_c2c_ds`, `message_ds` |
| `auth_logic` | `auth_ds`, `token_ds` |
| `websocket_logic` | `websocket_ds` |

### 依赖的 Lib 模块

- `imboy_syn.erl`: 分布式进程注册
- `imboy_cache.erl`: 缓存操作
- `elib_async.erl`: 异步执行
- `elib_hashids.erl`: ID 编码/解码

---

## 数据模型

### 消息投递流程

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Handler   │───►│    Logic    │───►│      DS     │
└─────────────┘    └─────────────┘    └─────────────┘
                          │                   │
                          ▼                   ▼
                    ┌─────────────┐    ┌─────────────┐
                    │   Message   │    │    Repo     │
                    │   Router    │    └─────────────┘
                    └─────────────┘           │
                          │                   ▼
                          ▼            ┌─────────────┐
                    ┌─────────────┐    │ PostgreSQL  │
                    │   Client    │    └─────────────┘
                    └─────────────┘
```

---

## 测试与质量

### 测试文件位置

```
test/logic/
└── user_logic_tests.erl

test/api/
├── auth_logic_tests.erl
├── friend_logic_tests.erl
├── group_logic_tests.erl
├── group_member_logic_tests.erl
├── group_notice_logic_tests.erl
├── msg_c2c_logic_tests.erl
├── msg_c2g_logic_tests.erl
├── msg_s2c_logic_tests.erl
├── passport_logic_tests.erl
├── user_collect_logic_tests.erl
├── user_denylist_logic_tests.erl
├── user_device_logic_tests.erl
├── user_tag_logic_tests.erl
├── user_tag_relation_logic_tests.erl
└── websocket_logic_tests.erl
```

---

## 常见问题 (FAQ)

### Q: 如何添加新的业务逻辑?

1. 在 `src/logic/` 创建新的 logic 文件
2. 调用 DS 层进行数据操作
3. 编写测试

### Q: 如何实现消息重试?

使用 `elib_retry:with_retry/1,2,3,4` 或消息自带的定时器机制。

---

## 相关文件清单

### Logic 文件 (26 个)

```
src/logic/
├── adm_app_version_logic.erl
├── adm_passport_logic.erl
├── adm_user_logic.erl
├── auth_logic.erl
├── e2ee_logic.erl
├── friend_category_logic.erl
├── friend_logic.erl
├── fts_logic.erl
├── group_logic.erl
├── group_member_logic.erl
├── location_logic.erl
├── message_router_logic.erl
├── msg_ack_logic.erl
├── msg_c2c_logic.erl
├── msg_c2g_logic.erl
├── msg_c2s_logic.erl
├── msg_s2c_logic.erl
├── passport_logic.erl
├── user_collect_logic.erl
├── user_denylist_logic.erl
├── user_device_logic.erl
├── user_logic.erl
├── user_server.erl
├── user_tag_logic.erl
├── user_tag_relation_logic.erl
├── webrtc_ws_logic.erl
├── websocket_logic.erl
└── group_notice_logic.erl
```

---

## 变更记录 (Changelog)

### 2026-01-21
- **修复 E2EE API 权限**：允许任何登录用户获取其他用户的公钥
- **新增 WebSocket API v2.0 文档**：详细说明消息格式和常见错误
- **修复 WebSocket 消息字段只支持 `to`/`from`字段名格式（数据库存储的是对应的int类型 to_id 和 from_id）
- **字段类型说明**：
  - `<<"to">>` 字段值类型为 **binary**（hashids编码的字符串，如 `<<"gdwqa5">>`）
  - `<<"from">>` 字段值类型为 **binary**（hashids编码的字符串，如 `<<"p25vd5">>`）
  - 服务端内部解码后的 `ToId`/`FromId` 变量类型为 **integer**（如 `12345`）

### 2026-01-20
- 新增 `message_router_logic.erl` 消息路由器
- 新增 `e2ee_logic.erl` 端到端加密
- 完善 Logic 层文档

---

**文档维护**: 请在添加新的业务逻辑时同步更新此文档。
