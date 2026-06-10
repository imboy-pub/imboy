# Logic 层 - 业务逻辑层

[根目录](../CLAUDE.md) > **src/logic** | 76 个模块（截至 2026-06，以 `find src/logic -name '*.erl'\|wc -l` 为准） | 职责：核心业务逻辑，调用 DS 层，处理消息路由与分发

---

## 模块清单

### 用户

| 模块 | 说明 |
|------|------|
| `user_logic` | 用户信息管理 |
| `user_server` | 用户进程管理 |
| `user_device_logic` | 设备管理 |
| `user_collect_logic` | 收藏管理 |
| `user_denylist_logic` | 黑名单管理 |
| `user_tag_logic` | 用户标签 |
| `user_tag_relation_logic` | 标签关系 |

### 认证

| 模块 | 说明 |
|------|------|
| `auth_logic` | 认证逻辑 |
| `passport_logic` | 登录注册 |

### 好友

| 模块 | 说明 |
|------|------|
| `friend_logic` | 好友管理 |
| `friend_category_logic` | 好友分组 |

### 群组

| 模块 | 说明 |
|------|------|
| `group_logic` | 群组管理 |
| `group_member_logic` | 群成员管理 |
| `group_notice_logic` | 群公告 |

### 消息

| 模块 | 说明 |
|------|------|
| `msg_c2c_logic` | 单聊消息逻辑（含已读回执） |
| `msg_c2g_logic` | 群聊消息逻辑 |
| `msg_c2s_logic` | 客户端请求逻辑 |
| `msg_s2c_logic` | 系统消息逻辑 |
| `msg_ack_logic` | 消息确认逻辑 |
| `message_router_logic` | 消息路由器（含 message_read 分支） |

### WebSocket & E2EE & 其他

| 模块 | 说明 |
|------|------|
| `websocket_logic` | WebSocket 业务逻辑 |
| `webrtc_ws_logic` | WebRTC 信令 |
| `e2ee_logic` | 端到端加密 |
| `e2ee_transfer_logic` | E2EE 设备间传输 |
| `e2ee_social_logic` | E2EE 社交恢复 |
| `location_logic` | 位置服务 |
| `fts_logic` | 全文搜索 |

### 管理后台

| 模块 | 说明 |
|------|------|
| `adm_passport_logic` | 管理员认证 |
| `adm_app_version_logic` | 版本管理 |
| `adm_user_logic` | 用户管理 |

---

## 依赖关系

| Logic | 依赖 DS | 依赖 Lib |
|-------|---------|---------|
| `user_logic` | `user_ds`, `user_setting_ds` | `imboy_cache` |
| `friend_logic` | `friend_ds`, `friend_category_ds` | — |
| `group_logic` | `group_ds` | — |
| `msg_c2c_logic` | `msg_c2c_ds`, `message_ds` | `elib_async` |
| `auth_logic` | `auth_ds`, `token_ds` | — |
| `websocket_logic` | `websocket_ds` | `imboy_syn` |
| 所有 Logic | — | `elib_tsid`, `elib_cnv` |

---

## WebSocket API v2.0 — C2C 消息格式

### 消息字段

| 字段 | 类型 | 说明 |
|------|------|------|
| `<<"id">>` | binary | 消息 ID |
| `<<"type">>` | binary | `<<"C2C">>` |
| `<<"from">>` | binary | 发送者 TSID 字符串（推荐，兼容 `from_id`） |
| `<<"to">>` | binary | 接收者 TSID 字符串（推荐，兼容 `to_id`） |
| `<<"msg_type">>` | binary | `text` / `image` / `file` / `e2ee` |
| `<<"payload">>` | binary | 消息内容（加密时为 base64 密文） |
| `<<"e2ee">>` | map | E2EE 元数据（必须是 Map，不是 JSON 字符串） |
| `<<"created_at">>` | integer | 客户端时间戳（毫秒），仅作业务时间参考 |

> `message_ds:decode_websocket_message/1` 同时接受 `to`/`from`（推荐）和 `to_id`/`from_id`（兼容）。

### E2EE 消息（msg_type = <<"e2ee">>）

```erlang
%% e2ee 字段结构（Map，不是 JSON 字符串）
#{
  <<"e2ee">>       => true,
  <<"e2ee_ver">>   => 1,
  <<"e2ee_suite">> => <<"RSA-OAEP-256+AES-256-GCM">>,
  <<"nonce">>      => Base64Nonce,
  <<"keys">>       => [#{<<"did">> => DeviceId, <<"kid">> => KeyId,
                         <<"wrap_alg">> => <<"RSA-OAEP-256">>,
                         <<"ek">> => Base64EncryptedKey}]
}
%% payload: base64(nonce).base64(ciphertext)
```

### 常见错误

| 错误 | 原因 | 修复 |
|------|------|------|
| `{badkey,<<"to">>}` | 客户端发送了 `to_id` 而非 `to` | 改用 `to` 字段名 |
| e2ee 解码失败 | e2ee 字段是 JSON 字符串 | 直接发送 Map，不要 `json.encode()` |

---

## 排序约束

- 客户端 `id` / `msg_id` 只保证唯一和近似时间有序
- 跨 DC / 跨节点不保证严格单调
- 严格顺序业务使用 `conv_seq`（服务端顺序字段）

---

## 调用链

```
Handler → Logic → DS → Repo → PostgreSQL
```

消息投递重试：2s → 5s → 7s → 11s，4 次失败后转离线存储

---

**文档维护**: 添加新 Logic 模块时同步更新此文档。
