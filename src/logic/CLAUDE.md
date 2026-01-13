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

### 2026-01-20
- 新增 `message_router_logic.erl` 消息路由器
- 新增 `e2ee_logic.erl` 端到端加密
- 完善 Logic 层文档

---

**文档维护**: 请在添加新的业务逻辑时同步更新此文档。
