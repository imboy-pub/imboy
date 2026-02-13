# DS 层文档 - 数据服务层

[根目录](../CLAUDE.md) > **src/ds**

> **最后更新**: 2026-01-31 21:30:00 CST
> **模块数量**: 15 个
> **职责**: 封装数据操作，调用 Repo 层访问数据库，提供缓存支持

---

## 模块职责

DS 层是 Imboy 系统的数据服务层，负责：
- 封装数据访问逻辑
- 调用 Repo 层进行数据库操作
- 提供缓存支持
- 消息投递服务
- 配置管理
- WebSocket 状态管理

---

## 入口与启动

DS 模块由 Logic 层调用：

```erlang
% Logic 调用 DS
{ok, User} = user_ds:find_by_uid(Uid).

% DS 调用 Repo
{ok, User} = user_repo:find_by_uid(Uid).
```

---

## 对外接口

### 用户相关 DS

| DS | 说明 |
|----|------|
| `user_ds.erl` | 用户数据服务 |
| `user_setting_ds.erl` | 用户设置 |
| `account_ds.erl` | 账户服务 |

### 认证相关 DS

| DS | 说明 |
|----|------|
| `auth_ds.erl` | 认证数据服务 |
| `token_ds.erl` | Token 管理 |

### 好友相关 DS

| DS | 说明 |
|----|------|
| `friend_ds.erl` | 好友数据服务 |
| `friend_category_ds.erl` | 好友分组 |

### 群组相关 DS

| DS | 说明 |
|----|------|
| `group_ds.erl` | 群组数据服务 |

### 消息相关 DS

| DS | 说明 |
|----|------|
| `msg_c2c_ds.erl` | 单聊消息服务（含已读回执） |
| `msg_c2g_ds.erl` | 群聊消息服务 |
| `msg_c2s_ds.erl` | 客户端请求服务 |
| `msg_s2c_ds.erl` | 系统消息服务 |
| `message_ds.erl` | 消息投递服务 |
| `msg_store_ds.erl` | 消息存储服务 |
| `msg_store_worker.erl` | 批量写入 Worker |

### 其他 DS

| DS | 说明 |
|----|------|
| `websocket_ds.erl` | WebSocket 状态管理 |
| `config_ds.erl` | 配置管理 |
| `feedback_ds.erl` | 反馈服务 |
| `app_version_ds.erl` | 版本服务 |
| `app_ddl_ds.erl` | DDL 配置 |
| `e2ee_social_ds.erl` | E2EE 社交恢复 |
| `e2ee_transfer_ds.erl` | E2EE 设备传输 |

---

## 关键依赖与配置

### 依赖的 Repo 模块

| DS | 依赖的 Repo |
|----|-------------|
| `user_ds` | `user_repo`, `user_setting_repo` |
| `friend_ds` | `friend_repo`, `friend_category_repo` |
| `group_ds` | `group_repo` |
| `msg_c2c_ds` | `msg_c2c_repo` |
| `auth_ds` | `user_repo` |

### 依赖的 Lib 模块

- `imboy_cache.erl`: 缓存操作
- `elib_pg.erl`: 数据库连接
- `imboy_syn.erl`: 分布式进程注册

---

## 数据模型

### 消息存储服务

`msg_store_ds.erl` 和 `msg_store_worker.erl` 实现消息批量写入：

```erlang
% 存储消息
ok = msg_store_ds:store(Msg).

% 批量写入
{ok, _} = msg_store_worker:write_batch(Msgs).
```

### 消息投递服务

`message_ds.erl` 实现消息投递：

```erlang
% 投递消息
ok = message_ds:send_next(Uid, Msg).
```

---

## 测试与质量

### 测试文件位置

```
test/ds/
├── account_ds_tests.erl
├── app_version_ds_tests.erl
├── auth_ds_tests.erl
├── feedback_ds_tests.erl
├── friend_category_ds_tests.erl
├── friend_ds_tests.erl
├── msg_c2c_ds_tests.erl
├── msg_c2g_ds_tests.erl
├── msg_c2s_ds_tests.erl
├── msg_s2c_ds_tests.erl
└── user_setting_ds_tests.erl
```

---

## 常见问题 (FAQ)

### Q: 如何添加新的数据服务?

1. 在 `src/ds/` 创建新的 DS 文件
2. 调用 Repo 层进行数据库操作
3. 编写测试

### Q: 如何实现缓存?

使用 `imboy_cache:get/1,2` 和 `imboy_cache:set/3,4`。

---

## 相关文件清单

### DS 文件 (15 个)

```
src/ds/
├── account_ds.erl
├── app_ddl_ds.erl
├── app_version_ds.erl
├── auth_ds.erl
├── config_ds.erl
├── e2ee_social_ds.erl
├── e2ee_transfer_ds.erl
├── feedback_ds.erl
├── friend_category_ds.erl
├── friend_ds.erl
├── group_ds.erl
├── login_attempt_ds.erl
├── message_ds.erl
├── msg_c2c_ds.erl
├── msg_c2s_ds.erl
├── msg_s2c_ds.erl
├── msg_store_ds.erl
├── msg_store_sup.erl
├── msg_store_worker.erl
├── token_ds.erl
├── user_ds.erl
├── user_setting_ds.erl
└── websocket_ds.erl
```

---

## 变更记录 (Changelog)

### 2026-01-31
- 新增 `e2ee_social_ds.erl` E2EE 社交恢复数据服务
- 新增 `e2ee_transfer_ds.erl` E2EE 设备传输数据服务
- 完善 DS 层文档

### 2026-01-20
- 新增 `msg_store_ds.erl` 消息存储服务
- 新增 `msg_store_worker.erl` 批量写入 Worker
- 完善 DS 层文档

---

**文档维护**: 请在添加新的数据服务时同步更新此文档。
