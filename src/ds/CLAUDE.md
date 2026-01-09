[根目录](../CLAUDE.md) > **src/ds**

---

# DS 层 (src/ds/)

> **最后更新**: 2026-01-09 13:30:00 CST
> **模块数量**: 16 个 | **覆盖率**: 50%

## 模块职责

DS 层 (Data Service) 是 Imboy 系统的 **数据服务层**，位于 Logic 和 Repo 之间，负责：

1. **数据封装**: 封装数据操作，为 Logic 层提供清晰接口
2. **缓存管理**: 决定何时读缓存、何时读数据库
3. **数据组装**: 将多个 Repo 的数据组装为业务对象
4. **跨实体操作**: 协调多个 Repo 的操作

## 模块列表

### 用户与认证

| 模块 | 说明 |
|------|------|
| `user_ds.erl` | 用户数据服务 |
| `user_setting_ds.erl` | 用户设置数据服务 |
| `auth_ds.erl` | 认证数据服务 |
| `token_ds.erl` | Token 数据服务 |
| `account_ds.erl` | 账号数据服务 |

### 系统配置

| 模块 | 说明 |
|------|------|
| `config_ds.erl` | 配置数据服务 |

### 消息与通信

| 模块 | 说明 |
|------|------|
| `message_ds.erl` | 消息数据服务 |
| `msg_c2c_ds.erl` | 单聊消息数据服务 |
| `msg_c2g_ds.erl` | 群聊消息数据服务 |
| `msg_c2s_ds.erl` | C2S 消息数据服务 |
| `msg_s2c_ds.erl` | S2C 消息数据服务 |
| `msg_store_ds.erl` | 消息存储服务（异步批量写入） |
| `msg_store_sup.erl` | 消息存储监管树 |
| `msg_store_worker.erl` | 消息批量写入工作进程 |
| `websocket_ds.erl` | WebSocket 数据服务 |

### 关系与功能

| 模块 | 说明 |
|------|------|
| `friend_ds.erl` | 好友数据服务 |
| `friend_category_ds.erl` | 好友分组数据服务 |
| `group_ds.erl` | 群组数据服务 |
| `feedback_ds.erl` | 反馈数据服务 |
| `app_version_ds.erl` | 版本数据服务 |
| `app_ddl_ds.erl` | DDL 数据服务 |

## 对外接口

### 用户数据服务 (`user_ds.erl`)

```erlang
% 获取用户信息（带缓存）
user(Uid) -> Map | undefined

% 更新用户信息
update(Uid, Data) -> {ok, 1} | {error, Reason}

% 检查用户是否存在
exists(Uid) -> true | false
```

### 认证数据服务 (`auth_ds.erl`)

```erlang
% 验证 Token
verify_token(Token) -> {ok, Uid} | {error, Reason}

% 生成 Token
encrypt_token(Uid) -> {ok, Token}

% 刷新 Token
refresh_token(RefreshToken) -> {ok, NewToken} | {error, Reason}
```

### 配置数据服务 (`config_ds.erl`)

```erlang
% 获取配置
env(Key) -> Value
env(Key, Default) -> Value

% 重新加载配置
reload() -> ok
local_reload() -> ok
```

### 消息数据服务 (`message_ds.erl`)

```erlang
% 发送消息（带重试）
send_next(Uid, MsgId, Msg, RetryTimes, ExcludeDIDs, IsSync) -> ok

% 组装消息
assemble_msg(Type, To, From, Payload, MsgId) -> Map
```

### WebSocket 数据服务 (`websocket_ds.erl`)

```erlang
% 注册连接
register(Uid, DeviceId, DeviceType, Pid) -> ok

% 注销连接
unregister(Uid, DeviceId) -> ok

% 查找连接
find(Uid, DeviceId) -> Pid | undefined

% 所有连接
list_by_uid(Uid) -> [{Pid, DeviceInfo}]
```

## 关键依赖

### 上游调用
- `src/logic/`: Logic 层调用 DS 层

### 下层依赖
- `src/repo/`: DS 层调用 Repo 层
- `src/lib/imboy_cache.erl`: 缓存操作
- `src/lib/imboy_syn.erl`: 进程注册

## 数据模型

### 用户数据结构

```erlang
#{id => Uid,
  account => Account,
  nickname => Nickname,
  avatar => Avatar,
  status => Status,
  created_at => Timestamp}
```

### Token 数据结构

```erlang
#{uid => Uid,
  device_id => DeviceId,
  expire_at => Timestamp,
  refresh_token => RefreshToken}
```

## 缓存策略

### 缓存键格式

```erlang
% 用户信息
{user, Uid}

% 用户账号
{user_account, Account}

% Token
{token, Token}

% 配置
{config, Key}
```

### 缓存更新策略

```erlang
% 读缓存
case imboy_cache:get(Key) of
    undefined ->
        % 从数据库加载
        Data = load_from_db(),
        % 写入缓存
        imboy_cache:set(Key, Data, TTL),
        Data;
    Cached ->
        Cached
end.

% 更新时删除缓存
update(Uid, Data) ->
    user_repo:update(Uid, Data),
    imboy_cache:flush({user, Uid}),
    {ok, 1}.
```

## 核心流程

### Token 验证流程

```
1. 客户端请求携带 Token
2. auth_ds:verify_token(Token)
3. 检查缓存
   - 命中且有效 -> 返回 Uid
   - 未命中 -> 从数据库验证 -> 更新缓存
4. 检查设备是否在线
5. 返回验证结果
```

### 消息投递流程

```
1. Logic 层调用 message_ds:send_next
2. 查询用户在线状态（websocket_ds）
3. 用户在线：
   - 立即投递
   - 设置定时器
4. 用户离线：
   - 存储离线消息
5. 客户端确认：
   - 取消定时器
   - 删除离线消息
```

## 测试覆盖

### 测试文件

```
test/ds/
├── user_ds_tests.erl
├── auth_ds_tests.erl
├── config_ds_tests.erl
├── message_ds_tests.erl
└── ...
```

### 覆盖情况

- **覆盖率**: 约 50%
- **已测试**: 基本 CRUD 操作
- **待补充**: 缓存逻辑、边缘情况

## 常见问题

### Q: DS 层和 Repo 层的区别?

A:
- **DS 层**: 封装数据操作，管理缓存，可调用多个 Repo
- **Repo 层**: 直接操作数据库，一个 Repo 对应一张表

### Q: 何时使用缓存?

A:
- **读取频繁**: 用户信息、群组信息
- **计算密集**: 搜索结果、统计数据
- **短期有效**: Token、验证码

### Q: 缓存更新策略?

A:
- **写穿透**: 先更新数据库，再删除缓存
- **懒加载**: 缓存不存在时从数据库加载
- **TTL**: 设置合理的过期时间

## 相关文件

- `src/ds/`: 所有 DS 模块
- `src/repo/`: 数据仓库层
- `src/lib/imboy_cache.erl`: 缓存封装
- `test/ds/`: 测试文件

## 变更记录

### 2026-01-07
- 更新模块列表
- 更新覆盖率统计

### 2026-01-03
- 初始化 DS 层文档
- 整理核心接口和流程
