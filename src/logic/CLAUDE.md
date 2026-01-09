[根目录](../CLAUDE.md) > **src/logic**

---

# Logic 层 (src/logic/)

> **最后更新**: 2026-01-07 10:05:54 CST
> **模块数量**: 26 个 | **覆盖率**: 70%

## 模块职责

Logic 层是 Imboy 系统的 **应用层 (Application Layer)**，负责：

1. **业务逻辑实现**: 核心业务规则和流程
2. **事务协调**: 跨多个数据操作的事务管理
3. **数据组装**: 将多个数据源组装为业务对象
4. **缓存策略**: 决定何时读取/更新缓存
5. **外部服务调用**: 调用第三方 API（如千帆 AI）

## 模块列表

### 用户与认证

| 模块 | 说明 |
|------|------|
| `user_logic.erl` | 用户核心逻辑 |
| `user_server.erl` | 用户账号 GenServer |
| `auth_logic.erl` | 认证逻辑 |
| `passport_logic.erl` | 注册登录逻辑 |
| `user_device_logic.erl` | 设备管理逻辑 |
| `user_collect_logic.erl` | 收藏逻辑 |
| `user_denylist_logic.erl` | 黑名单逻辑 |
| `user_tag_logic.erl` | 用户标签逻辑 |
| `user_tag_relation_logic.erl` | 标签关联逻辑 |

### 好友与群组

| 模块 | 说明 |
|------|------|
| `friend_logic.erl` | 好友核心逻辑 |
| `friend_category_logic.erl` | 好友分组逻辑 |
| `group_logic.erl` | 群组核心逻辑 |
| `group_member_logic.erl` | 群成员逻辑 |
| `group_notice_logic.erl` | 群公告逻辑 |

### 消息处理

| 模块 | 说明 |
|------|------|
| `msg_c2c_logic.erl` | 单聊消息逻辑 |
| `msg_c2g_logic.erl` | 群聊消息逻辑 |
| `msg_c2s_logic.erl` | 客户端到服务器消息 |
| `msg_s2c_logic.erl` | 服务器到客户端消息 |

### 连接与通信

| 模块 | 说明 |
|------|------|
| `websocket_logic.erl` | WebSocket 业务逻辑 |
| `webrtc_ws_logic.erl` | WebRTC WebSocket 逻辑 |

### 功能扩展

| 模块 | 说明 |
|------|------|
| `location_logic.erl` | 位置服务逻辑 |
| `fts_logic.erl` | 全文搜索逻辑 |

### 管理后台

| 模块 | 说明 |
|------|------|
| `adm_user_logic.erl` | 后台用户管理 |
| `adm_passport_logic.erl` | 后台认证逻辑 |
| `adm_app_version_logic.erl` | 版本管理逻辑 |

## 对外接口

### 用户管理逻辑 (`user_logic.erl`)

```erlang
% 用户在线
user_logic:online(Uid, DeviceId, DeviceType, Meta) -> ok

% 用户离线
user_logic:offline(Uid, DeviceId) -> ok

% 更新用户资料
user_logic:update(Uid, Data) -> {ok, Map} | {error, Reason}

% 获取用户信息
user_logic:info(Uid) -> Map
```

### 认证逻辑 (`auth_logic.erl`)

```erlang
% 验证 Token
auth_logic:verify_token(Token) -> {ok, Uid} | {error, Reason}

% 刷新 Token
auth_logic:refresh_token(RefreshToken) -> {ok, NewToken} | {error, Reason}

% 检查权限
auth_logic:check_permission(Uid, Resource) -> ok | {error, Reason}
```

### 好友逻辑 (`friend_logic.erl`)

```erlang
% 添加好友
friend_logic:add(Uid, ToUid) -> ok | {error, Reason}

% 删除好友
friend_logic:delete(Uid, ToUid) -> ok

% 好友列表
friend_logic:list(Uid, Page, Size) -> {ok, List}
```

### 群组逻辑 (`group_logic.erl`)

```erlang
% 创建群组
group_logic:add(Uid, Name, Members) -> {ok, GroupId}

% 解散群组
group_logic:dissolve(GroupId, Uid) -> ok | {error, Reason}

% 群组详情
group_logic:detail(GroupId) -> Map
```

### 消息逻辑

#### 单聊消息 (`msg_c2c_logic.erl`)

```erlang
% 发送单聊消息
msg_c2c_logic:send(FromUid, ToUid, Payload) -> {ok, MsgId}

% 消息历史
msg_c2c_logic:history(Uid, ToUid, Page, Size) -> {ok, List}
```

#### 群聊消息 (`msg_c2g_logic.erl`)

```erlang
% 发送群聊消息
msg_c2g_logic:send(FromUid, GroupId, Payload) -> {ok, MsgId}

% 群消息历史
msg_c2g_logic:history(GroupId, Page, Size) -> {ok, List}
```

#### WebSocket 逻辑 (`websocket_logic.erl`)

```erlang
% 处理 C2S 消息
websocket_logic:c2s(MsgId, CurrentUid, Data) -> ok

% 取消重试定时器
websocket_logic:cancel_timer(CurrentUid, DID, MsgId) -> ok
```

## 关键依赖

### 上游依赖
- `src/ds/`: 数据服务层

### 下游调用
- `src/repo/`: 数据仓库层（通过 DS 层间接调用）
- `src/lib/imboy_cache.erl`: 缓存操作

## 核心流程

### 用户上线流程

```
1. WebSocket 连接建立
2. 验证 Token (auth_logic)
3. 用户上线 (user_logic:online)
4. 注册进程 (imboy_syn)
5. 加载离线消息 (msg_handler)
6. 发送 S2C 通知
```

### 消息发送流程

```
1. 客户端发送消息 (WebSocket)
2. Handler 解析参数
3. Logic 验证权限
4. 存储消息 (DS -> Repo)
5. 查询在线状态 (imboy_syn)
6. 投递消息 (message_ds:send_next)
7. 等待确认 (QoS 重试)
```

## 数据模型

### 用户在线状态

```erlang
#{uid => Uid,
  did => DeviceId,
  dtype => DeviceType,
  node => Node,
  pid => Pid}
```

### 消息结构

```erlang
#{id => MsgId,
  type => <<"C2C">> | <<"C2G">> | <<"S2C">>,
  from => FromUid,
  to => ToUid,
  payload => Payload,
  created_at => Timestamp}
```

## 测试覆盖

### 测试文件

```
test/logic/
├── user_logic_tests.erl
├── auth_logic_tests.erl
├── friend_logic_tests.erl
├── group_logic_tests.erl
├── msg_c2c_logic_tests.erl
├── msg_c2g_logic_tests.erl
└── ...
```

### 覆盖情况

- **覆盖率**: 约 70%
- **已测试**: 核心业务逻辑
- **待补充**: 边缘情况、并发场景

## 常见问题

### Q: Logic 层和 DS 层的区别?

A:
- **Logic 层**: 实现业务逻辑，可以调用多个 DS，协调事务
- **DS 层**: 封装数据操作，通常对应一个实体，主要负责缓存

### Q: 何时使用缓存?

A:
- **读多写少**: 用户资料、群组信息
- **计算密集**: 搜索结果、统计信息
- **短期缓存**: 验证码、Token 黑名单

### Q: 如何处理并发?

A:
- 使用 Erlang 进程隔离
- 使用 `syn` 注册唯一进程
- 数据库事务处理

## 相关文件

- `src/logic/user_server.erl`: 用户账号 GenServer
- `src/ds/`: 数据服务层
- `src/lib/imboy_syn.erl`: 分布式进程注册
- `test/logic/`: 测试文件

## 变更记录

### 2026-01-07
- 更新模块列表，补充完整模块信息
- 更新覆盖率统计

### 2026-01-03
- 初始化 Logic 层文档
- 整理核心接口和流程
