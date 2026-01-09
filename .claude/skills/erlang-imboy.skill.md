# Erlang Imboy Development Skill

## Description
专门用于 imboy 项目开发的 Erlang/OTP 技能集，包含项目特有的代码模式、架构约定和最佳实践。

## Project Architecture
- **语言**: Erlang/OTP 28+
- **Web框架**: Cowboy 2.10
- **数据库**: PostgreSQL 18
- **依赖管理**: Erlang.mk
- **项目结构**: 单应用 4 层架构 (Handler → Logic → DS → Repo)

## Code Layering (4层架构)

项目遵循严格的 4 层代码分层：

```
┌─────────────────────────────────────────────────────────────┐
│                    Handler 层 (API)                          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │   HTTP REST  │  │   WebSocket  │  │   Admin API  │      │
│  │    Handler   │  │    Handler   │  │    Handler   │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    Logic 层 (业务逻辑)                        │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │ Friend Logic │  │  Group Logic │  │   Msg Logic  │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    DS 层 (数据服务)                           │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │   User DS    │  │   Auth DS    │  │   Config DS  │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    Repo 层 (数据访问)                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │  User Repo   │  │  Friend Repo │  │   Msg Repo   │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    PostgreSQL 数据库                          │
└─────────────────────────────────────────────────────────────┘
```

### 层级职责

| 层级 | 目录 | 职责 | 命名规则 |
|------|------|------|----------|
| Handler | `src/api/` | HTTP 请求处理、WebSocket 连接 | `*_handler.erl` |
| Logic | `src/logic/` | 业务逻辑实现 | `*_logic.erl` |
| DS | `src/ds/` | 数据操作封装、缓存管理 | `*_ds.erl` |
| Repo | `src/repo/` | SQL 查询执行 | `*_repo.erl` |
| Lib | `src/lib/` | 基础工具函数 | `imboy_*.erl` |

### 关键原则

**SOLID 原则**:
- **S** (Single Responsibility): 每层只负责自己的职责
- **O** (Open/Closed): 通过 behaviour 扩展，不修改现有代码
- **D** (Dependency Inversion): 依赖抽象接口，不依赖具体实现

**数据流规则**:
1. Handler 不能直接调用 Repo，必须通过 Logic
2. Logic 可以调用多个 DS，协调事务
3. DS 封装缓存策略，优先从缓存读取
4. Repo 只执行 SQL，不包含业务逻辑

## Code Conventions

### 数据格式规范

#### JSON 解析格式

**统一使用 `map` 格式** (2026-01-03 迁移完成):

```erlang
% ✅ 正确 - 使用 map
Data = jsone:decode(Msg, [{object_format, map}]),
Payload = maps:get(<<"payload">>, Data),
Text = maps:get(<<"text">>, Payload),

% ❌ 旧格式 - 不再使用 proplist
Data = jsone:decode(Msg, [{object_format, proplist}]),
Payload = proplists:get_value(<<"payload">>, Data),
```

#### Payload 格式转换

```
WebSocket 接收 (JSON string)
    ↓ jsone:decode [{object_format, map}]
Data (map)
    ↓ maps:get
Payload (map)
    ↓ jsone:encode [native_utf8]
PayloadJson (JSON binary)
    ↓
[存储] PostgreSQL (TEXT, 加密)
[传输] WebSocket (JSON)
```

#### 消息组装

```erlang
% message_ds:assemble_msg/5 返回 map
Msg = #{
    <<"id">> => MsgId,
    <<"type">> => Type,
    <<"from">> => From,
    <<"to">> => To,
    <<"payload">> => Payload,  % map 格式
    <<"server_ts">> => imboy_dt:millisecond()
}.

% 发送时编码为 JSON
MsgJson = jsone:encode(Msg, [native_utf8]).
```

### 模块生成

```bash
# REST Handler
make new t=imboy.rest_handler n=demo_handler

# Logic 层
make new t=imboy.logic n=demo_logic

# Repository 层
make new t=imboy.repository n=demo_repo

# DS 层
make new t=imboy.ds n=demo_ds
```

### 环境管理

```bash
IMBOYENV=local make run   # 本地开发
IMBOYENV=dev make run     # 开发环境
IMBOYENV=test make run    # 测试环境
IMBOYENV=pro make run     # 生产环境
```

### 热更新

```erlang
% 重新加载路由
Routes = imboy_router:get_routes(),
Dispatch = cowboy_router:compile(Routes),
cowboy:set_env(imboy_listener, dispatch, Dispatch).

% 重新加载配置
config_ds:local_reload().

% 重新加载所有模块
lm().
```

## Best Practices

### 异步执行与重试

Imboy 项目提供了统一的异步执行模块 `imboy_async` 和重试模块 `imboy_retry`：

```erlang
%% ========== imboy_async - 异步执行模块 ==========

% 简单异步执行（无重试）
imboy_async:async(fun() ->
    msg_c2c_repo:delete_overflow_msg(ToUid, Limit)
end).

% 异步执行带重试（推荐）
% 默认参数：3次重试，1秒延迟，指数退避
imboy_async:async_retry(fun() ->
    msg_store:enqueue(c2c, MsgId, Data)
end).

% 自定义重试次数和延迟
imboy_async:async_retry(fun() ->
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end, 3, 1000).  % 3次重试，1秒延迟

% ========== imboy_retry - 重试逻辑模块 ==========

% 同步重试（会阻塞）
case imboy_retry:with_retry(fun() ->
    msg_c2c_repo:write_msg(CreatedAt, MsgId, Payload, From, To, ServerTs)
end) of
    {ok, Result} -> ok;
    {error, Reason} -> {error, Reason}
end.

% 自定义重试次数和延迟
imboy_retry:with_retry(Fun, 5, 2000).  % 5次重试，2秒延迟

% 指定退避策略：fixed | exponential | linear
imboy_retry:with_retry(Fun, 3, 1000, exponential).

% 带超时的重试
imboy_retry:with_retry_and_timeout(Fun, 5000, 3).  % 5秒超时，3次重试
```

**何时使用**：
- ✅ **imboy_async**: 所有需要异步执行的场景（删除溢出消息、ACK 超时检测、消息投递）
- ✅ **imboy_async:async_retry**: 需要确保操作成功的异步任务
- ✅ **imboy_retry**: 同步调用，需要确保操作成功

**替换 spawn**：
```erlang
% ❌ 旧写法
spawn(fun() ->
    msg_store:enqueue(c2c, MsgId, Data)
end).

% ✅ 新写法（自动重试，更可靠）
imboy_async:async_retry(fun() ->
    msg_store:enqueue(c2c, MsgId, Data)
end).
```

### GenServer 使用

```erlang
%% 初始化
init([]) ->
    {ok, #state{}}.

%% 同步调用
handle_call(Request, From, State) ->
    {reply, Reply, State}.

%% 异步调用
handle_cast(Request, State) ->
    {noreply, State}.

%% 定时器/消息
handle_info(Info, State) ->
    {noreply, State}.

%% 清理资源
terminate(Reason, State) ->
    ok.

%% 代码热更新
code_change(OldVsn, State, Extra) ->
    {ok, State}.
```

### Supervisor 树

```erlang
%% One-for-one 策略
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    ChildSpecs = [
        #{id => worker1, start => {worker1, start_link, []},
          restart => permanent, shutdown => 5000, type => worker,
          modules => [worker1]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

### 错误处理

```erlang
% ✅ 好的做法 - 使用 case 进行模式匹配
case dangerous_operation() of
    {ok, Result} -> process(Result);
    {error, Reason} -> handle_error(Reason)
end.

% ✅ 好的做法 - 使用 try...catch 处理异常
try
    risky_operation()
catch
    Class:Reason:Stacktrace ->
        logger:error("Error: ~p:~p~nStack: ~p", [Class, Reason, Stacktrace]),
        {error, Reason}
end.

% ❌ 避免 - 除非必要，避免使用 catch 捕获所有异常
catch risky_operation()
```

### 未使用参数

使用下划线前缀标记未使用的参数：

```erlang
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.
```

## 消息队列机制

### 架构概述

Imboy 使用 **shq + PostgreSQL 备份表** 实现零丢失消息队列：

```
Logic 层
    │
    ├───► msg_store:stage()  ──► PostgreSQL 备份表
    │      (同步, ~50ms)                   msg_store_staging (TEXT)
    │
    └───► msg_store:enqueue() ──► shq 队列 (ETS)
           (异步, ~1ms)                    msg_store_shq
                                               │
                                               ▼
  Worker 进程 (msg_store_worker)
    │
    └───► 批量出队 (100条/1秒)
           │
           ├───► Payload 解码 (JSON binary → map) [C2C 需要获取字段]
           │
           ├───► msg_xxx_ds:write_msg() ──► 正式表
           │      (批量写入)                   msg_c2c/g/s
           │
           └───► msg_store:unstage() ──► 删除备份表记录
```

### 发送消息流程

```erlang
%% Logic 层发送消息 (msg_c2c_logic.erl)
c2c(MsgId, CurrentUid, Data) ->
    ...
    % 1. 提取 Payload (map 格式)
    Payload = maps:get(<<"payload">>, Data),

    % 2. 转换为 JSON binary (用于存储和传输)
    PayloadJson = jsone:encode(Payload, [native_utf8]),

    % 3. 立即响应客户端
    self() ! {reply, [{<<"id">>, MsgId}, {<<"type">>, <<"C2C_SERVER_ACK">>}]},

    % 4. 立即投递给接收方 (异步，带重试)
    imboy_async:async_retry(fun() ->
        message_ds:send_next(ToId, MsgId, MsgJson, [0, 5000, 7000, 11000])
    end),

    % 5. 写入备份表 (同步，使用 JSON binary)
    CreatedAtRfc = imboy_dt:to_rfc3339(CreatedAt),
    msg_store:stage(<<"c2c">>, MsgId, PayloadJson, CurrentUid, ToId, CreatedAtRfc, NowTs),

    % 6. 入队 (异步，使用 JSON binary)
    msg_store:enqueue(c2c, MsgId, #{
        payload => PayloadJson,  % JSON binary
        from_id => CurrentUid,
        to_id => ToId,
        created_at => CreatedAtRfc,
        server_ts => NowMS
    }),
    ok.
```

### Worker 处理流程

```erlang
%% Worker 批量写入 (msg_store_worker.erl)
batch_write_by_type(c2c, MsgList) ->
    lists:foreach(fun({c2c, MsgId, Data}) ->
        PayloadBin = maps:get(payload, Data),  % JSON binary

        % C2C 需要解码获取 created_at/server_ts 字段
        Payload = case is_binary(PayloadBin) of
            true ->
                try jsone:decode(PayloadBin, [{object_format, map}]) of
                    Map -> Map
                catch
                    _:_ -> #{<<"payload">> => PayloadBin}
                end;
            false when is_map(PayloadBin) ->
                PayloadBin
        end,

        FromId = maps:get(from_id, Data),
        ToId = maps:get(to_id, Data),
        CreatedAt = maps:get(<<"created_at">>, Payload, 0),
        ServerTs = maps:get(<<"server_ts">>, Payload, 0),

        % 写入数据库时使用 JSON binary
        case msg_c2c_ds:write_msg(CreatedAt, MsgId, PayloadBin, FromId, ToId, ServerTs) of
            {ok, _} ->
                msg_store:unstage(MsgId);
            {error, Reason} ->
                % 失败重新入队
                shq:in_r(?QUEUE_NAME, {c2c, MsgId, Data})
        end
    end, MsgList).
```

### 表膨胀控制

```erlang
%% 方式 1: TRUNCATE 清理 (推荐，立即释放磁盘空间)
msg_store_repo:truncate_processed().
% 流程: 保存未处理消息 → TRUNCATE → 恢复未处理消息

%% 方式 2: VACUUM 清理 (定期维护)
msg_store_repo:vacuum_table().
% 用途: 清理死元组，不锁表，建议低峰期执行

%% 方式 3: 定时任务配置 (每小时 TRUNCATE + 每天 VACUUM)
%% 在 imboy_cron 中添加:
{cron, {0, 0, every, hour}, {imboy_cron, cleanup_staging_table, []}}.
cleanup_staging_table() ->
    msg_store_repo:truncate_processed().

{cron, {0, 0, 3, daily}, {imboy_cron, vacuum_staging_table, []}}.
vacuum_staging_table() ->
    msg_store_repo:vacuum_table().
```

### 备份表结构

```sql
CREATE TABLE IF NOT EXISTS msg_store_staging (
    id BIGSERIAL PRIMARY KEY,
    msg_type VARCHAR(10) NOT NULL CHECK (msg_type IN ('c2c', 'c2g', 's2c', 'c2s')),
    msg_id VARCHAR(64) NOT NULL,
    payload TEXT NOT NULL,  -- 消息体 json 格式 (加密后 base64 字符串)
    from_id BIGINT NOT NULL,
    to_id BIGINT,
    to_id_list BIGINT[],
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    retry_count INTEGER DEFAULT 0,
    processed_at TIMESTAMPTZ,
    error_msg TEXT,
    CONSTRAINT uk_staging_msg_id UNIQUE (msg_type, msg_id)
) WITH (
  autovacuum_vacuum_scale_factor = 0.1,   -- 10% 数据变化触发 VACUUM
  autovacuum_vacuum_threshold = 50,        -- 至少 50 条变化触发
  autovacuum_analyze_scale_factor = 0.05,  -- 5% 数据变化触发 ANALYZE
  fillfactor = 70                          -- 页面填充率 70%
);
```

### 数据格式转换

```
客户端消息 (JSON)
    ↓ jsone:decode (map 格式)
Data (map) → Payload (map)
    ↓ jsone:encode
PayloadJson (JSON binary)
    ↓
[stage] → PostgreSQL (TEXT, 加密)
[enqueue] → shq 队列 (JSON binary)
    ↓
Worker 出队
    ↓ jsone:decode (如需访问字段)
Payload (map)
    ↓
msg_xxx_ds:write_msg (使用 JSON binary)
```

### CLIENT_ACK 确认机制

```erlang
%% WebSocket Handler 处理 ACK (websocket_handler.erl)
websocket_handle({text, <<"CLIENT_ACK,", Tail/binary>>}, State) ->
    [Type, MsgId, DID] = binary:split(Tail, <<",">>, [global]),

    % 1. 取消重试定时器
    websocket_logic:cancel_timer(CurrentUid, DID, MsgId),

    % 2. 设置 ACK 标志，防止后续超时重试
    AckReceivedKey = {ack_received, CurrentUid, DID, MsgId},
    imboy_cache:set(AckReceivedKey, true, 60000),

    % 3. 按消息类型处理 ACK，删除离线消息
    case Type of
        <<"C2C">> -> msg_c2c_logic:c2c_client_ack(MsgId, CurrentUid, DID);
        <<"C2G">> -> msg_c2g_logic:c2g_client_ack(MsgId, CurrentUid, DID);
        <<"S2C">> -> msg_s2c_logic:s2c_client_ack(MsgId, CurrentUid, DID);
        <<"C2S">> -> ok
    end,

    % 4. 返回 ACK 确认消息
    {reply, {text, jsone:encode(AckConfirmMsg)}, State, hibernate}.
```

### 队列监控

```erlang
% 查看队列状态
msg_store:status().
% 返回: #{queue_len => 42, staging_stats => #{pending => 5, processed => 100, total => 105}}

% 查看备份表统计
msg_store_repo:get_staging_stats().

% 查看表膨胀情况
SELECT
    schemaname,
    tablename,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS total_size,
    n_live_tup,
    n_dead_tup,
    round(100 * n_dead_tup / NULLIF(n_live_tup + n_dead_tup, 0), 2) AS dead_ratio
FROM pg_stat_user_tables
WHERE tablename = 'msg_store_staging';

% 查看待处理消息
% SQL: SELECT * FROM msg_store_staging WHERE processed_at IS NULL;
```

### 相关模块

| 模块 | 文件 | 职责 |
|------|------|------|
| 队列服务 | `src/lib/msg_store.erl` | 队列管理、备份表操作 |
| 队列 Supervisor | `src/lib/msg_store_sup.erl` | Worker 进程树管理 |
| 批量 Worker | `src/lib/msg_store_worker.erl` | 批量写入、启动恢复、Payload 解码 |
| 队列 Repo | `src/repo/msg_store_repo.erl` | 备份表 CRUD、TRUNCATE/VACUUM |
| C2C Logic | `src/logic/msg_c2c_logic.erl` | 单聊消息发送、Payload 转换 |
| C2G Logic | `src/logic/msg_c2g_logic.erl` | 群聊消息发送 |
| S2C Logic | `src/logic/msg_s2c_logic.erl` | 系统消息发送 |
| WebSocket Logic | `src/logic/websocket_logic.erl` | ACK 定时器管理、跨节点同步 |
| WebSocket Handler | `src/api/websocket_handler.erl` | ACK 处理、map 格式解析 |
| Message DS | `src/ds/message_ds.erl` | 消息投递重试、map 格式组装 |

## 消息确认机制

### 重试策略

```
0ms   → 第 1 次投递 (立即)
5s    → 第 2 次投递 (如果未确认)
7s    → 第 3 次投递 (如果未确认)
11s   → 第 4 次投递 (如果未确认)
之后  → 存储离线消息
```

### 幂等性保障

```erlang
% 超时重试时检查 ACK 标志，防止重复投递
websocket_info({timeout, _Ref, {MsLi, {Uid, DID, MsgId}, Msg}}, State) ->
    AckReceivedKey = {ack_received, Uid, DID, MsgId},
    case imboy_cache:get(AckReceivedKey) of
        {ok, true} ->
            % ACK 已接收，停止重试
            {ok, State, hibernate};
        undefined ->
            % 未收到 ACK，继续重试
            message_ds:send_next(Uid, MsgId, Msg, MsLi, [DID], true),
            {reply, {text, Msg}, State, hibernate}
    end.
```

## Database (PostgreSQL)

### Schema 设计
- 参考文档: `./doc/postgresql/`
- 基于版本: PostgreSQL 18
- **消息表统一使用 TEXT 类型存储 payload**（加密后 base64 字符串）

### 常用模式

```erlang
% 参数化查询，避免 SQL 注射
Sql = "SELECT * FROM users WHERE id = $1",
{ok, _, [{Result}]} = imboy_pg:query(Sql, [UserId]).

% 加密存储 (使用 imboy_hasher)
payload => {raw, imboy_hasher:encoded_val(PayloadJson)}
% 生成的 SQL: encode(encrypt('...', 'key', 'aes-cbc/pad:pkcs'), 'base64')
```

## Distributed Operations

### 节点管理

```erlang
% Ping 节点
net_adm:ping('imboy@api.docker.imboy.pub').

% 查看节点
net_adm:names().

% 连接节点
net_kernel:connect_node('imboy@api.docker.imboy.pub').

% 广播到所有节点（ACK 取消）
Nodes = [node() | nodes()],
rpc:multicall(Nodes, ?MODULE, handle_ack_cancel, [CurrentUid, DID, MsgId]).
```

### 进程注册

```erlang
% 注册用户进程
imboy_syn:register({user, Uid}, self()).

% 查找用户进程
case imboy_syn:find({user, Uid}) of
    undefined -> not_online;
    Pid -> {ok, Pid}
end.

% 发布消息给用户
imboy_syn:publish({user, Uid}, {msg, "Hello"}).
```

### 远程 Shell

```bash
_rel/imboy/bin/imboy remote_console

# 从外部连接
erl -name debug@127.0.0.1 -setcookie imboy
net_adm:ping('imboy@127.0.0.1').
```

## Testing

### 单元测试
参考: `./doc/test.md` 或使用 `eunit-testing` skill

### 压测
- 测试文档: `test/doc/test1.md`
- 结果: 100万+ TCP 连接，90分钟以上

## Release Management

### 构建发布

```bash
IMBOYENV=local make rel
IMBOYENV=local make relup  # 升级包
```

### 部署

```bash
./script/deploy.sh <ip> <new_version> <old_version>
```

## Code Quality Tools

### Dialyzer (静态分析)

```bash
make dialyze
```

### 代码格式化

```bash
# 格式检查
./efmt -c src/file.erl

# 自动格式化
./efmt -w src/file.erl
```

### 编译检查

```bash
make app  # 编译应用
```

## Common Tasks

当用户需要以下操作时，使用此 skill：
- 创建新的 Handler/Logic/Repository/DS 模块
- 实现业务逻辑
- 数据库操作（使用 map 格式）
- 实现消息队列功能（Payload 转换为 JSON binary）
- 处理 ACK 确认机制
- 防止表膨胀（TRUNCATE/VACUUM）
- 异步执行与重试（使用 `imboy_async:async_retry` 替代 `spawn`）
- 热更新代码
- 分布式节点操作
- 性能优化
- 问题调试

**代码规范提醒**：
- ❌ 禁止直接使用 `spawn`，统一使用 `imboy_async` 模块
- ✅ 所有需要异步执行的代码使用 `imboy_async:async_retry`
- ✅ 所有需要重试的同步操作使用 `imboy_retry:with_retry`

## API Documentation

- **项目总览**: `./CLAUDE.md`
- **API 参考**: `./doc/API定义.md`
- **消息类型**: `./doc/消息类型.md`
- **消息确认**: `./doc/message_ack.md`
- **设计思考**: `./doc/design_thinking.md`
- **数据库访问**: `./doc/database_access_llayer.md`
