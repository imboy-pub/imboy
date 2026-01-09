[根目录](../CLAUDE.md) > **src/lib**

---

# Lib 层 (src/lib/)

> **最后更新**: 2026-01-07 10:05:54 CST
> **模块数量**: 29 个 | **覆盖率**: 75%

## 模块职责

Lib 层是 Imboy 系统的 **基础设施层 (Infrastructure Layer)**，提供：

1. **通用工具函数**: 字符串、日期、类型转换等
2. **数据库封装**: PostgreSQL 连接和查询
3. **缓存管理**: depcache 封装和分布式同步
4. **安全功能**: 加解密、哈希、密码处理
5. **请求响应**: HTTP 请求解析和响应格式化
6. **异步执行**: 异步任务和重试机制

## 模块列表

### 数据库相关

| 模块 | 说明 |
|------|------|
| `imboy_pg.erl` | 数据库连接，核心模块 |
| `imboy_pg_sql.erl` | SQL 构建器 |
| `epgsql_codec_rfc3339_bin.erl` | RFC3339 时间编解码器 |

### 缓存相关

| 模块 | 说明 |
|------|------|
| `imboy_cache.erl` | 缓存封装 |
| `imboy_cache_sync.erl` | 分布式缓存同步 |
| `imboy_syn.erl` | 分布式进程注册 |

### 安全相关

| 模块 | 说明 |
|------|------|
| `imboy_password.erl` | 密码哈希和验证 |
| `imboy_cipher.erl` | 加解密工具 |
| `imboy_hashids.erl` | ID 混淆编码 |
| `imboy_hasher.erl` | 哈希工具 |

### HTTP 相关

| 模块 | 说明 |
|------|------|
| `imboy_req.erl` | 请求处理 |
| `imboy_response.erl` | 响应处理 |
| `imboy_uri.erl` | URI 处理 |
| `imboy_param.erl` | 参数处理 |

### 工具函数

| 模块 | 说明 |
|------|------|
| `imboy_dt.erl` | 日期时间工具 |
| `imboy_str.erl` | 字符串工具 |
| `imboy_type.erl` | 类型转换工具 |
| `imboy_func.erl` | 函数工具 |
| `imboy_kv.erl` | 键值存储工具 |
| `imboy_cnv.erl` | 数据转换工具 |
| `imboy_log.erl` | 日志工具 |

### 异步执行与重试

| 模块 | 说明 |
|------|------|
| `imboy_async.erl` | 异步任务执行 |
| `imboy_retry.erl` | 同步重试逻辑 |

### 其他功能

| 模块 | 说明 |
|------|------|
| `imboy_cluster.erl` | 集群工具 |
| `imboy_dtl.erl` | 模板引擎封装 |
| `imboy_migrate.erl` | 数据库迁移 |
| `imboy_sms.erl` | 短信服务 |
| `qianfan_api.erl` | 千帆 AI API |
| `eunit_runner.erl` | EUnit 运行器 |

## 核心模块

### 数据库相关

#### `imboy_pg.erl` - 数据库连接

```erlang
% 执行查询
-spec query(iodata(), [term()]) ->
    {ok, [map()]} | {error, term()}.

% 事务封装
-spec with_tx(fun((epgsql:connection() | pid()) -> term())) -> term() | {rollback, term()}.
-spec with_tx(fun((epgsql:connection() | pid(), list()) -> term()), epgsql:transaction_opts()) ->
          term() | {rollback, term()} | no_return().

% 使用特定连接池
query(PoolName, Sql, Params) -> {ok, Cols, Rows}
```

#### `imboy_pg_sql.erl` - SQL 构建器

```erlang
% 构建 SELECT
select("users") |> where(#{<<"status">> => 1}) |> build()

% 构建 INSERT
insert("users", #{<<"name">> => <<"Alice">>}) |> build()

% 构建 UPDATE
update("users", #{<<"name">> => <<"Bob">>}) |> where(#{<<"id">> => 1}) |> build()
```

### 缓存相关

#### `imboy_cache.erl` - 缓存封装

```erlang
% 获取缓存
get(Key) -> Value | undefined

% 设置缓存
set(Key, Value) -> ok
set(Key, Value, TTL) -> ok

% 删除缓存
flush(Key) -> ok
```

#### `imboy_syn.erl` - 分布式进程注册

```erlang
% 初始化
init() -> ok

% 注册进程
register(Key, Pid) -> ok

% 查找进程
find(Key) -> Pid | undefined

% 发布消息
publish(Key, Msg) -> ok
```

### 安全相关

#### `imboy_password.erl` - 密码处理

```erlang
% 哈希密码
hash(Password) -> Hash

% 验证密码
verify(Password, Hash) -> true | false
```

#### `imboy_cipher.erl` - 加解密

```erlang
% AES 加密
aes_encrypt(PlainText, Key) -> CipherText

% AES 解密
aes_decrypt(CipherText, Key) -> PlainText

% RSA 加密
rsa_encrypt(PlainText, PublicKey) -> CipherText
```

#### `imboy_hashids.erl` - ID 混淆

```erlang
% 编码 UID
uid_encode(Uid) -> HashId

% 解码 HashId
uid_decode(HashId) -> Uid
```

### HTTP 相关

#### `imboy_req.erl` - 请求处理

```erlang
% 获取 Token
token(Req) -> {ok, Token} | {error, Reason}

% 获取 Body 参数
body_params(Req) -> Map

% 获取 Query 参数
query_params(Req) -> Map
```

#### `imboy_response.erl` - 响应处理

```erlang
% 成功响应
success(Data) -> Map

% 错误响应
error(Msg) -> Map
error(Code, Msg) -> Map

% 分页响应
page(Total, Page, Size, List) -> Map
```

### 工具函数

#### `imboy_dt.erl` - 日期时间

```erlang
% 当前时间戳（毫秒）
millisecond() -> integer()

% 当前时间戳（秒）
now() -> integer()

% 转换为 RFC3339
to_rfc3339(Timestamp) -> binary()
```

#### `imboy_str.erl` - 字符串工具

```erlang
% 二进制转整数
bin_to_int(Bin) -> integer()

% 拼接字符串
format(Format, Args) -> binary()
```

### 异步执行与重试

#### `imboy_async.erl` - 异步任务执行

```erlang
% 默认异步执行（无重试）
-spec async(fun()) -> pid().

% 异步执行（带超时，默认 5 秒）
-spec async(fun(), timeout()) -> pid().

% 异步执行（带重试，默认 3 次，1 秒延迟）
-spec async_retry(fun()) -> pid().

% 异步执行（带重试，自定义次数）
-spec async_retry(fun(), pos_integer()) -> pid().

% 异步执行（带重试，自定义次数和延迟）
-spec async_retry(fun(), pos_integer(), pos_integer()) -> pid().

% 异步执行（带回调）
-spec async_with_callback(fun(), pid()) -> pid().
```

#### `imboy_retry.erl` - 同步重试逻辑

```erlang
% 默认重试（3次，1秒延迟，指数退避）
-spec with_retry(fun()) -> {ok, term()} | {error, term()}.

% 自定义重试次数
-spec with_retry(fun(), pos_integer()) -> {ok, term()} | {error, term()}.

% 自定义重试次数和延迟（毫秒）
-spec with_retry(fun(), pos_integer(), pos_integer()) -> {ok, term()} | {error, term()}.

% 完整参数（退避策略：fixed | exponential | linear）
-spec with_retry(fun(), pos_integer(), pos_integer(), fixed | exponential | linear) -> {ok, term()} | {error, term()}.

% 带超时的重试
-spec with_retry_and_timeout(fun(), timeout(), pos_integer()) -> {ok, term()} | {error, term()}.
```

## 关键依赖

### 外部库
- `depcache`: 内存缓存
- `epgsql`: PostgreSQL 驱动
- `pooler`: 连接池
- `syn`: 分布式进程注册
- `hashids_erlang`: ID 编码

### 内部依赖
- `config_ds`: 配置管理

## 使用示例

### 数据库查询

```erlang
% 简单查询
Sql = "SELECT * FROM users WHERE id = $1",
{ok, _Cols, [Row]} = imboy_pg:query(Sql, [Uid]).

% 使用 SQL 构建器
Sql = imboy_pg_sql:select("users")
    |> imboy_pg_sql:where(#{<<"id">> => Uid})
    |> imboy_pg_sql:build(),
{ok, _Cols, [Row]} = imboy_pg:query(Sql, []).
```

### 缓存操作

```erlang
% 读取缓存
case imboy_cache:get({user, Uid}) of
    undefined ->
        % 从数据库加载
        User = user_repo:find_by_id(Uid),
        % 写入缓存（1小时）
        imboy_cache:set({user, Uid}, User, 3600),
        User;
    Cached ->
        Cached
end.
```

### 分布式进程注册

```erlang
% 注册用户进程
imboy_syn:register({user, Uid}, self()).

% 查找用户进程
case imboy_syn:find({user, Uid}) of
    undefined -> not_online;
    Pid -> {ok, Pid}
end.

% 发送消息给用户
imboy_syn:publish({user, Uid}, {msg, "Hello"}).
```

### 异步执行

```erlang
% 简单异步执行
imboy_async:async(fun() ->
    io:format("Hello from async~n")
end).

% 异步执行带重试（推荐用于消息发送）
imboy_async:async_retry(fun() ->
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end, 3, 1000).

% 异步执行带回调
imboy_async:async_with_callback(fun() ->
    user_repo:find_by_id(Uid)
end, self()),
receive
    {async_result, {ok, User}} -> process_user(User);
    {async_result, {error, Reason}} -> handle_error(Reason)
end.
```

### 同步重试

```erlang
% 默认重试（3次，1秒延迟，指数退避）
case imboy_retry:with_retry(fun() ->
    imboy_pg:query("SELECT 1", [])
end) of
    {ok, _Cols, _Rows} -> ok;
    {error, Reason} -> error_logger:error_msg("Query failed: ~p~n", [Reason])
end.

% 自定义重试参数
case imboy_retry:with_retry(Fun, 5, 2000, exponential) of
    {ok, Result} -> Result;
    {error, Reason} -> handle_error(Reason)
end.
```

## 测试覆盖

### 测试文件

```
test/lib/
├── imboy_pg_tests.erl
├── imboy_cache_tests.erl
├── imboy_password_tests.erl
├── imboy_cipher_tests.erl
├── imboy_async_tests.erl
├── imboy_retry_tests.erl
├── msg_store_ds_tests.erl
└── ...
```

### 覆盖情况

- **覆盖率**: 约 75%
- **已测试**: 核心工具函数
- **待补充**: 边缘情况、性能测试

## 常见问题

### Q: `imboy_pg` 和 `imboy_pg_sql` 的区别?

A:
- `imboy_pg`: 执行 SQL 查询，管理连接
- `imboy_pg_sql`: 构建 SQL 语句，防止注入

### Q: 何时使用 `imboy_syn`?

A:
- 需要跨节点查找进程
- 需要进程唯一性保证
- 需要发布订阅消息

### Q: 缓存同步如何工作?

A:
1. 节点 A 更新缓存
2. 通过 `imboy_cache_sync` 广播更新
3. 其他节点接收并更新本地缓存

### Q: `imboy_async` 和 `imboy_retry` 的区别?

A:
- `imboy_async`: 异步执行，立即返回 pid，不阻塞调用者
- `imboy_retry`: 同步执行，等待结果或重试失败，阻塞调用者

### Q: 消息队列如何使用?

A:
1. `msg_store_sup` 启动监管树
2. `msg_store_ds` 管理队列
3. `msg_store_worker` 处理消息写入
4. 通过 `enqueue` 添加消息

## 相关文件

- `src/lib/`: 所有基础库模块
- `src/ds/config_ds.erl`: 配置管理
- `test/lib/`: 测试文件
- `doc/libraries/async.md`: 异步执行详细指南
- `doc/libraries/retry.md`: 重试机制示例

## 变更记录

### 2026-01-07
- 更新模块列表
- 更新覆盖率统计

### 2026-01-05
- 新增 `imboy_async.erl` 异步执行模块文档
- 新增 `imboy_retry.erl` 重试逻辑模块文档
- 新增消息队列相关模块文档
- 完善异步执行与重试机制说明

### 2026-01-03
- 初始化 Lib 层文档
- 整理核心工具模块
