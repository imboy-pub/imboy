# elib_async 使用指南

## 概述

`elib_async` 是 Imboy 项目的异步执行模块，封装了 `spawn` 和重试逻辑，提供简洁的异步操作 API。

---

## 🎯 核心函数

### 1. `async/1-2` - 基本异步执行

```erlang
% 无超时控制
elib_async:async(fun() ->
    io:format("Hello~n")
end).

% 带超时控制（5秒）
elib_async:async(fun() ->
    timer:sleep(10000),
    never_reached
end, 5000).
```

**使用场景：**
- 快速异步操作
- 需要超时控制的异步任务

---

### 2. `async_retry/1-3` - 异步执行带重试 ⭐ 推荐

```erlang
% 默认参数（3次重试，1秒延迟）
elib_async:async_retry(fun() ->
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end).

% 自定义重试次数
elib_async:async_retry(fun() ->
    user_repo:find_by_id(Uid)
end, 5).

% 自定义重试次数和延迟
elib_async:async_retry(fun() ->
    external_api:call(Url)
end, 3, 2000).
```

**使用场景：**
- 消息发送
- 数据库操作
- API 调用

---

### 3. `async_with_callback/2` - 异步执行带回调

```erlang
% 执行完成后将结果发送给 CallbackPid
elib_async:async_with_callback(fun() ->
    user_repo:find_by_id(Uid)
end, self()),

receive
    {async_result, {ok, User}} ->
        process_user(User);
    {async_result, {error, Reason}} ->
        handle_error(Reason)
end.
```

**使用场景：**
- 需要获取异步执行结果
- 需要处理异步操作的结果

---

## 📝 在 msg_c2c_logic.erl 中使用

### 原始代码（嵌套 spawn）

```erlang
%% ❌ 原始写法：嵌套 fun()
spawn(fun() ->
    try
        msg_store:enqueue(c2c, MsgId, #{
            payload => PayloadJson,
            from_id => CurrentUid,
            to_id => ToId,
            created_at => CreatedAtRfc,
            server_ts => NowTs
        }),

        Msg = #{...},
        MsgJson = jsone:encode(Msg, [native_utf8]),
        MsLi = [0, 5000, 7000, 11000, 17000],
        message_ds:send_next(ToId, MsgId, MsgJson, MsLi),
        io:format("✅ 消息发送成功")
    catch
        Type:Error:Stacktrace ->
            ?ERROR_LOG("❌ 消息发送错误: ~p:~p", [Type, Error])
    end
end).
```

---

### 改进代码 1：使用 `async_retry`（简洁）

```erlang
%% ✅ 改进：使用 elib_async:async_retry
elib_async:async_retry(fun() ->
    % ① 入队
    msg_store:enqueue(c2c, MsgId, #{
        payload => PayloadJson,
        from_id => CurrentUid,
        to_id => ToId,
        created_at => CreatedAtRfc,
        server_ts => NowTs
    }),

    % ② 投递消息
    Msg = #{...},
    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = [0, 5000, 7000, 11000, 17000],
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi),

    % ③ 记录日志
    io:format("✅ [C2C_END] Total time: +~pms~n",
              [erlang:monotonic_time(millisecond) - StartTime])
end, 3, 1000).
```

**优势：**
- ✅ 代码更简洁
- ✅ 自动重试（3次，1秒延迟）
- ✅ 错误自动记录到日志

---

### 改进代码 2：使用 `async_with_callback`（需要结果）

```erlang
%% ✅ 如果需要处理发送结果
elib_async:async_with_callback(fun() ->
    Msg = #{...},
    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = [0, 5000, 7000, 11000, 17000],
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end, self()),

receive
    {async_result, {ok, _}} ->
        io:format("✅ 消息发送成功");
    {async_result, {error, Reason}} ->
        ?ERROR_LOG("❌ 消息发送失败: ~p", [Reason])
end.
```

---

## 🔄 对比：原始 vs 改进

### 场景：ACK 超时监控

```erlang
%% ❌ 原始写法
spawn(fun() ->
    timer:sleep(15000),
    case imboy_cache:get(AckTimeoutKey) of
        {ok, _} ->
            io:format("⚠️ [ACK_TIMEOUT] ..."),
            imboy_cache:flush(AckTimeoutKey);
        _ ->
            ok
    end
end).

%% ✅ 改进写法
elib_async:async(fun() ->
    timer:sleep(15000),
    case imboy_cache:get(AckTimeoutKey) of
        {ok, _} ->
            io:format("⚠️ [ACK_TIMEOUT] ..."),
            imboy_cache:flush(AckTimeoutKey);
        _ ->
            ok
    end
end, 20000).  % 20秒超时
```

---

## 📊 API 对比表

| 原始写法 | 改进写法 | 优势 |
|---------|---------|------|
| `spawn(fun() -> ... end)` | `elib_async:async(fun() -> ... end)` | 语义清晰 |
| `spawn(fun() -> retry:with_retry(...) end)` | `elib_async:async_retry(fun() -> ... end)` | 简洁 |
| 手动接收结果 | `async_with_callback/2` | 自动回调 |

---

## 🎯 常见使用模式

### 模式 1：发送消息（不需要结果）

```erlang
% 最简洁
elib_async:async_retry(fun() ->
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end).
```

### 模式 2：发送消息（需要日志）

```erlang
% 带日志
elib_async:async_retry(fun() ->
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi),
    io:format("✅ 消息发送成功")
end).
```

### 模式 3：数据库操作（需要结果）

```erlang
% 带回调处理结果
elib_async:async_with_callback(fun() ->
    user_repo:find_by_id(Uid)
end, self()),

receive
    {async_result, {ok, User}} -> process_user(User);
    {async_result, {error, Reason}} -> handle_error(Reason)
end.
```

### 模式 4：多个异步操作

```erlang
% 入队和投递分开
elib_async:async(fun() ->
    msg_store:enqueue(...)
end),

elib_async:async_retry(fun() ->
    message_ds:send_next(...)
end, 3, 1000).
```

---

## ⚠️ 注意事项

### 1. 变量捕获

```erlang
% ✅ 正确：变量会被自动捕获
ToId = 123,
MsgId = <<"msg123">>,

elib_async:async_retry(fun() ->
    % 可以使用外部变量
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end).
```

### 2. 不能直接返回结果

```erlang
% ❌ 错误：async 立即返回 pid，不是结果
Result = elib_async:async_retry(fun() ->
    user_repo:find_by_id(Uid)
end),
% Result 是 pid()，不是用户数据！

% ✅ 正确：使用回调
elib_async:async_with_callback(fun() ->
    user_repo:find_by_id(Uid)
end, self()),
receive
    {async_result, {ok, User}} -> User
end.
```

### 3. 错误处理

```erlang
% async_retry 会自动记录错误日志
elib_async:async_retry(fun() ->
    risky_operation()
end).

% 如果需要自定义错误处理，使用 async_with_callback
elib_async:async_with_callback(fun() ->
    risky_operation()
end, self()),
receive
    {async_result, {ok, Result}} -> handle_success(Result);
    {async_result, {error, Reason}} -> handle_error(Reason)
end.
```

---

## 🚀 迁移指南

### 从原始 spawn 迁移到 elib_async

| 原始代码 | 迁移后 |
|---------|--------|
| `spawn(fun() -> op() end)` | `elib_async:async(fun() -> op() end)` |
| `spawn(fun() -> retry:with_retry(fun() -> op() end, 3, 1000) end)` | `elib_async:async_retry(fun() -> op() end, 3, 1000)` |
| 手动接收结果 | `elib_async:async_with_callback/2` |

---

## 💡 最佳实践

1. **不需要结果**：使用 `async_retry/1-3`
2. **需要结果**：使用 `async_with_callback/2`
3. **快速操作**：使用 `async/1-2`
4. **可能失败**：使用 `async_retry/1-3`

---

## 📚 相关模块

- `elib_retry` - 重试逻辑（内部使用）
- `imboy_cache` - 缓存操作
- `message_ds` - 消息数据服务
