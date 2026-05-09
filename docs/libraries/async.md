# `elib_async` 使用指南

> Last Updated: 2026-03-09  
> Status: 长期组件文档  
> Scope: 异步执行、重试、超时与回调场景的统一使用约定  
> Source of truth: `src/lib/elib_async.erl`  
> Related docs: `src/lib/elib_retry.erl`, `doc/architecture/overview.md`, `doc/operations/security.md`

## 1. 文档目的

本文档用于说明 `elib_async` 在当前仓库中的适用场景、接口差异和使用边界。

`elib_async` 的目标不是替代所有并发模型，而是把项目里常见的几类“轻量异步执行”收束为统一写法：

- 直接异步执行；
- 带超时保护的异步执行；
- 带重试的异步执行；
- 需要把结果回传给调用方的异步执行。

## 2. 接口总览

| 接口 | 返回值 | 适用场景 | 说明 |
|---|---|---|---|
| `async/1` | `pid()` | 纯 fire-and-forget | 直接 `spawn/1`，不带超时、不带重试 |
| `async/2` | `pid()` | 需要超时保护 | 超时后会结束内部执行进程并记录日志 |
| `async_retry/1-4` | `pid()` | 可能失败、需要自动重试 | 依赖 `elib_retry`，失败后统一记录日志 |
| `async_with_timeout/2` | `pid()` | 需要“超时 + 重试”组合 | 通过 `elib_retry:with_retry_and_timeout/3` 执行 |
| `async_with_callback/2` | `pid()` | 需要结果回传给调用方 | 执行完成后向指定进程发送 `{async_result, Result}` |

说明：这些接口的**直接返回值都是 `pid()`**，而不是业务结果。

## 3. 选择建议

### 3.1 不关心结果，只想异步跑掉

优先使用 `async/1`：

```erlang
elib_async:async(fun() ->
    io:format("hello from async~n")
end).
```

适合：轻量日志、非关键后台动作、一次性通知等。

### 3.2 不关心结果，但希望避免长时间卡住

优先使用 `async/2`：

```erlang
elib_async:async(fun() ->
    timer:sleep(5000),
    ok
end, 1000).
```

适合：可以异步执行，但必须设置执行上限的任务。

### 3.3 操作可能失败，希望自动重试

优先使用 `async_retry/1-4`：

```erlang
elib_async:async_retry(fun() ->
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end).
```

默认行为：

- 默认重试 3 次；
- 默认初始延迟 1000ms；
- 默认退避策略为 `exponential`。

如需更细控制，可传入重试次数、延迟和退避类型：

```erlang
elib_async:async_retry(fun() ->
    risky_operation()
end, 5, 500, linear).
```

### 3.4 需要拿到异步执行结果

优先使用 `async_with_callback/2`：

```erlang
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

适合：查询结果回传、异步计算结果回传、需要由调用方决定后续处理的场景。

## 4. 常见用法

### 4.1 消息投递：默认重试

```erlang
elib_async:async_retry(fun() ->
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end).
```

推荐原因：消息投递这类动作可能受到临时错误影响，重试往往比单次 `spawn` 更稳妥。

### 4.2 需要超时兜底的后台任务

```erlang
elib_async:async(fun() ->
    timer:sleep(15000),
    do_background_job()
end, 5000).
```

适合：允许异步，但不允许无限挂住的任务。

### 4.3 查询结果异步回传

```erlang
elib_async:async_with_callback(fun() ->
    user_repo:find_by_id(Uid)
end, self()),

receive
    {async_result, {ok, User}} ->
        process_user(User);
    {async_result, {error, Reason}} ->
        handle_error(Reason)
after 3000 ->
    timeout
end.
```

建议调用方自己给 `receive` 设置超时，避免等待方无限阻塞。

### 4.4 需要超时与重试组合

```erlang
elib_async:async_with_timeout(fun() ->
    maybe_slow_operation()
end, 5000).
```

适合：既可能失败，又不希望执行时间失控的异步任务。

## 5. 与原始 `spawn` 的迁移建议

| 原始写法 | 推荐替换 | 适用说明 |
|---|---|---|
| `spawn(fun() -> op() end)` | `elib_async:async(fun() -> op() end)` | 最基础的异步执行 |
| `spawn(fun() -> retry:with_retry(...) end)` | `elib_async:async_retry(...)` | 统一重试行为与日志 |
| `spawn(fun() -> ... end)` + 手写超时管理 | `elib_async:async/2` | 统一超时保护 |
| `spawn` 后手动回传消息 | `elib_async:async_with_callback/2` | 统一回传消息格式 |

## 6. 注意事项

### 6.1 返回的是 `pid()`，不是业务结果

下面这种写法是错误理解：

```erlang
Result = elib_async:async_retry(fun() ->
    user_repo:find_by_id(Uid)
end).
```

这里的 `Result` 是进程 ID，而不是用户数据。

### 6.2 `async_retry/1-4` 只负责重试和记日志

`async_retry/1-4` 适合“执行即可”的任务，不会主动把结果回传给调用方。

如果调用方必须拿到结果，应使用 `async_with_callback/2`，或者由上层明确设计消息通知机制。

### 6.3 外部变量会被闭包捕获，但仍要关注一致性

这种写法是允许的：

```erlang
ToId = 123,
MsgId = <<"msg123">>,

elib_async:async_retry(fun() ->
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end).
```

但要确保这些变量在异步执行语义下仍然成立，不要依赖“稍后可能被修改的上下文”。

### 6.4 回调消息格式应保持稳定

`async_with_callback/2` 的回调消息格式是：

```erlang
{async_result, {ok, Result}}
{async_result, {error, Reason}}
```

调用方应按这个格式接收，不要自行发散成多个变体。

## 7. 相关模块

- `src/lib/elib_async.erl`
- `src/lib/elib_retry.erl`
- `src/ds/message_ds.erl`
- `src/repo/user_repo.erl`
