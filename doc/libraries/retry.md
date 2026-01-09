# imboy_retry 使用示例

## 基本使用

### 示例 1：在 msg_c2c_logic.erl 中使用

```erlang
% 原代码（无重试）
spawn(fun() ->
    message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
end).

% 使用重试后
spawn(fun() ->
    try
        case imboy_retry:with_retry(fun() ->
            message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
        end, 3, 1000, exponential) of
            {ok, _} ->
                io:format("✅ 消息发送成功~n");
            {error, Reason} ->
                ?ERROR_LOG("❌ 消息发送失败: ~p~n", [Reason])
        end
    catch
        Type:Error:Stacktrace ->
            ?ERROR_LOG("❌ 消息发送错误: ~p:~p~nStack: ~p~n",
                       [Type, Error, Stacktrace])
    end
end).
```

### 示例 2：数据库操作重试

```erlang
% 在 Logic 层使用
get_user_with_retry(Uid) ->
    case imboy_retry:with_retry(fun() ->
        user_repo:find_by_id(Uid)
    end, 3, 500) of
        {ok, User} when User =/= #{} ->
            {ok, User};
        {ok, #{}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.
```

### 示例 3：API 调用重试

```erlang
% 调用外部 API
send_sms_with_retry(Phone, Message) ->
    case imboy_retry:with_retry(fun() ->
        imboy_sms:send(Phone, Message)
    end, 3, 2000, linear) of
        {ok, Result} ->
            {ok, Result};
        {error, max_retries_exceeded} ->
            {error, sms_send_failed}
    end.
```

### 示例 4：带超时的重试

```erlang
% 操作可能在5秒内超时
spawn(fun() ->
    case imboy_retry:with_retry_and_timeout(fun() ->
        slow_operation()
    end, 5000, 3, 2000) of
        {ok, Result} ->
            io:format("操作成功: ~p~n", [Result]);
        {error, max_retries_exceeded} ->
            io:format("所有重试均失败~n")
    end
end).
```

## 退避策略对比

### Fixed（固定延迟）

```erlang
% 每次重试间隔都是 1000ms
imboy_retry:with_retry(Fun, 3, 1000, fixed).

% 时间线：
% 0ms    - 第1次尝试
% 1000ms - 第2次尝试
% 2000ms - 第3次尝试
% 3000ms - 放弃
```

### Linear（线性增长）

```erlang
% 每次重试间隔递增 1000ms
imboy_retry:with_retry(Fun, 3, 1000, linear).

% 时间线：
% 0ms    - 第1次尝试
% 1000ms - 第2次尝试（延迟 1000ms）
% 3000ms - 第3次尝试（延迟 2000ms）
% 6000ms - 放弃
```

### Exponential（指数增长）

```erlang
% 每次重试间隔翻倍
imboy_retry:with_retry(Fun, 3, 1000, exponential).

% 时间线：
% 0ms    - 第1次尝试
% 1000ms - 第2次尝试（延迟 1000ms）
% 3000ms - 第3次尝试（延迟 2000ms）
% 7000ms - 放弃
```

## 最佳实践

### 1. 选择合适的重试次数

```erlang
% 网络 API - 重试次数多
imboy_retry:with_retry(ApiFun, 5, 1000).

% 数据库操作 - 重试次数少
imboy_retry:with_retry(DbFun, 2, 500).

% 关键操作 - 重试次数多且延迟长
imboy_retry:with_retry(CriticalFun, 5, 2000, exponential).
```

### 2. 记录详细日志

```erlang
spawn(fun() ->
    MsgId2 = MsgId,  % 捕获变量
    case imboy_retry:with_retry(fun() ->
        message_ds:send_next(ToId, MsgId2, MsgJson, MsLi)
    end, 3, 1000) of
        {ok, _} ->
            ?INFO_LOG("[SEND_SUCCESS] MsgId=~s", [MsgId2]);
        {error, Reason} ->
            ?ERROR_LOG("[SEND_FAILED] MsgId=~s, Reason=~p", [MsgId2, Reason])
    end
end).
```

### 3. 结合使用 spawn

```erlang
% 异步重试
spawn(fun() ->
    imboy_retry:with_retry(fun() ->
        % 可能失败的操作
        risky_operation()
    end, 3, 1000)
end).
```

### 4. 处理不同类型的错误

```erlang
spawn(fun() ->
    try
        imboy_retry:with_retry(fun() ->
            case do_something() of
                {ok, Result} -> Result;
                {error, temporary} -> erlang:error(temporary_error);
                {error, permanent} -> erlang:error(Permanent_error)
            end
        end, 3, 1000)
    catch
        exit:permanent_error ->
            % 永久错误，不重试
            {error, permanent};
        _:Error:_ ->
            % 其他错误，已重试
            {error, Error}
    end
end).
```

## 常见场景

### 场景 1：消息发送重试

```erlang
% 在 msg_c2c_logic.erl 中
c2c(MsgId, CurrentUid, Data) ->
    ...
    spawn(fun() ->
        ToId2 = ToId,  % 捕获变量
        MsgId2 = MsgId,
        MsgJson2 = MsgJson,
        MsLi2 = MsLi,

        case imboy_retry:with_retry(fun() ->
            message_ds:send_next(ToId2, MsgId2, MsgJson2, MsLi2)
        end, 3, 1000, exponential) of
            {ok, _} ->
                io:format("✅ [C2C_END] 消息发送成功");
            {error, Reason} ->
                ?ERROR_LOG("[C2C_SEND_FAILED] 重试失败: ~p", [Reason])
        end
    end),
    ok.
```

### 场景 2：缓存更新重试

```erlang
% 在 DS 层使用
update_user_cache(Uid, Data) ->
    imboy_retry:with_retry(fun() ->
        imboy_cache:set({user, Uid}, Data, 3600)
    end, 2, 100).
```

### 场景 3：数据库写入重试

```erlang
% 在 Repo 层使用
save_with_retry(Table, Data) ->
    case imboy_retry:with_retry(fun() ->
        imboy_pg:query(Sql, [Data])
    end, 3, 500, exponential) of
        {ok, _, _} = Result -> Result;
        {error, Reason} -> {error, Reason}
    end.
```

## 测试验证

```erlang
% 运行测试
erl -pa ebin -pa test/lib -include include

> eunit:test(imboy_retry_tests, [verbose]).
```

## 性能考虑

| 策略 | 适用场景 | 优点 | 缺点 |
|------|---------|------|------|
| **fixed** | 短暂故障 | 简单、可预测 | 可能加重负载 |
| **linear** | 网络波动 | 平衡重试 | 总耗时较长 |
| **exponential** | 长时间故障 | 减轻负载 | 首次失败后等待较长 |

**建议：**
- 短暂故障：`exponential` 或 `linear`
- 快速恢复：`fixed`
- 未知故障：`exponential`（默认）
