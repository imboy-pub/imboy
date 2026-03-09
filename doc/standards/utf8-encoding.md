# UTF-8 字符串编码规范

> Last Updated: 2026-03-08  
> Status: 长期编码规范文档  
> Scope: Erlang 代码中的 UTF-8 字符串与中文文本处理  
> Related docs: `doc/standards/api-format.md`, `doc/standards/error-codes.md`

## 规则说明

在 Erlang 中，中文字符串必须使用 `/utf8` 后缀来确保正确的 Unicode 处理。

## 正确写法

```erlang
<<"操作成功"/utf8>>
<<"参数错误"/utf8>>
<<"登录已过期，请重新登录"/utf8>>
```

## 错误写法

```erlang
<<"操作成功">>   % ❌ 缺少 /utf8 后缀
```

## 注意事项

### 1. 必须添加 /utf8 后缀

所有包含中文的二进制字符串都必须添加 `/utf8` 后缀：

```erlang
% ✅ 正确
Msg = <<"操作成功"/utf8>>,

% ❌ 错误
Msg = <<"操作成功">>,
```

### 2. 纯 ASCII 不需要 /utf8

纯 ASCII 字符（如英文、数字、符号）不需要 `/utf8` 后缀：

```erlang
% ✅ 正确（纯 ASCII，不需要 /utf8）
<<"success">>
<<"error">>
<<"Hello World">>

% ✅ 正确（虽然加了也不算错，但不必要）
<<"success"/utf8>>
```

### 3. 错误码宏定义

错误码宏定义文件 `include/error_code.hrl` 中所有中文消息都要使用 `/utf8`：

```erlang
% include/error_code.hrl
-define(ERR_OK, 0).
-define(ERR_BAD_REQUEST, 400).

% 中文错误消息
-define(MSG_ERR_BAD_REQUEST, <<"参数错误"/utf8>>).
-define(MSG_ERR_UNAUTHORIZED, <<"未授权访问"/utf8>>).
```

### 4. 错误消息辅助函数

错误消息辅助函数 `error_msg/1` 的返回值也要使用 `/utf8`：

```erlang
% 示例错误消息函数
error_msg(?ERR_BAD_REQUEST) -> <<"参数错误"/utf8>>;
error_msg(?ERR_UNAUTHORIZED) -> <<"未授权访问"/utf8>>;
error_msg(?ERR_NOT_FOUND) -> <<"资源不存在"/utf8>>;
error_msg(_) -> <<"未知错误"/utf8>>.
```

## 实际应用示例

### HTTP 响应

```erlang
% 在 Handler 中返回错误响应
elib_response:error(Req, <<"用户不存在"/utf8>>, ?ERR_USER_NOT_FOUND).
```

### 日志输出

```erlang
% 使用 lager 记录日志
?LOG_INFO("用户 ~p 登录成功"/utf8, [Uid]),
?LOG_ERROR("操作失败: ~p"/utf8, [Reason]).
```

### 消息发送

```erlang
% WebSocket 消息
Msg = #{
    <<"type">> => <<"S2C">>,
    <<"payload">> => #{
        <<"msg_type">> <<"error">>,
        <<"content">> <<"操作失败，请重试"/utf8>>
    }
}.
```

## 常见错误

### ❌ 错误 1：忘记添加 /utf8

```erlang
% 错误
Msg = <<"操作成功">>,
```

**修复**：
```erlang
Msg = <<"操作成功"/utf8>>,
```

### ❌ 错误 2：混合中英文时未使用 /utf8

```erlang
% 错误
Msg = <<"Error: 操作失败">>,
```

**修复**：
```erlang
Msg = <<"Error: 操作失败"/utf8>>,
```

### ❌ 错误 3：字符串拼接时丢失 /utf8

```erlang
% 错误
Msg = <<"操作成功，" ++ "已完成">>,
```

**修复**：
```erlang
Msg = <<"操作成功，已完成"/utf8>>,
% 或者
Part1 = <<"操作成功，"/utf8>>,
Part2 = <<"已完成"/utf8>>,
Msg = <<Part1/binary, Part2/binary>>,
```

## 工具函数

### 检查二进制是否为有效 UTF-8

```erlang
%% @doc 检查二进制是否为有效的 UTF-8 编码
-spec is_valid_utf8(binary()) -> boolean().
is_valid_utf8(Binary) ->
    try
        _ = unicode:characters_to_list(Binary, utf8),
        true
    catch
        _:_ -> false
    end.
```

### 强制转换为 UTF-8

```erlang
%% @doc 确保二进制是有效的 UTF-8 编码
-spec ensure_utf8(binary() | string()) -> binary().
ensure_utf8(Binary) when is_binary(Binary) ->
    case is_valid_utf8(Binary) of
        true -> Binary;
        false -> <<"Invalid UTF-8"/utf8>>
    end;
ensure_utf8(String) when is_list(String) ->
    unicode:characters_to_binary(String, utf8).
```

## 相关文档

- **错误码规范**: [error-codes.md](./error-codes.md)
- **HashID 编码规范**: [hashid-encoding.md](./hashid-encoding.md)
- **API 格式规范**: [api-format.md](./api-format.md)
- **主文档**: [CLAUDE.md](../../CLAUDE.md)
