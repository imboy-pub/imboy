# 错误码使用规范

> Last Updated: 2026-03-08  
> Status: 长期接口规范文档  
> Scope: 错误码定义、引用方式与响应使用约定  
> Source of truth: `include/error_code.hrl`  
> Related docs: `doc/standards/api-format.md`, `doc/api/rest-api.md`, `doc/api/websocket-api-2.md`

## 概述

错误码定义在 `include/error_code.hrl` 中，使用时需引入头文件。

## 引入头文件

```erlang
-include("error_code.hrl").
```

## 基本用法

### 成功响应

```erlang
% 简单成功响应
elib_response:success(Req, Payload).

% 带消息的成功响应
elib_response:success(Req, Payload, <<"创建成功"/utf8>>).
```

### 错误响应

```erlang
% 使用宏定义的错误码
elib_response:error(Req, <<"用户不存在"/utf8>>, ?ERR_USER_NOT_FOUND).
elib_response:error(Req, <<"Token 已过期"/utf8>>, ?ERR_TOKEN_EXPIRED).
elib_response:error(Req, <<"请先登录"/utf8>>, ?ERR_UNAUTHORIZED).

% 使用辅助函数获取错误消息
elib_response:error(Req, error_msg(?ERR_USER_NOT_FOUND), ?ERR_USER_NOT_FOUND).
```

## 错误码分类

### 分类速查表

| 分类 | 范围 | 宏前缀 | 说明 | 示例 |
|------|------|--------|------|------|
| **成功** | 0 | `?ERR_OK` | 操作成功 | `?ERR_OK` = 0 |
| **客户端错误** | 400-499 | `?ERR_*` | 参数、认证、资源错误 | `?ERR_BAD_REQUEST`, `?ERR_UNAUTHORIZED`, `?ERR_NOT_FOUND` |
| **服务端错误** | 500-599 | `?ERR_*` | 服务器内部错误 | `?ERR_INTERNAL_SERVER_ERROR`, `?ERR_SERVICE_UNAVAILABLE` |
| **业务错误** | 900-999 | `?ERR_*` | IM 业务特定错误 | `?ERR_NOT_FRIENDS`, `?ERR_NOT_GROUP_MEMBER` |

### 成功码 (0)

```erlang
-define(ERR_OK, 0).
```

### 客户端错误 (400-499)

```erlang
% 通用客户端错误
-define(ERR_BAD_REQUEST, 400).        % 请求参数错误
-define(ERR_UNAUTHORIZED, 401).        % 未授权访问
-define(ERR_TOKEN_MISSING, 401).       % Token 缺失
-define(ERR_TOKEN_INVALID, 401).       % Token 无效
-define(ERR_TOKEN_EXPIRED, 401).       % Token 已过期
-define(ERR_FORBIDDEN, 403).           % 禁止访问
-define(ERR_NOT_FOUND, 404).           % 资源不存在
-define(ERR_METHOD_NOT_ALLOWED, 405).  % 方法不允许
-define(ERR_CONFLICT, 409).            % 资源冲突
```

### 服务端错误 (500-599)

```erlang
-define(ERR_INTERNAL_SERVER_ERROR, 500).     % 内部服务器错误
-define(ERR_NOT_IMPLEMENTED, 501).           % 未实现
-define(ERR_SERVICE_UNAVAILABLE, 503).       % 服务不可用
```

### 业务错误 (900-999)

```erlang
% 认证相关
-define(ERR_LOGIN_ELSEWHERE, 910).           % 在其他设备登录

% 好友相关
-define(ERR_NOT_FRIENDS, 920).               % 不是好友
-define(ERR_ALREADY_FRIENDS, 921).           % 已经是好友
-define(ERR_FRIEND_REQUEST_EXISTS, 922).     % 好友申请已存在

% 群组相关
-define(ERR_NOT_GROUP_MEMBER, 930).          % 非群组成员
-define(ERR_NOT_GROUP_ADMIN, 931).           % 非群管理员
-define(ERR_GROUP_NOT_FOUND, 932).           % 群组不存在
-define(ERR_ALREADY_GROUP_MEMBER, 933).      % 已是群成员

% 用户相关
-define(ERR_USER_NOT_FOUND, 940).            % 用户不存在
-define(ERR_USER_OFFLINE, 941).              % 用户离线
-define(ERR_USER_DISABLED, 942).             % 用户已禁用

% 消息相关
-define(ERR_MESSAGE_NOT_FOUND, 950).         % 消息不存在
-define(ERR_MESSAGE_SEND_FAILED, 951).       % 消息发送失败
```

## 常用错误码示例

### 认证相关

```erlang
% Token 缺失
-define(ERR_TOKEN_MISSING, 401).
elib_response:error(Req, error_msg(?ERR_TOKEN_MISSING), ?ERR_TOKEN_MISSING).

% Token 无效
-define(ERR_TOKEN_INVALID, 401).
elib_response:error(Req, error_msg(?ERR_TOKEN_INVALID), ?ERR_TOKEN_INVALID).

% Token 已过期
-define(ERR_TOKEN_EXPIRED, 401).
elib_response:error(Req, error_msg(?ERR_TOKEN_EXPIRED), ?ERR_TOKEN_EXPIRED).

% 在其他设备登录
-define(ERR_LOGIN_ELSEWHERE, 910).
elib_response:error(Req, error_msg(?ERR_LOGIN_ELSEWHERE), ?ERR_LOGIN_ELSEWHERE).
```

### 资源相关

```erlang
% 用户不存在
-define(ERR_USER_NOT_FOUND, 404).
elib_response:error(Req, error_msg(?ERR_USER_NOT_FOUND), ?ERR_USER_NOT_FOUND).

% 好友不存在
-define(ERR_FRIEND_NOT_FOUND, 404).
elib_response:error(Req, error_msg(?ERR_FRIEND_NOT_FOUND), ?ERR_FRIEND_NOT_FOUND).

% 群组不存在
-define(ERR_GROUP_NOT_FOUND, 404).
elib_response:error(Req, error_msg(?ERR_GROUP_NOT_FOUND), ?ERR_GROUP_NOT_FOUND).
```

### IM 业务相关

```erlang
% 不是好友
-define(ERR_NOT_FRIENDS, 920).
elib_response:error(Req, error_msg(?ERR_NOT_FRIENDS), ?ERR_NOT_FRIENDS).

% 非群组成员
-define(ERR_NOT_GROUP_MEMBER, 930).
elib_response:error(Req, error_msg(?ERR_NOT_GROUP_MEMBER), ?ERR_NOT_GROUP_MEMBER).

% 非群管理员
-define(ERR_NOT_GROUP_ADMIN, 931).
elib_response:error(Req, error_msg(?ERR_NOT_GROUP_ADMIN), ?ERR_NOT_GROUP_ADMIN).

% 用户离线
-define(ERR_USER_OFFLINE, 940).
elib_response:error(Req, error_msg(?ERR_USER_OFFLINE), ?ERR_USER_OFFLINE).
```

## 辅助函数

### 分类判断函数

```erlang
%% @doc 判断是否为客户端错误（4xx）
-spec is_client_error(integer()) -> boolean().
is_client_error(Code) when Code >= 400, Code < 500 -> true;
is_client_error(_) -> false.

%% @doc 判断是否为服务端错误（5xx）
-spec is_server_error(integer()) -> boolean().
is_server_error(Code) when Code >= 500, Code < 600 -> true;
is_server_error(_) -> false.

%% @doc 判断是否为业务错误（9xx）
-spec is_business_error(integer()) -> boolean().
is_business_error(Code) when Code >= 900, Code < 1000 -> true;
is_business_error(_) -> false.
```

### 错误消息函数

```erlang
%% @doc 根据错误码获取默认错误消息
-spec error_msg(integer()) -> binary().
error_msg(?ERR_OK) -> <<"操作成功"/utf8>>;
error_msg(?ERR_BAD_REQUEST) -> <<"参数错误"/utf8>>;
error_msg(?ERR_UNAUTHORIZED) -> <<"未授权访问"/utf8>>;
error_msg(?ERR_TOKEN_MISSING) -> <<"Token 缺失"/utf8>>;
error_msg(?ERR_TOKEN_INVALID) -> <<"Token 无效"/utf8>>;
error_msg(?ERR_TOKEN_EXPIRED) -> <<"Token 已过期"/utf8>>;
error_msg(?ERR_NOT_FOUND) -> <<"资源不存在"/utf8>>;
error_msg(?ERR_USER_NOT_FOUND) -> <<"用户不存在"/utf8>>;
error_msg(?ERR_NOT_FRIENDS) -> <<"不是好友关系"/utf8>>;
error_msg(?ERR_NOT_GROUP_MEMBER) -> <<"非群组成员"/utf8>>;
error_msg(?ERR_NOT_GROUP_ADMIN) -> <<"非群管理员"/utf8>>;
error_msg(?ERR_USER_OFFLINE) -> <<"用户离线"/utf8>>;
error_msg(_) -> <<"未知错误"/utf8>>.
```

## 禁止事项

### ❌ 禁止 1：硬编码错误码

```erlang
% ❌ 错误
elib_response:error(Req, "用户不存在", 404).
```

**修复**：
```erlang
% ✅ 正确
elib_response:error(Req, error_msg(?ERR_USER_NOT_FOUND), ?ERR_USER_NOT_FOUND).
```

### ❌ 禁止 2：使用魔法数字

```erlang
% ❌ 错误
case Code of
    401 -> handle_unauthorized();
    404 -> handle_not_found();
    _ -> handle_unknown()
end.
```

**修复**：
```erlang
% ✅ 正确
case Code of
    ?ERR_UNAUTHORIZED -> handle_unauthorized();
    ?ERR_NOT_FOUND -> handle_not_found();
    _ -> handle_unknown()
end.
```

### ❌ 禁止 3：自定义错误码

```erlang
% ❌ 错误：自定义未定义的错误码
-define(ERR_MY_CUSTOM_ERROR, 12345).
```

**修复**：
```erlang
% ✅ 正确：使用预定义的错误码范围
% 业务错误使用 900-999
-define(ERR_MY_CUSTOM_ERROR, 950).
```

## 最佳实践

### 1. 使用宏定义

```erlang
% ✅ 推荐
case user_repo:find(Uid) of
    {ok, User} -> {ok, User};
    {error, not_found} -> {error, ?ERR_USER_NOT_FOUND}
end.
```

### 2. 辅助函数获取消息

```erlang
% ✅ 推荐
{error, Code} = some_operation(),
elib_response:error(Req, error_msg(Code), Code).
```

### 3. 统一错误处理

```erlang
% ✅ 推荐：统一错误处理函数
handle_error(Req, Error) ->
    {Code, Msg} = case Error of
        {error, not_found} -> {?ERR_NOT_FOUND, error_msg(?ERR_NOT_FOUND)};
        {error, unauthorized} -> {?ERR_UNAUTHORIZED, error_msg(?ERR_UNAUTHORIZED)};
        {error, Reason} -> {?ERR_INTERNAL_SERVER_ERROR, <<"内部错误"/utf8>>}
    end,
    elib_response:error(Req, Msg, Code).
```

## 错误码设计原则

### 语义化

错误码应遵循 HTTP 状态码语义，便于理解：

- **4xx**: 客户端错误（参数错误、认证失败、资源不存在）
- **5xx**: 服务端错误（服务器内部错误）
- **9xx**: 业务特定错误（IM 业务专用）

### 扩展性

预留足够的错误码空间：

- **400-499**: 客户端错误（100 个）
- **500-599**: 服务端错误（100 个）
- **900-999**: 业务错误（100 个）

### 兼容性

- 不要修改已有错误码的含义
- 废弃的错误码保留，不重复使用
- 新增错误码添加到合适范围的末尾

## 相关文档

- **UTF-8 编码规范**: [utf8-encoding.md](./utf8-encoding.md)
- **HashID 编码规范**: [hashid-encoding.md](./hashid-encoding.md)
- **API 格式规范**: [api-format.md](./api-format.md)
- **类型规范**: [type-specification.md](./type-specification.md)
- **主文档**: [CLAUDE.md](../../CLAUDE.md)
