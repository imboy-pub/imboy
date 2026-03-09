# HashID 编码/解码规范

> Last Updated: 2026-03-08  
> Status: 长期接口规范文档  
> Scope: 用户、群组及相关资源 ID 的 HashID 编码 / 解码约束  
> Source of truth: `src/lib/elib_hashids.erl`  
> Related docs: `doc/api/rest-api.md`, `doc/api/channel_api_contract_v1.md`, `doc/api/moment_api_contract_v1.md`

## 核心原则

**所有涉及 `user_id` 和 `group_id` 的输入输出必须进行 HashID 编码/解码处理**

```
✅ 输入参数（客户端 → 服务端）：必须 decode
✅ 输出参数（服务端 → 客户端）：必须 encode
✅ 数据库操作：使用原始数字 ID
❌ 禁止：在 API 中直接暴露原始数字 ID
```

### 设计目标

1. **输入解码**：所有从客户端接收的 ID 字段必须使用 `elib_hashids:decode/1` 解码
2. **输出编码**：所有返回给客户端的 ID 字段必须使用 `elib_hashids:encode/1` 编码
3. **数据库使用原始 ID**：所有 Repo 层和 DS 层使用原始数字 ID，不存储 HashID

## 适用字段

### HTTP 请求参数

- `id`、`uid`、`user_id`
- `gid`、`group_id`
- `from`、`to`（消息发送方/接收方）
- `owner_uid`、`creator_uid`
- 任何包含 `uid`、`user_id`、`group_id` 的字段

### WebSocket 消息字段

- `from`、`to`（消息发送方/接收方）
- `uid`、`gid`
- 任何用户或群组 ID 字段

## 分层职责

### Handler 层（API 入口）

```erlang
% ✅ 正确：解码 Query String 参数
#{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req),
Uid2 = elib_hashids:decode(Uid),
User = user_logic:find_by_id(Uid2, Column),

% ✅ 正确：编码响应数据
elib_response:success(Req, elib_hashids:replace_id(User)).
```

### Logic 层（业务逻辑）

```erlang
% ✅ 正确：解码 POST/WS 参数
To = maps:get(<<"to">>, Data),
ToId = elib_hashids:decode(To),
% 使用 ToId（原始数字 ID）进行业务逻辑

% ✅ 正确：编码发送给客户端的消息
From = elib_hashids:encode(CurrentUid),
Msg = #{<<"from">> => From, <<"to">> => To, ...}.
```

### DS/Repo 层（数据访问）

```erlang
% ✅ 正确：使用原始数字 ID 查询数据库
user_repo:find_by_id(Uid, Column),  % Uid 是数字
friend_repo:is_friend(Uid, ToId),   % 都是数字
```

## 常用函数

### 1. 解码单个 ID

```erlang
% 将 HashID 解码为原始数字 ID
Uid2 = elib_hashids:decode(Uid),
Gid2 = elib_hashids:decode(Gid).

% 示例
% elib_hashids:decode(<<"XyZ9aBcDeF">>) => 12345
```

### 2. 编码单个 ID

```erlang
% 将原始数字 ID 编码为 HashID
From = elib_hashids:encode(CurrentUid),
Gid = elib_hashids:encode(Gid).

% 示例
% elib_hashids:encode(12345) => <<"XyZ9aBcDeF">>
```

### 3. 替换单个 ID 字段

```erlang
% 替换指定字段的 ID
User2 = elib_hashids:replace_id(User, <<"id">>).
```

### 4. 替换默认 "id" 字段

```erlang
% 替换 Map 中默认的 "id" 字段
User2 = elib_hashids:replace_id(User).
```

### 5. 链式替换多个 ID 字段

```erlang
% 链式替换多个 ID 字段
GData3 = elib_hashids:replace_id(
    elib_hashids:replace_id(
        elib_hashids:replace_id(G, <<"id">>),
    <<"owner_uid">>),
<<"creator_uid">>).
```

### 6. 批量解码列表

```erlang
% 批量解码 ID 列表
MemberUids2 = [elib_hashids:decode(Id) || Id <- MemberUids].
```

### 7. 批量编码列表

```erlang
% 批量编码 ID 列表
MemberUidsEncoded = [elib_hashids:encode(Id) || Id <- MemberUids2].
```

## 代码示例

### Handler 层示例

```erlang
% src/api/user_handler.erl
get_user(Req0, State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req0),
    Uid2 = elib_hashids:decode(Uid),  % 必须解码
    User = user_logic:find_by_id(Uid2, Column),
    Payload = #{
        <<"id">> => elib_hashids:encode(Uid2),  % 必须编码
        <<"name">> => maps:get(<<"name">>, User)
    },
    elib_response:success(Req0, Payload).
```

### Logic 层示例

```erlang
% src/logic/msg_c2c_logic.erl
send_message(CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    ToId = elib_hashids:decode(To),  % 必须解码
    % 使用 ToId 进行业务逻辑
    Msg = #{
        <<"from">> => elib_hashids:encode(CurrentUid),  % 必须编码
        <<"to">> => To,
        <<"payload">> => Payload
    },
    % 发送消息
    ok.
```

### 离线消息处理示例

```erlang
% src/api/msg_handler.erl
% 将数据库中的 from_id/to_id 替换为 from/to（编码后）
process_message(Msg) ->
    FromId = maps:get(<<"from_id">>, Msg, undefined),
    ToId = maps:get(<<"to_id">>, Msg, undefined),
    Msg3 = maps:remove(<<"from_id">>, Msg),
    Msg4 = maps:remove(<<"to_id">>, Msg3),
    Msg4#{<<"from">> => elib_hashids:encode(FromId),
         <<"to">> => elib_hashids:encode(ToId)}.
```

## 错误处理

### 解码失败处理

`elib_hashids:decode/1` 在解码失败时返回 `0`，需要检查：

```erlang
Uid2 = elib_hashids:decode(Uid),
case Uid2 of
    0 ->
        elib_response:error(Req, <<"无效的用户 ID"/utf8>>, ?ERR_BAD_REQUEST);
    _ ->
        % 继续处理
        User = user_logic:find_by_id(Uid2, Column),
        elib_response:success(Req, User)
end.
```

### 错误处理函数

```erlang
%% @doc 解码 ID，失败时返回错误
-spec decode_id(binary()) -> {ok, integer()} | {error, term()}.
decode_id(Id) ->
    case elib_hashids:decode(Id) of
        0 -> {error, invalid_id};
        DecodedId -> {ok, DecodedId}
    end.

%% @doc 解码 ID，失败时返回默认值
-spec decode_id(binary(), integer()) -> integer().
decode_id(Id, Default) ->
    case elib_hashids:decode(Id) of
        0 -> Default;
        DecodedId -> DecodedId
    end.
```

## 安全建议

### 1. 立即解码

在 Handler 层或 Logic 层入口立即解码，避免混淆：

```erlang
% ✅ 推荐：立即解码
handle_request(Req, State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req),
    Uid2 = elib_hashids:decode(Uid),  % 立即解码
    % 后续使用 Uid2
    process_with_uid(Uid2).

% ❌ 不推荐：延迟解码
handle_request(Req, State) ->
    #{id := Uid} = cowboy_req:match_qs([{id, [], undefined}], Req),
    % 传递 HashID，容易混淆
    process_with_hashid(Uid).
```

### 2. 统一命名

解码后的变量使用 `Id`、`Uid`、`Gid` 等后缀（原始数字 ID）：

```erlang
% ✅ 推荐
Uid2 = elib_hashids:decode(Uid),
ToId2 = elib_hashids:decode(To),
Gid2 = elib_hashids:decode(Gid).

% ❌ 不推荐（容易混淆）
UidDecoded = elib_hashids:decode(Uid),
DecodedToId = elib_hashids:decode(To).
```

### 3. 类型检查

使用 `when is_binary(Id)` 或 `when is_integer(Id)` 进行类型守卫：

```erlang
% ✅ 推荐
encode_id(Id) when is_integer(Id) ->
    elib_hashids:encode(Id).

% ✅ 推荐
decode_id(Id) when is_binary(Id) ->
    elib_hashids:decode(Id).
```

### 4. 错误处理

解码失败时返回 `0`，必须检查：

```erlang
% ✅ 推荐：始终检查解码结果
Uid2 = elib_hashids:decode(Uid),
case Uid2 of
    0 -> handle_invalid_id();
    _ -> handle_valid_id(Uid2)
end.

% ❌ 不推荐：不检查解码结果
Uid2 = elib_hashids:decode(Uid),
% 直接使用，可能导致错误
process(Uid2).
```

### 5. 禁止数据库存储 HashID

数据库中只存储原始数字 ID：

```erlang
% ✅ 推荐：数据库使用原始 ID
insert_user(Uid) when is_integer(Uid) ->
    elib_pg:insert(<<"users">>, #{<<"id">> => Uid}).

% ❌ 不推荐：数据库存储 HashID
insert_user(HashId) when is_binary(HashId) ->
    elib_pg:insert(<<"users">>, #{<<"id">> => HashId}).
```

## 测试验证

### 关键测试点

- ✅ Query String 参数正确解码
- ✅ POST 参数正确解码
- ✅ WebSocket 消息参数正确解码
- ✅ 响应数据正确编码
- ✅ 离线消息正确编码
- ✅ 批量操作正确解码/编码
- ✅ 解码失败时正确处理（返回 `0`）

### 测试示例

```erlang
encode_decode_test() ->
    % 测试编码和解码
    OriginalId = 12345,
    HashId = elib_hashids:encode(OriginalId),
    DecodedId = elib_hashids:decode(HashId),
    ?assertEqual(OriginalId, DecodedId).

batch_decode_test() ->
    % 测试批量解码
    Ids = [<<"XyZ9">>, <<"AbCd1">>, <<"EfGh2">>],
    DecodedIds = [elib_hashids:decode(Id) || Id <- Ids],
    ?assertEqual(3, length(DecodedIds)),
    lists:foreach(fun(Id) -> ?assert(Id > 0) end, DecodedIds).

invalid_id_test() ->
    % 测试无效 ID 解码
    InvalidId = <<"InvalidHashId">>,
    DecodedId = elib_hashids:decode(InvalidId),
    ?assertEqual(0, DecodedId).
```

## 常见错误

### ❌ 错误 1：忘记解码

```erlang
% 错误：直接使用 HashID
Uid = cowboy_req:binding(uid, Req),
User = user_repo:find_by_id(Uid, Column).  % Uid 是 HashID，不是数字
```

**修复**：
```erlang
% 正确：先解码再使用
Uid = cowboy_req:binding(uid, Req),
Uid2 = elib_hashids:decode(Uid),
User = user_repo:find_by_id(Uid2, Column).
```

### ❌ 错误 2：忘记编码

```erlang
% 错误：直接返回原始 ID
User = user_repo:find_by_id(Uid2, Column),
Payload = #{<<"id">> => Uid2, ...}.  % Uid2 是数字，应该编码
elib_response:success(Req, Payload).
```

**修复**：
```erlang
% 正确：编码后再返回
User = user_repo:find_by_id(Uid2, Column),
Payload = #{<<"id">> => elib_hashids:encode(Uid2), ...},
elib_response:success(Req, Payload).
```

### ❌ 错误 3：数据库使用 HashID

```erlang
% 错误：数据库存储 HashID
HashId = elib_hashids:encode(Uid),
elib_pg:insert(<<"users">>, #{<<"id">> => HashId}).  % 应该存储数字 ID
```

**修复**：
```erlang
% 正确：数据库存储原始 ID
elib_pg:insert(<<"users">>, #{<<"id">> => Uid}).
```

## 相关文档

- **UTF-8 编码规范**: [utf8-encoding.md](./utf8-encoding.md)
- **错误码规范**: [error-codes.md](./error-codes.md)
- **API 格式规范**: [api-format.md](./api-format.md)
- **类型规范**: [type-specification.md](./type-specification.md)
- **主文档**: [CLAUDE.md](../../CLAUDE.md)
