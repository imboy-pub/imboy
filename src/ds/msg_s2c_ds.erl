-module(msg_s2c_ds).
%%%
% msg_s2c_ds 是 msg_s2c domain service 缩写
%%%

-include("chat.hrl").
-include("log.hrl").

-export([write_msg/6]).
-export([write_msg/8]).
-export([write_msg/9]).
-export([read_msg/2]).
-export([read_msg/3]).
-export([delete_msg/1]).
-export([send/7]).


%% @doc 发送服务端到客户端的消息（v2.0 格式，完整参数）
%%
%% 向指定用户列表发送消息，支持实时发送和先存储再发送两种模式
%% v2.0: S2C 消息使用 action 字段，与 assemble_msg/8 保持一致
%%
%% @param FromId 发送方用户ID（整数、二进制或列表）
%% @param ToUids 接收消息的用户ID列表
%% @param Action S2C 消息操作类型（binary）
%% @param MsgType 消息类型（S2C消息通常为空）
%% @param E2EE 端到端加密信息（S2C消息通常为空）
%% @param Payload 消息负载内容
%% @param Save 发送模式，save表示先存储再发送，其他表示直接发送
%% @returns ok 表示操作成功
-spec send(integer() | binary() | list(), list(), binary(), binary(), map() | null, map(), atom()) -> ok.
send(_, [], _, _, _, _, _) ->
    ok;
send(FromId, [ToUid | Tail], Action, MsgType, E2EE, Payload, Save) ->
    MsgId = elib_id:gen("s2c"),
    Msg = message_ds:assemble_msg(
        <<"S2C">>,
        elib_hashids:encode(FromId),
        elib_hashids:encode(ToUid),
        Payload,
        MsgId,
        MsgType,   % S2C 消息通常为空
        Action,    % S2C 消息的 action 字段
        E2EE       % S2C 消息通常为空
    ),
    EncodedMsg = jsone:encode(Msg, [native_utf8]),
    case Save of
        save ->
            CreatedAt = elib_dt:now(),
            % 只存储实际的 payload 数据，而不是整个消息对象
            % 避免 API 返回时出现 payload 嵌套问题
            PayloadJson = jsone:encode(Payload, [native_utf8]),
            _ = write_msg(CreatedAt, MsgId, PayloadJson, FromId, ToUid, CreatedAt, Action, MsgType, null),
            MsLi = [0, 1_000_000, 1_000_000],
            _ = message_ds:send_next(ToUid, MsgId, EncodedMsg, MsLi),
            ok;
        _ ->
            imboy_syn:publish(ToUid, EncodedMsg)
    end,
    send(FromId, Tail, Action, MsgType, E2EE, Payload, Save).


%% @doc 存储服务端到客户端的消息
%%
%% 将消息存储到数据库中，支持列表格式的自动JSON编码，如果存储的消息数量超过限制，会自动删除旧消息
%%
%% @param CreatedAt 消息创建时间戳
%% @param Id 消息ID
%% @param Payload 消息内容，可以是JSON格式的列表或二进制数据
%% @param From 发送方用户ID
%% @param To 接收方用户ID
%% @param ServerTS 服务器时间戳
%% @returns any() 数据库操作结果
-spec write_msg(binary(), binary(), binary() | map() | list(), integer(), integer(), binary()) -> any().
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) when is_map(Payload); is_list(Payload) ->
    write_msg(CreatedAt, Id, jsone:encode(Payload, [native_utf8]), From, To, ServerTS);
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) ->
    %% 从 Payload 中提取 action 和 msg_type 字段
    PayloadMap =
        try
            jsone:decode(Payload)
        of
            Map when is_map(Map) ->
                Map;
            _ ->
                #{}
        catch
            _:_ ->
                #{}
        end,

    %% S2C 消息必须有 action 字段
    Action = maps:get(<<"action">>, PayloadMap, <<>>),
    MsgType = maps:get(<<"msg_type">>, PayloadMap, <<>>),

    % 检查消息存储数量，如果数量大于limit 删除旧数据、插入新数据
    Count = msg_s2c_repo:count_by_to_id(To),
    case Count >= ?SAVE_MSG_LIMIT of
        true ->
            Limit = Count - ?SAVE_MSG_LIMIT + 1,
            elib_async:async_retry(fun() ->
                case msg_s2c_repo:delete_overflow_msg(To, Limit) of
                    {ok, _} -> ok;
                    {error, Reason} ->
                        ?ERROR_LOG([msg_s2c_delete_overflow_failed, To, Limit, Reason]),
                        {error, Reason}
                end
            end);
        false ->
            ok
    end,
    msg_s2c_repo:write_msg(CreatedAt, Id, Payload, From, To, ServerTS, Action, MsgType, null).


%% @doc 存储服务端到客户端的消息（8 参数版本，E2EE 默认为 null）
%%
%% @param CreatedAt 消息创建时间戳
%% @param Id 消息ID
%% @param Payload 消息内容
%% @param From 发送方用户ID
%% @param To 接收方用户ID
%% @param ServerTS 服务器时间戳
%% @param Action S2C消息指令
%% @param MsgType 消息类型
%% @returns any() 数据库操作结果
-spec write_msg(binary(), binary(), binary() | map() | list(), integer(), integer(), binary(), binary(), binary()) ->
          any().
write_msg(CreatedAt, Id, Payload, From, To, ServerTS, Action, MsgType) ->
    write_msg(CreatedAt, Id, Payload, From, To, ServerTS, Action, MsgType, null).


%% @doc 存储服务端到客户端的消息（v2.0 格式，支持 action 和 msg_type）
%%
%% 将消息存储到数据库中，包含 action 和 msg_type
%%
%% @param CreatedAt 消息创建时间戳
%% @param Id 消息ID
%% @param Payload 消息内容，可以是JSON格式的列表或二进制数据
%% @param From 发送方用户ID
%% @param To 接收方用户ID
%% @param ServerTS 服务器时间戳
%% @param Action S2C消息指令
%% @param MsgType 消息类型（可选）
%% @param E2EE 端到端加密信息（S2C 消息通常为 null）
%% @returns any() 数据库操作结果
-spec write_msg(binary(), binary(), binary() | map() | list(), integer(), integer(), binary(), binary(), binary(), binary() | null) ->
          any().
write_msg(CreatedAt, Id, Payload, From, To, ServerTS, Action, MsgType, E2EE) when is_map(Payload); is_list(Payload) ->
    write_msg(CreatedAt, Id, jsone:encode(Payload, [native_utf8]), From, To, ServerTS, Action, MsgType, E2EE);
write_msg(CreatedAt, Id, Payload, From, To, ServerTS, Action, MsgType, E2EE) ->
    % 检查消息存储数量，如果数量大于limit 删除旧数据、插入新数据
    Count = msg_s2c_repo:count_by_to_id(To),
    case Count >= ?SAVE_MSG_LIMIT of
        true ->
            Limit = Count - ?SAVE_MSG_LIMIT + 1,
            elib_async:async_retry(fun() ->
                case msg_s2c_repo:delete_overflow_msg(To, Limit) of
                    {ok, _} -> ok;
                    {error, Reason} ->
                        ?ERROR_LOG([msg_s2c_delete_overflow_failed, To, Limit, Reason]),
                        {error, Reason}
                end
            end);
        false ->
            ok
    end,
    msg_s2c_repo:write_msg(CreatedAt, Id, Payload, From, To, ServerTS, Action, MsgType, E2EE).


%% @doc 读取服务端到客户端的消息
%%
%% 从数据库中读取指定用户的消息，默认从最早的消息开始读取
%%
%% @param ToUid 接收方用户ID
%% @param Limit 读取消息数量限制
%% @returns list() 消息列表，每条消息包含完整信息
-spec read_msg(any(), integer()) -> [map()].
read_msg(ToUid, Limit) ->
    read_msg(ToUid, Limit, undefined).


%% @doc 读取服务端到客户端的消息（带时间戳参数）
%%
%% 从数据库中读取指定用户的未读消息，支持按时间戳过滤
%%
%% @param ToUid 接收方用户ID
%% @param Limit 读取消息数量限制
%% @param Ts 时间戳参数，undefined表示读取所有消息，整数或二进制表示指定时间之后的消息
%% @returns list() 消息列表，每条消息包含完整信息
-spec read_msg(any(), integer(), undefined | integer() | binary()) -> [map()].
read_msg(ToUid, Limit, undefined) ->
    Column = <<"id, payload, from_id, to_id,
        created_at, server_ts, msg_id, msg_type, action, e2ee">>,
    Where = <<"WHERE to_id = $1">>,
    Vals = [ToUid],
    read_msg(Where, Vals, Column, Limit);
read_msg(ToUid, Limit, Ts) ->
    % 使用 elib_dt:to_rfc3339/1 统一转换时间戳为 RFC3339 格式
    FixedTs = elib_dt:to_rfc3339(Ts),
    Column = <<"id, payload, from_id, to_id,
        created_at, server_ts, msg_id, msg_type, action, e2ee">>,
    Where = <<"WHERE to_id = $1 AND created_at >= $2">>,
    Vals = [ToUid, FixedTs],
    read_msg(Where, Vals, Column, Limit).


%% @doc 删除指定的服务端到客户端的消息
%%
%% 根据消息ID从数据库中删除消息
%%
%% @param Id 消息ID
%% @returns any() 数据库删除操作结果
-spec delete_msg(any()) -> any().
delete_msg(Id) ->
    msg_s2c_repo:delete_msg(Id).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 内部函数：根据查询条件过滤和读取消息
%%
%% 执行数据库查询并处理返回的消息数据，包括解码JSON格式的payload
%%
%% @param Where SQL查询条件
%% @param Vals SQL查询参数列表
%% @param Column 查询列名
%% @param Limit 查询结果数量限制
%% @returns list() 处理后的消息列表（map 格式）
-spec read_msg(binary(), list(), binary(), integer()) -> [map()].
read_msg(Where, Vals, Column, Limit) ->
    Res = msg_s2c_repo:read_msg(Where, Vals, Column, Limit),
    % ?DEBUG_LOG([Res]),
    case Res of
        {ok, Rows} ->
            [process_s2c_row(Row) || Row <- Rows];
        _ ->
            []
    end.

%% @doc 处理 S2C 消息行数据
%%
%% 1. 解码 payload 字段
%% 2. 用 msg_id 替换 id 字段（前端期望 id 是消息 ID，而不是数据库自增 ID）
%% 3. 处理旧数据的嵌套 payload 格式
%%
%% @param Row 数据库返回的行数据
%% @returns map() 处理后的消息数据
-spec process_s2c_row(map()) -> map().
process_s2c_row(Row) ->
    % 解码 payload 字段
    Row1 = elib_response:json_decode_field(Row, <<"payload">>),
    Payload = maps:get(<<"payload">>, Row1, #{}),

    % 用 msg_id 替换 id 字段
    MsgId = maps:get(<<"msg_id">>, Row1, <<>>),
    Row2 = Row1#{<<"id">> => MsgId},

    % 移除 msg_id 字段（已经替换到 id）
    Row3 = maps:remove(<<"msg_id">>, Row2),

    % 处理旧数据的嵌套 payload 格式
    % 旧数据: payload 是完整消息对象，包含嵌套的 payload 子字段
    % 新数据: payload 只包含实际数据
    case is_map(Payload) andalso maps:is_key(<<"payload">>, Payload) of
        true ->
            % 旧数据格式：提取嵌套的 payload
            ActualPayload = maps:get(<<"payload">>, Payload, #{}),
            Row3#{<<"payload">> => ActualPayload};
        false ->
            % 新数据格式：直接使用
            Row3
    end.
