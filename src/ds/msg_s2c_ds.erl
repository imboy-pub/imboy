-module(msg_s2c_ds).
%%%
% msg_s2c_ds 是 msg_s2c domain service 缩写
%%%

-include("chat.hrl").
-include("log.hrl").

-export([write_msg/6]).
-export([read_msg/2]).
-export([read_msg/3]).
-export([delete_msg/1]).
-export([send/4]).



%% @doc 发送服务端到客户端的消息
%%
%% 向指定用户列表发送消息，支持实时发送和先存储再发送两种模式
%%
%% @param FromId 发送方用户ID
%% @param MsgType 消息类型，可以是二进制或映射格式
%% @param ToUids 接收消息的用户ID列表
%% @param Save 发送模式，save表示先存储再发送，其他表示直接发送
%% @returns ok 表示操作成功
-spec send(any(), [binary()|map()], list(), atom()) -> ok.
send(_, _, [], _) ->
    ok;
% 给在线好友发送上线消息
send(FromId, MsgType, [ToUid | Tail], Save) ->
    Payload0 = if
        is_binary(MsgType) ->
            [{<<"msg_type">>, MsgType}];
        is_map(MsgType) ->
            MsgType
    end,
    MsgId = imboy_func:uid("s2c"),
    Payload = message_ds:assemble_msg(<<"S2C">>,
       imboy_hashids:encode(FromId),
       imboy_hashids:encode(ToUid),
       Payload0 ,
       MsgId),
    Msg = jsone:encode(Payload, [native_utf8]),
    case Save of
        save ->
            CreatedAt = imboy_dt:now(),
            write_msg(CreatedAt, MsgId, Payload, FromId, ToUid, CreatedAt),

            MsLi = [0, 1_000_000, 1_000_000],
            message_ds:send_next(ToUid, MsgId, Msg, MsLi),
            ok;
        _ ->
            imboy_syn:publish(ToUid, Msg)
    end,
    send(FromId, MsgType, Tail, Save).

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
-spec write_msg(binary(), binary(), binary() | list(), integer(), integer(), binary()) -> any().
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) when is_list(Payload) ->
    write_msg(CreatedAt, Id, jsone:encode(Payload, [native_utf8]), From, To, ServerTS);
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) ->
    % 检查消息存储数量，如果数量大于limit 删除旧数据、插入新数据
    case msg_s2c_repo:count_by_to_id(To) of
        Count when Count >= ?SAVE_MSG_LIMIT ->
            Limit = Count - ?SAVE_MSG_LIMIT + 1,
            msg_s2c_repo:delete_overflow_msg(To, Limit);
        _ ->
            ok
    end,
    msg_s2c_repo:write_msg(CreatedAt, Id, Payload, From, To, ServerTS).

%% @doc 读取服务端到客户端的消息
%%
%% 从数据库中读取指定用户的消息，默认从最早的消息开始读取
%%
%% @param ToUid 接收方用户ID
%% @param Limit 读取消息数量限制
%% @returns list() 消息列表，每条消息包含完整信息
-spec read_msg(any(), integer()) -> list().
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
-spec read_msg(any(), integer(), undefined | integer() | binary()) -> list().
read_msg(ToUid, Limit, undefined) ->
    P = imboy_hasher:decoded_payload(),
    Column = <<"id, ", P/binary, ", from_id, to_id,
        created_at, server_ts, msg_id">>,
    Where = <<"WHERE to_id = $1">>,
    Vals = [ToUid],
    read_msg(Where, Vals, Column, Limit);
read_msg(ToUid, Limit, Ts) ->
    % 使用 imboy_dt:to_rfc3339/1 统一转换时间戳为 RFC3339 格式
    FixedTs = imboy_dt:to_rfc3339(Ts),
    P = imboy_hasher:decoded_payload(),
    Column = <<"id, ", P/binary, ", from_id, to_id,
        created_at, server_ts, msg_id">>,
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
%% @returns list() 处理后的消息列表（proplist 格式）
-spec read_msg(binary(), list(), binary(), integer()) -> list().
read_msg(Where, Vals, Column, Limit) ->
    Res = msg_s2c_repo:read_msg(Where, Vals, Column, Limit),
    % ?DEBUG_LOG([Res]),
    case Res of
        {ok, Rows} ->
            % 将 map 格式的数据库行转换为 proplist，供 message_ds:sent_offline_msg/3 使用
            [maps_to_proplist(imboy_response:json_decode_field(Row, <<"payload">>)) || Row <- Rows];
        _ ->
            []
    end.


%% @doc 将 map 转换为 proplist
%%
%% 将数据库返回的 map 格式转换为 proplist 格式，供 message_ds:sent_offline_msg/3 使用
%% 这样 is_map(Row) 会返回 false，走 C2C/S2C 分支而不是 C2G 分支
%%
%% @param Map 输入的 map
%% @return proplist 格式的数据
-spec maps_to_proplist(map()) -> proplists:proplist().
maps_to_proplist(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Map).
