-module(msg_s2c_ds).
%%%
% msg_s2c_ds 是 msg_s2c domain service 缩写
%%%

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").

-export([write_msg/6]).
-export([read_msg/2]).
-export([read_msg/3]).
-export([delete_msg/1]).
-export([send/4]).



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


-spec write_msg(binary(), binary(), binary() | list(), integer(), integer(), binary()) -> any().
%% 存储消息
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


%% 读取消息
read_msg(ToUid, Limit) ->
    read_msg(ToUid, Limit, undefined).


read_msg(ToUid, Limit, undefined) ->
    P = imboy_hasher:decoded_payload(),
    Column = <<"id, ", P/binary, ", from_id, to_id,
        created_at, server_ts, msg_id">>,
    Where = <<"WHERE to_id = $1">>,
    Vals = [ToUid],
    read_msg(Where, Vals, Column, Limit);
read_msg(ToUid, Limit, Ts) when is_binary(Ts) ->
    read_msg(ToUid, Limit, binary_to_integer(Ts));
read_msg(ToUid, Limit, Ts) ->
    P = imboy_hasher:decoded_payload(),
    Column = <<"id, ", P/binary, ", from_id, to_id,
        created_at, server_ts, msg_id">>,
    Where = <<"WHERE to_id = $1 AND created_at > $2">>,
    Vals = [ToUid, Ts],
    read_msg(Where, Vals, Column, Limit).


delete_msg(Id) ->
    msg_s2c_repo:delete_msg(Id).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


read_msg(Where, Vals, Column, Limit) ->
    Res = msg_s2c_repo:read_msg(Where, Vals, Column, Limit),
    % ?DEBUG_LOG([Res]),
    case Res of
        {ok, Column2, Rows} ->
            [ lists:zipwith(fun(X, Y) -> {X, Y} end, Column2, tuple_to_list(Row)) || Row <- Rows ];
        _ ->
            []
    end.
