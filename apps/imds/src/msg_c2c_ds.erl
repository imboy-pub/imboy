-module(msg_c2c_ds).
%%%
% msg_c2c_ds 是 msg_c2c domain service 缩写
%%%

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").

-export([write_msg/6]).
-export([revoke_offline_msg/4]).
-export([read_msg/2]).
-export([read_msg/3]).
-export([delete_msg/1]).

%% ===================================================================
%% API
%% ===================================================================


-spec write_msg(integer(), binary(), binary() | list(), integer(), integer(), integer()) -> any().
%% 存储消息
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) when is_list(Payload) ->
    write_msg(CreatedAt, Id, jsone:encode(Payload, [native_utf8]), From, To, ServerTS);
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) ->
    % 检查消息存储数量，如果数量大于limit 删除旧数据、插入新数据
    case msg_c2c_repo:count_by_to_id(To) of
        Count when Count >= ?SAVE_MSG_LIMIT ->
            Limit = Count - ?SAVE_MSG_LIMIT + 1,
            msg_c2c_repo:delete_overflow_msg(To, Limit);
        _ ->
            ok
    end,
    msg_c2c_repo:write_msg(CreatedAt, Id, Payload, From, To, ServerTS).


%% 读取消息
read_msg(ToUid, Limit) ->
    read_msg(ToUid, Limit, undefined).

read_msg(ToUid, Limit, undefined) ->
    Where = <<"WHERE to_id = ", (ec_cnv:to_binary(ToUid))/binary>>,
    read_msg_filter(Where, Limit);
read_msg(ToUid, Limit, Ts) ->
    Where = <<"WHERE to_id = " , (ec_cnv:to_binary(ToUid))/binary
        , " AND created_at > ", (ec_cnv:to_binary(Ts))/binary>>,
    read_msg_filter(Where, Limit).


delete_msg(Id) ->
    msg_c2c_repo:delete_msg(Id),
    ok.


%% 撤销离线消息
-spec revoke_offline_msg(integer(), binary(), integer(), integer()) -> ok.
revoke_offline_msg(NowTs, Id, FromId, ToId) ->
    Payload = jsone:encode([{<<"msg_type">>, <<"custom">>}, {<<"custom_type">>, <<"peer_revoked">>}]),
    % 存储消息
    msg_c2c_ds:write_msg(NowTs, Id, Payload, FromId, ToId, NowTs),
    Tb = msg_c2c_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET payload = $1 WHERE msg_id = $2">>,
    imboy_db:execute(Sql, [Payload, Id]),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


read_msg_filter(Where, Limit) ->
    P = imboy_hasher:decoded_payload(),
    Column = <<"id, ", P/binary, ", from_id, to_id, created_at, server_ts, msg_id">>,
    Res = msg_c2c_repo:read_msg(Where, Column, Limit),
    % ?LOG([Res]),
    case Res of
        {ok, ColumnLi, Rows} ->
            [ lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnLi, tuple_to_list(Row)) || Row <- Rows ];
        _ ->
            []
    end.
