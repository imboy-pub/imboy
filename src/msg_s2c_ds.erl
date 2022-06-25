-module(msg_s2c_ds).
%%%
% msg_s2c_ds 是 msg_s2c domain service 缩写
%%%

-include_lib("imboy/include/chat.hrl").
-include_lib("imboy/include/common.hrl").

-export([write_msg/6]).
-export([revoke_offline_msg/4]).
-export([read_msg/3]).
-export([delete_msg/1]).


-spec write_msg(integer(),
                binary(),
                binary() | list(),
                integer(),
                integer(),
                integer()) -> any().
%% 存储消息
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) when is_list(Payload) ->
    write_msg(CreatedAt, Id, jsone:encode(Payload, [native_utf8]),
        From, To, ServerTS);
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) ->
    % 检查消息存储数量，如果数量大于limit 删除旧数据、插入新数据
    case msg_s2c_repo:count_by_to_id(To) of
        {ok, _, [[Count]]} when Count >= ?SAVE_MSG_LIMIT ->
            Limit = Count - ?SAVE_MSG_LIMIT + 1,
            msg_s2c_repo:delete_overflow_msg(To, Limit);
        _ ->
            ok
    end,
    msg_s2c_repo:write_msg(CreatedAt,
                           Id,
                           Payload,
                           From,
                           To,
                           ServerTS).


%% 读取消息
read_msg(ToUid, Limit, undefined) ->
    Column = <<"`id`, `payload`, `from_id`, `to_id`,
        `created_at`, `server_ts`, `msg_id`">>,
    Where = <<"WHERE `to_id` = ?">>,
    Vals = [ToUid],
    read_msg(Where, Vals, Column, Limit);
read_msg(ToUid, Limit, Ts) when is_binary(Ts) ->
    read_msg(ToUid, Limit, binary_to_integer(Ts));
read_msg(ToUid, Limit, Ts) ->
    Column = <<"`id`, `payload`, `from_id`, `to_id`,
        `created_at`, `server_ts`, `msg_id`">>,
    Where = <<"WHERE `to_id` = ? AND `created_at` > ?">>,
    Vals = [ToUid, Ts],
    read_msg(Where, Vals, Column, Limit).

delete_msg(Id) ->
    msg_s2c_repo:delete_msg(Id).


%% 撤销离线消息
-spec revoke_offline_msg(integer(), binary(), integer(), integer()) ->
          ok.
revoke_offline_msg(NowTs, Id, FromId, ToId) ->
    Payload = jsone:encode([{<<"msg_type">>, <<"custom">>},
                            {<<"custom_type">>, <<"revoked">>}]),
    % 存储消息
    msg_s2c_ds:write_msg(NowTs, Id, Payload, FromId, ToId, NowTs),
    Sql = <<"UPDATE `msg_s2c` SET `payload` = ? WHERE `msg_id` = ?">>,
    mysql_pool:query(Sql, [Payload, Id]),
    ok.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

read_msg(Where, Vals, Column, Limit) ->
    {ok, ColumnLi, Rows} = msg_s2c_repo:read_msg(
        Where,
        Vals,
        Column,
        Limit
    ),
    [lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnLi, Row) || Row <- Rows].
