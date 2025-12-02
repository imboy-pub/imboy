-module(msg_s2c_repo).
%%%
% msg_s2c_repo 是 msg_s2c repository 缩写
% 系统到用户消息数据仓库层，提供S2C消息的基础数据库操作
%%%

-include_lib("imlib/include/log.hrl").

-export([tablename/0]).
-export([read_msg/4]).
-export([write_msg/6]).
-export([delete_msg/1]).
-export([delete_msg/2]).
-export([count_by_to_id/1]).
-export([delete_by_to_id/1]).
-export([delete_by_msg_id_and_to_id/2]).
-export([delete_by_msg_ids_and_to_id/2]).
-export([delete_overflow_msg/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取S2C消息表的表名
%% @return 返回S2C消息表的完整表名
-spec tablename() -> binary().
tablename() ->
    imboy_db:public_tablename(<<"msg_s2c">>).


read_msg(Where, Vals, Column, Limit) ->
    Tb = tablename(),
    LimitIndex = integer_to_binary(length(Vals) + 1),
    % use index i_ToId
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary, " ORDER BY id ASC LIMIT $",
            LimitIndex/binary>>,
    % ?DEBUG_LOG(Sql),
    imboy_db:query(Sql, Vals ++ [Limit]).


write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(CreatedAt, Id, Payload, FromId2, ToId, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) when is_integer(ToId) ->
    ToId2 = list_to_binary(integer_to_list(ToId)),
    write_msg(CreatedAt, Id, Payload, FromId, ToId2, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) ->
    imboy_db:insert_into(tablename(), #{
        payload => {raw, imboy_hasher:encoded_val(Payload)},
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTS,
        msg_id => Id
    }).

delete_msg(Id) when is_integer(Id) ->
    Where = <<"WHERE id = $1">>,
    delete_msg(Where, Id);
delete_msg(Id) ->
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, Id).


delete_msg(Where, Val) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    imboy_db:execute(Sql, [Val]).


% msg_s2c_repo:count_by_to_id(1).
count_by_to_id(ToUid) ->
    ToUid2 = integer_to_binary(ToUid),
    % use index i_ToId
    imboy_db:pluck(tablename(), <<"to_id = ", ToUid2/binary>>, <<"count(*) as count">>, 0).


delete_overflow_msg(ToUid, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE to_id = $1 ORDER BY id ASC LIMIT $2">>,
    Sql = <<"SELECT id FROM ", Tb/binary, Where/binary>>,
    case imboy_db:query(Sql, [ToUid, Limit]) of
        {ok, _, []} ->
            ok;
        {ok, _, Rows} ->
            [ delete_msg(Id) || {Id} <- Rows ],
            ok
    end.

% 删除用户的所有系统消息
delete_by_to_id(ToUid) ->
    Where = <<"WHERE to_id = $1">>,
    delete_msg(Where, ToUid).

% 根据消息ID和接收者ID删除特定系统消息
delete_by_msg_id_and_to_id(MsgId, ToUid) ->
    Where = <<"WHERE msg_id = $1 AND to_id = $2">>,
    delete_msg(Where, [MsgId, ToUid]).

% 批量删除多个消息ID（使用 IN 语句的单个 SQL）
delete_by_msg_ids_and_to_id(MsgIds, ToUid) when is_list(MsgIds), length(MsgIds) > 0 ->
    % 构建占位符字符串 ($1, $2, $3, ...)
    Placeholders = lists:join(<<",">>, [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(MsgIds))]),
    Where = <<"WHERE msg_id IN (", Placeholders/binary, ") AND to_id = $", (integer_to_binary(length(MsgIds) + 1))/binary>>,
    delete_msg(Where, MsgIds ++ [ToUid]);
delete_by_msg_ids_and_to_id([], _ToUid) ->
    {ok, 0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
