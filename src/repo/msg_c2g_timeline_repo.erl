-module(msg_c2g_timeline_repo).
%%%
% msg_c2g_timeline_repo 是 msg_c2g_timeline repository 缩写
%%%

-include("log.hrl").

-export([tablename/0]).
-export([client_ack/2]).
-export([delete_timeline/2]).
-export([list_by_uid/2, list_by_uid/3]).
-export([check_msg/1]).
-export([count_by_to_id/1]).
-export([delete_by_to_uid/1]).
-export([delete_by_msg_id/1]).
-export([delete_overflow_timeline/2]).
-export([delete_by_msg_id_and_to_id/2]).
-export([delete_by_msg_ids_and_to_id/2]).

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_pg_sql:public_tablename(<<"msg_c2g_timeline">>).


% msg_c2g_timeline_repo:list_by_uid(2, <<"msg_id">>, 10).
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000000).


list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    % use index idx_c2g_timeline_ToUid_ClientAck
    Where = <<" WHERE to_uid = $1 AND client_ack = 0 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_pg:query(Sql, [Uid, Limit]).

% msg_c2g_timeline_repo:client_ack(109, <<"cor1aup1a20rgjtl5t8g">>).
client_ack(ToUid, MsgId) ->
    Tb = tablename(),
    % use index uk_c2g_timeline_ToUid_MsgId
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"UPDATE ", Tb/binary, " SET client_ack = 1 WHERE to_uid = $1 AND msg_id = $2">>,
    imboy_pg:execute(Sql, [ToUid, MsgId]).

% msg_c2g_timeline_repo:delete_timeline(6).
delete_timeline(ToUid, MsgId) ->
    Tb = tablename(),
    % use index uk_c2g_timeline_ToUid_MsgId
    Where = <<" WHERE to_uid = $1 AND  msg_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    imboy_pg:execute(Sql, [ToUid, MsgId]).


% msg_c2g_timeline_repo:check_msg(1).
check_msg(MsgId) ->
    % use index uk_c2g_timeline_MsgId
    % 使用安全的参数化查询，避免SQL注入
    Tb = tablename(),
    Sql = <<"SELECT count(*) as count FROM ", Tb/binary, " WHERE msg_id = $1">>,
    case imboy_pg:query(Sql, [MsgId]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.


% msg_c2g_timeline_repo:count_by_to_id(1).
count_by_to_id(ToUid) ->
    % use index uk_c2g_timeline_ToUid_MsgId
    % 使用安全的参数化查询，避免SQL注入
    imboy_pg:pluck_value(tablename(), <<"count(*)">>, #{to_uid => ToUid}, #{}, 0).

% msg_c2g_timeline_repo:delete_overflow_timeline(1, 100).
delete_overflow_timeline(ToUid, Limit) ->
    Tb = tablename(),
    % use index uk_c2g_timeline_ToUid_MsgId
    Where = <<" WHERE to_uid = $1 ORDER BY created_at ASC LIMIT $2">>,
    Sql = <<"SELECT msg_id FROM ", Tb/binary, Where/binary>>,
    % ?DEBUG_LOG(Sql),
    case imboy_pg:query(Sql, [ToUid, Limit]) of
        {ok, []} ->
            ok;
        {ok, Rows} ->
            [ delete_timeline(ToUid, MsgId) || #{<<"msg_id">> := MsgId} <- Rows ],
            {msg_ids, [ MsgId || #{<<"msg_id">> := MsgId} <- Rows ]}
    end.

% 删除用户的所有群消息时间线记录
delete_by_to_uid(ToUid) ->
    Tb = tablename(),
    Where = <<"WHERE to_uid = $1">>,
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    imboy_pg:execute(Sql, [ToUid]).

% 根据消息ID删除群消息时间线记录
delete_by_msg_id(MsgId) ->
    Tb = tablename(),
    Where = <<"WHERE msg_id = $1">>,
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary, " RETURNING to_uid">>,
    case imboy_pg:execute(Sql, [MsgId]) of
        {ok, _Count, _Rows} ->
            {ok, 1};
        {ok, _Count} ->
             {ok, 1};
        {error, Reason} ->
            {error, Reason}
    end.


% 根据消息ID和接收者ID删除特定群消息
delete_by_msg_id_and_to_id(MsgId, ToUid) ->
    Tb = tablename(),
    Where = <<"WHERE msg_id = $1 AND to_uid = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary, " RETURNING to_uid">>,
    case imboy_pg:execute(Sql, [MsgId, ToUid]) of
        {ok, _Count, _Rows} ->
            {ok, 1};
        {ok, _Count} ->
             {ok, 1};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 批量删除多个消息ID（使用安全的参数化查询）
%% @param MsgIds 消息ID列表
%% @param ToUid 接收者用户ID
%% @return {ok, Count} | {error, Reason}
-spec delete_by_msg_ids_and_to_id(list(binary()), integer()) -> {ok, integer()} | {error, any()}.
delete_by_msg_ids_and_to_id(MsgIds, ToUid) when is_list(MsgIds), length(MsgIds) > 0 ->
    Tb = tablename(),
    {InClause, InParams} = imboy_pg_sql:in(<<"msg_id">>, MsgIds),
    WhereClause = <<"to_uid = $1 AND ", InClause/binary>>,
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", WhereClause/binary>>,
    imboy_pg:execute(Sql, [ToUid | InParams]);
delete_by_msg_ids_and_to_id([], _ToUid) ->
    {ok, 0}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

