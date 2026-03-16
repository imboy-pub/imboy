-module(channel_message_repo).
%%%
% channel_message_repo 是 channel_message repository 缩写
% 频道消息数据仓库层
%%%
%% Internal persistence detail for the channel_content domain.

-export([tablename/0]).
-export([add/1]).
-export([add/2]).
-export([find_by_id/1]).
-export([list_by_channel/3]).
-export([list_pinned/1]).
-export([update/2]).
-export([delete/1]).
-export([revoke/3]).
-export([increment_view_count/1]).
-export([delete_by_channel/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取频道消息表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"channel_message">>).

%% @doc 添加频道消息
-spec add(map()) -> {ok, term(), term()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>)).

%% @doc 添加频道消息（使用连接）
-spec add(any(), map()) -> {ok, term(), term()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:insert(Tb, Data, <<"RETURNING id">>),
    elib_pg:execute(Conn, Sql, Params).

%% @doc 根据ID查找消息
-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(MessageId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 AND status = 1 LIMIT 1">>,
    case elib_pg:one(Sql, [MessageId]) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 分页获取频道消息
%% @param ChannelId 频道ID
%% @param Cursor 游标（上一页最后一条消息的时间戳），0 表示获取最新
%% @param Limit 每页数量
-spec list_by_channel(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId, 0, Limit) ->
    % 获取最新消息
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE channel_id = $1 AND status = 1 "
            "ORDER BY created_at DESC LIMIT $2">>,
    elib_pg:query(Sql, [ChannelId, Limit]);
list_by_channel(ChannelId, Cursor, Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE channel_id = $1 AND status = 1 AND created_at < $2 "
            "ORDER BY created_at DESC LIMIT $3">>,
    CursorTs = elib_dt:to_rfc3339(Cursor),
    elib_pg:query(Sql, [ChannelId, CursorTs, Limit]).

%% @doc 获取频道置顶消息
-spec list_pinned(integer()) -> {ok, list(map())} | {error, any()}.
list_pinned(ChannelId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE channel_id = $1 AND status = 1 AND is_pinned = true "
            "ORDER BY created_at DESC">>,
    elib_pg:query(Sql, [ChannelId]).

%% @doc 更新消息
-spec update(integer(), map()) -> {ok, non_neg_integer()} | {error, any()}.
update(MessageId, Data) ->
    Tb = tablename(),
    UpdateData = maps:without([<<"id">>], Data),
    elib_pg:update(Tb, UpdateData, <<"id = $1">>, [MessageId]).

%% @doc 删除消息（软删除）
-spec delete(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(MessageId) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => -1}, <<"id = $1">>, [MessageId]).

%% @doc 撤回消息（幂等）
%% @param MessageId 消息ID
%% @param RevokedBy 撤回人ID
%% @param RevokedAt 撤回时间（RFC3339）
-spec revoke(integer(), integer(), binary()) -> {ok, non_neg_integer()} | {error, any()}.
revoke(MessageId, RevokedBy, RevokedAt) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET revoked = true, revoked_by = $2, revoked_at = $3, updated_at = $3 "
            "WHERE id = $1 AND status = 1 AND revoked = false">>,
    elib_pg:execute(Sql, [MessageId, RevokedBy, RevokedAt]).

%% @doc 增加阅读量
-spec increment_view_count(integer()) -> {ok, non_neg_integer()} | {error, any()}.
increment_view_count(MessageId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET view_count = view_count + 1 WHERE id = $1">>,
    elib_pg:execute(Sql, [MessageId]).

%% @doc 删除频道的所有消息
-spec delete_by_channel(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_channel(ChannelId) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => -1}, <<"channel_id = $1">>, [ChannelId]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
