-module(channel_subscription_repo).
%%%
% channel_subscription_repo 是 channel_subscription repository 缩写
% 频道订阅关系数据仓库层
%%%

-export([tablename/0]).
-export([add/1]).
-export([add/2]).
-export([find/2]).
-export([list_by_uid/1]).
-export([list_by_channel/1]).
-export([list_by_channel/3]).
-export([delete/2]).
-export([update/3]).
-export([increment_unread/1]).
-export([clear_unread/1]).
-export([count_unread/1]).
-export([count_unread_channels/1]).
-export([is_subscribed/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取频道订阅表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"channel_subscription">>).

%% @doc 添加订阅关系
-spec add(map()) -> {ok, term()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    elib_pg:insert(Tb, Data).

%% @doc 添加订阅关系（使用连接）
-spec add(any(), map()) -> {ok, term(), term()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:insert(Tb, Data, <<"RETURNING id">>),
    elib_pg:execute(Conn, Sql, Params).

%% @doc 查找订阅关系
%% @param ChannelId 频道ID
%% @param Uid 用户ID
%% @return map() | #{}
-spec find(integer(), integer()) -> map().
find(ChannelId, Uid) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE channel_id = $1 AND user_id = $2 AND status = 1 LIMIT 1">>,
    case elib_pg:one(Sql, [ChannelId, Uid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 查询用户的所有订阅
-spec list_by_uid(integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary,
            " WHERE user_id = $1 AND status = 1 "
            "ORDER BY is_pinned DESC, subscribed_at DESC">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 查询频道的所有订阅者
-spec list_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId) ->
    Tb = tablename(),
    Sql = <<"SELECT user_id FROM ", Tb/binary,
            " WHERE channel_id = $1 AND status = 1">>,
    elib_pg:query(Sql, [ChannelId]).

%% @doc 分页查询频道的订阅者列表
%% @param ChannelId 频道ID
%% @param Cursor 游标（上一页最后一条记录的ID），0 表示从头开始
%% @param Limit 每页数量
-spec list_by_channel(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId, 0, Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT id, user_id, is_pinned, subscribed_at, last_read_at, unread_count "
            "FROM ", Tb/binary,
            " WHERE channel_id = $1 AND status = 1 "
            "ORDER BY is_pinned DESC, subscribed_at DESC LIMIT $2">>,
    elib_pg:query(Sql, [ChannelId, Limit]);
list_by_channel(ChannelId, Cursor, Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT id, user_id, is_pinned, subscribed_at, last_read_at, unread_count "
            "FROM ", Tb/binary,
            " WHERE channel_id = $1 AND status = 1 AND id < $2 "
            "ORDER BY is_pinned DESC, subscribed_at DESC LIMIT $3">>,
    elib_pg:query(Sql, [ChannelId, Cursor, Limit]).

%% @doc 删除订阅关系（软删除）
-spec delete(integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(ChannelId, Uid) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => 0}, <<"channel_id = $1 AND user_id = $2">>, [ChannelId, Uid]).

%% @doc 更新订阅信息
-spec update(integer(), integer(), map()) -> {ok, non_neg_integer()} | {error, any()}.
update(ChannelId, Uid, Data) ->
    Tb = tablename(),
    elib_pg:update(Tb, Data, <<"channel_id = $1 AND user_id = $2">>, [ChannelId, Uid]).

%% @doc 增加未读计数
-spec increment_unread(integer()) -> ok.
increment_unread(ChannelId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET unread_count = unread_count + 1 "
            "WHERE channel_id = $1 AND status = 1">>,
    _ = elib_pg:execute(Sql, [ChannelId]),
    ok.

%% @doc 清除未读计数
-spec clear_unread(integer()) -> {ok, non_neg_integer()} | {error, any()}.
clear_unread(ChannelId) ->
    Tb = tablename(),
    elib_pg:update(Tb,
        #{unread_count => 0, last_read_at => elib_dt:now()},
        <<"channel_id = $1 AND status = 1">>,
        [ChannelId]).

%% @doc 获取用户在所有频道的未读总数
-spec count_unread(integer()) -> non_neg_integer().
count_unread(Uid) ->
    Tb = tablename(),
    Sql = <<"SELECT COALESCE(SUM(unread_count), 0) as total FROM ", Tb/binary,
            " WHERE user_id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, #{<<"total">> := Total}} -> Total;
        _ -> 0
    end.

%% @doc 获取有未读消息的频道数量
-spec count_unread_channels(integer()) -> non_neg_integer().
count_unread_channels(Uid) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary,
            " WHERE user_id = $1 AND status = 1 AND unread_count > 0">>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 检查用户是否订阅了频道
-spec is_subscribed(integer(), integer()) -> boolean().
is_subscribed(ChannelId, Uid) ->
    Tb = tablename(),
    Sql = <<"SELECT 1 FROM ", Tb/binary,
            " WHERE channel_id = $1 AND user_id = $2 AND status = 1 LIMIT 1">>,
    case elib_pg:one(Sql, [ChannelId, Uid]) of
        {ok, _} -> true;
        _ -> false
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
