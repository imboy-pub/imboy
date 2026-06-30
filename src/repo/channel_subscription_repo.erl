-module(channel_subscription_repo).
%%%
% channel_subscription_repo 是 channel_subscription repository 缩写
% 频道订阅关系数据仓库层
%%%
%% Internal persistence detail for the channel_content domain.

-export([tablename/0]).
-export([upsert_active/3]).
-export([find/2]).
-export([list_by_uid/1]).
-export([list_by_channel/1]).
-export([list_by_channel/3]).
-export([delete/2]).
-export([delete/3]).
-export([increment_unread/1]).
-export([clear_unread/1]).
-export([clear_unread/2]).
-export([get_unread_count/2]).
-export([count_unread/1]).
-export([count_unread_channels/1]).
-export([list_unread_by_uid/1]).
-export([list_unread_counts_by_channel/1]).
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

%% @doc 幂等激活订阅关系（插入或将 status 恢复为 1）
%% @param Conn 事务连接
%% @param ChannelId 频道ID
%% @param Uid 用户ID
%% @return {ok, true} 表示状态真实变化（新插入或从0恢复到1）
%%         {ok, false} 表示原本已是订阅状态（幂等）
%%         {error, Reason}
-spec upsert_active(any(), integer(), integer()) -> {ok, boolean()} | {error, term()}.
upsert_active(Conn, ChannelId, Uid) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    GenId = elib_tsid:generate(channel_subscription),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, channel_id, user_id, subscribed_at, status) "
            "VALUES ($1, $2, $3, $4, 1) "
            "ON CONFLICT (channel_id, user_id) DO UPDATE "
            "SET status = 1, subscribed_at = EXCLUDED.subscribed_at "
            "WHERE ", Tb/binary,
            ".status <> 1 "
            "RETURNING 1 AS changed">>,
    case elib_pg:query(Conn, Sql, [GenId, ChannelId, Uid, Now]) of
        {ok, []} ->
            {ok, false};
        {ok, [_ | _]} ->
            {ok, true};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查找订阅关系
%% @param ChannelId 频道ID
%% @param Uid 用户ID
%% @return map() | #{}
-spec find(integer(), integer()) -> map().
find(ChannelId, Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT * FROM ", Tb/binary,
            " WHERE channel_id = $1 AND user_id = $2 AND status = 1 LIMIT 1">>,
    case elib_pg:one(Sql, [ChannelId, Uid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 查询用户的所有订阅
-spec list_by_uid(integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT * FROM ", Tb/binary,
            " WHERE user_id = $1 AND status = 1 "
            "ORDER BY is_pinned DESC, subscribed_at DESC">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 查询频道的所有订阅者
-spec list_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId) ->
    Tb = tablename(),
    Sql = <<"SELECT user_id FROM ", Tb/binary, " WHERE channel_id = $1 AND status = 1">>,
    elib_pg:query(Sql, [ChannelId]).

%% @doc 分页查询频道的订阅者列表
%% @param ChannelId 频道ID
%% @param Cursor 游标（上一页最后一条记录的ID），0 表示从头开始
%% @param Limit 每页数量
-spec list_by_channel(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId, 0, Limit) ->
    Tb = tablename(),
    UTb = user_repo:tablename(),
    Sql = <<
        "SELECT cs.id, cs.user_id, cs.is_pinned, cs.subscribed_at, cs.last_read_at, "
        "cs.unread_count, cs.is_muted, u.nickname, u.avatar "
        "FROM ",
        Tb/binary,
        " cs LEFT JOIN ",
        UTb/binary,
        " u ON u.id = cs.user_id "
        "WHERE cs.channel_id = $1 AND cs.status = 1 "
        "ORDER BY cs.is_pinned DESC, cs.subscribed_at DESC LIMIT $2"
    >>,
    elib_pg:query(Sql, [ChannelId, Limit]);
list_by_channel(ChannelId, Cursor, Limit) ->
    Tb = tablename(),
    UTb = user_repo:tablename(),
    Sql = <<
        "SELECT cs.id, cs.user_id, cs.is_pinned, cs.subscribed_at, cs.last_read_at, "
        "cs.unread_count, cs.is_muted, u.nickname, u.avatar "
        "FROM ",
        Tb/binary,
        " cs LEFT JOIN ",
        UTb/binary,
        " u ON u.id = cs.user_id "
        "WHERE cs.channel_id = $1 AND cs.status = 1 AND cs.id < $2 "
        "ORDER BY cs.is_pinned DESC, cs.subscribed_at DESC LIMIT $3"
    >>,
    elib_pg:query(Sql, [ChannelId, Cursor, Limit]).

%% @doc 删除订阅关系（软删除）
-spec delete(integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(ChannelId, Uid) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => 0}, <<"channel_id = $1 AND user_id = $2 AND status = 1">>, [
        ChannelId, Uid
    ]).

-spec delete(any(), integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(Conn, ChannelId, Uid) ->
    Tb = tablename(),
    elib_pg:update(
        Conn,
        Tb,
        #{status => 0},
        <<"channel_id = $1 AND user_id = $2 AND status = 1">>,
        [ChannelId, Uid]
    ).

%% @doc 增加未读计数
-spec increment_unread(integer()) -> ok.
increment_unread(ChannelId) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET unread_count = unread_count + 1 "
            "WHERE channel_id = $1 AND status = 1">>,
    _ = elib_pg:execute(Sql, [ChannelId]),
    ok.

%% @doc 清除频道的所有用户未读计数（慎用）
-spec clear_unread(integer()) -> {ok, non_neg_integer()} | {error, any()}.
clear_unread(ChannelId) ->
    Tb = tablename(),
    elib_pg:update(
        Tb,
        #{unread_count => 0, last_read_at => elib_dt:now()},
        <<"channel_id = $1 AND status = 1">>,
        [ChannelId]
    ).

%% @doc 清除指定用户在指定频道的未读计数
%% @param ChannelId 频道ID
%% @param Uid 用户ID
%% @returns {ok, AffectedRows} | {error, Reason}
-spec clear_unread(integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
clear_unread(ChannelId, Uid) ->
    Tb = tablename(),
    elib_pg:update(
        Tb,
        #{unread_count => 0, last_read_at => elib_dt:now()},
        <<"channel_id = $1 AND user_id = $2 AND status = 1">>,
        [ChannelId, Uid]
    ).

%% @doc 获取用户在所有频道的未读总数
-spec count_unread(integer()) -> non_neg_integer().
count_unread(Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT COALESCE(SUM(unread_count), 0) as total FROM ", Tb/binary,
            " WHERE user_id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, #{<<"total">> := Total}} -> Total;
        _ -> 0
    end.

%% @doc 获取指定用户在指定频道的未读数
-spec get_unread_count(integer(), integer()) -> non_neg_integer().
get_unread_count(ChannelId, Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT unread_count FROM ", Tb/binary,
            " WHERE channel_id = $1 AND user_id = $2 AND status = 1 LIMIT 1">>,
    case elib_pg:one(Sql, [ChannelId, Uid]) of
        {ok, #{<<"unread_count">> := Count}} when is_integer(Count), Count >= 0 ->
            Count;
        _ ->
            0
    end.

%% @doc 获取有未读消息的频道数量
-spec count_unread_channels(integer()) -> non_neg_integer().
count_unread_channels(Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT COUNT(*) as count FROM ", Tb/binary,
            " WHERE user_id = $1 AND status = 1 AND unread_count > 0">>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 获取用户在各频道的未读明细（仅 unread_count > 0）
-spec list_unread_by_uid(integer()) -> {ok, list(map())} | {error, any()}.
list_unread_by_uid(Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT channel_id, unread_count FROM ", Tb/binary,
            " WHERE user_id = $1 AND status = 1 AND unread_count > 0 ",
            "ORDER BY unread_count DESC, channel_id ASC">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 获取频道内所有订阅者的未读计数（包含 0）
-spec list_unread_counts_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_unread_counts_by_channel(ChannelId) ->
    Tb = tablename(),
    Sql =
        <<"SELECT user_id, unread_count FROM ", Tb/binary,
            " WHERE channel_id = $1 AND status = 1 ORDER BY user_id ASC">>,
    elib_pg:query(Sql, [ChannelId]).

%% @doc 检查用户是否订阅了频道
-spec is_subscribed(integer(), integer()) -> boolean().
is_subscribed(ChannelId, Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT 1 FROM ", Tb/binary,
            " WHERE channel_id = $1 AND user_id = $2 AND status = 1 LIMIT 1">>,
    case elib_pg:one(Sql, [ChannelId, Uid]) of
        {ok, _} -> true;
        _ -> false
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
