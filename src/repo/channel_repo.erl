-module(channel_repo).
%%%
% channel_repo 是 channel repository 缩写
% 频道数据仓库层，提供频道数据的基础数据库操作
%%%

-export([tablename/0]).
-export([add/1]).
-export([add/2]).
-export([find_by_id/2]).
-export([find_by_custom_id/1]).
-export([list_by_ids/2]).
-export([list_subscribed/2]).
-export([list_managed/1]).
-export([update/2]).
-export([delete/1]).
-export([increment_subscribers/2]).
-export([search/3]).
-export([list_discover/2]).
% 统计相关
-export([get_reaction_count/1]).
-export([has_viewed_message/2]).
-export([insert_message_view/4]).
-export([insert_reaction/5]).
-export([delete_reaction/4]).
-export([get_daily_stats/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取频道表的表名
%% @return 返回频道表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"channel">>).

%% @doc 添加新频道
%% @param Data 包含频道信息的map
%% @return {ok, ChannelId, #{}} | {error, Reason}
-spec add(map()) -> {ok, integer(), map()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>)).

%% @doc 添加新频道（使用连接）
%% @param Conn 数据库连接
%% @param Data 包含频道信息的map
%% @return {ok, ChannelId, #{}} | {error, Reason}
-spec add(any(), map()) -> {ok, integer(), map()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Conn, Tb, Data, <<"RETURNING id">>)).

%% @doc 根据频道ID查找频道信息
%% @param ChannelId 频道ID
%% @param Column 要查询的列名
%% @return Row | {error, Reason}
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, any()}.
find_by_id(ChannelId, Column) when is_list(ChannelId); is_binary(ChannelId) ->
    find_by_id(ec_cnv:to_integer(ChannelId), Column);
find_by_id(ChannelId, Column) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => ChannelId, status => 1}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据自定义ID查找频道
%% @param CustomId 自定义ID
%% @return Row | {error, Reason}
-spec find_by_custom_id(binary()) -> map() | {error, any()}.
find_by_custom_id(CustomId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE custom_id = $1 AND status = 1 LIMIT 1">>,
    case elib_pg:one(Sql, [CustomId]) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据频道ID列表批量查询频道信息
%% @param Ids 频道ID列表
%% @param Column 要查询的列名
%% @return {ok, Rows} | {error, Reason}
-spec list_by_ids(list(integer() | binary()), binary()) -> {ok, list(map())} | {error, any()}.
list_by_ids(Ids, Column) when length(Ids) > 0 ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => {in, Ids}, status => 1}, #{}),
    elib_pg:query(Sql, Params);
list_by_ids([], _Column) ->
    {ok, []}.

%% @doc 查询用户订阅的频道列表
%% @param Uid 用户ID
%% @param Column 要查询的列名
%% @return {ok, Rows} | {error, Reason}
-spec list_subscribed(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_subscribed(Uid, Column) ->
    Tb = tablename(),
    SubTb = channel_subscription_repo:tablename(),
    AdminTb = channel_admin_repo:tablename(),
    Sql = <<"SELECT ", Column/binary, ", "
            "COALESCE(a.role, 0) as user_role, "
            "true as is_subscribed "
            "FROM ", Tb/binary, " c "
            "INNER JOIN ", SubTb/binary, " s ON c.id = s.channel_id "
            "LEFT JOIN ", AdminTb/binary, " a ON c.id = a.channel_id AND a.user_id = s.user_id "
            "WHERE s.user_id = $1 AND s.status = 1 AND c.status = 1 "
            "ORDER BY s.is_pinned DESC, s.subscribed_at DESC">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 查询用户管理的频道列表
%% @param Uid 用户ID
%% @return {ok, Rows} | {error, Reason}
-spec list_managed(integer()) -> {ok, list(map())} | {error, any()}.
list_managed(Uid) ->
    Tb = tablename(),
    AdminTb = channel_admin_repo:tablename(),
    Sql = <<"SELECT c.*, a.role as user_role, true as is_subscribed "
            "FROM ", Tb/binary, " c "
            "INNER JOIN ", AdminTb/binary, " a ON c.id = a.channel_id "
            "WHERE a.user_id = $1 AND c.status = 1 "
            "ORDER BY a.role DESC, c.created_at DESC">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 更新频道信息
%% @param ChannelId 频道ID
%% @param Data 要更新的数据
%% @return {ok, Count} | {error, Reason}
-spec update(integer(), map()) -> {ok, non_neg_integer()} | {error, any()}.
update(ChannelId, Data) ->
    Tb = tablename(),
    UpdateData = maps:without([<<"id">>], Data),
    elib_pg:update(Tb, UpdateData, <<"id = $1">>, [ChannelId]).

%% @doc 删除频道（软删除）
%% @param ChannelId 频道ID
%% @return {ok, Count} | {error, Reason}
-spec delete(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(ChannelId) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => -1}, <<"id = $1">>, [ChannelId]).

%% @doc 增减订阅者数量
%% @param ChannelId 频道ID
%% @param Delta 变化量（正数增加，负数减少）
%% @return {ok, Count} | {error, Reason}
-spec increment_subscribers(integer(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
increment_subscribers(ChannelId, Delta) ->
    Tb = tablename(),
    Op = if Delta > 0 -> <<"+">>; true -> <<"-">> end,
    DeltaAbs = abs(Delta),
    Sql = <<"UPDATE ", Tb/binary,
            " SET subscriber_count = subscriber_count ", Op/binary, " $1, "
            "updated_at = CURRENT_TIMESTAMP "
            "WHERE id = $2 AND status = 1">>,
    elib_pg:execute(Sql, [DeltaAbs, ChannelId]).

%% @doc 搜索频道
%% @param Keyword 搜索关键词
%% @param Limit 返回数量限制
%% @param Column 要查询的列名
%% @return {ok, Rows} | {error, Reason}
-spec search(binary(), integer(), binary()) -> {ok, list(map())} | {error, any()}.
search(Keyword, Limit, Column) ->
    Tb = tablename(),
    Pattern = <<"%", Keyword/binary, "%">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE status = 1 AND (name LIKE $1 OR description LIKE $1 OR custom_id LIKE $1) "
            "ORDER BY subscriber_count DESC LIMIT $2">>,
    elib_pg:query(Sql, [Pattern, Limit]).

%% @doc 获取发现频道列表（推荐频道）
%% 返回公开的、活跃的频道，按订阅数和创建时间排序
%% @param Limit 返回数量限制
%% @param Column 要查询的列名
%% @return {ok, Rows} | {error, Reason}
-spec list_discover(integer(), binary()) -> {ok, list(map())} | {error, any()}.
list_discover(Limit, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE status = 1 AND type = 0 "  % status=1 表示正常，type=0 表示公开频道
            "ORDER BY subscriber_count DESC, created_at DESC LIMIT $1">>,
    elib_pg:query(Sql, [Limit]).

%% ===================================================================
%% 统计相关函数
%% ===================================================================

%% @doc 获取频道反应总数
-spec get_reaction_count(integer()) -> {ok, integer()}.
get_reaction_count(ChannelId) ->
    Sql = <<"SELECT COUNT(*) FROM channel_reaction WHERE channel_id = $1">>,
    case elib_pg:one(Sql, [ChannelId]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        _ -> {ok, 0}
    end.

%% @doc 检查用户是否已阅读消息
-spec has_viewed_message(integer(), integer()) -> boolean().
has_viewed_message(MessageId, UserId) ->
    Sql = <<"SELECT 1 FROM channel_message_view WHERE message_id = $1 AND user_id = $2 LIMIT 1">>,
    case elib_pg:one(Sql, [MessageId, UserId]) of
        {ok, _} -> true;
        _ -> false
    end.

%% @doc 插入消息阅读记录
-spec insert_message_view(integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
insert_message_view(ChannelId, MessageId, UserId, ViewedAt) ->
    Sql = <<"INSERT INTO channel_message_view (channel_id, message_id, user_id, viewed_at) "
            "VALUES ($1, $2, $3, $4) "
            "ON CONFLICT (message_id, user_id) DO NOTHING RETURNING id">>,
    elib_pg:one(Sql, [ChannelId, MessageId, UserId, ViewedAt]).

%% @doc 插入消息反应
-spec insert_reaction(integer(), integer(), integer(), binary(), integer()) -> {ok, map()} | {error, term()}.
insert_reaction(ChannelId, MessageId, UserId, ReactionType, CreatedAt) ->
    Sql = <<"INSERT INTO channel_reaction (channel_id, message_id, user_id, reaction_type, created_at) "
            "VALUES ($1, $2, $3, $4, $5) "
            "ON CONFLICT (message_id, user_id, reaction_type) DO NOTHING RETURNING id">>,
    elib_pg:one(Sql, [ChannelId, MessageId, UserId, ReactionType, CreatedAt]).

%% @doc 删除消息反应
-spec delete_reaction(integer(), integer(), integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete_reaction(ChannelId, MessageId, UserId, ReactionType) ->
    Sql = <<"DELETE FROM channel_reaction "
            "WHERE channel_id = $1 AND message_id = $2 AND user_id = $3 AND reaction_type = $4">>,
    elib_pg:execute(Sql, [ChannelId, MessageId, UserId, ReactionType]).

%% @doc 获取频道每日统计数据
-spec get_daily_stats(integer(), integer()) -> {ok, list(map())} | {error, term()}.
get_daily_stats(ChannelId, Days) ->
    Sql = <<"SELECT channel_id, stats_date, new_subscribers, unsubscribers, net_subscribers, "
            "messages_count, total_views, total_reactions, active_viewers "
            "FROM channel_stats_daily "
            "WHERE channel_id = $1 AND stats_date >= CURRENT_DATE - INTERVAL '1 day' * $2 "
            "ORDER BY stats_date DESC">>,
    elib_pg:query(Sql, [ChannelId, Days]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
