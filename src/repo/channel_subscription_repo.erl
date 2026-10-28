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
-export([clear_unread_tx/3]).
-export([advance_view_tx/4]).
-export([advance_view/3]).
-export([advance_view_to_current_tx/3]).
-export([advance_view_to_current/2]).
-export([count_unseen/2]).
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
            "SET status = 1, subscribed_at = EXCLUDED.subscribed_at, "
            %% W08（§6.8 J13.e）：复用订阅行 id 不代表复用旧查看资格——
            %% 重新激活在状态变更同事务递增激活版本并重置查看边界。
            "activation_version = ",
            Tb/binary,
            ".activation_version + 1, "
            "view_seq = 0 "
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
    Access = organization_resource_access_repo:channel_predicate_sql(<<"cs.channel_id">>, <<"$1">>),
    Sql =
        <<"SELECT cs.* FROM ", Tb/binary, " cs WHERE cs.user_id = $1 AND cs.status = 1 AND ",
            Access/binary,
            " "
            "ORDER BY is_pinned DESC, subscribed_at DESC">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 查询频道的所有订阅者
-spec list_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId) ->
    Tb = tablename(),
    %% Cache retained subscription facts; delivery checks current parent access.
    Sql = <<"SELECT user_id FROM ", Tb/binary, " WHERE channel_id = $1 AND status = 1">>,
    elib_pg:query(Sql, [ChannelId]).

%% @doc 分页查询频道的订阅者列表
%% @param ChannelId 频道ID
%% @param Cursor 偏移量（客户端传「已加载条数」，见
%%   channel_subscriber_page.dart _pageSize 注释），0 表示从头开始
%% @param Limit 每页数量
%% 分页契约：cursor = offset 型。曾按 `cs.id < cursor` 游标实现——id 为
%% TSID 大整数，与「条数」永不可比，第二页恒空，尾部订阅者在 UI 上
%% 永久不可见。排序补 cs.id DESC 保证翻页稳定。
-spec list_by_channel(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId, Cursor, Limit) when Cursor >= 0 ->
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
        "ORDER BY cs.is_pinned DESC, cs.subscribed_at DESC, cs.id DESC "
        "LIMIT $2 OFFSET $3"
    >>,
    elib_pg:query(Sql, [ChannelId, Limit, Cursor]).

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

-spec clear_unread_tx(any(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, any()}.
clear_unread_tx(Conn, ChannelId, Uid) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET unread_count = 0, last_read_at = $1",
            " WHERE channel_id = $2 AND user_id = $3 AND status = 1">>,
    elib_pg:execute_count(Conn, Sql, [elib_dt:now(), ChannelId, Uid]).

%% @doc W08（§6.8）：事务内推进查看边界 V。
%% 水位推进一律 GREATEST（J13.d：两端 B1<B2，晚到旧边界不倒退）；
%% boundary 超过频道已发布上界时夹到 publish_seq（防伪造前推）。
%% 仅激活订阅行可推进（status=1）。
-spec advance_view_tx(any(), integer(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, any()}.
advance_view_tx(Conn, ChannelId, Uid, Boundary) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " cs SET view_seq = GREATEST(cs.view_seq, "
            "LEAST($1, c.publish_seq)) "
            "FROM channel c "
            "WHERE cs.channel_id = c.id AND cs.channel_id = $2 "
            "AND cs.user_id = $3 AND cs.status = 1 "
            "RETURNING cs.view_seq">>,
    case elib_pg:query(Conn, Sql, [Boundary, ChannelId, Uid]) of
        {ok, [#{<<"view_seq">> := V}]} -> {ok, V};
        {ok, []} -> {error, not_subscribed};
        {error, _} = Err -> Err
    end.

%% @doc W08（§6.8）：推进查看边界（非事务包装，经 workspace 写守卫；
%% 归档频道 skipped——与 clear_unread 同款语义）。
-spec advance_view(integer(), integer(), integer()) ->
    {ok, non_neg_integer()} | skipped | {error, any()}.
advance_view(ChannelId, Uid, Boundary) ->
    case
        workspace_guard:write_tx_or_skip({channel, ChannelId}, fun(Conn) ->
            advance_view_tx(Conn, ChannelId, Uid, Boundary)
        end)
    of
        {written, Result} -> Result;
        skipped -> skipped;
        {error, _} = Error -> Error
    end.

%% W08（§10.4 混合版本）：旧 mark_read 的水位适配器内核——单语句把
%% view_seq 推进到"当前发布上界"（GREATEST 单调；服务端 publish_seq 即
%% 当次边界，不读聚合 unread_count，不从迁移前状态推断）。旧端显式
%% "标已读" = 确认当次服务端边界；之后发布的新消息 update_seq > 边界，
%% 不会被清掉。
-spec advance_view_to_current_tx(pid(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, not_subscribed} | {error, any()}.
advance_view_to_current_tx(Conn, ChannelId, Uid) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " cs SET view_seq = GREATEST(cs.view_seq, c.publish_seq) "
            "FROM channel c "
            "WHERE cs.channel_id = c.id AND cs.channel_id = $1 "
            "AND cs.user_id = $2 AND cs.status = 1 "
            "RETURNING cs.view_seq">>,
    case elib_pg:query(Conn, Sql, [ChannelId, Uid]) of
        {ok, [#{<<"view_seq">> := V}]} -> {ok, V};
        {ok, []} -> {error, not_subscribed};
        {error, _} = Err -> Err
    end.

%% @doc W08（§10.4）：旧 mark_read 适配器的非事务包装（写守卫/归档
%% skipped 语义同 advance_view/3）。
-spec advance_view_to_current(integer(), integer()) ->
    {ok, non_neg_integer()} | skipped | {error, any()}.
advance_view_to_current(ChannelId, Uid) ->
    case
        workspace_guard:write_tx_or_skip({channel, ChannelId}, fun(Conn) ->
            advance_view_to_current_tx(Conn, ChannelId, Uid)
        end)
    of
        {written, Result} -> Result;
        skipped -> skipped;
        {error, _} = Error -> Error
    end.

%% @doc W08（§6.8）：未查看更新数 = 当前可读、未撤销且 update_seq > V 的
%% 发布项（作者自身发布已在发布事务内推进其 V，天然不计）。
-spec count_unseen(integer(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, any()}.
count_unseen(ChannelId, ViewSeq) ->
    MsgTb = channel_message_repo:tablename(),
    Sql =
        <<"SELECT COUNT(*) AS count FROM ", MsgTb/binary,
            " WHERE channel_id = $1 AND update_seq > $2 "
            "AND status = 1 AND revoked = false">>,
    case elib_pg:one(Sql, [ChannelId, ViewSeq]) of
        {ok, #{<<"count">> := N}} when is_integer(N), N >= 0 -> {ok, N};
        {ok, #{<<"count">> := N}} -> {ok, erlang:max(0, elib_cnv:safe_to_integer(N))};
        {error, _} = Err -> Err
    end.

%% @doc 获取用户在所有频道的未读总数
-spec count_unread(integer()) -> non_neg_integer().
count_unread(Uid) ->
    Tb = tablename(),
    Access = organization_resource_access_repo:channel_predicate_sql(<<"cs.channel_id">>, <<"$1">>),
    Sql =
        <<"SELECT COALESCE(SUM(unread_count), 0) as total FROM ", Tb/binary,
            " cs WHERE cs.user_id = $1 AND cs.status = 1 AND ", Access/binary>>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, #{<<"total">> := Total}} -> Total;
        _ -> 0
    end.

%% @doc 获取指定用户在指定频道的未读数
-spec get_unread_count(integer(), integer()) -> non_neg_integer().
get_unread_count(ChannelId, Uid) ->
    Tb = tablename(),
    Access = organization_resource_access_repo:channel_predicate_sql(<<"cs.channel_id">>, <<"$2">>),
    Sql =
        <<"SELECT unread_count FROM ", Tb/binary,
            " cs WHERE cs.channel_id = $1 AND cs.user_id = $2 AND cs.status = 1 AND ",
            Access/binary, " LIMIT 1">>,
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
    Access = organization_resource_access_repo:channel_predicate_sql(<<"cs.channel_id">>, <<"$1">>),
    Sql =
        <<"SELECT COUNT(*) as count FROM ", Tb/binary,
            " cs WHERE cs.user_id = $1 AND cs.status = 1 AND unread_count > 0 AND ",
            Access/binary>>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 获取用户在各频道的未读明细（仅 unread_count > 0）
-spec list_unread_by_uid(integer()) -> {ok, list(map())} | {error, any()}.
list_unread_by_uid(Uid) ->
    Tb = tablename(),
    Access = organization_resource_access_repo:channel_predicate_sql(<<"cs.channel_id">>, <<"$1">>),
    Sql =
        <<"SELECT channel_id, unread_count FROM ", Tb/binary,
            " cs WHERE cs.user_id = $1 AND cs.status = 1 AND unread_count > 0 AND ", Access/binary,
            " ", "ORDER BY unread_count DESC, channel_id ASC">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 获取频道内所有订阅者的未读计数（包含 0）
-spec list_unread_counts_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_unread_counts_by_channel(ChannelId) ->
    Tb = tablename(),
    Access = organization_resource_access_repo:channel_predicate_sql(
        <<"cs.channel_id">>, <<"cs.user_id">>
    ),
    Sql =
        <<"SELECT user_id, unread_count FROM ", Tb/binary,
            " cs WHERE cs.channel_id = $1 AND cs.status = 1 AND ", Access/binary,
            " ORDER BY cs.user_id ASC">>,
    elib_pg:query(Sql, [ChannelId]).

%% @doc 检查用户是否订阅了频道
-spec is_subscribed(integer(), integer()) -> boolean().
is_subscribed(ChannelId, Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT 1 FROM ", Tb/binary,
            " WHERE channel_id = $1 AND user_id = $2 AND status = 1 LIMIT 1">>,
    %% elib_pg:one/2 uses #{} as its no-row default.  Treating every
    %% {ok, _} as a hit turns a missing subscription into true.
    case elib_pg:one(Sql, [ChannelId, Uid]) of
        {ok, Row} when is_map(Row), map_size(Row) > 0 -> true;
        _ -> false
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
