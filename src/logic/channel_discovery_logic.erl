-module(channel_discovery_logic).
%%%
% channel_discovery_logic 是频道发现业务逻辑模块
% 处理频道搜索、发现、分类浏览、热门频道等业务逻辑
%%%

-export([search/3]).
-export([search/4]).
-export([discover/4]).
-export([featured/1]).
-export([trending/2]).
-export([categories/0]).

-include("log.hrl").
-include("imboy_const.hrl").

-define(DEFAULT_LIMIT, 20).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 搜索频道（全文搜索）
%% 只返回 status=1（活跃）的频道
%% @param Keyword 搜索关键词
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, #{list => [map()], total => integer()}} | {error, binary()}
-spec search(binary(), pos_integer(), pos_integer()) -> {ok, map()} | {error, binary()}.
search(Keyword, Page, Size) ->
    search(Keyword, Page, Size, undefined).

%% @doc 搜索频道（全文搜索，支持分类筛选）
-spec search(binary(), pos_integer(), pos_integer(), integer() | undefined) ->
    {ok, map()} | {error, binary()}.
search(<<>>, _Page, _Size, _CategoryId) ->
    {error, <<"搜索关键词不能为空"/utf8>>};
search(Keyword, Page, Size, CategoryId) ->
    Offset = (Page - 1) * Size,
    Total = count_channel_search(Keyword, CategoryId),
    case channel_search_page(Keyword, Size, Offset, CategoryId) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows, <<"total">> => Total}};
        {error, Reason} ->
            %% DB 错误 term 不 dump 给用户，记日志后返回中文兜底
            ?ERROR_LOG([<<"channel_discovery search failed">>, Keyword, Reason]),
            {error, <<"搜索失败，请稍后重试"/utf8>>}
    end.

%% @doc 发现页频道列表
%% @param Page 页码
%% @param Size 每页数量
%% @param CategoryId 分类ID（undefined 表示全部）
%% @param Sort 排序方式（popular | newest | active）
%% @return {ok, #{list => [map()], total => integer()}} | {error, binary()}
-spec discover(pos_integer(), pos_integer(), integer() | undefined, binary()) ->
    {ok, map()} | {error, binary()}.
discover(Page, Size, CategoryId, Sort) ->
    OrderBy =
        case Sort of
            <<"popular">> -> <<"subscriber_count DESC">>;
            <<"newest">> -> <<"created_at DESC">>;
            <<"active">> -> <<"updated_at DESC">>;
            _ -> <<"subscriber_count DESC">>
        end,
    Offset = (Page - 1) * Size,
    Sql = build_discover_sql(CategoryId, OrderBy),
    Params =
        case CategoryId of
            undefined -> [Size, Offset];
            _ -> [CategoryId, Size, Offset]
        end,
    case elib_pg:query(Sql, Params) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows, <<"total">> => length(Rows)}};
        {error, Reason} ->
            ?ERROR_LOG([<<"channel_discovery discover failed">>, Reason]),
            {error, <<"查询失败，请稍后重试"/utf8>>}
    end.

%% @doc 精选频道（运营推荐）
-spec featured(pos_integer()) -> {ok, map()} | {error, binary()}.
featured(Limit) ->
    Sql = <<
        "SELECT c.id, c.name, c.description, c.avatar, c.type, c.custom_id, "
        "c.subscriber_count, c.is_verified, c.tags, c.category_id, c.created_at "
        "FROM public.channel c "
        "WHERE c.status = 1 AND c.is_featured = true "
        "ORDER BY c.featured_at DESC NULLS LAST "
        "LIMIT $1"
    >>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows}};
        {error, Reason} ->
            ?ERROR_LOG([<<"channel_discovery featured failed">>, Reason]),
            {error, <<"查询失败，请稍后重试"/utf8>>}
    end.

%% @doc 热门频道（基于订阅数 + 近期活跃度）
%% 使用 channel_stats_daily 表的统计数据进行排序
%% @param Period 统计周期（7 | 30 天）
%% @param Limit 返回数量
-spec trending(integer(), pos_integer()) -> {ok, map()} | {error, binary()}.
trending(Period, Limit) ->
    % 计算热门度：订阅数 + 近期消息数 + 活跃浏览数
    Sql = <<
        "SELECT c.id, c.name, c.description, c.avatar, c.type, c.custom_id, "
        "c.subscriber_count, c.is_verified, c.tags, c.category_id, c.created_at, "
        "COALESCE(SUM(s.new_subscribers), 0) as recent_subscribers, "
        "COALESCE(SUM(s.messages_count), 0) as recent_messages, "
        "COALESCE(SUM(s.active_viewers), 0) as recent_viewers "
        "FROM public.channel c "
        "LEFT JOIN public.channel_stats_daily s ON c.id = s.channel_id "
        "AND s.stats_date >= CURRENT_DATE - $1::integer "
        "WHERE c.status = 1 "
        "GROUP BY c.id "
        "ORDER BY (c.subscriber_count * 0.4 + COALESCE(SUM(s.new_subscribers), 0) * 0.3 "
        "  + COALESCE(SUM(s.messages_count), 0) * 0.2 + COALESCE(SUM(s.active_viewers), 0) * 0.1) DESC "
        "LIMIT $2"
    >>,
    case elib_pg:query(Sql, [Period, Limit]) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows}};
        {error, Reason} ->
            ?ERROR_LOG([<<"channel_discovery trending failed">>, Reason]),
            {error, <<"查询失败，请稍后重试"/utf8>>}
    end.

%% @doc 获取频道分类列表
-spec categories() -> {ok, map()} | {error, binary()}.
categories() ->
    Sql = <<
        "SELECT id, name, icon, sort_order FROM public.channel_category "
        "WHERE status = 1 ORDER BY sort_order"
    >>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows}};
        {error, _} ->
            {ok, #{<<"list">> => []}}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 统计频道搜索结果数量
-spec count_channel_search(binary(), integer() | undefined) -> non_neg_integer().
count_channel_search(Keyword, CategoryId) ->
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            WhereClause = build_search_where(CategoryId),
            Sql = <<
                "SELECT count(*) as count FROM public.fts_channel fts "
                "LEFT JOIN public.channel c ON c.id = fts.channel_id "
                "WHERE ",
                WhereClause/binary
            >>,
            Params =
                case CategoryId of
                    undefined -> [Keyword2];
                    _ -> [Keyword2, CategoryId]
                end,
            case elib_pg:one(Sql, Params) of
                {ok, #{<<"count">> := Count}} -> Count;
                _ -> 0
            end;
        _ ->
            0
    end.

%% @doc 分页搜索频道（全文搜索）
-spec channel_search_page(binary(), integer(), integer(), integer() | undefined) ->
    {ok, list(map())} | {error, any()}.
channel_search_page(Keyword, Size, Offset, CategoryId) ->
    Sql1 =
        <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case elib_pg:query(Sql1, [Keyword]) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            WhereClause = build_search_where(CategoryId),
            Sql = <<
                "SELECT c.id, c.name, c.description, c.avatar, c.type, c.custom_id, "
                "c.subscriber_count, c.is_verified, c.tags, c.category_id, c.created_at, "
                "ts_rank_cd(fts.token, to_tsquery('jiebacfg', $1)) as rank "
                "FROM public.fts_channel fts "
                "LEFT JOIN public.channel c ON c.id = fts.channel_id "
                "WHERE ",
                WhereClause/binary,
                " "
                "ORDER BY rank DESC "
                "LIMIT $",
                (build_param_index(
                    length([
                        Keyword2
                        | case CategoryId of
                            undefined -> [];
                            _ -> [CategoryId]
                        end
                    ]) + 1
                ))/binary,
                " "
                "OFFSET $",
                (build_param_index(
                    length([
                        Keyword2
                        | case CategoryId of
                            undefined -> [];
                            _ -> [CategoryId]
                        end
                    ]) + 2
                ))/binary
            >>,
            Params =
                [Keyword2] ++
                    case CategoryId of
                        undefined -> [];
                        _ -> [CategoryId]
                    end ++ [Size, Offset],
            elib_pg:query(Sql, Params);
        _ ->
            {ok, []}
    end.

%% @doc 构建搜索 WHERE 子句
-spec build_search_where(integer() | undefined) -> binary().
build_search_where(undefined) ->
    <<"c.status = 1 AND fts.token @@ to_tsquery('jiebacfg', $1)">>;
build_search_where(_CategoryId) ->
    <<"c.status = 1 AND c.category_id = $2 AND fts.token @@ to_tsquery('jiebacfg', $1)">>.

%% @doc 构建发现页 SQL
-spec build_discover_sql(integer() | undefined, binary()) -> binary().
build_discover_sql(undefined, OrderBy) ->
    <<
        "SELECT c.id, c.name, c.description, c.avatar, c.type, c.custom_id, "
        "c.subscriber_count, c.is_verified, c.tags, c.category_id, c.created_at "
        "FROM public.channel c "
        "WHERE c.status = 1 "
        "ORDER BY c.",
        OrderBy/binary,
        " "
        "LIMIT $1 OFFSET $2"
    >>;
build_discover_sql(_CategoryId, OrderBy) ->
    <<
        "SELECT c.id, c.name, c.description, c.avatar, c.type, c.custom_id, "
        "c.subscriber_count, c.is_verified, c.tags, c.category_id, c.created_at "
        "FROM public.channel c "
        "WHERE c.status = 1 AND c.category_id = $1 "
        "ORDER BY c.",
        OrderBy/binary,
        " "
        "LIMIT $2 OFFSET $3"
    >>.

%% @doc 构建参数索引
-spec build_param_index(integer()) -> binary().
build_param_index(N) ->
    integer_to_binary(N).
