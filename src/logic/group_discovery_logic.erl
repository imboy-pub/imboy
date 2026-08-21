-module(group_discovery_logic).
%%%
% group_discovery_logic 是群组发现业务逻辑模块
% 处理公开群搜索、发现、分类浏览等业务逻辑
%%%

-export([search/3]).
-export([search/4]).
-export([discover/4]).
-export([featured/1]).
-export([hot/1]).
-export([categories/0]).
-export([preview/1]).

-include("log.hrl").
-include("imboy_const.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 搜索公开群组（全文搜索）
%% 只返回 status=1（活跃）且 type=1（公开）的群组
%% @param Keyword 搜索关键词
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, #{list => [map()], total => integer()}} | {error, binary()}
-spec search(binary(), pos_integer(), pos_integer()) -> {ok, map()} | {error, binary()}.
search(Keyword, Page, Size) ->
    search(Keyword, Page, Size, undefined).

%% @doc 搜索公开群组（全文搜索，支持分类筛选）
-spec search(binary(), pos_integer(), pos_integer(), integer() | undefined) ->
    {ok, map()} | {error, binary()}.
search(<<>>, _Page, _Size, _CategoryId) ->
    {error, <<"搜索关键词不能为空"/utf8>>};
search(Keyword, Page, Size, CategoryId) ->
    Offset = (Page - 1) * Size,
    Total = fts_group_ds:count_for_group_search(Keyword),
    case fts_group_ds:group_search_page(Keyword, Size, Offset, CategoryId) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows, <<"total">> => Total}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 发现页群组列表
%% @param Page 页码
%% @param Size 每页数量
%% @param CategoryId 分类ID（undefined 表示全部）
%% @param Sort 排序方式（popular | newest）
%% @return {ok, #{list => [map()], total => integer()}} | {error, binary()}
-spec discover(pos_integer(), pos_integer(), integer() | undefined, binary()) ->
    {ok, map()} | {error, binary()}.
discover(Page, Size, CategoryId, Sort) ->
    OrderBy = case Sort of
        <<"popular">> -> <<"member_count DESC">>;
        <<"newest">> -> <<"created_at DESC">>;
        _ -> <<"member_count DESC">>
    end,
    Offset = (Page - 1) * Size,
    case fts_group_ds:discover_groups(Page, Size, CategoryId, OrderBy) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows, <<"total">> => length(Rows)}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 精选群组（运营推荐）
-spec featured(pos_integer()) -> {ok, map()} | {error, binary()}.
featured(Limit) ->
    case fts_group_ds:featured_groups(Limit) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 热门群组（按成员数排序）
-spec hot(pos_integer()) -> {ok, map()} | {error, binary()}.
hot(Limit) ->
    case fts_group_ds:hot_groups(Limit) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取公开群分类列表
-spec categories() -> {ok, map()} | {error, binary()}.
categories() ->
    % 直接查询 group_category 表
    Sql = <<"SELECT id, name, icon, sort_order FROM public.group_category WHERE status = 1 ORDER BY sort_order">>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            {ok, #{<<"list">> => Rows}};
        {error, _} ->
            {ok, #{<<"list">> => []}}
    end.

%% @doc 群组预览（公开访问，无需加入）
%% 返回群组基本信息，用于发现页展示
%% @param GroupId 群组ID
%% @return {ok, map()} | {error, binary()}
-spec preview(integer()) -> {ok, map()} | {error, binary()}.
preview(GroupId) ->
    Columns = <<"id, title, avatar, introduction, member_count, type, join_limit, "
        "category_id, is_featured, created_at">>,
    case group_ds:find_by_id(GroupId, Columns) of
        Group when is_map(Group), map_size(Group) > 0 ->
            % 只返回公开信息，过滤敏感字段
            Preview = maps:with(
                [id, title, avatar, introduction, member_count, type, join_limit,
                    category_id, is_featured, created_at],
                Group
            ),
            {ok, Preview};
        {error, _} ->
            {error, <<"群组不存在或已删除"/utf8>>};
        _ ->
            {error, <<"群组不存在或已删除"/utf8>>}
    end.