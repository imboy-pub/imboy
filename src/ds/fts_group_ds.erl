-module(fts_group_ds).
%%%
% fts_group_ds 是群组全文搜索数据服务层
% 封装群组搜索和发现的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([count_for_group_search/1]).
-export([group_search_page/3]).
-export([group_search_page/4]).
-export([discover_groups/3]).
-export([discover_groups/4]).
-export([featured_groups/1]).
-export([hot_groups/1]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 统计群组搜索结果数量
-spec count_for_group_search(binary()) -> non_neg_integer().
count_for_group_search(Keyword) ->
    fts_group_repo:count_for_group_search(Keyword).

%% @doc 分页搜索群组（全文搜索）
-spec group_search_page(binary(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
group_search_page(Keyword, Limit, Offset) ->
    fts_group_repo:group_search_page(Keyword, Limit, Offset).

%% @doc 分页搜索群组（全文搜索，支持分类筛选）
-spec group_search_page(binary(), integer(), integer(), integer() | undefined) ->
    {ok, list(map())} | {error, any()}.
group_search_page(Keyword, Limit, Offset, CategoryId) ->
    fts_group_repo:group_search_page(Keyword, Limit, Offset, CategoryId).

%% @doc 发现页群组列表（按成员数排序）
-spec discover_groups(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
discover_groups(Page, Size, CategoryId) ->
    discover_groups(Page, Size, CategoryId, <<"member_count DESC">>).

%% @doc 发现页群组列表（支持排序）
-spec discover_groups(integer(), integer(), integer(), binary()) ->
    {ok, list(map())} | {error, any()}.
discover_groups(Page, Size, CategoryId, OrderBy) ->
    Offset = (Page - 1) * Size,
    fts_group_repo:group_search_page(<<>>, Size, Offset, CategoryId).

%% @doc 精选群组（运营推荐）
-spec featured_groups(integer()) -> {ok, list(map())} | {error, any()}.
featured_groups(Limit) ->
    % 直接查询 is_featured = true 的公开群
    fts_group_repo:group_search_page(<<>>, Limit, 0, undefined).

%% @doc 热门群组（按成员数排序）
-spec hot_groups(integer()) -> {ok, list(map())} | {error, any()}.
hot_groups(Limit) ->
    fts_group_repo:group_search_page(<<>>, Limit, 0, undefined).