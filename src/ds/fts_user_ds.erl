-module(fts_user_ds).
%%%
% fts_user_ds 是全文搜索用户数据服务层
% 封装用户全文搜索的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([allow_search/1]).
-export([count_for_user_search_page/1]).
-export([user_search_page/3]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 检查用户是否允许被搜索
%% @param Uid 用户ID
%% @return true 允许搜索 | false 不允许搜索
-spec allow_search(integer()) -> boolean().
allow_search(Uid) ->
    fts_user_repo:allow_search(Uid).

%% @doc 统计用户搜索结果数量
%% @param Keyword 搜索关键词（空字符串返回0）
%% @return Count 匹配的用户数量
-spec count_for_user_search_page(binary()) -> non_neg_integer().
count_for_user_search_page(Keyword) ->
    fts_user_repo:count_for_user_search_page(Keyword).

%% @doc 分页搜索用户（全文搜索）
%% 使用 PostgreSQL 的 pg_jieba 分词插件进行中文全文搜索
%% @param Keyword 搜索关键词
%% @param Limit 查询结果数量限制
%% @param Offset 查询结果偏移量
%% @return {ok, Rows} | {error, Reason}
-spec user_search_page(binary(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
user_search_page(Keyword, Limit, Offset) ->
    fts_user_repo:user_search_page(Keyword, Limit, Offset).

%% ===================================================================
%% Internal Functions
%% ===================================================================
