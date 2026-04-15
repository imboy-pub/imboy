-module(user_collect_ds).
%%%
% user_collect_ds 是用户收藏数据服务层
% 封装用户收藏的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

-export([count_by_uid_kind_id/2]).
-export([delete/2]).
-export([update/3]).
-export([page/5]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 统计用户指定类型的收藏数量
%% @param Uid 用户ID
%% @param KindId 收藏类型ID
%% @return Count 收藏数量
-spec count_by_uid_kind_id(integer(), binary()) -> non_neg_integer().
count_by_uid_kind_id(Uid, KindId) ->
    user_collect_repo:count_by_uid_kind_id(Uid, KindId).

%% @doc 删除用户收藏
%% @param Uid 用户ID
%% @param KindId 收藏类型ID
%% @return {ok, Count} | {error, Reason}
-spec delete(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete(Uid, KindId) ->
    user_collect_repo:delete(Uid, KindId).

%% @doc 更新用户收藏
%% @param Uid 用户ID
%% @param KindId 收藏类型ID
%% @param Data 要更新的数据map
%% @return {ok, Count} | {error, Reason}
-spec update(integer(), binary(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Uid, KindId, Data) ->
    user_collect_repo:update(Uid, KindId, Data).

%% @doc 分页查询用户收藏列表
%% @param Column 查询列
%% @param WhereMap 过滤条件
%% @param Order 排序
%% @param Page 页码
%% @param Size 每页大小
%% @return {ok, map()} | {error, term()}
-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, WhereMap, Order, Page, Size) ->
    Tb = user_collect_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, WhereMap, Order, Page, Size).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
