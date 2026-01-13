-module(user_denylist_ds).
%%%
% user_denylist_ds 是用户黑名单数据服务层
% 封装用户黑名单的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([count_for_uid/1]).
-export([page_for_uid/3]).
-export([add/3]).
-export([remove/2]).
-export([in_denylist/2]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 统计用户黑名单数量
%% @param Uid 用户ID
%% @return integer() 黑名单数量
-spec count_for_uid(integer()) -> integer().
count_for_uid(Uid) ->
    user_denylist_repo:count_for_uid(Uid).

%% @doc 获取用户黑名单分页数据
%% @param Uid 用户ID
%% @param Size 每页大小
%% @param Offset 偏移量
%% @return {ok, list(map())} | {error, any()}
-spec page_for_uid(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
page_for_uid(Uid, Size, Offset) ->
    user_denylist_repo:page_for_uid(Uid, Size, Offset).

%% @doc 添加黑名单
%% @param Uid 用户ID
%% @param DeniedUserId 被拉黑的用户ID
%% @param CreatedAt 创建时间
%% @return {ok, integer()} | {error, any()}
-spec add(integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
add(Uid, DeniedUserId, CreatedAt) ->
    user_denylist_repo:add(Uid, DeniedUserId, CreatedAt).

%% @doc 移除黑名单
%% @param Uid 用户ID
%% @param DeniedUserId 被移除的用户ID
%% @return {ok, integer()} | {error, any()}
-spec remove(integer(), integer()) -> {ok, integer()} | {error, any()}.
remove(Uid, DeniedUserId) ->
    user_denylist_repo:remove(Uid, DeniedUserId).

%% @doc 检查用户是否在黑名单中
%% @param Uid 用户ID
%% @param DeniedUserId 待检查的用户ID
%% @return integer() 1 表示在黑名单中，0 表示不在
-spec in_denylist(integer(), integer()) -> integer().
in_denylist(Uid, DeniedUserId) ->
    user_denylist_repo:in_denylist(Uid, DeniedUserId).

%% ===================================================================
%% Internal Functions
%% ===================================================================
