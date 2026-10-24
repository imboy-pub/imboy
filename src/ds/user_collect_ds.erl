-module(user_collect_ds).
-moduledoc "用户收藏数据服务层 / user collect data service。".
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
-export([find_any_by_uid_kind_id/2, sealed_exists/2, tombstone_sealed/2]).

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
%% E2EE 收藏 v1（迁移 227）：转发 repo，语义见 user_collect_repo 同名函数
%% ===================================================================

%% @doc 取该 (user_id, kind_id) 任意 status 行（单行模型，至多一行）。
-spec find_any_by_uid_kind_id(integer(), binary()) -> not_found | {ok, map()}.
find_any_by_uid_kind_id(Uid, KindId) ->
    user_collect_repo:find_any_by_uid_kind_id(Uid, KindId).

%% @doc sealed 行存在性（任意 status）——legacy 守卫。
-spec sealed_exists(integer(), binary()) -> boolean().
sealed_exists(Uid, KindId) ->
    user_collect_repo:sealed_exists(Uid, KindId).

%% @doc sealed 行墓碑化（§7.3/§9）：事务内 key_state 锁先行 + status=0
%% + generation+1 + attach=''（触发器 −1）。幂等：已墓碑为 noop。
-spec tombstone_sealed(integer(), binary()) -> {ok, tombstoned | noop | deleted} | {error, term()}.
tombstone_sealed(Uid, KindId) ->
    elib_pg:with_tx(fun(Conn) ->
        %% 墓碑不携带新 key_version：仅取用户级行锁串行化（§7.2），
        %% 不过门不推进高水位。
        {ok, _Hw} = user_collect_repo:key_state_lock_tx(Conn, Uid),
        {ok, Res} = user_collect_repo:tombstone_sealed_tx(Conn, Uid, KindId),
        {ok, Res}
    end).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
