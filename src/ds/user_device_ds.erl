-module(user_device_ds).
%%%
% user_device_ds 是用户设备数据服务层
% 封装用户设备的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([save/4]).
-export([login_count/2]).
-export([device_name/2]).
-export([delete/2]).
-export([update_by_did/4]).
-export([count_by_uid/1, page/3]).
-export([list_public_keys/1]).
-export([list_public_keys_by_uids/1]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 获取用户设备列表
%% @param Uid 用户ID
%% @param Limit 每页数量
%% @param Offset 偏移量
%% @return {ok, list(map())} | {error, any()}
-spec page(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
page(Uid, Limit, Offset) ->
    user_device_repo:page(Uid, Limit, Offset).

%% @doc 获取用户公钥列表
%% @param Uid 用户ID
%% @return {ok, list(map())} | {error, any()}
-spec list_public_keys(integer()) -> {ok, list(map())} | {error, any()}.
list_public_keys(Uid) ->
    user_device_repo:list_public_keys(Uid).

%% @doc 批量获取用户公钥列表
%% @param Uids 用户ID列表
%% @return {ok, list(map())} | {error, any()}
-spec list_public_keys_by_uids([integer()]) -> {ok, list(map())} | {error, any()}.
list_public_keys_by_uids(Uids) ->
    user_device_repo:list_public_keys_by_uids(Uids).

%% @doc 统计用户设备数量
%% @param Uid 用户ID
%% @return non_neg_integer()
-spec count_by_uid(integer()) -> non_neg_integer().
count_by_uid(Uid) ->
    user_device_repo:count_by_uid(Uid).

%% @doc 获取设备名称
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return binary()
-spec device_name(integer(), binary()) -> binary().
device_name(Uid, DID) ->
    user_device_repo:device_name(Uid, DID).

%% @doc 获取登录次数
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return integer()
-spec login_count(integer(), binary()) -> integer().
login_count(Uid, DID) ->
    user_device_repo:login_count(Uid, DID).

%% @doc 保存设备信息
%% @param Now 当前时间
%% @param Uid 用户ID
%% @param DID 设备ID
%% @param PostVals 请求参数
%% @return ok | {error, any()}
-spec save(binary(), integer(), binary(), map()) -> ok | {error, any()}.
save(Now, Uid, DID, PostVals) ->
    user_device_repo:save(Now, Uid, DID, PostVals).

%% @doc 删除设备
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return ok
-spec delete(integer(), binary()) -> ok.
delete(Uid, DID) ->
    user_device_repo:delete(Uid, DID).

%% @doc 根据设备ID更新设备信息
%% @param Uid 用户ID
%% @param DID 设备ID
%% @param Set SET 子句
%% @param SetArgs 参数
%% @return {ok, integer()} | {error, any()}
-spec update_by_did(integer(), binary(), binary(), list()) -> {ok, integer()} | {error, any()}.
update_by_did(Uid, DID, Set, SetArgs) ->
    user_device_repo:update_by_did(Uid, DID, Set, SetArgs).

%% ===================================================================
%% Internal Functions
%% ===================================================================
