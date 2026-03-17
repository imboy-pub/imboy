-module(adm_app_version_logic).
%%%
% adm_app_version 业务逻辑模块
% adm_app_version business logic module
%%%

-export ([delete/1]).
-export ([delete_by_id/1]).
-export ([save/1]).

-export([vsn_sort/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 保存或更新应用版本信息
%% 根据 ID 存在与否决定是更新还是新建记录
%% @param Data 包含版本信息的数据映射
%% @return {ok, Result} | {error, Reason}
%% @example adm_app_version_logic:save(#{<<"id">> => 1, <<"vsn">> => <<"1.0.0">>}).
-spec save(map()) -> {ok, any()} | {error, any()}.
save(Data) ->
    % 使用 DS 层接口
    app_version_ds:save(Data).

%% @doc 删除应用版本记录
%% 根据 WHERE 条件删除对应的版本记录
%% @param Where 删除条件的 SQL WHERE 子句
%% @return ok
-spec delete(binary()) -> ok.
delete(Where) ->
    % 使用 DS 层接口
    app_version_ds:delete(Where).

%% @doc 根据 ID 删除应用版本记录（安全版本，使用参数化查询）
%% @param Id 要删除的版本记录 ID（原始数字 ID，非 HashID）
%% @return {ok, Affected} | {error, Reason}
-spec delete_by_id(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_id(Id) when is_integer(Id), Id > 0 ->
    % 使用 DS 层接口
    app_version_ds:delete_by_id(Id);
delete_by_id(_Id) ->
    {error, invalid_id}.

% adm_app_version_logic:vsn_sort(<<"0.2">>).
% adm_app_version_logic:vsn_sort(<<"0.2.22">>).
%  adm_app_version_logic:vsn_sort(<<"10.102.22">>).
%% @doc 将版本号转换为数值用于排序
%% 支持语义化版本号格式，转换为单一数值便于数据库排序
%% @param Vsn 版本号字符串，如 "1.2.3"
%% @return integer() 转换后的数值，用于排序比较
-spec vsn_sort(binary()) -> integer().
vsn_sort(Vsn) ->
    {Major2, Minor2, Patch2} = case ec_semver:parse(Vsn) of
        {{Major, Minor, Patch, _}, _} ->
            {Major, Minor, Patch};
        {{Major, Minor, Patch}, _} ->
            {Major, Minor, Patch};
        {{Major, Minor}, _} ->
            {Major, Minor, 0};
        {Major, _} when is_integer(Major) ->
            {Major, 0, 0};
        {_, _} ->
            {0, 0, 0}
    end,
    Major2 * 1_000_000 + Minor2 * 1_000 + Patch2.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
