-module(app_version_ds).
%%%
% app_version 领域服务模块
% app_version domain service 缩写
%%%

%% @doc 获取应用签名密钥
%% 根据客户端操作系统、版本号和包名获取签名密钥
%% @param ClientOS 客户端操作系统（android、ios、macos）
%% @param Vsn 版本号
%% @param Pkg 包名
%% @returns 签名密钥（二进制）
-export ([sign_key/3]).
-export ([set_sign_key/4]).
-export ([get_sign_key/4]).

%% 数据操作导出
-export([save/1]).
-export([delete/1]).
-export([delete_by_id/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

% app_version_ds:sign_key(<<"android">>, <<"1">>, <<"pub.imboy.apk">>).
% app_version_ds:sign_key(<<"ios">>, <<"1">>, <<"pub.imboy.2">>).
% app_version_ds:sign_key(<<"macos">>, <<"1">>, <<"pub.imboy.macos">>).
-spec sign_key(binary(), binary(), binary()) -> binary().
sign_key(ClientOS, Vsn, Pkg) when is_binary(ClientOS), is_binary(Vsn),is_binary(Pkg) ->
    %
    Key = <<Pkg/binary, "_", ClientOS/binary, "_", Vsn/binary>>,
    ok = ?DEBUG_LOG([sign_key, Key]),
    config_ds:get(Key, <<>>).

-spec set_sign_key(binary(), binary(), binary(), term()) -> ok.
set_sign_key(ClientOS, Vsn, Pkg, Val) when is_binary(ClientOS), is_binary(Vsn),is_binary(Pkg) ->
    Key = <<Pkg/binary, "_", ClientOS/binary, "_", Vsn/binary>>,
    config_ds:set(Key, Val).


-spec get_sign_key(binary(), binary(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_sign_key(ClientOS, Vsn, Pkg, Field) ->
    % 使用安全的参数化查询，避免SQL注入
    Where2 = <<"vsn = $1 AND package_name = $2 AND type = $3">>,
    % Defalut = config_ds:env(solidified_key),
    case elib_pg:one(<<"SELECT ", Field/binary, " FROM app_version WHERE ", Where2/binary>>, [Vsn, Pkg, ClientOS]) of
        {ok, Row} -> {ok, maps:get(Field, Row, undefined)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 保存或更新应用版本信息
%% @param Data 包含版本信息的数据映射
%% @return {ok, Result} | {error, Reason}
-spec save(map()) -> {ok, any()} | {error, any()}.
save(Data) ->
    Id = ec_cnv:to_integer(maps:get(<<"id">>, Data, 0)),
    if Id > 0 ->
            % 更新现有记录
            Tb = app_version_repo:tablename(),
            Data2 = maps:remove(<<"id">>, Data),
            elib_pg:update(Tb, Data2#{<<"updated_at">> => elib_dt:now()},
                         <<"id = $1">>, [Id]);
        true ->
            % 新建记录
            Data2 = maps:remove(<<"id">>, Data),
            app_version_repo:add(Data2#{<<"created_at">> => elib_dt:now()})
    end.

%% @doc 删除应用版本记录
%% @param Where 删除条件的 SQL WHERE 子句
%% @return ok
-spec delete(binary()) -> ok.
delete(Where) ->
    Tb = app_version_repo:tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", Where/binary>>,
    _ = elib_pg:execute(Sql, []),
    ok.

%% @doc 根据 ID 删除应用版本记录
%% @param Id 要删除的版本记录 ID（原始数字 ID，非 HashID）
%% @return {ok, Affected} | {error, Reason}
-spec delete_by_id(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_id(Id) when is_integer(Id), Id > 0 ->
    app_version_repo:delete_by_id(Id).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-
%% ===================================================================
%% EUnit tests.
%% ===================================================================

