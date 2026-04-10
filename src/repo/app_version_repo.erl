-module (app_version_repo).
%%%
% app_version 相关操作都放到该模块，存储库模块
% app_version related operations are put in this module, repository module
% 应用版本数据仓库层，提供应用版本信息的基础数据库操作
%%%

-export ([tablename/0]).
-export ([find/2]).
-export ([add/1]).
-export ([delete_by_id/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取应用版本表的表名
%% @return 返回应用版本表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"app_version">>).

%% @doc 查询应用版本信息（使用参数化查询，防止SQL注入）
%% @param Type 客户端类型（如 "web", "ios", "android"）
%% @param RegionCode 区域代码（可选，如 "cn"）
%% @return map() 查询成功返回行数据，未找到时返回空map
%% @details 按sort和updated_at降序排序，只返回第一条记录
-spec find(binary(), binary()) -> map().
find(Type, RegionCode) ->
    {Sql, Params} = build_find_query(Type, RegionCode),
    execute_find_query(Sql, Params).

%% @private
%% @doc 构建查询语句和参数
-spec build_find_query(binary(), binary()) -> {binary(), [term()]}.
build_find_query(Type, <<>>) ->
    Sql = <<"SELECT region_code, type, package_name, app_name, vsn, download_url, description, "
            "force_update, min_supported_vsn, grayscale_percent, upgrade_type, changelog, file_size, file_hash "
            "FROM ", (tablename())/binary,
            " WHERE status = 1 AND type = $1 ORDER BY sort desc, updated_at desc limit 1">>,
    {Sql, [Type]};
build_find_query(Type, RegionCode) ->
    Sql = <<"SELECT region_code, type, package_name, app_name, vsn, download_url, description, "
            "force_update, min_supported_vsn, grayscale_percent, upgrade_type, changelog, file_size, file_hash "
            "FROM ", (tablename())/binary,
            " WHERE status = 1 AND region_code = $1 AND type = $2 ORDER BY sort desc, updated_at desc limit 1">>,
    {Sql, [RegionCode, Type]}.

%% @private
%% @doc 执行查询并返回结果
-spec execute_find_query(binary(), [term()]) -> map().
execute_find_query(Sql, Params) ->
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

% app_version_repo:add(#{<<"type">> => "andriod", <<"package_name">> => <<>>, <<"app_name">> => <<>>, <<"vsn">> => "0.1.24", <<"download_url">> => <<>>, <<"description">> => <<>>, <<"force_update">> => 2, created_at => elib_dt:now(), <<"sign_key">> => <<>>})
% app_version_repo:add(#{<<"region_code">> => <<"cn">>, <<"type">> => "ios", <<"package_name">> => <<>>, <<"app_name">> => <<>>, <<"vsn">> => "0.1.24", <<"download_url">> => <<>>, <<"description">> => <<>>, <<"force_update">> => 2, created_at => elib_dt:now(), <<"sign_key">> => <<>>})
%% @doc 添加应用版本记录
%% @param Data 包含版本信息的map
%% @returns {ok, any()} | {error, any()}
-spec add(map()) -> {ok, integer()} | {error, any()}.
add(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(app_version),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 根据 ID 删除应用版本记录（使用参数化查询，防止SQL注入）
%% @param Id 要删除的版本记录 ID（数字 ID）
%% @return {ok, Affected} | {error, Reason}
-spec delete_by_id(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_id(Id) when is_integer(Id), Id > 0 ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE id = $1">>,
    elib_pg:execute(Sql, [Id]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

