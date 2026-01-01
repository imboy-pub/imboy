-module (app_version_repo).
%%%
% app_version 相关操作都放到该模块，存储库模块
% app_version related operations are put in this module, repository module
% 应用版本数据仓库层，提供应用版本信息的基础数据库操作
%%%

-export ([tablename/0]).
-export ([find/2]).
-export ([add/1]).
-export ([demo/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
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
    imboy_pg_sql:public_tablename(<<"app_version">>).

%% @doc 查询应用版本信息（使用参数化查询，防止SQL注入）
%% @param Type 客户端类型（如 "web", "ios", "android"）
%% @param RegionCode 区域代码（可选，如 "cn"）
%% @return map() 查询成功返回行数据，未找到时返回空map
%% @details 按sort和updated_at降序排序，只返回第一条记录
-spec find(binary(), binary()) -> map().
find(Type, RegionCode) ->
    Tb = tablename(),
    Column = <<"region_code,type, package_name, app_name, vsn, download_url, description, force_update">>,
    OrderBy = <<"sort desc, updated_at desc">>,
    case RegionCode of
        <<>> ->
            Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
                    " WHERE status = 1 AND type = $1 ORDER BY ", OrderBy/binary, " limit 1">>,
            case imboy_pg:one(Sql, [Type]) of
                {ok, Row} -> Row;
                _ -> #{}
            end;
        _ ->
            Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
                    " WHERE status = 1 AND region_code = $1 AND type = $2 ORDER BY ", OrderBy/binary, " limit 1">>,
            case imboy_pg:one(Sql, [RegionCode, Type]) of
                {ok, Row} -> Row;
                _ -> #{}
            end
    end.

% app_version_repo:add(#{<<"type">> => "andriod", <<"package_name">> => <<>>, <<"app_name">> => <<>>, <<"vsn">> => "0.1.24", <<"download_url">> => <<>>, <<"description">> => <<>>, <<"force_update">> => 2, created_at => imboy_dt:now(), <<"sign_key">> => <<"">>})
% app_version_repo:add(#{<<"region_code">> => <<"cn">>, <<"type">> => "ios", <<"package_name">> => <<>>, <<"app_name">> => <<>>, <<"vsn">> => "0.1.24", <<"download_url">> => <<>>, <<"description">> => <<>>, <<"force_update">> => 2, created_at => imboy_dt:now(), <<"sign_key">> => <<"">>})
add(Data) ->
    Tb = tablename(),
    imboy_pg:insert(Tb, Data).

%%% demo方法描述
-spec demo(integer(), binary(), binary()) ->
    {ok, list(map())} | {error, any()}.
demo(Uid, _Val1, _Val2) ->
    Tb = tablename(),
    Sql = <<"SELECT id FROM ", Tb/binary, " WHERE id = $1">>,
    imboy_pg:query(Sql, [Uid]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
