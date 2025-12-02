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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取应用版本表的表名
%% @return 返回应用版本表的完整表名
-spec tablename() -> binary().
tablename() ->
    imboy_db:public_tablename(<<"app_version">>).

%% @doc 查询应用版本信息
%% @param Where SQL WHERE子句条件
%% @param Column 要查询的列名
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
%% @details 按sort和updated_at降序排序
-spec find(binary(), binary()) -> {ok, list(), list()} | {error, any()}.
find(Where, Column) ->
    Tb = tablename(),
    OrderBy = <<"sort desc, updated_at desc">>,
    imboy_db:find(Tb, Where, OrderBy, Column).

% app_version_repo:add(#{<<"type">> => "andriod", <<"package_name">> => <<>>, <<"app_name">> => <<>>, <<"vsn">> => "0.1.24", <<"download_url">> => <<>>, <<"description">> => <<>>, <<"force_update">> => 2, created_at => imboy_dt:now(), <<"sign_key">> => <<"">>})
% app_version_repo:add(#{<<"region_code">> => <<"cn">>, <<"type">> => "ios", <<"package_name">> => <<>>, <<"app_name">> => <<>>, <<"vsn">> => "0.1.24", <<"download_url">> => <<>>, <<"description">> => <<>>, <<"force_update">> => 2, created_at => imboy_dt:now(), <<"sign_key">> => <<"">>})
add(Data) ->
    Tb = tablename(),
    imboy_db:insert_into(Tb, Data).

%%% demo方法描述
-spec demo(integer(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
demo(Uid, _Val1, _Val2) ->
    Sql = <<"SELECT id FROM app_version WHERE id = $1">>,
    imboy_db:query(Sql, [Uid]).

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
