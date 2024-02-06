-module (app_version_repo).
%%%
% app_version 相关操作都放到该模块，存储库模块
% app_version related operations are put in this module, repository module
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
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"app_version">>).

find(Where, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary, " order by vsn desc limit 1">>,
    ?LOG(['Sql', Sql]),
    imboy_db:find(Sql).

% app_version_repo:add(#{<<"type">> => "andriod", <<"package_name">> => <<>>, <<"app_name">> => <<>>, <<"vsn">> => "0.1.24", <<"download_url">> => <<>>, <<"description">> => <<>>, <<"app_db_vsn">> => 5, <<"force_update">> => 2, created_at => imboy_dt:utc(millisecond), <<"sign_key">> => <<"">>})
% app_version_repo:add(#{<<"region_code">> => <<"cn">>, <<"type">> => "ios", <<"package_name">> => <<>>, <<"app_name">> => <<>>, <<"vsn">> => "0.1.24", <<"download_url">> => <<>>, <<"description">> => <<>>, <<"app_db_vsn">> => 5, <<"force_update">> => 2, created_at => imboy_dt:utc(millisecond), <<"sign_key">> => <<"">>})
add(Data) ->
    Tb = tablename(),
    % Column = <<"(user_id, status, created_at)">>,
    % Value = [],
    % Column = [ binary_to_list(S) || S <- Data ],
    Column = <<"(", (imboy_cnv:implode(",", maps:keys(Data)))/binary, ")">>,
    Value = imboy_db:assemble_value(Data),
    imboy_db:insert_into(Tb, Column, Value).

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
