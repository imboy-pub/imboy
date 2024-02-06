-module (app_ddl_repo).
%%%
% app_ddl 相关操作都放到该模块，存储库模块
% app_ddl related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export ([add/1]).

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
    imboy_db:public_tablename(<<"app_ddl">>).

% app_ddl_repo:save(#{<<"ddl">> => <<"cn">>, <<"type">> => "ios", <<"package_name">> => <<>>, <<"app_name">> => <<>>, <<"vsn">> => "0.1.24", <<"download_url">> => <<>>, <<"description">> => <<>>, <<"app_db_vsn">> => 5, <<"force_update">> => 2, created_at => imboy_dt:utc(millisecond), <<"sign_key">> => <<"">>})
add(Data) ->
    Tb = tablename(),
    % Column = <<"(user_id, status, created_at)">>,
    % Value = [],
    % Column = [ binary_to_list(S) || S <- Data ],
    Column = <<"(", (imboy_cnv:implode(",", maps:keys(Data)))/binary, ")">>,
    Value = imboy_db:assemble_value(Data),
    imboy_db:insert_into(Tb, Column, Value).

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
