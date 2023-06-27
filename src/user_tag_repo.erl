-module (user_tag_repo).
%%%
% user_tag 相关操作都放到该模块，存储库模块
% user_tag related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export ([demo/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"user_tag">>).

%%% demo方法描述
-spec demo(integer(), binary(), binary()) ->
    {ok, list(), list()} | {error, any()}.
demo(Uid, _Val1, _Val2) ->
    Sql = <<"SELECT id FROM user_tag WHERE id = $1">>,
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
