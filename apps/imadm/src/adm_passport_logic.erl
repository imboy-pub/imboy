-module(adm_passport_logic).
%%%
% adm_passport 业务逻辑模块
% adm_passport business logic module
%%%

-export([demo/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%%% demo方法描述
-spec demo(Uid :: integer(), Val1 :: binary(), Val2 :: binary()) -> ok.
demo(Uid, Val1, Val2) ->
    adm_passport_repo:demo(Uid, Val1, Val2),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

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
