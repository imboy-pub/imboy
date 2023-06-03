-module(auth_ds).
%%%
% auth 领域服务模块
% auth domain service 缩写
%%%

-export ([get_token/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% 获取资源服务访问token
% auth_ds:get_token(assets, <<"dev">>, "333").
get_token(assets, S, V) ->
    AuthKeys = imboy_func:env(auth_keys),
    Key = proplists:get_value(S, AuthKeys),
    binary:part(imboy_hasher:md5(Key ++ V), {8, 16}).

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
