-module(auth_ds).
%%%
% auth 领域服务模块
% auth domain service 缩写
%%%

-export([get_token/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


%%% 获取资源服务访问token
% {imboy_dt:utc(second), auth_ds:get_token(assets, <<"dev">>, integer_to_list(imboy_dt:utc(second)))}.
% auth_ds:get_token(assets, <<"open">>, "/img/20225/25_21/ca73910gph0gio9q2pg0.png?1687988290").
get_token(assets, _Scene, Num) ->
    % TODO public key sign
    Key = config_ds:get(<<"upload_key">>),
    Num2 = ec_cnv:to_binary(Num),
    binary:part(imboy_hasher:md5(<<Key/binary, Num2/binary>>), {8, 16}).

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
