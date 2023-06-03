-module(auth_logic).
%%%
% auth 业务逻辑模块
% auth business logic module
%%%

-export ([verify_for_assets/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

verify_for_assets(undefined, _AuthTk, _Name) ->
    <<"fail">>;
verify_for_assets(_Scene, undefined, _Name) ->
    <<"fail">>;
verify_for_assets(_Scene, _AuthTk, undefined) ->
    <<"fail">>;
verify_for_assets(Scene, AuthTk, Val) ->
    Now = imboy_dt:second(),
    % V = binary_to_integer(Val),
    {V, _} = string:to_integer(Val),

    Diff = 7200,
    if
        is_integer(V) andalso Now < (V + Diff) ->
            NewTk = auth_ds:get_token(assets, Scene, binary_to_list(Val)),
            do_verify_for_assets(NewTk, AuthTk);
        true ->
            <<"fail">>
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-


%%% 执行验证码
do_verify_for_assets(NewTk, T) when NewTk == T ->
    <<"ok">>;
do_verify_for_assets(_, _) ->
    <<"fail">>.

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
