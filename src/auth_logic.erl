-module(auth_logic).
%%%
% auth 业务逻辑模块
% auth business logic module
%%%

-export ([verify_for_open/3]).
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

verify_for_assets(undefined, _Tk, _Name) ->
    <<"fail">>;
verify_for_assets(_Scene, undefined, _Name) ->
    <<"fail">>;
verify_for_assets(_Scene, _Tk, undefined) ->
    <<"fail">>;
verify_for_assets(Scene, Tk, Val) ->
    Now = imboy_dt:second(),
    % V = binary_to_integer(Val),
    {V, _} = string:to_integer(Val),
    Diff = 7200,
    lager:info(io_lib:format("V:~p ~p ~n", [V, Now < (V + Diff) ])),
    if
        is_integer(V) andalso Now < (V + Diff) ->
            NewTk = auth_ds:get_token(assets, Scene, binary_to_list(Val)),
            do_verify_for_assets(NewTk, Tk);
        true ->
            <<"fail">>
    end.

verify_for_open(undefined, _Tk, _Val) ->
    <<"fail">>;
verify_for_open(_Path, undefined, _Val) ->
    <<"fail">>;
verify_for_open(Path, Tk, Val) ->
    NewTk = auth_ds:get_token(assets, <<"open">>, binary_to_list(<<Path/binary, "?", Val/binary>>)),
    % lager:info(io_lib:format("auth_logic:verify_for_open/3 new ~p, Tk:~p;~n", [NewTk, Tk])),
    do_verify_for_assets(NewTk, Tk).

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
