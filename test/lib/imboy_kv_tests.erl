-module(imboy_kv_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

start_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, imboy_kv:start())
    end).

start_is_idempotent_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, imboy_kv:start()),
        ?assertEqual(ok, imboy_kv:start())
    end).

start_export_contract_test() ->
    {module, imboy_kv} = code:ensure_loaded(imboy_kv),
    ?assert(erlang:function_exported(imboy_kv, start, 0)).
