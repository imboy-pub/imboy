-module(msg_store_sup_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

start_link_function_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(is_function(fun msg_store_sup:start_link/0, 0))
    end).

init_returns_expected_child_specs_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, {SupFlags, ChildSpecs}} = msg_store_sup:init([]),
        ?assertMatch(#{strategy := one_for_one, intensity := 10, period := 60}, SupFlags),
        ?assertEqual(2, length(ChildSpecs)),
        [MsgStoreDsSpec, MsgStoreWorkerSpec] = ChildSpecs,
        ?assertMatch(#{
            id := msg_store_ds,
            start := {msg_store_ds, start_link, []},
            type := worker
        }, MsgStoreDsSpec),
        ?assertMatch(#{
            id := msg_store_worker,
            start := {msg_store_worker, start_link, []},
            type := worker
        }, MsgStoreWorkerSpec)
    end).
