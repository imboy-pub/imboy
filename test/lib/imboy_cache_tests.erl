-module(imboy_cache_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

memo_function_forwards_to_depcache_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 2, fun(Function, Server) ->
            ?assertEqual(imboy_cache, Server),
            Function()
        end}
    ], fun() ->
        ?assertEqual(<<"cached_result">>, imboy_cache:memo(fun() -> <<"cached_result">> end))
    end).

memo_tuple_variant_uses_undefined_key_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 5, fun(Function, Key, MaxAge, Deps, Server) ->
            ?assertEqual(undefined, Key),
            ?assertEqual(1800, MaxAge),
            ?assertEqual([], Deps),
            ?assertEqual(imboy_cache, Server),
            {M, F, A} = Function,
            apply(M, F, A)
        end}
    ], fun() ->
        ?assertEqual("test", imboy_cache:memo({erlang, binary_to_list, [<<"test">>]}, 1800))
    end).

memo_with_key_uses_default_hour_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 5, fun(Function, Key, MaxAge, Deps, Server) ->
            ?assertEqual(<<"test_key">>, Key),
            ?assertEqual(3600, MaxAge),
            ?assertEqual([], Deps),
            ?assertEqual(imboy_cache, Server),
            Function()
        end}
    ], fun() ->
        ?assertEqual(<<"value">>, imboy_cache:memo(fun() -> <<"value">> end, <<"test_key">>))
    end).

memo_with_dependencies_forwards_all_arguments_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 5, fun(Function, Key, MaxAge, Deps, Server) ->
            ?assertEqual(<<"dep_key">>, Key),
            ?assertEqual(900, MaxAge),
            ?assertEqual([dep1, dep2], Deps),
            ?assertEqual(imboy_cache, Server),
            Function()
        end}
    ], fun() ->
        ?assertEqual(
            <<"dep_value">>,
            imboy_cache:memo(fun() -> <<"dep_value">> end, <<"dep_key">>, 900, [dep1, dep2])
        )
    end).

set_local_cache_key_skips_broadcast_test_() ->
    ?WITH_MECKS([
        {depcache, [
            {'set', 5, fun(Key, Value, TTL, Deps, Server) ->
                ?assertEqual({local_cache, <<"k">>}, Key),
                ?assertEqual(<<"v">>, Value),
                ?assertEqual(3600, TTL),
                ?assertEqual([], Deps),
                ?assertEqual(imboy_cache, Server),
                ok
            end}
        ]},
        {imboy_cache_sync, [
            {'broadcast', 1, fun(_Message) -> {ok, 1} end}
        ]}
    ], fun() ->
        ?assertEqual(ok, imboy_cache:set({local_cache, <<"k">>}, <<"v">>)),
        ?assertEqual(0, meck:num_calls(imboy_cache_sync, broadcast, 1))
    end).

set_broadcasts_when_dsync_enabled_test_() ->
    ?TEST_WITH_APP(fun() ->
        application:set_env(imboy, dsync_enabled, true),
        meck:new(depcache, [unstick, passthrough, no_link]),
        meck:new(imboy_cache_sync, [unstick, passthrough, no_link]),
        try
            meck:expect(depcache, set, 5, fun(<<"cache_key">>, <<"cache_value">>, 120, [], imboy_cache) -> ok end),
            meck:expect(imboy_cache_sync, broadcast, 1, fun({set, <<"cache_key">>, <<"cache_value">>, 120, []}) -> {ok, 1} end),

            ?assertEqual(ok, imboy_cache:set(<<"cache_key">>, <<"cache_value">>, 120)),
            ?assert(meck:called(imboy_cache_sync, broadcast, 1))
        after
            application:unset_env(imboy, dsync_enabled),
            meck:unload(imboy_cache_sync),
            meck:unload(depcache)
        end
    end).

get_variants_forward_to_depcache_test_() ->
    ?WITH_MECKS([
        {depcache, [
            {'get', 2, fun(<<"k">>, imboy_cache) -> {ok, <<"v">>} end},
            {'get', 3, fun(<<"k">>, <<"sub">>, imboy_cache) -> {ok, <<"sub_v">>} end},
            {'get_wait', 2, fun(<<"wait_k">>, imboy_cache) -> {ok, <<"wait_v">>} end},
            {'get_subkey', 3, fun(<<"k">>, <<"sub">>, imboy_cache) -> {ok, <<"subkey_v">>} end}
        ]}
    ], fun() ->
        ?assertEqual({ok, <<"v">>}, imboy_cache:get(<<"k">>)),
        ?assertEqual({ok, <<"sub_v">>}, imboy_cache:get(<<"k">>, <<"sub">>)),
        ?assertEqual({ok, <<"wait_v">>}, imboy_cache:get_wait(<<"wait_k">>)),
        ?assertEqual({ok, <<"subkey_v">>}, imboy_cache:get_subkey(<<"k">>, <<"sub">>))
    end).

flush_and_delete_forward_to_depcache_test_() ->
    ?TEST_WITH_APP(fun() ->
        application:set_env(imboy, dsync_enabled, true),
        meck:new(depcache, [unstick, passthrough, no_link]),
        meck:new(imboy_cache_sync, [unstick, passthrough, no_link]),
        try
            meck:expect(depcache, flush, 1, fun(imboy_cache) -> ok end),
            meck:expect(depcache, flush, 2, fun(Key, Server) ->
                ?assertEqual(imboy_cache, Server),
                ?assert(lists:member(Key, [<<"flush_key">>, <<"delete_key">>])),
                ok
            end),
            meck:expect(imboy_cache_sync, broadcast, 1, fun
                (flush) -> {ok, 1};
                ({flush, <<"flush_key">>}) -> {ok, 1};
                ({flush, <<"delete_key">>}) -> {ok, 1}
            end),

            ?assertEqual(ok, imboy_cache:flush()),
            ?assertEqual(ok, imboy_cache:flush(<<"flush_key">>)),
            ?assertEqual(ok, imboy_cache:delete(<<"delete_key">>)),
            ?assertEqual(3, meck:num_calls(imboy_cache_sync, broadcast, 1))
        after
            application:unset_env(imboy, dsync_enabled),
            meck:unload(imboy_cache_sync),
            meck:unload(depcache)
        end
    end).

start_link_extracts_memory_max_test_() ->
    ?WITH_MECK(depcache, [
        {'start_link', 2, fun(imboy_cache, #{memory_max := 2048, callback := {imboy_cache, record_depcache_event, [[{depcache_memory_max, 2048}]]}}) ->
            {ok, spawned}
        end}
    ], fun() ->
        ?assertEqual({ok, self()}, imboy_cache:start_link([{depcache_memory_max, 2048}]))
    end).

size_and_process_helpers_forward_test_() ->
    ?WITH_MECKS([
        {depcache, [
            {'size', 1, fun(imboy_cache) -> 42 end},
            {'in_process_server', 1, fun(imboy_cache) -> true;
                                      (custom_server) -> false end},
            {'in_process', 1, fun(true) -> true end},
            {'flush_process_dict', 0, fun() -> {ok, 3} end}
        ]}
    ], fun() ->
        ?assertEqual(42, imboy_cache:size()),
        ?assertEqual(true, imboy_cache:in_process_server()),
        ?assertEqual(false, imboy_cache:in_process_server(custom_server)),
        ?assertEqual(true, imboy_cache:in_process(true)),
        ?assertEqual({ok, 3}, imboy_cache:flush_process_dict())
    end).

record_depcache_event_returns_ok_test() ->
    ?assertEqual(ok, imboy_cache:record_depcache_event(#{event => test})).
