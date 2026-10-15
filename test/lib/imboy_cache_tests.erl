-module(imboy_cache_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

memo_function_forwards_to_depcache_test_() ->
    ?WITH_MECK(
        depcache,
        [
            {'memo', 5, fun(Function, Key, _MaxAge, _Deps, Server) ->
                ?assertEqual(imboy_cache, Server),
                ?assertEqual(<<"cached_result">>, Key),
                Function()
            end}
        ],
        fun() ->
            ?assertEqual(<<"cached_result">>, imboy_cache:memo(fun() -> <<"cached_result">> end))
        end
    ).

memo_tuple_variant_uses_actual_derived_key_test_() ->
    ?WITH_MECK(
        depcache,
        [
            {'memo', 5, fun(Function, Key, MaxAge, Deps, Server) ->
                ?assertEqual({erlang, binary_to_list, [<<"test">>]}, Key),
                ?assertEqual(1800, MaxAge),
                ?assertEqual([], Deps),
                ?assertEqual(imboy_cache, Server),
                Function()
            end}
        ],
        fun() ->
            ?assertEqual("test", imboy_cache:memo({erlang, binary_to_list, [<<"test">>]}, 1800))
        end
    ).

memo_with_key_uses_default_hour_test_() ->
    ?WITH_MECK(
        depcache,
        [
            {'memo', 5, fun(Function, Key, MaxAge, Deps, Server) ->
                ?assertEqual(<<"test_key">>, Key),
                ?assertEqual(3600, MaxAge),
                ?assertEqual([], Deps),
                ?assertEqual(imboy_cache, Server),
                Function()
            end}
        ],
        fun() ->
            ?assertEqual(<<"value">>, imboy_cache:memo(fun() -> <<"value">> end, <<"test_key">>))
        end
    ).

memo_with_dependencies_forwards_all_arguments_test_() ->
    ?WITH_MECK(
        depcache,
        [
            {'memo', 5, fun(Function, Key, MaxAge, Deps, Server) ->
                ?assertEqual(<<"dep_key">>, Key),
                ?assertEqual(900, MaxAge),
                ?assertEqual([dep1, dep2], Deps),
                ?assertEqual(imboy_cache, Server),
                Function()
            end}
        ],
        fun() ->
            ?assertEqual(
                <<"dep_value">>,
                imboy_cache:memo(fun() -> <<"dep_value">> end, <<"dep_key">>, 900, [dep1, dep2])
            )
        end
    ).

set_local_cache_key_skips_broadcast_test_() ->
    ?WITH_MECKS(
        [
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
        ],
        fun() ->
            ?assertEqual(ok, imboy_cache:set({local_cache, <<"k">>}, <<"v">>)),
            ?assertEqual(0, meck:num_calls(imboy_cache_sync, broadcast, 1))
        end
    ).

set_broadcasts_when_dsync_enabled_test_() ->
    ?TEST_WITH_APP(fun() ->
        application:set_env(imboy, dsync_enabled, true),
        meck:new(depcache, [unstick, passthrough, no_link]),
        meck:new(imboy_cache_sync, [unstick, passthrough, no_link]),
        try
            meck:expect(depcache, set, 5, fun(
                <<"cache_key">>, <<"cache_value">>, 120, [], imboy_cache
            ) ->
                ok
            end),
            meck:expect(imboy_cache_sync, broadcast, 1, fun(
                {set, <<"cache_key">>, <<"cache_value">>, 120, []}
            ) ->
                {ok, 1}
            end),

            ?assertEqual(ok, imboy_cache:set(<<"cache_key">>, <<"cache_value">>, 120)),
            ?assert(meck:called(imboy_cache_sync, broadcast, 1))
        after
            application:unset_env(imboy, dsync_enabled),
            meck:unload(imboy_cache_sync),
            meck:unload(depcache)
        end
    end).

get_variants_forward_to_depcache_test_() ->
    ?WITH_MECKS(
        [
            {depcache, [
                {'get', 2, fun(<<"k">>, imboy_cache) -> {ok, <<"v">>} end},
                {'get', 3, fun(<<"k">>, <<"sub">>, imboy_cache) -> {ok, <<"sub_v">>} end},
                {'get_wait', 2, fun(<<"wait_k">>, imboy_cache) -> {ok, <<"wait_v">>} end},
                {'get_subkey', 3, fun(<<"k">>, <<"sub">>, imboy_cache) -> {ok, <<"subkey_v">>} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, <<"v">>}, imboy_cache:get(<<"k">>)),
            ?assertEqual({ok, <<"sub_v">>}, imboy_cache:get(<<"k">>, <<"sub">>)),
            ?assertEqual({ok, <<"wait_v">>}, imboy_cache:get_wait(<<"wait_k">>)),
            ?assertEqual({ok, <<"subkey_v">>}, imboy_cache:get_subkey(<<"k">>, <<"sub">>))
        end
    ).

flush_and_delete_forward_to_depcache_test_() ->
    ?TEST_WITH_APP(fun() ->
        application:set_env(imboy, dsync_enabled, true),
        meck:new(depcache, [unstick, passthrough, no_link]),
        meck:new(imboy_cache_sync, [unstick, passthrough, no_link]),
        try
            meck:expect(depcache, flush, 1, fun(imboy_cache) -> ok end),
            meck:expect(depcache, flush, 2, fun
                (Key, imboy_cache) when Key =:= <<"flush_key">>; Key =:= <<"delete_key">> -> ok;
                (Key, Server) -> meck:passthrough([Key, Server])
            end),
            meck:expect(imboy_cache_sync, broadcast, 1, fun
                (flush) -> {ok, 1};
                ({flush, <<"flush_key">>}) -> {ok, 1};
                ({flush, <<"delete_key">>}) -> {ok, 1}
            end),

            ?assertEqual(ok, imboy_cache:flush()),
            ?assertEqual(ok, imboy_cache:flush(<<"flush_key">>)),
            ?assertEqual(ok, imboy_cache:delete(<<"delete_key">>)),
            ?assert(meck:called(depcache, flush, [<<"flush_key">>, imboy_cache])),
            ?assert(meck:called(depcache, flush, [<<"delete_key">>, imboy_cache])),
            ?assertEqual(3, meck:num_calls(imboy_cache_sync, broadcast, 1))
        after
            application:unset_env(imboy, dsync_enabled),
            meck:unload(imboy_cache_sync),
            meck:unload(depcache)
        end
    end).

start_link_extracts_memory_max_test_() ->
    ?WITH_MECK(
        depcache,
        [
            {'start_link', 2, fun(
                imboy_cache,
                #{
                    memory_max := 2048,
                    callback :=
                        {imboy_cache, record_depcache_event, [[{depcache_memory_max, 2048}]]}
                }
            ) ->
                {ok, spawned}
            end}
        ],
        fun() ->
            %% 契约（2026-08-29 修复）：start_link 必须把真实 depcache 实例
            %% pid 透传给 supervisor（此前错误地返回 {ok, self()}，sup 记
            %% child pid 失真导致实例被杀后永不重启）。
            ?assertEqual({ok, spawned}, imboy_cache:start_link([{depcache_memory_max, 2048}]))
        end
    ).

size_and_process_helpers_forward_test_() ->
    ?WITH_MECKS(
        [
            {depcache, [
                {'size', 1, fun(imboy_cache) -> 42 end},
                {'in_process_server', 1, fun
                    (imboy_cache) -> true;
                    (custom_server) -> false
                end},
                {'in_process', 1, fun(true) -> true end},
                {'flush_process_dict', 0, fun() -> {ok, 3} end}
            ]}
        ],
        fun() ->
            ?assertEqual(42, imboy_cache:size()),
            ?assertEqual(true, imboy_cache:in_process_server()),
            ?assertEqual(false, imboy_cache:in_process_server(custom_server)),
            ?assertEqual(true, imboy_cache:in_process(true)),
            ?assertEqual({ok, 3}, imboy_cache:flush_process_dict())
        end
    ).

record_depcache_event_returns_ok_test() ->
    ?assertEqual(ok, imboy_cache:record_depcache_event(#{event => test})).

credential_policy_is_enforced_on_real_publish_and_receive_test_() ->
    ?TEST_WITH_APP(fun() ->
        Old = application:get_env(imboy, dsync_enabled),
        Keys = [{fcm_access_token_dyn}, qianfan_access_token, {apns_jwt, synthetic}],
        Legacy = {credential_legacy_probe, native},
        application:set_env(imboy, dsync_enabled, true),
        ok = syn:join(imboy_cache, dsync_handler, self(), #{}),
        try
            [
                ?assertEqual(
                    {error, policy_denied},
                    imboy_cache:broadcast(
                        {set, K, <<"direct-synthetic-token">>, 60, []}
                    )
                )
             || K <- Keys
            ],
            [ok = imboy_cache:set(K, <<"local-synthetic-token">>, 60) || K <- Keys],
            ok = imboy_cache:set(Legacy, compatibility_value, 60),
            receive
                {cache_sync, {set, Legacy, compatibility_value, 60, []}} -> ok
            after 1000 -> error(missing_real_legacy_publish)
            end,
            receive
                {cache_sync, {set, K, _, _, _}} -> ?assertNot(lists:member(K, Keys))
            after 0 -> ok
            end,
            Sync = whereis(imboy_cache_sync),
            [Sync ! {cache_sync, {set, K, <<"foreign-synthetic-token">>, 60, []}} || K <- Keys],
            pong = gen_server:call(Sync, ping_ok, 5000),
            [?assertEqual({ok, <<"local-synthetic-token">>}, imboy_cache:get(K)) || K <- Keys],
            ?assertEqual({ok, compatibility_value}, imboy_cache:get(Legacy)),
            ?assertMatch({ok, _}, imboy_cache:broadcast({flush, Legacy})),
            receive
                {cache_sync, {flush, Legacy}} -> ok
            after 1000 -> error(missing_real_legacy_flush)
            end,
            pong = gen_server:call(Sync, ping_ok, 5000),
            ?assertEqual(undefined, imboy_cache:get(Legacy))
        after
            syn:leave(imboy_cache, dsync_handler, self()),
            [depcache:flush(K, imboy_cache) || K <- [Legacy | Keys]],
            case Old of
                undefined -> application:unset_env(imboy, dsync_enabled);
                {ok, V} -> application:set_env(imboy, dsync_enabled, V)
            end
        end
    end).

retired_delete_only_key_keeps_real_read_write_fence_test_() ->
    ?TEST_WITH_APP(fun() ->
        OldCfg = application:get_env(imboy, cache_replication),
        OldReg = application:get_env(imboy, cache_registry_overrides),
        Key = {retired_delete_only, native},
        application:set_env(imboy, cache_registry_overrides, #{
            retired_delete_only => {replicate, false}
        }),
        application:set_env(imboy, cache_replication, #{whitelist => [retired_delete_only]}),
        try
            wait_for_native_cache_ready(80),
            ok = imboy_cache:put_delete(Key),
            ?assertNot(ets:member(imboy_cache_meta, {rindex, Key})),
            [Before] = ets:lookup(imboy_cache_meta, {rev, Key}),
            application:unset_env(imboy, cache_registry_overrides),
            ?assertEqual(legacy, imboy_cache_policy:policy(Key)),
            ?assertEqual(
                {error, policy_denied},
                imboy_cache:broadcast(
                    {set, Key, direct_resurrection, 60, []}
                )
            ),
            Sync = whereis(imboy_cache_sync),
            Sync ! {cache_sync, {set, Key, resurrected_old_value, 60, []}},
            pong = gen_server:call(Sync, ping_ok, 5000),
            ?assertEqual(undefined, depcache:get(Key, imboy_cache)),
            ?assertEqual([Before], ets:lookup(imboy_cache_meta, {rev, Key})),
            ?assertError({unsupported_replicate_api, set}, imboy_cache:set(Key, bypass, 60)),
            ok = depcache:set(Key, raw_legacy_value, 60, [], imboy_cache),
            ?assertEqual(undefined, imboy_cache:get(Key))
        after
            depcache:flush(Key, imboy_cache),
            imboy_cache_sync_apply:delete_metadata({rev, Key}),
            [
                case V of
                    undefined -> application:unset_env(imboy, K);
                    {ok, C} -> application:set_env(imboy, K, C)
                end
             || {K, V} <-
                    [{cache_replication, OldCfg}, {cache_registry_overrides, OldReg}]
            ]
        end
    end).

wait_for_native_cache_ready(Attempts) ->
    case imboy_cache_sync:status() of
        ready ->
            ok;
        _ when Attempts > 0 ->
            timer:sleep(25),
            wait_for_native_cache_ready(Attempts - 1);
        _ ->
            error(cache_boot_not_ready)
    end.
