-module(dnd_cache_closeout_tests).
-include_lib("eunit/include/eunit.hrl").

dnd_cache_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> {timeout, 10, ?_test(stale_rule_loader_is_retried())} end,
        fun(_) -> ?_test(authority_failure_is_not_cached()) end,
        fun(_) -> ?_test(unfenced_legacy_ghost_is_ignored()) end
    ]}.
setup() ->
    Saved = [
        {K, application:get_env(imboy, K)}
     || K <- [dsync_enabled, cluster_nodes, cache_fenced_namespaces]
    ],
    cache_fixture_helper:setup(Saved, fun() -> setup_components(Saved) end).
setup_components(Saved) ->
    application:set_env(imboy, dsync_enabled, false),
    application:set_env(imboy, cluster_nodes, []),
    application:set_env(imboy, cache_fenced_namespaces, [user_dnd_rule]),
    application:ensure_all_started(crypto),
    application:ensure_all_started(syn),
    syn:add_node_to_scopes([imboy_cache]),
    D = cache_fixture_helper:acquire(imboy_cache, fun() ->
        depcache:start_link(imboy_cache, #{memory_max => 64})
    end),
    M = cache_fixture_helper:acquire(elib_metric, fun elib_metric:start_link/0),
    S = cache_fixture_helper:acquire(imboy_cache_sync, fun imboy_cache_sync:start_link/0),
    cache_fixture_helper:invalidate([{user_dnd_rule, 11}, {user_dnd_rule, 12}, {user_dnd_rule, 13}]),
    cache_fixture_helper:mock(user_dnd_rule_repo, [no_link, passthrough]),
    {D, M, S, Saved}.
cleanup({D, M, S, Saved}) ->
    Actions =
        [fun() -> meck:unload(Mod) end || Mod <- [user_dnd_rule_repo]] ++
            [
                fun() ->
                    cache_fixture_helper:invalidate([
                        {user_dnd_rule, 11}, {user_dnd_rule, 12}, {user_dnd_rule, 13}
                    ])
                end
            ] ++
            [fun() -> cache_fixture_helper:release(P) end || P <- [S, M, D]],
    try
        cache_fixture_helper:cleanup(Actions)
    after
        cache_fixture_helper:restore_env(Saved)
    end.

rule(Start) -> #{<<"start_min">> => Start, <<"end_min">> => 1440, <<"status">> => 1}.
stale_rule_loader_is_retried() ->
    Parent = self(),
    Calls = atomics:new(1, []),
    meck:expect(user_dnd_rule_repo, find_by_uid, fun(11) ->
        case atomics:add_get(Calls, 1, 1) of
            1 ->
                Parent ! {old_captured, self()},
                receive
                    resume -> rule(1320)
                after 3000 -> error(loader_timeout)
                end;
            _ ->
                rule(480)
        end
    end),
    Reader = spawn(fun() -> Parent ! {read, self(), user_dnd_rule_ds:find_by_uid(11)} end),
    receive
        {old_captured, Reader} -> ok
    after 3000 -> error(loader_not_started)
    end,
    ok = imboy_cache:invalidate({user_dnd_rule, 11}),
    Reader ! resume,
    receive
        {read, Reader, Uids} -> ?assertEqual(rule(480), Uids)
    after 3000 -> error(loader_not_completed)
    end,
    ?assertEqual(rule(480), user_dnd_rule_ds:find_by_uid(11)),
    %% 共享 app actor 可使快照二次失效：每次读最多两次 fenced 尝试加权威兜底。
    ?assert(atomics:get(Calls, 1) >= 2),
    ?assert(atomics:get(Calls, 1) =< 6).
authority_failure_is_not_cached() ->
    meck:expect(user_dnd_rule_repo, find_by_uid, fun(12) -> #{} end),
    ?assertEqual(#{}, user_dnd_rule_ds:find_by_uid(12)),
    Key =
        {cache_v2, {user_dnd_rule, 12}, {
            imboy_cache_sync:node_boot(), imboy_cache_sync:generation({user_dnd_rule, 12})
        }},
    ?assertEqual(undefined, depcache:get(Key, imboy_cache)),
    meck:expect(user_dnd_rule_repo, find_by_uid, fun(12) -> rule(720) end),
    ?assertEqual(rule(720), user_dnd_rule_ds:find_by_uid(12)).

unfenced_legacy_ghost_is_ignored() ->
    application:set_env(imboy, cache_fenced_namespaces, []),
    depcache:set({user_dnd_rule, 13}, rule(1320), 3600, [], imboy_cache),
    meck:expect(user_dnd_rule_repo, find_by_uid, fun(13) -> rule(480) end),
    ?assertEqual(rule(480), user_dnd_rule_ds:find_by_uid(13)),
    ?assertEqual(undefined, depcache:get({user_dnd_rule, 13, uncached_v1}, imboy_cache)),
    ?assertEqual(rule(480), user_dnd_rule_ds:find_by_uid(13)),
    ?assertEqual(2, meck:num_calls(user_dnd_rule_repo, find_by_uid, 1)).
