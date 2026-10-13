-module(channel_subscriber_cache_tests).
-include_lib("eunit/include/eunit.hrl").

subscriber_cache_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> {timeout, 10, ?_test(stale_subscriber_loader_is_retried())} end,
        fun(_) -> ?_test(authority_failure_is_not_cached()) end
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
    application:set_env(imboy, cache_fenced_namespaces, [channel_subs]),
    application:ensure_all_started(crypto),
    application:ensure_all_started(syn),
    syn:add_node_to_scopes([imboy_cache]),
    D = cache_fixture_helper:acquire(imboy_cache, fun() ->
        depcache:start_link(imboy_cache, #{memory_max => 64})
    end),
    M = cache_fixture_helper:acquire(elib_metric, fun elib_metric:start_link/0),
    S = cache_fixture_helper:acquire(imboy_cache_sync, fun imboy_cache_sync:start_link/0),
    cache_fixture_helper:invalidate([{channel_subs, 11}, {channel_subs, 12}]),
    cache_fixture_helper:mock(channel_subscription_repo, [no_link, passthrough]),
    {D, M, S, Saved}.
cleanup({D, M, S, Saved}) ->
    Actions =
        [fun() -> meck:unload(Mod) end || Mod <- [channel_subscription_repo]] ++
            [fun() -> cache_fixture_helper:invalidate([{channel_subs, 11}, {channel_subs, 12}]) end] ++
            [fun() -> cache_fixture_helper:release(P) end || P <- [S, M, D]],
    try
        cache_fixture_helper:cleanup(Actions)
    after
        cache_fixture_helper:restore_env(Saved)
    end.

rows(Uids) -> {ok, [#{<<"user_id">> => Uid} || Uid <- Uids]}.
stale_subscriber_loader_is_retried() ->
    Parent = self(),
    Calls = atomics:new(1, []),
    meck:expect(channel_subscription_repo, list_by_channel, fun(11) ->
        case atomics:add_get(Calls, 1, 1) of
            1 ->
                Parent ! {old_captured, self()},
                receive
                    resume -> rows([1001])
                after 3000 -> error(loader_timeout)
                end;
            _ ->
                rows([1002])
        end
    end),
    Reader = spawn(fun() -> Parent ! {read, self(), channel_ds:subscriber_uids(11)} end),
    receive
        {old_captured, Reader} -> ok
    after 3000 -> error(loader_not_started)
    end,
    ok = imboy_cache:invalidate({channel_subs, 11}),
    Reader ! resume,
    receive
        {read, Reader, Uids} -> ?assertEqual([1002], Uids)
    after 3000 -> error(loader_not_completed)
    end,
    ?assertEqual([1002], channel_ds:subscriber_uids(11)),
    ?assertEqual(2, atomics:get(Calls, 1)).
authority_failure_is_not_cached() ->
    meck:expect(channel_subscription_repo, list_by_channel, fun(12) -> {error, unavailable} end),
    ?assertEqual([], channel_ds:subscriber_uids(12)),
    Key =
        {cache_v2, {channel_subs, 12}, {
            imboy_cache_sync:node_boot(), imboy_cache_sync:generation({channel_subs, 12})
        }},
    ?assertEqual(undefined, depcache:get(Key, imboy_cache)),
    meck:expect(channel_subscription_repo, list_by_channel, fun(12) -> rows([1003]) end),
    ?assertEqual([1003], channel_ds:subscriber_uids(12)).
