-module(security_refill_closeout_tests).
-include_lib("eunit/include/eunit.hrl").

security_refill_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> {timeout, 10, ?_test(stale_loader_is_retried(auth))} end,
        fun(_) -> {timeout, 10, ?_test(stale_loader_is_retried(admin))} end,
        fun(_) -> {timeout, 10, ?_test(stale_loader_is_retried(device))} end,
        fun(_) -> {timeout, 10, ?_test(stale_loader_is_retried(group_mode))} end,
        fun(_) -> {timeout, 10, ?_test(stale_loader_is_retried(billing))} end,
        fun(_) -> ?_test(errors_do_not_reside(auth)) end,
        fun(_) -> ?_test(errors_do_not_reside(admin)) end,
        fun(_) -> ?_test(errors_do_not_reside(device)) end,
        fun(_) -> ?_test(errors_do_not_reside(group_mode)) end,
        fun(_) -> ?_test(errors_do_not_reside(billing)) end,
        fun(_) -> ?_test(legacy_group_mode_keeps_tuple_contract()) end
    ]}.
setup() ->
    Saved = [
        {K, application:get_env(imboy, K)}
     || K <- [dsync_enabled, cluster_nodes, cache_fenced_namespaces, adm_cookie_secret]
    ],
    cache_fixture_helper:setup(Saved, fun() -> setup_components(Saved) end).
setup_components(Saved) ->
    application:set_env(imboy, dsync_enabled, false),
    application:set_env(imboy, adm_cookie_secret, <<"fixture_cookie_key">>),
    application:set_env(imboy, cluster_nodes, []),
    application:set_env(imboy, cache_fenced_namespaces, [
        auth_session_epoch, adm_session_epoch, user_device_active, group_e2ee_mode, billing_meter
    ]),
    application:ensure_all_started(crypto),
    application:ensure_all_started(syn),
    syn:add_node_to_scopes([imboy_cache]),
    D = cache_fixture_helper:acquire(imboy_cache, fun() ->
        depcache:start_link(imboy_cache, #{memory_max => 64})
    end),
    M = cache_fixture_helper:acquire(elib_metric, fun elib_metric:start_link/0),
    S = cache_fixture_helper:acquire(imboy_cache_sync, fun imboy_cache_sync:start_link/0),
    cache_fixture_helper:invalidate([key(K) || K <- [auth, admin, device, group_mode, billing]]),
    [
        cache_fixture_helper:mock(Mod, [no_link, passthrough])
     || Mod <- [elib_pg, user_device_repo, group_repo, billing_subscription_ds, billing_usage_ds]
    ],
    meck:expect(billing_usage_ds, incr, fun(Id, _, _, _) ->
        put(last_meter_sub, Id),
        {ok, 1}
    end),
    {D, M, S, Saved}.
cleanup({D, M, S, Saved}) ->
    Actions =
        [
            fun() -> meck:unload(Mod) end
         || Mod <- [
                elib_pg, user_device_repo, group_repo, billing_subscription_ds, billing_usage_ds
            ]
        ] ++
            [
                fun() ->
                    cache_fixture_helper:invalidate([
                        key(K)
                     || K <- [auth, admin, device, group_mode, billing]
                    ])
                end
            ] ++
            [fun() -> cache_fixture_helper:release(P) end || P <- [S, M, D]],
    try
        cache_fixture_helper:cleanup(Actions)
    after
        cache_fixture_helper:restore_env(Saved)
    end.

key(auth) -> {auth_session_epoch, 11};
key(admin) -> {adm_session_epoch, 11};
key(device) -> {user_device_active, 11, <<"fixture_did">>};
key(group_mode) -> {group_e2ee_mode, 11};
key(billing) -> {billing_meter, active_sub, 0}.
read(auth) ->
    auth_session_ds:current_epoch(11);
read(admin) ->
    adm_session_ds:current_epoch(11);
read(device) ->
    user_device_logic:is_active(11, <<"fixture_did">>);
read(group_mode) ->
    group_ds:e2ee_mode(11);
read(billing) ->
    billing_meter:do_meter(<<"fixture_metric">>, 1),
    get(last_meter_sub).
expected(auth) -> {ok, 2};
expected(admin) -> {ok, 2};
expected(device) -> false;
expected(group_mode) -> {ok, 1};
expected(billing) -> 42.
reply(auth, old) -> {ok, [#{<<"epoch">> => 1}]};
reply(auth, new) -> {ok, [#{<<"epoch">> => 2}]};
reply(admin, V) -> reply(auth, V);
reply(device, old) -> {ok, true};
reply(device, new) -> {ok, false};
reply(group_mode, old) -> #{<<"e2ee_mode">> => 0};
reply(group_mode, new) -> #{<<"e2ee_mode">> => 1};
reply(billing, old) -> #{<<"id">> => 41};
reply(billing, new) -> #{<<"id">> => 42}.
install(auth, F) ->
    meck:expect(elib_pg, query, fun(_, [11]) -> F() end);
install(admin, F) ->
    install(auth, F);
install(device, F) ->
    meck:expect(user_device_repo, is_active, fun(11, _) -> F() end);
install(group_mode, F) ->
    meck:expect(group_repo, find_by_id, fun(11, _) -> F() end);
install(billing, F) ->
    meck:expect(billing_subscription_ds, find_active_by_tenant, fun(0) -> F() end).
stale_loader_is_retried(Kind) ->
    Parent = self(),
    Calls = atomics:new(1, []),
    install(Kind, fun() ->
        case atomics:add_get(Calls, 1, 1) of
            1 ->
                Parent ! {old_captured, self()},
                receive
                    resume -> reply(Kind, old)
                after 3000 -> error(loader_timeout)
                end;
            _ ->
                reply(Kind, new)
        end
    end),
    Reader = spawn(fun() -> Parent ! {read, self(), read(Kind)} end),
    receive
        {old_captured, Reader} -> ok
    after 3000 -> error(loader_not_started)
    end,
    ok = imboy_cache:invalidate(key(Kind)),
    Reader ! resume,
    receive
        {read, Reader, Value} -> ?assertEqual(expected(Kind), Value)
    after 3000 -> error(loader_not_completed)
    end,
    ?assertEqual(expected(Kind), read(Kind)),
    ?assert(atomics:get(Calls, 1) >= 2),
    case Kind of
        auth ->
            ?assert(auth_session_ds:revoked(11, 1));
        admin ->
            Sig = adm_session_ds:issue_with(<<"11">>, 1, erlang:system_time(second) + 60),
            ?assertEqual({error, revoked}, adm_session_ds:verify(<<"11">>, Sig));
        _ ->
            ok
    end.

errors_do_not_reside(Kind) ->
    Failure =
        case Kind of
            billing -> #{};
            _ -> {error, unavailable}
        end,
    install(Kind, fun() -> Failure end),
    read(Kind),
    K = key(Kind),
    Physical = {cache_v2, K, {imboy_cache_sync:node_boot(), imboy_cache_sync:generation(K)}},
    ?assertEqual(undefined, depcache:get(Physical, imboy_cache)),
    install(Kind, fun() -> reply(Kind, new) end),
    ?assertEqual(expected(Kind), read(Kind)),
    case Kind of
        auth -> ?assert(auth_session_ds:revoked(11, 1));
        _ -> ok
    end.

legacy_group_mode_keeps_tuple_contract() ->
    application:set_env(imboy, cache_fenced_namespaces, []),
    depcache:set({group_e2ee_mode, 11}, 1, 60, [], imboy_cache),
    meck:expect(group_repo, find_by_id, fun(_, _) -> error(authority_must_not_be_read) end),
    ?assertEqual({ok, 1}, group_ds:e2ee_mode(11)).
