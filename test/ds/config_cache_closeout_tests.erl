-module(config_cache_closeout_tests).
-include_lib("eunit/include/eunit.hrl").

config_cache_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(C) -> {timeout, 10, ?_test(stale_loader(C, false))} end,
        fun(C) -> {timeout, 10, ?_test(stale_loader(C, true))} end,
        fun(C) -> ?_test(write_failure_does_not_invalidate(C)) end,
        fun(C) -> ?_test(encryption_failure_does_not_write(C)) end,
        fun(_) -> ?_test(public_callers_do_not_swallow_write_failure()) end,
        fun(_) -> ?_test(legacy_retained_value_not_reused()) end,
        fun(C) -> {timeout, 10, ?_test(unfenced_loader_keeps_zero_ttl(C))} end
    ]}.

setup() ->
    Keys = [dsync_enabled, cluster_nodes, cache_fenced_namespaces, postgre_aes_key],
    Saved = [{K, application:get_env(imboy, K)} || K <- Keys],
    application:set_env(imboy, dsync_enabled, false),
    application:set_env(imboy, cluster_nodes, []),
    application:set_env(imboy, cache_fenced_namespaces, []),
    application:set_env(imboy, postgre_aes_key, <<"synthetic_test_key">>),
    application:ensure_all_started(crypto),
    application:ensure_all_started(syn),
    syn:add_node_to_scopes([imboy_cache]),
    {ok, D} = depcache:start_link(imboy_cache, #{memory_max => 64}),
    unlink(D),
    {ok, M} = elib_metric:start_link(),
    unlink(M),
    {ok, S} = imboy_cache_sync:start_link(),
    unlink(S),
    wait_ready(100),
    Tab = ets:new(config_authority_fixture, [public, set]),
    ets:insert(Tab, {value, <<"old">>}),
    meck:new(elib_pg, [no_link, passthrough]),
    meck:expect(elib_pg, pluck, fun(_, _, _, _) -> {ok, 0} end),
    meck:expect(elib_pg, query, fun(Sql, Params) -> query(Tab, Sql, Params) end),
    meck:expect(elib_pg, insert, fun(_, Data) ->
        ?assertMatch(<<"aes_cbc_", _/binary>>, maps:get(<<"value">>, Data)),
        {pending, Value} = get(config_pending),
        ets:insert(Tab, {value, Value}),
        {ok, 1}
    end),
    {Tab, D, M, S, Saved}.

cleanup({Tab, D, M, S, Saved}) ->
    try
        meck:unload(elib_pg)
    after
        ets:delete(Tab),
        [gen_server:stop(P) || P <- [S, M, D]],
        [
            case V of
                undefined -> application:unset_env(imboy, K);
                {ok, X} -> application:set_env(imboy, K, X)
            end
         || {K, V} <- Saved
        ]
    end.
wait_ready(0) ->
    error(sync_not_ready);
wait_ready(N) ->
    case imboy_cache_sync:status() of
        ready ->
            ok;
        _ ->
            timer:sleep(25),
            wait_ready(N - 1)
    end.

query(_Tab, <<"SELECT 'aes_cbc_'", _/binary>>, [Json, _]) ->
    put(config_pending, {pending, jsone:decode(Json)}),
    {ok, [#{<<"value">> => <<"aes_cbc_synthetic_cipher">>}]};
query(Tab, <<"SELECT decode", _/binary>>, _) ->
    [{value, Value}] = ets:lookup(Tab, value),
    case ets:take(Tab, block_reader) of
        [{block_reader, Parent}] ->
            Parent ! {old_captured, self()},
            receive
                resume -> ok
            after 3000 -> error(reader_timeout)
            end;
        [] ->
            ok
    end,
    {ok, [#{<<"value">> => jsone:encode(Value)}]}.

stale_loader({Tab, _, _, _, _}, Fenced) ->
    application:set_env(
        imboy,
        cache_fenced_namespaces,
        case Fenced of
            true -> [config5];
            false -> []
        end
    ),
    Key = <<"closeout_fixture">>,
    ets:insert(Tab, {block_reader, self()}),
    Parent = self(),
    Reader = spawn(fun() -> Parent ! {read, self(), config_ds:get(Key)} end),
    receive
        {old_captured, Reader} -> ok
    after 3000 -> error(loader_not_started)
    end,
    ok = config_ds:set(Key, <<"new">>),
    Reader ! resume,
    Expected =
        case Fenced of
            true -> <<"new">>;
            false -> <<"old">>
        end,
    receive
        {read, Reader, Result} -> ?assertEqual(Expected, Result)
    after 3000 -> error(loader_not_finished)
    end,
    ?assertEqual(<<"new">>, config_ds:get(Key)),
    case Fenced of
        false -> ?assertEqual(undefined, depcache:get({config5, Key}, imboy_cache));
        true -> ok
    end.

write_failure_does_not_invalidate({_Tab, _, _, _, _}) ->
    meck:expect(elib_pg, insert, fun(_, _) -> {error, unavailable} end),
    meck:new(imboy_cache, [no_link, passthrough]),
    try
        meck:expect(imboy_cache, invalidate, fun(_) -> error(failed_write_must_not_invalidate) end),
        ?assertError(
            {config_write_failed, authority_write_failed}, config_ds:set(<<"fixture">>, <<"new">>)
        ),
        ?assertEqual(0, meck:num_calls(imboy_cache, invalidate, 1))
    after
        meck:unload(imboy_cache)
    end.

encryption_failure_does_not_write({_Tab, _, _, _, _}) ->
    meck:expect(elib_pg, query, fun(_, _) -> {error, encryption_unavailable} end),
    ?assertError(
        {config_write_failed, authority_write_failed}, config_ds:set(<<"fixture">>, <<"new">>)
    ),
    ?assertEqual(0, meck:num_calls(elib_pg, insert, 2)),
    ?assertEqual(0, meck:num_calls(elib_pg, pluck, 4)).

public_callers_do_not_swallow_write_failure() ->
    meck:new(config_ds, [no_link, passthrough]),
    try
        meck:expect(config_ds, set, fun(_, _) ->
            error({config_write_failed, authority_write_failed})
        end),
        ?assertError(
            {config_write_failed, authority_write_failed},
            user_onboarding_logic:put_config(#{<<"enabled">> => false})
        ),
        ?assertError(
            {config_write_failed, authority_write_failed},
            ai_agent_kb_logic:put_config(#{<<"enabled">> => false})
        )
    after
        meck:unload(config_ds)
    end.

legacy_retained_value_not_reused() ->
    Key = <<"retained_fixture">>,
    ok = depcache:set({config5, Key}, <<"stale_legacy">>, 864000, [], imboy_cache),
    ?assertEqual(<<"old">>, config_ds:get(Key)).

%% 只证明入口已选非 fenced 的 loader 保持零 TTL；不签发生产热切换合同。
unfenced_loader_keeps_zero_ttl({Tab, _, _, _, _}) ->
    Key = <<"flag_sampling_fixture">>,
    ets:insert(Tab, {block_reader, self()}),
    Parent = self(),
    Reader = spawn(fun() -> Parent ! {read, self(), config_ds:get(Key)} end),
    receive
        {old_captured, Reader} -> ok
    after 3000 -> error(loader_not_started)
    end,
    application:set_env(imboy, cache_fenced_namespaces, [config5]),
    ets:insert(Tab, {value, <<"new">>}),
    Reader ! resume,
    receive
        {read, Reader, Value} -> ?assertEqual(<<"old">>, Value)
    after 3000 -> error(loader_not_finished)
    end,
    ?assertEqual(undefined, depcache:get({config5, Key, uncached_v1}, imboy_cache)),
    application:set_env(imboy, cache_fenced_namespaces, []),
    ?assertEqual(<<"new">>, config_ds:get(Key)).
