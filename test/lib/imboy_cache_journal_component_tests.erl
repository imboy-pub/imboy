-module(imboy_cache_journal_component_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("imboy_cache_sync_state.hrl").

journal_captures_actor_origin_for_local_and_remote_writes_test_() ->
    ?TEST_WITH_APP(fun capture_actor_origins/0).

capture_actor_origins() ->
    OldCfg = application:get_env(imboy, cache_replication),
    OldRegistry = application:get_env(imboy, cache_registry_overrides),
    Peer = 'journal_component@synthetic',
    PeerBoot = <<9:128>>,
    Round = <<9:96>>,
    try
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{journal_component => {replicate, false}}
        ),
        application:set_env(imboy, cache_replication, #{whitelist => [journal_component]}),
        assert_actor_origins(Peer, PeerBoot, Round)
    after
        cleanup_fixture(Peer, PeerBoot),
        restore(cache_replication, OldCfg),
        restore(cache_registry_overrides, OldRegistry)
    end.

assert_actor_origins(Peer, PeerBoot, Round) ->
    ok = await_ready(100),
    #state{boot_id = Boot, origin_seq = Before} = sys:get_state(imboy_cache_sync),
    sys:replace_state(imboy_cache_sync, fun(S) ->
        registered = imboy_cache_sync_proto:register_incarnation(Peer, PeerBoot),
        S
    end),
    ets:insert(imboy_cache_meta, {{snapshot_sender, self()}, #{round => Round}}),
    ok = imboy_cache_sync_apply:journal_refresh(),
    Start = imboy_cache_sync_apply:journal_position(),
    Epoch = imboy_cache_sync_proto:epoch(),
    Local = {journal_component, local},
    ok = imboy_cache:put(Local, value, 120, replicate),
    ok = imboy_cache:put_delete(Local),
    Now = erlang:system_time(millisecond),
    send_remote(set, remote, value, Now + 60000, 1, Peer, PeerBoot),
    send_remote(set, expired, value, Now - 1, 2, Peer, PeerBoot),
    send_remote(delete, remote, undefined, Now + 60000, 3, Peer, PeerBoot),
    {ok, Entries} = imboy_cache_sync_apply:journal_after(Start),
    ?assertEqual(5, length(Entries)),
    ?assertEqual(
        [
            {node(), Boot, Before + 1},
            {node(), Boot, Before + 2},
            {Peer, PeerBoot, 1},
            {Peer, PeerBoot, 2},
            {Peer, PeerBoot, 3}
        ],
        [Origin || {_, _, _, Origin} <- Entries]
    ),
    ?assertEqual(lists:duplicate(5, Epoch), [E || {_, E, _, _} <- Entries]),
    ?assertMatch(
        {expired, {journal_component, expired}, undefined, _, _},
        element(3, lists:nth(4, Entries))
    ),
    ?assertEqual(undefined, depcache:get({journal_component, expired}, imboy_cache)).

cleanup_fixture(Peer, PeerBoot) ->
    ets:delete(imboy_cache_meta, {snapshot_sender, self()}),
    imboy_cache_sync_apply:journal_refresh(),
    sys:replace_state(imboy_cache_sync, fun(S) ->
        [
            begin
                Key = {journal_component, Name},
                depcache:flush(Key, imboy_cache),
                imboy_cache_sync_apply:delete_metadata({rev, Key}),
                imboy_cache_sync_apply:delete_metadata({rindex, Key})
            end
         || Name <- [local, remote, expired]
        ],
        [
            ets:delete(imboy_cache_meta, K)
         || K <- [
                {peer_incarnation, Peer},
                {peer_counter, Peer},
                {peer_epoch, Peer},
                {hash_divergent, Peer},
                {seq, {Peer, PeerBoot}},
                {covered_seq, {Peer, PeerBoot}}
            ]
        ],
        S
    end).

send_remote(Op, Name, Value, Exp, Seq, Peer, Boot) ->
    E = (imboy_cache_sync_proto:event(
        Op,
        {journal_component, Name},
        Value,
        Exp,
        {1000 + Seq, Peer},
        Seq,
        Boot
    ))#{
        origin_node => Peer
    },
    imboy_cache_sync ! {cache_sync, E},
    pong = gen_server:call(imboy_cache_sync, ping_ok).

await_ready(0) ->
    error(cache_not_ready);
await_ready(Left) ->
    case imboy_cache_sync:status() of
        ready ->
            ok;
        _ ->
            timer:sleep(50),
            await_ready(Left - 1)
    end.

restore(Key, undefined) -> application:unset_env(imboy, Key);
restore(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).
