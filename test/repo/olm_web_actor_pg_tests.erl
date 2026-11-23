-module(olm_web_actor_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/0]).

-define(ACTOR, 99003201).
-define(TARGET, 99003202).
-define(OUTSIDER, 99003203).
-define(ROW, 99003211).
-define(DEVICE, <<"olm-web-actor">>).
-define(PEER, <<"olm-native-peer">>).

%% Explicit runner in a fresh VM with a coordinator-owned disposable PostgreSQL fixture.
run() ->
    undefined = whereis(pgsql),
    {ok, _} = application:ensure_all_started(pooler),
    ok = application:set_env(imboy, sql_driver, pgsql),
    Fixture = inttest_marker_db:provision(#{env_prefix => <<"OLM_WEB_ACTOR_INTTEST">>}),
    try
        #{conn := Conn, db := DbName, server := Server} = Fixture,
        {ok, _} = pooler:new_pool(#{
            name => pgsql,
            init_count => 4,
            max_count => 4,
            start_mfa => {epgsql, connect, [Server#{database => DbName}]}
        }),
        try
            {ok, _} = imboy_cache:start_link([{depcache_memory_max, 8}]),
            ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
            seed(Conn),
            check(Conn)
        after
            ok = pooler:rm_pool(pgsql)
        end
    after
        inttest_marker_db:release(Fixture)
    end.

seed(Conn) ->
    {ok, 3} = epgsql:equery(
        Conn,
        <<
            "INSERT INTO public.\"user\"(id,account,password,reg_ip,reg_cosv) VALUES "
            "($1,'olm-web-actor','synthetic-disabled','127.0.0.1','synthetic'),"
            "($2,'olm-native-peer','synthetic-disabled','127.0.0.1','synthetic'),"
            "($3,'olm-unrelated','synthetic-disabled','127.0.0.1','synthetic')"
        >>,
        [?ACTOR, ?TARGET, ?OUTSIDER]
    ),
    {ok, 3} = epgsql:equery(
        Conn,
        <<
            "INSERT INTO public.user_device(id,user_id,device_id,device_type,status) VALUES "
            "($1,$2,$3,'web',1),(99003212,$4,$5,'android',1),"
            "(99003213,$6,'olm-unrelated','android',1)"
        >>,
        [?ROW, ?ACTOR, ?DEVICE, ?TARGET, ?PEER, ?OUTSIDER]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<
            "INSERT INTO public.\"group\"(id,owner_uid,creator_uid,title,type,join_limit,status,scope,e2ee_mode) "
            "VALUES (99003221,$1,$1,'olm fixture',2,3,1,'personal',0)"
        >>,
        [?ACTOR]
    ),
    {ok, 2} = epgsql:equery(
        Conn,
        <<
            "INSERT INTO public.group_member(id,group_id,user_id,role,status) VALUES "
            "(99003231,99003221,$1,1,1),(99003232,99003221,$2,1,1)"
        >>,
        [?ACTOR, ?TARGET]
    ).

check(Conn) ->
    {Public, Private} = crypto:generate_key(eddsa, ed25519),
    Ed = base64:encode(Public),
    Curve = base64:encode(crypto:strong_rand_bytes(32)),
    Signature = sign(Private, Curve),
    Fallback = base64:encode(crypto:strong_rand_bytes(32)),
    KeyId = <<"olm-fallback">>,
    FallbackCanonical =
        <<"device_id=", ?DEVICE/binary, "\nkey_base64=", Fallback/binary, "\nkey_id=", KeyId/binary,
            "\nuser_id=", (integer_to_binary(?ACTOR))/binary>>,
    Identity = fun(RowId) ->
        olm_identity_logic:report_web_identity(
            ?ACTOR, ?DEVICE, RowId, Ed, Curve, Signature, <<>>, <<"web">>
        )
    end,
    Prekeys = fun(RowId) ->
        olm_identity_logic:report_web_prekeys(?ACTOR, ?DEVICE, RowId, [{<<"otk">>, Fallback}], 100)
    end,
    FallbackReport = fun(RowId) ->
        olm_identity_logic:report_web_fallback(
            ?ACTOR, ?DEVICE, RowId, KeyId, Fallback, sign(Private, FallbackCanonical)
        )
    end,
    ?assertEqual({error, <<"device_revoked">>}, Identity(?ROW + 10)),
    ?assertEqual(ok, Identity(?ROW)),
    ?assertEqual({ok, 1}, Prekeys(?ROW)),
    ?assertEqual(ok, FallbackReport(?ROW)),
    ExistingMaterial = material_state(Conn),
    ?assertEqual(
        {error, <<"internal_error">>},
        olm_identity_logic:report_web_prekeys(
            ?ACTOR, ?DEVICE, ?ROW, [{<<"duplicate">>, Fallback}, {<<"duplicate">>, Fallback}], 100
        )
    ),
    ?assertEqual(ExistingMaterial, material_state(Conn)),
    ?assertEqual(
        ok,
        olm_identity_logic:report_identity(
            ?TARGET, ?PEER, Ed, Curve, Signature, <<"android">>
        )
    ),
    ?assertEqual(
        {ok, 2},
        olm_identity_logic:report_one_time_keys(
            ?TARGET, ?PEER, [{<<"peer-otk-1">>, Fallback}, {<<"peer-otk-2">>, Fallback}], 100
        )
    ),
    Limiter = fun(Uid) ->
        self() ! {budget, Uid},
        false
    end,
    ?assertEqual({error, <<"bad_request">>}, claim(?TARGET, <<>>, Limiter)),
    ?assertEqual(
        {error, <<"claim_not_authorized">>}, claim(?OUTSIDER, <<"olm-unrelated">>, Limiter)
    ),
    assert_no_budget(),
    {ok, Result} = claim(?TARGET, ?PEER, Limiter),
    ?assertMatch(#{<<"type">> := <<"one_time">>}, Result),
    receive
        {budget, ?TARGET} -> ok
    after 0 -> error(target_quota_not_counted)
    end,
    ?assertEqual({ok, Result}, claim(?TARGET, ?PEER, Limiter)),
    receive
        {budget, ?TARGET} -> ok
    after 0 -> error(retry_quota_not_counted)
    end,
    ?assertEqual({error, <<"rate_limited">>}, claim(?TARGET, ?PEER, fun(?TARGET) -> true end)),
    ?assertEqual({ok, 1}, olm_identity_repo:count_one_time_keys(?TARGET, ?PEER)),
    Before = material_state(Conn),
    revoke_while_blocked(Conn, [
        Identity,
        Prekeys,
        FallbackReport,
        fun(_) -> claim(?TARGET, ?PEER, fun(_) -> error(revoked_actor_consumed_quota) end) end
    ]),
    ?assertEqual(Before, material_state(Conn)),
    ?assertEqual({ok, 1}, olm_identity_repo:count_one_time_keys(?TARGET, ?PEER)),
    assert_no_budget().

claim(To, TargetDid, Limiter) ->
    olm_identity_logic:claim_web_key(
        ?ACTOR, ?DEVICE, ?ROW, To, TargetDid, <<"olm-web-test">>, Limiter
    ).

assert_no_budget() ->
    receive
        {budget, Uid} -> error({invalid_target_quota, Uid})
    after 0 -> ok
    end.

revoke_while_blocked(Conn, Operations) ->
    {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
    try
        {ok, _, [{Blocker}]} = epgsql:equery(Conn, <<"SELECT pg_backend_pid()">>, []),
        {ok, _, [{?ROW}]} = epgsql:equery(
            Conn, <<"SELECT id FROM public.user_device WHERE id=$1 FOR UPDATE">>, [?ROW]
        ),
        Jobs = [
            begin
                Parent = self(),
                Ref = make_ref(),
                Pid = spawn(fun() -> Parent ! {Ref, Operation(?ROW)} end),
                {Ref, Pid}
            end
         || Operation <- Operations
        ],
        try
            ok = wait_for_waiters(Conn, Blocker, length(Jobs), 200),
            {ok, 1} = epgsql:equery(
                Conn, <<"UPDATE public.user_device SET status=0 WHERE id=$1">>, [?ROW]
            ),
            {ok, [], []} = epgsql:squery(Conn, "COMMIT"),
            lists:foreach(
                fun({Ref, _}) ->
                    receive
                        {Ref, Result} -> ?assertEqual({error, <<"device_revoked">>}, Result)
                    after 5000 -> error(blocked_request_not_released)
                    end
                end,
                Jobs
            )
        after
            [exit(Pid, kill) || {_, Pid} <- Jobs, is_process_alive(Pid)]
        end
    after
        try
            epgsql:squery(Conn, "ROLLBACK")
        catch
            _:_ -> ok
        end
    end.

wait_for_waiters(_, _, _, 0) ->
    error(expected_postgres_row_lock_wait);
wait_for_waiters(Conn, Blocker, Expected, Attempts) ->
    {ok, _, _} = epgsql:squery(Conn, "SELECT pg_stat_clear_snapshot()"),
    {ok, _, [{Count}]} = epgsql:equery(
        Conn,
        <<
            "SELECT count(*) FROM pg_stat_activity a WHERE a.datname=current_database() "
            "AND a.wait_event_type='Lock' AND a.pid <> $1"
        >>,
        [Blocker]
    ),
    case Count >= Expected of
        true ->
            ok;
        false ->
            timer:sleep(25),
            wait_for_waiters(Conn, Blocker, Expected, Attempts - 1)
    end.

material_state(Conn) ->
    [
        begin
            {ok, _, Rows} = epgsql:equery(
                Conn,
                <<"SELECT * FROM public.", Table/binary, " WHERE user_id=$1 ORDER BY id">>,
                [?ACTOR]
            ),
            Rows
        end
     || Table <- [<<"olm_identity">>, <<"olm_one_time_key">>, <<"olm_fallback_key">>]
    ].

sign(Private, Value) ->
    base64:encode(crypto:sign(eddsa, none, Value, [Private, ed25519])).
