-module(oa_direct_cloud_retention_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-define(H, intbe02_http_support).

retention_test_() ->
    {timeout, 900,
        {setup, fun setup/0, fun close/1, fun(S) ->
            {inorder, [
                {"human_http_all_device_ack_keeps_cloud_body", fun() ->
                    ack_retention(S, <<"human">>)
                end},
                {"application_http_all_device_ack_keeps_cloud_body", fun() ->
                    ack_retention(S, <<"application">>)
                end},
                {"archive_fault_rolls_back_acceptance_and_idempotency_then_retries", fun() ->
                    archive_fault(S)
                end},
                {"caller_archive_replay_is_strict_and_cross_partition_safe", fun() -> replay(S) end},
                {"two_archive_waiters_keep_original_sequence_and_single_body", fun() ->
                    waiters(S)
                end},
                {"late_audit_failure_rolls_back_archive_and_retries", fun() -> audit_fault(S) end},
                {"compressed_body_and_runtime_jobs_preserve_authorized_http_read", fun() ->
                    compressed(S)
                end}
            ]}
        end}}.
setup() ->
    S = ?H:setup_all(),
    ok = elib_tsid:register(msg_store),
    #{port := 4337} = maps:get(server, S),
    C = maps:get(conn, S),
    PreviousSuper = application:get_env(imboy, super_account),
    PreviousPool = application:get_env(imboy, pg_conf),
    {ok, SourceOpts} = PreviousSuper,
    OwnOpts = SourceOpts#{database => maps:get(db, S)},
    application:set_env(imboy, super_account, OwnOpts),
    {ok, Pool} = PreviousPool,
    application:set_env(imboy, pg_conf, Pool#{start_mfa => {epgsql, connect, [OwnOpts]}}),
    ?assertEqual({ok, 169, false}, erlang_migrate:version(#{conn => C, dir => "priv/migrations"})),
    ?assertEqual(0, count(C, <<"config">>)),
    io:format(user, "OWN_MARKER ~s PORT4337 SCHEMA169 CONFIG0~n", [maps:get(db, S)]),
    PreviousArchive = application:get_env(imboy, msg_archive_enabled),
    application:set_env(imboy, msg_archive_enabled, false),
    lists:foreach(
        fun(D) ->
            ok = ?H:sql_exec(
                C,
                <<"INSERT INTO user_device(id,user_id,device_id,status,last_active_at) VALUES($1,995012,$2,1,NOW())">>,
                [elib_tsid:generate(user_device), D]
            )
        end,
        [<<"oa-cloud-device-a">>, <<"oa-cloud-device-b">>]
    ),
    ok = app_version_ds:set_sign_key(
        <<"synthetic">>,
        <<"oa-cloud">>,
        <<"synthetic.oa-cloud">>,
        <<"synthetic-oa-cloud-device-key">>
    ),
    S#{
        previous_jwt => application:get_env(imboy, jwt_key),
        previous_archive => PreviousArchive,
        previous_super => PreviousSuper,
        previous_pool => PreviousPool
    }.
close(S) ->
    restore_env(jwt_key, maps:get(previous_jwt, S)),
    restore_env(msg_archive_enabled, maps:get(previous_archive, S)),
    restore_env(super_account, maps:get(previous_super, S)),
    restore_env(pg_conf, maps:get(previous_pool, S)),
    ?H:teardown_all(S),
    inttest_marker_db:release(S).
body(Mode) ->
    B = #{
        <<"sender_mode">> => Mode,
        <<"recipient_user_id">> => <<"intbe02-ext-h2">>,
        <<"msg_type">> => <<"text">>,
        <<"content">> => <<"synthetic durable OA body">>
    },
    case Mode of
        <<"human">> -> B#{<<"sender_user_id">> => <<"intbe02-ext-h1">>};
        _ -> B
    end.
key() -> <<"oa-cloud-", (integer_to_binary(elib_tsid:generate(msg_store)))/binary>>.
send(S, Mode, Key) ->
    Headers = maps:merge(?H:auth(maps:get(cred_a, S)), ?H:idem(Key)),
    ?H:http(
        maps:get(port, S), <<"POST">>, <<"/api/internal/v1/messages/direct">>, body(Mode), Headers
    ).
accepted(R) ->
    ?assertEqual(200, maps:get(status, R)),
    maps:get(<<"msg_id">>, jsone:decode(maps:get(body, R))).
row(C, M) ->
    (?H:one(C, <<"SELECT *,payload::text AS payload FROM msg_c2c WHERE msg_id=$1">>, [M]))#{
        <<"type">> => <<"c2c">>
    }.
ack_retention(#{conn := C} = S, Mode) ->
    K = key(),
    R = send(S, Mode, K),
    M = accepted(R),
    Original = row(C, M),
    ok = msg_ack_logic:client_ack(<<"c2c">>, M, 995012, <<"oa-cloud-device-a">>),
    ?assertEqual(1, count_msg(C, <<"msg_c2c">>, M)),
    ok = msg_ack_logic:client_ack(<<"c2c">>, M, 995012, <<"oa-cloud-device-b">>),
    ?assertEqual(0, count_msg(C, <<"msg_c2c">>, M)),
    ?assertEqual(1, count_msg(C, <<"enterprise_message_origin">>, M)),
    ?assertEqual(1, count_msg(C, <<"msg_store">>, M)),
    [Archived] = rows(C, <<"SELECT * FROM msg_store WHERE msg_id=$1">>, [M]),
    ?assertEqual(maps:get(<<"conv_seq">>, Original), maps:get(<<"conv_seq">>, Archived)),
    ?assertEqual(
        jsone:decode(maps:get(<<"payload">>, Original)),
        jsone:decode(maps:get(<<"payload">>, Archived))
    ),
    ?assertEqual(null, maps:get(<<"e2ee">>, Archived)),
    ?assertEqual(maps:get(body, R), maps:get(body, send(S, Mode, K))),
    ?assertEqual(1, count_msg(C, <<"msg_store">>, M)),
    {ok, History} = messaging_logic:history(
        995012, <<"c2c">>, integer_to_binary(maps:get(<<"from_id">>, Original)), 0, 100
    ),
    ?assert(
        lists:any(fun(H) -> maps:get(<<"msg_id">>, H) =:= M end, maps:get(<<"messages">>, History))
    ),
    {ok, Other} = messaging_logic:history(
        995013, <<"c2c">>, integer_to_binary(maps:get(<<"from_id">>, Original)), 0, 100
    ),
    ?assertNot(
        lists:any(fun(H) -> maps:get(<<"msg_id">>, H) =:= M end, maps:get(<<"messages">>, Other))
    ).
archive_fault(#{conn := C} = S) ->
    K = key(),
    Before = snapshot(C),
    ok = ?H:sql_exec(
        C,
        <<"CREATE FUNCTION oa_cloud_fault() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic archive fault' USING ERRCODE='23505'; END $$">>
    ),
    ok = ?H:sql_exec(
        C,
        <<"CREATE TRIGGER oa_cloud_fault BEFORE INSERT ON msg_store FOR EACH ROW EXECUTE FUNCTION oa_cloud_fault()">>
    ),
    try
        R = send(S, <<"human">>, K),
        ?assertEqual(500, maps:get(status, R)),
        ?assertEqual(Before, snapshot(C))
    after
        ok = ?H:sql_exec(C, <<"DROP TRIGGER oa_cloud_fault ON msg_store">>),
        ok = ?H:sql_exec(C, <<"DROP FUNCTION oa_cloud_fault()">>)
    end,
    M = accepted(send(S, <<"human">>, K)),
    ?assertEqual(1, count_msg(C, <<"msg_store">>, M)).
replay(#{conn := C} = S) ->
    M = accepted(send(S, <<"human">>, key())),
    Row = row(C, M),
    ?assertEqual(ok, elib_pg:with_tx(fun(Tx) -> msg_archive_repo:archive_tx(Tx, Row) end)),
    Decoded = jsone:decode(maps:get(<<"payload">>, Row)),
    ?assertEqual(
        ok,
        elib_pg:with_tx(fun(Tx) ->
            msg_archive_repo:archive_tx(Tx, Row#{<<"payload">> => Decoded})
        end)
    ),
    ?assertMatch(
        {rollback, _},
        elib_pg:with_tx(fun(Tx) ->
            msg_archive_repo:archive_tx(Tx, Row#{<<"payload">> => <<"{\"content\":\"different\"}">>})
        end)
    ),
    ?assertMatch(
        {rollback, _},
        elib_pg:with_tx(fun(Tx) ->
            msg_archive_repo:archive_tx(Tx, Row#{<<"created_at">> => <<"2024-01-01T00:00:00Z">>})
        end)
    ),
    lists:foreach(
        fun({K, V}) ->
            ?assertMatch(
                {rollback, _},
                elib_pg:with_tx(fun(Tx) ->
                    msg_archive_repo:archive_tx(Tx, Row#{K => V})
                end)
            )
        end,
        [
            {<<"from_id">>, 995013},
            {<<"to_id">>, 995013},
            {<<"conv_seq">>, 999},
            {<<"server_ts">>, <<"2024-01-01T00:00:00Z">>},
            {<<"expire_at">>, <<"2027-01-01T00:00:00Z">>},
            {<<"sender_did">>, <<"different-device">>},
            {<<"e2ee">>, #{<<"ciphertext">> => <<"different">>}},
            {<<"msg_type">>, <<"file">>}
        ]
    ),
    ?assertEqual(1, count_msg(C, <<"msg_store">>, M)).
waiters(#{conn := C} = S) ->
    M = accepted(send(S, <<"human">>, key())),
    Row = row(C, M),
    Before = ?H:one(
        C,
        <<"SELECT seq FROM msg_store_seq WHERE conv_key=$1">>,
        [msg_archive_repo:conv_key(<<"c2c">>, 995011, 995012)]
    ),
    {ok, Opts} = application:get_env(imboy, super_account),
    {ok, Lock} = inttest_marker_db:safe_connect(Opts),
    Parent = self(),
    Ref = make_ref(),
    try
        ok = ?H:sql_exec(Lock, <<"BEGIN">>),
        #{<<"pid">> := Pid} = ?H:one(Lock, <<"SELECT pg_backend_pid() AS pid">>),
        _ = rows(Lock, <<"SELECT pg_advisory_xact_lock(hashtextextended('msg_c2c:'||$1,0))">>, [M]),
        lists:foreach(
            fun(_) ->
                spawn(fun() ->
                    Parent !
                        {Ref, elib_pg:with_tx(fun(Tx) -> msg_archive_repo:archive_tx(Tx, Row) end)}
                end)
            end,
            [1, 2]
        ),
        wait_two(C, Pid, 150),
        ok = ?H:sql_exec(Lock, <<"COMMIT">>),
        lists:foreach(
            fun(_) ->
                receive
                    {Ref, R} -> ?assertEqual(ok, R)
                after 3000 -> error(archive_waiter_timeout)
                end
            end,
            [1, 2]
        ),
        ?assertEqual(1, count_msg(C, <<"msg_store">>, M)),
        ?assertEqual(
            Before,
            ?H:one(
                C,
                <<"SELECT seq FROM msg_store_seq WHERE conv_key=$1">>,
                [msg_archive_repo:conv_key(<<"c2c">>, 995011, 995012)]
            )
        )
    after
        ?H:sql_exec(Lock, <<"ROLLBACK">>),
        epgsql:close(Lock)
    end.
wait_two(_, _, 0) ->
    error(two_real_archive_waiters_missing);
wait_two(C, Pid, N) ->
    #{<<"n">> := Count} = ?H:one(
        C,
        <<"SELECT count(*) AS n FROM pg_stat_activity WHERE datname=current_database() AND wait_event_type='Lock' AND $1=ANY(pg_blocking_pids(pid))">>,
        [Pid]
    ),
    case Count >= 2 of
        true ->
            ok;
        false ->
            timer:sleep(10),
            wait_two(C, Pid, N - 1)
    end.
audit_fault(#{conn := C} = S) ->
    K = key(),
    Before = snapshot(C),
    ok = ?H:sql_exec(
        C,
        <<"CREATE FUNCTION oa_audit_fault() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic late audit fault'; END $$">>
    ),
    ok = ?H:sql_exec(
        C,
        <<"CREATE TRIGGER oa_audit_fault BEFORE INSERT ON enterprise_audit_event FOR EACH ROW EXECUTE FUNCTION oa_audit_fault()">>
    ),
    try
        ?assertEqual(500, maps:get(status, send(S, <<"human">>, K))),
        ?assertEqual(Before, snapshot(C))
    after
        ok = ?H:sql_exec(C, <<"DROP TRIGGER oa_audit_fault ON enterprise_audit_event">>),
        ok = ?H:sql_exec(C, <<"DROP FUNCTION oa_audit_fault()">>)
    end,
    M = accepted(send(S, <<"human">>, K)),
    ?assertEqual(1, count_msg(C, <<"msg_store">>, M)).
compressed(#{conn := C} = S) ->
    R = send(S, <<"human">>, key()),
    M = accepted(R),
    ok = msg_ack_logic:client_ack(<<"c2c">>, M, 995012, <<"oa-cloud-device-a">>),
    ok = msg_ack_logic:client_ack(<<"c2c">>, M, 995012, <<"oa-cloud-device-b">>),
    ?assertEqual(0, count_msg(C, <<"msg_c2c">>, M)),
    ?assertMatch(
        [_ | _],
        rows(
            C,
            <<"SELECT job_id FROM timescaledb_information.jobs WHERE hypertable_name='msg_store' AND proc_name='policy_compression' AND (config->>'compress_after')::interval=INTERVAL '3 days'">>,
            []
        )
    ),
    ?assertEqual(
        [],
        rows(
            C,
            <<"SELECT job_id FROM timescaledb_information.jobs WHERE hypertable_name='msg_store' AND proc_name='policy_retention'">>,
            []
        )
    ),
    ?assertMatch(
        [_ | _],
        rows(
            C,
            <<"SELECT compress_chunk(c,if_not_compressed=>true) FROM show_chunks('msg_store') c">>,
            []
        )
    ),
    ?assertMatch(
        [_ | _],
        rows(
            C,
            <<"SELECT chunk_name FROM timescaledb_information.chunks WHERE hypertable_name='msg_store' AND is_compressed">>,
            []
        )
    ),
    H = history_http(S, 995012),
    ?assert(lists:any(fun(X) -> maps:get(<<"msg_id">>, X) =:= M end, H)),
    ?assertNot(lists:any(fun(X) -> maps:get(<<"msg_id">>, X) =:= M end, history_http(S, 995013))).
history_http(S, Uid) ->
    application:set_env(imboy, jwt_key, <<"synthetic-oa-cloud-jwt-only-key">>),
    Did = <<"oa-cloud-device-a">>,
    Token = token_ds:encrypt_token(Uid),
    Sign = elib_hasher:hmac_sha256(
        <<Did/binary, "|oa-cloud|synthetic|synthetic.oa-cloud">>,
        <<"synthetic-oa-cloud-device-key">>
    ),
    Headers = #{
        <<"authorization">> => <<"Bearer ", Token/binary>>,
        <<"did">> => Did,
        <<"vsn">> => <<"oa-cloud">>,
        <<"cos">> => <<"synthetic">>,
        <<"pkg">> => <<"synthetic.oa-cloud">>,
        <<"sign">> => Sign,
        <<"method">> => <<"sha256">>
    },
    R = ?H:http(
        maps:get(port, S),
        <<"GET">>,
        <<"/api/v1/msg/history?chat_type=c2c&peer_id=995011&after_seq=0&limit=100">>,
        <<>>,
        Headers
    ),
    ?assertEqual(200, maps:get(status, R)),
    #{<<"code">> := 0, <<"payload">> := #{<<"messages">> := Messages}} = jsone:decode(
        maps:get(body, R)
    ),
    Messages.
restore_env(K, {ok, V}) -> application:set_env(imboy, K, V);
restore_env(K, undefined) -> application:unset_env(imboy, K).
rows(C, Sql, P) ->
    {ok, R} = elib_pg:query(C, Sql, P),
    R.
count(C, T) -> maps:get(<<"n">>, ?H:one(C, [<<"SELECT count(*) AS n FROM ">>, T])).
count_msg(C, T, M) ->
    maps:get(
        <<"n">>, ?H:one(C, [<<"SELECT count(*) AS n FROM ">>, T, <<" WHERE msg_id=$1">>], [M])
    ).
snapshot(C) ->
    [
        maps:get(
            <<"v">>,
            ?H:one(C, [
                <<"SELECT COALESCE(jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text),'[]'::jsonb)::text AS v FROM ">>,
                T,
                <<" t">>
            ])
        )
     || T <-
            [
                <<"msg_c2c">>,
                <<"msg_store">>,
                <<"msg_store_seq">>,
                <<"enterprise_message_origin">>,
                <<"enterprise_audit_event">>,
                <<"enterprise_internal_idempotency">>
            ]
    ].
