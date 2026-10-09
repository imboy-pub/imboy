-module(personal_order_delivery_pg_tests).
-include_lib("eunit/include/eunit.hrl").

delivery_order_test_() ->
    {timeout, 900,
        {setup, fun setup/0, fun close/1, fun(_) ->
            {inorder, [
                {Name, {timeout, 45, fun() -> run(F) end}}
             || {Name, F} <- [
                    {"all_formal_write_variants_preserve_staging_sequence", fun formal_variants/1},
                    {"direct_formal_acceptance_and_retry", fun direct_formal/1},
                    {"late_archive_preserves_accepted_order", fun late_archive/1},
                    {"claimed_null_archive_and_replay_keep_original_sequence", fun claimed_null/1},
                    {"ordered_duplicate_returns_original_sequence", fun ordered_duplicate/1},
                    {"archive_failure_keeps_staging_until_retry_commits", fun archive_retry/1},
                    {"formal_failure_rolls_back_counter_then_retry_succeeds",
                        fun formal_rollback/1},
                    {"cross_conversation_stage_formal_race_is_serialized",
                        fun cross_surface_race/1},
                    {"live_and_duplicate_ack_use_server_sequence", fun live_ack/1},
                    {"offline_db_and_envelope_keep_sequence_and_ciphertext", fun offline/1},
                    {"enterprise_direct_shared_c2c_origin_is_preserved", fun enterprise_direct/1},
                    {"legacy_unlocked_writer_collision_rolls_back_new_counter",
                        fun legacy_collision/1},
                    {"archive_false_23505_keeps_staging_then_retry", fun archive_unique_retry/1},
                    {"archive_replay_rejects_different_identity_and_content",
                        fun archive_replay_conflicts/1},
                    {"c2g_legacy_archive_replay_keeps_original_sequence", fun c2g_replay/1}
                ]
            ]}
        end}}.

setup() ->
    {ok, C} = eunit_runner:eunit_setup_with_db(),
    ?assertEqual({ok, 169, false}, erlang_migrate:version(#{conn => C, dir => "priv/migrations"})),
    C.
close(C) -> eunit_runner:eunit_cleanup_db(C).

run(F) ->
    A = elib_tsid:generate(),
    B = elib_tsid:generate(),
    C = elib_tsid:generate(),
    S = #{a => A, b => B, c => C, key => msg_archive_repo:conv_key(<<"c2c">>, A, B)},
    try
        F(S)
    after
        cleanup(S)
    end.

formal_variants(S) ->
    A = maps:get(a, S),
    B = maps:get(b, S),
    Ts = timestamp(),
    lists:foreach(
        fun({N, Writer}) ->
            M = msg(A, N),
            {ok, new} = stage(S, M),
            Seq = staged_seq(M),
            ok = Writer(Ts, M, cipher(), A, B, Ts, <<"text">>, metadata()),
            [R] = query(<<"SELECT conv_seq,payload,e2ee FROM public.msg_c2c WHERE msg_id=$1">>, [M]),
            ?assertEqual(Seq, maps:get(<<"conv_seq">>, R)),
            ?assertEqual(cipher(), maps:get(<<"payload">>, R)),
            ?assertEqual(metadata(), json_map(maps:get(<<"e2ee">>, R)))
        end,
        [
            {1, fun msg_c2c_repo:write_msg/8},
            {2, fun(T, M, P, F, To, ST, MT, E) ->
                msg_c2c_repo:write_msg_with_sender(
                    T, M, P, F, To, ST, MT, E, <<"synthetic-device">>
                )
            end},
            {3, fun(T, M, P, F, To, ST, MT, E) ->
                msg_c2c_repo:write_msg(T, M, P, F, To, ST, MT, E, <<"2026-12-01T00:00:00Z">>)
            end},
            {4, fun msg_c2c_repo:write_msg_if_absent/8},
            {5, fun(T, M, P, F, To, ST, MT, E) ->
                msg_c2c_repo:write_msg_if_absent_with_sender(
                    T, M, P, F, To, ST, MT, E, <<"synthetic-device">>
                )
            end},
            {6, fun(T, M, P, F, To, ST, MT, E) ->
                msg_c2c_repo:write_msg_with_reply(
                    T, M, P, F, To, ST, MT, E, <<"synthetic-reference">>, F, <<"synthetic snippet">>
                )
            end}
        ]
    ),
    ?assertEqual(6, head(S)).

direct_formal(S) ->
    A = maps:get(a, S),
    B = maps:get(b, S),
    M = msg(A, 10),
    T = timestamp(),
    ok = msg_c2c_repo:write_msg(T, M, cipher(), A, B, T, <<"text">>, metadata()),
    ?assertEqual(1, formal_seq(M)),
    ?assertEqual(
        {error, conflict_no_insert},
        msg_c2c_repo:write_msg(T, M, cipher(), A, B, T, <<"text">>, metadata())
    ),
    ?assertEqual(1, head(S)),
    ?assertEqual(
        {error, msg_id_conflict},
        msg_c2c_repo:write_msg(T, M, cipher(), A, maps:get(c, S), T, <<"text">>, metadata())
    ).

late_archive(S) ->
    A = maps:get(a, S),
    M1 = msg(A, 20),
    M2 = msg(A, 21),
    {ok, new} = stage(S, M1),
    {ok, new} = stage(S, M2),
    [R1] = staged(M1),
    [R2] = staged(M2),
    ok = msg_archive_repo:archive(R2),
    ok = msg_archive_repo:archive(R1),
    ?assertEqual(1, archive_seq(M1)),
    ?assertEqual(2, archive_seq(M2)),
    ?assertEqual(2, head(S)).

claimed_null(S) ->
    A = maps:get(a, S),
    M = msg(A, 30),
    {ok, new} = stage(S, M),
    [R] = staged(M),
    %% A worker may have claimed NULL before migration169 repaired the DB row.
    Claimed = R#{<<"conv_seq">> => null},
    ok = msg_archive_repo:archive(Claimed),
    ?assertEqual(1, archive_seq(M)),
    ?assertEqual(1, head(S)),
    ok = msg_archive_repo:archive(Claimed),
    ?assertEqual(1, head(S)),
    ?assertEqual(1, length(query(<<"SELECT id FROM public.msg_store WHERE msg_id=$1">>, [M]))).

ordered_duplicate(S) ->
    M = msg(maps:get(a, S), 40),
    ?assertEqual({ok, new, 1}, stage_ordered(S, M)),
    ?assertEqual({ok, duplicate, 1}, stage_ordered(S, M)),
    [R] = staged(M),
    ok = msg_archive_repo:archive(R),
    execute(<<"DELETE FROM public.msg_store_staging WHERE msg_id=$1">>, [M]),
    ?assertEqual({ok, duplicate, 1}, stage_ordered(S, M)),
    ?assertEqual(1, head(S)).

archive_retry(S) ->
    archive_retry_fault(S, <<"RAISE EXCEPTION 'synthetic archive rejection'">>).
archive_unique_retry(S) ->
    archive_retry_fault(
        S,
        <<"RAISE EXCEPTION 'synthetic archive unique' USING ERRCODE='23505', CONSTRAINT='uk_msg_store_msg_id_created_at'">>
    ).
archive_retry_fault(S, Fault) ->
    A = maps:get(a, S),
    M = msg(A, 50),
    Old = application:get_env(imboy, msg_archive_enabled),
    application:set_env(imboy, msg_archive_enabled, true),
    %% Real PG rejects archive INSERT; acceptance/formal persistence is real.
    execute(
        <<"CREATE FUNCTION public.personal_order_archive_fault() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN ",
            Fault/binary, "; END $$">>,
        []
    ),
    execute(
        <<
            "CREATE TRIGGER personal_order_archive_fault BEFORE INSERT ON public.msg_store\n"
            "        FOR EACH ROW EXECUTE FUNCTION public.personal_order_archive_fault()"
        >>,
        []
    ),
    try
        {ok, new} = stage(S, M),
        gen_statem:cast(msg_store_worker, kick),
        wait(
            fun() ->
                case staged(M) of
                    [R] -> maps:get(<<"retry_count">>, R) > 0;
                    _ -> false
                end
            end,
            200
        ),
        [Failed] = staged(M),
        ?assertEqual(null, maps:get(<<"processed_at">>, Failed)),
        ?assertEqual(1, maps:get(<<"conv_seq">>, Failed)),
        ?assertEqual(1, formal_seq(M)),
        ?assertEqual([], query(<<"SELECT id FROM public.msg_store WHERE msg_id=$1">>, [M])),
        execute(<<"DROP TRIGGER personal_order_archive_fault ON public.msg_store">>, []),
        execute(<<"UPDATE public.msg_store_staging SET available_at=now() WHERE msg_id=$1">>, [M]),
        gen_statem:cast(msg_store_worker, kick),
        wait(
            fun() ->
                case staged(M) of
                    [R] -> maps:get(<<"processed_at">>, R) =/= null;
                    _ -> false
                end
            end,
            200
        ),
        ?assertEqual(1, archive_seq(M)),
        ?assertEqual(1, head(S))
    after
        execute(<<"DROP TRIGGER IF EXISTS personal_order_archive_fault ON public.msg_store">>, []),
        execute(<<"DROP FUNCTION public.personal_order_archive_fault()">>, []),
        restore_env(msg_archive_enabled, Old)
    end.

archive_replay_conflicts(S) ->
    M = msg(maps:get(a, S), 56),
    {ok, new} = stage(S, M),
    execute(
        <<"UPDATE public.msg_store_staging SET payload=$2::jsonb WHERE msg_id=$1">>,
        [
            M,
            <<"{\"text\":\"synthetic\",\"origin\":{\"application_id\":999,\"kind\":\"enterprise_application\"}}">>
        ]
    ),
    [Row] = staged(M),
    ok = msg_archive_repo:archive(Row),
    Before = query(<<"SELECT * FROM public.msg_store WHERE msg_id=$1">>, [M]),
    ok = msg_archive_repo:archive(Row),
    %% JSON object member order differs; deep content including origin is equal.
    ok = msg_archive_repo:archive(Row#{
        <<"payload">> =>
            <<"{\"origin\":{\"kind\":\"enterprise_application\",\"application_id\":999},\"text\":\"synthetic\"}">>,
        <<"e2ee">> => <<"{\"synthetic_envelope\":\"unchanged\",\"meta_version\":2}">>
    }),
    Changes = [
        #{<<"from_id">> => maps:get(c, S)},
        #{<<"to_id">> => maps:get(c, S)},
        #{<<"conv_seq">> => 2},
        #{<<"payload">> => <<"{\"text\":\"different\"}">>},
        #{<<"payload">> => <<"{\"origin\":{\"application_id\":999}}">>},
        #{<<"e2ee">> => <<"{\"synthetic_envelope\":\"different\"}">>},
        #{<<"sender_did">> => <<"different-device">>}
    ],
    lists:foreach(
        fun(Change) ->
            ?assertMatch({error, _}, msg_archive_repo:archive(maps:merge(Row, Change))),
            ?assertEqual(Before, query(<<"SELECT * FROM public.msg_store WHERE msg_id=$1">>, [M])),
            [Pending] = staged(M),
            ?assertEqual(null, maps:get(<<"processed_at">>, Pending))
        end,
        Changes
    ),
    ok = msg_archive_repo:archive(Row).

c2g_replay(S) ->
    M = msg(maps:get(a, S), 57),
    G = maps:get(c, S),
    {ok, new} = stage(S, M),
    [R] = staged(M),
    GroupRow = R#{
        <<"type">> => <<"c2g">>,
        <<"conv_seq">> => null,
        <<"payload">> => jsone:encode(#{
            <<"to">> => integer_to_binary(G), <<"text">> => <<"synthetic group">>
        })
    },
    Key = msg_archive_repo:conv_key(<<"c2g">>, maps:get(a, S), G),
    try
        ok = msg_archive_repo:archive(GroupRow),
        Before = query(<<"SELECT * FROM public.msg_store WHERE msg_id=$1">>, [M]),
        ok = msg_archive_repo:archive(GroupRow),
        ?assertEqual(Before, query(<<"SELECT * FROM public.msg_store WHERE msg_id=$1">>, [M])),
        ?assertEqual(1, scalar(<<"SELECT seq FROM public.msg_store_seq WHERE conv_key=$1">>, [Key]))
    after
        execute(<<"DELETE FROM public.msg_store_seq WHERE conv_key=$1">>, [Key])
    end.

formal_rollback(S) ->
    A = maps:get(a, S),
    B = maps:get(b, S),
    T = timestamp(),
    ?assertMatch(
        {error, _},
        msg_c2c_repo:write_msg(
            T, binary:copy(<<"x">>, 41), cipher(), A, B, T, <<"text">>, metadata()
        )
    ),
    ?assertEqual(
        [], query(<<"SELECT seq FROM public.msg_store_seq WHERE conv_key=$1">>, [maps:get(key, S)])
    ),
    M = msg(A, 60),
    ok = msg_c2c_repo:write_msg(T, M, cipher(), A, B, T, <<"text">>, metadata()),
    ?assertEqual(1, formal_seq(M)),
    ?assertEqual(1, head(S)).

cross_surface_race(S) ->
    A = maps:get(a, S),
    C = maps:get(c, S),
    M = msg(A, 70),
    T = timestamp(),
    {ok, Opts} = application:get_env(imboy, super_account),
    {ok, Lock} = inttest_marker_db:safe_connect(Opts),
    Parent = self(),
    try
        {ok, _} = elib_pg:query(
            Lock, <<"SELECT pg_advisory_lock(hashtextextended('msg_c2c:'||$1,0))">>, [M]
        ),
        spawn(fun() -> Parent ! {stage_result, stage_ordered(S, M)} end),
        spawn(fun() ->
            Parent !
                {formal_result,
                    msg_c2c_repo:write_msg(T, M, cipher(), A, C, T, <<"text">>, metadata())}
        end),
        wait(
            fun() ->
                [R] = query(
                    <<
                        "SELECT count(*) AS n FROM pg_locks WHERE locktype='advisory'\n"
                        "            AND database=(SELECT oid FROM pg_database WHERE datname=current_database()) AND NOT granted"
                    >>,
                    []
                ),
                maps:get(<<"n">>, R) >= 2
            end,
            150
        ),
        io:format(user, "REAL_PG_STAGE_FORMAL_TWO_IDENTITY_WAITERS~n", []),
        {ok, _} = elib_pg:query(
            Lock, <<"SELECT pg_advisory_unlock(hashtextextended('msg_c2c:'||$1,0))">>, [M]
        ),
        Stage =
            receive
                {stage_result, X} -> X
            after 5000 -> error(stage_timeout)
            end,
        Formal =
            receive
                {formal_result, Y} -> Y
            after 5000 -> error(formal_timeout)
            end,
        ?assert(
            lists:member({Stage, Formal}, [
                {{ok, new, 1}, {error, msg_id_conflict}}, {{error, msg_id_conflict}, ok}
            ])
        ),
        ?assertEqual(
            1,
            length(
                query(
                    <<
                        "SELECT msg_id FROM public.msg_store_staging WHERE type='c2c' AND msg_id=$1\n"
                        "            UNION ALL SELECT msg_id FROM public.msg_c2c WHERE msg_id=$1"
                    >>,
                    [M]
                )
            )
        ),
        ?assertEqual([1], [
            maps:get(<<"seq">>, R)
         || R <- query(
                <<
                    "SELECT seq FROM public.msg_store_seq\n"
                    "            WHERE conv_key=ANY($1::text[])"
                >>,
                [[maps:get(key, S), msg_archive_repo:conv_key(<<"c2c">>, A, C)]]
            )
        ])
    after
        elib_pg:query(Lock, <<"SELECT pg_advisory_unlock_all()">>, []),
        epgsql:close(Lock)
    end.

live_ack(S) ->
    A = maps:get(a, S),
    B = maps:get(b, S),
    M = msg(A, 80),
    Parent = self(),
    execute(<<"INSERT INTO public.user_friend(id,from_user_id,to_user_id) VALUES($1,$2,$3)">>, [
        elib_tsid:generate(), A, B
    ]),
    Mocked = [
        elib_async,
        msg_store_ds,
        imboy_message_helper,
        push_notification_logic,
        ai_agent_reply,
        bot_ds,
        billing_meter
    ],
    lists:foreach(fun(Mod) -> ok = meck:new(Mod, [passthrough]) end, Mocked),
    try
        meck:expect(elib_async, async, fun(F) ->
            F(),
            self()
        end),
        meck:expect(elib_async, async_retry, fun(F, _, _) ->
            F(),
            ok
        end),
        meck:expect(msg_store_ds, enqueue, fun(_, _, _) -> ok end),
        meck:expect(imboy_message_helper, encode_and_send, fun(_, _, Frame, _) ->
            Parent ! {live_frame, Frame},
            ok
        end),
        meck:expect(push_notification_logic, maybe_push_for_c2c, fun(_, _, _, _) -> ok end),
        meck:expect(ai_agent_reply, maybe_dispatch, fun(_, _, _) -> ok end),
        meck:expect(bot_ds, is_bot, fun(_) -> false end),
        meck:expect(billing_meter, meter, fun(_, _) -> ok end),
        Data = #{
            <<"to">> => integer_to_binary(B),
            <<"payload">> => cipher(),
            <<"msg_type">> => <<"text">>,
            <<"created_at">> => elib_dt:millisecond(),
            <<"e2ee">> => metadata(),
            <<"conv_seq">> => 999999
        },
        ok = msg_c2c_logic:c2c(M, A, Data),
        Ack =
            receive
                {reply, V} -> V
            after 1000 -> error(no_ack)
            end,
        Frame =
            receive
                {live_frame, V2} -> V2
            after 1000 -> error(no_live)
            end,
        ?assertEqual(1, maps:get(<<"conv_seq">>, Ack)),
        ?assertEqual(1, maps:get(<<"conv_seq">>, Frame)),
        ?assertEqual(cipher(), maps:get(<<"payload">>, Frame)),
        ?assertEqual(metadata(), maps:get(<<"e2ee">>, Frame)),
        ok = msg_c2c_logic:c2c(M, A, Data),
        DuplicateAck =
            receive
                {reply, V3} -> V3
            after 1000 -> error(no_duplicate_ack)
            end,
        ?assertEqual(1, maps:get(<<"conv_seq">>, DuplicateAck)),
        ?assertEqual(1, head(S)),
        receive
            {live_frame, _} -> error(duplicate_live_dispatch)
        after 0 -> ok
        end
    after
        lists:foreach(fun meck:unload/1, lists:reverse(Mocked))
    end.

offline(S) ->
    A = maps:get(a, S),
    B = maps:get(b, S),
    M = msg(A, 90),
    T = timestamp(),
    {ok, new} = stage(S, M),
    ok = msg_c2c_repo:write_msg_with_sender(
        T, M, cipher(), A, B, T, <<"text">>, metadata(), <<"synthetic-device">>
    ),
    [Row] = msg_c2c_ds:read_msg_for_device(B, <<"synthetic-receiver">>, 10, undefined),
    ?assertEqual(1, maps:get(<<"conv_seq">>, Row)),
    Frame = message_ds:offline_envelope(<<"C2C">>, Row),
    ?assertEqual(1, maps:get(<<"conv_seq">>, Frame)),
    ?assertEqual(M, maps:get(<<"id">>, Frame)),
    ?assertEqual(cipher(), maps:get(<<"payload">>, Frame)),
    ?assertEqual(metadata(), maps:get(<<"e2ee">>, Frame)),
    ?assertEqual(<<"synthetic-device">>, maps:get(<<"sender_did">>, Frame)).

legacy_collision(S) ->
    A = maps:get(a, S),
    B = maps:get(b, S),
    C = maps:get(c, S),
    M = msg(A, 95),
    Gate = A rem 2147483647,
    {ok, Opts} = application:get_env(imboy, super_account),
    {ok, Lock} = inttest_marker_db:safe_connect(Opts),
    Parent = self(),
    Ref = make_ref(),
    try
        execute(
            iolist_to_binary([
                "CREATE FUNCTION public.personal_order_legacy_gate() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_advisory_xact_lock(169,",
                integer_to_binary(Gate),
                "); RETURN NEW; END $$"
            ]),
            []
        ),
        execute(
            iolist_to_binary([
                "CREATE TRIGGER personal_order_legacy_gate BEFORE INSERT ON public.msg_store_staging FOR EACH ROW WHEN (NEW.to_id=",
                integer_to_binary(B),
                ") EXECUTE FUNCTION public.personal_order_legacy_gate()"
            ]),
            []
        ),
        {ok, _} = elib_pg:query(Lock, <<"SELECT pg_advisory_lock(169,$1)">>, [Gate]),
        {Pid, Monitor} = spawn_monitor(fun() ->
            Parent !
                {Ref,
                    try
                        stage_ordered(S, M)
                    catch
                        Class:Reason -> {Class, Reason}
                    end}
        end),
        try
            wait(
                fun() ->
                    scalar(
                        <<"SELECT count(*) FROM pg_locks WHERE locktype='advisory' AND classid=169 AND objid=$1::oid AND NOT granted AND database=(SELECT oid FROM pg_database WHERE datname=current_database())">>,
                        [Gate]
                    ) =:= 1
                end,
                150
            ),
            execute(
                <<"INSERT INTO public.msg_store_staging (id,type,msg_id,msg_type,payload,from_id,to_id,conv_seq) VALUES ($1,'c2c',$2,'text','{}',$3,$4,1)">>,
                [elib_tsid:generate(), M, A, C]
            ),
            execute(<<"INSERT INTO public.msg_store_seq(conv_key,seq) VALUES ($1,1)">>, [
                msg_archive_repo:conv_key(<<"c2c">>, A, C)
            ]),
            {ok, _} = elib_pg:query(Lock, <<"SELECT pg_advisory_unlock(169,$1)">>, [Gate]),
            receive
                {Ref, Result} -> ?assertEqual({error, msg_id_conflict}, Result)
            after 5000 -> error(legacy_collision_timeout)
            end,
            ?assertEqual(
                [],
                query(<<"SELECT seq FROM public.msg_store_seq WHERE conv_key=$1">>, [
                    maps:get(key, S)
                ])
            ),
            ?assertEqual(1, length(staged(M))),
            ?assertEqual(C, maps:get(<<"to_id">>, hd(staged(M))))
        after
            elib_pg:query(Lock, <<"SELECT pg_advisory_unlock(169,$1)">>, [Gate]),
            receive
                {'DOWN', Monitor, process, Pid, _} -> ok
            after 5000 -> error(legacy_worker_timeout)
            end
        end
    after
        epgsql:close(Lock),
        execute(
            <<"DROP TRIGGER IF EXISTS personal_order_legacy_gate ON public.msg_store_staging">>, []
        ),
        execute(<<"DROP FUNCTION IF EXISTS public.personal_order_legacy_gate()">>, [])
    end.

%% OA direct transport shares msg_c2c; it is not evidence of personal scope.
enterprise_direct(S) ->
    A = maps:get(a, S),
    B = maps:get(b, S),
    M = msg(A, 90),
    Origin = #{
        <<"kind">> => <<"enterprise_application">>,
        <<"application_id">> => maps:get(c, S),
        <<"sender_kind">> => <<"human">>,
        <<"sender_user_id">> => A
    },
    Payload = #{<<"text">> => <<"synthetic enterprise direct">>, <<"origin">> => Origin},
    {ok, Opts} = application:get_env(imboy, super_account),
    {ok, Conn} = inttest_marker_db:safe_connect(Opts),
    try
        {ok, _} = enterprise_message_repo:insert_direct_tx(Conn, M, A, B, <<"text">>, Payload),
        Before = query(<<"SELECT id,payload,e2ee,conv_seq FROM public.msg_c2c WHERE msg_id=$1">>, [
            M
        ]),
        ?assertEqual(Payload, json_map(maps:get(<<"payload">>, hd(Before)))),
        ?assertEqual(null, maps:get(<<"e2ee">>, hd(Before))),
        ?assertEqual(1, maps:get(<<"conv_seq">>, hd(Before))),
        {ok, new, 2} = stage_ordered(S, msg(A, 91)),
        ?assertEqual(
            Before,
            query(<<"SELECT id,payload,e2ee,conv_seq FROM public.msg_c2c WHERE msg_id=$1">>, [M])
        )
    after
        epgsql:close(Conn)
    end.

stage(S, M) ->
    msg_store_ds:stage(
        <<"c2c">>,
        M,
        <<"text">>,
        <<>>,
        metadata(),
        cipher(),
        maps:get(a, S),
        maps:get(b, S),
        timestamp(),
        timestamp(),
        <<"synthetic-device">>
    ).
stage_ordered(S, M) ->
    msg_store_ds:stage_with_sequence(
        <<"c2c">>,
        M,
        <<"text">>,
        <<>>,
        metadata(),
        cipher(),
        maps:get(a, S),
        maps:get(b, S),
        timestamp(),
        timestamp(),
        <<"synthetic-device">>
    ).
staged(M) ->
    query(<<"SELECT * FROM public.msg_store_staging WHERE type='c2c' AND msg_id=$1">>, [M]).
staged_seq(M) ->
    [R] = staged(M),
    maps:get(<<"conv_seq">>, R).
formal_seq(M) -> scalar(<<"SELECT conv_seq FROM public.msg_c2c WHERE msg_id=$1">>, [M]).
archive_seq(M) ->
    scalar(<<"SELECT conv_seq FROM public.msg_store WHERE chat_type='c2c' AND msg_id=$1">>, [M]).
head(S) -> scalar(<<"SELECT seq FROM public.msg_store_seq WHERE conv_key=$1">>, [maps:get(key, S)]).
timestamp() -> elib_dt:now().
cipher() -> <<"synthetic-protected-ciphertext">>.
metadata() -> #{<<"meta_version">> => 2, <<"synthetic_envelope">> => <<"unchanged">>}.
msg(A, N) -> <<"pd", (integer_to_binary(A))/binary, "_", (integer_to_binary(N))/binary>>.
json_map(M) when is_map(M) -> M;
json_map(B) -> jsone:decode(B, [{object_format, map}]).
query(Sql, Args) ->
    {ok, R} = elib_pg:query(Sql, Args),
    R.
execute(Sql, Args) ->
    {ok, _} = elib_pg:execute(Sql, Args),
    ok.
scalar(Sql, Args) ->
    [R] = query(Sql, Args),
    [V] = maps:values(R),
    V.
wait(_, 0) ->
    error(oracle_timeout);
wait(F, N) ->
    case F() of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait(F, N - 1)
    end.
restore_env(K, undefined) -> application:unset_env(imboy, K);
restore_env(K, {ok, V}) -> application:set_env(imboy, K, V).
cleanup(S) ->
    A = maps:get(a, S),
    C = maps:get(c, S),
    lists:foreach(
        fun(T) -> execute(<<"DELETE FROM public.", T/binary, " WHERE from_id=$1">>, [A]) end,
        [<<"msg_store_staging">>, <<"msg_c2c">>, <<"msg_store">>]
    ),
    execute(
        <<"DELETE FROM public.msg_store_seq WHERE conv_key=ANY($1::text[])">>,
        [[maps:get(key, S), msg_archive_repo:conv_key(<<"c2c">>, A, C)]]
    ),
    execute(<<"DELETE FROM public.user_friend WHERE from_user_id=$1">>, [A]).
