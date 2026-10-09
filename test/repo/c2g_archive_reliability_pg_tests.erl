-module(c2g_archive_reliability_pg_tests).
-include_lib("eunit/include/eunit.hrl").

archive_reliability_test_() ->
    {timeout, 600,
        {setup, fun setup/0, fun eunit_runner:eunit_cleanup_db/1, fun(_) ->
            {inorder, [
                {Name, {timeout, 45, fun() -> run(F) end}}
             || {Name, F} <- [
                    {"legacy_null_replay_keeps_original_sequence", fun legacy_replay/1},
                    {"archive_only_replay_repairs_missing_counter_without_renumbering",
                        fun replay_counter_repair/1},
                    {"claimed_null_resolves_committed_recipient_sequence", fun claimed_null/1},
                    {"same_message_deep_replay_and_conflicts", fun replay_conflicts/1},
                    {"false_23505_rolls_back_legacy_sequence", fun false_unique/1},
                    {"worker_false_unique_retains_staging_then_retry", fun worker_retry/1},
                    {"same_group_concurrent_null_replay_serializes", fun concurrent_same/1},
                    {"different_group_same_id_conflict_serializes", fun concurrent_identity/1}
                ]
            ]}
        end}}.
setup() ->
    {ok, C} = eunit_runner:eunit_setup_with_db(),
    ?assertEqual({ok, 169, false}, erlang_migrate:version(#{conn => C, dir => "priv/migrations"})),
    C.
run(F) ->
    S = #{
        a => elib_tsid:generate(),
        b => elib_tsid:generate(),
        g => elib_tsid:generate(),
        h => elib_tsid:generate()
    },
    try
        F(S)
    after
        cleanup(S)
    end.
row(S, N, Seq) ->
    T = elib_dt:now(),
    #{
        <<"type">> => <<"c2g">>,
        <<"msg_id">> => id(S, N),
        <<"msg_type">> => <<"text">>,
        <<"from_id">> => maps:get(a, S),
        <<"to_id">> => maps:get(g, S),
        <<"payload">> => payload(maps:get(g, S)),
        <<"e2ee">> => <<"{\"meta_version\":2,\"synthetic\":true}">>,
        <<"sender_did">> => <<"synthetic-device">>,
        <<"created_at">> => T,
        <<"server_ts">> => T,
        <<"conv_seq">> => Seq
    }.
payload(G) ->
    jsone:encode(#{
        <<"to">> => integer_to_binary(G),
        <<"payload">> => #{
            <<"text">> => <<"synthetic">>,
            <<"origin">> => #{<<"application_id">> => 999, <<"actor_user_id">> => 123}
        }
    }).
id(S, N) ->
    <<"cg", (integer_to_binary(maps:get(a, S)))/binary, "_", (integer_to_binary(N))/binary>>.
key(S) -> msg_archive_repo:conv_key(<<"c2g">>, 0, maps:get(g, S)).
legacy_replay(S) ->
    R = row(S, 1, null),
    ok = msg_archive_repo:archive(R),
    Before = archives(R),
    ok = msg_archive_repo:archive(R),
    ?assertEqual(Before, archives(R)),
    ?assertEqual(1, head(S)).
replay_counter_repair(S) ->
    R = row(S, 8, 5),
    stage(S, R),
    ok = msg_archive_repo:archive(R),
    Before = archives(R),
    execute(<<"DELETE FROM msg_store_seq WHERE conv_key=$1">>, [key(S)]),
    ok = msg_archive_repo:archive(R#{<<"conv_seq">> => null}),
    ?assertEqual(5, head(S)),
    execute(<<"DELETE FROM msg_store_staging WHERE msg_id=$1">>, [maps:get(<<"msg_id">>, R)]),
    execute(<<"DELETE FROM msg_c2g_recipient_snapshot WHERE msg_id=$1">>, [
        maps:get(<<"msg_id">>, R)
    ]),
    execute(<<"DELETE FROM msg_store_seq WHERE conv_key=$1">>, [key(S)]),
    ok = msg_archive_repo:archive(R#{<<"conv_seq">> => null}),
    ?assertEqual(5, head(S)),
    ?assertEqual(Before, archives(R)).

claimed_null(S) ->
    R = row(S, 2, 5),
    stage(S, R),
    ok = msg_archive_repo:archive(R#{<<"conv_seq">> => null}),
    ?assertEqual(5, maps:get(<<"conv_seq">>, hd(archives(R)))),
    ?assertEqual(5, head(S)),
    ok = msg_archive_repo:archive(R#{<<"conv_seq">> => null}),
    ?assertEqual(5, head(S)),
    execute(<<"UPDATE msg_c2g_recipient_snapshot SET conv_seq=6 WHERE msg_id=$1">>, [
        maps:get(<<"msg_id">>, R)
    ]),
    ?assertMatch({error, _}, msg_archive_repo:archive(R#{<<"conv_seq">> => null})),
    ?assertEqual(5, head(S)).
replay_conflicts(S) ->
    R = row(S, 3, 1),
    stage(S, R),
    ok = msg_archive_repo:archive(R),
    Before = archives(R),
    ok = msg_archive_repo:archive(R#{<<"e2ee">> => <<"{\"synthetic\":true,\"meta_version\":2}">>}),
    Changes = [
        #{<<"from_id">> => maps:get(b, S)},
        #{<<"conv_seq">> => 2},
        #{<<"payload">> => payload(maps:get(h, S))},
        #{<<"payload">> => <<"{\"to\":\"1\",\"payload\":{\"origin\":{\"application_id\":1000}}}">>},
        #{<<"sender_did">> => <<"other">>},
        #{<<"e2ee">> => null},
        #{<<"created_at">> => <<"2025-01-01T00:00:00Z">>}
    ],
    lists:foreach(
        fun(C) ->
            ?assertMatch({error, _}, msg_archive_repo:archive(maps:merge(R, C))),
            ?assertEqual(Before, archives(R))
        end,
        Changes
    ),
    ?assertEqual(1, head(S)).
false_unique(S) ->
    R = row(S, 4, null),
    fault(),
    try
        ?assertMatch({error, _}, msg_archive_repo:archive(R)),
        ?assertEqual([], archives(R)),
        ?assertEqual([], query(<<"SELECT seq FROM msg_store_seq WHERE conv_key=$1">>, [key(S)]))
    after
        unfault()
    end,
    ok = msg_archive_repo:archive(R),
    ?assertEqual(1, head(S)).
worker_retry(S) ->
    R = row(S, 5, 1),
    stage(S, R),
    Old = application:get_env(imboy, msg_archive_enabled),
    application:set_env(imboy, msg_archive_enabled, true),
    fault(),
    try
        gen_statem:cast(msg_store_worker, kick),
        wait(
            fun() ->
                case staged(R) of
                    [X] -> maps:get(<<"retry_count">>, X) > 0;
                    _ -> false
                end
            end,
            200
        ),
        [Pending] = staged(R),
        ?assertEqual(null, maps:get(<<"processed_at">>, Pending)),
        ?assertEqual([], archives(R)),
        ?assertEqual(1, head(S)),
        ?assertEqual(
            1,
            length(query(<<"SELECT id FROM msg_c2g WHERE msg_id=$1">>, [maps:get(<<"msg_id">>, R)]))
        ),
        unfault(),
        execute(<<"UPDATE msg_store_staging SET available_at=now() WHERE msg_id=$1">>, [
            maps:get(<<"msg_id">>, R)
        ]),
        gen_statem:cast(msg_store_worker, kick),
        wait(
            fun() ->
                case staged(R) of
                    [X] -> maps:get(<<"processed_at">>, X) =/= null;
                    _ -> false
                end
            end,
            200
        ),
        ?assertEqual(1, maps:get(<<"conv_seq">>, hd(archives(R)))),
        ?assertEqual(1, head(S))
    after
        unfault(),
        restore(Old)
    end.
concurrent_same(S) ->
    R = row(S, 6, null),
    seed(S, 0),
    {ok, O} = application:get_env(imboy, super_account),
    {ok, C} = inttest_marker_db:safe_connect(O),
    execute_conn(C, <<"BEGIN">>, []),
    query_conn(C, <<"SELECT seq FROM msg_store_seq WHERE conv_key=$1 FOR UPDATE">>, [key(S)]),
    try
        concurrency(
            C,
            [R, R],
            fun() ->
                wait(
                    fun() ->
                        scalar(
                            <<"SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND wait_event_type='Lock' AND query LIKE '%msg_store_seq%' AND pid<>pg_backend_pid()">>,
                            []
                        ) >= 2
                    end,
                    150
                ),
                io:format(user, "REAL_PG_C2G_COUNTER_TWO_WAITERS~n", []),
                execute_conn(C, <<"COMMIT">>, [])
            end,
            [ok, ok]
        )
    after
        epgsql:close(C)
    end,
    ?assertEqual(1, length(archives(R))),
    ?assertEqual(1, head(S)).
concurrent_identity(S) ->
    R = row(S, 7, null),
    Other = R#{<<"payload">> => payload(maps:get(h, S))},
    {ok, O} = application:get_env(imboy, super_account),
    {ok, C} = inttest_marker_db:safe_connect(O),
    M = maps:get(<<"msg_id">>, R),
    query_conn(C, <<"SELECT pg_advisory_lock(hashtextextended('msg_c2g:'||$1,0))">>, [M]),
    try
        concurrency(
            C,
            [R, Other],
            fun() ->
                wait(
                    fun() ->
                        scalar(
                            <<"SELECT count(*) FROM pg_locks WHERE locktype='advisory' AND NOT granted AND database=(SELECT oid FROM pg_database WHERE datname=current_database())">>,
                            []
                        ) >= 2
                    end,
                    150
                ),
                io:format(user, "REAL_PG_C2G_IDENTITY_TWO_WAITERS~n", []),
                query_conn(C, <<"SELECT pg_advisory_unlock(hashtextextended('msg_c2g:'||$1,0))">>, [
                    M
                ])
            end,
            one_winner
        )
    after
        epgsql:close(C)
    end,
    ?assertEqual(1, length(archives(R))),
    ?assertEqual([1], [
        maps:get(<<"seq">>, X)
     || X <- query(<<"SELECT seq FROM msg_store_seq WHERE conv_key=ANY($1::text[])">>, [
            [key(S), msg_archive_repo:conv_key(<<"c2g">>, 0, maps:get(h, S))]
        ])
    ]).
concurrency(_C, Rows, Release, Expected) ->
    P = self(),
    Ref = make_ref(),
    Ws = [
        spawn_monitor(fun() ->
            P !
                {Ref,
                    try
                        msg_archive_repo:archive(R)
                    catch
                        K:E -> {K, E}
                    end}
        end)
     || R <- Rows
    ],
    try
        Release(),
        Results = [
            receive
                {Ref, X} -> X
            after 5000 -> error(concurrency_timeout)
            end
         || _ <- Rows
        ],
        case Expected of
            one_winner ->
                ?assertEqual(1, length([ok || ok <- Results])),
                ?assertEqual(1, length([X || {error, _} = X <- Results]));
            _ ->
                ?assertEqual(Expected, Results)
        end
    after
        [
            receive
                {'DOWN', Mon, process, Pid, _} -> ok
            after 5000 -> error(worker_timeout)
            end
         || {Pid, Mon} <- Ws
        ]
    end.
stage(S, R) ->
    M = maps:get(<<"msg_id">>, R),
    Seq = maps:get(<<"conv_seq">>, R),
    A = maps:get(a, S),
    G = maps:get(g, S),
    T = maps:get(<<"created_at">>, R),
    seed(S, Seq),
    execute(
        <<"INSERT INTO msg_c2g_recipient_snapshot(msg_id,from_id,to_gid,conv_seq,recipient_uids,created_at) VALUES($1,$2,$3,$4,$5::bigint[],$6)">>,
        [M, A, G, Seq, [maps:get(b, S)], T]
    ),
    execute(
        <<"INSERT INTO msg_store_staging(id,type,msg_id,msg_type,from_id,to_id,to_id_list,payload,e2ee,sender_did,conv_seq,created_at,server_ts) VALUES($1,'c2g',$2,'text',$3,$4,$5::bigint[],$6::jsonb,$7::jsonb,$8,$9,$10,$10)">>,
        [
            elib_tsid:generate(),
            M,
            A,
            G,
            [maps:get(b, S)],
            maps:get(<<"payload">>, R),
            maps:get(<<"e2ee">>, R),
            maps:get(<<"sender_did">>, R),
            Seq,
            T
        ]
    ).
seed(S, N) -> execute(<<"INSERT INTO msg_store_seq(conv_key,seq) VALUES($1,$2)">>, [key(S), N]).
fault() ->
    execute(
        <<"CREATE FUNCTION public.c2g_archive_fault() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic archive unique' USING ERRCODE='23505',CONSTRAINT='uk_msg_store_msg_id_created_at'; END $$">>,
        []
    ),
    execute(
        <<"CREATE TRIGGER c2g_archive_fault BEFORE INSERT ON msg_store FOR EACH ROW EXECUTE FUNCTION public.c2g_archive_fault()">>,
        []
    ).
unfault() ->
    execute(<<"DROP TRIGGER IF EXISTS c2g_archive_fault ON msg_store">>, []),
    execute(<<"DROP FUNCTION IF EXISTS public.c2g_archive_fault()">>, []).
archives(R) ->
    query(<<"SELECT * FROM msg_store WHERE msg_id=$1 ORDER BY created_at">>, [
        maps:get(<<"msg_id">>, R)
    ]).
staged(R) ->
    query(<<"SELECT * FROM msg_store_staging WHERE msg_id=$1">>, [maps:get(<<"msg_id">>, R)]).
head(S) -> scalar(<<"SELECT seq FROM msg_store_seq WHERE conv_key=$1">>, [key(S)]).
query(Q, A) ->
    {ok, R} = elib_pg:query(Q, A),
    R.
execute(Q, A) ->
    {ok, _} = elib_pg:execute(Q, A),
    ok.
query_conn(C, Q, A) ->
    {ok, R} = elib_pg:query(C, Q, A),
    R.
execute_conn(C, Q, A) ->
    {ok, _} = elib_pg:execute(C, Q, A),
    ok.
scalar(Q, A) ->
    [R] = query(Q, A),
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
restore(undefined) -> application:unset_env(imboy, msg_archive_enabled);
restore({ok, V}) -> application:set_env(imboy, msg_archive_enabled, V).
cleanup(S) ->
    A = maps:get(a, S),
    [
        execute(<<"DELETE FROM ", T/binary, " WHERE from_id=$1">>, [A])
     || T <- [
            <<"msg_store_staging">>,
            <<"msg_c2g">>,
            <<"msg_store">>,
            <<"msg_c2g_recipient_snapshot">>
        ]
    ],
    execute(<<"DELETE FROM msg_c2g_timeline WHERE to_uid=$1">>, [maps:get(b, S)]),
    execute(<<"DELETE FROM msg_store_seq WHERE conv_key=ANY($1::text[])">>, [
        [key(S), msg_archive_repo:conv_key(<<"c2g">>, 0, maps:get(h, S))]
    ]).
