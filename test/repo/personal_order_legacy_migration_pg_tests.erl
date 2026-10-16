-module(personal_order_legacy_migration_pg_tests).
-include_lib("eunit/include/eunit.hrl").

%% 一次性真 PG；升级旧 168 数据，不以发送者时间推断顺序。
%% Disposable PG: upgrade schema 168 data without sender-clock ordering.
legacy_migration_test_() ->
    {timeout, 900,
        {setup, fun setup/0, fun close/1, fun(S) ->
            {inorder, [
                {Name, {timeout, 60, fun() -> run_case(S, F) end}}
             || {Name, F} <- [
                    {"archive_sequence_and_duplicate_identity_preserved", fun archive_preserved/1},
                    {"archive_only_counter_repair", fun archive_only/1},
                    {"formal_only_failed_archive_and_stable_proxy", fun formal_only/1},
                    {"accepted_staging_sequence_preserved", fun staging_sequence/1},
                    {"down_up_retains_formal_only_sequence", fun roundtrip/1},
                    {"identity_conflict_rolls_back_entire_migration", fun identity_conflict/1},
                    {"sequence_collision_rolls_back_entire_migration", fun sequence_conflict/1},
                    {"same_message_sequence_conflict_rolls_back", fun message_sequence_conflict/1},
                    {"archive_conversation_key_conflict_rolls_back", fun archive_key_conflict/1},
                    {"compressed_formal_legacy_chunk", fun compressed_legacy/1},
                    {"migration_waits_for_accepted_transaction", fun concurrent_acceptance/1}
                ]
            ]}
        end}}.

setup() ->
    S = inttest_marker_db:provision(#{
        env_prefix => <<"PERSONAL_ORDER169">>,
        migration_target => 168,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, S),
    ?assertEqual(0, scalar(C, <<"SELECT count(*) AS n FROM public.config">>, [])),
    io:format(user, "OWN_MARKER ~s INITIAL_CONFIG0~n", [maps:get(db, S)]),
    #{host := H, port := P, username := U, password := Pass} = maps:get(server, S),
    Opts = #{
        host => H,
        port => P,
        username => U,
        password => Pass,
        database => maps:get(db, S),
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    },
    application:set_env(imboy, sql_driver, pgsql),
    PgConfBefore = application:get_env(imboy, pg_conf),
    application:set_env(imboy, pg_conf, #{
        name => pgsql,
        max_count => 8,
        init_count => 2,
        start_mfa => {epgsql, connect, [Opts]}
    }),
    ok = eunit_runner:ensure_default_pool(),
    S#{opts => Opts, pg_conf_before => PgConfBefore}.

close(S) ->
    %% 恢复共享 VM 的默认 pg_conf 并重建 pgsql 池：setup 覆写的 marker 库
    %% pg_conf 若不还原，后续套件（如 trust_audit）按残留配置重建池会指向
    %% 已 release 的 marker 库，全量尾部 no_connection（r2/r3/r5 实证）。
    case maps:get(pg_conf_before, S, undefined) of
        {ok, Conf} when is_map(Conf) ->
            application:set_env(imboy, pg_conf, Conf);
        _ ->
            ok
    end,
    _ = (catch eunit_runner:ensure_default_pool()),
    inttest_marker_db:release(S).

run_case(S, F) ->
    C = maps:get(conn, S),
    reset(C),
    try
        F(S)
    after
        reset(C)
    end.

reset(C) ->
    %% 失败迁移先由真库 oracle 验证回滚，再在自有 marker 修复台账。
    %% Repair the owned marker ledger only after the failure oracle.
    case erlang_migrate:version(config(C)) of
        {ok, _, true} -> ok = erlang_migrate:force(config(C), 168);
        _ -> ok = erlang_migrate:goto(config(C), 168)
    end,
    lists:foreach(
        fun(T) -> exec(C, <<"TRUNCATE public.", T/binary>>, []) end,
        [<<"msg_c2c">>, <<"msg_store_staging">>, <<"msg_store">>, <<"msg_store_seq">>]
    ).

archive_preserved(S) ->
    C = maps:get(conn, S),
    archive(C, <<"old">>, 7),
    formal(C, 101, <<"old">>, <<"2026-09-15T01:00:00Z">>),
    staging(C, 201, <<"old">>, null),
    staging(C, 202, <<"unarchived">>, null),
    counter(C, 2),
    %% C2G/企业群的既有顺序不属于本迁移 / C2G sequence is untouched.
    exec(C, <<"INSERT INTO public.msg_store_seq VALUES ('c2g:50', 19)">>, []),
    exec(
        C,
        <<
            "INSERT INTO public.msg_store_staging\n"
            "        (id,type,msg_id,from_id,to_id,payload,conv_seq) VALUES\n"
            "        (299,'c2g','group-synthetic',10,50,'{}',19)"
        >>,
        []
    ),
    ok = up(C),
    ?assertEqual([7, 7], sequences(C, <<"old">>)),
    ?assertEqual([8], sequences(C, <<"unarchived">>)),
    ?assertEqual(
        7, scalar(C, <<"SELECT conv_seq AS n FROM public.msg_store WHERE msg_id='old'">>, [])
    ),
    ?assertEqual(8, head(C)),
    ?assertEqual(
        19, scalar(C, <<"SELECT seq AS n FROM public.msg_store_seq WHERE conv_key='c2g:50'">>, [])
    ),
    ?assertEqual(
        19, scalar(C, <<"SELECT conv_seq AS n FROM public.msg_store_staging WHERE id=299">>, [])
    ),
    Before = snapshot(C),
    ok = raw_up(C),
    ?assertEqual(Before, snapshot(C)).

archive_only(S) ->
    C = maps:get(conn, S),
    archive(C, <<"archive-only">>, 83),
    counter(C, 1),
    ok = up(C),
    ?assertEqual(83, head(C)),
    ?assertEqual(83, scalar(C, <<"SELECT conv_seq AS n FROM public.msg_store">>, [])),
    counter(C, 100),
    ok = raw_up(C),
    ?assertEqual(100, head(C)).

formal_only(S) ->
    C = maps:get(conn, S),
    %% Worker archive failure + staging cleanup leaves only this formal row.
    %% 时间反序也必须按持久 id 稳定代理回填 / Reverse sender times are irrelevant.
    formal(C, 101, <<"formal-first">>, <<"2026-09-15T02:00:00Z">>),
    formal(C, 102, <<"formal-second">>, <<"2026-09-15T01:00:00Z">>),
    counter(C, 100),
    ok = up(C),
    ?assertEqual([101], sequences(C, <<"formal-first">>)),
    ?assertEqual([102], sequences(C, <<"formal-second">>)),
    ?assertEqual(102, head(C)),
    Before = snapshot(C),
    ok = raw_up(C),
    ?assertEqual(Before, snapshot(C)).

staging_sequence(S) ->
    C = maps:get(conn, S),
    staging(C, 201, <<"accepted">>, 12),
    formal(C, 101, <<"accepted">>, <<"2026-09-15T01:00:00Z">>),
    counter(C, 2),
    ok = up(C),
    ?assertEqual([12, 12], sequences(C, <<"accepted">>)),
    ?assertEqual(12, head(C)).

roundtrip(S) ->
    C = maps:get(conn, S),
    formal(C, 101, <<"formal-only">>, <<"2026-09-15T01:00:00Z">>),
    ok = up(C),
    ?assertEqual(
        {ok, 169, false}, erlang_migrate:version(config(C))
    ),
    Before = snapshot(C),
    ok = erlang_migrate:goto(config(C), 168),
    ?assertEqual({ok, 168, false}, erlang_migrate:version(config(C))),
    %% Non-destructive downgrade keeps the only durable sequence copy.
    ?assertEqual(Before, snapshot(C)),
    ok = up(C),
    ?assertEqual(Before, snapshot(C)).

identity_conflict(S) ->
    C = maps:get(conn, S),
    archive(C, <<"conflict">>, 7),
    formal(C, 101, <<"conflict">>, <<"2026-09-15T01:00:00Z">>),
    exec(C, <<"UPDATE public.msg_c2c SET to_id=30">>, []),
    staging(C, 201, <<"valid-null">>, null),
    counter(C, 1),
    ?assertMatch({error, _}, up(C)),
    ?assertEqual({ok, 169, true}, erlang_migrate:version(config(C))),
    ?assertEqual(1, head(C)),
    ?assertEqual(null, scalar(C, <<"SELECT conv_seq AS n FROM public.msg_store_staging">>, [])),
    ?assertEqual(7, scalar(C, <<"SELECT conv_seq AS n FROM public.msg_store">>, [])),
    ?assertEqual(1, scalar(C, <<"SELECT count(*) AS n FROM public.msg_c2c">>, [])).

sequence_conflict(S) ->
    C = maps:get(conn, S),
    archive(C, <<"first">>, 7),
    archive(C, <<"second">>, 7),
    staging(C, 201, <<"valid-null">>, null),
    counter(C, 1),
    ?assertMatch({error, _}, up(C)),
    ?assertEqual({ok, 169, true}, erlang_migrate:version(config(C))),
    ?assertEqual(1, head(C)),
    ?assertEqual(null, scalar(C, <<"SELECT conv_seq AS n FROM public.msg_store_staging">>, [])),
    ?assertEqual([7, 7], [
        maps:get(<<"conv_seq">>, R)
     || R <- rows(C, <<"SELECT conv_seq FROM public.msg_store">>, [])
    ]).

compressed_legacy(S) ->
    C = maps:get(conn, S),
    formal(C, 101, <<"compressed">>, <<"2026-09-15T01:00:00Z">>),
    [_] = rows(C, <<"SELECT compress_chunk(show_chunks('public.msg_c2c'))">>, []),
    ?assertEqual(
        1,
        scalar(
            C,
            <<
                "SELECT count(*) AS n FROM timescaledb_information.chunks\n"
                "        WHERE hypertable_name='msg_c2c' AND is_compressed"
            >>,
            []
        )
    ),
    ok = up(C),
    ?assertEqual([1], sequences(C, <<"compressed">>)),
    ?assertEqual(1, scalar(C, <<"SELECT count(*) AS n FROM public.msg_c2c">>, [])).

message_sequence_conflict(S) ->
    C = maps:get(conn, S),
    archive(C, <<"same-message">>, 7),
    staging(C, 201, <<"same-message">>, 8),
    counter(C, 9),
    ?assertMatch({error, _}, up(C)),
    ?assertEqual({ok, 169, true}, erlang_migrate:version(config(C))),
    ?assertEqual(9, head(C)),
    ?assertEqual([8], sequences_without_formal(C, <<"same-message">>)),
    ?assertEqual(7, scalar(C, <<"SELECT conv_seq AS n FROM public.msg_store">>, [])).

archive_key_conflict(S) ->
    C = maps:get(conn, S),
    archive(C, <<"wrong-key">>, 7),
    exec(C, <<"UPDATE public.msg_store SET conv_key='c2c:10:30'">>, []),
    counter(C, 1),
    ?assertMatch({error, _}, up(C)),
    ?assertEqual({ok, 169, true}, erlang_migrate:version(config(C))),
    ?assertEqual(1, head(C)),
    ?assertEqual(<<"c2c:10:30">>, scalar(C, <<"SELECT conv_key AS n FROM public.msg_store">>, [])).

sequences_without_formal(C, Msg) ->
    [
        maps:get(<<"conv_seq">>, R)
     || R <- rows(
            C,
            <<"SELECT conv_seq FROM public.msg_store_staging WHERE msg_id=$1">>,
            [Msg]
        )
    ].

concurrent_acceptance(S) ->
    C = maps:get(conn, S),
    archive(C, <<"archive">>, 7),
    staging(C, 201, <<"legacy-null">>, null),
    counter(C, 7),
    {ok, M} = inttest_marker_db:safe_connect(maps:get(opts, S)),
    Parent = self(),
    try
        exec(C, <<"BEGIN">>, []),
        Controller = scalar(C, <<"SELECT pg_backend_pid() AS n">>, []),
        exec(C, <<"UPDATE public.msg_store_seq SET seq=8 WHERE conv_key='c2c:10:20'">>, []),
        staging(C, 202, <<"inflight-accepted">>, 8),
        spawn(fun() -> Parent ! {migration_done, up(M)} end),
        wait_blocker(C, Controller, 100),
        receive
            {migration_done, _} -> error(migration_did_not_wait)
        after 0 -> ok
        end,
        exec(C, <<"COMMIT">>, []),
        receive
            {migration_done, Result} -> ?assertEqual(ok, Result)
        after 10000 -> error(migration_timeout)
        end,
        ?assertEqual([8], sequences(C, <<"inflight-accepted">>)),
        ?assertEqual([9], sequences(C, <<"legacy-null">>)),
        Data = #{
            id => 203,
            type => <<"c2c">>,
            msg_id => <<"post-migration">>,
            from_id => 10,
            to_id => 20,
            payload => <<"{}">>
        },
        ?assertEqual(
            {ok, 203}, personal_message_order_repo:stage_c2c(<<"public.msg_store_staging">>, Data)
        ),
        ?assertEqual([10], sequences(C, <<"post-migration">>)),
        ?assertEqual(10, head(C))
    after
        exec(C, <<"ROLLBACK">>, []),
        epgsql:close(M)
    end.

wait_blocker(_, _, 0) ->
    error(no_real_pg_lock_waiter);
wait_blocker(C, Pid, N) ->
    case
        scalar(
            C,
            <<
                "SELECT count(*) AS n FROM pg_stat_activity WHERE datname=current_database()\n"
                "        AND $1::integer=ANY(pg_blocking_pids(pid))"
            >>,
            [Pid]
        )
    of
        0 ->
            timer:sleep(20),
            wait_blocker(C, Pid, N - 1);
        _ ->
            io:format(user, "REAL_PG_MIGRATION_BLOCKED_BY_ACCEPTANCE~n", []),
            ok
    end.

config(C) -> #{conn => C, dir => "priv/migrations", strict => true}.
%% This fixture exercises only 168 -> 169; later forward-only domains are unrelated.
up(C) -> erlang_migrate:goto(config(C), 169).
raw_up(C) ->
    {ok, SQL} = file:read_file("priv/migrations/00000169_personal_message_acceptance_order.up.sql"),
    erlang_migrate_pg:exec_sql(C, SQL).
counter(C, N) ->
    exec(
        C,
        <<
            "INSERT INTO public.msg_store_seq VALUES ('c2c:10:20',$1)\n"
            "    ON CONFLICT (conv_key) DO UPDATE SET seq=EXCLUDED.seq"
        >>,
        [N]
    ).
head(C) ->
    scalar(C, <<"SELECT seq AS n FROM public.msg_store_seq WHERE conv_key='c2c:10:20'">>, []).
formal(C, Id, Msg, Ts) ->
    exec(
        C,
        <<
            "INSERT INTO public.msg_c2c\n"
            "    (id,msg_id,from_id,to_id,msg_type,payload,created_at) VALUES ($1,$2,10,20,'text','{}',$3)"
        >>,
        [Id, Msg, Ts]
    ).
staging(C, Id, Msg, Seq) ->
    exec(
        C,
        <<
            "INSERT INTO public.msg_store_staging\n"
            "    (id,type,msg_id,from_id,to_id,payload,conv_seq) VALUES ($1,'c2c',$2,10,20,'{}',$3)"
        >>,
        [Id, Msg, Seq]
    ).
archive(C, Msg, Seq) ->
    exec(
        C,
        <<
            "INSERT INTO public.msg_store\n"
            "    (id,chat_type,conv_key,conv_seq,msg_id,msg_type,from_id,to_id,payload)\n"
            "    VALUES ($1,'c2c','c2c:10:20',$2,$3,'text',10,20,'{}')"
        >>,
        [Seq * 100 + byte_size(Msg), Seq, Msg]
    ).
sequences(C, Msg) ->
    lists:sort([
        maps:get(<<"conv_seq">>, R)
     || R <- rows(
            C,
            <<
                "SELECT conv_seq FROM public.msg_c2c WHERE msg_id=$1 UNION ALL\n"
                "       SELECT conv_seq FROM public.msg_store_staging WHERE type='c2c' AND msg_id=$1"
            >>,
            [Msg]
        )
    ]).
snapshot(C) ->
    [
        rows(C, <<"SELECT msg_id,conv_seq FROM public.", T/binary, " ORDER BY msg_id">>, [])
     || T <- [<<"msg_c2c">>, <<"msg_store_staging">>, <<"msg_store">>]
    ] ++
        [rows(C, <<"SELECT conv_key,seq FROM public.msg_store_seq ORDER BY conv_key">>, [])].
rows(C, Sql, Params) ->
    {ok, Rows} = elib_pg:query(C, Sql, Params),
    Rows.
scalar(C, Sql, Params) ->
    [R] = rows(C, Sql, Params),
    [V] = maps:values(R),
    V.
exec(C, Sql, Params) ->
    {ok, _} = elib_pg:execute(C, Sql, Params),
    ok.
