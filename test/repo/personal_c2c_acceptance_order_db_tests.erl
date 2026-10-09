-module(personal_c2c_acceptance_order_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% 真 PG：接受顺序在 staging commit 时确定，不依赖发送时间或 worker。
%% Real PG: acceptance order is fixed by staging commit, not sender time/worker.
acceptance_order_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun acceptance_order/0).

acceptance_order() ->
    A = elib_tsid:generate(),
    B = elib_tsid:generate(),
    Key = msg_archive_repo:conv_key(<<"c2c">>, A, B),
    M1 = id(A, 1),
    M2 = id(A, 2),
    try
        {ok, _} = stage(A, B, M1, 2000),
        S1 = sequence(M1),
        ?assert(is_integer(S1) andalso S1 > 0),
        %% 发送时间倒退不改变接受顺序 / Sender clock reversal is irrelevant.
        {ok, _} = stage(A, B, M2, 1000),
        ?assertEqual(S1 + 1, sequence(M2)),
        Before = counter(Key),
        ?assertMatch({error, {unique_violation, _}}, stage(A, B, M1, 2000)),
        ?assertEqual({ok, duplicate}, stage_ds(A, B, M1, 2000)),
        ?assertEqual(S1, sequence(M1)),
        ?assertEqual(Before, counter(Key))
    after
        elib_pg:execute(<<"DELETE FROM public.msg_store_staging WHERE from_id = $1">>, [A]),
        elib_pg:execute(<<"DELETE FROM public.msg_store_seq WHERE conv_key = $1">>, [Key])
    end.

rollback_does_not_consume_sequence_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        A = elib_tsid:generate(),
        B = elib_tsid:generate(),
        Key = msg_archive_repo:conv_key(<<"c2c">>, A, B),
        Good = id(A, 1),
        try
            {ok, _} = stage(A, B, Good, 1000),
            Before = counter(Key),
            %% DB CHECK rejects the oversized msg_id after sequence allocation.
            %% 超长 msg_id 由 PG 列约束拒绝，序号分配也必须回滚。
            ?assertMatch({error, _}, stage(A, B, binary:copy(<<"x">>, 51), 2000)),
            ?assertEqual(Before, counter(Key))
        after
            elib_pg:execute(<<"DELETE FROM public.msg_store_staging WHERE from_id = $1">>, [A]),
            elib_pg:execute(<<"DELETE FROM public.msg_store_seq WHERE conv_key = $1">>, [Key])
        end
    end).

unrelated_unique_constraint_remains_error_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        A = elib_tsid:generate(),
        B = elib_tsid:generate(),
        C = elib_tsid:generate(),
        First = id(A, 30),
        Second = id(A, 31),
        Ts = elib_dt:to_rfc3339(1790000000000, millisecond),
        try
            {ok, _} = stage(A, B, First, 1000),
            {ok, [#{<<"id">> := ExistingId}]} = elib_pg:query(
                <<"SELECT id FROM public.msg_store_staging WHERE msg_id = $1">>, [First]
            ),
            Data = #{
                id => ExistingId,
                type => <<"c2c">>,
                msg_id => Second,
                from_id => A,
                to_id => C,
                msg_type => <<"text">>,
                action => <<>>,
                e2ee => null,
                payload => <<"{\"text\":\"synthetic\"}">>,
                created_at => Ts,
                server_ts => Ts,
                retry_count => 0
            },
            {error, {error, _, <<"23505">>, unique_violation, _, Extra}} =
                personal_message_order_repo:stage_c2c(<<"public.msg_store_staging">>, Data),
            ?assertEqual(<<"msg_store_staging_pkey">>, proplists:get_value(constraint_name, Extra)),
            ?assertEqual([], counters([msg_archive_repo:conv_key(<<"c2c">>, A, C)])),
            ?assertEqual(1, sequence(First))
        after
            cleanup(First, [{A, B}, {A, C}]),
            cleanup(Second, [{A, C}])
        end
    end).

%% 同 msg_id 的 from/to 必须保持原身份，反向发送也不是合法重试。
%% Same msg_id must retain its sender/recipient identity, even for reversed peers.
cross_identity_conflict_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        A = elib_tsid:generate(),
        B = elib_tsid:generate(),
        C = elib_tsid:generate(),
        MsgId = id(A, 10),
        try
            {ok, _} = stage(A, B, MsgId, 1000),
            lists:foreach(
                fun({From, To}) ->
                    ?assertEqual({error, msg_id_conflict}, stage(From, To, MsgId, 1000)),
                    ?assertEqual({error, msg_id_conflict}, stage_ds(From, To, MsgId, 1000))
                end,
                [{A, C}, {C, B}, {B, A}]
            ),
            {ok, [Row]} = identity(MsgId),
            ?assertMatch(#{<<"from_id">> := A, <<"to_id">> := B, <<"conv_seq">> := 1}, Row),
            ?assertEqual(1, counter(msg_archive_repo:conv_key(<<"c2c">>, A, B))),
            ?assertEqual(
                [],
                counters([
                    msg_archive_repo:conv_key(<<"c2c">>, A, C),
                    msg_archive_repo:conv_key(<<"c2c">>, C, B)
                ])
            )
        after
            cleanup(MsgId, [{A, B}, {A, C}, {C, B}])
        end
    end).

stored_identity_conflict_test_() ->
    [stored_identity_case(Surface) || Surface <- [formal, archive]].

stored_identity_case(Surface) ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        A = elib_tsid:generate(),
        B = elib_tsid:generate(),
        C = elib_tsid:generate(),
        MsgId = id(A, 20),
        Ts = elib_dt:to_rfc3339(1790000000000, millisecond),
        try
            {ok, _} = stage(A, B, MsgId, 1000),
            case Surface of
                formal ->
                    ok = msg_c2c_repo:write_msg_with_sender(
                        Ts,
                        MsgId,
                        <<"{\"text\":\"synthetic\"}">>,
                        A,
                        B,
                        Ts,
                        <<"text">>,
                        null,
                        null
                    );
                archive ->
                    {ok, [Row]} = elib_pg:query(
                        <<"SELECT * FROM public.msg_store_staging WHERE msg_id = $1">>, [MsgId]
                    ),
                    ok = msg_archive_repo:archive(Row)
            end,
            {ok, _} = elib_pg:execute(
                <<"DELETE FROM public.msg_store_staging WHERE msg_id = $1">>, [MsgId]
            ),
            ?assertEqual({error, {unique_violation, MsgId}}, stage(A, B, MsgId, 1000)),
            ?assertEqual({error, msg_id_conflict}, stage(A, C, MsgId, 1000)),
            ?assertEqual({error, msg_id_conflict}, stage(B, A, MsgId, 1000)),
            ?assertEqual([], counters([msg_archive_repo:conv_key(<<"c2c">>, A, C)]))
        after
            elib_pg:execute(<<"DELETE FROM public.msg_c2c WHERE msg_id = $1">>, [MsgId]),
            elib_pg:execute(<<"DELETE FROM public.msg_store WHERE msg_id = $1">>, [MsgId]),
            cleanup(MsgId, [{A, B}, {A, C}])
        end
    end).

same_identity_concurrent_retry_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        A = elib_tsid:generate(),
        B = elib_tsid:generate(),
        MsgId = id(A, 11),
        try
            Results = concurrent([{A, B}, {A, B}], MsgId),
            ?assertEqual(1, length([R || {ok, _} = R <- Results])),
            ?assertEqual(
                1,
                length([
                    R
                 || {error, {unique_violation, ExistingId}} = R <- Results, ExistingId =:= MsgId
                ])
            ),
            ?assertEqual(1, sequence(MsgId)),
            ?assertEqual(1, counter(msg_archive_repo:conv_key(<<"c2c">>, A, B)))
        after
            cleanup(MsgId, [{A, B}])
        end
    end).

%% INSERT gate 阻塞赢家；另一会话必须等待共享 msg_id 锁，不能同时穿过身份检查。
%% The INSERT gate blocks the winner while the other conversation waits on the shared message lock.
cross_conversation_insert_race_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        A = elib_tsid:generate(),
        B = elib_tsid:generate(),
        C = elib_tsid:generate(),
        MsgId = id(A, 12),
        Gate = A rem 2147483647,
        {ok, #{start_mfa := {epgsql, connect, [Opts]}}} = application:get_env(imboy, pg_conf),
        {ok, Conn} = epgsql:connect(Opts),
        try
            install_gate(Conn, MsgId, Gate),
            {ok, _, _} = epgsql:equery(Conn, "SELECT pg_advisory_lock(169, $1)", [Gate]),
            Ref = make_ref(),
            Parent = self(),
            Workers = [
                spawn_monitor(fun() -> Parent ! {Ref, stage(From, To, MsgId, 1000)} end)
             || {From, To} <- [{A, B}, {A, C}]
            ],
            try
                await_waiters(Conn, Gate, 100),
                {ok, _, _} = epgsql:equery(Conn, "SELECT pg_advisory_unlock(169, $1)", [Gate]),
                Results = [
                    receive
                        {Ref, R} -> R
                    after 10000 -> error(race_timeout)
                    end
                 || _ <- Workers
                ],
                ?assertEqual(1, length([R || {ok, _} = R <- Results])),
                ?assertEqual(1, length([R || {error, msg_id_conflict} = R <- Results])),
                ?assertEqual(1, sequence(MsgId)),
                {ok, [Row]} = identity(MsgId),
                Winner = {maps:get(<<"from_id">>, Row), maps:get(<<"to_id">>, Row)},
                ?assert(lists:member(Winner, [{A, B}, {A, C}])),
                ?assertEqual(
                    [1],
                    counters([
                        msg_archive_repo:conv_key(<<"c2c">>, A, B),
                        msg_archive_repo:conv_key(<<"c2c">>, A, C)
                    ])
                )
            after
                [erlang:demonitor(Mon, [flush]) || {_, Mon} <- Workers]
            end
        after
            epgsql:equery(Conn, "SELECT pg_advisory_unlock(169, $1)", [Gate]),
            epgsql:squery(
                Conn,
                "DROP TRIGGER IF EXISTS inttest_personal_order_gate ON public.msg_store_staging"
            ),
            epgsql:squery(Conn, "DROP FUNCTION IF EXISTS public.inttest_personal_order_gate()"),
            epgsql:equery(Conn, "SELECT pg_advisory_unlock(169, $1)", [Gate]),
            epgsql:close(Conn),
            cleanup(MsgId, [{A, B}, {A, C}])
        end
    end).

concurrent(Peers, MsgId) ->
    Parent = self(),
    Ref = make_ref(),
    Workers = [
        spawn_monitor(fun() ->
            receive
                {go, Ref} ->
                    Parent ! {Ref, stage(A, B, MsgId, 1000)}
            end
        end)
     || {A, B} <- Peers
    ],
    try
        [Pid ! {go, Ref} || {Pid, _} <- Workers],
        [
            receive
                {Ref, R} -> R
            after 10000 -> error(retry_timeout)
            end
         || _ <- Workers
        ]
    after
        [erlang:demonitor(Mon, [flush]) || {_, Mon} <- Workers]
    end.

identity(MsgId) ->
    elib_pg:query(
        <<
            "SELECT from_id, to_id, conv_seq FROM public.msg_store_staging "
            "WHERE type = 'c2c' AND msg_id = $1"
        >>,
        [MsgId]
    ).

counters(Keys) ->
    {ok, Rows} = elib_pg:query(
        <<"SELECT seq FROM public.msg_store_seq WHERE conv_key = ANY($1::text[]) ORDER BY seq">>, [
            Keys
        ]
    ),
    [maps:get(<<"seq">>, R) || R <- Rows].

cleanup(MsgId, Peers) ->
    elib_pg:execute(
        <<"DELETE FROM public.msg_store_staging WHERE type = 'c2c' AND msg_id = $1">>, [MsgId]
    ),
    [
        elib_pg:execute(
            <<"DELETE FROM public.msg_store_seq WHERE conv_key = $1">>,
            [msg_archive_repo:conv_key(<<"c2c">>, A, B)]
        )
     || {A, B} <- Peers
    ],
    ok.

install_gate(Conn, MsgId, Gate) ->
    %% MsgId is generated by id/2 above and contains only ASCII letters/digits/hyphens.
    %% 标识符来自本测试生成器，不接受外部输入。
    Sql = iolist_to_binary([
        "CREATE FUNCTION public.inttest_personal_order_gate() RETURNS trigger "
        "LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_advisory_xact_lock(169, ",
        integer_to_binary(Gate),
        "); RETURN NEW; END $$; CREATE TRIGGER inttest_personal_order_gate "
        "BEFORE INSERT ON public.msg_store_staging FOR EACH ROW WHEN (NEW.msg_id = '",
        MsgId,
        "') EXECUTE FUNCTION public.inttest_personal_order_gate();"
    ]),
    Results = epgsql:squery(Conn, Sql),
    ?assert(
        lists:all(
            fun
                ({ok, _, _}) -> true;
                ({ok, _}) -> true;
                (_) -> false
            end,
            Results
        )
    ).

await_waiters(_Conn, _Gate, 0) ->
    error(insert_gate_timeout);
await_waiters(Conn, Gate, Attempts) ->
    {ok, _, [{Count}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM pg_locks WHERE locktype = 'advisory' AND classid = 169 "
        "AND objid = $1::oid AND NOT granted AND database = (SELECT oid FROM pg_database WHERE datname = current_database())",
        [Gate]
    ),
    {ok, _, [{OtherCount}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM pg_locks WHERE locktype = 'advisory' AND NOT granted "
        "AND NOT (classid = 169 AND objid = $1::oid) "
        "AND database = (SELECT oid FROM pg_database WHERE datname = current_database())",
        [Gate]
    ),
    case {Count, OtherCount} of
        {1, 1} ->
            ok;
        _ ->
            timer:sleep(25),
            await_waiters(Conn, Gate, Attempts - 1)
    end.

stage(A, B, Id, Ms) ->
    Ts = elib_dt:to_rfc3339(1790000000000 + Ms, millisecond),
    msg_store_repo:stage(
        <<"c2c">>,
        Id,
        <<"text">>,
        <<>>,
        null,
        <<"{\"text\":\"synthetic\"}">>,
        A,
        B,
        Ts,
        Ts,
        <<>>
    ).

stage_ds(A, B, Id, Ms) ->
    Ts = elib_dt:to_rfc3339(1790000000000 + Ms, millisecond),
    msg_store_ds:stage(
        <<"c2c">>,
        Id,
        <<"text">>,
        <<>>,
        null,
        <<"{\"text\":\"synthetic\"}">>,
        A,
        B,
        Ts,
        Ts,
        <<>>
    ).

id(A, N) -> iolist_to_binary(["personal-order-", integer_to_binary(A), "-", integer_to_binary(N)]).

sequence(Id) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT conv_seq FROM public.msg_store_staging WHERE type = 'c2c' AND msg_id = $1">>, [Id]
    ),
    maps:get(<<"conv_seq">>, Row).

counter(Key) ->
    {ok, [Row]} = elib_pg:query(<<"SELECT seq FROM public.msg_store_seq WHERE conv_key = $1">>, [
        Key
    ]),
    maps:get(<<"seq">>, Row).
