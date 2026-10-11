-module(enterprise_direct_order_pg_tests).
-include_lib("eunit/include/eunit.hrl").

order_test_() ->
    {timeout, 900,
        {setup, fun setup/0, fun close/1, fun(_) ->
            {inorder, [
                {Name, fun() -> run(F) end}
             || {Name, F} <- [
                    {"application_and_human_share_acceptance_counter", fun sender_modes/1},
                    {"same_conn_failure_rolls_back_message_and_counter", fun rollback/1},
                    {"cross_identity_replay_rejected_without_counter_consumption", fun collision/1},
                    {"origin_audit_and_counter_use_caller_transaction", fun audit_atomic/1},
                    {"human_oa_same_counter_lock_waiter_preserves_acceptance", fun lock_waiter/1}
                ]
            ]}
        end}}.
setup() ->
    {ok, C} = eunit_runner:eunit_setup_with_db(),
    ?assertEqual({ok, 169, false}, erlang_migrate:version(#{conn => C, dir => "priv/migrations"})),
    C.
close(C) -> eunit_runner:eunit_cleanup_db(C).
run(F) ->
    A = elib_tsid:generate(user),
    B = elib_tsid:generate(user),
    S = #{a => A, b => B, key => msg_archive_repo:conv_key(<<"c2c">>, A, B)},
    try
        F(S)
    after
        {ok, _} = elib_pg:execute(<<"DELETE FROM msg_c2c WHERE from_id=$1">>, [A]),
        {ok, _} = elib_pg:execute(<<"DELETE FROM msg_store_seq WHERE conv_key=$1">>, [
            maps:get(key, S)
        ])
    end.
sender_modes(S) ->
    lists:foreach(
        fun({Kind, N}) ->
            M = msg(S, N),
            P = payload(S, Kind),
            {ok, _} = elib_pg:with_tx(fun(C) -> direct(C, S, M, P) end),
            [R] = rows(<<"SELECT conv_seq,payload,e2ee FROM msg_c2c WHERE msg_id=$1">>, [M]),
            ?assertEqual(N, maps:get(<<"conv_seq">>, R)),
            ?assertEqual(P, jsone:decode(maps:get(<<"payload">>, R), [{object_format, map}])),
            ?assertEqual(null, maps:get(<<"e2ee">>, R))
        end,
        [{<<"application">>, 1}, {<<"human">>, 2}]
    ),
    ?assertEqual(2, head(S)).
rollback(S) ->
    M = msg(S, 3),
    ?assertEqual(
        {rollback, synthetic_audit_failure},
        elib_pg:with_tx(fun(C) ->
            {ok, _} = direct(C, S, M, payload(S, <<"human">>)),
            throw({rollback, synthetic_audit_failure})
        end)
    ),
    ?assertEqual([], rows(<<"SELECT id FROM msg_c2c WHERE msg_id=$1">>, [M])),
    ?assertEqual(
        [], rows(<<"SELECT seq FROM msg_store_seq WHERE conv_key=$1">>, [maps:get(key, S)])
    ),
    {ok, _} = elib_pg:with_tx(fun(C) -> direct(C, S, M, payload(S, <<"human">>)) end),
    ?assertEqual(1, head(S)).
collision(S) ->
    M = msg(S, 4),
    {ok, _} = elib_pg:with_tx(fun(C) -> direct(C, S, M, payload(S, <<"human">>)) end),
    S2 = S#{b => elib_tsid:generate(user)},
    ?assertEqual(
        {rollback, msg_id_conflict},
        elib_pg:with_tx(fun(C) -> direct(C, S2, M, payload(S2, <<"human">>)) end)
    ),
    ?assertEqual(1, head(S)),
    ?assertEqual(
        [],
        rows(<<"SELECT seq FROM msg_store_seq WHERE conv_key=$1">>, [
            msg_archive_repo:conv_key(<<"c2c">>, maps:get(a, S2), maps:get(b, S2))
        ])
    ).
lock_waiter(S) ->
    A = maps:get(a, S),
    B = maps:get(b, S),
    Key = maps:get(key, S),
    {ok, Opts} = application:get_env(imboy, super_account),
    {ok, Lock} = inttest_marker_db:safe_connect(Opts),
    Parent = self(),
    try
        {ok, [#{<<"pid">> := LockPid}]} = elib_pg:query(
            Lock, <<"SELECT pg_backend_pid() AS pid">>, []
        ),
        {ok, _} = elib_pg:execute(Lock, <<"BEGIN">>, []),
        {ok, _} = elib_pg:execute(
            Lock, <<"INSERT INTO msg_store_seq(conv_key,seq) VALUES($1,0)">>, [Key]
        ),
        {ok, _} = direct(Lock, S, msg(S, 99), payload(S, <<"human">>)),
        spawn(fun() ->
            Parent !
                {waiter_result,
                    elib_pg:with_tx(fun(C) ->
                        direct(C, S, msg(S, 1), payload(S, <<"application">>))
                    end)}
        end),
        wait(
            fun() ->
                [#{<<"n">> := N}] = rows(
                    <<"SELECT count(*) AS n FROM pg_stat_activity WHERE datname=current_database() AND wait_event_type='Lock' AND $1=ANY(pg_blocking_pids(pid))">>,
                    [LockPid]
                ),
                N > 0
            end,
            150
        ),
        ?assertEqual([], rows(<<"SELECT id FROM msg_c2c WHERE msg_id=$1">>, [msg(S, 1)])),
        {ok, _} = elib_pg:execute(Lock, <<"COMMIT">>, []),
        receive
            {waiter_result, {ok, _}} -> ok
        after 5000 -> error(waiter_timeout)
        end,
        ?assertEqual(2, head(S)),
        [#{<<"conv_seq">> := 1}] = rows(<<"SELECT conv_seq FROM msg_c2c WHERE msg_id=$1">>, [
            msg(S, 99)
        ]),
        [#{<<"conv_seq">> := 2}] = rows(<<"SELECT conv_seq FROM msg_c2c WHERE msg_id=$1">>, [
            msg(S, 1)
        ]),
        T = elib_dt:now(),
        HumanId = msg(S, 0),
        ok = msg_c2c_repo:write_msg(
            T, HumanId, <<"synthetic Human ciphertext">>, A, B, T, <<"text">>, null
        ),
        [#{<<"conv_seq">> := 3}] = rows(<<"SELECT conv_seq FROM msg_c2c WHERE msg_id=$1">>, [
            HumanId
        ]),
        ?assertEqual(3, head(S))
    after
        elib_pg:execute(Lock, <<"ROLLBACK">>, []),
        epgsql:close(Lock)
    end.
wait(_, 0) ->
    error(real_lock_wait_timeout);
wait(F, N) ->
    case F() of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait(F, N - 1)
    end.

audit_atomic(S) ->
    M = msg(S, 5),
    A = maps:get(a, S),
    Org = elib_tsid:generate(organization),
    Result = elib_pg:with_tx(fun(C) ->
        {ok, _} = elib_pg:execute(
            C,
            <<"INSERT INTO \"user\" (id,password,account,reg_ip,reg_cosv) VALUES ($1,'x',$2,'127.0.0.1','x')">>,
            [A, M]
        ),
        {ok, _} = elib_pg:execute(
            C,
            <<"INSERT INTO organization (id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>,
            [Org, M, A]
        ),
        {ok, App} = enterprise_application_repo:create_tx(C, Org, M, M),
        AppId = maps:get(<<"id">>, App),
        P = payload(S, <<"human">>),
        Ctx = #{organization_id => Org, application_id => AppId},
        Digest = binary:copy(<<"a">>, 64),
        {ok, inserted} = enterprise_internal_idempotency:begin_tx(C, Ctx, <<"msg_c2c">>, M, Digest),
        {ok, Row} = direct(C, S, M, P),
        {ok, _} = enterprise_message_origin_repo:insert_tx(C, direct, Org, AppId, human, A, Row, M),
        Detail = #{
            <<"origin_application_id">> => AppId, <<"sender_user_id">> => A, <<"msg_id">> => M
        },
        {ok, _} = enterprise_message_repo:insert_audit_tx(C, Org, #{resource_id => Row}, Detail),
        {ok, [#{<<"conv_seq">> := Seq}]} = elib_pg:query(
            C, <<"SELECT conv_seq FROM msg_c2c WHERE msg_id=$1">>, [M]
        ),
        ?assertEqual(1, Seq),
        {ok, [Origin]} = elib_pg:query(
            C,
            <<"SELECT application_id,sender_user_id FROM enterprise_message_origin WHERE msg_id=$1">>,
            [M]
        ),
        ?assertEqual(AppId, maps:get(<<"application_id">>, Origin)),
        ?assertEqual(A, maps:get(<<"sender_user_id">>, Origin)),
        {ok, [#{<<"detail">> := Stored}]} = elib_pg:query(
            C, <<"SELECT detail FROM enterprise_audit_event WHERE resource_id=$1">>, [Row]
        ),
        ?assertEqual(Detail, jsone:decode(Stored, [{object_format, map}])),
        Snapshot = <<"{ \"accepted\" : true }">>,
        ok = enterprise_internal_idempotency:complete_tx(
            C, Ctx, <<"msg_c2c">>, M, Row, 200, Snapshot
        ),
        {ok, replay, Replay} = enterprise_internal_idempotency:begin_tx(
            C, Ctx, <<"msg_c2c">>, M, Digest
        ),
        ?assertEqual(Snapshot, maps:get(response_body, Replay)),
        ?assertEqual(Row, maps:get(resource_id, Replay)),
        {ok, [#{<<"seq">> := 1}]} = elib_pg:query(
            C, <<"SELECT seq FROM msg_store_seq WHERE conv_key=$1">>, [maps:get(key, S)]
        ),
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            C, <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE resource_id=$1">>, [Row]
        ),
        throw({rollback, synthetic_complete_tx_failure})
    end),
    ?assertEqual({rollback, synthetic_complete_tx_failure}, Result),
    ?assertEqual([], rows(<<"SELECT id FROM msg_c2c WHERE msg_id=$1">>, [M])),
    ?assertEqual([], rows(<<"SELECT id FROM enterprise_message_origin WHERE msg_id=$1">>, [M])),
    ?assertEqual(
        [], rows(<<"SELECT id FROM enterprise_audit_event WHERE organization_id=$1">>, [Org])
    ),
    ?assertEqual(
        [], rows(<<"SELECT seq FROM msg_store_seq WHERE conv_key=$1">>, [maps:get(key, S)])
    ).

direct(C, S, M, P) ->
    enterprise_message_repo:insert_direct_tx(C, M, maps:get(a, S), maps:get(b, S), <<"text">>, P).
payload(S, K) ->
    #{
        <<"text">> => <<"synthetic OA body">>,
        <<"origin">> => #{
            <<"kind">> => <<"enterprise_application">>,
            <<"application_id">> => maps:get(a, S),
            <<"sender_kind">> => K
        },
        <<"conv_seq">> => 999999
    }.
msg(S, N) ->
    <<"oadirect_", (integer_to_binary(maps:get(a, S)))/binary, "_", (integer_to_binary(N))/binary>>.
rows(Sql, P) ->
    {ok, R} = elib_pg:query(Sql, P),
    R.
head(S) ->
    [#{<<"seq">> := N}] = rows(<<"SELECT seq FROM msg_store_seq WHERE conv_key=$1">>, [
        maps:get(key, S)
    ]),
    N.
