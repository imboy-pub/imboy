-module(group_schedule_remind_pg_tests).
-include_lib("eunit/include/eunit.hrl").

%% Requires an explicitly provisioned, exclusive database; failures never skip.
remind_transaction_test_() -> {timeout, 30, fun remind_transaction/0}.

remind_transaction() ->
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(pooler),
    application:set_env(imboy, sql_driver, pgsql),
    ok = elib_tsid:init(#{dc_id => 0, node_id => 1}),
    Conf = #{
        host => os:getenv("PGHOST"),
        port => list_to_integer(os:getenv("PGPORT")),
        username => os:getenv("PGUSER"),
        password => os:getenv("PGPASSWORD"),
        database => os:getenv("PGDATABASE"),
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    },
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        max_count => 8,
        init_count => 4,
        start_mfa => {epgsql, connect, [Conf]}
    }),
    SystemId = list_to_binary(os:getenv("IMBOY_TEST_PG_SYSTEM_ID")),
    DbName = list_to_binary(os:getenv("IMBOY_TEST_EXCLUSIVE_DATABASE")),
    true = DbName =/= <<>>,
    {ok, [#{<<"system_identifier">> := SystemId}]} =
        elib_pg:query(<<"SELECT system_identifier::text FROM pg_control_system()">>, []),
    {ok, [#{<<"current_database">> := DbName}]} =
        elib_pg:query(<<"SELECT current_database()">>, []),
    {ok, [#{<<"owned">> := true}]} = elib_pg:query(
        <<"SELECT shobj_description(oid, 'pg_database') LIKE 'imboy-delivery-v3.2 merge schedule%exclusive%' AS owned FROM pg_database WHERE datname=current_database()">>,
        []
    ),
    ok = msg_store_repo:ensure_table_exists(),
    meck:new(workspace_resolver, [passthrough, no_link]),
    meck:new(msg_store_ds, [passthrough, no_link]),
    meck:new(message_ds, [passthrough, no_link]),
    meck:new(msg_store_repo, [passthrough, no_link]),
    meck:expect(workspace_resolver, resolve_workspace, fun(_) -> personal end),
    meck:expect(msg_store_ds, enqueue, fun(_, _, _) -> ok end),
    meck:expect(message_ds, send_next, fun(_, _, _, _) -> ok end),
    try
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM group_schedule_remind WHERE schedule_id='merge_schedule'">>, []
        ),
        {ok, _} = elib_pg:execute(<<"DELETE FROM group_schedule WHERE id=9900001">>, []),
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM msg_store_staging WHERE to_id=9900003 AND action='schedule_remind'">>, []
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO group_schedule (id, group_id, schedule_id, title, creator_id, start_at, end_at, remind_before) VALUES (9900001, 9900001, 'merge_schedule', 'test', 9900001, '2099-01-01Z', '2099-01-02Z', 15)">>,
            []
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO group_schedule_remind (id, schedule_id, user_id, remind_at) VALUES (9900002, 'merge_schedule', 9900003, now())">>,
            []
        ),
        Parent = self(),
        [
            spawn(fun() ->
                Parent !
                    {done,
                        try
                            group_schedule_ds:dispatch_remind(
                                9900002, <<"merge_schedule">>, 9900003
                            )
                        catch
                            C:R:St -> {crash, C, R, St}
                        end}
            end)
         || _ <- [1, 2]
        ],
        A =
            receive
                {done, X} -> X
            after 10000 -> timeout
            end,
        B =
            receive
                {done, Y} -> Y
            after 10000 -> timeout
            end,
        ?assertEqual([{written, sent}, {written, skipped}], lists:sort([A, B])),
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003 AND accepted_at IS NOT NULL AND expire_at IS NULL">>,
            []
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        meck:expect(msg_store_repo, stage_schedule_remind_tx, fun(_, _, _, _, _) ->
            {error, injected}
        end),
        ?assertEqual(
            {error, injected},
            group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)
        ),
        {ok, [#{<<"is_sent">> := false}]} = elib_pg:query(
            <<"SELECT is_sent FROM group_schedule_remind WHERE id=9900002">>, []
        ),
        meck:unload(msg_store_repo),
        ?assertEqual(
            {written, sent},
            group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)
        ),
        {ok, [#{<<"n">> := 2}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>,
            []
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        ?assertEqual(
            {ok, 1}, group_schedule_ds:cancel_with_reminders(<<"merge_schedule">>, 9900001)
        ),
        ?assertEqual(
            {written, skipped},
            group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)
        ),
        {ok, [#{<<"is_sent">> := true}]} = elib_pg:query(
            <<"SELECT is_sent FROM group_schedule_remind WHERE id=9900002">>, []
        ),
        {ok, _} = elib_pg:execute(<<"UPDATE group_schedule SET status=1 WHERE id=9900001">>, []),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        meck:expect(msg_store_ds, enqueue, fun(_, _, _) -> {error, injected_postcommit} end),
        ?assertMatch(
            {error, {after_commit_failed, {written, sent}, _}},
            group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)
        ),
        {ok, [#{<<"is_sent">> := true}]} = elib_pg:query(
            <<"SELECT is_sent FROM group_schedule_remind WHERE id=9900002">>, []
        ),
        {ok, [#{<<"n">> := 3}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>,
            []
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM group_schedule_participant WHERE id=9900004">>, []
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO group_schedule_participant (id, schedule_id, user_id) VALUES (9900004, 'merge_schedule', 9900003)">>,
            []
        ),
        ?assertEqual(
            {ok, 1},
            group_schedule_ds:update_with_reminders(
                <<"merge_schedule">>,
                9900001,
                #{start_at => <<"2099-02-01T00:00:00Z">>, end_at => <<"2099-02-02T00:00:00Z">>},
                <<"2099-02-01T00:00:00Z">>
            )
        ),
        {ok, [#{<<"is_sent">> := true}]} = elib_pg:query(
            <<"SELECT is_sent FROM group_schedule_remind WHERE id=9900002">>, []
        ),
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM group_schedule_remind WHERE schedule_id='merge_schedule' AND is_sent=false">>,
            []
        ),
        ?assertEqual(
            {written, skipped},
            group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        Conn = pooler:take_member(pgsql),
        {ok, [], []} = epgsql:squery(Conn, <<"BEGIN">>),
        {ok, _, [_]} = epgsql:equery(
            Conn, <<"SELECT id FROM group_schedule WHERE id=9900001 FOR UPDATE">>, []
        ),
        spawn(fun() ->
            Parent !
                {blocked_dispatch,
                    group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)}
        end),
        ok = await_waiter(50),
        {ok, 1} = epgsql:equery(
            Conn, <<"UPDATE group_schedule SET status=4 WHERE id=9900001">>, []
        ),
        {ok, _} = epgsql:equery(
            Conn,
            <<"UPDATE group_schedule_remind SET is_sent=true WHERE schedule_id='merge_schedule' AND is_sent=false">>,
            []
        ),
        {ok, [], []} = epgsql:squery(Conn, <<"COMMIT">>),
        ok = pooler:return_member(pgsql, Conn, ok),
        receive
            {blocked_dispatch, Result} -> ?assertEqual({written, skipped}, Result)
        after 10000 -> error(dispatch_timeout)
        end,
        {ok, _} = elib_pg:execute(<<"UPDATE group_schedule SET status=1 WHERE id=9900001">>, []),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        {ok, _} = elib_pg:execute(<<"DELETE FROM \"group\" WHERE id=9900001">>, []),
        {ok, _} = elib_pg:execute(<<"DELETE FROM workspace WHERE id=9900005">>, []),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO workspace (id,name,owner_id,status) SELECT 9900005,'merge schedule test',id,'archived' FROM \"user\" ORDER BY id LIMIT 1">>,
            []
        ),
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO \"group\" (id,owner_uid,creator_uid,scope,workspace_id) SELECT 9900001,id,id,'workspace',9900005 FROM \"user\" ORDER BY id LIMIT 1">>,
            []
        ),
        meck:unload(workspace_resolver),
        ?assertEqual(
            {ok, 9900005}, workspace_resolver:resolve_workspace({group_schedule_remind, 9900002})
        ),
        ?assertEqual(
            skipped, group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)
        ),
        {ok, [#{<<"is_sent">> := false}]} = elib_pg:query(
            <<"SELECT is_sent FROM group_schedule_remind WHERE id=9900002">>, []
        ),
        {ok, [#{<<"n">> := 3}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>,
            []
        ),
        {ok, _} = elib_pg:execute(<<"UPDATE workspace SET status='active' WHERE id=9900005">>, []),
        meck:expect(msg_store_ds, enqueue, fun(_, _, _) -> ok end),
        meck:new(msg_store_repo, [passthrough, no_link]),
        meck:expect(msg_store_repo, stage_schedule_remind_tx, fun(C, M, U, P, T) ->
            Staged = meck:passthrough([C, M, U, P, T]),
            Parent ! {staged, self()},
            receive
                proceed -> Staged
            after 10000 -> error(stage_timeout)
            end
        end),
        spawn(fun() ->
            Parent !
                {first_dispatch,
                    group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)}
        end),
        Writer =
            receive
                {staged, Pid} -> Pid
            after 10000 -> error(stage_timeout)
            end,
        spawn(fun() ->
            Parent !
                {cancel_after,
                    group_schedule_ds:cancel_with_reminders(<<"merge_schedule">>, 9900001)}
        end),
        ok = await_waiter(50),
        Writer ! proceed,
        receive
            {first_dispatch, First} -> ?assertEqual({written, sent}, First)
        after 10000 -> error(dispatch_timeout)
        end,
        receive
            {cancel_after, Cancel} -> ?assertEqual({ok, 1}, Cancel)
        after 10000 -> error(cancel_timeout)
        end,
        {ok, [#{<<"n">> := 4}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>,
            []
        ),
        {ok, _} = elib_pg:execute(<<"UPDATE group_schedule SET status=1 WHERE id=9900001">>, []),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        spawn(fun() ->
            Parent !
                {reschedule_dispatch,
                    group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)}
        end),
        Writer2 =
            receive
                {staged, Pid2} -> Pid2
            after 10000 -> error(stage_timeout)
            end,
        spawn(fun() ->
            Parent !
                {reschedule_after,
                    group_schedule_ds:update_with_reminders(
                        <<"merge_schedule">>,
                        9900001,
                        #{
                            start_at => <<"2099-03-01T00:00:00Z">>,
                            end_at => <<"2099-03-02T00:00:00Z">>
                        },
                        <<"2099-03-01T00:00:00Z">>
                    )}
        end),
        ok = await_waiter(50),
        Writer2 ! proceed,
        receive
            {reschedule_dispatch, D} -> ?assertEqual({written, sent}, D)
        after 10000 -> error(dispatch_timeout)
        end,
        receive
            {reschedule_after, U} -> ?assertEqual({ok, 1}, U)
        after 10000 -> error(update_timeout)
        end,
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM group_schedule_remind WHERE schedule_id='merge_schedule' AND is_sent=false">>,
            []
        ),
        {ok, [#{<<"n">> := 5}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>,
            []
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        meck:new(group_schedule_repo, [passthrough, no_link]),
        meck:expect(group_schedule_repo, update_tx, fun(C, I, Data) ->
            Updated = meck:passthrough([C, I, Data]),
            Parent ! {updated, self()},
            receive
                proceed -> Updated
            after 10000 -> error(update_timeout)
            end
        end),
        spawn(fun() ->
            Parent !
                {first_update,
                    group_schedule_ds:update_with_reminders(
                        <<"merge_schedule">>,
                        9900001,
                        #{
                            start_at => <<"2099-04-01T00:00:00Z">>,
                            end_at => <<"2099-04-02T00:00:00Z">>
                        },
                        <<"2099-04-01T00:00:00Z">>
                    )}
        end),
        Updater =
            receive
                {updated, Pid3} -> Pid3
            after 10000 -> error(update_timeout)
            end,
        spawn(fun() ->
            Parent !
                {dispatch_after_update,
                    group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)}
        end),
        ok = await_waiter(50),
        Updater ! proceed,
        receive
            {first_update, U2} -> ?assertEqual({ok, 1}, U2)
        after 10000 -> error(update_timeout)
        end,
        receive
            {dispatch_after_update, D2} -> ?assertEqual({written, skipped}, D2)
        after 10000 -> error(dispatch_timeout)
        end,
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM group_schedule_remind WHERE schedule_id='merge_schedule' AND is_sent=false">>,
            []
        ),
        {ok, [#{<<"n">> := 5}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>,
            []
        ),
        meck:unload(group_schedule_repo),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        {CrashPid, Ref} = spawn_monitor(fun() ->
            group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)
        end),
        receive
            {staged, CrashPid} -> ok
        after 10000 -> error(stage_timeout)
        end,
        exit(CrashPid, kill),
        receive
            {'DOWN', Ref, process, CrashPid, killed} -> ok
        after 10000 -> error(crash_timeout)
        end,
        ok = await_rollback(50),
        {ok, [#{<<"n">> := 5}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>,
            []
        ),
        meck:unload(msg_store_repo),
        ?assertEqual(
            {written, sent},
            group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)
        ),
        {ok, [#{<<"n">> := 6}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>,
            []
        ),
        {ok, _} = elib_pg:execute(
            <<"DELETE FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>, []
        ),
        {ok, _} = elib_pg:execute(
            <<"UPDATE group_schedule_remind SET is_sent=false WHERE id=9900002">>, []
        ),
        meck:expect(msg_store_ds, enqueue, fun(_, _, _) -> {error, injected_postcommit} end),
        ?assertMatch(
            {error, {after_commit_failed, {written, sent}, _}},
            group_schedule_ds:dispatch_remind(9900002, <<"merge_schedule">>, 9900003)
        ),
        {ok, [#{<<"msg_id">> := RecoveryMsg}]} = elib_pg:query(
            <<"SELECT msg_id FROM msg_store_staging WHERE action='schedule_remind' AND to_id=9900003">>,
            []
        ),
        application:set_env(imboy, msg_store_worker_tick_ms, 50),
        {ok, W0} = msg_store_worker:start_link(),
        unlink(W0),
        gen_statem:stop(W0),
        {ok, W1} = msg_store_worker:start_link(),
        unlink(W1),
        ok = await_worker(RecoveryMsg, 100),
        gen_statem:stop(W1),
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            <<"SELECT count(*) AS n FROM msg_s2c WHERE msg_id=$1 AND to_id=9900003">>, [RecoveryMsg]
        ),
        ok
    after
        lists:foreach(fun(M) -> catch meck:unload(M) end, [
            group_schedule_repo, msg_store_repo, message_ds, msg_store_ds, workspace_resolver
        ])
    end.

await_waiter(0) ->
    error(lock_wait_timeout);
await_waiter(N) ->
    {ok, [#{<<"n">> := Count}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM pg_stat_activity WHERE datname=current_database() AND wait_event_type='Lock' AND (query LIKE 'SELECT id, status, title, start_at, remind_before FROM %group_schedule%FOR UPDATE%' OR query LIKE 'SELECT status FROM workspace%FOR UPDATE%')">>,
        []
    ),
    case Count > 0 of
        true ->
            ok;
        false ->
            timer:sleep(20),
            await_waiter(N - 1)
    end.

await_rollback(0) ->
    error(rollback_timeout);
await_rollback(N) ->
    case elib_pg:query(<<"SELECT is_sent FROM group_schedule_remind WHERE id=9900002">>, []) of
        {ok, [#{<<"is_sent">> := false}]} ->
            ok;
        _ ->
            timer:sleep(20),
            await_rollback(N - 1)
    end.

await_worker(_, 0) ->
    error(worker_recovery_timeout);
await_worker(MsgId, N) ->
    case
        elib_pg:query(
            <<"SELECT processed_at IS NOT NULL AS processed FROM msg_store_staging WHERE msg_id=$1">>,
            [MsgId]
        )
    of
        {ok, [#{<<"processed">> := true}]} ->
            ok;
        _ ->
            timer:sleep(50),
            await_worker(MsgId, N - 1)
    end.
