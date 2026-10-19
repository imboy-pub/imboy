-module(personal_order_guard_pg_tests).
-include_lib("eunit/include/eunit.hrl").

order_guard_test_() ->
    {timeout, 900,
        {setup, fun() -> inttest_marker_db:provision(#{env_prefix => <<"C2C_GUARD">>}) end,
            fun inttest_marker_db:release/1, fun(S) ->
                {inorder, [
                    {"reject null and nonpositive sequence on all C2C surfaces", fun() ->
                        missing(S)
                    end},
                    {"preserve sequence across copies and reject conflicting identities", fun() ->
                        copies(S)
                    end},
                    {"cross-surface concurrent slot collision has one winner", fun() -> race(S) end},
                    {"legacy metadata updates remain possible", fun() -> legacy(S) end},
                    {"allocator repairs a deleted or lagging counter", fun() -> allocator(S) end},
                    {"down and up preserve messages", fun() -> roundtrip(S) end},
                    {"startup refuses a disabled order guard", fun() -> startup(S) end},
                    {"empty restored history is not automatically backfilled", fun() ->
                        bootstrap(S)
                    end}
                ]}
            end}}.

missing(#{conn := C}) ->
    exec(C, <<"BEGIN">>),
    lists:foreach(
        fun(T) ->
            lists:foreach(
                fun(Seq) ->
                    rejected(C, insert(T, 1, <<"missing">>, Seq), <<"c2c_sequence_required">>)
                end,
                [null, 0, -1]
            )
        end,
        [staging, formal, archive]
    ),
    exec(C, <<"ROLLBACK">>).

copies(#{conn := C}) ->
    exec(C, <<"BEGIN">>),
    ok = write(C, staging, 10, <<"accepted">>, 7),
    ok = write(C, formal, 11, <<"accepted">>, 7),
    ok = write(C, archive, 12, <<"accepted">>, 7),
    rejected(C, insert(formal, 13, <<"another">>, 7), <<"c2c_order_conflict">>),
    rejected(C, insert(archive, 14, <<"accepted">>, 8), <<"c2c_order_conflict">>),
    rejected(
        C,
        <<"UPDATE msg_store_staging SET conv_seq=8 WHERE msg_id='accepted'">>,
        <<"c2c_order_immutable">>
    ),
    rejected(
        C,
        <<"UPDATE msg_store SET conv_key='c2c:10:30' WHERE msg_id='accepted'">>,
        <<"c2c_conversation_mismatch">>
    ),
    rejected(
        C,
        <<"UPDATE msg_store_staging SET type='c2g' WHERE msg_id='accepted'">>,
        <<"c2c_order_immutable">>
    ),
    exec(C, <<"ROLLBACK">>).

race(#{conn := C, server := Server, db := Db}) ->
    {ok, Other} = inttest_marker_db:safe_connect(Server#{database => Db}),
    Parent = self(),
    try
        exec(C, <<"BEGIN">>),
        ok = write(C, staging, 20, <<"winner">>, 9),
        {ok, _, [{Backend}]} = epgsql:equery(Other, <<"SELECT pg_backend_pid()">>, []),
        {Pid, Ref} = spawn_monitor(fun() ->
            Parent ! {race_result, epgsql:squery(Other, insert(formal, 21, <<"loser">>, 9))}
        end),
        wait_blocked(C, Backend, 100),
        exec(C, <<"COMMIT">>),
        receive
            {race_result, R} -> assert_rejected(R, <<"c2c_order_conflict">>)
        after 5000 -> error({race_timeout, Pid})
        end,
        receive
            {'DOWN', Ref, process, Pid, normal} -> ok
        after 5000 -> error(race_exit_timeout)
        end,
        exec(C, <<"DELETE FROM msg_store_staging WHERE msg_id='winner'">>)
    after
        epgsql:squery(C, <<"ROLLBACK">>),
        epgsql:close(Other)
    end.

legacy(#{conn := C}) ->
    exec(C, <<"BEGIN">>),
    exec(C, <<"DROP TRIGGER trg_00_c2c_order_guard ON msg_store_staging">>),
    ok = write(C, staging, 30, <<"legacy">>, null),
    apply_up(C),
    exec(C, <<"UPDATE msg_store_staging SET retry_count=retry_count+1 WHERE msg_id='legacy'">>),
    rejected(
        C,
        <<"UPDATE msg_store_staging SET conv_seq=NULL WHERE msg_id='legacy'">>,
        <<"c2c_sequence_required">>
    ),
    exec(C, <<"ROLLBACK">>).

allocator(#{conn := C, server := Server, db := Db}) ->
    {ok, Old} = application:get_env(imboy, pg_conf),
    application:set_env(imboy, pg_conf, #{
        name => pgsql,
        init_count => 2,
        max_count => 4,
        start_mfa => {epgsql, connect, [Server#{database => Db}]}
    }),
    ok = eunit_runner:ensure_default_pool(),
    try
        ok = write(C, archive, 40, <<"existing-head">>, 40),
        Data = #{
            id => 41,
            type => <<"c2c">>,
            msg_id => <<"new-acceptance">>,
            from_id => 10,
            to_id => 20,
            payload => <<"{}">>
        },
        ?assertEqual(
            {ok, 41, 41},
            personal_message_order_repo:stage_c2c_ordered(<<"public.msg_store_staging">>, Data)
        ),
        exec(C, <<"UPDATE msg_store_seq SET seq=1 WHERE conv_key='c2c:10:20'">>),
        ?assertEqual(
            {ok, 42, 42},
            personal_message_order_repo:stage_c2c_ordered(
                <<"public.msg_store_staging">>, Data#{id => 42, msg_id => <<"next-acceptance">>}
            )
        ),
        retention(C)
    after
        exec(C, <<"DELETE FROM msg_store WHERE msg_id='existing-head'">>),
        exec(
            C,
            <<"DELETE FROM msg_store_staging WHERE msg_id IN ('new-acceptance','next-acceptance')">>
        ),
        exec(C, <<"DELETE FROM msg_store_seq WHERE conv_key='c2c:10:20'">>),
        application:set_env(imboy, pg_conf, Old),
        eunit_runner:ensure_default_pool()
    end.

retention(C) ->
    {ok, 1} = msg_store_repo:mark_processed(<<"c2c">>, <<"new-acceptance">>),
    {ok, 1} = msg_store_repo:mark_terminal(
        <<"c2c">>, <<"next-acceptance">>, <<"c2c_sequence_conflict">>
    ),
    exec(C, <<
        "UPDATE msg_store_staging SET processed_at=now()-interval '2 hours' "
        "WHERE msg_id IN ('new-acceptance','next-acceptance')"
    >>),
    ?assertEqual({ok, 1}, msg_store_repo:delete_processed(3600)),
    {ok, _, [{<<"next-acceptance">>, <<"c2c_sequence_conflict">>}]} = epgsql:equery(
        C,
        <<"SELECT msg_id,error_msg FROM msg_store_staging WHERE msg_id IN ('new-acceptance','next-acceptance')">>,
        []
    ).

roundtrip(#{conn := C}) ->
    exec(C, <<"BEGIN">>),
    ok = write(C, staging, 50, <<"roundtrip">>, 2),
    {ok, Down} = file:read_file("priv/migrations/00000194_c2c_order_write_guard.down.sql"),
    ok = exec(C, Down),
    apply_up(C),
    {ok, _, [{2}]} = epgsql:equery(
        C, <<"SELECT conv_seq FROM msg_store_staging WHERE msg_id='roundtrip'">>, []
    ),
    exec(C, <<"ROLLBACK">>).

apply_up(C) ->
    {ok, Up} = file:read_file("priv/migrations/00000194_c2c_order_write_guard.up.sql"),
    ok = exec(C, Up).

startup(#{conn := C, server := Server, db := Db}) ->
    {ok, Old} = application:get_env(imboy, super_account),
    application:set_env(imboy, super_account, Server#{database => Db}),
    try
        ok = imboy_migrate:migrate("priv/migrations"),
        exec(C, <<"ALTER TABLE msg_store_staging DISABLE TRIGGER trg_00_c2c_order_guard">>),
        ?assertError({c2c_order_guards_missing, 2}, imboy_migrate:migrate("priv/migrations"))
    after
        exec(C, <<"ALTER TABLE msg_store_staging ENABLE TRIGGER trg_00_c2c_order_guard">>),
        application:set_env(imboy, super_account, Old)
    end.

bootstrap(#{conn := C, server := Server, db := Db}) ->
    Head = inttest_marker_db:migration_head(),
    {ok, Old} = application:get_env(imboy, super_account),
    application:set_env(imboy, super_account, Server#{database => Db}),
    exec(C, <<"CREATE TEMP TABLE saved_history AS SELECT * FROM schema_migrations_history">>),
    try
        exec(C, <<"DELETE FROM schema_migrations_history">>),
        ?assertError(
            {migration_failed, {strict_bootstrap_needed, Head}},
            imboy_migrate:migrate("priv/migrations")
        )
    after
        exec(C, <<"INSERT INTO schema_migrations_history SELECT * FROM saved_history">>),
        application:set_env(imboy, super_account, Old)
    end.

insert(Table, Id, Msg, Seq) ->
    {Name, Extra, Values} =
        case Table of
            staging -> {"msg_store_staging", ",type", ",'c2c'"};
            formal -> {"msg_c2c", "", ""};
            archive -> {"msg_store", ",chat_type,conv_key", ",'c2c','c2c:10:20'"}
        end,
    Number =
        case Seq of
            null -> "NULL";
            _ -> integer_to_list(Seq)
        end,
    iolist_to_binary([
        "INSERT INTO ",
        Name,
        "(id,msg_id,from_id,to_id,payload,msg_type,conv_seq",
        Extra,
        ") VALUES (",
        integer_to_list(Id),
        ",'",
        Msg,
        "',10,20,'{}','text',",
        Number,
        Values,
        ")"
    ]).

write(C, Table, Id, Msg, Seq) -> exec(C, insert(Table, Id, Msg, Seq)).
exec(C, Sql) ->
    case epgsql:squery(C, Sql) of
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok;
        Results when is_list(Results) ->
            lists:foreach(fun(R) -> ?assertEqual(ok, element(1, R)) end, Results);
        Other ->
            error({sql_failed, Sql, Other})
    end.

rejected(C, Sql, Constraint) ->
    exec(C, <<"SAVEPOINT expected_failure">>),
    assert_rejected(epgsql:squery(C, Sql), Constraint),
    exec(C, <<"ROLLBACK TO SAVEPOINT expected_failure">>).

assert_rejected({error, {error, _, <<"23514">>, check_violation, _, Extra}}, Constraint) ->
    ?assertEqual(Constraint, proplists:get_value(constraint_name, Extra));
assert_rejected(Other, _) ->
    error({expected_constraint_failure, Other}).

wait_blocked(_, _, 0) ->
    error(no_database_lock_wait);
wait_blocked(C, Backend, N) ->
    {ok, _, [{Blocked}]} = epgsql:equery(
        C,
        <<"SELECT cardinality(pg_blocking_pids($1))>0">>,
        [Backend]
    ),
    case Blocked of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait_blocked(C, Backend, N - 1)
    end.
