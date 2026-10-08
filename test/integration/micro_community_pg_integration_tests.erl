-module(micro_community_pg_integration_tests).
-export([run/0, connect_owned/1]).
-include_lib("kernel/include/file.hrl").

-define(CONFIG,
    "/Users/leeyi/.codex/micro-community-runs/mc-20261008T083109Z/private/pg-owned.config"
).
-define(OWNER, <<"Owned by MC run mc-20261008T083109Z; synthetic micro-community test data only">>).
-define(UID, 9007199254742001).
-define(OTHER, 9007199254742002).
-define(G1, 9007199254742010).
-define(G2, 9007199254742011).
-define(C1, 9007199254742020).
-define(C2, 9007199254742021).
-define(M1, 9007199254742040).
-define(M2, 9007199254742041).
-define(M3, 9007199254742042).

%% Explicit opt-in entry point; broad EUnit discovery never connects to a DB.
%% Run in a fresh VM. Do not start imboy, eunit_runner or a migration runner.
run() ->
    _ = logger:set_primary_config(level, none),
    try
        assert(not lists:keymember(imboy, 1, application:which_applications()), imboy_started),
        assert(not lists:keymember(pooler, 1, application:which_applications()), pooler_started),
        Opts = private_config(),
        case connect_owned(Opts) of
            {ok, Observer} ->
                try
                    run_connected(Observer, Opts)
                after
                    epgsql:close(Observer)
                end;
            _ ->
                fail(owned_connection_unavailable)
        end
    catch
        throw:{micro_pg_failed, Code} -> {error, Code};
        _:_ -> {error, micro_pg_unexpected_failure}
    end.

private_config() ->
    Path = os:getenv("IMBOY_MICRO_PG_CONFIG"),
    assert(Path =:= ?CONFIG, private_config_path_required),
    case file:read_link_info(Path) of
        {ok, #file_info{type = regular, mode = Mode}} ->
            assert(Mode band 8#077 =:= 0, private_config_permissions);
        _ ->
            fail(private_config_unavailable)
    end,
    case file:consult(Path) of
        {ok, [#{database := "imboy_test_group", host := Host, port := 4323} = Opts]} when
            Host =:= "localhost"; Host =:= "127.0.0.1"
        ->
            maps:with([host, port, database, username, password, ssl, timeout, codecs], Opts);
        _ ->
            fail(private_config_target_rejected)
    end.

%% Used by the actual pool. Never return a raw connection failure with secrets.
connect_owned(Opts) ->
    try epgsql:connect(Opts) of
        {ok, Conn} ->
            try
                verify_owned(Conn),
                sql(Conn, <<"SET statement_timeout='15000'; SET lock_timeout='10000'">>),
                {ok, Conn}
            catch
                _:_ ->
                    epgsql:close(Conn),
                    {error, owned_target_rejected}
            end;
        _ ->
            {error, owned_connection_unavailable}
    catch
        _:_ -> {error, owned_connection_unavailable}
    end.

verify_owned(Conn) ->
    Rows = query(
        Conn,
        <<
            "SELECT current_database(), "
            "shobj_description(oid,'pg_database') FROM pg_database "
            "WHERE datname=current_database()"
        >>,
        []
    ),
    assert(Rows =:= [{<<"imboy_test_group">>, ?OWNER}], owned_database_mismatch).

run_connected(Observer, Opts) ->
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(pooler, pools, []),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    ok = elib_tsid:init(#{dc_id => 7, node_id => 127, dc_bits => 3, names => [group_log]}),
    Pool = #{
        name => pgsql,
        init_count => 2,
        max_count => 2,
        start_mfa => {?MODULE, connect_owned, [Opts]}
    },
    case pooler:new_pool(Pool) of
        {ok, _} ->
            try
                run_fixtures(Observer)
            after
                pooler:rm_pool(pgsql)
            end;
        _ ->
            fail(private_pool_creation_failed)
    end.

run_fixtures(Conn) ->
    fixture_absent(Conn),
    committed(Conn, fun setup_fixture/1),
    Outcome =
        try
            binding_and_repeat(Conn),
            ordinary_user_denied(Conn),
            rollback_keeps_binding(Conn),
            nonempty_denied(Conn),
            concurrent_single_winner(Conn),
            ok
        catch
            throw:{micro_pg_failed, Code} -> {error, Code};
            _:_ -> {error, micro_pg_unexpected_failure}
        end,
    Cleanup = cleanup_result(Conn),
    case {Outcome, Cleanup} of
        {ok, ok} -> ok;
        {{error, Failure}, _} -> fail(Failure);
        {ok, {error, Failure}} -> fail(Failure)
    end.

cleanup_result(Conn) ->
    try
        committed(Conn, fun cleanup_fixture/1),
        fixture_absent(Conn),
        io:format("MC_PG_FIXTURE_CLEANUP_PASS~n"),
        ok
    catch
        _:_ ->
            io:format("MC_PG_FIXTURE_CLEANUP_FAILED~n"),
            {error, fixture_cleanup_failed}
    end.

setup_fixture(Conn) ->
    exec(
        Conn,
        <<
            "INSERT INTO public.\"user\" "
            "(id,account,password,reg_ip,reg_cosv) VALUES "
            "($1,'mc_pg_2001','synthetic-disabled','127.0.0.1','MC synthetic'),"
            "($2,'mc_pg_2002','synthetic-disabled','127.0.0.1','MC synthetic')"
        >>,
        [?UID, ?OTHER]
    ),
    exec(
        Conn,
        <<
            "INSERT INTO public.\"group\" "
            "(id,owner_uid,creator_uid,title,type,join_limit,status,scope,e2ee_mode) "
            "VALUES ($1,$3,$3,'MC repo synthetic A',2,3,1,'personal',0),"
            "($2,$3,$3,'MC repo synthetic B',2,3,1,'personal',0)"
        >>,
        [?G1, ?G2, ?UID]
    ),
    exec(
        Conn,
        <<
            "INSERT INTO public.channel (id,name,creator_uid,scope,status) "
            "VALUES ($1,'MC repo synthetic A',$3,'personal',1),"
            "($2,'MC repo synthetic B',$3,'personal',1)"
        >>,
        [?C1, ?C2, ?UID]
    ),
    exec(
        Conn,
        <<
            "INSERT INTO public.group_member (id,group_id,user_id,role,status,is_join) "
            "VALUES ($1,$3,$5,5,1,true),($2,$4,$5,5,1,true)"
        >>,
        [?M1, ?M2, ?G1, ?G2, ?UID]
    ),
    ok.

binding_and_repeat(Conn) ->
    expect_binding(micro_community_binding_ds:set(?UID, ?C1, ?G1), ?G1),
    assert(binding(Conn, ?C1) =:= ?G1, binding_not_committed),
    assert(audit_count(Conn, ?G1) =:= 1, binding_audit_missing),
    assert(
        query(
            Conn,
            <<
                "SELECT option_uid,body::jsonb->>'old_group_id',"
                "body::jsonb->>'new_group_id' FROM public.group_log WHERE group_id=$1 AND type=301"
            >>,
            [?G1]
        ) =:= [{?UID, null, integer_to_binary(?G1)}],
        binding_audit_payload
    ),
    expect_binding(micro_community_binding_ds:set(?UID, ?C1, ?G1), ?G1),
    assert(audit_count(Conn, ?G1) =:= 1, duplicate_binding_audit),
    io:format("MC_PG_BIND_AND_REPEAT_PASS~n").

ordinary_user_denied(Conn) ->
    assert(
        micro_community_binding_ds:set(?OTHER, ?C1, null) =:= {error, forbidden},
        ordinary_user_not_denied
    ),
    assert(binding(Conn, ?C1) =:= ?G1, ordinary_user_wrote_binding),
    assert(audit_count(Conn, ?G1) =:= 1, ordinary_user_wrote_audit),
    io:format("MC_PG_ORDINARY_UID_ZERO_WRITE_PASS~n").

rollback_keeps_binding(Conn) ->
    Result = micro_community_binding_repo:transaction(fun(Tx) ->
        ok = micro_community_binding_repo:write_tx(Tx, ?UID, ?C1, {?G1, null}),
        throw({rollback, micro_pg_injected_abort})
    end),
    assert(Result =:= {rollback, micro_pg_injected_abort}, rollback_not_returned),
    assert(binding(Conn, ?C1) =:= ?G1, rollback_lost_binding),
    assert(audit_count(Conn, ?G1) =:= 1, rollback_committed_audit),
    io:format("MC_PG_REAL_TRANSACTION_ROLLBACK_PASS~n").

nonempty_denied(Conn) ->
    exec(
        Conn,
        <<
            "INSERT INTO public.group_member "
            "(id,group_id,user_id,role,status,is_join) VALUES($1,$2,$3,0,1,true)"
        >>,
        [?M3, ?G1, ?OTHER]
    ),
    assert(
        micro_community_binding_ds:set(?UID, ?C1, null) =:= {error, group_not_pristine},
        nonempty_group_not_denied
    ),
    assert(binding(Conn, ?C1) =:= ?G1, nonempty_unbind_changed_binding),
    assert(audit_count(Conn, ?G1) =:= 1, nonempty_unbind_changed_audit),
    exec(Conn, <<"DELETE FROM public.group_member WHERE id=$1">>, [?M3]),
    expect_binding(micro_community_binding_ds:set(?UID, ?C1, null), null),
    assert(binding(Conn, ?C1) =:= null, empty_unbind_failed),
    assert(audit_count(Conn, ?G1) =:= 2, empty_unbind_audit_missing),
    io:format("MC_PG_NONEMPTY_REFUSAL_AND_EMPTY_UNBIND_PASS~n").

concurrent_single_winner(Conn) ->
    Parent = self(),
    Workers = [
        spawn_monitor(fun() ->
            Parent ! {ready, self()},
            receive
                go ->
                    Result = micro_community_binding_ds:set(?UID, Cid, ?G2),
                    Parent ! {binding_result, self(), Result}
            after 5000 -> exit(start_timeout)
            end
        end)
     || Cid <- [?C1, ?C2]
    ],
    try
        [await_ready(Pid) || {Pid, _} <- Workers],
        [Pid ! go || {Pid, _} <- Workers],
        Results = [await_result(Pid, Ref) || {Pid, Ref} <- Workers],
        Success = {ok, #{discussion_group_id => ?G2}},
        assert(
            lists:sort(Results) =:= lists:sort([Success, {error, binding_conflict}]),
            concurrent_winner_count
        ),
        assert(
            query(
                Conn,
                <<
                    "SELECT count(*) FROM public.channel "
                    "WHERE id IN ($1,$2) AND discussion_group_id=$3"
                >>,
                [?C1, ?C2, ?G2]
            ) =:= [{1}],
            concurrent_database_winner_count
        ),
        assert(audit_count(Conn, ?G2) =:= 1, concurrent_audit_count),
        io:format("MC_PG_TWO_CHANNEL_CONCURRENCY_SINGLE_WINNER_PASS~n")
    after
        [stop_worker(Pid, Ref) || {Pid, Ref} <- Workers]
    end.

await_ready(Pid) ->
    receive
        {ready, Pid} -> ok
    after 5000 -> fail(concurrent_worker_not_ready)
    end.

await_result(Pid, Ref) ->
    receive
        {binding_result, Pid, Result} -> Result;
        {'DOWN', Ref, process, Pid, _} -> fail(concurrent_worker_failed)
    after 25000 -> fail(concurrent_worker_timeout)
    end.

stop_worker(Pid, Ref) ->
    case is_process_alive(Pid) of
        true -> exit(Pid, kill);
        false -> ok
    end,
    erlang:demonitor(Ref, [flush]).

cleanup_fixture(Conn) ->
    exec(Conn, <<"UPDATE public.channel SET discussion_group_id=NULL WHERE id IN ($1,$2)">>, [
        ?C1, ?C2
    ]),
    exec(
        Conn,
        <<
            "DELETE FROM public.group_log WHERE group_id IN ($1,$2) AND type=301 "
            "AND option_uid=$3"
        >>,
        [?G1, ?G2, ?UID]
    ),
    exec(
        Conn,
        <<"DELETE FROM public.msg_store_seq WHERE conv_key IN ($1,$2)">>,
        [conv_key(?G1), conv_key(?G2)]
    ),
    exec(Conn, <<"DELETE FROM public.group_member WHERE id IN ($1,$2,$3)">>, [?M1, ?M2, ?M3]),
    exec(Conn, <<"DELETE FROM public.channel WHERE id IN ($1,$2)">>, [?C1, ?C2]),
    exec(Conn, <<"DELETE FROM public.\"group\" WHERE id IN ($1,$2)">>, [?G1, ?G2]),
    exec(Conn, <<"DELETE FROM public.\"user\" WHERE id IN ($1,$2)">>, [?UID, ?OTHER]),
    ok.

fixture_absent(Conn) ->
    Checks = [
        {<<"public.\"user\"">>, [?UID, ?OTHER]},
        {<<"public.\"group\"">>, [?G1, ?G2]},
        {<<"public.channel">>, [?C1, ?C2]}
    ],
    [
        assert(
            query(Conn, <<"SELECT count(*) FROM ", Table/binary, " WHERE id IN ($1,$2)">>, Ids) =:=
                [{0}],
            fixture_id_collision_or_residue
        )
     || {Table, Ids} <- Checks
    ],
    assert(
        query(
            Conn,
            <<
                "SELECT count(*) FROM public.group_member "
                "WHERE id IN ($1,$2,$3) OR group_id IN ($4,$5)"
            >>,
            [?M1, ?M2, ?M3, ?G1, ?G2]
        ) =:= [{0}],
        fixture_member_collision_or_residue
    ),
    assert(
        query(
            Conn,
            <<"SELECT count(*) FROM public.group_log WHERE group_id IN ($1,$2)">>,
            [?G1, ?G2]
        ) =:= [{0}],
        fixture_audit_collision_or_residue
    ),
    assert(
        query(
            Conn,
            <<"SELECT count(*) FROM public.msg_store_seq WHERE conv_key IN ($1,$2)">>,
            [conv_key(?G1), conv_key(?G2)]
        ) =:= [{0}],
        fixture_sequence_collision_or_residue
    ).

binding(Conn, Cid) ->
    case query(Conn, <<"SELECT discussion_group_id FROM public.channel WHERE id=$1">>, [Cid]) of
        [{Value}] -> Value;
        _ -> fail(binding_oracle_unavailable)
    end.

audit_count(Conn, Gid) ->
    case
        query(Conn, <<"SELECT count(*) FROM public.group_log WHERE group_id=$1 AND type=301">>, [
            Gid
        ])
    of
        [{Count}] -> Count;
        _ -> fail(audit_oracle_unavailable)
    end.

expect_binding(Result, Gid) ->
    assert(Result =:= {ok, #{discussion_group_id => Gid}}, binding_result_mismatch).

committed(Conn, Fun) ->
    Result = epgsql:with_transaction(Conn, Fun, [{reraise, true}]),
    assert(Result =:= ok, fixture_transaction_failed).

query(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _, Rows} -> Rows;
        _ -> fail(oracle_query_failed)
    end.

exec(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, Count} when is_integer(Count) -> ok;
        _ -> fail(fixture_write_failed)
    end.

sql(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _, _} ->
            ok;
        {ok, _} ->
            ok;
        Results when is_list(Results) ->
            assert(
                lists:all(
                    fun
                        ({ok, _, _}) -> true;
                        ({ok, _}) -> true;
                        (_) -> false
                    end,
                    Results
                ),
                connection_settings_failed
            );
        _ ->
            fail(connection_settings_failed)
    end.

conv_key(Gid) -> <<"c2g:", (integer_to_binary(Gid))/binary>>.
assert(true, _) -> ok;
assert(false, Code) -> fail(Code).
fail(Code) -> throw({micro_pg_failed, Code}).
