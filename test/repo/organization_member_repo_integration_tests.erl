-module(organization_member_repo_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PG_HOST, "127.0.0.1").
-define(PG_PORT, 4323).
-define(PG_USER, <<"imboy_user">>).
-define(PG_PASS, <<"abc54321">>).
-define(PG_DB, <<"moya_zcode_181902">>).

-define(OWNER, 99113001).
-define(MEMBER, 99113002).
-define(ORG, 99113101).
-define(WS, 99113201).
-define(GROUP, 99113301).

organization_member_persistence_test_() ->
    {setup, fun setup_conn/0, fun close_conn/1, fun
        (skip) ->
            [];
        (Conn) ->
            ?_test(with_rollback(Conn, fun verify_persistence/1))
    end}.

setup_conn() ->
    try
        {ok, _} = application:ensure_all_started(epgsql),
        {ok, Conn} = epgsql:connect(#{
            host => ?PG_HOST,
            port => ?PG_PORT,
            username => ?PG_USER,
            password => ?PG_PASS,
            database => ?PG_DB,
            timeout => 5000
        }),
        Conn
    catch
        _:_ -> skip
    end.

close_conn(skip) ->
    ok;
close_conn(Conn) ->
    try
        epgsql:close(Conn)
    catch
        _:_ -> ok
    end,
    ok.

with_rollback(Conn, Fun) ->
    ok = squery(Conn, <<"BEGIN">>),
    try
        ok = apply_migration(Conn),
        Fun(Conn)
    after
        ok = squery(Conn, <<"ROLLBACK">>)
    end.

apply_migration(Conn) ->
    {ok, Sql} = file:read_file("priv/migrations/00000113_organization_member.up.sql"),
    case epgsql:squery(Conn, Sql) of
        {error, Reason} -> erlang:error({migration_failed, Reason});
        _ -> ok
    end.

verify_persistence(Conn) ->
    assert_schema(Conn),
    seed(Conn),
    assert_owner_synced(Conn),
    assert_member_lifecycle(Conn),
    assert_workspace_membership_unchanged(Conn),
    assert_owner_guard(Conn),
    assert_owner_transfer_rolls_back_on_failure(Conn),
    assert_owner_transfer_keeps_workspace_unchanged(Conn).

assert_schema(Conn) ->
    {ok, _, [{<<"organization_member">>}]} = epgsql:equery(
        Conn, <<"SELECT to_regclass('public.organization_member')::text">>, []
    ),
    {ok, _, [{2}]} = epgsql:equery(
        Conn,
        <<"SELECT COUNT(*) FROM pg_trigger WHERE tgname IN ",
            "('trg_organization_owner_member_sync',",
            " 'trg_organization_primary_owner_member_guard') AND NOT tgisinternal">>,
        []
    ).

seed(Conn) ->
    ok = exec(Conn, <<
        "INSERT INTO \"user\" (id,password,account,reg_ip,reg_cosv) VALUES "
        "(99113001,'x','org_member_owner','127.0.0.1','x'),"
        "(99113002,'x','org_member_peer','127.0.0.1','x')"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO organization (id,name,owner_id) "
        "VALUES (99113101,'Organization member integration',99113001)"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO workspace (id,name,owner_id,organization_id) "
        "VALUES (99113201,'Cross organization workspace',99113001,99113101)"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO workspace_member "
        "(workspace_id,user_id,role,invited_by,joined_at,status) VALUES "
        "(99113201,99113001,'owner',NULL,CURRENT_TIMESTAMP,'active'),"
        "(99113201,99113002,'member',99113001,CURRENT_TIMESTAMP,'active')"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO \"group\" "
        "(id,owner_uid,creator_uid,scope,workspace_id,title) "
        "VALUES (99113301,99113001,99113001,'workspace',99113201,'Shared group')"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO group_member (id,group_id,user_id,role,is_join,status) "
        "VALUES (99113401,99113301,99113002,0,true,1)"
    >>),
    ok = exec(Conn, <<"SET CONSTRAINTS ALL IMMEDIATE">>).

assert_owner_synced(Conn) ->
    {ok, Owner} = organization_member_repo:find_active_tx(
        Conn, ?ORG, ?OWNER, <<"organization_id,user_id,role,status">>
    ),
    ?assertEqual(<<"owner">>, maps:get(<<"role">>, Owner)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Owner)).

assert_member_lifecycle(Conn) ->
    ?assertMatch(
        {ok, changed, _},
        organization_member_repo:upsert_active_tx(
            Conn, ?ORG, ?MEMBER, <<"member">>, ?OWNER
        )
    ),
    ?assertMatch(
        {ok, unchanged, _},
        organization_member_repo:upsert_active_tx(
            Conn, ?ORG, ?MEMBER, <<"member">>, ?OWNER
        )
    ),
    ok = organization_member_repo:update_role_tx(Conn, ?ORG, ?MEMBER, <<"admin">>),
    ok = organization_member_repo:remove_tx(Conn, ?ORG, ?MEMBER),
    ?assertEqual(
        {error, not_found},
        organization_member_repo:find_active_tx(Conn, ?ORG, ?MEMBER, <<"role">>)
    ),
    ?assertMatch(
        {ok, changed, _},
        organization_member_repo:upsert_active_tx(
            Conn, ?ORG, ?MEMBER, <<"member">>, ?OWNER
        )
    ).

assert_owner_guard(Conn) ->
    assert_rejected_write(Conn, fun() ->
        organization_member_repo:update_role_tx(Conn, ?ORG, ?OWNER, <<"member">>)
    end),
    assert_rejected_write(Conn, fun() ->
        organization_member_repo:remove_tx(Conn, ?ORG, ?OWNER)
    end),
    {ok, Owner} = organization_member_repo:find_active_tx(
        Conn, ?ORG, ?OWNER, <<"role,status">>
    ),
    ?assertEqual(<<"owner">>, maps:get(<<"role">>, Owner)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Owner)).

assert_rejected_write(Conn, Fun) ->
    ok = squery(Conn, <<"SAVEPOINT organization_owner_guard">>),
    ?assertMatch({error, _}, Fun()),
    ok = squery(Conn, <<"ROLLBACK TO SAVEPOINT organization_owner_guard">>),
    ok = squery(Conn, <<"RELEASE SAVEPOINT organization_owner_guard">>).

assert_workspace_membership_unchanged(Conn) ->
    ok = organization_member_repo:remove_tx(Conn, ?ORG, ?MEMBER),
    {ok, _, [{<<"active">>}]} = epgsql:equery(
        Conn,
        <<"SELECT status FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
        [?WS, ?MEMBER]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        <<"SELECT status FROM group_member WHERE group_id = $1 AND user_id = $2">>,
        [?GROUP, ?MEMBER]
    ),
    ?assertMatch(
        {ok, changed, _},
        organization_member_repo:upsert_active_tx(
            Conn, ?ORG, ?MEMBER, <<"member">>, ?OWNER
        )
    ).

assert_owner_transfer_rolls_back_on_failure(Conn) ->
    with_bound_logic_tx(Conn, fun() ->
        meck:new(organization_member_repo, [passthrough, no_link]),
        meck:expect(
            organization_member_repo,
            update_role_tx,
            fun(_Conn, ?ORG, ?OWNER, <<"admin">>) -> {error, injected_role_update} end
        ),
        try
            ?assertMatch(
                {error, {500, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG, ?MEMBER)
            )
        after
            meck:unload(organization_member_repo)
        end
    end),
    {ok, _, [{?OWNER}]} = epgsql:equery(
        Conn, <<"SELECT owner_id FROM organization WHERE id = $1">>, [?ORG]
    ),
    {ok, _, [{<<"owner">>}]} = epgsql:equery(
        Conn,
        <<"SELECT role FROM organization_member WHERE organization_id = $1 AND user_id = $2">>,
        [?ORG, ?OWNER]
    ),
    {ok, _, [{<<"member">>}]} = epgsql:equery(
        Conn,
        <<"SELECT role FROM organization_member WHERE organization_id = $1 AND user_id = $2">>,
        [?ORG, ?MEMBER]
    ).

assert_owner_transfer_keeps_workspace_unchanged(Conn) ->
    with_bound_logic_tx(Conn, fun() ->
        ?assertMatch(
            {ok, #{
                organization_id := ?ORG,
                owner_id := ?MEMBER,
                previous_owner_id := ?OWNER,
                previous_owner_role := <<"admin">>
            }},
            organization_member_logic:transfer_owner(?OWNER, ?ORG, ?MEMBER)
        )
    end),
    {ok, NewOwner} = organization_member_repo:find_active_tx(
        Conn, ?ORG, ?MEMBER, <<"role,status">>
    ),
    {ok, PreviousOwner} = organization_member_repo:find_active_tx(
        Conn, ?ORG, ?OWNER, <<"role,status">>
    ),
    ?assertEqual(<<"owner">>, maps:get(<<"role">>, NewOwner)),
    ?assertEqual(<<"admin">>, maps:get(<<"role">>, PreviousOwner)),
    {ok, _, [{?OWNER, ?ORG}]} = epgsql:equery(
        Conn,
        <<"SELECT owner_id,organization_id FROM workspace WHERE id = $1">>,
        [?WS]
    ),
    {ok, _, [{<<"owner">>}]} = epgsql:equery(
        Conn,
        <<"SELECT role FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
        [?WS, ?OWNER]
    ),
    {ok, _, [{<<"member">>}]} = epgsql:equery(
        Conn,
        <<"SELECT role FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
        [?WS, ?MEMBER]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        <<"SELECT status FROM group_member WHERE group_id = $1 AND user_id = $2">>,
        [?GROUP, ?MEMBER]
    ).

with_bound_logic_tx(Conn, Fun) ->
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, with_tx, fun(TxFun) -> savepoint_tx(Conn, TxFun) end),
    try
        Fun()
    after
        meck:unload(elib_pg)
    end.

savepoint_tx(Conn, Fun) ->
    ok = squery(Conn, <<"SAVEPOINT organization_owner_logic">>),
    try
        Result = Fun(Conn),
        ok = squery(Conn, <<"RELEASE SAVEPOINT organization_owner_logic">>),
        Result
    catch
        throw:{abort_tx, Reason} ->
            rollback_savepoint(Conn),
            {error, Reason};
        Class:Reason:Stacktrace ->
            rollback_savepoint(Conn),
            erlang:raise(Class, Reason, Stacktrace)
    end.

rollback_savepoint(Conn) ->
    ok = squery(Conn, <<"ROLLBACK TO SAVEPOINT organization_owner_logic">>),
    ok = squery(Conn, <<"RELEASE SAVEPOINT organization_owner_logic">>).

exec(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok;
        {error, Reason} ->
            erlang:error({sql_error, Reason});
        Results when is_list(Results) ->
            case [Reason || {error, Reason} <- Results] of
                [] -> ok;
                [Reason | _] -> erlang:error({sql_error, Reason})
            end
    end.

squery(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.
