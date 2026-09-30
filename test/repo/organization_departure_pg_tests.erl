-module(organization_departure_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

%% 只能指向新建独立测试库的 Unix socket；使用真实 Logic/DS/Repo/事务和迁移守卫。
run(SocketPath) ->
    {ok, Conn} = epgsql:connect(#{
        host => {local, SocketPath},
        port => 0,
        username => "departure_test",
        database => "postgres"
    }),
    try
        schema(Conn),
        setup_mocks(Conn),
        eunit:test(
            [
                {atom_to_list(Mode), fun() -> verify(Conn, Mode) end}
             || Mode <- [
                    leave,
                    offboard,
                    suspended,
                    archived,
                    dependency,
                    second_workspace_owner,
                    second_channel_owner,
                    platform,
                    platform_dependency,
                    platform_channel_owner,
                    platform_audit_failure
                ]
            ],
            [verbose]
        )
    after
        meck:unload(),
        epgsql:close(Conn)
    end.

setup_mocks(Conn) ->
    meck:new(config_ds, [non_strict, no_link]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:new(pooler, [non_strict, no_link]),
    meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
    meck:expect(pooler, return_member, fun(pgsql, C) when C =:= Conn -> ok end),
    meck:expect(pooler, return_member, fun(pgsql, C, _) when C =:= Conn -> ok end),
    meck:new(group_ds, [non_strict, no_link]),
    meck:expect(group_ds, leave, fun(2, Gid) ->
        ?assertEqual(
            [{0}],
            rows(
                Conn,
                <<"SELECT status FROM group_member WHERE group_id=$1">>,
                [Gid]
            )
        ),
        ?assertEqual(
            [{<<"removed">>}],
            rows(
                Conn,
                <<"SELECT status FROM organization_member WHERE organization_id=10 AND user_id=2">>,
                []
            )
        ),
        ok
    end),
    meck:new(imboy_cache, [non_strict, no_link]),
    meck:expect(imboy_cache, flush, fun(_) -> ok end),
    meck:new(imboy_domain_event, [non_strict, no_link]),
    meck:expect(imboy_domain_event, publish, fun(_) -> ok end),
    meck:new(adm_operation_log_ds, [non_strict, no_link]),
    meck:expect(adm_operation_log_ds, insert_tx, fun(
        C, 900, Action, 10, <<"organization">>, Detail, Ip
    ) ->
        ?assertEqual(Conn, C),
        ?assertEqual(<<"organization_member_remove">>, Action),
        ?assertEqual(2, maps:get(<<"target_user_id">>, Detail)),
        case Ip of
            audit_failure ->
                {error, injected_audit_failure};
            _ ->
                {ok, 1} = epgsql:equery(C, <<"INSERT INTO departure_audit VALUES($1,$2)">>, [
                    900, 10
                ]),
                ok
        end
    end).

schema(Conn) ->
    Statements = [
        "CREATE TABLE organization(id bigint PRIMARY KEY,owner_id bigint,status text,name text,branding jsonb,settings jsonb,created_at timestamptz,updated_at timestamptz)",
        "CREATE TABLE departure_audit(actor bigint,organization_id bigint)",
        "CREATE TABLE organization_member(organization_id bigint,user_id bigint,role text,status text,updated_at timestamptz,PRIMARY KEY(organization_id,user_id))",
        "CREATE TABLE workspace(id bigint PRIMARY KEY,organization_id bigint,owner_id bigint,status text)",
        "CREATE TABLE workspace_member(workspace_id bigint,user_id bigint,status text,updated_at timestamptz,PRIMARY KEY(workspace_id,user_id))",
        "CREATE TABLE project(id bigint,workspace_id bigint,owner_id bigint,name text)",
        "CREATE TABLE project_task(id bigint,project_id bigint,assignee_id bigint,title text,status text)",
        "CREATE TABLE \"group\"(id bigint,workspace_id bigint,owner_uid bigint,scope text,status integer,title text)",
        "CREATE TABLE group_member(id bigint,group_id bigint,user_id bigint,status integer,updated_at timestamptz)",
        "CREATE TABLE group_member_generation(group_id bigint,user_id bigint,end_seq bigint,close_reason text,updated_at timestamptz)",
        "CREATE TABLE msg_store_seq(conv_key text PRIMARY KEY,seq bigint)",
        "CREATE TABLE channel(id bigint PRIMARY KEY,workspace_id bigint,scope text,creator_uid bigint,status integer,name text,subscriber_count integer,updated_at timestamptz)",
        "CREATE TABLE channel_subscription(channel_id bigint,user_id bigint,status integer)",
        "CREATE TABLE channel_admin(channel_id bigint,user_id bigint,role integer)",
        "CREATE TABLE organization_business_identity_assignment(organization_id bigint,user_id bigint,status text)"
    ],
    lists:foreach(fun(Sql) -> execute(Conn, Sql) end, Statements),
    {ok, Migration} = file:read_file(
        "priv/migrations/00000114_enterprise_business_identity.up.sql"
    ),
    [_, Guard] = binary:split(
        Migration,
        <<"CREATE OR REPLACE FUNCTION fn_organization_member_offboarding_guard()">>
    ),
    execute(
        Conn,
        <<"CREATE OR REPLACE FUNCTION fn_organization_member_offboarding_guard()", Guard/binary>>
    ).

reset(Conn, Mode) ->
    execute(
        Conn,
        "TRUNCATE departure_audit,organization,organization_member,workspace,workspace_member,project,project_task,\"group\",group_member,group_member_generation,msg_store_seq,channel,channel_subscription,channel_admin,organization_business_identity_assignment"
    ),
    lists:foreach(fun(Sql) -> execute(Conn, Sql) end, [
        "INSERT INTO organization(id,owner_id,status) VALUES(10,1,'active'),(11,1,'active')",
        "INSERT INTO organization_member VALUES(10,1,'owner','active',NULL),(10,2,'member','active',NULL),(11,2,'member','active',NULL)",
        "INSERT INTO workspace VALUES(20,10,1,'active'),(21,10,1,'archived'),(22,11,1,'active')",
        "INSERT INTO workspace_member VALUES(20,2,'active',NULL),(21,2,'active',NULL),(22,2,'active',NULL)",
        "INSERT INTO \"group\" VALUES(30,20,1,'workspace',1,'a'),(31,21,1,'workspace',1,'b'),(32,22,1,'workspace',1,'other')",
        "INSERT INTO group_member VALUES(100,30,2,1,NULL),(101,31,2,1,NULL),(102,32,2,1,NULL)",
        "INSERT INTO group_member_generation VALUES(30,2,NULL,NULL,NULL),(31,2,NULL,NULL,NULL),(32,2,NULL,NULL,NULL)",
        "INSERT INTO channel VALUES(40,20,'workspace',1,1,'a',1,NULL),(41,21,'workspace',1,1,'b',1,NULL),(42,22,'workspace',2,1,'other',1,NULL),(43,20,'personal',2,1,'personal',1,NULL)",
        "INSERT INTO channel_subscription VALUES(40,2,1),(41,2,1),(42,2,1),(43,2,1)",
        "INSERT INTO channel_admin VALUES(40,2,2),(41,2,2),(42,2,3),(43,2,3)"
    ]),
    mode(Conn, Mode),
    lists:foreach(fun meck:reset/1, [group_ds, imboy_cache, imboy_domain_event]).

mode(Conn, dependency) ->
    execute(Conn, "INSERT INTO organization_business_identity_assignment VALUES(10,2,'active')");
mode(Conn, suspended) ->
    execute(
        Conn,
        "UPDATE organization_member SET status='suspended' WHERE organization_id=10 AND user_id=2"
    );
mode(Conn, archived) ->
    execute(Conn, "UPDATE organization SET status='archived' WHERE id=10");
mode(Conn, second_workspace_owner) ->
    execute(Conn, "UPDATE workspace SET owner_id=2 WHERE id=21");
mode(Conn, second_channel_owner) ->
    execute(Conn, "UPDATE channel SET creator_uid=2 WHERE id=41");
mode(Conn, platform_channel_owner) ->
    mode(Conn, second_channel_owner);
mode(Conn, platform_dependency) ->
    mode(Conn, dependency);
mode(_, _) ->
    ok.

verify(Conn, Mode) ->
    reset(Conn, Mode),
    Before = snapshot(Conn),
    Result =
        case Mode of
            offboard ->
                organization_member_logic:remove(1, 10, 2);
            M when M =:= platform; M =:= platform_dependency; M =:= platform_channel_owner ->
                organization_admin_logic:admin_member_remove(900, 10, 2, #{});
            platform_audit_failure ->
                organization_admin_logic:admin_member_remove(900, 10, 2, #{ip => audit_failure});
            _ ->
                organization_member_logic:leave(2, 10)
        end,
    case
        lists:member(Mode, [
            dependency,
            second_workspace_owner,
            second_channel_owner,
            platform_dependency,
            platform_channel_owner,
            platform_audit_failure
        ])
    of
        true ->
            Code =
                case Mode of
                    platform_audit_failure -> 500;
                    _ -> 409
                end,
            ?assertMatch({error, {Code, _}}, Result),
            ?assertEqual(Before, snapshot(Conn)),
            ?assertEqual(0, meck:num_calls(group_ds, leave, '_')),
            ?assertEqual(0, meck:num_calls(imboy_cache, flush, '_')),
            ?assertEqual(0, meck:num_calls(imboy_domain_event, publish, '_'));
        false ->
            ?assertMatch({ok, #{affected_workspaces := [_, _]}}, Result),
            Audit =
                case Mode of
                    platform -> [{900, 10}];
                    _ -> []
                end,
            ?assertEqual(Audit, rows(Conn, <<"SELECT * FROM departure_audit">>, [])),
            successful(Conn)
    end.

successful(Conn) ->
    ?assertEqual(
        [{20, <<"removed">>}, {21, <<"removed">>}, {22, <<"active">>}],
        rows(Conn, <<"SELECT workspace_id,status FROM workspace_member ORDER BY workspace_id">>, [])
    ),
    ?assertEqual(
        [{30, 0}, {31, 0}, {32, 1}],
        rows(Conn, <<"SELECT group_id,status FROM group_member ORDER BY group_id">>, [])
    ),
    ?assertEqual(
        [{40, 0}, {41, 0}, {42, 1}, {43, 1}],
        rows(Conn, <<"SELECT channel_id,status FROM channel_subscription ORDER BY channel_id">>, [])
    ),
    ?assertEqual(
        [{42}, {43}], rows(Conn, <<"SELECT channel_id FROM channel_admin ORDER BY channel_id">>, [])
    ),
    ?assertEqual(
        [{30, 0}, {31, 0}, {32, null}],
        rows(Conn, <<"SELECT group_id,end_seq FROM group_member_generation ORDER BY group_id">>, [])
    ),
    ?assertEqual(
        [{11, <<"active">>}],
        rows(
            Conn,
            <<"SELECT organization_id,status FROM organization_member WHERE organization_id=11 AND user_id=2">>,
            []
        )
    ),
    ?assertEqual(2, meck:num_calls(group_ds, leave, '_')).

snapshot(Conn) ->
    [
        rows(Conn, iolist_to_binary(["SELECT * FROM ", Table, " ORDER BY 1,2"]), [])
     || Table <- [
            "departure_audit",
            "organization_member",
            "workspace_member",
            "group_member",
            "group_member_generation",
            "msg_store_seq",
            "channel",
            "channel_subscription",
            "channel_admin"
        ]
    ].

rows(Conn, Sql, Params) ->
    {ok, _, Rows} = epgsql:equery(Conn, Sql, Params),
    Rows.

execute(Conn, Sql) ->
    Results =
        case epgsql:squery(Conn, Sql) of
            L when is_list(L) -> L;
            R -> [R]
        end,
    lists:foreach(fun(R) -> ?assertEqual(ok, element(1, R)) end, Results).
