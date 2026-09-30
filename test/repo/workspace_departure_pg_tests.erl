-module(workspace_departure_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

%% 显式运行，缺少独立测试库就失败；不连接配置中的业务数据库，不静默跳过。
run(SocketPath) ->
    {ok, Conn} = epgsql:connect(#{
        host => {local, SocketPath},
        port => 0,
        username => "departure_test",
        database => "postgres"
    }),
    try
        create_tables(Conn),
        eunit:test(
            [
                {"scope and idempotency", fun() -> scope_test(Conn) end},
                {"transaction rollback", fun() -> rollback_test(Conn) end},
                {"creator handover required", fun() -> handover_test(Conn) end}
            ],
            [verbose]
        )
    after
        epgsql:close(Conn)
    end.

create_tables(Conn) ->
    {ok, [], []} = epgsql:squery(Conn, <<
        "CREATE TEMP TABLE channel (id bigint PRIMARY KEY,"
        " workspace_id bigint, scope text, creator_uid bigint, status integer, name text,"
        " subscriber_count integer, updated_at timestamptz)"
    >>),
    {ok, [], []} = epgsql:squery(Conn, <<
        "CREATE TEMP TABLE channel_subscription (channel_id bigint,"
        " user_id bigint, status integer, PRIMARY KEY(channel_id,user_id))"
    >>),
    {ok, [], []} = epgsql:squery(Conn, <<
        "CREATE TEMP TABLE channel_admin (channel_id bigint,"
        " user_id bigint, role integer, PRIMARY KEY(channel_id,user_id))"
    >>).

reset(Conn) ->
    {ok, [], []} = epgsql:squery(Conn, <<"TRUNCATE channel, channel_subscription, channel_admin">>),
    {ok, 5} = epgsql:squery(Conn, <<
        "INSERT INTO channel VALUES"
        " (20,10,'workspace',1,1,'active',2,NULL),"
        " (21,10,'workspace',1,0,'archived',1,NULL),"
        " (22,10,'workspace',1,1,'admin only',0,NULL),"
        " (40,11,'workspace',2,1,'other workspace',1,NULL),"
        " (60,10,'personal',2,1,'personal',1,NULL)"
    >>),
    {ok, 4} = epgsql:squery(Conn, <<
        "INSERT INTO channel_subscription VALUES"
        " (20,2,1),(21,2,1),(40,2,1),(60,2,1)"
    >>),
    {ok, 4} = epgsql:squery(Conn, <<
        "INSERT INTO channel_admin VALUES"
        " (20,2,2),(22,2,1),(40,2,3),(60,2,3)"
    >>).

scope_test(Conn) ->
    reset(Conn),
    ?assertEqual(
        {ok, [
            #{<<"channel_id">> => 20},
            #{<<"channel_id">> => 21},
            #{<<"channel_id">> => 22}
        ]},
        workspace_member_repo:remove_channels_tx(Conn, 10, 2)
    ),
    ?assertEqual(
        [{20, 0}, {21, 0}, {40, 1}, {60, 1}],
        rows(
            Conn,
            <<"SELECT channel_id,status FROM channel_subscription ORDER BY channel_id">>
        )
    ),
    ?assertEqual(
        [{20, 1}, {21, 0}, {22, 0}, {40, 1}, {60, 1}],
        rows(
            Conn,
            <<"SELECT id,subscriber_count FROM channel ORDER BY id">>
        )
    ),
    ?assertEqual(
        [{40}, {60}], rows(Conn, <<"SELECT channel_id FROM channel_admin ORDER BY channel_id">>)
    ),
    ?assertEqual({ok, []}, workspace_member_repo:remove_channels_tx(Conn, 10, 2)),
    ?assertEqual(
        [{20, 1}, {21, 0}],
        rows(
            Conn,
            <<"SELECT id,subscriber_count FROM channel WHERE id IN (20,21) ORDER BY id">>
        )
    ).

rollback_test(Conn) ->
    reset(Conn),
    {ok, [], []} = epgsql:squery(Conn, <<"BEGIN">>),
    {ok, _} = workspace_member_repo:remove_channels_tx(Conn, 10, 2),
    {ok, [], []} = epgsql:squery(Conn, <<"ROLLBACK">>),
    ?assertEqual(
        [{20, 1}, {21, 1}, {40, 1}, {60, 1}],
        rows(
            Conn,
            <<"SELECT channel_id,status FROM channel_subscription ORDER BY channel_id">>
        )
    ),
    ?assertEqual(
        [{20, 2}, {21, 1}],
        rows(
            Conn,
            <<"SELECT id,subscriber_count FROM channel WHERE id IN (20,21) ORDER BY id">>
        )
    ),
    ?assertEqual(
        [{20}, {22}, {40}, {60}],
        rows(
            Conn,
            <<"SELECT channel_id FROM channel_admin ORDER BY channel_id">>
        )
    ).

handover_test(Conn) ->
    reset(Conn),
    {ok, 1} = epgsql:squery(Conn, <<"UPDATE channel SET creator_uid = 2 WHERE id = 20">>),
    ?assertThrow(
        {abort_tx, {membership_conflict, #{owned_channels := [_]}}},
        workspace_member_repo:remove_channels_tx(Conn, 10, 2)
    ),
    ?assertEqual(
        [{20, 1}, {21, 1}],
        rows(
            Conn,
            <<"SELECT channel_id,status FROM channel_subscription WHERE channel_id IN (20,21) ORDER BY channel_id">>
        )
    ).

rows(Conn, Sql) ->
    {ok, _, Rows} = epgsql:equery(Conn, Sql, []),
    Rows.
