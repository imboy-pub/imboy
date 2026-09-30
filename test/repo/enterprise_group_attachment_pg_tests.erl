-module(enterprise_group_attachment_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

%% 仅连接任务新建的 Unix socket 合成库，不访问业务库。
run(SocketPath) ->
    {ok, C} = epgsql:connect(#{
        host => {local, SocketPath},
        port => 0,
        username => "departure_test",
        database => "postgres"
    }),
    try
        schema(C),
        setup_pool(C),
        eunit:test(
            [
                {"enterprise group attachment access", fun() -> verify(C) end},
                {"enterprise channel attachment scope", fun() -> verify_channel(C) end}
            ] ++
                [
                    {atom_to_list(Kind) ++ " " ++ atom_to_list(Mode), fun() ->
                        verify_upload_lock(C, SocketPath, Kind, Mode)
                    end}
                 || Kind <- [group, channel], Mode <- [suspend, remove_workspace]
                ],
            [verbose]
        )
    after
        meck:unload(),
        epgsql:close(C)
    end.

schema(C) ->
    sql(C, <<
        "CREATE TABLE organization(id bigint PRIMARY KEY,status text);"
        "CREATE TABLE organization_member(organization_id bigint,user_id bigint,status text,role text);"
        "CREATE TABLE workspace(id bigint PRIMARY KEY,organization_id bigint,status text);"
        "CREATE TABLE workspace_member(workspace_id bigint,user_id bigint,status text);"
        "CREATE TABLE \"group\"(id bigint PRIMARY KEY,status int,scope text,workspace_id bigint);"
        "CREATE TABLE channel(id bigint PRIMARY KEY,status int,scope text,workspace_id bigint);"
        "CREATE TABLE group_member(group_id bigint,user_id bigint,status int);"
        "CREATE TABLE group_member_generation(group_id bigint,user_id bigint,start_seq bigint,end_seq bigint);"
        "CREATE TABLE group_file(id bigint,group_id bigint,status int);"
        "CREATE TABLE attachment(path text,scope text,scope_ref text,status int,"
        "anchor_conv_seq bigint,group_file_id bigint);"
        "INSERT INTO organization VALUES(10,'active'),(20,'active');"
        "INSERT INTO organization_member VALUES(10,1,'active','member'),(20,1,'active','member');"
        "INSERT INTO workspace VALUES(100,10,'active'),(200,20,'active'),(300,NULL,'active');"
        "INSERT INTO workspace_member VALUES(100,1,'active'),(200,1,'active'),(300,1,'active');"
        "INSERT INTO \"group\" VALUES(11,1,'workspace',100),(22,1,'workspace',200),"
        "(33,1,'personal',NULL),(44,1,'workspace',300);"
        "INSERT INTO channel VALUES(11,1,'workspace',100),(22,1,'workspace',200),"
        "(33,1,'personal',NULL),(44,1,'workspace',300),(55,0,'personal',NULL);"
        "INSERT INTO group_member SELECT id,1,1 FROM \"group\";"
        "INSERT INTO group_member_generation SELECT id,1,5,NULL FROM \"group\";"
        "INSERT INTO group_file VALUES(9,11,1);"
        "INSERT INTO attachment VALUES('a','group','11',1,5,NULL),"
        "('b','group','22',1,5,NULL),('personal','group','33',1,5,NULL),"
        "('ws-personal','group','44',1,5,NULL),('file','group','11',1,NULL,9);"
    >>).

verify(C) ->
    ?assert(allowed(C, <<"a">>)),
    ?assert(allowed(C, <<"file">>)),
    sql(C, <<"UPDATE organization_member SET status='suspended' WHERE organization_id=10">>),
    ?assertNot(allowed(C, <<"a">>)),
    ?assertNot(allowed(C, <<"file">>)),
    ?assert(allowed(C, <<"b">>)),
    ?assert(allowed(C, <<"personal">>)),
    ?assert(allowed(C, <<"ws-personal">>)),
    sql(C, <<"UPDATE organization_member SET status='active' WHERE organization_id=10">>),
    ?assert(allowed(C, <<"a">>)),
    ?assert(allowed(C, <<"file">>)),
    sql(C, <<"UPDATE organization_member SET status='removed' WHERE organization_id=10">>),
    ?assertNot(allowed(C, <<"a">>)),
    sql(C, <<"DELETE FROM organization_member WHERE organization_id=10">>),
    ?assertNot(allowed(C, <<"a">>)),
    sql(C, <<
        "INSERT INTO organization_member VALUES(10,1,'active','member');"
        "UPDATE workspace_member SET status='removed' WHERE workspace_id=100"
    >>),
    ?assertNot(allowed(C, <<"a">>)),
    sql(C, <<"UPDATE organization_member SET role='admin' WHERE organization_id=10">>),
    ?assert(allowed(C, <<"a">>)),
    sql(C, <<"UPDATE organization_member SET status='suspended' WHERE organization_id=10">>),
    ?assertNot(allowed(C, <<"a">>)),
    sql(C, <<
        "UPDATE organization_member SET status='active',role='member' WHERE organization_id=10;"
        "UPDATE workspace_member SET status='active' WHERE workspace_id=100;"
        "UPDATE workspace SET status='archived' WHERE id=100"
    >>),
    ?assert(allowed(C, <<"a">>)),
    sql(C, <<"UPDATE organization SET status='archived' WHERE id=10">>),
    ?assertNot(allowed(C, <<"a">>)),
    sql(C, <<
        "UPDATE organization SET status='active' WHERE id=10;"
        "UPDATE attachment SET anchor_conv_seq=4 WHERE path='a'"
    >>),
    ?assertNot(allowed(C, <<"a">>)),
    ?assert(allowed(C, <<"file">>)),
    sql(C, <<"UPDATE group_file SET status=0 WHERE id=9">>),
    ?assertNot(allowed(C, <<"file">>)),
    sql(C, <<
        "UPDATE group_file SET status=1 WHERE id=9;"
        "UPDATE group_member_generation SET end_seq=10 WHERE group_id=11"
    >>),
    ?assertNot(allowed(C, <<"file">>)),
    {ok, _, [{5}]} = epgsql:equery(C, <<"SELECT count(*) FROM attachment WHERE status=1">>, []).

allowed(C, Key) ->
    {ok, _, [{Result}]} = epgsql:equery(
        C,
        attachment_repo:group_access_sql(<<"public.attachment">>),
        [Key, 1]
    ),
    Result.

sql(C, Query) ->
    R = epgsql:squery(C, Query),
    lists:foreach(
        fun(Item) -> ?assertNotMatch({error, _}, Item) end,
        case is_list(R) of
            true -> R;
            false -> [R]
        end
    ).

verify_channel(C) ->
    sql(C, <<
        "UPDATE workspace SET status='active';"
        "UPDATE organization_member SET status='active',role='member';"
        "UPDATE workspace_member SET status='active'"
    >>),
    ?assert(channel_allowed(C, 11)),
    ?assert(channel_allowed(C, 33)),
    ?assert(channel_allowed(C, 44)),
    ?assertNot(channel_allowed(C, 55)),
    ?assertNot(channel_allowed(C, 999)),
    sql(C, <<"UPDATE organization_member SET status='suspended' WHERE organization_id=10">>),
    ?assertNot(channel_allowed(C, 11)),
    ?assert(channel_allowed(C, 22)),
    ?assert(channel_allowed(C, 33)),
    sql(C, <<"UPDATE organization_member SET status='removed' WHERE organization_id=10">>),
    ?assertNot(channel_allowed(C, 11)),
    sql(C, <<"DELETE FROM organization_member WHERE organization_id=10">>),
    ?assertNot(channel_allowed(C, 11)),
    sql(C, <<
        "INSERT INTO organization_member VALUES(10,1,'active','member');"
        "UPDATE workspace_member SET status='removed' WHERE workspace_id=100"
    >>),
    ?assertNot(channel_allowed(C, 11)),
    sql(C, <<"UPDATE organization_member SET role='admin' WHERE organization_id=10">>),
    ?assert(channel_allowed(C, 11)),
    sql(C, <<"UPDATE organization SET status='archived' WHERE id=10">>),
    ?assertNot(channel_allowed(C, 11)),
    sql(C, <<
        "UPDATE organization SET status='active' WHERE id=10;"
        "UPDATE workspace SET status='archived' WHERE id=100"
    >>),
    ?assert(channel_allowed(C, 11)),
    sql(C, <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=300">>),
    ?assertNot(channel_allowed(C, 44)).

channel_allowed(C, ChannelId) ->
    {ok, _, [{Result}]} = epgsql:equery(
        C,
        attachment_repo:channel_scope_access_sql(),
        [ChannelId, 1]
    ),
    Result.

setup_pool(C) ->
    meck:new(config_ds, [non_strict, no_link]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:new(pooler, [non_strict, no_link]),
    meck:expect(pooler, take_member, fun(pgsql) -> C end),
    meck:expect(pooler, return_member, fun(pgsql, C0) when C0 =:= C -> ok end),
    meck:expect(pooler, return_member, fun(pgsql, C0, _) when C0 =:= C -> ok end).

verify_upload_lock(C, Socket, Kind, Mode) ->
    sql(C, <<
        "UPDATE workspace SET status='active';"
        "UPDATE organization_member SET status='active',role='member';"
        "UPDATE workspace_member SET status='active'"
    >>),
    sql(C, <<"BEGIN">>),
    ?assertEqual(ok, attachment_ds:ensure_upload_scope_tx(C, {Kind, 11}, 1)),
    Token = make_ref(),
    Parent = self(),
    spawn(fun() -> revoke_concurrently(Parent, Token, Socket, Mode) end),
    BackendPid =
        receive
            {Token, backend, Pid} -> Pid
        after 2000 -> error(worker_not_started)
        end,
    await_lock(C, BackendPid, 200),
    Key = iolist_to_binary([atom_to_binary(Kind), <<"-">>, atom_to_binary(Mode)]),
    {ok, 1} = epgsql:equery(
        C,
        <<"INSERT INTO attachment VALUES($1,'group','11',1,5,NULL)">>,
        [Key]
    ),
    sql(C, <<"COMMIT">>),
    receive
        {Token, result, Result} -> ?assertEqual({ok, 1}, Result)
    after 2000 -> error(revoke_not_finished)
    end,
    sql(C, <<"BEGIN">>),
    ?assertEqual({error, forbidden}, attachment_ds:ensure_upload_scope_tx(C, {Kind, 11}, 1)),
    sql(C, <<"ROLLBACK">>),
    {ok, _, [{1}]} = epgsql:equery(
        C,
        <<"SELECT count(*) FROM attachment WHERE path=$1">>,
        [Key]
    ).

revoke_concurrently(Parent, Token, Socket, Mode) ->
    {ok, C} = epgsql:connect(#{
        host => {local, Socket},
        port => 0,
        username => "departure_test",
        database => "postgres"
    }),
    try
        {ok, _, [{BackendPid}]} = epgsql:equery(C, <<"SELECT pg_backend_pid()">>, []),
        Parent ! {Token, backend, BackendPid},
        Query =
            case Mode of
                suspend ->
                    <<"UPDATE organization_member SET status='suspended' WHERE organization_id=10 AND user_id=1">>;
                remove_workspace ->
                    <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=100 AND user_id=1">>
            end,
        Parent ! {Token, result, epgsql:equery(C, Query, [])}
    after
        epgsql:close(C)
    end.

await_lock(_C, _Pid, 0) ->
    error(revocation_did_not_wait_for_upload);
await_lock(C, Pid, Attempts) ->
    {ok, _, [{Blocked}]} = epgsql:equery(
        C,
        <<"SELECT cardinality(pg_blocking_pids($1)) > 0">>,
        [Pid]
    ),
    case Blocked of
        true ->
            ok;
        false ->
            timer:sleep(10),
            await_lock(C, Pid, Attempts - 1)
    end.
