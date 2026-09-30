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
        eunit:test(
            [
                {"enterprise group attachment access", fun() -> verify(C) end},
                {"enterprise channel attachment scope", fun() -> verify_channel(C) end}
            ],
            [verbose]
        )
    after
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
