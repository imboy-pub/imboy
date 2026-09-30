-module(workspace_member_groups_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

run(Socket) ->
    {ok, C} = epgsql:connect(#{
        host => {local, Socket},
        port => 0,
        username => "departure_test",
        database => "postgres",
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }),
    try
        fixture(C),
        meck:new(config_ds, [non_strict, no_link]),
        meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
        meck:new(pooler, [non_strict, no_link]),
        meck:expect(pooler, take_member, fun(pgsql) -> C end),
        meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
        meck:expect(pooler, return_member, fun(pgsql, _, _) -> ok end),
        eunit:test(
            [
                {"205 member groups span three pages without directory leakage", fun() ->
                    ?assertEqual(lists:seq(1, 205), pages(0)),
                    ?assertEqual(lists:seq(1, 205), pages(0, true)),
                    ?assertMatch(
                        {ok, #{has_more := true, next_cursor := 200}},
                        group_logic:list_member_workspace_groups(100, 1, 0, 200)
                    ),
                    ?assertEqual(
                        {ok, #{list => [], has_more => false, next_cursor => 0}},
                        group_logic:list_member_workspace_groups(100, 2, 0, 100)
                    )
                end},
                {"current revocation and archived read", fun() -> revocation(C) end},
                {"preview honors generation, expiry, deletion and acknowledged delivery", fun() ->
                    preview(C)
                end},
                {"preview index migration is reversible", fun() -> preview_index(C) end},
                {"invalid cursor and size do not query", fun() ->
                    lists:foreach(
                        fun({Cursor, Limit}) ->
                            ?assertMatch(
                                {error, _},
                                group_logic:list_member_workspace_groups(100, 1, Cursor, Limit)
                            )
                        end,
                        [{-1, 10}, {9223372036854775808, 10}, {0, 0}, {0, 201}]
                    )
                end}
            ],
            [verbose]
        )
    after
        meck:unload(),
        epgsql:close(C)
    end.

pages(Cursor) -> pages(Cursor, false).

pages(Cursor, Preview) ->
    {ok, #{list := List, has_more := More, next_cursor := Next}} =
        group_logic:list_member_workspace_groups(100, 1, Cursor, 100, Preview),
    Ids = [maps:get(<<"id">>, Row) || Row <- List],
    case More of
        true ->
            ?assert(Next > Cursor),
            Ids ++ pages(Next, Preview);
        false ->
            ?assertEqual(0, Next),
            Ids
    end.

revocation(C) ->
    lists:foreach(
        fun({Change, Restore}) ->
            sql(C, Change),
            ?assertEqual([], pages(0)),
            ?assertEqual([], pages(0, true)),
            sql(C, Restore)
        end,
        [
            {<<"UPDATE workspace_member SET status='removed'">>,
                <<"UPDATE workspace_member SET status='active'">>},
            {<<"UPDATE workspace SET status='deleted'">>,
                <<"UPDATE workspace SET status='active'">>},
            {<<"UPDATE organization SET status='archived'">>,
                <<"UPDATE organization SET status='active'">>},
            {<<"UPDATE group_member SET status=0">>, <<"UPDATE group_member SET status=1">>},
            {<<"UPDATE group_member_generation SET end_seq=10">>,
                <<"UPDATE group_member_generation SET end_seq=NULL">>}
        ]
    ),
    sql(C, <<"UPDATE workspace SET status='archived'">>),
    ?assertEqual(lists:seq(1, 205), pages(0)),
    sql(
        C,
        <<"UPDATE workspace SET status='active'; DELETE FROM group_member_generation WHERE group_id=205">>
    ),
    ?assertEqual(lists:seq(1, 204), pages(0)).

preview(C) ->
    sql(C, <<
        "UPDATE group_member_generation SET start_seq=3 WHERE group_id=1;"
        "INSERT INTO msg_c2g(msg_id,to_id,msg_type,payload,server_ts,created_at,expire_at) "
        "SELECT 'msg'||n,1,'text',jsonb_build_object('payload',jsonb_build_object('text','message'||n)),"
        "NOW(),NOW()+n*INTERVAL '1 second',CASE WHEN n=6 THEN NOW()-INTERVAL '1 second' ELSE NULL END "
        "FROM generate_series(1,6) n;"
        "INSERT INTO msg_c2g_timeline SELECT msg_id,1,to_id,created_at,substring(msg_id from 4)::bigint,true FROM msg_c2g;"
    >>),
    ?assertEqual(5, latest_seq()),
    sql(C, <<"DELETE FROM msg_c2g_timeline WHERE msg_id='msg5'">>),
    ?assertEqual(4, latest_seq()),
    sql(C, <<"DELETE FROM msg_c2g WHERE msg_id='msg4'">>),
    ?assertEqual(3, latest_seq()),
    sql(
        C,
        <<"UPDATE msg_c2g SET payload='{\"action\":\"message_revoke_ack\",\"payload\":{\"text\":\"\"}}' WHERE msg_id='msg3'">>
    ),
    #{<<"payload">> := #{<<"action">> := <<"message_revoke_ack">>}} = latest_message(),
    sql(C, <<"UPDATE group_member_generation SET start_seq=7 WHERE group_id=1">>),
    ?assertEqual(null, latest_message()),
    sql(C, <<"UPDATE group_member_generation SET start_seq=1,end_seq=7 WHERE group_id=1">>),
    {ok, #{list := List}} = group_logic:list_member_workspace_groups(100, 1, 0, 200, true),
    ?assertNot(lists:any(fun(R) -> maps:get(<<"id">>, R) =:= 1 end, List)),
    sql(C, <<
        "UPDATE group_member_generation SET end_seq=NULL,start_seq=1 WHERE group_id=1;"
        "TRUNCATE msg_c2g,msg_c2g_timeline"
    >>).

latest_message() ->
    {ok, #{list := [Row]}} = group_logic:list_member_workspace_groups(100, 1, 0, 1, true),
    maps:get(<<"latest_message">>, Row).

latest_seq() -> maps:get(<<"conv_seq">>, latest_message()).

preview_index(C) ->
    {ok, Up} = file:read_file("priv/migrations/00000158_workspace_message_preview_index.up.sql"),
    {ok, Down} = file:read_file(
        "priv/migrations/00000158_workspace_message_preview_index.down.sql"
    ),
    sql(C, Up),
    sql(C, Up),
    {ok, _, [{Def}]} = epgsql:squery(
        C,
        <<"SELECT indexdef FROM pg_indexes WHERE indexname='idx_c2g_timeline_workspace_preview'">>
    ),
    ?assertEqual(nomatch, binary:match(Def, <<"client_ack">>)),
    sql(C, Down),
    {ok, _, [{<<"0">>}]} = epgsql:squery(
        C,
        <<"SELECT count(*) FROM pg_indexes WHERE indexname='idx_c2g_timeline_workspace_preview'">>
    ).

fixture(C) ->
    sql(C, <<
        "CREATE TABLE organization(id bigint PRIMARY KEY,status text);"
        "CREATE TABLE workspace(id bigint PRIMARY KEY,organization_id bigint,status text);"
        "CREATE TABLE workspace_member(workspace_id bigint,user_id bigint,status text);"
        "CREATE TABLE group_member(group_id bigint,user_id bigint,status int);"
        "CREATE TABLE group_member_generation(group_id bigint,user_id bigint,start_seq bigint,end_seq bigint);"
        "CREATE TABLE \"group\"(id bigint PRIMARY KEY,type int,join_limit int,content_limit int,"
        "owner_uid bigint,creator_uid bigint,member_max int,member_count int,introduction text,"
        "avatar text,title text,status int,scope text,workspace_id bigint,updated_at timestamptz,created_at timestamptz);"
        "CREATE TABLE msg_c2g(msg_id text,to_id bigint,msg_type text,payload jsonb,e2ee jsonb,server_ts timestamptz,created_at timestamptz,expire_at timestamptz);"
        "CREATE TABLE msg_c2g_timeline(msg_id text,to_uid bigint,to_gid bigint,created_at timestamptz,conv_seq bigint,client_ack boolean);"
        "INSERT INTO organization VALUES(10,'active'); INSERT INTO workspace VALUES(100,10,'active'),(200,NULL,'active');"
        "INSERT INTO workspace_member VALUES(100,1,'active'),(200,1,'active');"
        "INSERT INTO \"group\"(id,status,scope,workspace_id,title) SELECT n,1,'workspace',100,'Group' FROM generate_series(1,205) n;"
        "INSERT INTO \"group\"(id,status,scope,workspace_id) VALUES(300,1,'personal',100),(301,1,'workspace',200),(302,0,'workspace',100),(303,1,'workspace',100);"
        "INSERT INTO group_member SELECT id,1,1 FROM \"group\" WHERE id<>303;"
        "INSERT INTO group_member_generation SELECT id,1,1,NULL FROM \"group\";"
    >>).

sql(C, Query) ->
    Result = epgsql:squery(C, Query),
    lists:foreach(
        fun(Item) -> ?assertNotMatch({error, _}, Item) end,
        case is_list(Result) of
            true -> Result;
            false -> [Result]
        end
    ).
