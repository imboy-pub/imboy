-module(workspace_attachment_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

run(Options) when is_map(Options) ->
    {ok, C} = epgsql:connect(Options),
    run_connected(C);
run(Socket) ->
    {ok, C} = epgsql:connect(#{
        host => {local, Socket},
        port => 0,
        username => "postgres",
        database => "postgres"
    }),
    run_connected(C).

run_connected(C) ->
    meck:new(elib_pg, [non_strict, no_link]),
    try
        fixture(C),
        meck:expect(elib_pg, query, fun(Sql, Args) ->
            case epgsql:equery(C, Sql, Args) of
                {ok, Cols, Rows} ->
                    Names = [element(2, Col) || Col <- Cols],
                    {ok, [maps:from_list(lists:zip(Names, tuple_to_list(Row))) || Row <- Rows]};
                Error ->
                    Error
            end
        end),
        eunit:test(
            [
                {"scope excludes another workspace and personal attachments", fun() ->
                    ?assertEqual([7, 4, 3, 2, 1], ids(10, #{})),
                    ?assertEqual([8, 5], ids(20, #{}))
                end},
                {"group channel uploader and literal keyword filters", fun() ->
                    ?assertEqual([2, 1], ids(10, #{<<"group_id">> => <<"100">>})),
                    ?assertEqual([4, 3], ids(10, #{<<"channel_id">> => <<"200">>})),
                    ?assertEqual([7, 4, 2], ids(10, #{<<"user_keyword">> => <<"Alice">>})),
                    ?assertEqual([1], ids(10, #{<<"keyword">> => <<"%_">>}))
                end},
                {"organization combines all sources and includes null-workspace records", fun() ->
                    {ok, F} = workspace_attachment_logic:filters(#{}),
                    {ok, Rows} = workspace_attachment_repo:scan_organization(100, F),
                    ?assertEqual([7, 6, 4, 3, 2, 1], [maps:get(<<"id">>, R) || R <- Rows]),
                    ?assertEqual([7, 4, 3, 2, 1], ids(10, #{})),
                    ?assertEqual([7], ids(10, #{<<"source">> => <<"talk_record">>})),
                    {ok, F2} = workspace_attachment_logic:filters(#{
                        <<"source">> => <<"talk_record">>, <<"cursor">> => <<"7">>
                    }),
                    {ok, [Only]} = workspace_attachment_repo:scan_organization(100, F2),
                    ?assertEqual(6, maps:get(<<"id">>, Only)),
                    ?assertEqual(null, maps:get(<<"workspace_id">>, Only))
                end},
                {"cursor and half-open date interval compose", fun() ->
                    ?assertEqual([2, 1], ids(10, #{<<"cursor">> => <<"3">>})),
                    ?assertEqual(
                        [4, 3, 2, 1],
                        ids(10, #{
                            <<"from_ms">> => <<"1767225600000">>,
                            <<"until_ms">> => <<"1767312000000">>
                        })
                    ),
                    ?assertEqual([7], ids(10, #{<<"from_ms">> => <<"1767312000000">>}))
                end}
            ],
            [verbose]
        )
    after
        meck:unload(elib_pg),
        epgsql:close(C)
    end.

ids(Ws, Input) ->
    {ok, Filters} = workspace_attachment_logic:filters(Input),
    {ok, Rows} = workspace_attachment_repo:scan(Ws, Filters),
    [maps:get(<<"id">>, Row) || Row <- Rows].

fixture(C) ->
    Sql =
        "CREATE TEMP TABLE \"group\"(id bigint,title text,workspace_id bigint,scope text,status int);"
        " CREATE TEMP TABLE channel(id bigint,name text,workspace_id bigint,scope text,status int);"
        " CREATE TEMP TABLE \"user\"(id bigint,nickname text,account text);"
        " CREATE TEMP TABLE attachment(id bigint,name text,size bigint,mime_type text,created_at timestamptz,"
        " creator_user_id bigint,scope text,scope_ref text,path text,status int,cipher text);"
        " CREATE TEMP TABLE workspace(id bigint,organization_id bigint,status text);"
        " CREATE TEMP TABLE talk_record(id bigint,title text,scope text,organization_id bigint,workspace_id bigint,deleted_at timestamptz);"
        " CREATE TEMP TABLE talk_record_attachment_ref(attachment_id bigint,record_id bigint,state text,role text,lease_until timestamptz);"
        " CREATE TEMP TABLE workspace_attachment_retention(attachment_id bigint,workspace_id bigint,retained_at timestamptz);"
        " CREATE TEMP TABLE enterprise_attachment_retention(attachment_id bigint,organization_id bigint,talk_record_id bigint,source_kind text,created_at timestamptz);"
        " INSERT INTO workspace VALUES(10,100,'active'),(20,200,'active');"
        " INSERT INTO talk_record VALUES(50,'null workspace','organization',100,NULL,NULL),(51,'workspace record','organization',100,10,NULL),(52,'foreign organization','organization',200,20,NULL);"
        " INSERT INTO talk_record_attachment_ref VALUES(6,50,'bound','assembled_audio',NULL),(7,51,'bound','part',NULL),(8,52,'bound','part',NULL);"
        " INSERT INTO \"group\" VALUES(100,'group',10,'workspace',1),(101,'other',20,'workspace',1);"
        " INSERT INTO channel VALUES(200,'channel',10,'workspace',1);"
        " INSERT INTO \"user\" VALUES(1,'Alice','alice'),(2,'Bob','bob');"
        " INSERT INTO attachment SELECT id,CASE WHEN id=1 THEN '100%_report.pdf' ELSE 'report.pdf' END,"
        " 100,'application/pdf','2026-01-01 00:00:00+00'::timestamptz,CASE WHEN id%2=0 THEN 1 ELSE 2 END,"
        " CASE WHEN id<=2 OR id=5 THEN 'group' ELSE 'channel' END,"
        " CASE WHEN id=5 THEN '101' WHEN id<=2 THEN '100' ELSE '200' END,'key',1,NULL FROM generate_series(1,5) id;"
        " INSERT INTO attachment SELECT n,'record.aac',100,'audio/aac',now(),1,'talk_record',"
        " (n+44)::text,'private',1,NULL FROM generate_series(6,8) n;",
    Results = epgsql:squery(C, Sql),
    ?assertEqual([], [R || R = {error, _} <- Results]),
    ok.
