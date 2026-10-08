-module(workspace_attachment_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

run(Socket) ->
    {ok, C} = epgsql:connect(#{
        host => {local, Socket},
        port => 0,
        username => "postgres",
        database => "postgres"
    }),
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
                    ?assertEqual([4, 3, 2, 1], ids(10, #{})),
                    ?assertEqual([5], ids(20, #{}))
                end},
                {"group channel uploader and literal keyword filters", fun() ->
                    ?assertEqual([2, 1], ids(10, #{<<"group_id">> => <<"100">>})),
                    ?assertEqual([4, 3], ids(10, #{<<"channel_id">> => <<"200">>})),
                    ?assertEqual([4, 2], ids(10, #{<<"user_keyword">> => <<"Alice">>})),
                    ?assertEqual([1], ids(10, #{<<"keyword">> => <<"%_">>}))
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
                    ?assertEqual([], ids(10, #{<<"from_ms">> => <<"1767312000000">>}))
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
        " INSERT INTO \"group\" VALUES(100,'group',10,'workspace',1),(101,'other',20,'workspace',1);"
        " INSERT INTO channel VALUES(200,'channel',10,'workspace',1);"
        " INSERT INTO \"user\" VALUES(1,'Alice','alice'),(2,'Bob','bob');"
        " INSERT INTO attachment SELECT id,CASE WHEN id=1 THEN '100%_report.pdf' ELSE 'report.pdf' END,"
        " 100,'application/pdf','2026-01-01 00:00:00+00'::timestamptz,CASE WHEN id%2=0 THEN 1 ELSE 2 END,"
        " CASE WHEN id<=2 OR id=5 THEN 'group' ELSE 'channel' END,"
        " CASE WHEN id=5 THEN '101' WHEN id<=2 THEN '100' ELSE '200' END,'key',1,NULL FROM generate_series(1,5) id;",
    Results = epgsql:squery(C, Sql),
    ?assertEqual([], [R || R = {error, _} <- Results]),
    ok.
