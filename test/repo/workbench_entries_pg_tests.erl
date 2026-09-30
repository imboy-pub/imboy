-module(workbench_entries_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

%% 只连接任务新建的合成测试库，验证真实 SQL 和实际响应投影。
run(SocketPath) ->
    {ok, Conn} = epgsql:connect(#{
        host => {local, SocketPath},
        port => 0,
        username => "departure_test",
        database => "postgres"
    }),
    try
        schema(Conn),
        eunit:test([{"scope", fun() -> verify(Conn) end}], [verbose])
    after
        epgsql:close(Conn)
    end.

schema(C) ->
    sql(C, <<
        "CREATE TABLE organization(id bigint PRIMARY KEY,status text);"
        "CREATE TABLE organization_member(organization_id bigint,user_id bigint,status text);"
        "CREATE TABLE \"user\"(id bigint PRIMARY KEY,status int,account_type int);"
        "CREATE TABLE enterprise_application(id bigint PRIMARY KEY,organization_id bigint,"
        "application_key text,name text,status text,allowed_redirect_uris text[]);"
        "CREATE TABLE enterprise_external_identity(organization_id bigint,application_id bigint,"
        "user_id bigint,status text);"
        "INSERT INTO organization VALUES(10,'active'),(20,'active');"
        "INSERT INTO \"user\" VALUES(1,1,0),(2,1,0);"
        "INSERT INTO organization_member VALUES(10,1,'active'),(20,1,'active');"
        "INSERT INTO enterprise_application VALUES"
        "(101,10,'shared-oa-key','Office A','active',ARRAY['https://oa.example.com/a']),"
        "(102,20,'shared-oa-key','Office B','active',ARRAY['https://oa.example.com/b']),"
        "(103,10,'disabled-oa','Disabled','disabled',ARRAY['https://oa.example.com/c']),"
        "(104,10,'unmapped-oa','Unmapped','active',ARRAY['https://oa.example.com/d']),"
        "(105,10,'empty-redirect','Empty','active',ARRAY[]::text[]);"
        "INSERT INTO enterprise_external_identity VALUES"
        "(10,101,1,'active'),(20,102,1,'active'),(10,103,1,'active'),(10,105,1,'active');"
    >>).

verify(C) ->
    [A] = entries(C, 1, 10),
    [B] = entries(C, 1, 20),
    ?assertEqual(101, maps:get(<<"application_id">>, A)),
    ?assertEqual(10, maps:get(<<"organization_id">>, A)),
    ?assertEqual(102, maps:get(<<"application_id">>, B)),
    ?assertEqual(<<"https://oa.example.com/a">>, maps:get(<<"redirect_uri">>, A)),
    ?assertEqual([], entries(C, 2, 10)),
    ?assertEqual([], entries(C, 1, 30)),
    ?assertEqual(
        lists:sort([
            <<"kind">>,
            <<"organization_id">>,
            <<"application_id">>,
            <<"application_key">>,
            <<"label">>,
            <<"redirect_uri">>
        ]),
        lists:sort(maps:keys(A))
    ),
    sql(C, <<"UPDATE organization_member SET status='removed' WHERE organization_id=10">>),
    ?assertEqual([], entries(C, 1, 10)),
    ?assertMatch([_], entries(C, 1, 20)),
    sql(C, <<"UPDATE organization SET status='archived' WHERE id=20">>),
    ?assertEqual([], entries(C, 1, 20)),
    sql(C, <<
        "UPDATE organization_member SET status='active' WHERE organization_id=10;"
        "UPDATE \"user\" SET status=0 WHERE id=1"
    >>),
    ?assertEqual([], entries(C, 1, 10)),
    sql(C, <<
        "UPDATE \"user\" SET status=1 WHERE id=1;"
        "INSERT INTO enterprise_application SELECT 1000+n,10,'oa-key-'||n,'Office',"
        "'active',ARRAY['https://oa.example.com/landing'] FROM generate_series(1,25) n;"
        "INSERT INTO enterprise_external_identity SELECT 10,1000+n,1,'active'"
        " FROM generate_series(1,25) n"
    >>),
    ?assertEqual(20, length(entries(C, 1, 10))),
    ?assertMatch({error, {401, _}}, enterprise_oa_sso_logic:entries(0, 10)),
    ?assertMatch({error, {400, _}}, enterprise_oa_sso_logic:entries(1, 0)).

entries(C, Uid, OrgId) ->
    {ok, #{<<"entries">> := Rows}} = enterprise_oa_sso_logic:entries_tx(C, Uid, OrgId),
    Rows.

sql(C, Query) ->
    Result = epgsql:squery(C, Query),
    lists:foreach(
        fun(R) -> ?assertNotMatch({error, _}, R) end,
        case is_list(Result) of
            true -> Result;
            false -> [Result]
        end
    ).
