-module(workbench_entries_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

%% 只连接任务新建的合成测试库，验证真实 SQL 和实际响应投影。
%% 2026-10-12 定位修订（organization 是业务对象，非租户）：发现返回实例内
%% 全部 active 且 allowlist 非空的应用——不 JOIN org/成员/身份映射；
%% organization_id 仅作业务归属回显。合成 schema 保留 org/member/user 表
%% 以证明其数据状态不影响结果（反例钉住：org archived / 成员 removed /
%% 无 identity 照常可见）。
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
        "INSERT INTO organization VALUES(10,'active'),(20,'archived');"
        "INSERT INTO \"user\" VALUES(1,1,0),(2,1,0);"
        "INSERT INTO organization_member VALUES(10,1,'removed');"
        "INSERT INTO enterprise_application VALUES"
        "(101,10,'oa-a','Office A','active',ARRAY['https://oa.example.com/a']),"
        "(102,20,'oa-b','Office B','active',ARRAY['https://oa.example.com/b']),"
        "(103,10,'oa-disabled','Disabled','disabled',ARRAY['https://oa.example.com/c']),"
        "(104,10,'oa-empty','Empty','active',ARRAY[]::text[]);"
        "INSERT INTO enterprise_external_identity VALUES(10,101,1,'active');"
    >>).

verify(C) ->
    %% 实例内 active + 非空 allowlist 全部返回（org archived / 成员 removed /
    %% 无 identity 均不参与发现）；按 application_id 排序。
    Entries = entries(C, 1),
    ?assertEqual(2, length(Entries)),
    [A, B] = Entries,
    ?assertEqual(101, maps:get(<<"application_id">>, A)),
    ?assertEqual(10, maps:get(<<"organization_id">>, A)),
    ?assertEqual(102, maps:get(<<"application_id">>, B)),
    ?assertEqual(20, maps:get(<<"organization_id">>, B)),
    ?assertEqual(<<"https://oa.example.com/a">>, maps:get(<<"redirect_uri">>, A)),
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
    %% 未入会/无映射用户（user 2 无 member 行）看到相同结果。
    ?assertEqual(Entries, entries(C, 2)),
    %% 上限 20。
    sql(C, <<
        "INSERT INTO enterprise_application SELECT 1000+n,10,'oa-key-'||n,'Office',"
        "'active',ARRAY['https://oa.example.com/landing'] FROM generate_series(1,25) n"
    >>),
    ?assertEqual(20, length(entries(C, 1))),
    %% 未认证 uid：池化入口 401（判定先于任何 DB 访问）。
    ?assertMatch({error, {401, _}}, enterprise_oa_sso_logic:entries(0)).

entries(C, Uid) ->
    {ok, #{<<"entries">> := Rows}} = enterprise_oa_sso_logic:entries_tx(C, Uid),
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
