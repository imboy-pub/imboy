-module(talk_record_parent_fixture_tests).
-include_lib("eunit/include/eunit.hrl").

%% 仅专属合成库可运行；事务回滚保留完整 schema 的触发器与约束。
parent_fixture_test_() ->
    case os:getenv("TALK_PARENT_FIXTURE_DATABASE") of
        false -> [];
        Db -> {timeout, 30, fun() -> check(Db) end}
    end.

check(Db) ->
    ?assertEqual(Db, os:getenv("T06_PG_DATABASE")),
    C = talk_record_t06_fixture:connect(),
    try
        {ok, [#{<<"db">> := Actual}]} = elib_pg:query(
            C,
            <<"SELECT current_database() db">>,
            []
        ),
        ?assertEqual(list_to_binary(Db), Actual),
        rollback(C, fun() ->
            ok = talk_record_t05_support:seed_fixtures(C),
            {ok, [#{<<"n">> := 3}]} = elib_pg:query(
                C,
                <<"SELECT count(*) n FROM workspace">>,
                []
            )
        end),
        rollback(C, fun() ->
            ok = talk_record_t06_fixture:seed_world(C),
            {ok, [#{<<"status">> := <<"archived">>}]} = elib_pg:query(
                C,
                <<"SELECT status FROM organization WHERE id=20">>,
                []
            )
        end),
        rollback(C, fun() -> minimal(C) end)
    after
        epgsql:close(C)
    end.

rollback(C, Fun) ->
    {ok, [], []} = epgsql:squery(C, "BEGIN"),
    try
        Fun()
    after
        {ok, [], []} = epgsql:squery(C, "ROLLBACK")
    end.

minimal(C) ->
    {ok, _, _} = epgsql:squery(C, "CREATE SCHEMA talk_parent_fixture_minimal"),
    {ok, _, _} = epgsql:squery(C, "SET LOCAL search_path=talk_parent_fixture_minimal"),
    {ok, _, _} = epgsql:squery(C, "CREATE TABLE \"user\"(id bigint PRIMARY KEY,nickname text)"),
    {ok, _, _} = epgsql:squery(C, "CREATE TABLE organization(id bigint PRIMARY KEY,status text)"),
    {ok, _, _} = epgsql:squery(
        C, "CREATE TABLE workspace(id bigint PRIMARY KEY,organization_id bigint)"
    ),
    ok = talk_record_parent_fixture:user(C, 1, #{<<"nickname">> => <<"synthetic">>}),
    ok = talk_record_parent_fixture:organization(C, 10, <<"synthetic">>, 1, <<"active">>),
    ok = talk_record_parent_fixture:workspace(C, 100, 10, <<"synthetic">>, 1),
    {ok, [#{<<"n">> := 1}]} = elib_pg:query(C, <<"SELECT count(*) n FROM workspace">>, []).
