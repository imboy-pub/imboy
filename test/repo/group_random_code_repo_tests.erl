-module(group_random_code_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_random_code_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组随机码数据访问层功能
%%% 覆盖：随机码生成、验证（使用 meck mock，不依赖真实数据库）
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]}
    ], fun() ->
        Result = group_random_code_repo:tablename(),
        ?assertEqual(<<"public.group_random_code">>, Result)
    end).

find_by_gid_found_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) ->
                {ok, #{<<"id">> => 1, <<"group_id">> => 1, <<"code">> => <<"ABC123">>}}
            end}
        ]}
    ], fun() ->
        Gid = 1,
        Column = <<"id, group_id, code, created_at">>,
        Result = group_random_code_repo:find_by_gid(Gid, Column),
        ?assertMatch(#{<<"id">> := _, <<"code">> := _}, Result)
    end).

find_by_gid_not_found_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
        ]}
    ], fun() ->
        Gid = 999999,
        Column = <<"id, group_id, code">>,
        Result = group_random_code_repo:find_by_gid(Gid, Column),
        ?assertEqual(#{}, Result)
    end).

add_code_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 500001 end}
        ]},
        {elib_pg, [
            {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Conn = self(),
        Data = #{
            group_id => 1,
            code => <<"ABC123">>
        },
        Result = group_random_code_repo:add(Conn, Data),
        ?assertEqual({ok, 500001}, Result)
    end).
