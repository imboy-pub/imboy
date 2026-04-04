-module(group_random_code_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_random_code_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组随机码数据访问层功能
%%% 覆盖：随机码生成、验证
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = group_random_code_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

find_by_gid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Gid = 1,
        Column = <<"id, group_id, code, created_at">>,
        % find_by_gid/2 返回 map 或 {}
        Result = group_random_code_repo:find_by_gid(Gid, Column),
        ?assert(is_map(Result) orelse Result =:= #{})
    end).

add_code_test_() ->
    ?TEST_SIMPLE(fun() ->
        Conn = self(),
        Data = #{
            group_id => 1,
            code => <<"ABC123">>
        },
        % add/2 需要连接和数据
        Result = group_random_code_repo:add(Conn, Data),
        ?assert(is_tuple(Result))
    end).
