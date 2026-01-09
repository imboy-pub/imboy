-module(group_log_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_log_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组日志数据访问层功能
%%% 覆盖：日志查询、创建
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = group_log_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

add_log_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = <<"group123">>,
        LogData = #{<<"action">> => <<"create">>, <<"uid">> => 1, <<"content">> => <<"Created group">>},
        Result = group_log_repo:add(GroupId, LogData),
        ?assertEqual(ok, Result)
    end).

add_log_with_full_data_test_() ->
    ?TEST_WITH_DB(fun() ->
        GroupId = <<"group456">>,
        LogData = #{
            <<"action">> => <<"member_join">>,
            <<"uid">> => 2,
            <<"content">> => <<"User joined group">>,
            <<"extra">> => #{<<"nickname">> => <<"TestUser">>}
        },
        Result = group_log_repo:add(GroupId, LogData),
        ?assertEqual(ok, Result)
    end).
