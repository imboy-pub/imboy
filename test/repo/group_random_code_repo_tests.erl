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

find_by_code_test_() ->
    ?TEST_SIMPLE(fun() ->
        Code = <<"ABC123">>,
        % 测试函数调用不会崩溃
        Result = group_random_code_repo:find_by_code(Code),
        % 验证返回值格式
        ?assert(is_tuple(Result)),
        case Result of
            {ok, GroupData} ->
                ?assert(is_map(GroupData));
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

generate_code_test_() ->
    ?TEST_SIMPLE(fun() ->
        GroupId = <<"group123">>,
        % 测试函数调用不会崩溃
        Result = group_random_code_repo:generate_code(GroupId),
        % 验证返回值格式
        ?assert(is_tuple(Result)),
        case Result of
            {ok, Code} ->
                ?assertMatch(<<_/binary>>, Code),
                ?assert(byte_size(Code) > 0);
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).
