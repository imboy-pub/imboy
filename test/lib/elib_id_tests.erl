-module(elib_id_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_id 模块的 EUnit 测试
%%%
%%% 目标：验证 ID 生成功能
%%% 覆盖：无前缀生成、带前缀生成（binary、integer、list）
%%%===================================================================

%% ===================================================================
%% ID 生成测试
%% ===================================================================

gen_without_prefix_generates_binary_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = elib_id:gen(),
        % 验证返回的是二进制
        ?assert(is_binary(Result)),
        % 验证不为空
        ?assert(byte_size(Result) > 0)
    end).

gen_with_binary_prefix_generates_binary_with_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        Prefix = <<"test_">>,
        Result = elib_id:gen(Prefix),
        % 验证返回的是二进制
        ?assert(is_binary(Result)),
        % 验证以指定前缀开头
        ?assertMatch(<<"test_", _/binary>>, Result)
    end).

gen_with_integer_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        Prefix = 123,
        Result = elib_id:gen(Prefix),
        % 验证返回的是二进制
        ?assert(is_binary(Result)),
        % 验证包含前缀
        ?assertMatch(<<"123", _/binary>>, Result)
    end).

gen_with_list_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        Prefix = "user_",
        Result = elib_id:gen(Prefix),
        % 验证返回的是二进制
        ?assert(is_binary(Result)),
        % 验证包含前缀
        ?assertMatch(<<"user_", _/binary>>, Result)
    end).

gen_generates_unique_values_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result1 = elib_id:gen(),
        Result2 = elib_id:gen(),
        % 验证两次生成的 ID 不同（极小概率相同）
        ?assert(Result1 =/= Result2)
    end).

gen_with_different_prefixes_test_() ->
    ?TEST_WITH_APP(fun() ->
        MsgId = elib_id:gen(<<"msg_">>),
        S2cId = elib_id:gen(<<"s2c">>),
        UserId = elib_id:gen("user_"),
        % 验证所有 ID 都是二进制
        ?assert(is_binary(MsgId)),
        ?assert(is_binary(S2cId)),
        ?assert(is_binary(UserId)),
        % 验证前缀正确
        ?assertMatch(<<"msg_", _/binary>>, MsgId),
        ?assertMatch(<<"s2c", _/binary>>, S2cId),
        ?assertMatch(<<"user_", _/binary>>, UserId),
        % 验证三个 ID 不相同
        ?assert(MsgId =/= S2cId),
        ?assert(S2cId =/= UserId),
        ?assert(UserId =/= MsgId)
    end).
