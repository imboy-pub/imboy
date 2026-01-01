-module(group_notice_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_notice_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群组公告业务逻辑功能
%%% 覆盖：demo 函数测试
%%%===================================================================

%% ===================================================================
%% demo/3 测试
%% ===================================================================

demo_with_valid_params_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'demo', 3, fun(_Uid, _Val1, _Val2) -> ok end}
    ], fun() ->
        Uid = 12345,
        Val1 = <<"Notice Title">>,
        Val2 = <<"Notice Content">>,
        
        Result = group_notice_logic:demo(Uid, Val1, Val2),
        ?assertEqual(ok, Result)
    end).

demo_with_empty_values_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'demo', 3, fun(_Uid, _Val1, _Val2) -> ok end}
    ], fun() ->
        Uid = 12345,
        Val1 = <<>>,  % 空标题
        Val2 = <<>>,  % 空内容
        
        Result = group_notice_logic:demo(Uid, Val1, Val2),
        ?assertEqual(ok, Result)
    end).

demo_with_long_content_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'demo', 3, fun(_Uid, _Val1, _Val2) -> ok end}
    ], fun() ->
        Uid = 12345,
        LongTitle = <<"This is a very long notice title for testing purposes">>,
        LongContent = <<"This is a very long notice content that contains detailed information about the meeting, including time, location, participants, and agenda items that need to be discussed.">>,
        
        Result = group_notice_logic:demo(Uid, LongTitle, LongContent),
        ?assertEqual(ok, Result)
    end).

demo_with_special_characters_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'demo', 3, fun(_Uid, _Val1, _Val2) -> ok end}
    ], fun() ->
        Uid = 12345,
        Val1 = <<"公告标题 & 特殊字符!@#$%">>,
        Val2 = <<"公告内容包含中文、English、123 数字!@#$%">>,
        
        Result = group_notice_logic:demo(Uid, Val1, Val2),
        ?assertEqual(ok, Result)
    end).

demo_repo_error_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'demo', 3, fun(_Uid, _Val1, _Val2) -> {error, <<"Database error">>} end}
    ], fun() ->
        Uid = 12345,
        Val1 = <<"Notice Title">>,
        Val2 = <<"Notice Content">>,
        
        Result = group_notice_logic:demo(Uid, Val1, Val2),
        % 即使 repo 返回错误，demo 函数仍然返回 ok
        ?assertEqual(ok, Result)
    end).

demo_with_different_uids_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'demo', 3, fun(Uid, Val1, Val2) -> 
            % 验证参数传递正确
            ?assert(is_integer(Uid)),
            ?assertMatch(<<_/binary>>, Val1),
            ?assertMatch(<<_/binary>>, Val2),
            ok 
        end}
    ], fun() ->
        % 测试不同的用户ID
        TestCases = [
            {1, <<"Title1">>, <<"Content1">>},
            {99999, <<"Title2">>, <<"Content2">>},
            {0, <<"Title3">>, <<"Content3">>}  % 边界情况
        ],
        
        lists:foreach(fun({Uid, Title, Content}) ->
            Result = group_notice_logic:demo(Uid, Title, Content),
            ?assertEqual(ok, Result)
        end, TestCases)
    end).
