-module(elib_retry_config_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% 测试用例
%% ===================================================================

%% @doc 测试 c2c 消息重试间隔（sync 范式：best-effort，2 次尝试）
intervals_c2c_test_() ->
    ?TEST_SIMPLE(fun() ->
        Expected = [0, 3000],
        ?assertEqual(Expected, elib_retry_config:intervals(<<"c2c">>))
    end).

%% @doc 测试 c2g 消息重试间隔（sync 范式：仅推送在线成员，1 次尝试）
intervals_c2g_test_() ->
    ?TEST_SIMPLE(fun() ->
        Expected = [0],
        ?assertEqual(Expected, elib_retry_config:intervals(<<"c2g">>))
    end).

%% @doc 测试 c2s 消息重试间隔
intervals_c2s_test_() ->
    ?TEST_SIMPLE(fun() ->
        Expected = [0, 5000, 7000, 11000],
        ?assertEqual(Expected, elib_retry_config:intervals(<<"c2s">>))
    end).

%% @doc 测试 s2c 消息重试间隔
intervals_s2c_test_() ->
    ?TEST_SIMPLE(fun() ->
        Expected = [0, 1500, 1500, 3000, 5000, 7000],
        ?assertEqual(Expected, elib_retry_config:intervals(<<"s2c">>))
    end).

%% @doc 测试 pull 消息重试间隔
intervals_pull_test_() ->
    ?TEST_SIMPLE(fun() ->
        Expected = [8000, 10000, 20000],
        ?assertEqual(Expected, elib_retry_config:intervals(<<"pull">>))
    end).

%% @doc 测试 notice 消息重试间隔
intervals_notice_test_() ->
    ?TEST_SIMPLE(fun() ->
        Expected = [0, 5000, 10000],
        ?assertEqual(Expected, elib_retry_config:intervals(<<"notice">>))
    end).

%% @doc 测试未知类型消息重试间隔（应返回默认值）
intervals_unknown_type_test_() ->
    ?TEST_SIMPLE(fun() ->
        Expected = [0, 5000, 7000, 11000],
        ?assertEqual(Expected, elib_retry_config:intervals(<<"unknown">>))
    end).

%% @doc 测试空二进制输入
intervals_empty_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Expected = [0, 5000, 7000, 11000],
        ?assertEqual(Expected, elib_retry_config:intervals(<<>>))
    end).

%% @doc 验证所有重试间隔都是非负整数列表
intervals_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        IntervalsList = [
            elib_retry_config:intervals(<<"c2c">>),
            elib_retry_config:intervals(<<"c2g">>),
            elib_retry_config:intervals(<<"c2s">>),
            elib_retry_config:intervals(<<"s2c">>),
            elib_retry_config:intervals(<<"pull">>),
            elib_retry_config:intervals(<<"notice">>),
            elib_retry_config:intervals(<<"unknown">>)
        ],
        ?assert(lists:all(fun(Intervals) ->
            is_list(Intervals) andalso
            lists:all(fun(I) -> is_integer(I) andalso I >= 0 end, Intervals)
        end, IntervalsList))
    end).
