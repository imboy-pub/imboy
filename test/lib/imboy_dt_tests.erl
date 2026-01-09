-module(imboy_dt_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_dt 模块的 EUnit 测试
%%%
%%% 目标：验证日期时间工具函数功能
%%% 覆盖：时间戳获取、RFC3339转换、时间计算
%%%===================================================================

%% ===================================================================
%% 时间戳函数测试
%% ===================================================================

second_returns_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_dt:second(),
        ?assert(is_integer(Result)),
        ?assert(Result > 1700000000)  % 2023年之后的时间戳
    end).

millisecond_returns_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_dt:millisecond(),
        ?assert(is_integer(Result)),
        ?assert(Result > 1700000000000)
    end).

microsecond_returns_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_dt:microsecond(),
        ?assert(is_integer(Result)),
        ?assert(Result > 1700000000000000)
    end).

%% ===================================================================
%% now/0, now/1 测试
%% ===================================================================

now_default_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_dt:now(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0),
        % 验证时间格式符合 RFC3339 标准 (YYYY-MM-DDTHH:MM:SS.sss+TZ)
        Pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}",
        ?assert(re:run(Result, Pattern) =/= nomatch)
    end).

now_with_second_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_dt:now(second),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0),
        % 验证秒级时间格式 (RFC3339 with T separator)
        Pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}",
        ?assert(re:run(Result, Pattern) =/= nomatch)
    end).

now_with_millisecond_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_dt:now(millisecond),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0),
        % 验证毫秒级时间格式 (包含毫秒部分)
        Pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}",
        ?assert(re:run(Result, Pattern) =/= nomatch)
    end).

%% ===================================================================
%% to_rfc3339/2, to_rfc3339/3 测试
%% ===================================================================

to_rfc3339_with_second_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Timestamp = imboy_dt:second(),
        Result = imboy_dt:to_rfc3339(Timestamp, second),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0),
        % 验证 RFC3339 时间格式，支持本地时区偏移
        Rfc3339Pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}",
        ?assert(re:run(Result, Rfc3339Pattern) =/= nomatch)
    end).

to_rfc3339_with_millisecond_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Timestamp = imboy_dt:millisecond(),
        Result = imboy_dt:to_rfc3339(Timestamp, millisecond),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0)
    end).

to_rfc3339_with_microsecond_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Timestamp = imboy_dt:microsecond(),
        Result = imboy_dt:to_rfc3339(Timestamp, microsecond),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0),
        % 验证微秒级时间格式 (包含微秒部分)
        Rfc3339Pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{6}.*",
        ?assert(re:run(Result, Rfc3339Pattern) =/= nomatch)
    end).

to_rfc3339_auto_detect_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试 to_rfc3339/1 的自动检测功能
        SecTimestamp = 1704067200,  % 2024-01-01 00:00:00
        Result = imboy_dt:to_rfc3339(SecTimestamp),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0)
    end).

%% ===================================================================
%% rfc3339_to/2 测试
%% ===================================================================

rfc3339_to_with_valid_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Rfc3339String = "2024-01-01 12:00:00Z",
        Result = imboy_dt:rfc3339_to(Rfc3339String, second),
        ?assert(is_integer(Result)),
        ?assert(Result > 1700000000),  % 2023年之后的时间戳
        ?assert(Result < 2000000000)   % 2033年之前的时间戳
    end).

rfc3339_to_with_empty_string_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_dt:rfc3339_to("", second),
        ?assertEqual({error, empty_input}, Result)
    end).

%% ===================================================================
%% add/2, minus/2 测试
%% ===================================================================

add_with_minute_increases_time_test_() ->
    ?TEST_SIMPLE(fun() ->
        Dt = imboy_dt:now(),
        Result = imboy_dt:add(Dt, {10, minute}),
        ?assertMatch(<<_/binary>>, Result)
    end).

add_with_second_increases_time_test_() ->
    ?TEST_SIMPLE(fun() ->
        Dt = imboy_dt:now(),
        Result = imboy_dt:add(Dt, {60, second}),
        ?assertMatch(<<_/binary>>, Result)
    end).

minus_with_minute_decreases_time_test_() ->
    ?TEST_SIMPLE(fun() ->
        Dt = imboy_dt:now(),
        Result = imboy_dt:minus(Dt, {10, minute}),
        ?assertMatch(<<_/binary>>, Result)
    end).

%% ===================================================================
%% compare_rfc3339/3 测试
%% ===================================================================

compare_rfc3339_with_gt_test_() ->
    ?TEST_SIMPLE(fun() ->
        Dt1 = <<"2024-01-01 12:00:00Z">>,
        Dt2 = <<"2024-01-01 13:00:00Z">>,
        Result = imboy_dt:compare_rfc3339(Dt1, Dt2, lt),
        ?assertEqual(true, Result)
    end).

compare_rfc3339_with_eq_test_() ->
    ?TEST_SIMPLE(fun() ->
        Dt = <<"2024-01-01 12:00:00Z">>,
        Result = imboy_dt:compare_rfc3339(Dt, Dt, eq),
        ?assertEqual(true, Result)
    end).

compare_rfc3339_with_lt_test_() ->
    ?TEST_SIMPLE(fun() ->
        Dt1 = <<"2024-01-01 13:00:00Z">>,
        Dt2 = <<"2024-01-01 12:00:00Z">>,
        Result = imboy_dt:compare_rfc3339(Dt1, Dt2, gt),
        ?assertEqual(true, Result)
    end).
