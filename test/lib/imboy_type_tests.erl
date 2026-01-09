-module(imboy_type_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_type 模块的 EUnit 测试
%%%
%%% 目标：验证类型判断工具功能
%%% 覆盖：类型检查、转换
%%%===================================================================

%% ===================================================================
%% 类型判断测试
%% ===================================================================

is_binary_with_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Value = <<"hello">>,
        Result = imboy_type:is_binary_type(Value),
        ?assertEqual(true, Result)
    end).

is_binary_with_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Value = "hello",
        Result = imboy_type:is_binary_type(Value),
        ?assertEqual(false, Result)
    end).

is_integer_with_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Value = 123,
        Result = imboy_type:is_integer_type(Value),
        ?assertEqual(true, Result)
    end).

is_integer_with_float_test_() ->
    ?TEST_SIMPLE(fun() ->
        Value = 123.45,
        Result = imboy_type:is_integer_type(Value),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% 类型转换测试
%% ===================================================================

to_integer_from_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        String = <<"123">>,
        Result = imboy_type:to_integer(String),
        % 验证字符串转整数结果
        ?assertEqual(123, Result)
    end).

to_binary_from_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Integer = 123,
        Result = imboy_type:to_binary(Integer),
        % 验证整数转二进制结果
        ?assertEqual(<<"123">>, Result)
    end).
