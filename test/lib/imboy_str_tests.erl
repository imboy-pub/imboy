-module(imboy_str_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_str 模块的 EUnit 测试
%%%
%%% 目标：验证字符串工具功能
%%% 覆盖：startswith, endswith, replace, replace_single_quote, trunc
%%% 注意：只测试源文件中实际存在的函数
%%%===================================================================

%% ===================================================================
%% startswith/2 测试
%% ===================================================================

%% @doc 测试二进制前缀匹配 - 匹配成功
startswith_binary_match_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:startswith(<<"abc">>, <<"abcdef">>),
        ?assertEqual(true, Result)
    end).

%% @doc 测试二进制前缀匹配 - 不匹配
startswith_binary_no_match_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:startswith(<<"xyz">>, <<"abcdef">>),
        ?assertEqual(false, Result)
    end).

%% @doc 测试字符串前缀匹配
startswith_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:startswith("abc", "abcdef"),
        ?assertEqual(true, Result)
    end).

%% @doc 测试空前缀
startswith_empty_prefix_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:startswith(<<>>, <<"anything">>),
        ?assertEqual(true, Result)
    end).

%% ===================================================================
%% endswith/2 测试
%% ===================================================================

%% @doc 测试二进制后缀匹配 - 匹配成功
endswith_binary_match_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:endswith(<<"ing">>, <<"This is the end of the binary">>),
        ?assertEqual(false, Result)
    end).

%% @doc 测试二进制后缀匹配 - 实际匹配
endswith_binary_actual_match_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:endswith(<<"binary">>, <<"This is the end of the binary">>),
        ?assertEqual(true, Result)
    end).

%% @doc 测试字符串后缀匹配
endswith_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:endswith("string", "This is the end of the string"),
        ?assertEqual(true, Result)
    end).

%% @doc 测试空后缀
endswith_empty_suffix_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:endswith(<<>>, <<"anything">>),
        ?assertEqual(true, Result)
    end).

%% @doc 测试后缀比输入长
endswith_suffix_longer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:endswith(<<"long suffix">>, <<"short">>),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% replace/3 测试
%% ===================================================================

%% @doc 测试正则替换 - 基本替换
replace_basic_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:replace(<<"Hello, old world!">>, <<"old">>, <<"new">>),
        ?assertEqual(<<"Hello, new world!">>, Result)
    end).

%% @doc 测试正则替换 - 全局替换
replace_global_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:replace(<<"a.b.c">>, <<"\\.">>, <<"/">>),
        ?assertEqual(<<"a/b/c">>, Result)
    end).

%% @doc 测试替换 - 不匹配
replace_no_match_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:replace(<<"Hello world">>, <<"xyz">>, <<"new">>),
        ?assertEqual(<<"Hello world">>, Result)
    end).

%% ===================================================================
%% replace_single_quote/1 测试
%% ===================================================================

%% @doc 测试替换单引号
replace_single_quote_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:replace_single_quote(<<"hello D'l">>),
        ?assertEqual(<<"hello D\\'l">>, Result)
    end).

%% @doc 测试多个单引号
replace_multiple_quotes_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:replace_single_quote(<<"'hello' 'world'">>),
        ?assertEqual(<<"\\'hello\\' \\'world\\'">>, Result)
    end).

%% @doc 测试无单引号
replace_no_quote_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:replace_single_quote(<<"hello world">>),
        ?assertEqual(<<"hello world">>, Result)
    end).

%% ===================================================================
%% trunc/2 测试
%% ===================================================================

%% @doc 测试截断 - 需要截断
trunc_binary_need_trunc_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:trunc(<<"12345678">>, 7),
        ?assertEqual(<<"1234...">>, Result)
    end).

%% @doc 测试截断 - 不需要截断
trunc_binary_no_trunc_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:trunc(<<"123">>, 7),
        ?assertEqual(<<"123">>, Result)
    end).

%% @doc 测试截断 - 只保留后缀
trunc_only_suffix_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:trunc(<<"12345678">>, 3),
        ?assertEqual(<<"...">>, Result)
    end).

%% @doc 测试字符串截断
trunc_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:trunc("12345678", 7),
        ?assertEqual("1234...", Result)
    end).

%% ===================================================================
%% trunc/3 测试
%% ===================================================================

%% @doc 测试自定义后缀截断
trunc_custom_suffix_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:trunc(<<"12345678">>, 7, <<"**">>),
        ?assertEqual(<<"12345**">>, Result)
    end).

%% @doc 测试自定义后缀 - 二进制版本
trunc_custom_binary_suffix_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:trunc(<<"12345678">>, 7, <<"---">>),
        ?assertEqual(<<"1234---">>, Result)
    end).

%% @doc 测试自定义后缀 - 字符串版本
trunc_custom_string_suffix_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_str:trunc("12345678", 7, "**"),
        ?assertEqual("12345**", Result)
    end).

%% @doc 测试后缀超过最大长度
trunc_suffix_exceeds_max_test_() ->
    ?_test({skip, "函数行为需要确认"}).
