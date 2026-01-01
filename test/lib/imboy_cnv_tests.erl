-module(imboy_cnv_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_cnv 模块的 EUnit 测试
%%%
%%% 目标：验证数据转换工具功能
%%% 覆盖：类型转换、格式化
%%%===================================================================

%% ===================================================================
%% 二进制转换测试
%% ===================================================================

binary_to_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Binary = <<"123">>,
        Result = imboy_cnv:binary_to_integer(Binary),
        ?assertEqual(123, Result)
    end).

binary_to_integer_with_invalid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Binary = <<"abc">>,
        Result = imboy_cnv:binary_to_integer(Binary),
        ?assertEqual(0, Result)
    end).

integer_to_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Integer = 123,
        Result = imboy_cnv:integer_to_binary(Integer),
        ?assertEqual(<<"123">>, Result)
    end).

list_to_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = "hello",
        Result = imboy_cnv:list_to_binary(List),
        ?assertEqual(<<"hello">>, Result)
    end).

binary_to_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Binary = <<"hello">>,
        Result = imboy_cnv:binary_to_list(Binary),
        ?assertEqual("hello", Result)
    end).

%% ===================================================================
%% 原子转换测试
%% ===================================================================

atom_to_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Atom = hello,
        Result = imboy_cnv:atom_to_binary(Atom),
        ?assertEqual(<<"hello">>, Result)
    end).

binary_to_atom_test_() ->
    ?TEST_SIMPLE(fun() ->
        Binary = <<"hello">>,
        Result = imboy_cnv:binary_to_atom(Binary),
        ?assertEqual(hello, Result)
    end).

%% ===================================================================
%% JSON 转换测试
%% ===================================================================

map_to_json_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{key => <<"value">>, number => 123},
        Result = imboy_cnv:map_to_json(Map),
        ?assertMatch(<<_/binary>>, Result),
        ?assertNotEqual(<<>>, Result)
    end).

json_to_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Json = <<"{\"key\":\"value\",\"number\":123}">>,
        Result = imboy_cnv:json_to_map(Json),
        Expected = #{<<"key">> => <<"value">>, <<"number">> => 123},
        ?assertEqual(Expected, Result)
    end).

json_to_map_invalid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Json = <<"{invalid json}">>,
        Result = imboy_cnv:json_to_map(Json),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% 列表转换测试
%% ===================================================================

map_to_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{a => 1, b => 2},
        Result = imboy_cnv:map_to_list(Map),
        % 验证结果包含所有键值对
        ?assert(length(Result) >= 2),
        ?assert(lists:keymember(a, 1, Result) orelse lists:keymember(<<"a">>, 1, Result))
    end).

list_to_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = [{a, 1}, {b, 2}],
        Result = imboy_cnv:list_to_map(List),
        Expected = #{a => 1, b => 2},
        ?assertEqual(Expected, Result)
    end).

%% ===================================================================
%% 格式化测试
%% ===================================================================

format_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Template = <<"Hello ~s, you have ~p messages">>,
        Name = <<"Alice">>,
        Count = 5,
        Result = imboy_cnv:format(Template, [Name, Count]),
        ?assertMatch(<<_/binary>>, Result),
        ?assertNotEqual(<<>>, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

empty_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Binary = <<>>,
        Result = imboy_cnv:binary_to_integer(Binary),
        ?assertEqual(0, Result)
    end).

empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = [],
        Result = imboy_cnv:list_to_binary(List),
        ?assertEqual(<<>>, Result)
    end).

null_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_cnv:atom_to_binary(undefined),
        ?assertEqual(<<"undefined">>, Result)
    end).