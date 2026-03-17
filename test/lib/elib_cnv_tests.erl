-module(elib_cnv_tests).
-compile(nowarn_all).
-include_lib("eunit/include/eunit.hrl").
-include("../../include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_cnv 模块的 EUnit 测试
%%%
%%% 目标：验证数据转换工具功能
%%% 覆盖：json_maybe, implode, remove_dups, vsn_major, map_to_query,
%%%       list_to_binary_string, safe_to_binary, convert_at_timestamps
%%%===================================================================

%% ===================================================================
%% json_maybe/1 测试
%% ===================================================================

%% @doc 测试 JSON 字符串解码 - 对象
json_maybe_object_test_() ->
    ?TEST_SIMPLE(fun() ->
        Json = <<"{\"key\":\"value\",\"number\":123}">>,
        Result = elib_cnv:json_maybe(Json),
        ?assertMatch(#{<<"key">> := <<"value">>, <<"number">> := 123}, Result)
    end).

%% @doc 测试 JSON 字符串解码 - 数组
json_maybe_array_test_() ->
    ?TEST_SIMPLE(fun() ->
        Json = <<"[1,2,3]">>,
        Result = elib_cnv:json_maybe(Json),
        ?assertEqual([1, 2, 3], Result)
    end).

%% @doc 测试非 JSON 字符串 - 保持原样
json_maybe_non_json_test_() ->
    ?TEST_SIMPLE(fun() ->
        Val = <<"not a json">>,
        Result = elib_cnv:json_maybe(Val),
        ?assertEqual(Val, Result)
    end).

%% @doc 测试整数 - 保持原样
json_maybe_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Val = 12345,
        Result = elib_cnv:json_maybe(Val),
        ?assertEqual(Val, Result)
    end).

%% @doc 测试空字符串
json_maybe_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Val = <<>>,
        Result = elib_cnv:json_maybe(Val),
        ?assertEqual(Val, Result)
    end).

%% @doc 测试无效 JSON
json_maybe_invalid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Val = <<"{invalid}">>,
        ?assertError(badarg, elib_cnv:json_maybe(Val))
    end).

%% @doc 测试 UTF-8 中文支持
json_maybe_utf8_chinese_test_() ->
    ?TEST_SIMPLE(fun() ->
        Json = unicode:characters_to_binary("{\"name\":\"张三\",\"city\":\"北京\"}", utf8),
        Result = elib_cnv:json_maybe(Json),
        ?assertMatch(#{<<"name">> := _, <<"city">> := _}, Result),
        ?assertEqual(<<"张三"/utf8>>, maps:get(<<"name">>, Result)),
        ?assertEqual(<<"北京"/utf8>>, maps:get(<<"city">>, Result))
    end).

%% @doc 测试 UTF-8 emoji 支持
json_maybe_utf8_emoji_test_() ->
    ?TEST_SIMPLE(fun() ->
        Json = unicode:characters_to_binary("{\"message\":\"你好 👋\",\"emoji\":\"😀\"}", utf8),
        Result = elib_cnv:json_maybe(Json),
        ?assertMatch(#{<<"message">> := _, <<"emoji">> := _}, Result),
        ?assertEqual(<<"你好 👋"/utf8>>, maps:get(<<"message">>, Result)),
        ?assertEqual(<<"😀"/utf8>>, maps:get(<<"emoji">>, Result))
    end).

%% @doc 测试 UTF-8 混合内容
json_maybe_utf8_mixed_test_() ->
    ?TEST_SIMPLE(fun() ->
        Json = unicode:characters_to_binary("{\"zh\":\"中文\",\"en\":\"English\",\"emoji\":\"🌍\"}", utf8),
        Result = elib_cnv:json_maybe(Json),
        ?assertMatch(#{<<"zh">> := _, <<"en">> := _, <<"emoji">> := _}, Result),
        ?assertEqual(<<"中文"/utf8>>, maps:get(<<"zh">>, Result)),
        ?assertEqual(<<"English">>, maps:get(<<"en">>, Result)),
        ?assertEqual(<<"🌍"/utf8>>, maps:get(<<"emoji">>, Result))
    end).

%% ===================================================================
%% implode/2 测试
%% ===================================================================

%% @doc 测试基本的 implode - 二进制
implode_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:implode(",", [<<"a">>, <<"b">>, <<"c">>]),
        ?assertEqual(<<"a,b,c">>, Result)
    end).

%% @doc 测试 implode - 字符串
implode_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:implode("', '", [<<"a">>, "b"]),
        ?assertEqual(<<"a', 'b">>, Result)
    end).

%% @doc 测试 implode - 整数
implode_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:implode(",", [1, 2, 3]),
        ?assertEqual(<<"1,2,3">>, Result)
    end).

%% @doc 测试 implode - 浮点数
implode_float_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:implode(",", [1, 2, 3.3]),
        FloatBin = ec_cnv:to_binary(3.3),
        ?assertEqual(<<"1,2,", FloatBin/binary>>, Result)
    end).

%% @doc 测试 implode - 空列表
implode_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:implode(",", []),
        ?assertEqual(<<>>, Result)
    end).

%% @doc 测试 implode - 整数分隔符
implode_integer_separator_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:implode(44, [<<"a">>, <<"b">>]),
        ?assertEqual(<<"a44b">>, Result)
    end).

%% ===================================================================
%% remove_dups/1 测试
%% ===================================================================

%% @doc 测试移除重复元素
remove_dups_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:remove_dups([1, 2, 2, 3, 3, 3]),
        ?assertEqual([1, 2, 3], Result)
    end).

%% @doc 测试空列表
remove_dups_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:remove_dups([]),
        ?assertEqual([], Result)
    end).

%% @doc 测试无重复元素
remove_dups_no_dups_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:remove_dups([1, 2, 3]),
        ?assertEqual([1, 2, 3], Result)
    end).

%% @doc 测试字符串列表
remove_dups_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:remove_dups([<<"a">>, <<"b">>, <<"a">>, <<"c">>]),
        ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Result)
    end).

%% ===================================================================
%% vsn_major/1 测试
%% ===================================================================

%% @doc 测试获取主版本号 - 完整版本
vsn_major_full_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:vsn_major("1.2.3"),
        ?assertEqual(<<"1">>, Result)
    end).

%% @doc 测试获取主版本号 - 二位版本
vsn_major_two_parts_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:vsn_major("2.3"),
        ?assertEqual(<<"2">>, Result)
    end).

%% @doc 测试获取主版本号 - 带预发布标识
vsn_major_prerelease_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:vsn_major("3.0.0-alpha"),
        ?assertEqual(<<"3">>, Result)
    end).

%% @doc 测试获取主版本号 - 带构建信息
vsn_major_build_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:vsn_major("1.2.3+build123"),
        ?assertEqual(<<"1">>, Result)
    end).

%% @doc 测试获取主版本号 - 整数输入
vsn_major_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertError({case_clause, 5}, elib_cnv:vsn_major(5))
    end).

%% ===================================================================
%% map_to_query/1 测试
%% ===================================================================

%% @doc 测试 map 转查询字符串
map_to_query_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:map_to_query(#{a => 1, b => 2, c => 3}),
        % 注意：map 的顺序可能不同
        ?assert(is_binary(Result)),
        ?assert(<<"a=1">> =:= Result orelse binary:match(Result, <<"a=1">>) =/= nomatch)
    end).

%% @doc 测试空 map
map_to_query_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:map_to_query(#{}),
        ?assertEqual(<<>>, Result)
    end).

%% @doc 测试单元素 map
map_to_query_single_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:map_to_query(#{key => <<"value">>}),
        ?assertEqual(<<"key=value">>, Result)
    end).

%% ===================================================================
%% list_to_binary_string/1 测试
%% ===================================================================

%% @doc 测试整数列表转字符串
list_to_binary_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:list_to_binary_string([513251, 62829, 62825]),
        ?assertEqual(<<"513251,62829,62825">>, Result)
    end).

%% @doc 测试单个元素
list_to_binary_string_single_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:list_to_binary_string([123]),
        ?assertEqual(<<"123">>, Result)
    end).

%% @doc 测试空列表
list_to_binary_string_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:list_to_binary_string([]),
        ?assertEqual(<<>>, Result)
    end).

%% ===================================================================
%% safe_to_binary/1 测试
%% ===================================================================

%% @doc 测试基本类型转换
safe_to_binary_string_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:safe_to_binary("hello"),
        ?assertEqual(<<"hello">>, Result)
    end).

%% @doc 测试二进制转换
safe_to_binary_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:safe_to_binary(<<"hello">>),
        ?assertEqual(<<"hello">>, Result)
    end).

%% @doc 测试整数转换
safe_to_binary_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:safe_to_binary(123),
        ?assertEqual(<<"123">>, Result)
    end).

%% @doc 测试原子转换
safe_to_binary_atom_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:safe_to_binary(hello),
        ?assertEqual(<<"hello">>, Result)
    end).

%% @doc 测试复杂结构转换
safe_to_binary_complex_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:safe_to_binary({error, reason, [details]}),
        ?assert(is_binary(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

%% @doc 测试 map 转换
safe_to_binary_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:safe_to_binary(#{key => <<"value">>}),
        ?assert(is_binary(Result)),
        ?assertNotEqual(<<>>, Result)
    end).

%% ===================================================================
%% convert_at_timestamps/1 测试
%% ===================================================================

%% @doc 测试空列表
convert_at_timestamps_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:convert_at_timestamps([]),
        ?assertEqual([], Result)
    end).

%% @doc 测试空 map
convert_at_timestamps_empty_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_cnv:convert_at_timestamps(#{}),
        ?assertEqual(#{}, Result)
    end).

%% @doc 测试带 _at 字段的 map
convert_at_timestamps_at_field_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = #{<<"created_at">> => <<"2024-01-01T00:00:00Z">>, <<"name">> => <<"test">>},
        Result = elib_cnv:convert_at_timestamps(Input),
        ?assertMatch(#{<<"created_at">> := _, <<"name">> := <<"test">>}, Result),
        % 验证时间戳是数字
        ?assert(is_number(maps:get(<<"created_at">>, Result)))
    end).

%% @doc 测试带 _ts 字段的 map
convert_at_timestamps_ts_field_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = #{<<"updated_ts">> => <<"2024-01-01T00:00:00Z">>},
        Result = elib_cnv:convert_at_timestamps(Input),
        ?assertMatch(#{<<"updated_ts">> := _}, Result),
        ?assert(is_number(maps:get(<<"updated_ts">>, Result)))
    end).

%% @doc 测试无时间字段的 map
convert_at_timestamps_no_time_field_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = #{<<"name">> => <<"test">>, <<"age">> => 25},
        Result = elib_cnv:convert_at_timestamps(Input),
        ?assertEqual(Input, Result)
    end).

%% @doc 测试 proplist
convert_at_timestamps_proplist_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = [{<<"created_at">>, <<"2024-01-01T00:00:00Z">>}, {<<"name">>, <<"test">>}],
        Result = elib_cnv:convert_at_timestamps(Input),
        ?assert(is_list(Result)),
        ?assert(length(Result) >= 1)
    end).

%% @doc 测试嵌套结构
convert_at_timestamps_nested_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = #{
            <<"user">> => #{
                <<"created_at">> => <<"2024-01-01T00:00:00Z">>
            }
        },
        Result = elib_cnv:convert_at_timestamps(Input),
        ?assertMatch(#{<<"user">> := #{<<"created_at">> := _}}, Result)
    end).

%% @doc 测试列表中的元素
convert_at_timestamps_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = [
            #{<<"created_at">> => <<"2024-01-01T00:00:00Z">>},
            #{<<"created_at">> => <<"2024-01-02T00:00:00Z">>}
        ],
        Result = elib_cnv:convert_at_timestamps(Input),
        ?assert(is_list(Result)),
        ?assertEqual(2, length(Result))
    end).

%% @doc 测试无效时间格式
convert_at_timestamps_invalid_time_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = #{<<"created_at">> => <<"invalid-date">>},
        Result = elib_cnv:convert_at_timestamps(Input),
        ?assertMatch(#{<<"created_at">> := {error, empty_input}}, Result)
    end).

%% @doc 测试空时间字符串
convert_at_timestamps_empty_time_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = #{<<"created_at">> => <<>>},
        Result = elib_cnv:convert_at_timestamps(Input),
        ?assertMatch(#{<<"created_at">> := {error, empty_input}}, Result)
    end).
