-module(imboy_kv_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_kv 模块的 EUnit 测试
%%%
%%% 目标：验证键值对工具功能
%%% 覆盖：Map 操作、列表转换
%%%===================================================================

%% ===================================================================
%% Map 操作测试
%% ===================================================================

get_value_from_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{key => <<"value">>},
        Key = key,
        Result = imboy_kv:get(Map, Key),
        ?assertEqual(<<"value">>, Result)
    end).

get_value_with_default_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{},
        Key = missing_key,
        Default = <<"default">>,
        Result = imboy_kv:get(Map, Key, Default),
        ?assertEqual(Default, Result)
    end).

set_value_in_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{},
        Key = new_key,
        Value = <<"new_value">>,
        Result = imboy_kv:set(Map, Key, Value),
        ?assertEqual(Value, maps:get(Key, Result))
    end).

%% ===================================================================
%% 列表转换测试
%% ===================================================================

list_to_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = [{a, 1}, {b, 2}],
        Result = imboy_kv:from_list(List),
        Expected = #{a => 1, b => 2},
        ?assertEqual(Expected, Result)
    end).

map_to_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{a => 1, b => 2},
        Result = imboy_kv:to_list(Map),
        % 验证结果包含所有键值对
        ?assert(length(Result) =:= 2),
        ?assert(lists:keymember(a, 1, Result)),
        ?assert(lists:keymember(b, 1, Result))
    end).

%% ===================================================================
%% 删除键测试
%% ===================================================================

delete_key_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{a => 1, b => 2, c => 3},
        Key = b,
        Result = imboy_kv:delete(Map, Key),
        Expected = #{a => 1, c => 3},
        ?assertEqual(Expected, Result),
        ?assertNot(maps:is_key(Key, Result))
    end).

delete_nonexistent_key_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{a => 1, b => 2},
        Key = missing,
        Result = imboy_kv:delete(Map, Key),
        ?assertEqual(Map, Result)
    end).

%% ===================================================================
%% 合并测试
%% ===================================================================

merge_maps_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map1 = #{a => 1, b => 2},
        Map2 = #{b => 3, c => 4},
        Result = imboy_kv:merge(Map1, Map2),
        Expected = #{a => 1, b => 3, c => 4},
        ?assertEqual(Expected, Result)
    end).

merge_with_empty_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{a => 1, b => 2},
        Empty = #{},
        Result = imboy_kv:merge(Map, Empty),
        ?assertEqual(Map, Result)
    end).

%% ===================================================================
%% 键存在性测试
%% ===================================================================

has_key_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{a => 1, b => 2},
        ?assert(imboy_kv:has_key(Map, a)),
        ?assert(imboy_kv:has_key(Map, b)),
        ?assertNot(imboy_kv:has_key(Map, c))
    end).

%% ===================================================================
%% 过滤测试
%% ===================================================================

filter_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{a => 1, b => 2, c => 3, d => 4},
        Fun = fun(_K, V) -> V rem 2 =:= 0 end,
        Result = imboy_kv:filter(Map, Fun),
        Expected = #{b => 2, d => 4},
        ?assertEqual(Expected, Result)
    end).

%% ===================================================================
%% 映射测试
%% ===================================================================

map_values_test_() ->
    ?TEST_SIMPLE(fun() ->
        Map = #{a => 1, b => 2, c => 3},
        Fun = fun(V) -> V * 2 end,
        Result = imboy_kv:map(Map, Fun),
        Expected = #{a => 2, b => 4, c => 6},
        ?assertEqual(Expected, Result)
    end).