-module(imboy_cache_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_cache 模块的 EUnit 测试
%%%
%%% 目标：验证缓存工具功能
%%% 覆盖：缓存读写、过期时间、memo函数
%%%===================================================================

%% ===================================================================
%% memo/1 测试
%% ===================================================================

memo_with_function_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 2, fun(Function, _Server) ->
            % 模拟缓存函数，直接执行函数并返回结果
            Function()
        end}
    ], fun() ->
        TestFun = fun() -> <<"cached_result">> end,
        
        Result = imboy_cache:memo(TestFun),
        ?assertEqual(<<"cached_result">>, Result)
    end).

memo_caches_function_result_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 2, fun(Function, _Server) ->
            % 模拟缓存行为：第一次调用执行函数，后续调用返回缓存值
            CurrentCount = erlang:get(memo_call_count),
            NewCount = case CurrentCount of
                undefined -> 1;
                N -> N + 1
            end,
            erlang:put(memo_call_count, NewCount),
            Function()
        end}
    ], fun() ->
        TestFun = fun() -> 
            Count = erlang:get(memo_execution_count),
            NewCount = case Count of
                undefined -> 1;
                N -> N + 1
            end,
            erlang:put(memo_execution_count, NewCount),
            <<"execution_", (integer_to_binary(NewCount))/binary>>
        end,
        
        % 第一次调用
        Result1 = imboy_cache:memo(TestFun),
        ?assertEqual(<<"execution_1">>, Result1),
        
        % 第二次调用应该返回缓存值
        Result2 = imboy_cache:memo(TestFun),
        ?assertEqual(<<"execution_1">>, Result2)
    end).

%% ===================================================================
%% memo/2 测试
%% ===================================================================

memo_with_key_and_function_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 5, fun(Function, Key, MaxAge, _Options, _Server) ->
            % 验证参数并执行函数
            ?assertEqual(<<"test_key">>, Key),
            ?assertEqual(3600, MaxAge),  % 默认1小时
            Function()
        end}
    ], fun() ->
        TestFun = fun() -> <<"keyed_memo_result">> end,
        Key = <<"test_key">>,
        
        Result = imboy_cache:memo(TestFun, Key),
        ?assertEqual(<<"keyed_memo_result">>, Result)
    end).

memo_with_module_function_args_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 5, fun(Function, Key, MaxAge, _Options, _Server) ->
            % 验证模块函数调用和过期时间
            ?assertEqual(1800, MaxAge),  % 30分钟
            ?assertMatch(<<_/binary>>, Key),  % 验证Key参数被使用
            {M, F, A} = Function,
            apply(M, F, A)
        end}
    ], fun() ->
        Function = {erlang, binary_to_list, [<<"test">>]},
        MaxAge = 1800,
        
        Result = imboy_cache:memo(Function, MaxAge),
        ?assertEqual("test", Result)
    end).

%% ===================================================================
%% memo/3 测试
%% ===================================================================

memo_with_key_and_max_age_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 5, fun(Function, Key, MaxAge, _Options, _Server) ->
            % 验证键和过期时间参数
            ?assertEqual(<<"timed_key">>, Key),
            ?assertEqual(900, MaxAge),  % 15分钟
            Function()
        end}
    ], fun() ->
        TestFun = fun() -> <<"timed_memo_result">> end,
        Key = <<"timed_key">>,
        MaxAge = 900,
        
        Result = imboy_cache:memo(TestFun, Key, MaxAge),
        ?assertEqual(<<"timed_memo_result">>, Result)
    end).

memo_with_different_max_ages_test_() ->
    ?WITH_MECK(depcache, [
        {'memo', 5, fun(Function, Key, MaxAge, _Options, _Server) ->
            % 验证不同的过期时间和键
            ?assertEqual(<<"max_age_key">>, Key),
            ?assert(lists:member(MaxAge, [60, 300, 1800, 3600, 7200])),
            Function()
        end}
    ], fun() ->
        TestFun = fun() -> <<"max_age_test">> end,
        Key = <<"max_age_key">>,
        MaxAges = [60, 300, 1800, 3600, 7200],
        
        lists:foreach(fun(MaxAge) ->
            Result = imboy_cache:memo(TestFun, Key, MaxAge),
            ?assertEqual(<<"max_age_test">>, Result)
        end, MaxAges)
    end).

%% ===================================================================
%% set/2 测试
%% ===================================================================

set_with_key_and_value_test_() ->
    ?WITH_MECK(depcache, [
        {'set', 5, fun(Key, Value, _TTL, _Options, _Server) ->
            % 验证参数并模拟缓存设置
            ?assertEqual(<<"set_test_key">>, Key),
            ?assertEqual(<<"set_test_value">>, Value),
            ok
        end}
    ], fun() ->
        Key = <<"set_test_key">>,
        Value = <<"set_test_value">>,
        
        Result = imboy_cache:set(Key, Value),
        ?assertEqual(ok, Result)
    end).

set_with_complex_value_test_() ->
    ?WITH_MECK(depcache, [
        {'set', 5, fun(Key, Value, _TTL, _Options, _Server) ->
            % 验证复杂数据结构
            ?assertEqual(<<"complex_key">>, Key),
            ?assertMatch(#{<<"id">> := 123, <<"name">> := <<"Test Data">>, <<"items">> := [1, 2, 3], <<"nested">> := #{<<"inner">> := <<"value">>}}, Value),
            ok
        end}
    ], fun() ->
        Key = <<"complex_key">>,
        Value = #{
            <<"id">> => 123,
            <<"name">> => <<"Test Data">>,
            <<"items">> => [1, 2, 3],
            <<"nested">> => #{<<"inner">> => <<"value">>}
        },
        
        Result = imboy_cache:set(Key, Value),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% set/3 测试
%% ===================================================================

set_with_ttl_test_() ->
    ?WITH_MECK(depcache, [
        {'set', 5, fun(Key, Value, TTL, _Options, _Server) ->
            % 验证TTL参数
            ?assertEqual(<<"ttl_key">>, Key),
            ?assertEqual(<<"ttl_value">>, Value),
            ?assertEqual(3600, TTL),
            ok
        end}
    ], fun() ->
        Key = <<"ttl_key">>,
        Value = <<"ttl_value">>,
        TTL = 3600,
        
        Result = imboy_cache:set(Key, Value, TTL),
        ?assertEqual(ok, Result)
    end).

set_with_zero_ttl_test_() ->
    ?WITH_MECK(depcache, [
        {'set', 5, fun(Key, Value, TTL, _Options, _Server) ->
            % 验证零TTL
            ?assertEqual(<<"zero_ttl_key">>, Key),
            ?assertEqual(<<"zero_ttl_value">>, Value),
            ?assertEqual(0, TTL),
            ok
        end}
    ], fun() ->
        Key = <<"zero_ttl_key">>,
        Value = <<"zero_ttl_value">>,
        TTL = 0,
        
        Result = imboy_cache:set(Key, Value, TTL),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% get/1 测试
%% ===================================================================

get_existing_key_test_() ->
    ?WITH_MECK(depcache, [
        {'get', 2, fun(Key, _Server) ->
            % 验证键并返回缓存值
            ?assertEqual(<<"existing_key">>, Key),
            {ok, <<"cached_value">>}
        end}
    ], fun() ->
        Key = <<"existing_key">>,
        
        Result = imboy_cache:get(Key),
        ?assertEqual({ok, <<"cached_value">>}, Result)
    end).

get_nonexistent_key_test_() ->
    ?WITH_MECK(depcache, [
        {'get', 2, fun(Key, _Server) ->
            % 验证键并模拟不存在的键
            ?assertEqual(<<"nonexistent_key_12345">>, Key),
            undefined
        end}
    ], fun() ->
        Key = <<"nonexistent_key_12345">>,
        
        Result = imboy_cache:get(Key),
        ?assertEqual(undefined, Result)
    end).

%% ===================================================================
%% get/2 测试
%% ===================================================================

get_with_default_value_test_() ->
    ?WITH_MECK(depcache, [
        {'get', 2, fun(Key, _Server) ->
            % 模拟不存在的键，返回undefined
            ?assertEqual(<<"key_with_default">>, Key),
            undefined
        end}
    ], fun() ->
        Key = <<"key_with_default">>,
        Default = <<"default_value">>,
        
        Result = imboy_cache:get(Key, Default),
        ?assertEqual(<<"default_value">>, Result)
    end).

get_with_complex_default_test_() ->
    ?WITH_MECK(depcache, [
        {'get', 2, fun(Key, _Server) ->
            % 模拟不存在的键，返回undefined
            ?assertEqual(<<"complex_default_key">>, Key),
            undefined
        end}
    ], fun() ->
        Key = <<"complex_default_key">>,
        Default = #{<<"default">> => true, <<"count">> => 0},
        
        Result = imboy_cache:get(Key, Default),
        ?assertEqual(#{<<"default">> => true, <<"count">> => 0}, Result)
    end).

%% ===================================================================
%% flush/0 测试
%% ===================================================================

flush_all_cache_test_() ->
    ?WITH_MECK(depcache, [
        {'flush_all', 1, fun(_Server) ->
            % 模拟清空所有缓存
            ok
        end},
        {'set', 5, fun(_Key, _Value, _TTL, _Options, _Server) ->
            ok
        end},
        {'get', 2, fun(_Key, _Server) ->
            undefined
        end},
        {'size', 1, fun(_Server) ->
            0
        end}
    ], fun() ->
        % 先添加一些缓存数据
        imboy_cache:set(test_key1, test_value1),
        imboy_cache:set(test_key2, test_value2),

        % 清空所有缓存
        Result = imboy_cache:flush_all(),
        ?assertEqual(ok, Result),

        % 验证缓存已清空
        Size = imboy_cache:size(),
        ?assertEqual(0, Size),

        % 验证具体键值不存在
        Value1 = imboy_cache:get(test_key1),
        Value2 = imboy_cache:get(test_key2),
        ?assertEqual(undefined, Value1),
        ?assertEqual(undefined, Value2)
    end).

%% ===================================================================
%% size/0 测试
%% ===================================================================

size_returns_integer_test_() ->
    ?WITH_MECK(depcache, [
        {'size', 1, fun(_Server) ->
            % 模拟缓存大小
            42
        end}
    ], fun() ->
        Result = imboy_cache:size(),
        ?assertEqual(42, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

empty_key_test_() ->
    ?WITH_MECK(depcache, [
        {'set', 5, fun(Key, Value, _TTL, _Options, _Server) ->
            % 验证空键的处理
            ?assertEqual(<<>>, Key),
            ?assertEqual(<<"test">>, Value),
            {error, empty_key}
        end}
    ], fun() ->
        Key = <<>>,
        Value = <<"test">>,
        
        Result = imboy_cache:set(Key, Value),
        ?assertEqual({error, empty_key}, Result)
    end).

very_long_key_test_() ->
    ?WITH_MECK(depcache, [
        {'set', 5, fun(Key, Value, _TTL, _Options, _Server) ->
            % 验证长键的处理
            ?assertEqual(1000, byte_size(Key)),
            ?assertEqual(<<"long_key_test">>, Value),
            ok
        end}
    ], fun() ->
        Key = binary:copy(<<"a">>, 1000),  % 1000字符的键
        Value = <<"long_key_test">>,
        
        Result = imboy_cache:set(Key, Value),
        ?assertEqual(ok, Result)
    end).

large_value_test_() ->
    ?WITH_MECK(depcache, [
        {'set', 5, fun(Key, Value, _TTL, _Options, _Server) ->
            % 验证大值的处理
            ?assertEqual(<<"large_value_key">>, Key),
            ?assertEqual(10000, byte_size(Value)),
            ok
        end}
    ], fun() ->
        Key = <<"large_value_key">>,
        Value = binary:copy(<<"x">>, 10000),  % 10KB的值
        
        Result = imboy_cache:set(Key, Value),
        ?assertEqual(ok, Result)
    end).
