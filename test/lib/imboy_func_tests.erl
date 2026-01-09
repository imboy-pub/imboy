-module(imboy_func_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_func 模块的 EUnit 测试
%%%
%%% 目标：验证函数工具功能
%%% 覆盖：随机数生成、列表操作
%%%===================================================================

%% ===================================================================
%% 随机数生成测试
%% ===================================================================

num_random_generates_integer_test_() ->
    ?TEST_SIMPLE(fun() ->
        Length = 40,
        Result = imboy_func:num_random(Length),
        % 验证返回的是整数
        ?assert(is_integer(Result)),
        % 验证数字在合理范围内
        ?assert(Result >= 0),
        % 验证数字长度符合要求（对于40位数字，应该在10^39到10^40-1之间）
        ?assert(Result >= trunc(math:pow(10, Length-1)))
    end).

num_random_with_different_lengths_test_() ->
    ?TEST_SIMPLE(fun() ->
        Length1 = 10,
        Length2 = 20,
        Result1 = imboy_func:num_random(Length1),
        Result2 = imboy_func:num_random(Length2),
        % 验证不同长度生成的数字位数不同
        ?assert(Result1 < trunc(math:pow(10, Length1))),
        ?assert(Result2 >= trunc(math:pow(10, Length2-1))),
        % 验证两个结果不相等（极小概率相等，但测试中可忽略）
        ?assert(Result1 =/= Result2)
    end).

%% ===================================================================
%% 列表工具测试
%% ===================================================================

list_shuffle_test_() ->
    ?TEST_SIMPLE(fun() ->
        OriginalList = [1, 2, 3, 4, 5],
        ShuffledList = imboy_func:list_shuffle(OriginalList),
        % 验证返回的是列表
        ?assertMatch([_|_], ShuffledList),
        % 验证长度不变
        ?assertEqual(length(OriginalList), length(ShuffledList)),
        % 验证包含相同的元素（使用排序比较）
        ?assertEqual(lists:sort(OriginalList), lists:sort(ShuffledList)),
        % 对于小列表，有可能打乱后顺序相同，但多次测试应该有不同的结果
        % 这里我们验证元素都是有效的
        lists:foreach(fun(Element) ->
            ?assert(lists:member(Element, OriginalList))
        end, ShuffledList)
    end).

list_unique_test_() ->
    ?TEST_SIMPLE(fun() ->
        ListWithDuplicates = [1, 2, 2, 3, 3, 3],
        UniqueList = imboy_func:list_unique(ListWithDuplicates),
        % 验证返回的是列表
        ?assertMatch([_|_], UniqueList),
        % 验证去重后的元素
        ExpectedUnique = [1, 2, 3],
        ?assertEqual(lists:sort(ExpectedUnique), lists:sort(UniqueList)),
        % 验证长度减少
        ?assert(length(UniqueList) < length(ListWithDuplicates)),
        % 验证所有元素都来自原列表
        lists:foreach(fun(Element) ->
            ?assert(lists:member(Element, ListWithDuplicates))
        end, UniqueList)
    end).
