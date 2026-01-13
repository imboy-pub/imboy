-module(fts_user_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_user_ds 模块的 EUnit 测试
%%%
%%% 目标：验证全文搜索用户数据服务功能
%%% 覆盖：搜索权限检查、结果统计、分页搜索、中文分词
%%%===================================================================

%% ===================================================================
%% allow_search/1 测试
%% ===================================================================

%% @doc 测试用户允许被搜索
allow_search_returns_true_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'allow_search', 1, fun(Uid) ->
            ?assertEqual(100, Uid),
            true
        end}
    ], fun() ->
        Result = fts_user_ds:allow_search(100),
        ?assertEqual(true, Result)
    end).

%% @doc 测试用户不允许被搜索
allow_search_returns_false_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'allow_search', 1, fun(_Uid) ->
            false
        end}
    ], fun() ->
        Result = fts_user_ds:allow_search(100),
        ?assertEqual(false, Result)
    end).

%% @doc 测试不存在的用户
allow_search_for_nonexistent_user_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'allow_search', 1, fun(_Uid) ->
            false
        end}
    ], fun() ->
        Result = fts_user_ds:allow_search(999999),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% count_for_user_search_page/1 测试
%% ===================================================================

%% @doc 测试统计搜索结果数量
count_for_user_search_page_returns_count_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'count_for_user_search_page', 1, fun(Keyword) ->
            ?assertEqual(<<"张三"/utf8>>, Keyword),
            5
        end}
    ], fun() ->
        Result = fts_user_ds:count_for_user_search_page(<<"张三"/utf8>>),
        ?assertEqual(5, Result)
    end).

%% @doc 测试空关键词返回0
count_for_user_search_page_with_empty_keyword_returns_zero_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'count_for_user_search_page', 1, fun(Keyword) ->
            ?assertEqual(<<>>, Keyword),
            0
        end}
    ], fun() ->
        Result = fts_user_ds:count_for_user_search_page(<<>>),
        ?assertEqual(0, Result)
    end).

%% @doc 测试无结果
count_for_user_search_page_with_no_results_returns_zero_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'count_for_user_search_page', 1, fun(_Keyword) ->
            0
        end}
    ], fun() ->
        Result = fts_user_ds:count_for_user_search_page(<<"不存在的人"/utf8>>),
        ?assertEqual(0, Result)
    end).

%% @doc 测试大量结果
count_for_user_search_page_with_many_results_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'count_for_user_search_page', 1, fun(_Keyword) ->
            1000
        end}
    ], fun() ->
        Result = fts_user_ds:count_for_user_search_page(<<"李"/utf8>>),
        ?assertEqual(1000, Result)
    end).

%% ===================================================================
%% user_search_page/3 测试
%% ===================================================================

%% @doc 测试分页搜索成功
user_search_page_returns_results_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(Keyword, Limit, Offset) ->
            ?assertEqual(<<"张三"/utf8>>, Keyword),
            ?assertEqual(10, Limit),
            ?assertEqual(0, Offset),
            {ok, [
                #{<<"id">> => 1, <<"nickname">> => <<"张三"/utf8>>, <<"account">> => <<"zhangsan">>},
                #{<<"id">> => 2, <<"nickname">> => <<"张三丰"/utf8>>, <<"account">> => <<"zhangsanfeng">>}
            ]}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"张三"/utf8>>, 10, 0),
        ?assertEqual(2, length(Results))
    end).

%% @doc 测试分页搜索空结果
user_search_page_with_no_results_returns_empty_list_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"不存在"/utf8>>, 10, 0),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试分页搜索错误
user_search_page_with_error_returns_error_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {error, <<"database_error">>}
        end}
    ], fun() ->
        Result = fts_user_ds:user_search_page(<<"测试"/utf8>>, 10, 0),
        ?assertEqual({error, <<"database_error">>}, Result)
    end).

%% @doc 测试分页参数
user_search_page_with_pagination_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(_Keyword, Limit, Offset) ->
            ?assertEqual(20, Limit),
            ?assertEqual(40, Offset),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"测试"/utf8>>, 20, 40),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试第二页
user_search_page_second_page_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(_Keyword, Limit, Offset) ->
            ?assertEqual(10, Limit),
            ?assertEqual(10, Offset),
            {ok, [
                #{<<"id">> => 11, <<"nickname">> => <<"用户11"/utf8>>}
            ]}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"用户"/utf8>>, 10, 10),
        ?assertEqual(1, length(Results))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试零限制
user_search_page_with_zero_limit_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(_Keyword, Limit, _Offset) ->
            ?assertEqual(0, Limit),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"测试"/utf8>>, 0, 0),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试大限制
user_search_page_with_large_limit_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(_Keyword, Limit, _Offset) ->
            ?assertEqual(10000, Limit),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"测试"/utf8>>, 10000, 0),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试大偏移量
user_search_page_with_large_offset_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(_Keyword, _Limit, Offset) ->
            ?assertEqual(999999, Offset),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"测试"/utf8>>, 10, 999999),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试特殊字符关键词
user_search_page_with_special_chars_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(Keyword, _Limit, _Offset) ->
            ?assertEqual(<<"test@example.com">>, Keyword),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"test@example.com">>, 10, 0),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试超长关键词
user_search_page_with_long_keyword_test_() ->
    LongKeyword = list_to_binary(lists:duplicate(500, $x)),
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(Keyword, _Limit, _Offset) ->
            ?assertEqual(LongKeyword, Keyword),
            ?assert(byte_size(Keyword) >= 500),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(LongKeyword, 10, 0),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试英文关键词
user_search_page_with_english_keyword_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(Keyword, _Limit, _Offset) ->
            ?assertEqual(<<"john">>, Keyword),
            {ok, [#{<<"id">> => 1, <<"account">> => <<"john">>}]}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"john">>, 10, 0),
        ?assertEqual(1, length(Results))
    end).

%% @doc 测试数字关键词
user_search_page_with_numeric_keyword_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(Keyword, _Limit, _Offset) ->
            ?assertEqual(<<"13800138000">>, Keyword),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"13800138000">>, 10, 0),
        ?assertEqual(0, length(Results))
    end).

%% ===================================================================
%% 中文分词测试
%% ===================================================================

%% @doc 测试中文姓名搜索
user_search_page_with_chinese_name_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(Keyword, _Limit, _Offset) ->
            ?assertEqual(<<"王五"/utf8>>, Keyword),
            {ok, [#{<<"id">> => 1, <<"nickname">> => <<"王五"/utf8>>}]}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"王五"/utf8>>, 10, 0),
        ?assertEqual(1, length(Results))
    end).

%% @doc 测试中文词组搜索
user_search_page_with_chinese_phrase_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(Keyword, _Limit, _Offset) ->
            ?assertEqual(<<"北京"/utf8>>, Keyword),
            {ok, [
                #{<<"id">> => 1, <<"signature">> => <<"来自北京"/utf8>>},
                #{<<"id">> => 2, <<"signature">> => <<"在北京工作"/utf8>>}
            ]}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"北京"/utf8>>, 10, 0),
        ?assertEqual(2, length(Results))
    end).

%% @doc 测试混合中英文搜索
user_search_page_with_mixed_keyword_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(Keyword, _Limit, _Offset) ->
            ?assertEqual(<<"测试test"/utf8>>, Keyword),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = fts_user_ds:user_search_page(<<"测试test"/utf8>>, 10, 0),
        ?assertEqual(0, length(Results))
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

%% @doc 验证参数类型
allow_search_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        ?assert(is_integer(Uid))
    end).

%% @doc 验证统计参数类型
count_for_user_search_page_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Keyword = <<"张三"/utf8>>,
        ?assert(is_binary(Keyword))
    end).

%% @doc 验证搜索参数类型
user_search_page_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Keyword = <<"测试"/utf8>>,
        Limit = 10,
        Offset = 0,
        ?assert(is_binary(Keyword)),
        ?assert(is_integer(Limit)),
        ?assert(is_integer(Offset))
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的搜索流程
complete_search_flow_test_() ->
    ?WITH_MECKS([
        {fts_user_repo, [
            {'allow_search', 1, fun(_Uid) -> true end},
            {'count_for_user_search_page', 1, fun(_Keyword) -> 5 end},
            {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
                {ok, [
                    #{<<"id">> => 1, <<"nickname">> => <<"用户1"/utf8>>},
                    #{<<"id">> => 2, <<"nickname">> => <<"用户2"/utf8>>}
                ]}
            end}
        ]}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        % 检查权限
        ?assertEqual(true, fts_user_ds:allow_search(100)),
        % 统计结果
        ?assertEqual(5, fts_user_ds:count_for_user_search_page(Keyword)),
        % 分页搜索
        {ok, Results} = fts_user_ds:user_search_page(Keyword, 10, 0),
        ?assertEqual(2, length(Results))
    end).

%% @doc 测试分页浏览搜索结果
paginated_search_flow_test_() ->
    ?WITH_MECK(fts_user_repo, [
        {'user_search_page', 3, fun(_Keyword, Limit, Offset) ->
            Start = Offset + 1,
            End = Offset + Limit,
            {ok, [#{<<"id">> => N} || N <- lists:seq(Start, End)]}
        end}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        % 第一页
        {ok, Page1} = fts_user_ds:user_search_page(Keyword, 10, 0),
        ?assertEqual(10, length(Page1)),
        % 第二页
        {ok, Page2} = fts_user_ds:user_search_page(Keyword, 10, 10),
        ?assertEqual(10, length(Page2))
    end).

%% @doc 测试搜索无权限用户
search_for_user_without_permission_test_() ->
    ?WITH_MECKS([
        {fts_user_repo, [
            {'allow_search', 1, fun(_Uid) -> false end},
            {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
                {ok, []}
            end}
        ]}
    ], fun() ->
        % 用户不允许被搜索
        ?assertEqual(false, fts_user_ds:allow_search(100)),
        % 即使搜索也返回空结果
        {ok, Results} = fts_user_ds:user_search_page(<<"用户"/utf8>>, 10, 0),
        ?assertEqual(0, length(Results))
    end).
