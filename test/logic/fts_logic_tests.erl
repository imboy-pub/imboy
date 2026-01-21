-module(fts_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_logic 模块的 EUnit 测试
%%%
%%% 目标：验证全文搜索功能
%%% 覆盖：用户搜索、关键词匹配、分页、边界条件
%%%===================================================================

%% ===================================================================
%% user_search/3 测试
%% ===================================================================

user_search_returns_results_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, [
                #{<<"id">> => 1, <<"nickname">> => <<"用户1"/utf8>>},
                #{<<"id">> => 2, <<"nickname">> => <<"用户2"/utf8>>}
            ]}
        end}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_, _]}, Result)
    end).

user_search_with_empty_keyword_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Keyword = <<>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).

user_search_with_no_results_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Keyword = <<"不存在的用户"/utf8>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).

user_search_with_pagination_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, Limit, Offset) ->
            % 返回第二页结果
            Start = Offset + 1,
            StartBin = integer_to_binary(Start),
            StartBin2 = integer_to_binary(Start + 1),
            {ok, [
                #{<<"id">> => Start, <<"nickname">> => StartBin},
                #{<<"id">> => Start + 1, <<"nickname">> => StartBin2}
            ]}
        end}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        Limit = 10,
        Offset = 10,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_, _]}, Result)
    end).

user_search_with_custom_limit_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, Limit, _Offset) ->
            {ok, lists:duplicate(Limit, #{<<"id">> => 1, <<"nickname">> => <<"测试"/utf8>>})}
        end}
    ], fun() ->
        Keyword = <<"测试"/utf8>>,
        Limit = 20,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        {ok, Users} = Result,
        ?assertEqual(20, length(Users))
    end).

user_search_with_special_characters_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, [#{<<"id">> => 1, <<"nickname">> => <<"用户@#$"/utf8>>}]}
        end}
    ], fun() ->
        Keyword = <<"@#$"/utf8>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_]}, Result)
    end).

user_search_with_english_keyword_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, [#{<<"id">> => 1, <<"nickname">> => <<"Alice"/utf8>>}]}
        end}
    ], fun() ->
        Keyword = <<"Alice">>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_]}, Result)
    end).

user_search_with_mixed_language_keyword_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, [#{<<"id">> => 1, <<"nickname">> => <<"测试Alice"/utf8>>}]}
        end}
    ], fun() ->
        Keyword = <<"测试Alice"/utf8>>,
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertMatch({ok, [_]}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

user_search_with_zero_limit_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        Limit = 0,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).

user_search_with_large_offset_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Keyword = <<"用户"/utf8>>,
        Limit = 10,
        Offset = 999999,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).

user_search_with_long_keyword_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        % 创建一个超长的关键词
        Keyword = binary:copy(<<"测"/utf8>>, 100),
        Limit = 10,
        Offset = 0,
        Result = fts_logic:user_search(Keyword, Limit, Offset),
        ?assertEqual({ok, []}, Result)
    end).
