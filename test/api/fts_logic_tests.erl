-module(fts_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_logic 模块的 EUnit 测试
%%%
%%% 目标：验证全文搜索业务逻辑功能
%%% 覆盖：用户搜索、最近用户、搜索结果排序
%%%===================================================================

%% 测试常量定义
-define(TEST_UID, 12345).
-define(TEST_KEYWORD, <<"john">>).
-define(TEST_PAGE, 1).
-define(TEST_SIZE, 10).

%% ===================================================================
%% user_search_page/4 测试
%% ===================================================================

user_search_page_with_results_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'count_for_user_search_page', 1, fun(_Keyword) -> 2 end},
        {'user_search_page', 3, fun(_Keyword, _Size, _Offset) ->
            {ok, [
                #{<<"uid">> => 67890, <<"nickname">> => <<"John Doe">>, <<"avatar">> => <<"avatar1.jpg">>,
                  <<"gender">> => 1, <<"signature">> => <<"sig1">>, <<"created_at">> => <<"2023-01-01">>},
                #{<<"uid">> => 67891, <<"nickname">> => <<"Johnny Smith">>, <<"avatar">> => <<"avatar2.jpg">>,
                  <<"gender">> => 2, <<"signature">> => <<"sig2">>, <<"created_at">> => <<"2023-01-02">>}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend', 3, fun(_Uid1, _Uid2, _Fields) -> {true, <<"同事"/utf8>>} end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(Uid) -> <<"encoded_", (integer_to_binary(Uid))/binary>> end}
            ], fun() ->
                Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, ?TEST_KEYWORD),
                ?assertMatch(#{<<"total">> := 2, <<"page">> := ?TEST_PAGE, <<"size">> := ?TEST_SIZE, <<"list">> := [_|_]}, Result),
                #{<<"list">> := List} = Result,
                ?assertEqual(2, length(List))
            end)
        end)
    end).

user_search_page_empty_keyword_returns_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, <<>>),
        ?assertMatch(#{<<"total">> := 0, <<"page">> := ?TEST_PAGE, <<"size">> := ?TEST_SIZE, <<"list">> := []}, Result)
    end).

user_search_page_filters_current_user_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'count_for_user_search_page', 1, fun(_Keyword) -> 2 end},
        {'user_search_page', 3, fun(_Keyword, _Size, _Offset) ->
            {ok, [
                #{<<"uid">> => ?TEST_UID, <<"nickname">> => <<"Current User">>},
                #{<<"uid">> => 67890, <<"nickname">> => <<"Other User">>}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend', 3, fun(_Uid1, _Uid2, _Fields) -> {false, <<>>} end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(Uid) -> <<"encoded_", (integer_to_binary(Uid))/binary>> end}
            ], fun() ->
                Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, ?TEST_KEYWORD),
                #{<<"list">> := List} = Result,
                % 当前用户应该被过滤掉
                ?assertEqual(1, length(List)),
                [#{<<"uid">> := EncodedUid}] = List,
                ?assertNotEqual(<<"encoded_", (integer_to_binary(?TEST_UID))/binary>>, EncodedUid)
            end)
        end)
    end).

user_search_page_empty_results_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'count_for_user_search_page', 1, fun(_Keyword) -> 0 end},
        {'user_search_page', 3, fun(_Keyword, _Size, _Offset) -> {ok, []} end}
    ], fun() ->
        Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, ?TEST_KEYWORD),
        ?assertMatch(#{<<"total">> := 0, <<"list">> := []}, Result)
    end).

user_search_page_db_error_returns_empty_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'count_for_user_search_page', 1, fun(_Keyword) -> 0 end},
        {'user_search_page', 3, fun(_Keyword, _Size, _Offset) -> {error, db_error} end}
    ], fun() ->
        Result = fts_logic:user_search_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, ?TEST_KEYWORD),
        ?assertMatch(#{<<"total">> := 0, <<"list">> := []}, Result)
    end).

%% ===================================================================
%% recently_user_page/4 测试
%% ===================================================================

recently_user_page_without_keyword_test_() ->
    ?WITH_MECK(elib_pg, [
        {'page_with_total', 6, fun(_Tb, _Column, _WhereMap, _OrderBy, _Page, _Size) ->
            {ok, #{total => 2, list => [
                #{<<"id">> => 67890, <<"nickname">> => <<"Alice">>, <<"avatar">> => <<"avatar1.jpg">>,
                  <<"gender">> => 1, <<"signature">> => <<"sig1">>, <<"region">> => <<"Beijing">>,
                  <<"created_at">> => <<"2023-01-01">>},
                #{<<"id">> => 67891, <<"nickname">> => <<"Bob">>, <<"avatar">> => <<"avatar2.jpg">>,
                  <<"gender">> => 2, <<"signature">> => <<"sig2">>, <<"region">> => <<"Shanghai">>,
                  <<"created_at">> => <<"2023-01-02">>}
            ]}}
        end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend', 3, fun(_Uid1, _Uid2, _Fields) -> {true, <<"朋友"/utf8>>} end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(Uid) -> <<"encoded_", (integer_to_binary(Uid))/binary>> end}
            ], fun() ->
                Result = fts_logic:recently_user_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, <<>>),
                ?assertMatch(#{<<"total">> := 2, <<"list">> := [_|_]}, Result),
                #{<<"list">> := List} = Result,
                ?assertEqual(2, length(List))
            end)
        end)
    end).

recently_user_page_with_keyword_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'count_for_user_search_page', 1, fun(_Keyword) -> 1 end},
        {'user_search_page', 3, fun(_Keyword, _Size, _Offset) ->
            {ok, [
                #{<<"uid">> => 67890, <<"nickname">> => <<"Alice">>, <<"avatar">> => <<"avatar1.jpg">>,
                  <<"gender">> => 1, <<"signature">> => <<"sig1">>, <<"region">> => <<"Beijing">>,
                  <<"created_at">> => <<"2023-01-01">>}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend', 3, fun(_Uid1, _Uid2, _Fields) -> {false, <<>>} end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(Uid) -> <<"encoded_", (integer_to_binary(Uid))/binary>> end}
            ], fun() ->
                Result = fts_logic:recently_user_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, <<"alice">>),
                ?assertMatch(#{<<"total">> := 1, <<"list">> := [_]}, Result)
            end)
        end)
    end).

recently_user_page_error_returns_empty_test_() ->
    ?WITH_MECK(elib_pg, [
        {'page_with_total', 6, fun(_Tb, _Column, _WhereMap, _OrderBy, _Page, _Size) ->
            {error, db_error}
        end}
    ], fun() ->
        Result = fts_logic:recently_user_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, <<>>),
        ?assertMatch(#{<<"total">> := 0, <<"list">> := []}, Result)
    end).

recently_user_page_filters_current_user_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'count_for_user_search_page', 1, fun(_Keyword) -> 2 end},
        {'user_search_page', 3, fun(_Keyword, _Size, _Offset) ->
            {ok, [
                #{<<"uid">> => ?TEST_UID, <<"nickname">> => <<"Current User">>, <<"avatar">> => <<"avatar.jpg">>,
                  <<"gender">> => 1, <<"signature">> => <<"sig">>, <<"region">> => <<"Beijing">>,
                  <<"created_at">> => <<"2023-01-01">>},
                #{<<"uid">> => 67890, <<"nickname">> => <<"Other User">>, <<"avatar">> => <<"avatar2.jpg">>,
                  <<"gender">> => 2, <<"signature">> => <<"sig2">>, <<"region">> => <<"Shanghai">>,
                  <<"created_at">> => <<"2023-01-02">>}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend', 3, fun(_Uid1, _Uid2, _Fields) -> {false, <<>>} end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(Uid) -> <<"encoded_", (integer_to_binary(Uid))/binary>> end}
            ], fun() ->
                Result = fts_logic:recently_user_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, <<"test">>),
                #{<<"list">> := List} = Result,
                % 当前用户应该被过滤掉
                ?assertEqual(1, length(List))
            end)
        end)
    end).

recently_user_page_empty_results_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'count_for_user_search_page', 1, fun(_Keyword) -> 0 end},
        {'user_search_page', 3, fun(_Keyword, _Size, _Offset) -> {ok, []} end}
    ], fun() ->
        Result = fts_logic:recently_user_page(?TEST_UID, ?TEST_PAGE, ?TEST_SIZE, <<"test">>),
        ?assertMatch(#{<<"total">> := 0, <<"list">> := []}, Result)
    end).
