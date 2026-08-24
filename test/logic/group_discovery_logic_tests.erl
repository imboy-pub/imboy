-module(group_discovery_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc group_discovery_logic 的业务逻辑单元测试
%%% 覆盖：搜索、发现、精选、热门、分类、预览
%%%===================================================================

%% ===================================================================
%% 搜索（search）
%% ===================================================================

%% 空关键词搜索返回错误
search_empty_keyword_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch({error, _}, group_discovery_logic:search(<<>>, 1, 20))
    end).

%% 搜索调用 DS 层并返回正确格式
search_returns_paginated_results_test_() ->
    ?WITH_MECKS(
        [
            {fts_group_ds, [
                {'count_for_group_search', 1, fun(<<"test">>) -> 5 end},
                {'group_search_page', 4, fun(<<"test">>, 20, 0, undefined) ->
                    {ok, [#{<<"id">> => 1, <<"title">> => <<"Test Group">>}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = group_discovery_logic:search(<<"test">>, 1, 20),
            ?assertEqual(5, maps:get(<<"total">>, Result)),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% 搜索支持分类筛选
search_with_category_filters_by_category_test_() ->
    ?WITH_MECKS(
        [
            {fts_group_ds, [
                {'count_for_group_search', 1, fun(<<"test">>) -> 3 end},
                {'group_search_page', 4, fun(<<"test">>, 20, 0, 1) ->
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = group_discovery_logic:search(<<"test">>, 1, 20, 1),
            ?assertEqual(3, maps:get(<<"total">>, Result))
        end
    ).

%% DS 层返回错误时不透传原始 term（P2-8e 中文化兜底），返回通用中文文案
search_propagates_ds_error_test_() ->
    ?WITH_MECKS(
        [
            {fts_group_ds, [
                {'count_for_group_search', 1, fun(<<"test">>) -> 0 end},
                {'group_search_page', 4, fun(<<"test">>, 20, 0, undefined) ->
                    {error, db_error}
                end}
            ]}
        ],
        fun() ->
            {error, Msg} = group_discovery_logic:search(<<"test">>, 1, 20),
            ?assertEqual(<<"搜索失败，请稍后重试"/utf8>>, Msg)
        end
    ).

%% ===================================================================
%% 发现（discover）
%% ===================================================================

%% 发现页默认按 popular 排序
discover_defaults_to_popular_sort_test_() ->
    ?WITH_MECKS(
        [
            {fts_group_ds, [
                {'discover_groups', 4, fun(1, 20, undefined, <<"member_count DESC">>) ->
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = group_discovery_logic:discover(1, 20, undefined, <<"popular">>),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% 发现页支持 newest 排序
discover_supports_newest_sort_test_() ->
    ?WITH_MECKS(
        [
            {fts_group_ds, [
                {'discover_groups', 4, fun(1, 20, undefined, <<"created_at DESC">>) ->
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, _} = group_discovery_logic:discover(1, 20, undefined, <<"newest">>)
        end
    ).

%% 发现页支持分类筛选
discover_with_category_test_() ->
    ?WITH_MECKS(
        [
            {fts_group_ds, [
                {'discover_groups', 4, fun(1, 20, 2, <<"member_count DESC">>) ->
                    {ok, [#{<<"id">> => 2}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = group_discovery_logic:discover(1, 20, 2, <<"popular">>),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% ===================================================================
%% 精选（featured）
%% ===================================================================

%% 精选调用 DS 层并返回正确格式
featured_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {fts_group_ds, [
                {'featured_groups', 1, fun(5) ->
                    {ok, [#{<<"id">> => 1, <<"title">> => <<"Featured">>}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = group_discovery_logic:featured(5),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% ===================================================================
%% 热门（hot）
%% ===================================================================

%% 热门调用 DS 层并返回正确格式
hot_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {fts_group_ds, [
                {'hot_groups', 1, fun(10) ->
                    {ok, [#{<<"id">> => 1, <<"title">> => <<"Hot Group">>}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = group_discovery_logic:hot(10),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% ===================================================================
%% 分类（categories）
%% ===================================================================

%% 分类查询返回正确格式
categories_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, _Params) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"group_category">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"status = 1">>)),
                    {ok, [
                        #{<<"id">> => 1, <<"name">> => <<"技术交流"/utf8>>, <<"icon">> => <<"code">>}
                    ]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = group_discovery_logic:categories(),
            List = maps:get(<<"list">>, Result),
            ?assertEqual(1, length(List))
        end
    ).

%% 分类查询出错时返回空列表
categories_returns_empty_list_on_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {error, db_error} end}
            ]}
        ],
        fun() ->
            {ok, Result} = group_discovery_logic:categories(),
            ?assertEqual([], maps:get(<<"list">>, Result))
        end
    ).

%% ===================================================================
%% 预览（preview）
%% ===================================================================

%% 预览返回公开群信息
preview_returns_group_info_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'find_by_id', 2, fun(123, _) ->
                    #{
                        <<"id">> => 123,
                        <<"title">> => <<"Test Group">>,
                        <<"introduction">> => <<"Hello">>,
                        <<"member_count">> => 50,
                        <<"type">> => 1,
                        <<"join_limit">> => 0
                    }
                end}
            ]}
        ],
        fun() ->
            {ok, Preview} = group_discovery_logic:preview(123),
            ?assertEqual(123, maps:get(<<"id">>, Preview)),
            ?assertEqual(<<"Test Group">>, maps:get(<<"title">>, Preview))
        end
    ).

%% 不存在的群组返回错误
preview_nonexistent_group_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'find_by_id', 2, fun(999, _) -> {error, notfound} end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, group_discovery_logic:preview(999))
        end
    ).
