-module(channel_discovery_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc channel_discovery_logic 的业务逻辑单元测试
%%% 覆盖：搜索、发现、精选、热门、分类
%%%===================================================================

%% ===================================================================
%% 搜索（search）
%% ===================================================================

%% 空关键词搜索返回错误
search_empty_keyword_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch({error, _}, channel_discovery_logic:search(<<>>, 1, 20))
    end).

%% 搜索两步查询：to_tsquery 转换 + 实际搜索
search_returns_paginated_results_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, _Params) ->
                    %% 计数 SQL 含 to_tsquery（WHERE 子句），用 "as keyword" 区分
                    case binary:match(Sql, <<"as keyword">>) of
                        nomatch ->
                            {ok, #{<<"count">> => 5}};
                        _ ->
                            {ok, #{<<"keyword">> => <<"test">>}}
                    end
                end},
                {'query', 2, fun(Sql, _Params) ->
                    case binary:match(Sql, <<"as keyword">>) of
                        nomatch ->
                            {ok, [#{<<"id">> => 1, <<"name">> => <<"Test Channel">>}]};
                        _ ->
                            {ok, [#{<<"keyword">> => <<"test">>}]}
                    end
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = channel_discovery_logic:search(<<"test">>, 1, 20),
            ?assertEqual(5, maps:get(<<"total">>, Result)),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% 搜索支持分类筛选
search_with_category_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, _Params) ->
                    case binary:match(Sql, <<"as keyword">>) of
                        nomatch ->
                            {ok, #{<<"count">> => 3}};
                        _ ->
                            {ok, #{<<"keyword">> => <<"test">>}}
                    end
                end},
                {'query', 2, fun(Sql, _Params) ->
                    case binary:match(Sql, <<"as keyword">>) of
                        nomatch ->
                            {ok, [#{<<"id">> => 1}]};
                        _ ->
                            {ok, [#{<<"keyword">> => <<"test">>}]}
                    end
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = channel_discovery_logic:search(<<"test">>, 1, 20, 1),
            ?assertEqual(3, maps:get(<<"total">>, Result))
        end
    ).

%% ===================================================================
%% 发现（discover）
%% ===================================================================

%% 发现页默认按 popular 排序
discover_defaults_to_popular_sort_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, _Params) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"subscriber_count DESC">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"c.status = 1">>)),
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = channel_discovery_logic:discover(1, 20, undefined, <<"popular">>),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% 发现页支持 newest 排序
discover_supports_newest_sort_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, _Params) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"created_at DESC">>)),
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, _}, channel_discovery_logic:discover(1, 20, undefined, <<"newest">>))
        end
    ).

%% 发现页支持 active 排序
discover_supports_active_sort_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, _Params) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"updated_at DESC">>)),
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, _}, channel_discovery_logic:discover(1, 20, undefined, <<"active">>))
        end
    ).

%% 发现页支持分类筛选
discover_with_category_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"category_id = $1">>)),
                    ?assertEqual(3, length(Params)),
                    {ok, [#{<<"id">> => 2}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = channel_discovery_logic:discover(1, 20, 2, <<"popular">>),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% ===================================================================
%% 精选（featured）
%% ===================================================================

%% 精选只返回 is_featured = true 的频道
featured_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [5]) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"is_featured = true">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"featured_at DESC">>)),
                    {ok, [#{<<"id">> => 1, <<"name">> => <<"Featured Channel">>}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = channel_discovery_logic:featured(5),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% ===================================================================
%% 热门（trending）
%% ===================================================================

%% 热门基于 channel_stats_daily 统计
trending_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [7, 20]) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"channel_stats_daily">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"subscriber_count * 0.4">>)),
                    {ok, [#{<<"id">> => 1, <<"name">> => <<"Trending Channel">>}]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = channel_discovery_logic:trending(7, 20),
            ?assertEqual(1, length(maps:get(<<"list">>, Result)))
        end
    ).

%% 热门支持 30 天周期
trending_supports_30d_period_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [30, 20]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"CURRENT_DATE - $1::integer">>)),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, _}, channel_discovery_logic:trending(30, 20))
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
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"channel_category">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"status = 1">>)),
                    {ok, [
                        #{<<"id">> => 1, <<"name">> => <<"科技"/utf8>>}
                    ]}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = channel_discovery_logic:categories(),
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
            {ok, Result} = channel_discovery_logic:categories(),
            ?assertEqual([], maps:get(<<"list">>, Result))
        end
    ).

%% ===================================================================
%% GZAPP-02/G5 企业频道内部化：公开发现/搜索面全部排除 scope='workspace'
%% （负例：ws 频道不出现在 discover/search/featured/trending——由 SQL
%%  WHERE c.scope <> 'workspace' 承载，断言各查询均带排除子句）
%% ===================================================================

g5_excludes_workspace_scope_from_discover_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, _Params) ->
                    ?assertNotEqual(
                        nomatch,
                        binary:match(Sql, <<"c.scope <> 'workspace'">>),
                        "discover must exclude workspace-scope channels"
                    ),
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, _}, channel_discovery_logic:discover(1, 20, undefined, <<"popular">>)
            ),
            ?assertMatch({ok, _}, channel_discovery_logic:discover(1, 20, 2, <<"newest">>))
        end
    ).

g5_excludes_workspace_scope_from_featured_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [5]) ->
                    ?assertNotEqual(
                        nomatch,
                        binary:match(Sql, <<"c.scope <> 'workspace'">>),
                        "featured must exclude workspace-scope channels"
                    ),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, _}, channel_discovery_logic:featured(5))
        end
    ).

g5_excludes_workspace_scope_from_trending_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [7, 20]) ->
                    ?assertNotEqual(
                        nomatch,
                        binary:match(Sql, <<"c.scope <> 'workspace'">>),
                        "trending must exclude workspace-scope channels"
                    ),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, _}, channel_discovery_logic:trending(7, 20))
        end
    ).

g5_excludes_workspace_scope_from_search_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, _Params) ->
                    case binary:match(Sql, <<"as keyword">>) of
                        nomatch ->
                            ?assertNotEqual(
                                nomatch,
                                binary:match(Sql, <<"c.scope <> 'workspace'">>),
                                "search count must exclude workspace-scope channels"
                            ),
                            {ok, #{<<"count">> => 0}};
                        _ ->
                            {ok, #{<<"keyword">> => <<"test">>}}
                    end
                end},
                {'query', 2, fun(Sql, _Params) ->
                    case binary:match(Sql, <<"as keyword">>) of
                        nomatch ->
                            ?assertNotEqual(
                                nomatch,
                                binary:match(Sql, <<"c.scope <> 'workspace'">>),
                                "search page must exclude workspace-scope channels"
                            ),
                            {ok, []};
                        _ ->
                            {ok, [#{<<"keyword">> => <<"test">>}]}
                    end
                end}
            ]}
        ],
        fun() ->
            %% 全量搜索与分类筛选两条 WHERE 分支都带排除子句
            ?assertMatch({ok, _}, channel_discovery_logic:search(<<"test">>, 1, 20)),
            ?assertMatch({ok, _}, channel_discovery_logic:search(<<"test">>, 1, 20, 1))
        end
    ).
