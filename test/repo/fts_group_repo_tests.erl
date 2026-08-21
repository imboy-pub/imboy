-module(fts_group_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc fts_group_repo 的 repo 层单元测试（基于 mock，无数据库依赖）
%%% 覆盖：全文搜索、发现页、分类筛选、精选、热门
%%%===================================================================

tablename_returns_fts_group_table_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(<<"fts_group">>, fts_group_repo:tablename())
    end).

%% 空关键词计数返回 0
count_for_empty_keyword_returns_zero_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(0, fts_group_repo:count_for_group_search(<<>>))
    end).

%% 计数查询参数化
count_for_group_search_uses_parameterized_query_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, _Params) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"to_tsquery">>)),
                    {ok, #{<<"keyword">> => <<"hello">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(0, fts_group_repo:count_for_group_search(<<"hello">>))
        end
    ).

%% 全文搜索两步：先 to_tsquery 转换，再实际搜索
group_search_page_uses_fulltext_search_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun
                    %% 第一步：to_tsquery 转换
                    (Sql1, [<<"test">>]) when is_binary(Sql1) ->
                        ?assertNotEqual(nomatch, binary:match(Sql1, <<"to_tsquery">>)),
                        {ok, [#{<<"keyword">> => <<"test">>}]};
                    %% 第二步：实际搜索
                    (Sql2, _Params) ->
                        ?assert(is_binary(Sql2)),
                        ?assertNotEqual(nomatch, binary:match(Sql2, <<"fts_group">>)),
                        ?assertNotEqual(
                            nomatch, binary:match(Sql2, <<"g.status = 1 AND g.type = 1">>)
                        ),
                        ?assertNotEqual(nomatch, binary:match(Sql2, <<"ts_rank_cd">>)),
                        {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, []}, fts_group_repo:group_search_page(<<"test">>, 10, 0))
        end
    ).

%% 搜索支持分类筛选
group_search_page_with_category_filters_by_category_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun
                    %% 第一步
                    (Sql1, [<<"test">>]) ->
                        ?assertNotEqual(nomatch, binary:match(Sql1, <<"to_tsquery">>)),
                        {ok, [#{<<"keyword">> => <<"test">>}]};
                    %% 第二步：带分类筛选
                    (Sql2, Params) ->
                        ?assert(is_binary(Sql2)),
                        ?assertNotEqual(nomatch, binary:match(Sql2, <<"category_id">>)),
                        ?assertEqual(4, length(Params)),
                        {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, []}, fts_group_repo:group_search_page(<<"test">>, 10, 0, 1))
        end
    ).

%% 空关键词发现页走 member_count 排序
discover_page_uses_member_count_order_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, _Params) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"ORDER BY g.member_count DESC">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"g.status = 1 AND g.type = 1">>)),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, []}, fts_group_repo:group_search_page(<<>>, 10, 0))
        end
    ).

%% 发现页支持分类筛选
discover_page_with_category_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    ?assert(is_binary(Sql)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"category_id = $3">>)),
                    ?assertEqual(2, length(Params)),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, []}, fts_group_repo:group_search_page(<<>>, 10, 0, 2))
        end
    ).

%% 查询返回正确的字段结构（两步查询）
group_search_page_returns_correct_fields_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun
                    %% 第一步：to_tsquery 转换
                    (Sql1, [<<"test">>]) ->
                        ?assertNotEqual(nomatch, binary:match(Sql1, <<"to_tsquery">>)),
                        {ok, [#{<<"keyword">> => <<"test">>}]};
                    %% 第二步：实际搜索，验证字段
                    (Sql2, _Params) ->
                        ?assert(is_binary(Sql2)),
                        ?assertNotEqual(nomatch, binary:match(Sql2, <<"g.id, g.title, g.avatar">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql2, <<"g.member_count">>)),
                        {ok, [
                            #{
                                <<"id">> => 1,
                                <<"title">> => <<"测试群"/utf8>>,
                                <<"member_count">> => 100
                            }
                        ]}
                end}
            ]}
        ],
        fun() ->
            {ok, Rows} = fts_group_repo:group_search_page(<<"test">>, 10, 0),
            ?assertEqual(1, length(Rows))
        end
    ).
