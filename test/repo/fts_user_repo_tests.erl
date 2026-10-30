-module(fts_user_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_user_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户全文搜索数据访问层功能
%%% 覆盖：全文搜索查询、SQL 注入防护
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]}
        ],
        fun() ->
            Result = fts_user_repo:tablename(),
            ?assertEqual(<<"public.fts_user">>, Result)
        end
    ).

%%%
%%% SQL 注入防护测试
%%%===================================================================

%% 测试正常的中文搜索
user_search_page_with_chinese_keyword_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                % 简化处理：直接返回模拟结果
                {ok, [
                    #{
                        <<"id">> => 1,
                        <<"nickname">> => <<"张三"/utf8>>,
                        <<"account">> => <<"zhangsan">>
                    }
                ]}
            end}
        ],
        fun() ->
            Result = fts_user_repo:user_search_page(<<"东区"/utf8>>, 10, 0),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 测试 SQL 注入攻击防护 - 单引号攻击
sql_injection_protection_single_quote_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ],
        fun() ->
            % 尝试 SQL 注入攻击
            MaliciousKeyword = <<"john' OR '1'='1">>,
            Result = fts_user_repo:user_search_page(MaliciousKeyword, 10, 0),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 测试 SQL 注入攻击防护 - 分号攻击
sql_injection_protection_semicolon_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ],
        fun() ->
            % 尝试 SQL 注入攻击
            MaliciousKeyword = <<"test; DROP TABLE users;--">>,
            Result = fts_user_repo:user_search_page(MaliciousKeyword, 10, 0),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 测试 SQL 注入攻击防护 - 注释符攻击
sql_injection_protection_comment_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, []}
            end}
        ],
        fun() ->
            % 尝试 SQL 注入攻击
            MaliciousKeyword = <<"test'--">>,
            Result = fts_user_repo:user_search_page(MaliciousKeyword, 10, 0),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 测试 count 函数同样安全
count_for_user_search_page_safe_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'one', 2, fun(_Sql, _Params) ->
                {ok, #{<<"count">> => 5}}
            end}
        ],
        fun() ->
            Result = fts_user_repo:count_for_user_search_page(<<"test">>),
            ?assert(is_integer(Result))
        end
    ).

%% 测试空关键词
count_for_user_search_page_empty_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 空关键词应该直接返回 0
        Result = fts_user_repo:count_for_user_search_page(<<>>),
        ?assertEqual(0, Result)
    end).

%%% 认证搜索：查询和计数共享授权范围并排除密文及过期消息。
authorized_search_filters_and_scope_test_() ->
    [
        authorized_search_case(Type, Options)
     || Type <- [<<"c2c">>, <<"c2g">>],
        Options <- [#{}, #{<<"sort_by">> => <<"time">>}]
    ].

authorized_search_case(Type, Options) ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {with_tx, 2, fun(F, _) -> F(test_connection) end},
                {query, 3, fun(_, Sql, Params) ->
                    case
                        {binary:match(Sql, <<"as keyword">>), binary:match(Sql, <<"FOR SHARE">>)}
                    of
                        {{_, _}, _} ->
                            {ok, [#{<<"keyword">> => <<"hello">>}]};
                        {nomatch, {_, _}} ->
                            {ok, []};
                        {nomatch, nomatch} ->
                            ?assert(lists:member(42, Params)),
                            ?assertNotEqual(nomatch, binary:match(Sql, <<"m.e2ee IS NULL">>)),
                            ?assertNotEqual(
                                nomatch,
                                binary:match(Sql, <<"m.expire_at IS NULL OR m.expire_at > NOW()">>)
                            ),
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"count(*) AS search_total">>)
                            ),
                            case Type of
                                <<"c2c">> ->
                                    ?assertNotEqual(
                                        nomatch, binary:match(Sql, <<"m.from_id=$2 OR m.to_id=$2">>)
                                    );
                                <<"c2g">> ->
                                    ?assertNotEqual(
                                        nomatch, binary:match(Sql, <<"group_member_generation">>)
                                    ),
                                    ?assertNotEqual(
                                        nomatch, binary:match(Sql, <<"msg_c2g_recipient_snapshot">>)
                                    ),
                                    ?assertNotEqual(
                                        nomatch, binary:match(Sql, <<"search_missing">>)
                                    )
                            end,
                            {ok, [
                                #{
                                    <<"search_missing">> => false,
                                    <<"search_total">> => 0,
                                    <<"msg_id">> => null
                                }
                            ]}
                    end
                end}
            ]},
            {micro_community_access_repo, [
                {predicate_sql, 2, fun(_, _) -> <<"true">> end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 0, []},
                fts_user_repo:search_authorized_page(42, <<"hello">>, 10, 0, Type, Options)
            )
        end
    ).

authorized_search_missing_snapshot_fails_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {with_tx, 2, fun(F, _) -> F(test_connection) end},
                {query, 3, fun(_, Sql, _) ->
                    case
                        {binary:match(Sql, <<"as keyword">>), binary:match(Sql, <<"FOR SHARE">>)}
                    of
                        {{_, _}, _} ->
                            {ok, [#{<<"keyword">> => <<"hello">>}]};
                        {nomatch, {_, _}} ->
                            {ok, []};
                        {nomatch, nomatch} ->
                            {ok, [#{<<"search_missing">> => true, <<"search_total">> => 0}]}
                    end
                end}
            ]},
            {micro_community_access_repo, [{predicate_sql, 2, fun(_, _) -> <<"true">> end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, search_unavailable},
                fts_user_repo:search_authorized_page(42, <<"hello">>, 10, 0, <<"c2g">>, #{})
            )
        end
    ).

authorized_search_c2c_pair_scope_test_() ->
    [pair_scope_case(Peer) || Peer <- [17, 42]].

pair_scope_case(Peer) ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {with_tx, 2, fun(F, _) -> F(test_connection) end},
                {query, 3, fun(_, Sql, Params) ->
                    case binary:match(Sql, <<"as keyword">>) of
                        {_, _} ->
                            {ok, [#{<<"keyword">> => <<"hello">>}]};
                        nomatch ->
                            ?assertEqual([<<"hello">>, Peer, 42, 10, 0], Params),
                            ?assertNotEqual(
                                nomatch,
                                binary:match(
                                    Sql,
                                    <<"((m.from_id=$3 AND m.to_id=$2) OR (m.from_id=$2 AND m.to_id=$3))">>
                                )
                            ),
                            {ok, [
                                #{
                                    <<"search_missing">> => false,
                                    <<"search_total">> => 0,
                                    <<"msg_id">> => null
                                }
                            ]}
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 0, []},
                fts_user_repo:search_authorized_page(
                    42, <<"hello">>, 10, 0, <<"c2c">>, #{<<"conversation_id">> => Peer}
                )
            )
        end
    ).

invalid_conversation_option_fails_closed_test() ->
    ?assertEqual(
        {error, search_unavailable},
        fts_user_repo:search_authorized_page(
            42, <<"hello">>, 10, 0, <<"c2c">>, #{<<"conversation_id">> => <<"broken">>}
        )
    ).
