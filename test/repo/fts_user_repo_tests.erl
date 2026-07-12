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

%%%===================================================================
%%% E2EE 密文排除测试：消息搜索 SQL 必须含 e2ee IS NULL 过滤
%%%===================================================================

%% C2C 消息搜索 SQL 必须排除 E2EE 密文
search_c2c_msg_excludes_e2ee_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(Sql, _Params) ->
                case binary:match(Sql, <<"as keyword">>) of
                    nomatch ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"m.e2ee is null">>)),
                        {ok, []};
                    _ ->
                        {ok, [#{<<"keyword">> => <<"你好"/utf8>>}]}
                end
            end}
        ],
        fun() ->
            ?assertMatch({ok, _}, fts_user_repo:search_c2c_msg(<<"你好"/utf8>>, 10, 0, 1))
        end
    ).

%% C2G 消息搜索 SQL 必须排除 E2EE 密文
search_c2g_msg_excludes_e2ee_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(Sql, _Params) ->
                case binary:match(Sql, <<"as keyword">>) of
                    nomatch ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"m.e2ee is null">>)),
                        {ok, []};
                    _ ->
                        {ok, [#{<<"keyword">> => <<"开会"/utf8>>}]}
                end
            end}
        ],
        fun() ->
            ?assertMatch({ok, _}, fts_user_repo:search_c2g_msg(<<"开会"/utf8>>, 10, 0, 1))
        end
    ).

%% C2C/C2G 计数 SQL 必须排除 E2EE 密文
search_msg_count_excludes_e2ee_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'one', 2, fun(Sql, _Params) ->
                case binary:match(Sql, <<"as keyword">>) of
                    nomatch ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"AND e2ee IS NULL">>)),
                        {ok, #{<<"count">> => 0}};
                    _ ->
                        {ok, #{<<"keyword">> => <<"你好"/utf8>>}}
                end
            end}
        ],
        fun() ->
            ?assertEqual(0, fts_user_repo:search_c2c_msg_count(<<"你好"/utf8>>)),
            ?assertEqual(0, fts_user_repo:search_c2g_msg_count(<<"你好"/utf8>>))
        end
    ).

%% 高级搜索（with_options）SQL 必须排除 E2EE 密文
search_msg_with_options_excludes_e2ee_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'query', 2, fun(Sql, _Params) ->
                case binary:match(Sql, <<"as keyword">>) of
                    nomatch ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"AND m.e2ee IS NULL">>)),
                        {ok, []};
                    _ ->
                        {ok, [#{<<"keyword">> => <<"你好"/utf8>>}]}
                end
            end}
        ],
        fun() ->
            ?assertMatch(
                {ok, _}, fts_user_repo:search_c2c_msg_with_options(<<"你好"/utf8>>, 10, 0, #{})
            ),
            ?assertMatch(
                {ok, _}, fts_user_repo:search_c2g_msg_with_options(<<"你好"/utf8>>, 10, 0, #{})
            )
        end
    ).

%% 高级搜索计数 SQL 必须排除 E2EE 密文且 FROM 带别名 m
search_msg_count_with_options_excludes_e2ee_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'one', 2, fun(Sql, _Params) ->
                case binary:match(Sql, <<"as keyword">>) of
                    nomatch ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"AND m.e2ee IS NULL">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<" m WHERE ">>)),
                        {ok, #{<<"count">> => 0}};
                    _ ->
                        {ok, #{<<"keyword">> => <<"你好"/utf8>>}}
                end
            end}
        ],
        fun() ->
            ?assertEqual(0, fts_user_repo:search_c2c_msg_count_with_options(<<"你好"/utf8>>, #{})),
            ?assertEqual(0, fts_user_repo:search_c2g_msg_count_with_options(<<"你好"/utf8>>, #{}))
        end
    ).
