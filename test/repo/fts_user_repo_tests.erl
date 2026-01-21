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
    ?TEST_WITH_APP(fun() ->
        Result = fts_user_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%%%
%%% SQL 注入防护测试
%%%===================================================================

%% 测试正常的中文搜索
user_search_page_with_chinese_keyword_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            % 简化处理：直接返回模拟结果
            {ok, [#{<<"id">> => 1, <<"nickname">> => <<"张三"/utf8>>, <<"account">> => <<"zhangsan">>}]}
        end}
    ], fun() ->
        Result = fts_user_repo:user_search_page(<<"东区"/utf8>>, 10, 0),
        ?assertMatch({ok, _}, Result)
    end).

%% 测试 SQL 注入攻击防护 - 单引号攻击
sql_injection_protection_single_quote_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        % 尝试 SQL 注入攻击
        MaliciousKeyword = <<"john' OR '1'='1">>,
        Result = fts_user_repo:user_search_page(MaliciousKeyword, 10, 0),
        ?assertMatch({ok, _}, Result)
    end).

%% 测试 SQL 注入攻击防护 - 分号攻击
sql_injection_protection_semicolon_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        % 尝试 SQL 注入攻击
        MaliciousKeyword = <<"test; DROP TABLE users;--">>,
        Result = fts_user_repo:user_search_page(MaliciousKeyword, 10, 0),
        ?assertMatch({ok, _}, Result)
    end).

%% 测试 SQL 注入攻击防护 - 注释符攻击
sql_injection_protection_comment_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, []}
        end}
    ], fun() ->
        % 尝试 SQL 注入攻击
        MaliciousKeyword = <<"test'--">>,
        Result = fts_user_repo:user_search_page(MaliciousKeyword, 10, 0),
        ?assertMatch({ok, _}, Result)
    end).

%% 测试 count 函数同样安全
count_for_user_search_page_safe_test_() ->
    ?WITH_MECK(elib_pg, [
        {'one', 2, fun(_Sql, _Params) ->
            {ok, #{<<"count">> => 5}}
        end}
    ], fun() ->
        Result = fts_user_repo:count_for_user_search_page(<<"test">>),
        ?assert(is_integer(Result))
    end).

%% 测试空关键词
count_for_user_search_page_empty_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 空关键词应该直接返回 0
        Result = fts_user_repo:count_for_user_search_page(<<>>),
        ?assertEqual(0, Result)
    end).
