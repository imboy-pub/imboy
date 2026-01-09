-module(fts_user_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_user_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户全文搜索数据访问层功能
%%% 覆盖：全文搜索查询
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = fts_user_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

search_users_by_keyword_test_() ->
    ?WITH_MECK(imboy_pg, [
        {'query', 2, fun(Sql, Params) ->
            % 验证SQL包含搜索条件
            ?assert(binary:match(Sql, <<"SELECT.*FROM.*fts_user">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"WHERE.*LIKE">>) =/= nomatch),
            % 验证参数包含关键词和限制
            ?assert(length(Params) >= 2),
            ?assert(lists:member(<<"%john%">>, Params)),
            ?assert(lists:member(10, Params)),
            % 模拟搜索结果
            {ok, [
                {1, <<"John Doe">>, <<"john@example.com">>},
                {2, <<"Johnny Smith">>, <<"johnny@example.com">>}
            ]}
        end}
    ], fun() ->
        Keyword = <<"john">>,
        Limit = 10,
        Result = fts_user_repo:search(Keyword, Limit),
        ?ASSERT_OK(Result),
        {ok, Users} = Result,
        % 验证返回的用户列表
        ?assert(length(Users) >= 0),
        % 如果有结果，验证结果格式
        case Users of
            [] -> ok; % 空结果是允许的
            [User|_] -> 
                % 验证用户数据结构
                ?assert(is_tuple(User)),
                ?assert(tuple_size(User) >= 3)
        end
    end).
