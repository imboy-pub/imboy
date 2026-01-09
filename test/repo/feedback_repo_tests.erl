-module(feedback_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% feedback_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户反馈数据访问层功能
%%% 覆盖：反馈查询、创建
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(imboy_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.feedback">> end}
    ], fun() ->
        Result = feedback_repo:tablename(),
        ?assertEqual(<<"public.feedback">>, Result)
    end).

%% ===================================================================
%% 反馈查询测试
%% ===================================================================

find_feedback_by_id_test_() ->
    ?WITH_MECK(imboy_pg, [
        {'query', 2, fun(Sql, Params) ->
            % 验证SQL查询包含反馈查询
            ?assert(binary:match(Sql, <<"SELECT.*FROM.*feedback">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"WHERE.*id">>) =/= nomatch),
            % 验证参数包含反馈ID
            ?assert(length(Params) >= 1),
            ?assert(lists:member(<<"feedback123">>, Params)),
            % 返回模拟的反馈数据
            {ok, [{<<"feedback123">>, 1, <<"App feedback">>, <<"bug">>, 1640995200}]}
        end}
    ], fun() ->
        Id = <<"feedback123">>,
        Result = feedback_repo:find(Id),
        ?ASSERT_OK(Result),
        {ok, Feedback} = Result,
        % 验证返回的反馈数据
        ?assertEqual(<<"feedback123">>, element(1, Feedback)),
        ?assertEqual(1, element(2, Feedback)),
        ?assertEqual(<<"App feedback">>, element(3, Feedback)),
        ?assertEqual(<<"bug">>, element(4, Feedback))
    end).

list_feedbacks_by_uid_test_() ->
    ?WITH_MECK(imboy_pg, [
        {'query', 2, fun(Sql, Params) ->
            % 验证SQL查询包含用户反馈列表查询
            ?assert(binary:match(Sql, <<"SELECT.*FROM.*feedback">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"WHERE.*user_id">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"LIMIT">>) =/= nomatch),
            % 验证参数包含用户ID和限制
            ?assert(length(Params) >= 2),
            ?assert(lists:member(1, Params)),
            ?assert(lists:member(10, Params)),
            % 返回模拟的反馈列表
            {ok, [{<<"feedback1">>, 1, <<"Feedback 1">>, <<"bug">>, 1640995200},
                  {<<"feedback2">>, 1, <<"Feedback 2">>, <<"feature">>, 1640995201}]}
        end}
    ], fun() ->
        Uid = 1,
        Limit = 10,
        Result = feedback_repo:list_by_uid(Uid, Limit),
        ?ASSERT_OK(Result),
        {ok, FeedbackList} = Result,
        % 验证返回的反馈列表
        ?assert(length(FeedbackList) >= 2),
        % 验证第一个反馈
        [Feedback1, _Feedback2 | _] = FeedbackList,
        ?assertEqual(<<"feedback1">>, element(1, Feedback1)),
        ?assertEqual(1, element(2, Feedback1)),
        ?assertEqual(<<"bug">>, element(4, Feedback1))
    end).

%% ===================================================================
%% 反馈创建测试
%% ===================================================================

create_feedback_test_() ->
    ?WITH_MECK(imboy_pg, [
        {'execute', 3, fun(Sql, Params) ->
            % 验证SQL包含反馈创建
            ?assert(binary:match(Sql, <<"INSERT.*INTO.*feedback">>) =/= nomatch),
            % 验证参数包含用户ID、内容和类型
            ?assert(length(Params) >= 3),
            ?assert(lists:member(1, Params)),
            ?assert(lists:member(<<"App feedback">>, Params)),
            ?assert(lists:member(<<"bug">>, Params)),
            {ok, 1}
        end}
    ], fun() ->
        Uid = 1,
        Content = <<"App feedback">>,
        Type = <<"bug">>,
        
        Result = feedback_repo:create(Uid, Content, Type),
        ?assertEqual({ok, 1}, Result)
    end).
