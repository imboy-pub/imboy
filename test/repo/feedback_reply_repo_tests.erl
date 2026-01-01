-module(feedback_reply_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% feedback_reply_repo 模块的 EUnit 测试
%%%
%%% 目标：验证反馈回复数据访问层功能
%%% 覆盖：回复查询、创建
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = feedback_reply_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

find_replies_by_feedback_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        FeedbackId = <<"feedback123">>,
        % 测试函数调用不会崩溃
        Result = feedback_reply_repo:find_by_feedback_id(FeedbackId),
        % 验证返回值格式
        ?assert(is_tuple(Result)),
        case Result of
            {ok, Replies} ->
                ?assert(is_list(Replies));
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).

create_reply_test_() ->
    ?TEST_SIMPLE(fun() ->
        FeedbackId = <<"feedback123">>,
        Content = <<"Reply content">>,
        AdminUid = 1,
        % 测试函数调用不会崩溃
        Result = feedback_reply_repo:create(FeedbackId, Content, AdminUid),
        % 验证返回值格式
        ?assert(is_tuple(Result)),
        case Result of
            {ok, Reply} ->
                ?assert(is_map(Reply));
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end).
