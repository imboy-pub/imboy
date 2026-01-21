-module(feedback_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% feedback_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户反馈数据访问层功能
%%% 覆盖：反馈添加、删除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.feedback">> end}
    ], fun() ->
        Result = feedback_repo:tablename(),
        ?assertEqual(<<"public.feedback">>, Result)
    end).

%% ===================================================================
%% add/11 测试
%% ===================================================================

add_feedback_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 2, fun(_Table, _Data) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        Did = <<"device123">>,
        COS = <<"iOS">>,
        COSV = <<"15.0">>,
        AppVsn = <<"1.0.0">>,
        Type = <<"bug">>,
        Rating = 5,
        ContactDetail = <<"user@example.com">>,
        Body = <<"应用崩溃"/utf8>>,
        Attach = <<"[{\"url\":\"https://example.com/screenshot.png\"}]">>,
        FeedbackMd5 = <<"abc123def456">>,

        Result = feedback_repo:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach, FeedbackMd5),
        ?assertEqual(ok, Result)
    end).

add_feedback_with_different_types_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 2, fun(_Table, _Data) -> {ok, 1} end}
    ], fun() ->
        % 测试功能建议类型
        Uid = 12345,
        Did = <<"device123">>,
        COS = <<"Android">>,
        COSV = <<"11">>,
        AppVsn = <<"1.0.0">>,
        Type = <<"feature">>,
        Rating = 4,
        ContactDetail = <<"user@example.com">>,
        Body = <<"希望添加新功能"/utf8>>,
        Attach = <<"[]">>,
        FeedbackMd5 = <<"xyz789">>,

        Result = feedback_repo:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach, FeedbackMd5),
        ?assertEqual(ok, Result)
    end).

add_feedback_with_empty_attachments_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 2, fun(_Table, _Data) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        Did = <<"device123">>,
        COS = <<"iOS">>,
        COSV = <<"15.0">>,
        AppVsn = <<"1.0.0">>,
        Type = <<"other">>,
        Rating = 3,
        ContactDetail = <<"">>,
        Body = <<"其他问题"/utf8>>,
        Attach = <<"[]">>,
        FeedbackMd5 = <<"empty123">>,

        Result = feedback_repo:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach, FeedbackMd5),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_feedback_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 3, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        FeedbackId = <<"feedback123">>,

        Result = feedback_repo:delete(Uid, FeedbackId),
        ?assertEqual(ok, Result)
    end).

delete_feedback_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 3, fun(_Sql, _Params) -> {error, connection_failed} end}
    ], fun() ->
        Uid = 12345,
        FeedbackId = <<"feedback123">>,

        Result = feedback_repo:delete(Uid, FeedbackId),
        ?assertEqual({error, connection_failed}, Result)
    end).

delete_feedback_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 3, fun(_Sql, _Params) -> {ok, 0} end}
    ], fun() ->
        Uid = 12345,
        FeedbackId = <<"nonexistent">>,

        Result = feedback_repo:delete(Uid, FeedbackId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 集成测试
%% ===================================================================

add_and_delete_feedback_flow_test_() ->
    ?WITH_MECK(elib_pg, [
        {'insert', 2, fun(_Table, _Data) -> {ok, 1} end},
        {'execute', 3, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        % 1. 添加反馈
        Uid = 12345,
        Did = <<"device123">>,
        COS = <<"iOS">>,
        COSV = <<"15.0">>,
        AppVsn = <<"1.0.0">>,
        Type = <<"bug">>,
        Rating = 5,
        ContactDetail = <<"user@example.com">>,
        Body = <<"测试反馈"/utf8>>,
        Attach = <<"[{\"url\":\"https://example.com/screenshot.png\"}]">>,
        FeedbackMd5 = <<"abc123">>,

        ?assertEqual(ok, feedback_repo:add(Uid, Did, COS, COSV, AppVsn, Type, Rating, ContactDetail, Body, Attach, FeedbackMd5)),

        % 2. 删除反馈
        FeedbackId = <<"feedback123">>,
        ?assertEqual(ok, feedback_repo:delete(Uid, FeedbackId))
    end).
