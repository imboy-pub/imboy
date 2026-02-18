-module(group_task_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% group_task_handler 模块的 EUnit 测试
%%%
%%% 目标：验证群作业 API 处理功能
%%% 覆盖：创建、更新、分配、提交、批改、查询
%%%===================================================================

%% ===================================================================
%% create/2 测试 - 创建作业
%% ===================================================================

create_success_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'create', 4, fun(_GroupId, _CreatorId, _Title, _Data) ->
            {ok, 1001}
        end}
    ], fun() ->
        State = #{current_uid => 456},
        Req0 = cowboy_req_h:new(#{
            method => <<"POST">>,
            body => #{<<"group_id">> => <<"123">>, <<"title">> => <<"完成第一章练习"/utf8>>}
        }),
        Result = group_task_handler:create(Req0, State),
        ?assertMatch({ok, _}, Result)
    end).

create_with_missing_title_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'create', 4, fun(_GroupId, _CreatorId, _Title, _Data) ->
            {error, <<"作业标题必填"/utf8>>, ?ERR_TASK_TITLE_REQUIRED}
        end}
    ], fun() ->
        State = #{current_uid => 456},
        Req0 = cowboy_req_h:new(#{
            method => <<"POST">>,
            body => #{<<"group_id">> => <<"123">>, <<"title">> => <<>>}
        }),
        Result = group_task_handler:create(Req0, State),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% update/2 测试 - 更新作业
%% ===================================================================

update_success_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'update', 3, fun(_TaskId, _CreatorId, _Data) ->
            ok
        end}
    ], fun() ->
        State = #{current_uid => 456},
        Req0 = cowboy_req_h:new(#{
            method => <<"POST">>,
            body => #{<<"task_id">> => <<"1001">>, <<"title">> => <<"更新后的标题"/utf8>>}
        }),
        Result = group_task_handler:update(Req0, State),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% assign/2 测试 - 分配作业
%% ===================================================================

assign_success_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'assign', 2, fun(_TaskId, _UserIds) ->
            ok
        end}
    ], fun() ->
        State = #{current_uid => 456},
        Req0 = cowboy_req_h:new(#{
            method => <<"POST">>,
            body => #{<<"task_id">> => <<"1001">>, <<"user_ids">> => [789, 790]}
        }),
        Result = group_task_handler:assign(Req0, State),
        ?assertEqual(ok, Result)
    end).

assign_empty_list_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'assign', 2, fun(_TaskId, _UserIds) ->
            {error, <<"成员列表不能为空"/utf8>>, ?ERR_BAD_REQUEST}
        end}
    ], fun() ->
        State = #{current_uid => 456},
        Req0 = cowboy_req_h:new(#{
            method => <<"POST">>,
            body => #{<<"task_id">> => <<"1001">>, <<"user_ids">> => []}
        }),
        Result = group_task_handler:assign(Req0, State),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% submit/2 测试 - 提交作业
%% ===================================================================

submit_success_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'submit', 3, fun(_TaskId, _UserId, _Data) ->
            ok
        end}
    ], fun() ->
        State = #{current_uid => 789},
        Req0 = cowboy_req_h:new(#{
            method => <<"POST">>,
            body => #{<<"task_id">> => <<"task123">>, <<"content">> => <<"作业内容"/utf8>>}
        }),
        Result = group_task_handler:submit(Req0, State),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% review/2 测试 - 批改作业
%% ===================================================================

review_success_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'review', 4, fun(_AssignmentId, _ReviewerId, _Data) ->
            ok
        end}
    ], fun() ->
        State = #{current_uid => 456},
        Req0 = cowboy_req_h:new(#{
            method => <<"POST">>,
            body => #{<<"assignment_id">> => <<"2001">>, <<"score">> => 95, <<"comment">> => <<"完成得很好"/utf8>>}
        }),
        Result = group_task_handler:review(Req0, State),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% list/2 测试 - 查询作业列表
%% ===================================================================

list_success_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'list', 3, fun(_GroupId, _Page, _Size) ->
            {ok, [{[{<<"id">>, 1001}, {<<"title">>, <<"作业1"/utf8>>}]}]}
        end}
    ], fun() ->
        State = #{current_uid => 456},
        Req0 = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => #{<<"group_id">> => <<"123">>, <<"page">> => <<"1">>, <<"size">> => <<"20">>}
        }),
        Result = group_task_handler:list(Req0, State),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% detail/2 测试 - 查询作业详情
%% ===================================================================

detail_success_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'detail', 1, fun(_TaskId) ->
            {ok, #{<<"id">> => 1001, <<"title">> => <<"作业1"/utf8>>}}
        end}
    ], fun() ->
        State = #{current_uid => 456},
        Req0 = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => #{<<"task_id">> => <<"1001">>}
        }),
        Result = group_task_handler:detail(Req0, State),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% my_tasks/2 测试 - 查询我的作业
%% ===================================================================

my_tasks_success_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'my_tasks', 3, fun(_UserId, _Page, _Size) ->
            {ok, [{[{<<"id">>, 2001}, {<<"task_id">>, <<"task123">>}, {<<"status">>, 2}]}]}
        end}
    ], fun() ->
        State = #{current_uid => 789},
        Req0 = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => #{<<"page">> => <<"1">>, <<"size">> => <<"20">>}
        }),
        Result = group_task_handler:my_tasks(Req0, State),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% pending_review/2 测试 - 查询待批改作业
%% ===================================================================

pending_review_success_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'pending_review', 3, fun(_TaskId, _Page, _Size) ->
            {ok, [{[{<<"id">>, 2001}, {<<"user_id">>, 789}, {<<"status">>, 2}]}]}
        end}
    ], fun() ->
        State = #{current_uid => 456},
        Req0 = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => #{<<"task_id">> => <<"task123">>, <<"page">> => <<"1">>, <<"size">> => <<"20">>}
        }),
        Result = group_task_handler:pending_review(Req0, State),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

create_with_invalid_group_id_test_() ->
    State = #{current_uid => 456},
    Req0 = cowboy_req_h:new(#{
        method => <<"POST">>,
        body => #{<<"group_id">> => <<"invalid">>, <<"title">> => <<"作业标题"/utf8>>}
    }),
    Result = group_task_handler:create(Req0, State),
    ?assertMatch({error, _}, Result).

submit_with_empty_content_test_() ->
    ?WITH_MECK(group_task_logic, [
        {'submit', 3, fun(_TaskId, _UserId, _Data) ->
            {error, <<"提交内容不能为空"/utf8>>, ?ERR_BAD_REQUEST}
        end}
    ], fun() ->
        State = #{current_uid => 789},
        Req0 = cowboy_req_h:new(#{
            method => <<"POST">>,
            body => #{<<"task_id">> => <<"task123">>, <<"content">> => <<>>}
        }),
        Result = group_task_handler:submit(Req0, State),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% 注意
%% ===================================================================
%% 测试使用 meck 模拟 Logic 层
%% 实际使用时请确保 Logic 层正常工作
%%===================================================================
