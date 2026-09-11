%% imboy_router_teaching_tasks_method_tests
%% R22-METHOD-01 — /api/v1/teaching/tasks method 分派 shim 的 HTTP method 契约测试。
%%
%% 直接模块调用 imboy_router:init/2（不起 cowboy，模式照
%% adm_app_ddl_handler_tests 的 init_405 用例 + teaching_task_handler_tests
%% 的 mailbox 探针）：
%%   GET    —— 分派 teaching_task_handler:handle_action(list, ...)
%%   POST   —— 分派 teaching_task_handler:handle_action(create, ...)
%%   PUT / PATCH / DELETE —— HTTP 405（"Method Not Allowed"，仓内惯例空头），
%%              不得进入 teaching_task_handler（handle_action 零调用）
%%   全部用例 —— 返回 {ok, Req, State}，State 移除 action 键
%%
%% 契约：/api/v1/teaching/tasks 只允许 GET/POST
%% （moya-teaching.yaml /api/v1/teaching/tasks 仅声明 get+post）。
%% 全部 meck，零真实网络、零真实库；每个用例 teardown meck:unload 全部
%% （?WITH_MECKS -> meck_helper:cleanup_mock，OTP29 meck 楔死教训）。

-module(imboy_router_teaching_tasks_method_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 970001).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

%% cowboy_req：method/1 固定返回 Method；reply/4 捕获三元组并返回标记。
%% teaching_task_handler：handle_action/3 发 mailbox 探针并返回标记，
%% 任何 method 若被转发进 handler 都会在此留下证据。
router_mocks(Method) ->
    [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> Method end},
            {'reply', 4, fun(Status, Headers, Body, _Req) ->
                self() ! {reply_captured, Status, Headers, Body},
                {mocked_reply, Status, Headers, Body}
            end}
        ]},
        {teaching_task_handler, [
            {'handle_action', 3, fun(Action, _Req, _State) ->
                self() ! {handle_action_called, Action},
                {mocked_handle_action, Action}
            end}
        ]}
    ].

tasks_state() ->
    #{action => tasks, current_uid => ?UID}.

%% 收集 handle_action 探针（init 为同进程直调，返回后 mailbox 已就绪）
action_calls() ->
    receive
        {handle_action_called, Action} -> [Action | action_calls()]
    after 0 ->
        []
    end.

%%%===================================================================
%%% GET → handle_action(list)
%%%===================================================================

get_dispatches_list_test_() ->
    ?WITH_MECKS(router_mocks(<<"GET">>), fun() ->
        {ok, Req1, State} = imboy_router:init(req0, tasks_state()),
        ?assertEqual({mocked_handle_action, list}, Req1),
        %% action 键移除、其余 opts 原样透传（与 teaching_task_handler:init/2 一致）
        ?assertEqual(#{current_uid => ?UID}, State),
        ?assertEqual([list], action_calls())
    end).

%%%===================================================================
%%% POST → handle_action(create)
%%%===================================================================

post_dispatches_create_test_() ->
    ?WITH_MECKS(router_mocks(<<"POST">>), fun() ->
        {ok, Req1, State} = imboy_router:init(req0, tasks_state()),
        ?assertEqual({mocked_handle_action, create}, Req1),
        ?assertEqual(#{current_uid => ?UID}, State),
        ?assertEqual([create], action_calls())
    end).

%%%===================================================================
%%% PUT / PATCH / DELETE → reply 405，handle_action 零调用
%%%===================================================================

put_rejected_405_test_() ->
    ?WITH_MECKS(router_mocks(<<"PUT">>), fun() ->
        {ok, Req1, _State} = imboy_router:init(req0, tasks_state()),
        %% 405 形态按仓内惯例（agent_card_handler:53 及 adm_* 30+ 处）：
        %% 空头 + "Method Not Allowed"
        ?assertEqual(
            {mocked_reply, 405, #{}, <<"Method Not Allowed">>}, Req1
        ),
        ?assertEqual([], action_calls(), "PUT must not reach handler")
    end).

patch_rejected_405_test_() ->
    ?WITH_MECKS(router_mocks(<<"PATCH">>), fun() ->
        {ok, Req1, _State} = imboy_router:init(req0, tasks_state()),
        ?assertEqual(
            {mocked_reply, 405, #{}, <<"Method Not Allowed">>}, Req1
        ),
        ?assertEqual([], action_calls(), "PATCH must not reach handler")
    end).

delete_rejected_405_test_() ->
    ?WITH_MECKS(router_mocks(<<"DELETE">>), fun() ->
        {ok, Req1, _State} = imboy_router:init(req0, tasks_state()),
        ?assertEqual(
            {mocked_reply, 405, #{}, <<"Method Not Allowed">>}, Req1
        ),
        ?assertEqual([], action_calls(), "DELETE must not reach handler")
    end).
