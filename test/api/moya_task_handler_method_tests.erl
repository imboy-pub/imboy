%% moya_task_handler_method_tests
%% R22-METHOD-01 — /api/v1/moya/tasks 同路径双语义（GET=list / POST=create，
%% 其余 method → 405）的 HTTP method 契约测试。
%%
%% 直接模块调用 moya_task_handler:init/2（不起 cowboy，模式照
%% adm_app_ddl_handler_tests 的 init_405 用例 + mailbox 探针）：
%%   GET    —— resolve_action(tasks, Req) → list → moya_task_logic:list/3
%%   POST   —— resolve_action(tasks, Req) → create → moya_task_logic:create/3
%%   PUT / PATCH / DELETE —— cowboy_req:reply(405, #{}, <<"Method Not Allowed">>)，
%%              不进入 moya_task_logic（零调用可证）
%%   全部用例 —— 返回 {ok, Req, State}，State 移除 action 键
%%
%% 契约：/api/v1/moya/tasks 只允许 GET/POST
%% （moya-teaching.yaml /api/v1/moya/tasks 仅声明 get+post）。
%% 全部 meck，零真实网络、零真实库；每个用例 teardown meck:unload 全部
%% （?WITH_MECKS -> meck_helper:cleanup_mock，OTP29 meck 楔死教训）。

-module(moya_task_handler_method_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 970001).
-define(IDEM_KEY, <<"idem-key-12345678">>).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

%% cowboy_req：method/1 固定返回 Method；parse_qs/1 空表（→ group_id 缺省、
%% page 缺省）；header/3 返回合法幂等键；reply/4 捕获 405 形态并返回标记。
%% moya_task_logic：list/3 与 create/3 发 mailbox 探针（405 用例靠零探针
%% 证明未触达）。
%% elib_response：成功响应只回标记（本用例只验 method 分派，不断言 payload）。
%% elib_param：post/1 固定 #{}（假 req 无 body）；page_qs/2 未 mock，
%% passthrough 走真实现 → 用例钉住真实缺省分页。
handler_mocks(Method) ->
    [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> Method end},
            {'parse_qs', 1, fun(_Req) -> [] end},
            {'header', 3, fun(_Key, _Req, _Default) -> ?IDEM_KEY end},
            {'reply', 4, fun(Status, Headers, Body, _Req) ->
                self() ! {reply_captured, Status, Headers, Body},
                {mocked_reply, Status, Headers, Body}
            end}
        ]},
        {moya_task_logic, [
            {'list', 3, fun(Uid, GroupIdOpt, Page) ->
                self() ! {logic_called, {list, Uid, GroupIdOpt, Page}},
                {ok, #{<<"list">> => []}}
            end},
            {'create', 3, fun(Uid, Key, Body) ->
                self() ! {logic_called, {create, Uid, Key, Body}},
                {ok, #{}}
            end}
        ]},
        {elib_response, [
            {'success_rfc3339', 2, fun(_Req, Payload) -> {mocked_success, Payload} end},
            {'success_rfc3339', 3, fun(_Req, Payload, _Msg) -> {mocked_success, Payload} end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]}
    ].

tasks_state() ->
    #{action => tasks, current_uid => ?UID}.

%% 收集 logic 探针（init 为同进程直调，返回后 mailbox 已就绪）
logic_calls() ->
    receive
        {logic_called, Call} -> [Call | logic_calls()]
    after 0 ->
        []
    end.

%%%===================================================================
%%% GET → moya_task_logic:list/3（page 缺省 {1,10}）
%%%===================================================================

get_dispatches_list_test_() ->
    ?WITH_MECKS(handler_mocks(<<"GET">>), fun() ->
        {ok, Req1, State} = moya_task_handler:init(req0, tasks_state()),
        ?assertEqual({mocked_success, #{<<"list">> => []}}, Req1),
        %% action 键移除、其余 opts 原样透传
        ?assertEqual(#{current_uid => ?UID}, State),
        ?assertEqual([{list, ?UID, undefined, {1, 10}}], logic_calls())
    end).

%%%===================================================================
%%% POST → moya_task_logic:create/3
%%%===================================================================

post_dispatches_create_test_() ->
    ?WITH_MECKS(handler_mocks(<<"POST">>), fun() ->
        {ok, Req1, State} = moya_task_handler:init(req0, tasks_state()),
        ?assertEqual({mocked_success, #{}}, Req1),
        ?assertEqual(#{current_uid => ?UID}, State),
        ?assertEqual([{create, ?UID, ?IDEM_KEY, #{}}], logic_calls())
    end).

%%%===================================================================
%%% PUT / PATCH / DELETE → reply 405，logic 零调用
%%%===================================================================

put_rejected_405_test_() ->
    ?WITH_MECKS(handler_mocks(<<"PUT">>), fun() ->
        {ok, Req1, _State} = moya_task_handler:init(req0, tasks_state()),
        %% 405 形态按仓内惯例（agent_card_handler:53 及 adm_* 30+ 处）：
        %% 空头 + "Method Not Allowed"
        ?assertEqual({mocked_reply, 405, #{}, <<"Method Not Allowed">>}, Req1),
        ?assertEqual([], logic_calls(), "PUT must not reach logic")
    end).

patch_rejected_405_test_() ->
    ?WITH_MECKS(handler_mocks(<<"PATCH">>), fun() ->
        {ok, Req1, _State} = moya_task_handler:init(req0, tasks_state()),
        ?assertEqual({mocked_reply, 405, #{}, <<"Method Not Allowed">>}, Req1),
        ?assertEqual([], logic_calls(), "PATCH must not reach logic")
    end).

delete_rejected_405_test_() ->
    ?WITH_MECKS(handler_mocks(<<"DELETE">>), fun() ->
        {ok, Req1, _State} = moya_task_handler:init(req0, tasks_state()),
        ?assertEqual({mocked_reply, 405, #{}, <<"Method Not Allowed">>}, Req1),
        ?assertEqual([], logic_calls(), "DELETE must not reach logic")
    end).
