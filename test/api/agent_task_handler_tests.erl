-module(agent_task_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%
% agent_task_handler：群内 agent 任务审批端点参数/授权透传回归。
% 重点：ApproverUid 恒取自 State 的 current_uid（JWT），task_id 取自请求体；
% 结果由 agent_task_observer 决定，handler 只做透传+响应映射。
%%%

req_mock() -> req.

resp_msgs() ->
    [
        {'success', 1, fun(_Req) -> req_ok end},
        {'success', 2, fun(_Req, Data) ->
            self() ! {resp_data, Data},
            req_ok
        end},
        {'success', 3, fun(_Req, _P, _M) -> req_ok end},
        {'success', 4, fun(_Req, _P, _M, _O) -> req_ok end},
        {'error', 1, fun(_Req) -> req_error end},
        {'error', 2, fun(_Req, _Msg) -> req_error end},
        {'error', 3, fun(_Req, _Msg, _Code) -> req_error end},
        {'error', 4, fun(_Req, _Msg, _Code, _O) -> req_error end}
    ].

recv_data() ->
    receive
        {resp_data, D} -> D
    after 100 -> timeout
    end.

%% approve：current_uid(JWT) 与 task_id(body) 正确透传给 observer，成功返回 status
approve_passes_current_uid_and_task_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [{'post', 1, fun(_Req) -> #{<<"task_id">> => <<"T1">>} end}]},
            {agent_task_observer, [
                {'approve', 2, fun(TaskId, ApproverUid) ->
                    self() ! {called, TaskId, ApproverUid},
                    {ok, approved}
                end}
            ]},
            {elib_response, resp_msgs()}
        ],
        fun() ->
            Result = agent_task_handler:approve(req_mock(), #{current_uid => 100}),
            ?assertEqual(req_ok, Result),
            %% ApproverUid 来自 current_uid=100（非请求体），task_id=T1
            receive
                {called, T, U} ->
                    ?assertEqual(<<"T1">>, T),
                    ?assertEqual(100, U)
            after 100 -> ?assert(false)
            end,
            Data = recv_data(),
            ?assertEqual(<<"approved">>, maps:get(<<"status">>, Data))
        end
    ).

%% reject → status rejected
reject_returns_rejected_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [{'post', 1, fun(_Req) -> #{<<"taskId">> => <<"T2">>} end}]},
            {agent_task_observer, [{'reject', 2, fun(<<"T2">>, 100) -> {ok, rejected} end}]},
            {elib_response, resp_msgs()}
        ],
        fun() ->
            ?assertEqual(req_ok, agent_task_handler:reject(req_mock(), #{current_uid => 100})),
            Data = recv_data(),
            ?assertEqual(<<"rejected">>, maps:get(<<"status">>, Data))
        end
    ).

%% 缺 task_id → 参数错误（不调 observer）
missing_task_id_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [{'post', 1, fun(_Req) -> #{} end}]},
            {agent_task_observer, [
                {'approve', 2, fun(_, _) ->
                    put(called, true),
                    {ok, approved}
                end}
            ]},
            {elib_response, resp_msgs()}
        ],
        fun() ->
            erase(called),
            ?assertEqual(req_error, agent_task_handler:approve(req_mock(), #{current_uid => 100})),
            ?assertEqual(undefined, get(called))
        end
    ).

%% observer 拒绝（not_authorized/already_decided/not_found）→ error 响应
observer_rejections_map_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [{'post', 1, fun(_Req) -> #{<<"task_id">> => <<"T3">>} end}]},
            {agent_task_observer, [{'approve', 2, fun(_, _) -> {error, not_authorized} end}]},
            {elib_response, resp_msgs()}
        ],
        fun() ->
            ?assertEqual(req_error, agent_task_handler:approve(req_mock(), #{current_uid => 7}))
        end
    ).
