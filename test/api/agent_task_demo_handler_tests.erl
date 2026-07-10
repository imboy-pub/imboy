-module(agent_task_demo_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%
% agent_task_demo_handler：PoC 演示端点参数透传回归。
% 重点：agent_uid 恒取自 State 的 current_uid（JWT），group_id 取自请求体；
% 成员经 group_ds:member_uids 解析后交 agent_task_demo:run_demo，handler 只做透传+响应。
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

%% demo：current_uid(JWT) 作 agent_uid，group_id(body) 与解析成员透传给 run_demo
demo_passes_current_uid_and_group_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [{'post', 1, fun(_Req) -> #{<<"group_id">> => 200} end}]},
            {group_ds, [{'member_uids', 1, fun(200) -> [100, 300, 400] end}]},
            {agent_task_demo, [
                {'run_demo', 3, fun(AgentUid, GroupId, MemberUids) ->
                    self() ! {called, AgentUid, GroupId, MemberUids},
                    ok
                end}
            ]},
            {elib_response, resp_msgs()}
        ],
        fun() ->
            Result = agent_task_demo_handler:demo(req_mock(), #{current_uid => 100}),
            ?assertEqual(req_ok, Result),
            %% AgentUid 来自 current_uid=100（非请求体），group_id=200，成员来自 group_ds
            receive
                {called, A, G, M} ->
                    ?assertEqual(100, A),
                    ?assertEqual(200, G),
                    ?assertEqual([100, 300, 400], M)
            after 100 -> ?assert(false)
            end,
            Data = recv_data(),
            ?assertEqual(200, maps:get(<<"group_id">>, Data)),
            ?assertEqual(100, maps:get(<<"agent_uid">>, Data)),
            ?assertEqual(3, maps:get(<<"member_count">>, Data))
        end
    ).

%% 缺 group_id → 参数错误（不解析成员、不调 run_demo）
missing_group_id_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [{'post', 1, fun(_Req) -> #{} end}]},
            {group_ds, [
                {'member_uids', 1, fun(_) ->
                    put(members_called, true),
                    []
                end}
            ]},
            {agent_task_demo, [
                {'run_demo', 3, fun(_, _, _) ->
                    put(demo_called, true),
                    ok
                end}
            ]},
            {elib_response, resp_msgs()}
        ],
        fun() ->
            erase(demo_called),
            erase(members_called),
            ?assertEqual(
                req_error,
                agent_task_demo_handler:demo(req_mock(), #{current_uid => 100})
            ),
            ?assertEqual(undefined, get(demo_called)),
            ?assertEqual(undefined, get(members_called))
        end
    ).
