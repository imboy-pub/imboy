%% moya_review_handler_tests
%% 老师侧 handler 契约测试（CM-F3 波次补齐；模式照 moya_task_handler_tests：
%% 直接模块调用，不起 cowboy，全部 meck，零真实网络、零真实库）。
%%
%% 覆盖：
%%   queue —— assignment_id 过滤参数透传给 logic（CM-F3：此前被 handler 丢弃，
%%            logic queue_with_groups 早已支持）；group_id/ai_status 一并透传；
%%            分页默认 {1,20}
%%   workbench / save_draft / publish —— 路径 TSID 解析 + reason→code 冒烟

-module(moya_review_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 981001).
-define(SUBMISSION, 981501).
-define(ASSIGNMENT, 981601).
-define(GROUP, 981701).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

handler_mocks(BindingId, Qs, Body) ->
    [
        {cowboy_req, [
            {'binding', 2, fun(id, _Req) -> BindingId end},
            {'parse_qs', 1, fun(_Req) -> Qs end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> Body end}
        ]},
        {elib_response, [
            {'success_rfc3339', 2, fun(_Req, Payload) ->
                #{resp => success, payload => Payload}
            end},
            {'success_rfc3339', 3, fun(_Req, Payload, _Msg) ->
                #{resp => success, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) ->
                #{resp => error, code => Code}
            end}
        ]}
    ].

queue_logic_mock(Return) ->
    {moya_review_logic, [
        {'queue', 3, fun(Uid, Filters, Page) ->
            self() ! {logic_queue, Uid, Filters, Page},
            Return
        end}
    ]}.

workbench_logic_mock(Return) ->
    {moya_review_logic, [
        {'workbench', 2, fun(Uid, SubId) ->
            self() ! {logic_workbench, Uid, SubId},
            Return
        end}
    ]}.

save_draft_logic_mock(Return) ->
    {moya_review_logic, [
        {'save_draft', 3, fun(Uid, SubId, Body) ->
            self() ! {logic_save_draft, Uid, SubId, Body},
            Return
        end}
    ]}.

queue_payload() ->
    #{
        <<"list">> => [],
        <<"page">> => 1,
        <<"size">> => 20,
        <<"total">> => 0
    }.

%%%===================================================================
%%% queue（CM-F3）：assignment_id 过滤接线
%%%===================================================================

%% moya teacher-api.ts fetchReviewQueue 会带 assignment_id query；logic
%% queue_with_groups 早已解析该键——handler 必须透传，不得丢弃。
queue_assignment_id_filter_passed_test_() ->
    ?WITH_MECKS(
        handler_mocks(
            undefined,
            [
                {<<"assignment_id">>, integer_to_binary(?ASSIGNMENT)},
                {<<"group_id">>, integer_to_binary(?GROUP)},
                {<<"ai_status">>, <<"queued">>}
            ],
            #{}
        ) ++ [queue_logic_mock({ok, queue_payload()})],
        fun() ->
            Req = moya_review_handler:handle_action(
                queue, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            receive
                {logic_queue, ?UID, Filters, {1, 20}} ->
                    ?assertEqual(
                        integer_to_binary(?ASSIGNMENT),
                        maps:get(<<"assignment_id">>, Filters, undefined),
                        "assignment_id filter must be passed to logic"
                    ),
                    ?assertEqual(integer_to_binary(?GROUP), maps:get(<<"group_id">>, Filters)),
                    ?assertEqual(<<"queued">>, maps:get(<<"ai_status">>, Filters))
            after 0 -> ?assert(false, "logic queue not called")
            end
        end
    ).

queue_assignment_id_alone_passed_test_() ->
    ?WITH_MECKS(
        handler_mocks(
            undefined,
            [
                {<<"assignment_id">>, integer_to_binary(?ASSIGNMENT)}, {<<"page">>, <<"2">>}
            ],
            #{}
        ) ++ [queue_logic_mock({ok, queue_payload()})],
        fun() ->
            _ = moya_review_handler:handle_action(
                queue, req0, #{current_uid => ?UID}
            ),
            receive
                {logic_queue, ?UID, Filters, {2, 20}} ->
                    ?assertEqual(
                        integer_to_binary(?ASSIGNMENT),
                        maps:get(<<"assignment_id">>, Filters, undefined)
                    ),
                    ?assertEqual(undefined, maps:get(<<"group_id">>, Filters, undefined))
            after 0 -> ?assert(false, "logic queue not called")
            end
        end
    ).

queue_not_staff_5424_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [], #{}) ++ [queue_logic_mock({error, not_staff})],
        fun() ->
            Req = moya_review_handler:handle_action(
                queue, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_TEACHING_NOT_STAFF, maps:get(code, Req))
        end
    ).

%%%===================================================================
%%% workbench / save_draft：路径解析冒烟
%%%===================================================================

workbench_passes_path_id_test_() ->
    ?WITH_MECKS(
        handler_mocks(integer_to_binary(?SUBMISSION), [], #{}) ++
            [workbench_logic_mock({ok, #{<<"submission">> => #{}}})],
        fun() ->
            Req = moya_review_handler:handle_action(
                workbench, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            receive
                {logic_workbench, ?UID, ?SUBMISSION} -> ok
            after 0 -> ?assert(false, "logic workbench not called with path id")
            end
        end
    ).

save_draft_body_passed_test_() ->
    Body = #{<<"comment">> => <<"结构不错"/utf8>>},
    ?WITH_MECKS(
        handler_mocks(integer_to_binary(?SUBMISSION), [], Body) ++
            [save_draft_logic_mock({ok, #{<<"review_id">> => <<"981801">>}})],
        fun() ->
            Req = moya_review_handler:handle_action(
                save_draft, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            receive
                {logic_save_draft, ?UID, ?SUBMISSION, Body} -> ok
            after 0 -> ?assert(false, "logic save_draft not called with body")
            end
        end
    ).

%%%===================================================================
%%% queue：ai_status 白名单（deny-by-default）
%%%===================================================================

%% 非法 ai_status：422 拒绝且绝不下发 logic（deny-by-default）
queue_ai_status_invalid_422_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [{<<"ai_status">>, <<"paused">>}], #{}) ++
            [queue_logic_mock({ok, queue_payload()})],
        fun() ->
            Req = moya_review_handler:handle_action(
                queue, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_PARAM_INVALID, maps:get(code, Req)),
            receive
                {logic_queue, _, _, _} ->
                    ?assert(false, "logic must not be called on invalid ai_status")
            after 0 -> ok
            end
        end
    ).

%% 白名单五态逐个被接受并原样透传（<<"none">> 也原样 binary 透传，
%% atom 转换是 logic 层的事）
queue_ai_status_whitelist_passed_test_() ->
    [
        ?WITH_MECKS(
            handler_mocks(undefined, [{<<"ai_status">>, AiStatus}], #{}) ++
                [queue_logic_mock({ok, queue_payload()})],
            fun() ->
                Req = moya_review_handler:handle_action(
                    queue, req0, #{current_uid => ?UID}
                ),
                ?assertEqual(success, maps:get(resp, Req)),
                receive
                    {logic_queue, ?UID, Filters, {1, 20}} ->
                        ?assertEqual(
                            AiStatus,
                            maps:get(<<"ai_status">>, Filters),
                            "ai_status must be passed through as binary"
                        )
                after 0 -> ?assert(false, "logic queue not called")
                end
            end
        )
     || AiStatus <-
            [<<"none">>, <<"queued">>, <<"running">>, <<"succeeded">>, <<"failed">>]
    ].
