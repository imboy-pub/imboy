%% moya_subscribe_handler_tests
%% 订阅消息授权上报 handler 契约测试 + moya_review_handler 发布挂钩接线断言。
%%
%% 直接模块调用（不起 cowboy，模式照 moya_learner_bind_handler_tests）：
%%   handler —— report 参数解析（template_ids 非空数组 / 元素非空 binary
%%              ≤64 字节）、logic reason → HTTP status + envelope code 映射
%%              （422/5488、500/1）
%%   hook   —— moya_review_handler:publish 真发布（Already=false）触发
%%              notify_review_published_async；幂等重放（Already=true）不触发；
%%              发布失败不触发
%%
%% 全部 meck，零真实网络、零真实库。

-module(moya_subscribe_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 992001).
-define(SUBMISSION, 992501).

-define(TPL_A, <<"tpl-a-0000000000000000000000000000000000000000000000">>).
-define(TPL_B, <<"tpl-b-0000000000000000000000000000000000000000000000">>).

%%%===================================================================
%%% Handler 契约：mock 基建
%%%===================================================================

%% Body：elib_param:post 的返回值
handler_mocks(Body) ->
    [
        {elib_param, [
            {'post', 1, fun(_Req) -> Body end}
        ]},
        {elib_response, [
            {'success_rfc3339', 3, fun(_Req, Payload, _Msg) ->
                #{resp => success, payload => Payload}
            end},
            {'error_with_status', 4, fun(_Req, Status, _Msg, Code) ->
                #{resp => error, http => Status, code => Code}
            end}
        ]}
    ].

report_logic_mock(Return) ->
    {moya_subscribe_logic, [
        {'report', 2, fun(Uid, TemplateIds) ->
            self() ! {logic_report, Uid, TemplateIds},
            Return
        end}
    ]}.

%%%===================================================================
%%% Handler：report 成功 + 参数解析
%%%===================================================================

report_success_test_() ->
    ?WITH_MECKS(
        handler_mocks(#{<<"template_ids">> => [?TPL_A, ?TPL_B]}) ++
            [report_logic_mock({ok, 2})],
        fun() ->
            Req = moya_subscribe_handler:handle_action(
                report,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            ?assertEqual(2, maps:get(<<"count">>, maps:get(payload, Req))),
            receive
                {logic_report, ?UID, [?TPL_A, ?TPL_B]} -> ok
            after 0 -> ?assert(false, "logic report not called with parsed ids")
            end
        end
    ).

%%%===================================================================
%%% Handler：template_ids 契约（缺失/非数组/元素非法 → 422+5488，
%%% 绝不下发 logic；空数组放行 → logic 收到空表返回 count:0）
%%%===================================================================

%% 空数组放行（e2e 契约：count:0；宽容语义与 logic 白名单过滤一致）
report_empty_list_passes_to_logic_test_() ->
    ?WITH_MECKS(
        handler_mocks(#{<<"template_ids">> => []}) ++
            [report_logic_mock({ok, 0})],
        fun() ->
            Req = moya_subscribe_handler:handle_action(
                report,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            ?assertEqual(0, maps:get(<<"count">>, maps:get(payload, Req))),
            receive
                {logic_report, ?UID, []} -> ok
            after 0 -> ?assert(false, "logic report not called with empty ids")
            end
        end
    ).

report_invalid_body_test_() ->
    TooLong = binary:copy(<<"x">>, 65),
    Cases = [
        {missing_key, #{}},
        {body_not_map, [<<"template_ids">>]},
        {not_a_list, #{<<"template_ids">> => ?TPL_A}},
        {element_not_binary, #{<<"template_ids">> => [?TPL_A, 42]}},
        {element_empty_bin, #{<<"template_ids">> => [<<>>]}},
        {element_too_long, #{<<"template_ids">> => [TooLong]}}
    ],
    [
        begin
            {Label, Body} = Case,
            {
                lists:flatten(io_lib:format("report ~p -> 422/5488", [Label])),
                ?WITH_MECKS(
                    handler_mocks(Body) ++ [report_logic_mock({ok, 0})],
                    fun() ->
                        Req = moya_subscribe_handler:handle_action(
                            report,
                            req0,
                            #{current_uid => ?UID}
                        ),
                        ?assertEqual(error, maps:get(resp, Req)),
                        ?assertEqual(422, maps:get(http, Req)),
                        ?assertEqual(?ERR_SUBSCRIBE_TEMPLATES_INVALID, maps:get(code, Req)),
                        receive
                            {logic_report, _, _} ->
                                ?assert(false, "logic must not be called on invalid body")
                        after 0 -> ok
                        end
                    end
                )
            }
        end
     || Case <- Cases
    ].

%%%===================================================================
%%% Handler：logic 错误映射
%%%===================================================================

report_logic_invalid_templates_test_() ->
    ?WITH_MECKS(
        handler_mocks(#{<<"template_ids">> => [?TPL_A]}) ++
            [report_logic_mock({error, invalid_templates})],
        fun() ->
            Req = moya_subscribe_handler:handle_action(
                report,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(422, maps:get(http, Req)),
            ?assertEqual(?ERR_SUBSCRIBE_TEMPLATES_INVALID, maps:get(code, Req))
        end
    ).

report_logic_db_error_test_() ->
    ?WITH_MECKS(
        handler_mocks(#{<<"template_ids">> => [?TPL_A]}) ++
            [report_logic_mock({error, db_error})],
        fun() ->
            Req = moya_subscribe_handler:handle_action(
                report,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(500, maps:get(http, Req)),
            ?assertEqual(?ERR_ERROR, maps:get(code, Req))
        end
    ).

%%%===================================================================
%%% 发布挂钩：moya_review_handler publish → notify_review_published_async
%%%===================================================================

%% 64 字节边界内最长合法模板 ID 恰可通过（65 字节在上方 TooLong 拒绝）
template_64_bytes_test_() ->
    Tpl64 = binary:copy(<<"x">>, 64),
    ?WITH_MECKS(
        handler_mocks(#{<<"template_ids">> => [Tpl64]}) ++
            [report_logic_mock({ok, 1})],
        fun() ->
            Req = moya_subscribe_handler:handle_action(
                report,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            receive
                {logic_report, ?UID, [Tpl64]} -> ok
            after 0 -> ?assert(false, "64-byte template id must be accepted")
            end
        end
    ).

publish_mocks(PublishReturn) ->
    [
        {cowboy_req, [
            {'binding', 2, fun(id, _Req) -> integer_to_binary(?SUBMISSION) end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"comment">> => <<"好"/utf8>>} end}
        ]},
        {elib_response, [
            {'success_rfc3339', 3, fun(_Req, _Payload, _Msg) ->
                #{resp => success}
            end}
        ]},
        {moya_review_logic, [
            {'publish', 3, fun(Uid, SubId, _Body) ->
                self() ! {logic_publish, Uid, SubId},
                PublishReturn
            end}
        ]},
        {moya_subscribe_logic, [
            {'notify_review_published_async', 1, fun(SubId) ->
                self() ! {notify_async, SubId},
                ok
            end}
        ]}
    ].

review_row() ->
    #{<<"id">> => integer_to_binary(?SUBMISSION), <<"status">> => <<"published">>}.

%% 真发布（Already=false）：构造成功响应之前触发订阅通知
publish_fresh_triggers_notify_test_() ->
    ?WITH_MECKS(publish_mocks({ok, review_row(), false}), fun() ->
        Req = moya_review_handler:handle_action(
            publish,
            req0,
            #{current_uid => ?UID}
        ),
        ?assertEqual(success, maps:get(resp, Req)),
        receive
            {notify_async, ?SUBMISSION} -> ok
        after 0 -> ?assert(false, "notify_review_published_async not called on fresh publish")
        end
    end).

%% 幂等重放（Already=true）：不重复触发订阅通知
publish_replay_skips_notify_test_() ->
    ?WITH_MECKS(publish_mocks({ok, review_row(), true}), fun() ->
        Req = moya_review_handler:handle_action(
            publish,
            req0,
            #{current_uid => ?UID}
        ),
        ?assertEqual(success, maps:get(resp, Req)),
        receive
            {notify_async, _} ->
                ?assert(false, "notify must not be called on idempotent replay")
        after 0 -> ok
        end
    end).

%% 发布失败（error 分支）：不触发订阅通知
publish_error_skips_notify_test_() ->
    Mocks =
        publish_mocks({error, draft_not_found}) ++
            [
                {moya_error, [
                    {'to_response', 2, fun(_Req, _Reason) ->
                        #{resp => error, http => 422}
                    end}
                ]}
            ],
    ?WITH_MECKS(Mocks, fun() ->
        Req = moya_review_handler:handle_action(
            publish,
            req0,
            #{current_uid => ?UID}
        ),
        ?assertEqual(error, maps:get(resp, Req)),
        receive
            {notify_async, _} ->
                ?assert(false, "notify must not be called when publish fails")
        after 0 -> ok
        end
    end).
