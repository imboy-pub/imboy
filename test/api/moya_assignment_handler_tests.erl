%% moya_assignment_handler_tests
%% 家长侧 handler 契约测试（CM-F1 波次补齐；模式照 moya_task_handler_tests：
%% 直接模块调用，不起 cowboy，全部 meck，零真实网络、零真实库）。
%%
%% 覆盖：
%%   withdraw —— 成功 payload 含契约必填 submission_id（TSID string）与
%%               status=withdrawn（MN-WITHDRAW-01 冻结契约：moya
%%               parent-api.ts tsidOrThrow(submission_id)，缺字段必抛错）；
%%               错误 reason → envelope code 映射（5481/5444/5423）
%%   history  —— 路径 TSID 解析 + 透传分页
%%   unread   —— A1-D08：since 格式白名单（非法 RFC3339 → 422 拒绝；
%%               合法/缺省透传不变）

-module(moya_assignment_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 978001).
-define(SUBMISSION, 979501).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

%% BindingId：cowboy_req:binding(id, _) 返回值；Qs：parse_qs 返回值
handler_mocks(BindingId, Qs) ->
    [
        {cowboy_req, [
            {'binding', 2, fun(id, _Req) -> BindingId end},
            {'parse_qs', 1, fun(_Req) -> Qs end}
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

withdraw_logic_mock(Return) ->
    {moya_review_logic, [
        {'withdraw', 2, fun(Uid, SubId) ->
            self() ! {logic_withdraw, Uid, SubId},
            Return
        end}
    ]}.

history_logic_mock(Return) ->
    {moya_review_logic, [
        {'history', 3, fun(Uid, LearnerId, Page) ->
            self() ! {logic_history, Uid, LearnerId, Page},
            Return
        end}
    ]}.

unread_logic_mock(Return) ->
    {moya_review_logic, [
        {'history_unread_count', 3, fun(Uid, LearnerId, Since) ->
            self() ! {logic_unread, Uid, LearnerId, Since},
            Return
        end}
    ]}.

%% CM-F4：status 过滤（四态白名单）。/3 为旧口径兜底（未收到 /4 消息即 RED）；
%% /4 为新契约（Uid, LearnerId, Page, Status）。
list_logic_mocks(Return3, Return4) ->
    {moya_assignment_logic, [
        {'list', 3, fun(_Uid, _LearnerId, _Page) -> Return3 end},
        {'list', 4, fun(Uid, LearnerId, Page, Status) ->
            self() ! {logic_list4, Uid, LearnerId, Page, Status},
            Return4
        end}
    ]}.

%%%===================================================================
%%% list（CM-F4）：status 四态过滤参数
%%%===================================================================

list_status_filter_passed_test_() ->
    Qs = [
        {<<"learner_id">>, <<"974001">>},
        {<<"status">>, <<"reviewing">>}
    ],
    ?WITH_MECKS(
        handler_mocks(undefined, Qs) ++
            [
                list_logic_mocks(
                    {ok, #{<<"list">> => [], <<"page">> => 1, <<"size">> => 20, <<"total">> => 0}},
                    {ok, #{<<"list">> => [], <<"page">> => 1, <<"size">> => 20, <<"total">> => 0}}
                )
            ],
        fun() ->
            Req = moya_assignment_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            receive
                {logic_list4, ?UID, 974001, {1, 20}, <<"reviewing">>} -> ok
            after 0 -> ?assert(false, "logic list/4 not called with status filter")
            end
        end
    ).

list_status_all_four_whitelisted_test_() ->
    lists:map(
        fun(Status) ->
            Qs = [{<<"learner_id">>, <<"974001">>}, {<<"status">>, Status}],
            ?WITH_MECKS(
                handler_mocks(undefined, Qs) ++
                    [
                        list_logic_mocks(
                            {ok, empty_page()},
                            {ok, empty_page()}
                        )
                    ],
                fun() ->
                    _ = moya_assignment_handler:handle_action(
                        list, req0, #{current_uid => ?UID}
                    ),
                    receive
                        {logic_list4, _, _, _, Status} -> ok
                    after 0 -> ?assert(false, "whitelisted status rejected")
                    end
                end
            )
        end,
        [<<"pending">>, <<"submitted">>, <<"reviewing">>, <<"reviewed">>]
    ).

%% 非法 status：deny（422 参数错误），不下发查询
list_status_invalid_422_test_() ->
    Qs = [
        {<<"learner_id">>, <<"974001">>},
        {<<"status">>, <<"done">>}
    ],
    ?WITH_MECKS(
        handler_mocks(undefined, Qs) ++
            [list_logic_mocks({ok, empty_page()}, {ok, empty_page()})],
        fun() ->
            Req = moya_assignment_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_PARAM_INVALID, maps:get(code, Req)),
            receive
                {logic_list4, _, _, _, _} -> ?assert(false, "invalid status must not reach logic")
            after 0 -> ok
            end
        end
    ).

list_status_absent_undefined_test_() ->
    Qs = [{<<"learner_id">>, <<"974001">>}],
    ?WITH_MECKS(
        handler_mocks(undefined, Qs) ++
            [
                list_logic_mocks(
                    {ok, empty_page()},
                    {ok, empty_page()}
                )
            ],
        fun() ->
            _ = moya_assignment_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            receive
                {logic_list4, ?UID, 974001, {1, 20}, undefined} -> ok
            after 0 -> ?assert(false, "absent status must pass as undefined")
            end
        end
    ).

empty_page() ->
    #{<<"list">> => [], <<"page">> => 1, <<"size">> => 20, <<"total">> => 0}.

%%%===================================================================
%%% withdraw（CM-F1）：成功 payload 契约
%%%===================================================================

%% 冻结契约：成功 payload 必含 submission_id（TSID string）；moya 端
%% tsidOrThrow 对缺失/非 string 必抛错——「只有真 HTTP 暴露」级契约字段。
withdraw_success_payload_has_submission_id_test_() ->
    ?WITH_MECKS(
        handler_mocks(integer_to_binary(?SUBMISSION), []) ++
            [withdraw_logic_mock({ok, withdrawn})],
        fun() ->
            Req = moya_assignment_handler:handle_action(
                withdraw, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            Payload = maps:get(payload, Req),
            ?assertEqual(integer_to_binary(?SUBMISSION), maps:get(<<"submission_id">>, Payload)),
            ?assertEqual(<<"withdrawn">>, maps:get(<<"status">>, Payload)),
            receive
                {logic_withdraw, ?UID, ?SUBMISSION} -> ok
            after 0 -> ?assert(false, "logic withdraw not called with path id")
            end
        end
    ).

withdraw_already_reviewed_5481_test_() ->
    ?WITH_MECKS(
        handler_mocks(integer_to_binary(?SUBMISSION), []) ++
            [withdraw_logic_mock({error, already_reviewed})],
        fun() ->
            Req = moya_assignment_handler:handle_action(
                withdraw, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_SUBMISSION_REVIEWED, maps:get(code, Req))
        end
    ).

withdraw_already_withdrawn_5444_test_() ->
    ?WITH_MECKS(
        handler_mocks(integer_to_binary(?SUBMISSION), []) ++
            [withdraw_logic_mock({error, already_withdrawn})],
        fun() ->
            Req = moya_assignment_handler:handle_action(
                withdraw, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_SUBMISSION_WITHDRAWN, maps:get(code, Req))
        end
    ).

withdraw_bad_path_id_422_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"not-a-tsid">>, []) ++ [withdraw_logic_mock({ok, withdrawn})],
        fun() ->
            Req = moya_assignment_handler:handle_action(
                withdraw, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_MISSING_PARAM, maps:get(code, Req))
        end
    ).

%%%===================================================================
%%% history：参数解析透传
%%%===================================================================

history_passes_page_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"974001">>, [{<<"page">>, <<"3">>}, {<<"size">>, <<"50">>}]) ++
            [
                history_logic_mock(
                    {ok, #{<<"list">> => [], <<"page">> => 3, <<"size">> => 50, <<"total">> => 0}}
                )
            ],
        fun() ->
            Req = moya_assignment_handler:handle_action(
                history, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            receive
                {logic_history, ?UID, 974001, {3, 50}} -> ok
            after 0 -> ?assert(false, "logic history not called with parsed page")
            end
        end
    ).

%%%===================================================================
%%% history/unread-count（A1-D08）：since 格式白名单
%%%===================================================================

%% 非法 since（无法解析的脏串）：422 拒绝，不下发 logic（deny-by-default；
%% 口径照抄 review-queue 的时间参数 deny 先例——脏串交给 PG ::timestamptz
%% 会被 epgsql rfc3339 codec 退化 epoch → 计数膨胀）
unread_since_invalid_422_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"974001">>, [{<<"since">>, <<"garbage">>}]) ++
            [unread_logic_mock({ok, #{<<"count">> => 0}})],
        fun() ->
            Req = moya_assignment_handler:handle_action(
                history_unread_count, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_PARAM_INVALID, maps:get(code, Req)),
            receive
                {logic_unread, _, _, _} -> ?assert(false, "invalid since must not reach logic")
            after 0 -> ok
            end
        end
    ).

%% 合法 since（服务端产出 published_at 原样回传形态：RFC3339 带时区偏移+小数秒）
unread_since_valid_rfc3339_passthrough_test_() ->
    Since = <<"2026-09-13T09:28:23.467976+08:00">>,
    ?WITH_MECKS(
        handler_mocks(<<"974001">>, [{<<"since">>, Since}]) ++
            [unread_logic_mock({ok, #{<<"count">> => 2}})],
        fun() ->
            Req = moya_assignment_handler:handle_action(
                history_unread_count, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            receive
                {logic_unread, ?UID, 974001, Since} -> ok
            after 0 -> ?assert(false, "valid since not passed through")
            end
        end
    ).

%% 缺省 since（不传）：行为不变——undefined 下发，计全部
unread_since_absent_undefined_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"974001">>, []) ++ [unread_logic_mock({ok, #{<<"count">> => 7}})],
        fun() ->
            _ = moya_assignment_handler:handle_action(
                history_unread_count, req0, #{current_uid => ?UID}
            ),
            receive
                {logic_unread, ?UID, 974001, undefined} -> ok
            after 0 -> ?assert(false, "absent since must pass as undefined")
            end
        end
    ).

%% date-only 形态也在白名单（review-queue ?TIME_PARAM_RE 同口径）
unread_since_date_only_allowed_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"974001">>, [{<<"since">>, <<"2026-09-13">>}]) ++
            [unread_logic_mock({ok, #{<<"count">> => 1}})],
        fun() ->
            _ = moya_assignment_handler:handle_action(
                history_unread_count, req0, #{current_uid => ?UID}
            ),
            receive
                {logic_unread, _, _, <<"2026-09-13">>} -> ok
            after 0 -> ?assert(false, "date-only since should be whitelisted")
            end
        end
    ).
