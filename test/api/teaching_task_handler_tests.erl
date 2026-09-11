%% teaching_task_handler_tests
%% MN-TASK-01（创建+列表）— 教师教学作业 handler 契约测试。
%%
%% 直接模块调用（不起 cowboy，模式照 teaching_learner_bind_handler_tests）：
%%   list   —— group_id 可选解析（string→integer）、默认分页 {1,10}、
%%              logic reason → envelope code 映射（5430/5424/422）
%%   create —— Idempotency-Key 缺失/过短/过长 → 5461；body 透传；
%%              成功 envelope（TSID string、replayed:false）；
%%              新错误码映射 5430/5431/5432（整数码常量，Wave 2 前本地断言形状）
%%
%% 全部 meck，零真实网络、零真实库。

-module(teaching_task_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 970001).
-define(GROUP_A1, 973201).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

%% Qs：parse_qs 返回值；Header：idempotency-key 返回值；Body：elib_param:post 返回值
handler_mocks(Qs, IdemKey, Body) ->
    [
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> Qs end},
            {'header', 3, fun(<<"idempotency-key">>, _Req, _Def) -> IdemKey end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> Body end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) ->
                #{resp => success, payload => Payload}
            end},
            {'success', 3, fun(_Req, Payload, _Msg) ->
                #{resp => success, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) ->
                #{resp => error, code => Code}
            end}
        ]}
    ].

list_logic_mock(Return) ->
    {teaching_task_logic, [
        {'list', 3, fun(Uid, GroupIdOpt, Page) ->
            self() ! {logic_list, Uid, GroupIdOpt, Page},
            Return
        end}
    ]}.

create_logic_mock(Return) ->
    {teaching_task_logic, [
        {'create', 3, fun(Uid, IdemKey, Body) ->
            self() ! {logic_create, Uid, IdemKey, Body},
            Return
        end}
    ]}.

list_payload() ->
    #{
        <<"list">> => [
            #{
                <<"task_id">> => <<"task97abc">>,
                <<"group_id">> => integer_to_binary(?GROUP_A1),
                <<"group_name">> => <<"A1-硬笔班"/utf8>>,
                <<"title">> => <<"横竖练习"/utf8>>,
                <<"description">> => <<>>,
                <<"deadline">> => null,
                <<"learner_count">> => 2,
                <<"submitted_count">> => 0,
                <<"pending_review_count">> => 0,
                <<"created_at">> => <<"2026-09-10T10:00:00Z">>
            }
        ],
        <<"page">> => 1,
        <<"size">> => 10,
        <<"total">> => 1
    }.

create_payload() ->
    #{
        <<"task_id">> => <<"task97abc">>,
        <<"assignments">> => [
            #{
                <<"assignment_id">> => <<"976001">>,
                <<"learner_id">> => <<"974001">>
            }
        ],
        <<"replayed">> => false
    }.

%%%===================================================================
%%% list：参数解析 + envelope + 错误映射
%%%===================================================================

list_success_envelope_test_() ->
    ?WITH_MECKS(
        handler_mocks([], <<>>, #{}) ++ [list_logic_mock({ok, list_payload()})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            Payload = maps:get(payload, Req),
            %% 契约：group_id TSID 一律 string；分页字段整数
            [Task] = maps:get(<<"list">>, Payload),
            ?assertEqual(integer_to_binary(?GROUP_A1), maps:get(<<"group_id">>, Task)),
            ?assertEqual(10, maps:get(<<"size">>, Payload)),
            ?assertEqual(1, maps:get(<<"page">>, Payload)),
            receive
                {logic_list, ?UID, undefined, {1, 10}} -> ok
            after 0 -> ?assert(false, "logic list not called with defaults")
            end
        end
    ).

list_group_id_parsed_test_() ->
    ?WITH_MECKS(
        handler_mocks(
            [{<<"group_id">>, <<"973201">>}, {<<"page">>, <<"2">>}], <<>>, #{}
        ) ++
            [list_logic_mock({ok, list_payload()})],
        fun() ->
            _ = teaching_task_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            receive
                {logic_list, ?UID, ?GROUP_A1, {2, 10}} -> ok
            after 0 -> ?assert(false, "group_id/page not parsed into logic args")
            end
        end
    ).

list_group_id_invalid_rejected_test_() ->
    ?WITH_MECKS(
        handler_mocks([{<<"group_id">>, <<"not-a-tsid">>}], <<>>, #{}) ++
            [list_logic_mock({ok, list_payload()})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_PARAM_INVALID, maps:get(code, Req))
        end
    ).

list_class_not_visible_5430_test_() ->
    ?WITH_MECKS(
        handler_mocks([], <<>>, #{}) ++
            [list_logic_mock({error, class_not_visible})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            %% 新错误码：整数常量（Wave 2 接线前本地形状断言）
            ?assertEqual(?ERR_TEACHING_CLASS_NOT_VISIBLE, maps:get(code, Req))
        end
    ).

list_not_staff_5424_test_() ->
    ?WITH_MECKS(
        handler_mocks([], <<>>, #{}) ++ [list_logic_mock({error, not_staff})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_TEACHING_NOT_STAFF, maps:get(code, Req))
        end
    ).

%%%===================================================================
%%% create：Idempotency-Key 守卫 + envelope + 错误映射
%%%===================================================================

create_success_envelope_test_() ->
    ?WITH_MECKS(
        handler_mocks([], <<"idem-key-970001">>, #{
            <<"group_id">> => <<"973201">>,
            <<"title">> => <<"横竖练习"/utf8>>,
            <<"learner_ids">> => [<<"974001">>]
        }) ++ [create_logic_mock({ok, create_payload()})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                create, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            Payload = maps:get(payload, Req),
            %% 契约：TSID 一律 string；replayed 为 boolean
            ?assertEqual(false, maps:get(<<"replayed">>, Payload)),
            [A] = maps:get(<<"assignments">>, Payload),
            ?assert(is_binary(maps:get(<<"assignment_id">>, A))),
            ?assert(is_binary(maps:get(<<"learner_id">>, A))),
            receive
                {logic_create, ?UID, <<"idem-key-970001">>, _Body} -> ok
            after 0 -> ?assert(false, "logic create not called with idem key")
            end
        end
    ).

create_idem_key_missing_5461_test_() ->
    ?WITH_MECKS(
        handler_mocks([], <<>>, #{
            <<"group_id">> => <<"973201">>,
            <<"title">> => <<"横竖练习"/utf8>>,
            <<"learner_ids">> => [<<"974001">>]
        }) ++ [create_logic_mock({ok, create_payload()})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                create, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_IDEMPOTENCY_KEY_REQUIRED, maps:get(code, Req))
        end
    ).

create_idem_key_too_long_5461_test_() ->
    LongKey = binary:copy(<<"a">>, 129),
    ?WITH_MECKS(
        handler_mocks([], LongKey, #{
            <<"group_id">> => <<"973201">>,
            <<"title">> => <<"横竖练习"/utf8>>,
            <<"learner_ids">> => [<<"974001">>]
        }) ++ [create_logic_mock({ok, create_payload()})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                create, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_IDEMPOTENCY_KEY_REQUIRED, maps:get(code, Req))
        end
    ).

create_idempotency_conflict_5460_test_() ->
    ?WITH_MECKS(
        handler_mocks([], <<"idem-key-970001">>, #{
            <<"group_id">> => <<"973201">>,
            <<"title">> => <<"横竖练习"/utf8>>,
            <<"learner_ids">> => [<<"974001">>]
        }) ++ [create_logic_mock({error, idempotency_conflict})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                create, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_IDEMPOTENCY_CONFLICT, maps:get(code, Req))
        end
    ).

create_learner_not_in_class_5431_test_() ->
    ?WITH_MECKS(
        handler_mocks([], <<"idem-key-970001">>, #{
            <<"group_id">> => <<"973201">>,
            <<"title">> => <<"横竖练习"/utf8>>,
            <<"learner_ids">> => [<<"974001">>]
        }) ++ [create_logic_mock({error, learner_not_in_class})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                create, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_TEACHING_LEARNER_NOT_IN_CLASS, maps:get(code, Req))
        end
    ).

create_guardian_setup_required_5432_test_() ->
    ?WITH_MECKS(
        handler_mocks([], <<"idem-key-970001">>, #{
            <<"group_id">> => <<"973201">>,
            <<"title">> => <<"横竖练习"/utf8>>,
            <<"learner_ids">> => [<<"974001">>]
        }) ++ [create_logic_mock({error, guardian_setup_required})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                create, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_TEACHING_GUARDIAN_SETUP_REQUIRED, maps:get(code, Req))
        end
    ).

create_deadline_passed_5442_test_() ->
    ?WITH_MECKS(
        handler_mocks([], <<"idem-key-970001">>, #{
            <<"group_id">> => <<"973201">>,
            <<"title">> => <<"横竖练习"/utf8>>,
            <<"learner_ids">> => [<<"974001">>]
        }) ++ [create_logic_mock({error, assignment_closed})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                create, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_ASSIGNMENT_CLOSED, maps:get(code, Req))
        end
    ).

create_replayed_true_envelope_test_() ->
    Base = create_payload(),
    Replay = Base#{<<"replayed">> => true},
    ?WITH_MECKS(
        handler_mocks([], <<"idem-key-970001">>, #{
            <<"group_id">> => <<"973201">>,
            <<"title">> => <<"横竖练习"/utf8>>,
            <<"learner_ids">> => [<<"974001">>]
        }) ++ [create_logic_mock({ok, Replay})],
        fun() ->
            Req = teaching_task_handler:handle_action(
                create, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            ?assertEqual(true, maps:get(<<"replayed">>, maps:get(payload, Req)))
        end
    ).
