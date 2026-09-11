%% teaching_roster_handler_tests
%% MN-ROSTER-01 — 只读班级学员名单 handler 契约测试。
%%
%% 直接模块调用（不起 cowboy，模式照 teaching_task_handler_tests）：
%%   list —— group_id（path binding id）TSID string 解析（非法/缺失 → 422）、
%%            JWT uid 从 State 取出透传 logic、成功 envelope（TSID string）、
%%            错误映射：class_not_visible→5430（整数码常量，Wave 2 前本地断言形状）、
%%            cross_org→5426（既有宏）、db_error→teaching_error 通道（code=1）、
%%            未知 reason→teaching_error fallback。
%%   响应深度遍历断言无 guardian_uid/openid/guardian*/relation/birth_year/contact 键
%%   （P0-2：永不返回监护人 UID、openid、联系方式、出生年份、关系详情）。
%%
%% 全部 meck，零真实网络、零真实库。

-module(teaching_roster_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 960001).
-define(GROUP_A1, 963201).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

%% Binding：cowboy_req:binding(id, Req, undefined) 返回值（路由建议
%% {"/api/v1/teaching/classes/:id/learners", teaching_roster_handler, #{action => list}}）
handler_mocks(Binding) ->
    [
        {cowboy_req, [
            {'binding', 3, fun(id, _Req, _Def) -> Binding end}
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

logic_mock(Return) ->
    {teaching_roster_logic, [
        {'list', 2, fun(Uid, GroupId) ->
            self() ! {logic_list, Uid, GroupId},
            Return
        end}
    ]}.

%% 覆盖三分分支的样例 payload（group_id/learner_id 均为 TSID string）
roster_payload() ->
    #{
        <<"group_id">> => integer_to_binary(?GROUP_A1),
        <<"learners">> => [
            #{
                <<"learner_id">> => <<"964001">>,
                <<"display_name">> => <<"L-OK"/utf8>>,
                <<"assignment_ready">> => true,
                <<"setup_reason">> => null
            },
            #{
                <<"learner_id">> => <<"964003">>,
                <<"display_name">> => <<"L-NOG"/utf8>>,
                <<"assignment_ready">> => false,
                <<"setup_reason">> => <<"no_submit_guardian">>
            },
            #{
                <<"learner_id">> => <<"964002">>,
                <<"display_name">> => <<"L-MULTI"/utf8>>,
                <<"assignment_ready">> => false,
                <<"setup_reason">> => <<"multiple_submit_guardians">>
            }
        ]
    }.

%%%===================================================================
%%% 深度遍历守卫：响应 payload 永不携带监护人/身份字段
%%%===================================================================

-define(FORBIDDEN_KEYS, [
    <<"guardian_uid">>,
    <<"guardian_id">>,
    <<"guardian">>,
    <<"guardians">>,
    <<"guardian_learner">>,
    <<"openid">>,
    <<"open_id">>,
    <<"relation">>,
    <<"birth_year">>,
    <<"contact">>,
    <<"phone">>,
    <<"wechat">>
]).

assert_no_guardian_keys(Term) ->
    walk(Term).

walk(Map) when is_map(Map) ->
    lists:foreach(
        fun({K, V}) ->
            ?assertNot(
                lists:member(K, ?FORBIDDEN_KEYS),
                {forbidden_key_in_payload, K}
            ),
            walk(V)
        end,
        maps:to_list(Map)
    );
walk(L) when is_list(L) ->
    lists:foreach(fun walk/1, L);
walk(_) ->
    ok.

%%%===================================================================
%%% list：参数解析 + envelope + 错误映射
%%%===================================================================

list_success_envelope_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"963201">>) ++ [logic_mock({ok, roster_payload()})],
        fun() ->
            Req = teaching_roster_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            Payload = maps:get(payload, Req),
            %% 契约：group_id / learner_id TSID 一律 string
            ?assertEqual(integer_to_binary(?GROUP_A1), maps:get(<<"group_id">>, Payload)),
            Learners = maps:get(<<"learners">>, Payload),
            ?assertEqual(3, length(Learners)),
            lists:foreach(
                fun(L) -> ?assert(is_binary(maps:get(<<"learner_id">>, L))) end,
                Learners
            ),
            %% P0-2：永不返回 guardian UID/openid/联系方式/出生年份/关系详情
            assert_no_guardian_keys(Payload),
            receive
                {logic_list, ?UID, ?GROUP_A1} -> ok
            after 0 -> ?assert(false, "logic list not called with parsed uid/group")
            end
        end
    ).

list_group_id_invalid_rejected_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"not-a-tsid">>) ++ [logic_mock({ok, roster_payload()})],
        fun() ->
            Req = teaching_roster_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            %% 非法 TSID = 参数错误（422），未触达 DB，不泄漏班级存在性
            ?assertEqual(?ERR_PARAM_INVALID, maps:get(code, Req))
        end
    ).

list_group_id_missing_rejected_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined) ++ [logic_mock({ok, roster_payload()})],
        fun() ->
            Req = teaching_roster_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_PARAM_INVALID, maps:get(code, Req))
        end
    ).

list_class_not_visible_5430_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"963201">>) ++ [logic_mock({error, class_not_visible})],
        fun() ->
            Req = teaching_roster_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            %% 新错误码：整数常量（Wave 2 接线前本地形状断言）
            ?assertEqual(?ERR_TEACHING_CLASS_NOT_VISIBLE, maps:get(code, Req))
        end
    ).

list_cross_org_5426_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"963201">>) ++ [logic_mock({error, cross_org})],
        fun() ->
            Req = teaching_roster_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(?ERR_TEACHING_CROSS_ORG, maps:get(code, Req))
        end
    ).

list_db_error_generic_channel_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"963201">>) ++ [logic_mock({error, db_error})],
        fun() ->
            Req = teaching_roster_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            %% db_error 走 teaching_error 既有通道（不暴露错误细节）
            ?assertEqual(?ERR_ERROR, maps:get(code, Req))
        end
    ).

list_unknown_reason_fallback_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"963201">>) ++ [logic_mock({error, some_future_reason})],
        fun() ->
            Req = teaching_roster_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            %% 未知 reason → teaching_error catch-all
            ?assertEqual(?ERR_ERROR, maps:get(code, Req))
        end
    ).

list_empty_roster_envelope_test_() ->
    Empty = #{<<"group_id">> => integer_to_binary(?GROUP_A1), <<"learners">> => []},
    ?WITH_MECKS(
        handler_mocks(<<"963201">>) ++ [logic_mock({ok, Empty})],
        fun() ->
            Req = teaching_roster_handler:handle_action(
                list, req0, #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            ?assertEqual([], maps:get(<<"learners">>, maps:get(payload, Req))),
            assert_no_guardian_keys(maps:get(payload, Req))
        end
    ).
