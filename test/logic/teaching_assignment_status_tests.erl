%% teaching_assignment_status_tests
%% CM-F2/CM-F4（Wave 2）：家长作业状态四态 + 详情富字段契约单元测试。
%%
%% 四态口径（与 moya parent-api.ts AssignmentStatus 对齐）：
%%   pending   = 无提交
%%   submitted = 有最新提交，无 published 回评，无老师草稿
%%   reviewing = 有最新提交，无 published 回评，最新提交已有老师草稿
%%               （「老师批改中」——不含草稿内容本身，仅状态位；D-10 剥除的是
%%               草稿内容/AI 字段，不禁止粗粒度状态）
%%   reviewed  = 存在 published 回评
%% 家长感知不到 AI（home.ts 硬约束）——reviewing 语义锚定老师人工动作。
%%
%% 全部 meck / 纯函数，零真实库。

-module(teaching_assignment_status_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 983001).
-define(LEARNER, 983101).
-define(ASSIGN, 983201).
-define(SUBMISSION, 983301).

%%%===================================================================
%%% assignment_summary：四态 + description
%%%===================================================================

base_row() ->
    #{
        <<"assignment_id">> => ?ASSIGN,
        <<"task_gid">> => 983401,
        <<"learner_id">> => ?LEARNER,
        <<"description">> => <<"每天一页，注意坐姿"/utf8>>,
        <<"deadline">> => <<"2026-09-20T12:00:00Z">>,
        <<"group_id">> => 983501,
        <<"group_title">> => <<"A1-硬笔班"/utf8>>,
        <<"title">> => <<"横竖练习"/utf8>>,
        <<"latest_submission_id">> => null,
        <<"latest_attempt_no">> => null,
        <<"has_published">> => false,
        <<"has_draft">> => false
    }.

summary_pending_test_() ->
    [
        ?_assertEqual(
            <<"pending">>,
            maps:get(<<"status">>, teaching_assignment_logic:assignment_summary(base_row()))
        )
    ].

summary_submitted_test_() ->
    Row = (base_row())#{
        <<"latest_submission_id">> => ?SUBMISSION, <<"latest_attempt_no">> => 1
    },
    [
        ?_assertEqual(
            <<"submitted">>,
            maps:get(<<"status">>, teaching_assignment_logic:assignment_summary(Row))
        )
    ].

%% CM-F4：submitted 与 reviewing 必须可分（此前 review 队列二态折叠丢失）
summary_reviewing_when_draft_test_() ->
    Row = (base_row())#{
        <<"latest_submission_id">> => ?SUBMISSION,
        <<"latest_attempt_no">> => 1,
        <<"has_draft">> => true
    },
    [
        ?_assertEqual(
            <<"reviewing">>,
            maps:get(<<"status">>, teaching_assignment_logic:assignment_summary(Row))
        )
    ].

summary_reviewed_wins_over_draft_test_() ->
    Row = (base_row())#{
        <<"latest_submission_id">> => ?SUBMISSION,
        <<"latest_attempt_no">> => 2,
        <<"has_draft">> => true,
        <<"has_published">> => true
    },
    [
        ?_assertEqual(
            <<"reviewed">>,
            maps:get(<<"status">>, teaching_assignment_logic:assignment_summary(Row))
        )
    ].

%% CM-F2：DTO 带 description（moya AssignmentSummary.description 可选字段）
summary_has_description_test_() ->
    [
        ?_assertEqual(
            <<"每天一页，注意坐姿"/utf8>>,
            maps:get(<<"description">>, teaching_assignment_logic:assignment_summary(base_row()))
        ),
        ?_assertEqual(
            null,
            maps:get(
                <<"description">>,
                teaching_assignment_logic:assignment_summary(
                    maps:remove(<<"description">>, base_row())
                )
            )
        )
    ].

%%%===================================================================
%%% detail：富 payload（title/description/deadline/真实 status）
%%%===================================================================

detail_mocks(Row) ->
    [
        {teaching_context_repo, [
            {'assignment_scope', 1, fun(?ASSIGN) ->
                {ok, #{
                    <<"assignment_id">> => ?ASSIGN,
                    <<"learner_id">> => ?LEARNER,
                    <<"group_id">> => 983501,
                    <<"task_gid">> => 983401
                }}
            end}
        ]},
        {teaching_acl, [
            {'resolve_guardian', 2, fun(?UID, ?LEARNER) -> {ok, #{}} end}
        ]},
        {teaching_submission_repo, [
            {'assignment_detail', 1, fun(?ASSIGN) -> {ok, Row} end}
        ]}
    ].

detail_row() ->
    (base_row())#{
        <<"latest_submission_id">> => ?SUBMISSION,
        <<"latest_attempt_no">> => 1,
        <<"has_draft">> => true
    }.

detail_rich_payload_test_() ->
    ?WITH_MECKS(detail_mocks(detail_row()), fun() ->
        {ok, Detail} = teaching_assignment_logic:detail(?UID, ?ASSIGN),
        ?assertEqual(<<"横竖练习"/utf8>>, maps:get(<<"title">>, Detail)),
        ?assertEqual(<<"每天一页，注意坐姿"/utf8>>, maps:get(<<"description">>, Detail)),
        ?assertEqual(<<"2026-09-20T12:00:00Z">>, maps:get(<<"deadline">>, Detail)),
        %% 真实推导态，不再是硬编码 pending
        ?assertEqual(<<"reviewing">>, maps:get(<<"status">>, Detail)),
        %% 列表/详情同形（moya AssignmentDetail extends AssignmentSummary）
        ?assertEqual(
            maps:get(<<"title">>, teaching_assignment_logic:assignment_summary(detail_row())),
            maps:get(<<"title">>, Detail)
        )
    end).

detail_not_found_test_() ->
    ?WITH_MECKS(
        [
            {teaching_context_repo, [
                {'assignment_scope', 1, fun(_) -> {ok, undefined} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_found}, teaching_assignment_logic:detail(?UID, ?ASSIGN))
        end
    ).
