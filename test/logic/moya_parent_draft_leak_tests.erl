%% MFS3-F09：家长视角 API 泄漏老师未发布草稿（发布门控旁路）。
%%
%% 场景（evidence/journey/probe-draft-leak.txt）：uid 兼任本班 staff +
%% 学员 guardian。submission_access_dispatch staff 优先（MFS-3-B2 契约），
%% 双身份用户即使以 guardian 身份调用 GET /api/v1/moya/submissions/:id
%% （submission_detail，家长域端点），也被派为 staff 视角 → teacher_view
%% 超集 → my_review_draft 草稿全文 + ai_draft 下发到家长端可达面，
%% published_review 同时为 null——「发布」门控被旁路。
%%
%% 契约依据（docs/plans/evidence/moya-calligraphy-ai-review/STEP-04/openapi/
%% moya-teaching.yaml）：
%%   - SubmissionParentView：「硬约束：schema 级排除任何 ai_draft 字段
%%     （D-10）」，且无 my_review_draft 字段（仅 published_review /
%%     ai_status_hint / assets / note）
%%   - AiReviewDraftTeacherView：「该 schema 永不出现在家长视角任何响应中
%%     （AI-02 / D-10）」
%%   - my_review_draft 仅定义于 SubmissionTeacherView 与 ReviewWorkbench
%%     （老师工作台专端点）
%%
%% 修复语义（与 withdrawn 分支 MFS-3-B2 降级先例同款）：submission_detail
%% 的 staff 视角若兼任该学员 active 监护人（can_view_review），降级返回
%% parent_view；纯 staff 保持 teacher_view 超集（老师工作台走
%% review-workbench 专端点，行为不变）。
-module(moya_parent_draft_leak_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 987001).
-define(SUBMISSION, 987101).
-define(LEARNER, 987201).
-define(ORG, 987301).
-define(ASSIGNMENT, 987401).
-define(REVIEW_DRAFT, 987501).
-define(AI_DRAFT, 987601).
-define(SECRET, <<"DRAFT-SECRET-MARKER">>).

scope() ->
    #{
        <<"learner_id">> => ?LEARNER,
        <<"org_id">> => ?ORG
    }.

sub_row() ->
    #{
        <<"id">> => ?SUBMISSION,
        <<"assignment_id">> => ?ASSIGNMENT,
        <<"learner_id">> => ?LEARNER,
        <<"attempt_no">> => 1,
        <<"status">> => <<"submitted">>,
        <<"submitted_at">> => <<"2026-09-18T10:00:00+08:00">>
    }.

%% 老师存而未发布的草稿（含探针明文标记，复刻 probe-draft-leak.mjs）
draft_row() ->
    #{
        <<"id">> => ?REVIEW_DRAFT,
        <<"submission_id">> => ?SUBMISSION,
        <<"reviewer_uid">> => ?UID,
        <<"positive_point">> => <<?SECRET/binary, "-优点-老师还没发布"/utf8>>,
        <<"focus_problem">> => <<?SECRET/binary, "-问题"/utf8>>,
        <<"practice_action">> => <<?SECRET/binary, "-练习"/utf8>>,
        <<"comment">> => <<?SECRET/binary, "-评语"/utf8>>,
        <<"video_attachment_id">> => null,
        <<"rework_required">> => false,
        <<"status">> => <<"draft">>,
        <<"published_at">> => null
    }.

%% succeeded 的 AI 草稿（result 同含标记——顺带证 ai_draft 不得进家长视图）
ai_draft_row() ->
    #{
        <<"id">> => ?AI_DRAFT,
        <<"submission_id">> => ?SUBMISSION,
        <<"status">> => <<"succeeded">>,
        <<"error_code">> => null,
        <<"model_profile">> => <<"test-profile">>,
        <<"prompt_version">> => <<"v1">>,
        <<"rubric_version">> => <<"v1">>,
        <<"result_json">> =>
            <<"{\"summary\":\"DRAFT-SECRET-MARKER-ai\"}">>,
        <<"created_at">> => <<"2026-09-18T10:01:00Z">>,
        <<"completed_at">> => <<"2026-09-18T10:02:00Z">>
    }.

%% 双身份读路径全 mock：ACL staff 优先放行（probe 场景），find_draft 命中
%% 未发布草稿、ai_draft 命中 succeeded 行、published 缺席（未发布）。
%% GuardianView：兼任监护人 {ok,_} / 纯 staff {error,not_guardian}
leak_mocks(GuardianView) ->
    [
        {moya_acl, [
            {'submission_access', 2, fun(?UID, ?SUBMISSION) ->
                {ok, staff, scope()}
            end},
            {'resolve_guardian', 3, fun(?UID, ?LEARNER, view_review) ->
                GuardianView
            end}
        ]},
        {moya_context_repo, [
            {'submission_scope', 1, fun(?SUBMISSION) -> {ok, scope()} end}
        ]},
        {moya_submission_repo, [
            {'find', 1, fun(?SUBMISSION) -> {ok, sub_row()} end},
            {'assets', 1, fun(?SUBMISSION) -> {ok, []} end}
        ]},
        {moya_review_repo, [
            {'ai_draft', 1, fun(?SUBMISSION) -> {ok, ai_draft_row()} end},
            {'find_published', 1, fun(?SUBMISSION) -> {ok, undefined} end},
            {'find_draft', 2, fun(?SUBMISSION, ?UID) -> {ok, draft_row()} end},
            {'assets', 1, fun(_) -> {ok, []} end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _P) -> {ok, []} end}
        ]}
    ].

%% 主断言（RED）：双身份用户（guardian 可达面）查 submission_detail
%% 不得含 teacher_view 超集字段——my_review_draft / ai_draft 键不存在
%% （parent_view 白名单构造，契约 SubmissionParentView），全文无草稿
%% 与 AI 明文标记，published_review=null（发布门控不被旁路）。
dual_role_guardian_view_must_not_leak_draft_test_() ->
    ?WITH_MECKS(
        leak_mocks({ok, #{}}),
        fun() ->
            {ok, Detail} = moya_review_logic:submission_detail(?UID, ?SUBMISSION),
            ?assertNot(maps:is_key(<<"my_review_draft">>, Detail)),
            ?assertNot(maps:is_key(<<"ai_draft">>, Detail)),
            Text = unicode:characters_to_binary(io_lib:format("~tp", [Detail])),
            ?assertEqual(nomatch, binary:match(Text, ?SECRET)),
            ?assertEqual(null, maps:get(<<"published_review">>, Detail))
        end
    ).

%% 对照（不误伤老师）：纯 staff 同请求仍见本人草稿（teacher_view 超集）
staff_only_detail_keeps_own_draft_test_() ->
    ?WITH_MECKS(
        leak_mocks({error, not_guardian}),
        fun() ->
            {ok, Detail} = moya_review_logic:submission_detail(?UID, ?SUBMISSION),
            ?assertMatch(
                #{<<"review_id">> := <<"987501">>},
                maps:get(<<"my_review_draft">>, Detail)
            )
        end
    ).

%% 对照（不误伤老师工作台）：双身份用户走 review-workbench 专端点
%% （staff-only 聚合）仍照常拿顶层 my_review_draft 恢复草稿。
dual_role_workbench_keeps_draft_test_() ->
    ?WITH_MECKS(
        leak_mocks({ok, #{}}),
        fun() ->
            {ok, WB} = moya_review_logic:workbench(?UID, ?SUBMISSION),
            ?assertMatch(
                #{<<"review_id">> := <<"987501">>},
                maps:get(<<"my_review_draft">>, WB)
            )
        end
    ).

%% 对照（guardian 单身份）：纯监护人路径本就 parent_view（回归锚点，
%% 证明泄漏仅源于 staff 优先 dispatch 的双身份分支）。
guardian_only_detail_never_has_draft_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'submission_access', 2, fun(?UID, ?SUBMISSION) ->
                    {ok, guardian, scope()}
                end}
            ]},
            {moya_context_repo, [
                {'submission_scope', 1, fun(?SUBMISSION) -> {ok, scope()} end}
            ]},
            {moya_submission_repo, [
                {'find', 1, fun(?SUBMISSION) -> {ok, sub_row()} end},
                {'assets', 1, fun(?SUBMISSION) -> {ok, []} end}
            ]},
            {moya_review_repo, [
                {'ai_draft', 1, fun(?SUBMISSION) -> {ok, ai_draft_row()} end},
                {'find_published', 1, fun(?SUBMISSION) -> {ok, undefined} end},
                {'find_draft', 2, fun(?SUBMISSION, ?UID) -> {ok, draft_row()} end},
                {'assets', 1, fun(_) -> {ok, []} end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _P) -> {ok, []} end}
            ]}
        ],
        fun() ->
            {ok, Detail} = moya_review_logic:submission_detail(?UID, ?SUBMISSION),
            ?assertNot(maps:is_key(<<"my_review_draft">>, Detail)),
            ?assertNot(maps:is_key(<<"ai_draft">>, Detail))
        end
    ).
