%% 撤回权限必须识别同一用户兼具 staff + guardian 的场景。
-module(moya_withdraw_dual_role_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 986001).
-define(SUBMISSION, 986101).
-define(LEARNER, 986201).
-define(ORG, 986301).

scope() ->
    #{
        <<"learner_id">> => ?LEARNER,
        <<"org_id">> => ?ORG
    }.

dual_role_guardian_can_withdraw_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'submission_access', 2, fun(?UID, ?SUBMISSION) -> {ok, staff, scope()} end},
                {'resolve_guardian', 3, fun(?UID, ?LEARNER, submit) -> {ok, #{}} end}
            ]},
            {moya_context_repo, [
                {'learner_org', 1, fun(?LEARNER) -> {ok, ?ORG} end}
            ]},
            {elib_pg, [
                {'with_tx', 2, fun(Tx, _Opts) -> Tx(test_conn) end}
            ]},
            {moya_submission_repo, [
                {'withdraw_tx', 3, fun(test_conn, ?SUBMISSION, ?UID) -> {ok, withdrawn} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, withdrawn}, moya_review_logic:withdraw(?UID, ?SUBMISSION))
        end
    ).

staff_without_guardian_relation_is_denied_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'submission_access', 2, fun(?UID, ?SUBMISSION) -> {ok, staff, scope()} end},
                {'resolve_guardian', 3, fun(?UID, ?LEARNER, submit) ->
                    {error, not_guardian}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_guardian}, moya_review_logic:withdraw(?UID, ?SUBMISSION))
        end
    ).

cross_org_dual_role_is_denied_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'submission_access', 2, fun(?UID, ?SUBMISSION) -> {ok, staff, scope()} end},
                {'resolve_guardian', 3, fun(?UID, ?LEARNER, submit) -> {ok, #{}} end}
            ]},
            {moya_context_repo, [
                {'learner_org', 1, fun(?LEARNER) -> {ok, ?ORG + 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, not_guardian}, moya_review_logic:withdraw(?UID, ?SUBMISSION))
        end
    ).

%%%===================================================================
%%% A1-D04：withdrawn 提交的日常读路径收口（workbench / submission_detail
%%% 的 staff 视角）。先例：view_url 对 withdrawn 一律拒绝（T17——
%%% moya_attach_logic:authorize 对已绑 submission 但 status/=submitted
%%% 返回 false → attach_logic:view_url {error, forbidden}）；写路径
%%% request_ai_draft/save_draft/publish 也均有 withdrawn 守卫。
%%% 此前读路径只走 submission_access（不查 status），撤回后 staff 打开
%%% 旧 workbench URL 仍 200 拿到 AI 草稿 + 学员元数据。
%%%===================================================================

-define(ASSIGNMENT_D4, 986401).

sub_row(Status) ->
    #{
        <<"id">> => ?SUBMISSION,
        <<"assignment_id">> => ?ASSIGNMENT_D4,
        <<"learner_id">> => ?LEARNER,
        <<"attempt_no">> => 1,
        <<"status">> => Status,
        <<"submitted_at">> => <<"2026-09-17T10:00:00+08:00">>
    }.

%% 读路径依赖全 mock：ACL 放行 + bundle 数据源（find/assets/ai_draft/
%% published/draft）+ learner_name/task_title 兜底查询。
read_mocks(Perspective, Status) ->
    [
        {moya_acl, [
            {'submission_access', 2, fun(?UID, ?SUBMISSION) ->
                {ok, Perspective, scope()}
            end}
        ]},
        {moya_context_repo, [
            {'submission_scope', 1, fun(?SUBMISSION) -> {ok, scope()} end}
        ]},
        {moya_submission_repo, [
            {'find', 1, fun(?SUBMISSION) -> {ok, sub_row(Status)} end},
            {'assets', 1, fun(?SUBMISSION) -> {ok, []} end}
        ]},
        {moya_review_repo, [
            {'ai_draft', 1, fun(?SUBMISSION) -> {ok, undefined} end},
            {'find_published', 1, fun(?SUBMISSION) -> {ok, undefined} end},
            {'find_draft', 2, fun(?SUBMISSION, ?UID) -> {ok, undefined} end},
            {'assets', 1, fun(_) -> {ok, []} end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _P) -> {ok, []} end}
        ]}
    ].

%% 主断言：撤回后 staff 打开 workbench → forbidden（不得再吐 AI 草稿/学员名）
workbench_withdrawn_denied_test_() ->
    ?WITH_MECKS(
        read_mocks(staff, <<"withdrawn">>),
        fun() ->
            ?assertEqual(
                {error, forbidden},
                moya_review_logic:workbench(?UID, ?SUBMISSION)
            )
        end
    ).

%% 主断言：撤回后 staff 查 submission_detail → forbidden（teacher_view 不外泄）
submission_detail_staff_withdrawn_denied_test_() ->
    ?WITH_MECKS(
        read_mocks(staff, <<"withdrawn">>),
        fun() ->
            ?assertEqual(
                {error, forbidden},
                moya_review_logic:submission_detail(?UID, ?SUBMISSION)
            )
        end
    ).

%% 回归：未撤回（submitted）时 workbench 照常聚合
workbench_submitted_still_served_test_() ->
    ?WITH_MECKS(
        read_mocks(staff, <<"submitted">>),
        fun() ->
            ?assertMatch(
                {ok, _},
                moya_review_logic:workbench(?UID, ?SUBMISSION)
            )
        end
    ).

%% 回归：未撤回时 staff detail 照常返回 teacher_view
submission_detail_staff_submitted_still_served_test_() ->
    ?WITH_MECKS(
        read_mocks(staff, <<"submitted">>),
        fun() ->
            ?assertMatch(
                {ok, _},
                moya_review_logic:submission_detail(?UID, ?SUBMISSION)
            )
        end
    ).

%% 回归：guardian 视角保留本人可见语义（parent_view 带 status=withdrawn，
%% 本人不经此路径泄漏 AI 草稿——parent_view 白名单本就无 AI 字段，D-10）
submission_detail_guardian_withdrawn_still_visible_test_() ->
    ?WITH_MECKS(
        read_mocks(guardian, <<"withdrawn">>),
        fun() ->
            ?assertMatch(
                {ok, #{<<"status">> := <<"withdrawn">>}},
                moya_review_logic:submission_detail(?UID, ?SUBMISSION)
            )
        end
    ).

%% Wave3 e2e 回归（MFS-3-B2）：submission_access_dispatch staff 优先——双角色
%% 用户（自己孩子的监护人兼本班老师）读 withdrawn 提交必被派为 staff 视角，
%% D04 守卫不得误杀其「监护人本人可见」语义：应降级返回 parent_view。
submission_detail_dual_role_withdrawn_guardian_fallback_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'submission_access', 2, fun(?UID, ?SUBMISSION) -> {ok, staff, scope()} end},
                {'resolve_guardian', 3, fun(?UID, ?LEARNER, view_review) -> {ok, #{}} end}
            ]},
            {moya_context_repo, [
                {'submission_scope', 1, fun(?SUBMISSION) -> {ok, scope()} end}
            ]},
            {moya_submission_repo, [
                {'find', 1, fun(?SUBMISSION) -> {ok, sub_row(<<"withdrawn">>)} end},
                {'assets', 1, fun(?SUBMISSION) -> {ok, []} end}
            ]},
            {moya_review_repo, [
                {'ai_draft', 1, fun(?SUBMISSION) -> {ok, undefined} end},
                {'find_published', 1, fun(?SUBMISSION) -> {ok, undefined} end},
                {'find_draft', 2, fun(?SUBMISSION, ?UID) -> {ok, undefined} end},
                {'assets', 1, fun(_) -> {ok, []} end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _P) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{<<"status">> := <<"withdrawn">>}},
                moya_review_logic:submission_detail(?UID, ?SUBMISSION)
            )
        end
    ).

%% 对照：staff 视角 + withdrawn + 无监护人关系 → 仍 forbidden（D04 主语义）
submission_detail_staff_only_withdrawn_still_denied_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'submission_access', 2, fun(?UID, ?SUBMISSION) -> {ok, staff, scope()} end},
                {'resolve_guardian', 3, fun(?UID, ?LEARNER, view_review) ->
                    {error, not_guardian}
                end}
            ]},
            {moya_context_repo, [
                {'submission_scope', 1, fun(?SUBMISSION) -> {ok, scope()} end}
            ]},
            {moya_submission_repo, [
                {'find', 1, fun(?SUBMISSION) -> {ok, sub_row(<<"withdrawn">>)} end},
                {'assets', 1, fun(?SUBMISSION) -> {ok, []} end}
            ]},
            {moya_review_repo, [
                {'ai_draft', 1, fun(?SUBMISSION) -> {ok, undefined} end},
                {'find_published', 1, fun(?SUBMISSION) -> {ok, undefined} end},
                {'find_draft', 2, fun(?SUBMISSION, ?UID) -> {ok, undefined} end},
                {'assets', 1, fun(_) -> {ok, []} end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _P) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, forbidden},
                moya_review_logic:submission_detail(?UID, ?SUBMISSION)
            )
        end
    ).
