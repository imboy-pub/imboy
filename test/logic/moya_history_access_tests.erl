%% moya_history_access_tests
%% A1-D02：learner_history staff 分支的机构一致性。
%% history_access/2 第三分支注释声称「学员所在班级（同机构）的任课老师」，
%% 但此前代码只做 staff_in_any（enrollment 班级 ∩ class_staff），未校验
%% learner.organization_id == 班级机构——跨机构脏 enrollment（seed/后台写入）
%% 会让 orgB 老师读到学员在 orgA 班级的全部回评历史。
%% 对照先例：roster SQL 的 l.organization_id = $2（moya_roster_repo）、
%% task readiness 的 learner_org==OrgId（moya_task_repo）。
%% 本文件锁定：
%%   staff 命中班 + 班机构 ≠ learner 机构 → not_guardian（5423，fail-closed）
%%   staff 命中班 + 班机构 == learner 机构 → 放行
%%   learner 无机构 / 机构查询失败       → 拒绝（fail-closed）

-module(moya_history_access_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(TEACHER, 989001).
-define(LEARNER, 989101).
-define(GROUP, 989201).
-define(ORG_A, 989301).
-define(ORG_B, 989302).

%%%===================================================================
%%% 夹具
%%%===================================================================

%% elib_pg:query/2 按 SQL 前缀分派：
%%   "SELECT id FROM ..."         → self_bound（本人绑定分支）
%%   "SELECT group_id FROM ..."   → learner_group_ids（staff 分支入口）
pg_mocks() ->
    {elib_pg, [
        {'query', 2, fun
            (<<"SELECT id FROM", _/binary>>, _P) ->
                {ok, []};
            (<<"SELECT group_id FROM", _/binary>>, _P) ->
                {ok, [#{<<"group_id">> => ?GROUP}]};
            (_Sql, _P) ->
                {ok, []}
        end}
    ]}.

history_mocks() ->
    {moya_submission_repo, [
        {'history', 3, fun(?LEARNER, 1, 20) -> {ok, [], 0} end}
    ]}.

%%%===================================================================
%%% A1-D02 主断言：staff 命中班但班级机构 ≠ learner 机构 → 拒绝
%%%===================================================================

staff_cross_org_enrollment_denied_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'resolve_guardian', 3, fun(?TEACHER, ?LEARNER, view_review) ->
                    {error, not_guardian}
                end},
                {'resolve_staff', 2, fun(?TEACHER, ?GROUP) -> {ok, #{}} end}
            ]},
            {moya_context_repo, [
                {'learner_org', 1, fun(?LEARNER) -> {ok, ?ORG_A} end},
                {'group_org', 1, fun(?GROUP) -> {ok, ?ORG_B} end}
            ]},
            history_mocks(),
            pg_mocks()
        ],
        fun() ->
            ?assertEqual(
                {error, not_guardian},
                moya_review_logic:history(?TEACHER, ?LEARNER, {1, 20})
            )
        end
    ).

%% fail-closed：learner 无机构（脏数据 NULL）时无任何 staff 入口
staff_learner_without_org_denied_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'resolve_guardian', 3, fun(?TEACHER, ?LEARNER, view_review) ->
                    {error, not_guardian}
                end},
                {'resolve_staff', 2, fun(?TEACHER, ?GROUP) -> {ok, #{}} end}
            ]},
            {moya_context_repo, [
                {'learner_org', 1, fun(?LEARNER) -> {ok, undefined} end},
                {'group_org', 1, fun(?GROUP) -> {ok, ?ORG_A} end}
            ]},
            pg_mocks()
        ],
        fun() ->
            ?assertEqual(
                {error, not_guardian},
                moya_review_logic:history(?TEACHER, ?LEARNER, {1, 20})
            )
        end
    ).

%% fail-closed：learner 机构解析查询失败 → db_error（不静默放行）
staff_learner_org_query_error_denied_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'resolve_guardian', 3, fun(?TEACHER, ?LEARNER, view_review) ->
                    {error, not_guardian}
                end},
                {'resolve_staff', 2, fun(?TEACHER, ?GROUP) -> {ok, #{}} end}
            ]},
            {moya_context_repo, [
                {'learner_org', 1, fun(?LEARNER) -> {error, db_down} end},
                {'group_org', 1, fun(?GROUP) -> {ok, ?ORG_A} end}
            ]},
            pg_mocks()
        ],
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_review_logic:history(?TEACHER, ?LEARNER, {1, 20})
            )
        end
    ).

%%%===================================================================
%%% 回归：同机构 staff 照常放行；guardian 分支不受影响
%%%===================================================================

staff_same_org_allowed_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'resolve_guardian', 3, fun(?TEACHER, ?LEARNER, view_review) ->
                    {error, not_guardian}
                end},
                {'resolve_staff', 2, fun(?TEACHER, ?GROUP) -> {ok, #{}} end}
            ]},
            {moya_context_repo, [
                {'learner_org', 1, fun(?LEARNER) -> {ok, ?ORG_A} end},
                {'group_org', 1, fun(?GROUP) -> {ok, ?ORG_A} end}
            ]},
            history_mocks(),
            pg_mocks()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                moya_review_logic:history(?TEACHER, ?LEARNER, {1, 20})
            )
        end
    ).

guardian_view_review_allowed_test_() ->
    ?WITH_MECKS(
        [
            {moya_acl, [
                {'resolve_guardian', 3, fun(?TEACHER, ?LEARNER, view_review) ->
                    {ok, #{}}
                end}
            ]},
            history_mocks()
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                moya_review_logic:history(?TEACHER, ?LEARNER, {1, 20})
            )
        end
    ).
