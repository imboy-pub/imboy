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

%%%===================================================================
%%% A1-D13：load_submission_bundle 对 assets / ai_draft / find_published
%%% 读错误的强匹配崩溃（{ok, _} = ...）→ 折叠 {error, db_error}。
%%% 同函数内 find/scope 已有优雅分支（case → db_error），唯独 bundle 组装的
%%% 三处强匹配在 DB 故障（连接抖动等）时函数子句崩溃 → HTTP 500。
%%%===================================================================

%% FailAt ∈ submission_assets | ai_draft | find_published：bundle 组装中
%% 指定哪一步返回 {error, conn_lost}（其余步骤正常返回）
bundle_fail_mocks(FailAt) ->
    [
        {moya_acl, [
            {'submission_access', 2, fun(?UID, ?SUBMISSION) ->
                {ok, staff, scope()}
            end}
        ]},
        {moya_context_repo, [
            {'submission_scope', 1, fun(?SUBMISSION) -> {ok, scope()} end}
        ]},
        {moya_submission_repo, [
            {'find', 1, fun(?SUBMISSION) -> {ok, sub_row(<<"submitted">>)} end},
            {'assets', 1, fun(?SUBMISSION) ->
                fail_at(submission_assets, FailAt, {ok, []})
            end}
        ]},
        {moya_review_repo, [
            {'ai_draft', 1, fun(?SUBMISSION) ->
                fail_at(ai_draft, FailAt, {ok, undefined})
            end},
            {'find_published', 1, fun(?SUBMISSION) ->
                fail_at(find_published, FailAt, {ok, undefined})
            end},
            {'find_draft', 2, fun(?SUBMISSION, ?UID) -> {ok, undefined} end},
            {'assets', 1, fun(_) -> {ok, []} end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _P) -> {ok, []} end}
        ]}
    ].

fail_at(Step, Step, _Ok) ->
    {error, conn_lost};
fail_at(_Step, _FailAt, Ok) ->
    Ok.

%% assets 读失败：submission_detail 与 workbench 均须优雅 db_error，不得崩溃
d13_assets_db_error_no_crash_test_() ->
    ?WITH_MECKS(
        bundle_fail_mocks(submission_assets),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_review_logic:submission_detail(?UID, ?SUBMISSION)
            ),
            ?assertEqual(
                {error, db_error},
                moya_review_logic:workbench(?UID, ?SUBMISSION)
            )
        end
    ).

%% ai_draft 读失败：同上折叠 db_error
d13_ai_draft_db_error_no_crash_test_() ->
    ?WITH_MECKS(
        bundle_fail_mocks(ai_draft),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_review_logic:submission_detail(?UID, ?SUBMISSION)
            ),
            ?assertEqual(
                {error, db_error},
                moya_review_logic:workbench(?UID, ?SUBMISSION)
            )
        end
    ).

%% find_published 读失败：同上折叠 db_error
d13_find_published_db_error_no_crash_test_() ->
    ?WITH_MECKS(
        bundle_fail_mocks(find_published),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_review_logic:submission_detail(?UID, ?SUBMISSION)
            ),
            ?assertEqual(
                {error, db_error},
                moya_review_logic:workbench(?UID, ?SUBMISSION)
            )
        end
    ).

%%%===================================================================
%%% A1-D11：B 发布后 A 的存量草稿成「僵尸」——save_draft 恒 5486（B 已占
%%% submission 级唯一 published），无 discard 端点，A 永远无法清理或发布。
%%% 读侧最小修复（方案 b，零状态迁移）：submission 已 published 且发布者
%%% 非本人时，teacher_view 的 my_review_draft 不下发（workbench 顶层与
%%% submission 内嵌同源）。发布者本人的草稿已被 publish_tx 消费（status
%%% 翻转、find_draft 只认 draft 行），此过滤对其零行为变化。
%%%===================================================================

-define(TEACHER_B, 986601).
-define(REVIEW_DRAFT_A, 986701).
-define(REVIEW_PUB_B, 986702).

draft_row_a() ->
    #{
        <<"id">> => ?REVIEW_DRAFT_A,
        <<"submission_id">> => ?SUBMISSION,
        <<"reviewer_uid">> => ?UID,
        <<"positive_point">> => <<"A 老师的存量草稿"/utf8>>,
        <<"focus_problem">> => <<>>,
        <<"practice_action">> => <<>>,
        <<"comment">> => <<>>,
        <<"video_attachment_id">> => null,
        <<"rework_required">> => false,
        <<"status">> => <<"draft">>,
        <<"published_at">> => null
    }.

published_row_b() ->
    #{
        <<"id">> => ?REVIEW_PUB_B,
        <<"submission_id">> => ?SUBMISSION,
        <<"reviewer_uid">> => ?TEACHER_B,
        <<"positive_point">> => <<"B 老师的正式回评"/utf8>>,
        <<"focus_problem">> => <<>>,
        <<"practice_action">> => <<>>,
        <<"comment">> => <<>>,
        <<"video_attachment_id">> => null,
        <<"rework_required">> => false,
        <<"status">> => <<"published">>,
        <<"published_at">> => <<"2026-09-17T12:00:00Z">>
    }.

%% 僵尸草稿场景 mocks：B 已发布（find_published 命中），Viewer 的本人草稿
%% 查询结果由 FindDraft 指定（A：命中存量草稿；B：undefined——已被消费）
zombie_mocks(Published, FindDraft) ->
    [
        {moya_acl, [
            {'submission_access', 2, fun(_Uid, ?SUBMISSION) ->
                {ok, staff, scope()}
            end}
        ]},
        {moya_context_repo, [
            {'submission_scope', 1, fun(?SUBMISSION) -> {ok, scope()} end}
        ]},
        {moya_submission_repo, [
            {'find', 1, fun(?SUBMISSION) -> {ok, sub_row(<<"submitted">>)} end},
            {'assets', 1, fun(?SUBMISSION) -> {ok, []} end}
        ]},
        {moya_review_repo, [
            {'ai_draft', 1, fun(?SUBMISSION) -> {ok, undefined} end},
            {'find_published', 1, fun(?SUBMISSION) -> Published end},
            {'find_draft', 2, fun(?SUBMISSION, _Uid) -> FindDraft end},
            {'assets', 1, fun(_) -> {ok, []} end}
        ]},
        {user_repo, [
            %% 发布行携带 reviewer_uid → reviewer_display_name 查 user 表
            %% （查无 → null 署名位，与生产语义一致）
            {'find_by_uid', 1, fun(_Uid) -> {error, not_found} end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _P) -> {ok, []} end}
        ]}
    ].

%% 主断言：B 发布后，A 打开 workbench 不再看到无法处理的僵尸草稿；
%% 已发布回评照常展示（A 转为读者视角）
d11_zombie_draft_hidden_from_other_teacher_test_() ->
    ?WITH_MECKS(
        zombie_mocks({ok, published_row_b()}, {ok, draft_row_a()}),
        fun() ->
            {ok, WB} = moya_review_logic:workbench(?UID, ?SUBMISSION),
            ?assertEqual(null, maps:get(<<"my_review_draft">>, WB)),
            Embedded =
                maps:get(
                    <<"my_review_draft">>,
                    maps:get(<<"submission">>, WB, #{}),
                    missing
                ),
            ?assertEqual(null, Embedded),
            Pub = maps:get(<<"published_review">>, maps:get(<<"submission">>, WB, #{})),
            ?assertMatch(#{<<"review_id">> := <<"986702">>}, Pub)
        end
    ).

%% staff 视角 submission_detail 同口径：teacher_view 不下发僵尸草稿
d11_zombie_draft_hidden_in_detail_test_() ->
    ?WITH_MECKS(
        zombie_mocks({ok, published_row_b()}, {ok, draft_row_a()}),
        fun() ->
            {ok, Detail} = moya_review_logic:submission_detail(?UID, ?SUBMISSION),
            ?assertEqual(null, maps:get(<<"my_review_draft">>, Detail))
        end
    ).

%% 发布者 B 本人：草稿已被 publish_tx 消费（find_draft → undefined），
%% workbench 正常聚合、published_review 可见——过滤不误伤发布者
d11_publisher_view_not_affected_test_() ->
    ?WITH_MECKS(
        zombie_mocks({ok, published_row_b()}, {ok, undefined}),
        fun() ->
            {ok, WB} = moya_review_logic:workbench(?TEACHER_B, ?SUBMISSION),
            ?assertEqual(null, maps:get(<<"my_review_draft">>, WB)),
            Pub = maps:get(<<"published_review">>, maps:get(<<"submission">>, WB, #{})),
            ?assertMatch(#{<<"review_id">> := <<"986702">>}, Pub)
        end
    ).

%% 回归边界：未发布（find_published undefined）时本人草稿照常下发——
%% 过滤条件必须钉死「已发布 ∧ 发布者非本人」，不得外溢到日常草稿恢复
d11_draft_visible_when_not_published_test_() ->
    ?WITH_MECKS(
        zombie_mocks({ok, undefined}, {ok, draft_row_a()}),
        fun() ->
            {ok, WB} = moya_review_logic:workbench(?UID, ?SUBMISSION),
            ?assertMatch(
                #{<<"review_id">> := <<"986701">>},
                maps:get(<<"my_review_draft">>, WB)
            )
        end
    ).
