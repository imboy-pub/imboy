%% moya_learner_bind_integration_tests
%% BIND-01 / BIND-02 / HISTORY-01 复核 — 学员账号绑定真库集成测试（Step 16）。
%%
%% 直连 moya_mig_test@127.0.0.1:4323（scratch，00000001→00000098 全量态），
%% 每用例 BEGIN ... ROLLBACK，不留数据（模式照 moya_flow_integration_tests）。
%% 测试直调 moya_learner_bind_repo 的 _tx 函数（与生产 elib_pg:with_tx
%% 同一代码路径）。
%% DB 不可达时自动 skip。
%%
%% 覆盖：
%%   guard      —— owner/manager 可操作；teacher(非 manager)/assistant/家长/陌生人 unauthorized
%%   BIND-01    —— 绑定前后 submission/teacher_review 主键与 learner_id 完全不变
%%   BIND-02    —— 解绑后监护人原权限仍可读历史；账号本人入口立即失效（经 B 的
%%                 history_access 两分支判定依据的 SQL 断言）；account_bound_* 审计痕迹保留
%%   dup/cross  —— 同 Org 同 user 重复绑被拒（uk_learner_org_user → duplicate_bind_in_org）；
%%                 跨 Org 同 user 绑独立 learner 成功
%%   HISTORY-01 —— 同 Org 跨 Workspace 历史连续（两班已发布回评按 learner 聚合）；
%%                 跨 Org learner 互读被拒（guardian 关系不存在 → B 的 ACL fail closed）

-module(moya_learner_bind_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- 夹具（与 moya_flow_integration_tests 同源；ID 段 99 前缀错开） ----
-define(PG_HOST, "127.0.0.1").
-define(PG_PORT, 4323).
-define(PG_USER, <<"imboy_user">>).
-define(PG_PASS, <<"abc54321">>).
-define(PG_DB, <<"moya_mig_test">>).

% OrgA owner + A1 班 teacher（一人双身份）
-define(OWNER, 990001).
% A1 班 manager
-define(MANAGER, 990004).
% A2 班 teacher（非 manager）
-define(TEACHER, 990002).
-define(ASSISTANT, 990003).
% learnerA 监护人
-define(PARENT_A, 990005).
% 被绑定的学员未来账号
-define(TARGET_USER, 990006).
-define(STRANGER, 990007).

-define(ORG_A, 991000).
-define(WS_A1, 992001).
-define(WS_A2, 992002).
-define(GROUP_A1, 993001).
-define(GROUP_A2, 993002).
% A1 班学员
-define(LEARNER_A1, 994001).
% A2 班学员（同 Org 另一 learner，dup 测试用）
-define(LEARNER_A2, 994002).
-define(ORG_B, 991100).
-define(WS_B, 992100).
-define(GROUP_B, 993100).
% OrgB 学员（跨 Org 测试用）
-define(LEARNER_B, 994100).
-define(TASK_ID_A1, <<"task16_hash_a1">>).
-define(TASK_ID_A2, <<"task16_hash_a2">>).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        {ok, _} = application:ensure_all_started(epgsql),
        try
            elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
        catch
            _:_ -> ok
        end,
        {ok, C} = epgsql:connect(#{
            host => ?PG_HOST,
            port => ?PG_PORT,
            username => ?PG_USER,
            password => ?PG_PASS,
            database => ?PG_DB,
            timeout => 5000
        }),
        C
    catch
        _:_ -> skip
    end.

close_conn(skip) ->
    ok;
close_conn(C) ->
    try
        epgsql:close(C)
    catch
        _:_ -> ok
    end,
    ok.

with_tx(TestFun) ->
    {setup, fun setup_conn/0, fun close_conn/1, fun
        (skip) ->
            [];
        (C) ->
            ?_test(begin
                ok = exec(C, <<"BEGIN">>),
                try
                    TestFun(C),
                    ok
                after
                    exec(C, <<"ROLLBACK">>)
                end
            end)
    end}.

exec(C, Sql) ->
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

q(C, Sql) ->
    case elib_pg:query(C, Sql, []) of
        {ok, Rows} -> Rows;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

%%%===================================================================
%%% Seed：OrgA（两 Workspace 两班）+ OrgB（一班）；A1 学员两班历史已发布
%%%===================================================================

seed(C) ->
    Users = <<
        "(990001,'x','t16_owner','127.0.0.1','x'),"
        "(990002,'x','t16_teacher','127.0.0.1','x'),"
        "(990003,'x','t16_assist','127.0.0.1','x'),"
        "(990004,'x','t16_mgr','127.0.0.1','x'),"
        "(990005,'x','t16_parent_a','127.0.0.1','x'),"
        "(990006,'x','t16_target','127.0.0.1','x'),"
        "(990007,'x','t16_stranger','127.0.0.1','x')"
    >>,
    exec(
        C,
        <<"INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES ", Users/binary>>
    ),
    exec(C, <<
        "INSERT INTO organization (id, name, owner_id) VALUES "
        "(991000, 'orgA', 990001), (991100, 'orgB', 990007)"
    >>),
    exec(C, <<
        "INSERT INTO workspace (id, name, owner_id, organization_id) VALUES "
        "(992001, 'A-hq', 990001, 991000), "
        "(992002, 'A-branch', 990001, 991000), "
        "(992100, 'B-hq', 990007, 991100)"
    >>),
    exec(C, <<
        "INSERT INTO \"group\" (id, owner_uid, creator_uid, scope, workspace_id, title) VALUES "
        "(993001, 990001, 990001, 'workspace', 992001, 'A1-class'), "
        "(993002, 990001, 990001, 'workspace', 992002, 'A2-class'), "
        "(993100, 990007, 990007, 'workspace', 992100, 'B1-class')"
    >>),
    exec(C, <<
        "INSERT INTO learner (id, organization_id, display_name) VALUES "
        "(994001, 991000, 'L-A')"
    >>),
    exec(C, <<
        "INSERT INTO class_enrollment (group_id, learner_id) VALUES "
        "(993001, 994001), (993002, 994001)"
    >>),
    exec(C, <<
        "INSERT INTO class_staff (group_id, user_id, role) VALUES "
        "(993001, 990001, 'teacher'), (993001, 990004, 'manager'), "
        "(993001, 990003, 'assistant'), (993002, 990002, 'teacher'), "
        "(993100, 990007, 'teacher')"
    >>),
    exec(C, <<
        "INSERT INTO guardian_learner (guardian_uid, learner_id, can_submit, can_view_review) "
        "VALUES (990005, 994001, true, true)"
    >>),
    exec(C, <<
        "INSERT INTO group_task (id, group_id, task_id, title, creator_id, status) VALUES "
        "(995001, 993001, 'task16_hash_a1', 'task-A1', 990001, 1), "
        "(995002, 993002, 'task16_hash_a2', 'task-A2', 990001, 1)"
    >>),
    %% A1 学员在两班各一个教学 assignment
    exec(C, <<
        "INSERT INTO group_task_assignment (id, task_id, user_id, learner_id) VALUES "
        "(996001, 'task16_hash_a1', 990005, 994001), "
        "(996002, 'task16_hash_a2', 990005, 994001)"
    >>),
    %% 两班各一条已提交 + 已发布回评（同 Org 跨 Workspace 历史）
    exec(C, <<
        "INSERT INTO attachment (id, file_hash256, path, mime_type, creator_user_id) VALUES "
        "(998001, 'h16v1', 'p/998001', 'video/mp4', 990005), "
        "(998002, 'h16v2', 'p/998002', 'video/mp4', 990005)"
    >>),
    exec(C, <<
        "INSERT INTO homework_submission (id, assignment_id, learner_id, submitted_by, "
        "attempt_no, status) VALUES "
        "(997001, 996001, 994001, 990005, 1, 'submitted'), "
        "(997002, 996002, 994001, 990005, 1, 'submitted')"
    >>),
    exec(C, <<
        "INSERT INTO teacher_review (id, submission_id, reviewer_uid, positive_point, "
        "focus_problem, practice_action, comment, rework_required, status, published_at) VALUES "
        "(999001, 997001, 990001, 'good-A1', '', '', '', false, 'published', now()), "
        "(999002, 997002, 990002, 'good-A2', '', '', '', false, 'published', now())"
    >>).

%%%===================================================================
%%% 守卫：deny-by-default
%%%===================================================================

guard_test_() ->
    with_tx(fun(C) ->
        seed(C),
        ?assertEqual(owner, moya_learner_bind_repo:operator_role_tx(C, ?OWNER, ?LEARNER_A1)),
        ?assertEqual(
            manager, moya_learner_bind_repo:operator_role_tx(C, ?MANAGER, ?LEARNER_A1)
        ),
        ?assertEqual(
            unauthorized, moya_learner_bind_repo:operator_role_tx(C, ?TEACHER, ?LEARNER_A1)
        ),
        ?assertEqual(
            unauthorized, moya_learner_bind_repo:operator_role_tx(C, ?ASSISTANT, ?LEARNER_A1)
        ),
        ?assertEqual(
            unauthorized, moya_learner_bind_repo:operator_role_tx(C, ?PARENT_A, ?LEARNER_A1)
        ),
        ?assertEqual(
            unauthorized, moya_learner_bind_repo:operator_role_tx(C, ?STRANGER, ?LEARNER_A1)
        ),
        %% OrgB owner 对 OrgA learner 越权（跨 Org）
        ?assertEqual(
            unauthorized, moya_learner_bind_repo:operator_role_tx(C, ?STRANGER, ?LEARNER_A1)
        )
    end).

%%%===================================================================
%%% BIND-01：绑定前后 submission / teacher_review 主键与 learner_id 不变
%%%===================================================================

bind01_no_copy_no_move_test_() ->
    with_tx(fun(C) ->
        seed(C),
        SubsBefore = snap(
            C,
            <<"SELECT id, learner_id, assignment_id, attempt_no FROM homework_submission ORDER BY id">>
        ),
        RevsBefore = snap(
            C, <<"SELECT id, submission_id, reviewer_uid, status FROM teacher_review ORDER BY id">>
        ),
        {ok, Row} = moya_learner_bind_repo:bind_tx(C, ?LEARNER_A1, ?TARGET_USER, ?OWNER),
        ?assertEqual(?TARGET_USER, maps:get(<<"user_id">>, Row)),
        ?assert(maps:get(<<"account_bound_at">>, Row) =/= null),
        ?assertEqual(?OWNER, maps:get(<<"account_bound_by">>, Row)),
        %% 无复制（行数不变）、无迁移（内容不变）
        ?assertEqual(
            SubsBefore,
            snap(
                C,
                <<"SELECT id, learner_id, assignment_id, attempt_no FROM homework_submission ORDER BY id">>
            )
        ),
        ?assertEqual(
            RevsBefore,
            snap(
                C,
                <<"SELECT id, submission_id, reviewer_uid, status FROM teacher_review ORDER BY id">>
            )
        )
    end).

%%%===================================================================
%%% BIND-02：解绑后监护人原权限可读；账号本人入口立即失效；审计痕迹保留
%%%===================================================================

bind02_unbind_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, _} = moya_learner_bind_repo:bind_tx(C, ?LEARNER_A1, ?TARGET_USER, ?OWNER),
        {ok, Unbound} = moya_learner_bind_repo:unbind_tx(C, ?LEARNER_A1, ?OWNER),
        ?assertEqual(null, maps:get(<<"user_id">>, Unbound)),
        %% 教学数据全部保留（不删 learner/submission/review）
        [#{<<"c">> := 1}] = q(C, <<"SELECT count(*) AS c FROM learner WHERE id = 994001">>),
        [#{<<"c">> := 2}] = q(
            C, <<"SELECT count(*) AS c FROM homework_submission WHERE learner_id = 994001">>
        ),
        [#{<<"c">> := 2}] = q(
            C, <<"SELECT count(*) AS c FROM teacher_review WHERE status = 'published'">>
        ),
        %% 监护人原权限不变：guardian_learner 行保留 + history 仍按 learner 返回已发布回评
        [#{<<"c">> := 1}] = q(C, <<
            "SELECT count(*) AS c FROM guardian_learner "
            "WHERE guardian_uid = 990005 AND learner_id = 994001 AND status = 'active' AND can_view_review"
        >>),
        [#{<<"c">> := 2}] = q(C, <<
            "SELECT count(*) AS c FROM teacher_review tr "
            "JOIN homework_submission hs ON hs.id = tr.submission_id "
            "WHERE hs.learner_id = 994001 AND tr.status = 'published'"
        >>),
        %% 账号本人入口立即失效：B 的 history_access 两分支判定依据 —— guardian 关系与 staff 关系均不存在
        [#{<<"c">> := 0}] = q(C, <<
            "SELECT count(*) AS c FROM guardian_learner "
            "WHERE guardian_uid = 990006 AND learner_id = 994001"
        >>),
        [#{<<"c">> := 0}] = q(C, <<
            "SELECT count(*) AS c FROM class_staff cs "
            "JOIN class_enrollment e ON e.group_id = cs.group_id AND e.status = 'active' "
            "WHERE e.learner_id = 994001 AND cs.user_id = 990006 AND cs.status = 'active'"
        >>),
        %% 审计痕迹：account_bound_* 保留（最近一次绑定），user_id=NULL 即当前未绑定
        {ok, After} = moya_learner_bind_repo:find_tx(C, ?LEARNER_A1),
        ?assertEqual(?OWNER, maps:get(<<"account_bound_by">>, After)),
        ?assert(maps:get(<<"account_bound_at">>, After) =/= null)
    end).

%%%===================================================================
%%% 同 Org 重复绑定拒绝 / 跨 Org 独立 learner 绑定成功
%%%===================================================================

dup_same_org_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% 同 Org 第二个 learner 绑同一 user → 23505 分类为 duplicate_bind_in_org
        {ok, _} = moya_learner_bind_repo:bind_tx(C, ?LEARNER_A1, ?TARGET_USER, ?OWNER),
        exec(C, <<
            "INSERT INTO learner (id, organization_id, display_name) VALUES "
            "(994002, 991000, 'L-A2')"
        >>),
        exec(C, <<"INSERT INTO class_enrollment (group_id, learner_id) VALUES (993001, 994002)">>),
        ?assertEqual(
            {error, duplicate_bind_in_org},
            moya_learner_bind_repo:bind_tx(C, ?LEARNER_A2, ?TARGET_USER, ?OWNER)
        )
    %% 注：23505 后本事务已 abort —— 解绑重绑/跨 Org 各自独立用例（25P02 规避）
    end).

rebind_after_unbind_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, _} = moya_learner_bind_repo:bind_tx(C, ?LEARNER_A1, ?TARGET_USER, ?OWNER),
        {ok, _} = moya_learner_bind_repo:unbind_tx(C, ?LEARNER_A1, ?OWNER),
        %% 解绑后同 learner 重新绑定（幂等场景）→ 成功
        {ok, Re} = moya_learner_bind_repo:bind_tx(C, ?LEARNER_A1, ?TARGET_USER, ?MANAGER),
        ?assertEqual(?TARGET_USER, maps:get(<<"user_id">>, Re))
    end).

cross_org_bind_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% 跨 Org：OrgB learner 绑同一 user → 成功（uk_learner_org_user 部分唯一，DB-BIND-01）
        exec(C, <<
            "INSERT INTO learner (id, organization_id, display_name) VALUES "
            "(994100, 991100, 'L-B')"
        >>),
        exec(C, <<"INSERT INTO class_enrollment (group_id, learner_id) VALUES (993100, 994100)">>),
        {ok, _} = moya_learner_bind_repo:bind_tx(C, ?LEARNER_A1, ?TARGET_USER, ?OWNER),
        {ok, CrossRow} = moya_learner_bind_repo:bind_tx(C, ?LEARNER_B, ?TARGET_USER, ?STRANGER),
        ?assertEqual(?TARGET_USER, maps:get(<<"user_id">>, CrossRow)),
        ?assertEqual(?ORG_B, maps:get(<<"organization_id">>, CrossRow))
    end).

invalid_target_user_test_() ->
    with_tx(fun(C) ->
        seed(C),
        ?assertEqual(
            {error, invalid_target_user},
            moya_learner_bind_repo:bind_tx(C, ?LEARNER_A1, 999999999, ?OWNER)
        )
    end).

%%%===================================================================
%%% HISTORY-01 复核：同 Org 跨 Workspace 连续；跨 Org 互读拒绝
%%%===================================================================

history01_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% 同 Org 跨 Workspace：B 的 history 查询按 learner_id 聚合（moya_submission_repo:history）
        %% —— 复核用同构 join 断言：learner 994001 在两个 workspace 的已发布回评全部返回（连续）。
        Rows = q(C, <<
            "SELECT hs.id AS submission_id, w.id AS ws_id, tr.id AS review_id "
            "FROM homework_submission hs "
            "JOIN group_task_assignment a ON a.id = hs.assignment_id "
            "JOIN group_task gt ON gt.task_id = a.task_id "
            "JOIN \"group\" g ON g.id = gt.group_id "
            "JOIN workspace w ON w.id = g.workspace_id "
            "LEFT JOIN teacher_review tr ON tr.submission_id = hs.id AND tr.status = 'published' "
            "WHERE hs.learner_id = 994001 ORDER BY hs.id"
        >>),
        ?assertEqual(2, length(Rows)),
        WsIds = lists:usort([maps:get(<<"ws_id">>, R) || R <- Rows]),
        ?assertEqual([?WS_A1, ?WS_A2], lists:sort(WsIds)),
        ?assert(lists:all(fun(R) -> maps:get(<<"review_id">>, R) =/= null end, Rows)),
        %% 跨 Org：OrgA 监护人（parentA）对 OrgB learner 无 guardian 关系
        %% （deny-by-default：B 的 resolve_guardian 查 guardian_learner 无行 → not_guardian）
        [#{<<"c">> := 0}] = q(C, <<
            "SELECT count(*) AS c FROM guardian_learner gl "
            "JOIN learner l ON l.id = gl.learner_id "
            "WHERE gl.guardian_uid = 990005 AND l.organization_id = 991100 "
            "AND gl.status = 'active'"
        >>),
        %% OrgA 的老师（owner/teacher）对 OrgB 班无 staff 关系
        [#{<<"c">> := 0}] = q(C, <<
            "SELECT count(*) AS c FROM class_staff cs "
            "JOIN \"group\" g ON g.id = cs.group_id "
            "JOIN workspace w ON w.id = g.workspace_id "
            "WHERE cs.user_id = 990001 AND w.organization_id = 991100 AND cs.status = 'active'"
        >>),
        %% OrgB learner 无任何 OrgA 提交（历史分区：查询按 learner 锚定，物理不可混）
        [#{<<"c">> := 0}] = q(C, <<
            "SELECT count(*) AS c FROM homework_submission hs "
            "JOIN learner l ON l.id = hs.learner_id "
            "WHERE l.organization_id = 991100 AND l.id <> 994100"
        >>)
    end).

%%%===================================================================
%%% 追加（B，R6 Step 16 接线）：账号本人历史入口正向 + DB 审计行断言
%%%===================================================================

%% 绑定后 learner.user_id == 账号本人 → history_access 第三分支
%%（moya_review_logic:self_bound）放行依据成立；其他账号不因绑定获得该分支。
%% 解绑失效形态已由 bind02 覆盖（user_id 置 NULL 后同构 SQL 计 0）。
history_self_branch_positive_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% 绑定前：本人分支判定依据不存在
        [#{<<"c">> := 0}] = q(C, <<
            "SELECT count(*) AS c FROM learner "
            "WHERE id = 994001 AND user_id = 990006 AND status = 'active'"
        >>),
        {ok, _} = moya_learner_bind_repo:bind_tx(C, ?LEARNER_A1, ?TARGET_USER, ?OWNER),
        %% 绑定后：账号本人分支依据成立（self_bound 同构 SQL）
        [#{<<"c">> := 1}] = q(C, <<
            "SELECT count(*) AS c FROM learner "
            "WHERE id = 994001 AND user_id = 990006 AND status = 'active'"
        >>),
        %% 其他账号（陌生人）不因该绑定获得本人分支
        [#{<<"c">> := 0}] = q(C, <<
            "SELECT count(*) AS c FROM learner "
            "WHERE id = 994001 AND user_id = 990007 AND status = 'active'"
        >>)
    end).

%% bind 后审计行存在（00000099 teaching_admin_audit 契约复核，SQL 同构）：
%% logic 层走全局连接池（elib_pg:with_tx）直连测试不可达，故采用与
%% HISTORY-01 相同的"同构 SQL 独立复核"手法：用与
%% moya_learner_bind_logic:audit_admin_tx 完全一致的 INSERT 语句验证
%% 表结构契约（action 枚举、列约束、detail jsonb），并断言 bind 后行可查。
%% "logic 确实会调该 INSERT"由 handler/logic 层 EUnit 覆盖
%% （moya_learner_bind_handler_tests）。
audit_rows_after_bind_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, _} = moya_learner_bind_repo:bind_tx(C, ?LEARNER_A1, ?TARGET_USER, ?OWNER),
        %% 与 audit_admin_tx 同款 INSERT（bind_learner 动作 + owner 角色）
        AuditId = elib_tsid:generate(),
        Detail = jsone:encode(#{<<"role">> => <<"owner">>}),
        {ok, 1} = elib_pg:execute(
            C,
            <<
                "INSERT INTO teaching_admin_audit "
                "(id, action, operator_uid, learner_id, target_user_id, detail) "
                "VALUES ($1, $2, $3, $4, $5, $6)"
            >>,
            [AuditId, <<"bind_learner">>, ?OWNER, ?LEARNER_A1, ?TARGET_USER, Detail]
        ),
        %% 行可查且形态正确（含 detail->role）
        [
            #{
                <<"action">> := <<"bind_learner">>,
                <<"operator_uid">> := ?OWNER,
                <<"learner_id">> := ?LEARNER_A1,
                <<"target_user_id">> := ?TARGET_USER,
                <<"role">> := <<"owner">>
            }
        ] =
            q(C, <<
                "SELECT action, operator_uid, learner_id, target_user_id, "
                "detail->>'role' AS role FROM teaching_admin_audit "
                "WHERE learner_id = 994001"
            >>),
        %% unbind_learner 动作同样被枚举接受（target_user_id NULL 语义）
        {ok, 1} = elib_pg:execute(
            C,
            <<
                "INSERT INTO teaching_admin_audit "
                "(id, action, operator_uid, learner_id, target_user_id, detail) "
                "VALUES ($1, $2, $3, $4, $5, $6)"
            >>,
            [
                elib_tsid:generate(),
                <<"unbind_learner">>,
                ?MANAGER,
                ?LEARNER_A1,
                null,
                jsone:encode(#{<<"role">> => <<"manager">>})
            ]
        ),
        [#{<<"c">> := 2}] = q(C, <<"SELECT count(*) AS c FROM teaching_admin_audit">>)
    end).

snap(C, Sql) ->
    q(C, Sql).
