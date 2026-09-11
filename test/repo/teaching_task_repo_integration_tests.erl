%% teaching_task_repo_integration_tests
%% MN-TASK-01 / MN-TASK-02 — 教师教学作业仓库层真库集成测试。
%%
%% 直连 scratch 库 moya_zcode_181902@127.0.0.1:4323（迁移 1→103 全量态 +
%% 00000106 group_task 幂等列），每用例 BEGIN ... ROLLBACK，不留数据。
%% 测试直接驱动 teaching_task_ds:create_in_tx 与 teaching_task_repo 的 _tx 函数
%% （与生产 elib_pg:with_tx 同一代码路径），验证：
%%   MN-TASK-02 配方：
%%     ① 幂等 CTE（ON CONFLICT 带部分索引谓词 uk_group_task_idempotency）
%%        + digest 判 5460 + 缺 key 拒绝
%%     ② task+assignments 同事务原子性（部分失败 → ROLLBACK 零半成品）
%%   MN-TASK-01 列表：
%%     ③ 含 0 提交新作业（不从 review queue 反派生）
%%     ④ 跨班过滤（staff 只见本班；assistant 可读）
%%     ⑤ 统计计数（learner/submitted/pending_review；withdrawn 不计）
%%     ⑥ 分页 page/total
%%     ⑦ learner_readiness 分支（0/多监护人、removed、跨班、跨机构）
%% DB 不可达时自动 skip。

-module(teaching_task_repo_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- 夹具（97 段独立 ID，与 98/99 段互不冲突） ----
-define(PG_HOST, "127.0.0.1").
-define(PG_PORT, 4323).
-define(PG_USER, <<"imboy_user">>).
-define(PG_PASS, <<"abc54321">>).
-define(PG_DB, <<"moya_zcode_181902">>).

-define(TEACHER_A1, 970001).
-define(TEACHER_B2, 970002).
-define(ASSIST_A1, 970003).
-define(MANAGER_A1, 970005).
-define(PARENT_1, 970011).
-define(PARENT_2, 970012).
-define(PARENT_3, 970013).
-define(PARENT_4, 970014).
-define(OUTSIDER, 970099).

-define(ORG_A, 973101).
-define(ORG_X, 973199).
-define(WS_A, 973111).
-define(WS_B, 973112).
-define(WS_X, 973191).
-define(GROUP_A1, 973201).
-define(GROUP_B2, 973202).

-define(LEARNER_OK, 974001).
-define(LEARNER_MULTI_G, 974002).
-define(LEARNER_NO_G, 974003).
-define(LEARNER_REMOVED, 974004).
-define(LEARNER_B2, 974005).
-define(LEARNER_XORG, 974099).

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
            timeout => 5000,
            %% 与生产 pg_conf 同款 timestamptz codec（RFC3339 binary）
            codecs => [{epgsql_codec_rfc3339_bin, []}]
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
    exec(C, Sql, []).

exec(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

q(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, Rows} -> Rows;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

one_int(C, Sql, Params) ->
    [#{<<"c">> := N}] = q(C, Sql, Params),
    N.

%%%===================================================================
%%% Seed（与 teaching_flow_integration_tests 同构，97 段独立夹具）
%%%===================================================================

seed(C) ->
    %% 用户（teacher/assistant/manager/parents/outsider）
    Uids = [
        {?TEACHER_A1, <<"t97_teacher_a1">>},
        {?TEACHER_B2, <<"t97_teacher_b2">>},
        {?ASSIST_A1, <<"t97_assist_a1">>},
        {?MANAGER_A1, <<"t97_manager_a1">>},
        {?PARENT_1, <<"t97_parent_1">>},
        {?PARENT_2, <<"t97_parent_2">>},
        {?PARENT_3, <<"t97_parent_3">>},
        {?PARENT_4, <<"t97_parent_4">>},
        {?OUTSIDER, <<"t97_outsider">>}
    ],
    lists:foreach(
        fun({Uid, Account}) ->
            exec(
                C,
                <<
                    "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) "
                    "VALUES ($1, 'x', $2, '127.0.0.1', 'x')"
                >>,
                [Uid, Account]
            )
        end,
        Uids
    ),
    %% 机构 / 工作区（X 机构用于跨机构 learner）
    exec(C, <<"INSERT INTO organization (id, name, owner_id) VALUES ($1, $2, $3)">>, [
        ?ORG_A, <<"机构97A"/utf8>>, ?MANAGER_A1
    ]),
    exec(C, <<"INSERT INTO organization (id, name, owner_id) VALUES ($1, $2, $3)">>, [
        ?ORG_X, <<"机构97X"/utf8>>, ?MANAGER_A1
    ]),
    exec(
        C,
        <<
            "INSERT INTO workspace (id, name, owner_id, organization_id) VALUES "
            "($1, ",
            "'A-校区'"/utf8,
            ", $3, $2), ($4, ",
            "'B-校区'"/utf8,
            ", $3, $2), ($5, ",
            "'X-校区'"/utf8,
            ", $3, $6)"
        >>,
        [?WS_A, ?ORG_A, ?MANAGER_A1, ?WS_B, ?WS_X, ?ORG_X]
    ),
    lists:foreach(
        fun(WsId) ->
            exec(
                C,
                <<
                    "INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, status) "
                    "VALUES ($1, $2, 'owner', $2, 'active')"
                >>,
                [WsId, ?MANAGER_A1]
            )
        end,
        [?WS_A, ?WS_B, ?WS_X]
    ),
    %% 班级群
    exec(
        C,
        <<
            "INSERT INTO \"group\" (id, owner_uid, creator_uid, scope, workspace_id, title) VALUES "
            "($1, $2, $2, 'workspace', $3, ",
            "'A1-硬笔班'"/utf8,
            "), "
            "($4, $2, $2, 'workspace', $5, ",
            "'B2-提高班'"/utf8,
            ")"
        >>,
        [?GROUP_A1, ?MANAGER_A1, ?WS_A, ?GROUP_B2, ?WS_B]
    ),
    %% staff：A1 teacher/assistant/manager；B2 teacher
    exec(
        C,
        <<
            "INSERT INTO class_staff (group_id, user_id, role) VALUES "
            "($1, $2, 'teacher'), ($1, $3, 'assistant'), ($1, $4, 'manager'), ($5, $6, 'teacher')"
        >>,
        [?GROUP_A1, ?TEACHER_A1, ?ASSIST_A1, ?MANAGER_A1, ?GROUP_B2, ?TEACHER_B2]
    ),
    %% learners（974099 属 X 机构）
    exec(
        C,
        <<
            "INSERT INTO learner (id, organization_id, display_name) VALUES "
            "($1, $2, ",
            "'学员OK'"/utf8,
            "), ($3, $2, ",
            "'双监护'"/utf8,
            "), ($4, $2, ",
            "'零监护'"/utf8,
            "), "
            "($5, $2, ",
            "'已移出'"/utf8,
            "), ($6, $2, ",
            "'B班学员'"/utf8,
            "), ($7, $8, ",
            "'跨机构'"/utf8,
            ")"
        >>,
        [
            ?LEARNER_OK,
            ?ORG_A,
            ?LEARNER_MULTI_G,
            ?LEARNER_NO_G,
            ?LEARNER_REMOVED,
            ?LEARNER_B2,
            ?LEARNER_XORG,
            ?ORG_X
        ]
    ),
    %% enrollment：A1 in {OK, MULTI, NOG, REMOVED(removed)}；B2 in {B2学员}
    exec(
        C,
        <<
            "INSERT INTO class_enrollment (group_id, learner_id, status) VALUES "
            "($1, $2, 'active'), ($1, $3, 'active'), ($1, $4, 'active'), "
            "($1, $5, 'removed'), ($6, $7, 'active')"
        >>,
        [
            ?GROUP_A1,
            ?LEARNER_OK,
            ?LEARNER_MULTI_G,
            ?LEARNER_NO_G,
            ?LEARNER_REMOVED,
            ?GROUP_B2,
            ?LEARNER_B2
        ]
    ),
    %% guardian_learner：OK 恰一可提交；MULTI 两个可提交；NOG 仅不可提交
    exec(
        C,
        <<
            "INSERT INTO guardian_learner (guardian_uid, learner_id, can_submit, can_view_review) VALUES "
            "($1, $2, true, true), "
            "($3, $4, true, true), ($5, $4, true, true), "
            "($6, $7, false, true)"
        >>,
        [
            ?PARENT_1,
            ?LEARNER_OK,
            ?PARENT_2,
            ?LEARNER_MULTI_G,
            ?PARENT_3,
            ?PARENT_4,
            ?LEARNER_NO_G
        ]
    ).

%%%===================================================================
%%% task 创建 helper（走 ds 生产配方：幂等 CTE + assignments）
%%%===================================================================

task_fields() ->
    #{title => <<"横竖练习"/utf8>>, description => <<"每天三行"/utf8>>, deadline => undefined}.

create_task(C, Uid, IdemKey, Digest, Learners) ->
    teaching_task_ds:create_in_tx(
        C, Uid, ?GROUP_A1, IdemKey, Digest, task_fields(), Learners
    ).

%%%===================================================================
%%% ⑦ learner_readiness：assignment_ready 分支
%%%===================================================================

readiness_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, Result} = teaching_task_repo:learner_readiness_tx(C, ?GROUP_A1, ?ORG_A, [
            ?LEARNER_OK,
            ?LEARNER_MULTI_G,
            ?LEARNER_NO_G,
            ?LEARNER_REMOVED,
            ?LEARNER_B2,
            ?LEARNER_XORG
        ]),
        ?assertEqual({ok, ?PARENT_1}, maps:get(?LEARNER_OK, Result)),
        %% 0 或多个 active can_submit 监护人 → setup-required（5432 语义）
        ?assertEqual(
            {error, guardian_setup_required}, maps:get(?LEARNER_MULTI_G, Result)
        ),
        ?assertEqual(
            {error, guardian_setup_required}, maps:get(?LEARNER_NO_G, Result)
        ),
        %% removed / 跨班 / 跨机构 → learner_not_in_class（5431 语义）
        ?assertEqual(
            {error, learner_not_in_class}, maps:get(?LEARNER_REMOVED, Result)
        ),
        ?assertEqual(
            {error, learner_not_in_class}, maps:get(?LEARNER_B2, Result)
        ),
        ?assertEqual(
            {error, learner_not_in_class}, maps:get(?LEARNER_XORG, Result)
        )
    end).

%%%===================================================================
%%% MN-TASK-02 ②：task+assignments 同事务原子性（部分失败零半成品）
%%%===================================================================

atomicity_partial_failure_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% SAVEPOINT 模拟外层事务边界（生产 elib_pg:with_tx 遇 {rollback,_} 执行 ROLLBACK）
        ok = exec(C, <<"SAVEPOINT sp_atomic">>),
        %% 同一 learner 两条 → 违反 uk_group_task_assignment_task_learner
        %% （模拟任一 assignment 写失败；生产中任何 DB 错误同路径）
        Result =
            create_task(C, ?TEACHER_A1, <<"idem-97-atomic">>, <<"digest-97-a">>, [
                {?LEARNER_OK, ?PARENT_1},
                {?LEARNER_OK, ?PARENT_1}
            ]),
        ?assertMatch({rollback, {db, _}}, Result),
        %% 外层回滚（with_tx 语义）后零半成品：task 与 assignments 行数均为 0
        ok = exec(C, <<"ROLLBACK TO SAVEPOINT sp_atomic">>),
        0 = one_int(
            C,
            <<
                "SELECT count(*) AS c FROM group_task WHERE idempotency_key = $1"
            >>,
            [<<"idem-97-atomic">>]
        ),
        0 = one_int(
            C,
            <<
                "SELECT count(*) AS c FROM group_task_assignment a "
                "JOIN group_task t ON t.task_id = a.task_id "
                "WHERE t.idempotency_key = $1"
            >>,
            [<<"idem-97-atomic">>]
        )
    end).

%%%===================================================================
%%% MN-TASK-02 ①：幂等重放（同 key 同 digest / 同 key 异 digest / 缺 key）
%%%===================================================================

idempotent_replay_test_() ->
    with_tx(fun(C) ->
        seed(C),
        Learners = [{?LEARNER_OK, ?PARENT_1}, {?LEARNER_MULTI_G, ?PARENT_2}],
        %% 首次创建
        {ok, P1} = create_task(C, ?TEACHER_A1, <<"idem-97-replay">>, <<"digest-97-x">>, Learners),
        ?assertEqual(false, maps:get(<<"replayed">>, P1)),
        TaskId = maps:get(<<"task_id">>, P1),
        ?assert(is_binary(TaskId) andalso byte_size(TaskId) > 0),
        ?assertEqual(2, length(maps:get(<<"assignments">>, P1))),
        %% 同 key 同 digest：返回原集合，行数不增
        {ok, P2} = create_task(C, ?TEACHER_A1, <<"idem-97-replay">>, <<"digest-97-x">>, Learners),
        ?assertEqual(true, maps:get(<<"replayed">>, P2)),
        ?assertEqual(TaskId, maps:get(<<"task_id">>, P2)),
        ?assertEqual(
            lists:sort(maps:get(<<"assignments">>, P1)),
            lists:sort(maps:get(<<"assignments">>, P2))
        ),
        1 = one_int(
            C,
            <<
                "SELECT count(*) AS c FROM group_task WHERE idempotency_key = $1"
            >>,
            [<<"idem-97-replay">>]
        ),
        2 = one_int(
            C,
            <<
                "SELECT count(*) AS c FROM group_task_assignment a "
                "JOIN group_task t ON t.task_id = a.task_id "
                "WHERE t.idempotency_key = $1"
            >>,
            [<<"idem-97-replay">>]
        ),
        %% 同 key 不同 digest：5460 冲突（不覆盖原结果）
        ?assertEqual(
            {rollback, idempotency_conflict},
            create_task(C, ?TEACHER_A1, <<"idem-97-replay">>, <<"digest-97-DIFFERENT">>, Learners)
        ),
        %% 冲突后原行未动
        1 = one_int(
            C,
            <<
                "SELECT count(*) AS c FROM group_task WHERE idempotency_key = $1"
            >>,
            [<<"idem-97-replay">>]
        ),
        %% 不同 key：新 task（幂等键作用域按 (creator,group,key)）
        {ok, P3} = create_task(C, ?TEACHER_A1, <<"idem-97-other">>, <<"digest-97-x">>, Learners),
        ?assertEqual(false, maps:get(<<"replayed">>, P3)),
        ?assertNotEqual(TaskId, maps:get(<<"task_id">>, P3))
    end).

idempotency_key_required_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% 缺 key（空 binary）→ 5461 语义拒绝（不落库）
        ?assertEqual(
            {rollback, idempotency_key_required},
            create_task(C, ?TEACHER_A1, <<>>, <<"digest-97-x">>, [{?LEARNER_OK, ?PARENT_1}])
        ),
        0 = one_int(
            C,
            <<
                "SELECT count(*) AS c FROM group_task WHERE group_id = $1 AND creator_id = $2"
            >>,
            [?GROUP_A1, ?TEACHER_A1]
        ),
        0 = one_int(
            C,
            <<
                "SELECT count(*) AS c FROM group_task_assignment a "
                "JOIN group_task t ON t.task_id = a.task_id "
                "WHERE t.group_id = $1 AND t.creator_id = $2"
            >>,
            [?GROUP_A1, ?TEACHER_A1]
        )
    end).

%%%===================================================================
%%% MN-TASK-01 ③④⑤⑥：列表（0 提交可见 / 跨班过滤 / 统计 / 分页）
%%%===================================================================

%% 直接插夹具 task（含幂等列），返回 task_id
seed_task(C, Id, TaskId, GroupId, Creator) ->
    exec(
        C,
        <<
            "INSERT INTO group_task (id, group_id, task_id, title, creator_id, status) "
            "VALUES ($1, $2, $3, ",
            "'横竖练习'"/utf8,
            ", $4, 1)"
        >>,
        [Id, GroupId, TaskId, Creator]
    ),
    TaskId.

seed_assignment(C, Id, TaskId, LearnerId, UserId) ->
    exec(
        C,
        <<
            "INSERT INTO group_task_assignment (id, task_id, user_id, learner_id) "
            "VALUES ($1, $2, $3, $4)"
        >>,
        [Id, TaskId, UserId, LearnerId]
    ).

seed_submission(C, Id, AssignmentId, LearnerId, Attempt, Status) ->
    %% withdrawn 需满足 00000098 撤回审计 CHECK（withdrawn_at/withdrawn_by 非空）
    {WAt, WBy} =
        case Status of
            <<"withdrawn">> -> {<<"2099-01-01T00:00:00Z">>, 970011};
            _ -> {null, null}
        end,
    exec(
        C,
        <<
            "INSERT INTO homework_submission "
            "(id, assignment_id, learner_id, attempt_no, status, withdrawn_at, withdrawn_by) "
            "VALUES ($1, $2, $3, $4, $5, $6, $7)"
        >>,
        [Id, AssignmentId, LearnerId, Attempt, Status, WAt, WBy]
    ).

seed_published_review(C, Id, SubmissionId, Reviewer) ->
    exec(
        C,
        <<
            "INSERT INTO teacher_review (id, submission_id, reviewer_uid, positive_point, status, published_at) "
            "VALUES ($1, $2, $3, $4, 'published', now())"
        >>,
        [Id, SubmissionId, Reviewer, <<"结构稳定"/utf8>>]
    ).

list_contains_zero_submission_task_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% 刚发布、尚无任何 submission 的作业必须出现在列表
        {ok, _} = create_task(C, ?TEACHER_A1, <<"idem-97-list0">>, <<"d0">>, [
            {?LEARNER_OK, ?PARENT_1}, {?LEARNER_MULTI_G, ?PARENT_2}
        ]),
        {ok, Rows} = teaching_task_repo:list_tasks_tx(C, ?TEACHER_A1, undefined, 1, 10),
        ?assertEqual(1, length(Rows)),
        [Row] = Rows,
        ?assertEqual(2, maps:get(<<"learner_count">>, Row)),
        ?assertEqual(0, maps:get(<<"submitted_count">>, Row)),
        ?assertEqual(0, maps:get(<<"pending_review_count">>, Row)),
        {ok, 1} = teaching_task_repo:count_tasks_tx(C, ?TEACHER_A1, undefined)
    end).

list_cross_group_filter_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, _} = create_task(C, ?TEACHER_A1, <<"idem-97-xa">>, <<"d">>, [
            {?LEARNER_OK, ?PARENT_1}
        ]),
        %% B2 班教师看不到 A1 班作业
        {ok, []} = teaching_task_repo:list_tasks_tx(C, ?TEACHER_B2, undefined, 1, 10),
        {ok, 0} = teaching_task_repo:count_tasks_tx(C, ?TEACHER_B2, undefined),
        %% 非.staff 任何班：空
        {ok, []} = teaching_task_repo:list_tasks_tx(C, ?OUTSIDER, undefined, 1, 10),
        %% assistant 可读本班（manager/teacher/assistant 均可读）
        {ok, Rows} = teaching_task_repo:list_tasks_tx(C, ?ASSIST_A1, undefined, 1, 10),
        ?assertEqual(1, length(Rows)),
        %% 指定班过滤：A1 教师查 B2 → 空（JOIN class_staff 同时限定 group）
        {ok, []} = teaching_task_repo:list_tasks_tx(C, ?TEACHER_A1, ?GROUP_B2, 1, 10),
        {ok, Rows2} = teaching_task_repo:list_tasks_tx(C, ?TEACHER_A1, ?GROUP_A1, 1, 10),
        ?assertEqual(1, length(Rows2))
    end).

list_stats_counts_test_() ->
    with_tx(fun(C) ->
        seed(C),
        TaskId = seed_task(C, 975001, <<"task97_stats">>, ?GROUP_A1, ?TEACHER_A1),
        %% 3 个教学 assignment
        seed_assignment(C, 976001, TaskId, ?LEARNER_OK, ?PARENT_1),
        seed_assignment(C, 976002, TaskId, ?LEARNER_MULTI_G, ?PARENT_2),
        seed_assignment(C, 976003, TaskId, ?LEARNER_NO_G, ?PARENT_4),
        %% 学员1：submitted 无 review；学员2：submitted + published review；
        %% 学员3：withdrawn（不计入 submitted）
        seed_submission(C, 977001, 976001, ?LEARNER_OK, 1, <<"submitted">>),
        seed_submission(C, 977002, 976002, ?LEARNER_MULTI_G, 1, <<"submitted">>),
        seed_submission(C, 977003, 976003, ?LEARNER_NO_G, 1, <<"withdrawn">>),
        seed_published_review(C, 978001, 977002, ?TEACHER_A1),
        {ok, [Row]} = teaching_task_repo:list_tasks_tx(C, ?TEACHER_A1, undefined, 1, 10),
        ?assertEqual(3, maps:get(<<"learner_count">>, Row)),
        ?assertEqual(2, maps:get(<<"submitted_count">>, Row)),
        ?assertEqual(1, maps:get(<<"pending_review_count">>, Row))
    end).

list_pagination_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% 3 个教学 task（id 递增，列表 id DESC：975003 排最前）
        lists:foreach(
            fun(N) ->
                TaskId = <<"task97_page_", (integer_to_binary(N))/binary>>,
                Id = 975000 + N,
                seed_task(C, Id, TaskId, ?GROUP_A1, ?TEACHER_A1),
                seed_assignment(C, 976000 + N, TaskId, ?LEARNER_OK, ?PARENT_1)
            end,
            [1, 2, 3]
        ),
        {ok, 3} = teaching_task_repo:count_tasks_tx(C, ?TEACHER_A1, undefined),
        {ok, Page1} = teaching_task_repo:list_tasks_tx(C, ?TEACHER_A1, undefined, 1, 2),
        ?assertEqual(2, length(Page1)),
        %% v3 P0-1 修复：list task_id = group_task.id（integer；客户端层再字符串化）
        ?assertEqual(975003, maps:get(<<"task_id">>, hd(Page1))),
        {ok, Page2} = teaching_task_repo:list_tasks_tx(C, ?TEACHER_A1, undefined, 2, 2),
        ?assertEqual(1, length(Page2)),
        ?assertEqual(975001, maps:get(<<"task_id">>, hd(Page2)))
    end).

active_staff_group_ids_test_() ->
    with_tx(fun(C) ->
        seed(C),
        {ok, GroupsA} = teaching_task_repo:active_staff_group_ids_tx(C, ?TEACHER_A1),
        ?assertEqual([?GROUP_A1], GroupsA),
        %% assistant 也是 active staff（读权限）
        {ok, GroupsAssist} = teaching_task_repo:active_staff_group_ids_tx(C, ?ASSIST_A1),
        ?assertEqual([?GROUP_A1], GroupsAssist),
        %% manager
        {ok, GroupsMgr} = teaching_task_repo:active_staff_group_ids_tx(C, ?MANAGER_A1),
        ?assertEqual([?GROUP_A1], GroupsMgr),
        %% 非 staff
        {ok, []} = teaching_task_repo:active_staff_group_ids_tx(C, ?OUTSIDER)
    end).

%%%===================================================================
%%% 普通（非教学）群作业不出现在教学列表
%%%===================================================================

list_excludes_plain_group_task_test_() ->
    with_tx(fun(C) ->
        seed(C),
        %% 无 learner_id 的普通群作业（learner_id NULL）
        TaskId = seed_task(C, 975091, <<"task97_plain">>, ?GROUP_A1, ?TEACHER_A1),
        seed_assignment_plain(C, 976091, TaskId, ?PARENT_1),
        {ok, []} = teaching_task_repo:list_tasks_tx(C, ?TEACHER_A1, undefined, 1, 10),
        {ok, 0} = teaching_task_repo:count_tasks_tx(C, ?TEACHER_A1, undefined)
    end).

seed_assignment_plain(C, Id, TaskId, UserId) ->
    exec(
        C,
        <<
            "INSERT INTO group_task_assignment (id, task_id, user_id) VALUES ($1, $2, $3)"
        >>,
        [Id, TaskId, UserId]
    ).
