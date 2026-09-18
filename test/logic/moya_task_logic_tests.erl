%% moya_task_logic_tests
%% MN-TASK-01 / MN-TASK-02 — 教师教学作业业务逻辑测试（角色矩阵 + learner 校验 + 事务传播）。
%%
%% 覆盖：
%%   create 角色矩阵 —— manager✓ teacher✓ assistant✗(role_denied)
%%                      非staff✗(not_staff) inactive✗(not_staff)
%%   create 参数校验 —— title trim 后空/超 200、deadline 过去(assignment_closed)/非法、
%%                      learner_ids 空/重复/非法 TSID、group_id 非法
%%   learner 校验    —— removed enrollment / 跨班 / 跨机构 → learner_not_in_class；
%%                      0 或多个 active can_submit 监护人 → guardian_setup_required
%%   机构一致性      —— group 无机构（workspace 未挂 org）→ cross_org
%%   事务传播        —— ds 返回 db_error → logic db_error；成功 → payload 透传
%%   list            —— 指定班非本人 staff → class_not_visible；
%%                      汇总无任何 staff 班 → class_not_visible；
%%                      repo 行 → envelope（TSID string、deadline|null）
%%
%% 全部 meck，零真实库（同事务原子性/幂等真源由 repo 集成测试验证）。

-module(moya_task_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID_MANAGER, 970005).
-define(UID_TEACHER, 970001).
-define(UID_ASSIST, 970003).
-define(UID_OUTSIDER, 970099).
-define(GROUP_A1, 973201).
-define(GROUP_B2, 973202).
-define(ORG_A, 973101).
-define(LEARNER_A1, 974001).
-define(LEARNER_A2, 974002).
-define(PARENT_A1, 970011).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

%% Role 模拟 moya_acl:resolve_staff(_, _, write) 的完整行为：
%% manager/teacher 放行；assistant（write 白名单外）→ role_denied；
%% 无 class_staff 行 → not_staff；关系 removed → inactive
acl_mocks(Role) ->
    StaffResult =
        case Role of
            manager -> {ok, #{<<"role">> => <<"manager">>}};
            teacher -> {ok, #{<<"role">> => <<"teacher">>}};
            assistant -> {error, role_denied};
            not_staff -> {error, not_staff};
            inactive -> {error, inactive}
        end,
    [
        {moya_acl, [
            {'resolve_staff', 3, fun(_Uid, _GroupId, _Need) -> StaffResult end}
        ]},
        {moya_context_repo, [
            {'group_org', 1, fun(_GroupId) -> {ok, ?ORG_A} end}
        ]}
    ].

%% Readiness = ok | zero_guardian | multi_guardians | removed | foreign | foreign_org
readiness_mock(Readiness) ->
    Result =
        case Readiness of
            ok ->
                #{
                    ?LEARNER_A1 => {ok, ?PARENT_A1},
                    ?LEARNER_A2 => {ok, ?PARENT_A1}
                };
            zero_guardian ->
                #{?LEARNER_A1 => {error, guardian_setup_required}};
            multi_guardians ->
                #{?LEARNER_A1 => {error, guardian_setup_required}}
        end,
    {moya_task_repo, [
        {'learner_readiness', 3, fun(_GroupId, _OrgId, _LearnerIds) -> {ok, Result} end}
    ]}.

readiness_mock_per_learner(Map) ->
    {moya_task_repo, [
        {'learner_readiness', 3, fun(_GroupId, _OrgId, _LearnerIds) -> {ok, Map} end}
    ]}.

ds_mock(Return) ->
    {moya_task_ds, [
        {'create', 6, fun(_Uid, _GroupId, _Key, _Digest, _Fields, _Learners) ->
            self() ! ds_create_called,
            Return
        end}
    ]}.

body() ->
    #{
        <<"group_id">> => integer_to_binary(?GROUP_A1),
        <<"title">> => <<" 横竖练习 "/utf8>>,
        <<"description">> => <<"每天三行"/utf8>>,
        <<"deadline">> => <<"2099-01-01T00:00:00Z">>,
        <<"learner_ids">> => [
            integer_to_binary(?LEARNER_A1), integer_to_binary(?LEARNER_A2)
        ]
    }.

%%%===================================================================
%%% create：角色矩阵（deny-by-default）
%%%===================================================================

create_manager_allowed_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            {ok, Payload} = moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, body()),
            ?assertEqual(false, maps:get(<<"replayed">>, Payload)),
            receive
                ds_create_called -> ok
            after 0 -> ?assert(false, "ds create not reached")
            end
        end
    ).

create_teacher_allowed_test_() ->
    ?WITH_MECKS(
        acl_mocks(teacher) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            {ok, _} = moya_task_logic:create(?UID_TEACHER, <<"idem-97">>, body())
        end
    ).

create_assistant_denied_test_() ->
    ?WITH_MECKS(
        acl_mocks(assistant) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, role_denied},
                moya_task_logic:create(?UID_ASSIST, <<"idem-97">>, body())
            ),
            receive
                ds_create_called -> ?assert(false, "assistant must not reach ds")
            after 0 -> ok
            end
        end
    ).

create_not_staff_denied_test_() ->
    ?WITH_MECKS(
        acl_mocks(not_staff) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, not_staff},
                moya_task_logic:create(?UID_OUTSIDER, <<"idem-97">>, body())
            )
        end
    ).

create_inactive_staff_denied_test_() ->
    ?WITH_MECKS(
        acl_mocks(inactive) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, not_staff},
                moya_task_logic:create(?UID_TEACHER, <<"idem-97">>, body())
            )
        end
    ).

%%%===================================================================
%%% create：参数校验
%%%===================================================================

create_title_blank_after_trim_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, bad_param},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"title">> => <<"   ">>
                })
            )
        end
    ).

create_title_too_long_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            Long = binary:copy(<<"横"/utf8>>, 201),
            ?assertEqual(
                {error, bad_param},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"title">> => Long
                })
            )
        end
    ).

create_title_trimmed_to_ds_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            {ok, _} = moya_task_logic:create(
                ?UID_MANAGER, <<"idem-97">>, (body())#{<<"title">> => <<"  横竖练习  "/utf8>>}
            )
        end
    ).

create_deadline_past_rejected_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, assignment_closed},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"deadline">> => <<"2000-01-01T00:00:00Z">>
                })
            )
        end
    ).

create_deadline_invalid_rejected_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, bad_param},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"deadline">> => <<"not-a-date">>
                })
            )
        end
    ).

create_learner_ids_empty_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, bad_param},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"learner_ids">> => []
                })
            )
        end
    ).

create_learner_ids_duplicated_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, bad_param},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"learner_ids">> => [
                        integer_to_binary(?LEARNER_A1),
                        integer_to_binary(?LEARNER_A1)
                    ]
                })
            )
        end
    ).

create_learner_ids_invalid_tsid_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, bad_param},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"learner_ids">> => [<<"abc-not-tsid">>]
                })
            )
        end
    ).

%%%===================================================================
%%% create：learner_ids 数量上限（A1-D09）
%%%===================================================================

learner_ids(N) ->
    [integer_to_binary(974100 + I) || I <- lists:seq(1, N)].

%% 201 个 → bad_param(422)：readiness IN 子句与逐条 insert 随列表线性膨胀，
%% 上限防线拒绝超长事务/巨型 SQL
create_learner_ids_over_limit_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, bad_param},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"learner_ids">> => learner_ids(201)
                })
            )
        end
    ).

%% 200 个（恰在上限）→ 放行：ds 收到全部 200 learners（repo 层计数 mock）
create_learner_ids_at_limit_200_test_() ->
    ReadyMap = maps:from_list([
        {974100 + I, {ok, ?PARENT_A1}}
     || I <- lists:seq(1, 200)
    ]),
    DsMock =
        {moya_task_ds, [
            {'create', 6, fun(_Uid, _G, _K, _D, _F, Learners) ->
                self() ! {ds_learners, length(Learners)},
                {ok, created_payload()}
            end}
        ]},
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock_per_learner(ReadyMap), DsMock],
        fun() ->
            {ok, _} = moya_task_logic:create(
                ?UID_MANAGER, <<"idem-97">>, (body())#{<<"learner_ids">> => learner_ids(200)}
            ),
            receive
                {ds_learners, 200} -> ok
            after 0 -> ?assert(false, "ds create not called with 200 learners")
            end
        end
    ).

create_group_id_invalid_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, bad_param},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"group_id">> => <<"x-1">>
                })
            )
        end
    ).

%%%===================================================================
%%% create：learner 校验分支（5431/5432 语义）
%%%===================================================================

create_zero_submit_guardian_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(zero_guardian), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, guardian_setup_required},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"learner_ids">> => [integer_to_binary(?LEARNER_A1)]
                })
            )
        end
    ).

create_multi_submit_guardians_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(multi_guardians), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, guardian_setup_required},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"learner_ids">> => [integer_to_binary(?LEARNER_A1)]
                })
            )
        end
    ).

create_learner_removed_enrollment_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++
            [
                readiness_mock_per_learner(#{?LEARNER_A1 => {error, learner_not_in_class}}),
                ds_mock({ok, created_payload()})
            ],
        fun() ->
            ?assertEqual(
                {error, learner_not_in_class},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"learner_ids">> => [integer_to_binary(?LEARNER_A1)]
                })
            )
        end
    ).

create_learner_foreign_class_test_() ->
    %% 跨班 learner：repo 返回 learner_not_in_class（行缺失或 enrollment removed）
    ?WITH_MECKS(
        acl_mocks(manager) ++
            [
                readiness_mock_per_learner(#{974099 => {error, learner_not_in_class}}),
                ds_mock({ok, created_payload()})
            ],
        fun() ->
            ?assertEqual(
                {error, learner_not_in_class},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, (body())#{
                    <<"learner_ids">> => [<<"974099">>]
                })
            )
        end
    ).

create_readiness_db_error_test_() ->
    RepoMock =
        {moya_task_repo, [
            {'learner_readiness', 3, fun(_G, _O, _L) -> {error, boom} end}
        ]},
    ?WITH_MECKS(acl_mocks(manager) ++ [RepoMock, ds_mock({ok, created_payload()})], fun() ->
        ?assertEqual(
            {error, db_error},
            moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, body())
        )
    end).

%%%===================================================================
%%% create：机构一致性 + 事务失败传播
%%%===================================================================

create_group_without_org_rejected_test_() ->
    %% 班级机构解析为 NULL → fail closed（T1/T2 语义）
    StaffMock =
        {moya_acl, [
            {'resolve_staff', 3, fun(_Uid, _GroupId, _Need) ->
                {ok, #{<<"role">> => <<"manager">>}}
            end}
        ]},
    CtxMock =
        {moya_context_repo, [
            {'group_org', 1, fun(_GroupId) -> {ok, undefined} end}
        ]},
    ?WITH_MECKS(
        [StaffMock] ++
            [CtxMock, readiness_mock(ok), ds_mock({ok, created_payload()})],
        fun() ->
            ?assertEqual(
                {error, cross_org},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, body())
            )
        end
    ).

create_ds_db_error_propagates_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({error, db_error})],
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, body())
            )
        end
    ).

create_ds_conflict_propagates_test_() ->
    ?WITH_MECKS(
        acl_mocks(manager) ++ [readiness_mock(ok), ds_mock({error, idempotency_conflict})],
        fun() ->
            ?assertEqual(
                {error, idempotency_conflict},
                moya_task_logic:create(?UID_MANAGER, <<"idem-97">>, body())
            )
        end
    ).

create_passes_unique_guardian_to_ds_test_() ->
    DsMock =
        {moya_task_ds, [
            {'create', 6, fun(_Uid, GroupId, _Key, _Digest, Fields, Learners) ->
                self() ! {ds_args, GroupId, Fields, Learners},
                {ok, created_payload()}
            end}
        ]},
    ?WITH_MECKS(acl_mocks(manager) ++ [readiness_mock(ok), DsMock], fun() ->
        {ok, _} = moya_task_logic:create(
            ?UID_MANAGER, <<"idem-97">>, (body())#{<<"deadline">> => undefined}
        ),
        receive
            {ds_args, ?GROUP_A1, Fields, Learners} ->
                %% title 已 trim；无 deadline 传 undefined；learner 携带唯一监护人 uid
                ?assertEqual(<<"横竖练习"/utf8>>, maps:get(title, Fields)),
                ?assertEqual(undefined, maps:get(deadline, Fields, undefined)),
                ?assertEqual(
                    [{?LEARNER_A1, ?PARENT_A1}, {?LEARNER_A2, ?PARENT_A1}],
                    lists:sort(Learners)
                )
        after 0 -> ?assert(false, "ds args not captured")
        end
    end).

%%%===================================================================
%%% list：班级可见性 + envelope
%%%===================================================================

list_mocks(StaffGroups, ListResult, CountResult) ->
    [
        {moya_task_repo, [
            {'active_staff_group_ids', 1, fun(_Uid) -> {ok, StaffGroups} end},
            {'list_tasks', 4, fun(_Uid, _GroupIdOpt, _Page, _Size) -> ListResult end},
            {'count_tasks', 2, fun(_Uid, _GroupIdOpt) -> CountResult end}
        ]}
    ].

list_row() ->
    %% v3 P0-1 修复后：repo list 返回 t.id AS task_id（integer），logic tsid 化
    #{
        <<"task_id">> => 975001000000000019,
        <<"group_id">> => ?GROUP_A1,
        <<"group_name">> => <<"A1-硬笔班"/utf8>>,
        <<"title">> => <<"横竖练习"/utf8>>,
        <<"description">> => <<>>,
        <<"deadline">> => null,
        <<"learner_count">> => 2,
        <<"submitted_count">> => 1,
        <<"pending_review_count">> => 1,
        <<"created_at">> => <<"2026-09-10T10:00:00Z">>
    }.

list_with_group_not_staff_denied_test_() ->
    ?WITH_MECKS(list_mocks([?GROUP_B2], {ok, []}, {ok, 0}), fun() ->
        ?assertEqual(
            {error, class_not_visible},
            moya_task_logic:list(?UID_TEACHER, ?GROUP_A1, {1, 10})
        )
    end).

list_all_without_any_staff_class_denied_test_() ->
    ?WITH_MECKS(list_mocks([], {ok, []}, {ok, 0}), fun() ->
        ?assertEqual(
            {error, class_not_visible},
            moya_task_logic:list(?UID_OUTSIDER, undefined, {1, 10})
        )
    end).

list_with_own_group_ok_test_() ->
    RepoMock =
        {moya_task_repo, [
            {'active_staff_group_ids', 1, fun(_Uid) -> {ok, [?GROUP_A1, ?GROUP_B2]} end},
            {'list_tasks', 4, fun(_Uid, GroupIdOpt, Page, Size) ->
                self() ! {repo_list, GroupIdOpt, Page, Size},
                {ok, [list_row()]}
            end},
            {'count_tasks', 2, fun(_Uid, _GroupIdOpt) -> {ok, 1} end}
        ]},
    ?WITH_MECKS([RepoMock], fun() ->
        {ok, Payload} = moya_task_logic:list(?UID_TEACHER, ?GROUP_A1, {2, 10}),
        ?assertEqual(1, maps:get(<<"total">>, Payload)),
        ?assertEqual(2, maps:get(<<"page">>, Payload)),
        [Task] = maps:get(<<"list">>, Payload),
        ?assertEqual(integer_to_binary(?GROUP_A1), maps:get(<<"group_id">>, Task)),
        ?assertEqual(null, maps:get(<<"deadline">>, Task)),
        ?assertEqual(2, maps:get(<<"learner_count">>, Task)),
        receive
            {repo_list, ?GROUP_A1, 2, 10} -> ok
        after 0 -> ?assert(false, "repo list args wrong")
        end
    end).

list_all_staff_classes_ok_test_() ->
    RepoMock =
        {moya_task_repo, [
            {'active_staff_group_ids', 1, fun(_Uid) -> {ok, [?GROUP_A1]} end},
            {'list_tasks', 4, fun(_Uid, GroupIdOpt, _Page, _Size) ->
                self() ! {repo_list_all, GroupIdOpt},
                {ok, []}
            end},
            {'count_tasks', 2, fun(_Uid, _GroupIdOpt) -> {ok, 0} end}
        ]},
    ?WITH_MECKS([RepoMock], fun() ->
        {ok, Payload} = moya_task_logic:list(?UID_ASSIST, undefined, {1, 10}),
        ?assertEqual([], maps:get(<<"list">>, Payload)),
        ?assertEqual(0, maps:get(<<"total">>, Payload)),
        receive
            {repo_list_all, undefined} -> ok
        after 0 -> ?assert(false, "aggregate list must pass undefined group")
        end
    end).

list_deadline_rfc3339_kept_test_() ->
    Expected = <<"2099-01-01T00:00:00Z">>,
    RepoMock =
        {moya_task_repo, [
            {'active_staff_group_ids', 1, fun(_Uid) -> {ok, [?GROUP_A1]} end},
            {'list_tasks', 4, fun(_Uid, _GroupIdOpt, _P, _S) ->
                Base = list_row(),
                {ok, [Base#{<<"deadline">> => Expected}]}
            end},
            {'count_tasks', 2, fun(_Uid, _GroupIdOpt) -> {ok, 1} end}
        ]},
    ?WITH_MECKS([RepoMock], fun() ->
        {ok, Payload} = moya_task_logic:list(?UID_TEACHER, undefined, {1, 10}),
        [Task] = maps:get(<<"list">>, Payload),
        ?assertEqual(<<"2099-01-01T00:00:00Z">>, maps:get(<<"deadline">>, Task))
    end).

list_repo_error_test_() ->
    RepoMock =
        {moya_task_repo, [
            {'active_staff_group_ids', 1, fun(_Uid) -> {ok, [?GROUP_A1]} end},
            {'list_tasks', 4, fun(_Uid, _G, _P, _S) -> {error, boom} end},
            {'count_tasks', 2, fun(_Uid, _G) -> {ok, 0} end}
        ]},
    ?WITH_MECKS([RepoMock], fun() ->
        ?assertEqual(
            {error, db_error},
            moya_task_logic:list(?UID_TEACHER, undefined, {1, 10})
        )
    end).

%%%===================================================================
%%% Fixtures
%%%===================================================================

created_payload() ->
    %% v3 P0-1 修复后：ds payload task_id = integer_to_binary(Id)（十进制 string）
    #{
        <<"task_id">> => <<"975001000000000019">>,
        <<"assignments">> => [
            #{
                <<"assignment_id">> => <<"976001">>,
                <<"learner_id">> => integer_to_binary(?LEARNER_A1)
            }
        ],
        <<"replayed">> => false
    }.
