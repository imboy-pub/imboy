%% teaching_acl_tests
%% ACL-01：跨 Organization / 跨 learner / 仅 Group 管理员（非 class_staff）/
%%         仅 Organization Owner 访问儿童提交（视频承载资源）→ 全部拒绝；
%%         合法 staff / guardian 放行。
%% ACL-02：多身份用户（guardian + teacher）上下文组装、switch 归属校验、
%%         跨班不串权限、TSID 输出一律字符串。
%% ACL 数据全部 meck teaching_context_repo（SQL 正确性由 STEP-08 behavior SQL
%% 在 scratch 库 moya_mig_test 上单独验证，见 STEP-08/commands.md）。

-module(teaching_acl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ---- 夹具（与 STEP-08 behavior SQL 同构）----
-define(ORG_A, 100).
-define(ORG_B, 200).
-define(GROUP_A1, 1001).
-define(GROUP_A2, 1002).
-define(GROUP_B1, 2001).
-define(LEARNER_A1, 3001).
-define(LEARNER_A2, 3002).
-define(LEARNER_B1, 3003).
-define(TEACHER_A, 5001).
-define(GUARDIAN_B, 5002).
%% L2 的 can_view_review=false 监护人
-define(GUARDIAN_C, 5003).
%% ORG_A Owner，非 staff 非 guardian
-define(OWNER_U, 5004).
%% 仅群管理员，无 class_staff 行
-define(GROUP_ADMIN, 5005).
%% G2 老师 + L1 监护人（多身份）
-define(MULTI_U, 5006).
%% G1 assistant（只读）
-define(ASSISTANT, 5007).
%% L1 在 G1 的提交（org A）
-define(SUB_A1, 6001).
%% L2 在 G1 的提交（org A）
-define(SUB_A2, 6002).
%% L3 在 G3(B1) 的提交（org B）
-define(SUB_B1, 6003).

%%%===================================================================
%%% ACL-01 矩阵
%%%===================================================================

%% 跨 Organization（T1）：A 机构老师访问 B 机构提交 → forbidden
acl01_cross_org_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertEqual(
            {error, forbidden},
            teaching_acl:submission_access(?TEACHER_A, ?SUB_B1)
        )
    end).

%% 跨 learner（T3）：L2 监护人访问 L1 的提交 → forbidden
acl01_cross_learner_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertEqual(
            {error, forbidden},
            teaching_acl:submission_access(?GUARDIAN_C, ?SUB_A1)
        )
    end).

%% 监护人 can_view_review=false（T3 变体）：即便绑定学员也不可读回评资源
acl01_guardian_without_view_right_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertEqual(
            {error, forbidden},
            teaching_acl:submission_access(?GUARDIAN_C, ?SUB_A2)
        )
    end).

%% 仅 Group 管理员（T4）：无 class_staff 行 → forbidden
acl01_group_admin_only_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertEqual(
            {error, forbidden},
            teaching_acl:submission_access(?GROUP_ADMIN, ?SUB_A1)
        )
    end).

%% 仅 Organization Owner（T5）：owner_not_granted（与 forbidden 同响应码语义）
acl01_org_owner_only_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertEqual(
            {error, owner_not_granted},
            teaching_acl:submission_access(?OWNER_U, ?SUB_A1)
        )
    end).

%% 合法放行：本班老师 / can_view_review 监护人
acl01_positive_staff_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertMatch(
            {ok, staff, _},
            teaching_acl:submission_access(?TEACHER_A, ?SUB_A1)
        )
    end).

acl01_positive_guardian_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertMatch(
            {ok, guardian, _},
            teaching_acl:submission_access(?GUARDIAN_B, ?SUB_A1)
        )
    end).

%% 资源不存在 / 非教学链（org NULL）→ not_found（不确认存在性，T14）
acl01_not_found_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertEqual(
            {error, not_found},
            teaching_acl:submission_access(?TEACHER_A, 999999)
        )
    end).

%%%===================================================================
%%% ACL-02：多身份用户
%%%===================================================================

%% contexts 返回 guardian + teacher 双身份；TSID 一律字符串
acl02_contexts_multi_role_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        {ok, #{contexts := Ctxs}} = teaching_context_logic:contexts(?MULTI_U),
        Types = [maps:get(<<"context_type">>, C) || C <- Ctxs],
        ?assertEqual(true, lists:member(<<"guardian">>, Types)),
        ?assertEqual(true, lists:member(<<"teacher">>, Types)),
        %% API-01：所有 64-bit ID 为 binary 字符串
        lists:foreach(
            fun(C) ->
                L = maps:get(<<"learner_id">>, C, undefined),
                G = maps:get(<<"group_id">>, C, undefined),
                [?assert(is_binary(V)) || V <- [L, G], V =/= undefined, V =/= <<>>]
            end,
            Ctxs
        ),
        GuardianCtx = hd([C || C <- Ctxs, maps:get(<<"context_type">>, C) =:= <<"guardian">>]),
        ?assertEqual(<<"3001">>, maps:get(<<"learner_id">>, GuardianCtx))
    end).

%% switch 只放行本人身份；G2 老师身份不能切换到 G1（teacher 分支）
acl02_switch_scoped_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        %% 本人 teacher 上下文（G2）→ ok
        ?assertMatch(
            {ok, _},
            teaching_context_logic:switch(?MULTI_U, #{
                <<"context_type">> => <<"teacher">>, <<"group_id">> => <<"1002">>
            })
        ),
        %% 非本人 teacher 上下文（G1：无 staff 行）→ context_mismatch
        ?assertEqual(
            {error, context_mismatch},
            teaching_context_logic:switch(?MULTI_U, #{
                <<"context_type">> => <<"teacher">>, <<"group_id">> => <<"1001">>
            })
        ),
        %% 本人 guardian 上下文 → ok
        ?assertMatch(
            {ok, _},
            teaching_context_logic:switch(?MULTI_U, #{
                <<"context_type">> => <<"guardian">>, <<"learner_id">> => <<"3001">>
            })
        )
    end).

%% 客户端自报 organization_id 与服务端解析不一致 → 拒绝（T12）
acl02_switch_claim_mismatch_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertEqual(
            {error, context_mismatch},
            teaching_context_logic:switch(?MULTI_U, #{
                <<"context_type">> => <<"guardian">>,
                <<"learner_id">> => <<"3001">>,
                <<"organization_id">> => <<"200">>
            })
        )
    end).

%% 多身份不串资源：G2 老师（MULTI_U）访问 G1 提交走 guardian 路径而非 staff
acl02_no_context_bleed_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        %% staff_relation(5006, 1001) 无行 → 不会因 G2 任教身份获得 G1 staff 视角
        ?assertMatch(
            {ok, guardian, _},
            teaching_acl:submission_access(?MULTI_U, ?SUB_A1)
        )
    end).

%%%===================================================================
%%% assistant 只读 / 写权限细分（TEACHER-02 前置）
%%%===================================================================

assistant_write_denied_test_() ->
    ?WITH_MECKS([{teaching_context_repo, repo_mocks()}], fun() ->
        ?assertMatch(
            {ok, _},
            teaching_acl:resolve_staff(?ASSISTANT, ?GROUP_A1)
        ),
        ?assertEqual(
            {error, role_denied},
            teaching_acl:resolve_staff(?ASSISTANT, ?GROUP_A1, write)
        ),
        ?assertEqual(
            {error, role_denied},
            teaching_acl:resolve_staff(?ASSISTANT, ?GROUP_A1, {roles, [manager, teacher]})
        )
    end).

%%%===================================================================
%%% Repo mocks（与 scratch 库 fixture 同构）
%%%===================================================================

repo_mocks() ->
    [
        {'guardian_relation', 2, fun
            (?GUARDIAN_B, ?LEARNER_A1) -> {ok, g_row(true, true)};
            (?GUARDIAN_C, ?LEARNER_A2) -> {ok, g_row(true, false)};
            (?MULTI_U, ?LEARNER_A1) -> {ok, g_row(true, true)};
            (_, _) -> {ok, undefined}
        end},
        {'staff_relation', 2, fun
            (?TEACHER_A, ?GROUP_A1) -> {ok, s_row(<<"teacher">>)};
            (?MULTI_U, ?GROUP_A2) -> {ok, s_row(<<"manager">>)};
            (?ASSISTANT, ?GROUP_A1) -> {ok, s_row(<<"assistant">>)};
            (_, _) -> {ok, undefined}
        end},
        {'org_owner_uid', 1, fun
            (?ORG_A) -> {ok, ?OWNER_U};
            (?ORG_B) -> {ok, 9999};
            (_) -> {ok, undefined}
        end},
        {'learner_org', 1, fun
            (?LEARNER_A1) -> {ok, ?ORG_A};
            (?LEARNER_A2) -> {ok, ?ORG_A};
            (?LEARNER_B1) -> {ok, ?ORG_B};
            (_) -> {ok, undefined}
        end},
        {'group_org', 1, fun
            (?GROUP_A1) -> {ok, ?ORG_A};
            (?GROUP_A2) -> {ok, ?ORG_A};
            (?GROUP_B1) -> {ok, ?ORG_B};
            (_) -> {ok, undefined}
        end},
        {'submission_scope', 1, fun
            (?SUB_A1) -> {ok, sub_row(?SUB_A1, ?LEARNER_A1, ?GROUP_A1, ?ORG_A)};
            (?SUB_A2) -> {ok, sub_row(?SUB_A2, ?LEARNER_A2, ?GROUP_A1, ?ORG_A)};
            (?SUB_B1) -> {ok, sub_row(?SUB_B1, ?LEARNER_B1, ?GROUP_B1, ?ORG_B)};
            (_) -> {ok, undefined}
        end},
        {'guardian_contexts', 1, fun
            (?MULTI_U) ->
                {ok, [
                    #{
                        <<"learner_id">> => ?LEARNER_A1,
                        <<"can_submit">> => true,
                        <<"can_view_review">> => true,
                        <<"relation">> => <<"guardian">>,
                        <<"display_name">> => <<"大宝"/utf8>>,
                        <<"organization_id">> => ?ORG_A,
                        <<"group_id">> => ?GROUP_A1,
                        <<"group_title">> => <<"硬笔一班"/utf8>>,
                        <<"workspace_id">> => 101,
                        <<"workspace_name">> => <<"A校区"/utf8>>,
                        <<"org_id">> => ?ORG_A,
                        <<"org_name">> => <<"机构A"/utf8>>
                    }
                ]};
            (_) ->
                {ok, []}
        end},
        {'staff_contexts', 1, fun
            (?MULTI_U) ->
                {ok, [
                    #{
                        <<"group_id">> => ?GROUP_A2,
                        <<"role">> => <<"manager">>,
                        <<"group_title">> => <<"硬笔二班"/utf8>>,
                        <<"workspace_id">> => 101,
                        <<"workspace_name">> => <<"A校区"/utf8>>,
                        <<"org_id">> => ?ORG_A,
                        <<"org_name">> => <<"机构A"/utf8>>
                    }
                ]};
            (_) ->
                {ok, []}
        end},
        {'owner_contexts', 1, fun(_) -> {ok, []} end}
    ].

g_row(CanSubmit, CanView) ->
    %% v3 H2：guardian_relation SQL 现带 learner_status（LEFT JOIN learner）
    #{
        <<"guardian_uid">> => 0,
        <<"learner_id">> => 0,
        <<"relation">> => <<"guardian">>,
        <<"can_submit">> => CanSubmit,
        <<"can_view_review">> => CanView,
        <<"status">> => <<"active">>,
        <<"learner_status">> => <<"active">>
    }.

s_row(Role) ->
    #{<<"group_id">> => 0, <<"user_id">> => 0, <<"role">> => Role, <<"status">> => <<"active">>}.

sub_row(Id, LearnerId, GroupId, OrgId) ->
    #{
        <<"submission_id">> => Id,
        <<"assignment_id">> => 7000 + Id rem 100,
        <<"learner_id">> => LearnerId,
        <<"submission_status">> => <<"submitted">>,
        <<"attempt_no">> => 1,
        <<"task_id">> => <<"taskhash">>,
        <<"assignment_learner_id">> => LearnerId,
        <<"group_id">> => GroupId,
        <<"org_id">> => OrgId
    }.
