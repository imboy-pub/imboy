-module(teaching_acl).
%%%
% 墨芽教学域集中式 ACL（deny-by-default）
% Centralized teaching authorization guard
%
% 设计（计划 §5 / STEP-04 threat-model.md T1–T12）：
%   - 一切教学资源权限在 Logic 层集中解析：JWT uid + DB 关系链，绝不信任
%     客户端自报的 organization_id / workspace_id / learner_id / reviewer_uid
%   - 所有 resolve_* 查不到关系、关系 status != active、机构解析为 NULL
%     → 一律 {error, Reason}（fail closed）
%   - Organization Owner 身份不授予儿童资源（§5.2）；仅 Group 管理员
%     （无 class_staff 行）同样拒绝（D-07）
%
% 行键约定：repo 行 map 键为 binary（epgsql column.name）。
% Reason 原子 → HTTP 错误码映射见 teaching_context_handler。
%%%

-export([
    resolve_guardian/2,
    resolve_guardian/3,
    resolve_staff/2,
    resolve_staff/3,
    resolve_org_owner/2,
    assert_same_org/2,
    submission_access/2
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 教学写权限角色（manager/teacher 可写回评；assistant 只读，Step 9 用）
-define(STAFF_WRITE_ROLES, [manager, teacher]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 监护人关系解析（active 才有效）
-spec resolve_guardian(integer(), integer()) ->
    {ok, map()} | {error, not_guardian | inactive | cannot_submit | cannot_view | db_error}.
resolve_guardian(Uid, LearnerId) ->
    resolve_guardian(Uid, LearnerId, undefined).

%% @doc 监护人关系解析 + 细分权限：
%% Need = submit（要求 can_submit=true）| view_review（要求 can_view_review=true）| undefined
-spec resolve_guardian(integer(), integer(), submit | view_review | undefined) ->
    {ok, map()}
    | {error, not_guardian | inactive | cannot_submit | cannot_view | db_error}.
resolve_guardian(Uid, LearnerId, Need) ->
    case teaching_context_repo:guardian_relation(Uid, LearnerId) of
        {ok, undefined} ->
            {error, not_guardian};
        {ok, #{<<"status">> := <<"active">>} = Row} ->
            check_guardian_scope(Need, Row);
        {ok, _InactiveRow} ->
            {error, inactive};
        {error, Reason} ->
            ?LOG_ERROR("teaching_acl resolve_guardian db error ~p", [Reason]),
            {error, db_error}
    end.

%% @doc 任课老师关系解析（active 才有效；role 原样返回供细分）
-spec resolve_staff(integer(), integer()) ->
    {ok, map()} | {error, not_staff | inactive | role_denied | db_error}.
resolve_staff(Uid, GroupId) ->
    resolve_staff(Uid, GroupId, undefined).

%% @doc 任课老师关系解析 + 角色白名单：
%% Need = write（manager/teacher）| {roles, Roles} | undefined
%% 仅 Group 管理员（无 class_staff 行）在这里必然 not_staff（T4）
-spec resolve_staff(integer(), integer(), write | {roles, [atom()]} | undefined) ->
    {ok, map()} | {error, not_staff | inactive | role_denied | db_error}.
resolve_staff(Uid, GroupId, Need) ->
    case teaching_context_repo:staff_relation(Uid, GroupId) of
        {ok, undefined} ->
            {error, not_staff};
        {ok, #{<<"status">> := <<"active">>} = Row} ->
            check_staff_scope(Need, Row);
        {ok, _InactiveRow} ->
            {error, inactive};
        {error, Reason} ->
            ?LOG_ERROR("teaching_acl resolve_staff db error ~p", [Reason]),
            {error, db_error}
    end.

%% @doc 机构 Owner 解析（organization.owner_id == Uid）
%% Owner 身份本身不授予儿童资源（T5）；本函数只用于机构管理类判断
-spec resolve_org_owner(integer(), integer()) -> ok | {error, not_owner | db_error}.
resolve_org_owner(Uid, OrgId) ->
    case teaching_context_repo:org_owner_uid(OrgId) of
        {ok, Owner} when is_integer(Owner), Owner =:= Uid ->
            ok;
        {ok, _} ->
            {error, not_owner};
        {error, Reason} ->
            ?LOG_ERROR("teaching_acl resolve_org_owner db error ~p", [Reason]),
            {error, db_error}
    end.

%% @doc 机构一致性守卫（T1/T2：跨 Organization 访问拒绝）
-spec assert_same_org(integer() | undefined, integer() | undefined) ->
    ok | {error, cross_org}.
assert_same_org(OrgId, OrgId) when is_integer(OrgId) ->
    ok;
assert_same_org(_, _) ->
    {error, cross_org}.

%% @doc 儿童提交（视频）访问守卫 —— ACL-01 核心。
%% 判定顺序（先 staff 后 guardian；同一人兼具两身份返回 staff 超集视角）：
%%   1. submission 资源链解析失败 / 机构为 NULL → not_found（不确认存在性，T14）
%%   2. staff 路径：本班 active class_staff，且班级机构 == 资源机构（T1 双保险）
%%   3. guardian 路径：can_view_review=true 的 active 监护人，且学员机构 == 资源机构
%%   4. Org Owner 不采纳（owner_not_granted，T5；与 forbidden 同响应码，便于测试区分）
%%   5. 其余 → forbidden
-spec submission_access(integer(), integer()) ->
    {ok, staff | guardian, map()}
    | {error, not_found | forbidden | owner_not_granted | db_error}.
submission_access(Uid, SubmissionId) ->
    case teaching_context_repo:submission_scope(SubmissionId) of
        {ok, undefined} ->
            {error, not_found};
        {ok,
            #{<<"org_id">> := OrgId, <<"learner_id">> := LearnerId, <<"group_id">> := GroupId} =
                Scope} when
            is_integer(OrgId), is_integer(LearnerId), is_integer(GroupId)
        ->
            submission_access_dispatch(Uid, OrgId, GroupId, LearnerId, Scope);
        {ok, _NoOrgScope} ->
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("teaching_acl submission_scope db error ~p", [Reason]),
            {error, db_error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_guardian_scope(submit | view_review | undefined, map()) ->
    {ok, map()} | {error, cannot_submit | cannot_view}.
check_guardian_scope(submit, #{<<"can_submit">> := true} = Row) ->
    {ok, Row};
check_guardian_scope(submit, _) ->
    {error, cannot_submit};
check_guardian_scope(view_review, #{<<"can_view_review">> := true} = Row) ->
    {ok, Row};
check_guardian_scope(view_review, _) ->
    {error, cannot_view};
check_guardian_scope(undefined, Row) ->
    {ok, Row}.

-spec check_staff_scope(write | {roles, [atom()]} | undefined, map()) ->
    {ok, map()} | {error, role_denied}.
check_staff_scope(write, #{<<"role">> := Role} = Row) ->
    case lists:member(role_atom(Role), ?STAFF_WRITE_ROLES) of
        true -> {ok, Row};
        false -> {error, role_denied}
    end;
check_staff_scope({roles, Roles}, #{<<"role">> := Role} = Row) ->
    case lists:member(role_atom(Role), Roles) of
        true -> {ok, Row};
        false -> {error, role_denied}
    end;
check_staff_scope(undefined, Row) ->
    {ok, Row}.

-spec submission_access_dispatch(integer(), integer(), integer(), integer(), map()) ->
    {ok, staff | guardian, map()} | {error, forbidden | owner_not_granted}.
submission_access_dispatch(Uid, OrgId, GroupId, LearnerId, Scope) ->
    case resolve_staff(Uid, GroupId) of
        {ok, Staff} ->
            %% 跨机构双保险：staff 班级机构必须与资源机构一致（T1）
            case teaching_context_repo:group_org(GroupId) of
                {ok, StaffOrg} ->
                    case assert_same_org(OrgId, StaffOrg) of
                        ok -> {ok, staff, Scope#{staff => Staff}};
                        {error, cross_org} -> {error, forbidden}
                    end;
                _ ->
                    {error, forbidden}
            end;
        _ ->
            case resolve_guardian(Uid, LearnerId, view_review) of
                {ok, Guardian} ->
                    %% 学员机构必须与资源机构一致（T1/T3）
                    case teaching_context_repo:learner_org(LearnerId) of
                        {ok, LearnerOrg} ->
                            case assert_same_org(OrgId, LearnerOrg) of
                                ok -> {ok, guardian, Scope#{guardian => Guardian}};
                                {error, cross_org} -> {error, forbidden}
                            end;
                        _ ->
                            {error, forbidden}
                    end;
                _ ->
                    %% Owner 不授予儿童资源（T5）；最后兜底 forbidden
                    owner_or_forbidden(Uid, OrgId)
            end
    end.

-spec owner_or_forbidden(integer(), integer()) ->
    {error, owner_not_granted | forbidden}.
owner_or_forbidden(Uid, OrgId) ->
    case resolve_org_owner(Uid, OrgId) of
        ok -> {error, owner_not_granted};
        _ -> {error, forbidden}
    end.

-spec role_atom(binary()) -> atom().
role_atom(<<"manager">>) -> manager;
role_atom(<<"teacher">>) -> teacher;
role_atom(<<"assistant">>) -> assistant;
role_atom(_) -> unknown.
