-module(moya_context_repo).
-moduledoc "墨芽教学上下文与 ACL 数据仓库模块。".
%%%
% 墨芽教学上下文与 ACL 数据仓库模块
% Teaching context & ACL repository
%
% 职责（Step 8）：
%   - 教学 SQL 唯一入口：guardian/staff/organization 上下文解析、ACL 关系查询、
%     submission 资源链解析（learner→assignment→task→group→workspace→organization）
%   - 只读；不做任何权限判断（判断集中在 moya_acl logic）
%
% deny-by-default 约定：
%   - group_org/1 解析不到 organization（workspace 未挂机构）= {error, not_found}
%   - 所有关系查询只认 status='active'（removed/archived 行不可见）
%%%

-export([tablename/1]).
-export([guardian_contexts/1, staff_contexts/1, organization_contexts/1, owner_contexts/1]).
-export([guardian_relation/2, staff_relation/2, org_owner_uid/1]).
-export([learner_org/1, group_org/1]).
-export([submission_scope/1, assignment_scope/1]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec tablename(binary()) -> binary().
tablename(Tb) ->
    elib_pg_sql:public_tablename(Tb).

%% @doc 监护人上下文列表：guardian_learner(active) + learner(active)
%% LEFT JOIN class_enrollment(active)→group→workspace→organization 取班级/机构信息。
%% 未入班的学员仍是合法上下文（家长可先看到孩子，再等入班）。
-spec guardian_contexts(integer()) -> {ok, [map()]} | {error, term()}.
guardian_contexts(Uid) ->
    Sql =
        <<
            "SELECT gl.learner_id, gl.can_submit, gl.can_view_review, gl.relation, "
            "l.display_name, l.organization_id, "
            "e.group_id, g.title AS group_title, "
            "w.id AS workspace_id, w.name AS workspace_name, "
            "o.id AS org_id, o.name AS org_name "
            "FROM ",
            (tb(guardian_learner))/binary,
            " gl "
            "JOIN ",
            (tb(learner))/binary,
            " l ON l.id = gl.learner_id AND l.status = 'active' "
            "LEFT JOIN ",
            (tb(class_enrollment))/binary,
            " e "
            " ON e.learner_id = gl.learner_id AND e.status = 'active' "
            "LEFT JOIN ",
            (tb(group))/binary,
            " g ON g.id = e.group_id "
            "LEFT JOIN ",
            (tb(workspace))/binary,
            " w ON w.id = g.workspace_id "
            "LEFT JOIN ",
            (tb(organization))/binary,
            " o ON o.id = w.organization_id "
            "WHERE gl.guardian_uid = $1 AND gl.status = 'active' "
            "ORDER BY gl.learner_id, e.group_id"
        >>,
    elib_pg:query(Sql, [Uid]).

%% @doc 老师上下文列表：class_staff(active) + 同机构班级群（workspace.organization_id 非空）。
%% 机构解析为 NULL 的群不构成教学上下文（deny-by-default，DB-ORG-03 同源语义）。
-spec staff_contexts(integer()) -> {ok, [map()]} | {error, term()}.
staff_contexts(Uid) ->
    Sql =
        <<
            "SELECT cs.group_id, cs.role, g.title AS group_title, "
            "w.id AS workspace_id, w.name AS workspace_name, "
            "o.id AS org_id, o.name AS org_name "
            "FROM ",
            (tb(class_staff))/binary,
            " cs "
            "JOIN ",
            (tb(group))/binary,
            " g ON g.id = cs.group_id "
            "JOIN ",
            (tb(workspace))/binary,
            " w ON w.id = g.workspace_id "
            "JOIN ",
            (tb(organization))/binary,
            " o ON o.id = w.organization_id "
            "WHERE cs.user_id = $1 AND cs.status = 'active' "
            "ORDER BY cs.group_id"
        >>,
    elib_pg:query(Sql, [Uid]).

%% @doc 机构治理上下文列表。owner/admin 只获得机构管理入口，不获得儿童资源权限。
-spec organization_contexts(integer()) -> {ok, [map()]} | {error, term()}.
organization_contexts(Uid) ->
    Sql =
        <<
            "SELECT o.id AS org_id, o.name AS org_name, om.role "
            "FROM ",
            (tb(organization_member))/binary,
            " om "
            "JOIN ",
            (tb(organization))/binary,
            " o ON o.id = om.organization_id "
            "WHERE om.user_id = $1 AND om.status = 'active' "
            "AND om.role IN ('owner', 'admin') AND o.status = 'active' ORDER BY o.id"
        >>,
    elib_pg:query(Sql, [Uid]).

%% 兼容旧客户端/BEAM：保留原 owner_id 查询语义，不返回 admin。
-spec owner_contexts(integer()) -> {ok, [map()]} | {error, term()}.
owner_contexts(Uid) ->
    Sql =
        <<"SELECT id AS org_id, name AS org_name FROM ", (tb(organization))/binary,
            " WHERE owner_id = $1 AND status = 'active' ORDER BY id">>,
    elib_pg:query(Sql, [Uid]).

%% @doc 单条监护关系（含 status，供 ACL 区分 not_found 与 inactive）
-spec guardian_relation(integer(), integer()) -> {ok, map() | undefined} | {error, term()}.
guardian_relation(Uid, LearnerId) ->
    Sql =
        <<
            "SELECT gl.guardian_uid, gl.learner_id, gl.relation, gl.can_submit, "
            "gl.can_view_review, gl.status, l.status AS learner_status "
            "FROM ",
            (tb(guardian_learner))/binary,
            " gl LEFT JOIN ",
            (tb(learner))/binary,
            " l ON l.id = gl.learner_id "
            "WHERE gl.guardian_uid = $1 AND gl.learner_id = $2 LIMIT 1"
        >>,
    case elib_pg:query(Sql, [Uid, LearnerId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 单条任教关系（含 status 与 role）
-spec staff_relation(integer(), integer()) -> {ok, map() | undefined} | {error, term()}.
staff_relation(Uid, GroupId) ->
    Sql =
        <<"SELECT group_id, user_id, role, status FROM ", (tb(class_staff))/binary,
            " WHERE user_id = $1 AND group_id = $2 LIMIT 1">>,
    case elib_pg:query(Sql, [Uid, GroupId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 机构 Owner uid
-spec org_owner_uid(integer()) -> {ok, integer() | undefined} | {error, term()}.
org_owner_uid(OrgId) ->
    Sql = <<"SELECT owner_id FROM ", (tb(organization))/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [OrgId]) of
        {ok, [#{<<"owner_id">> := Owner} | _]} -> {ok, Owner};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc learner → organization（learner 行不存在 = not_found）
-spec learner_org(integer()) -> {ok, integer() | undefined} | {error, term()}.
learner_org(LearnerId) ->
    Sql = <<"SELECT organization_id FROM ", (tb(learner))/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [LearnerId]) of
        {ok, [#{<<"organization_id">> := OrgId} | _]} -> {ok, OrgId};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc group → workspace → organization；机构解析为 NULL（未挂机构的群）按 not_found 处理
-spec group_org(integer()) -> {ok, integer() | undefined} | {error, term()}.
group_org(GroupId) ->
    Sql =
        <<
            "SELECT w.organization_id AS org_id "
            "FROM ",
            (tb(group))/binary,
            " g "
            "JOIN ",
            (tb(workspace))/binary,
            " w ON w.id = g.workspace_id "
            "WHERE g.id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Sql, [GroupId]) of
        {ok, [#{<<"org_id">> := OrgId} | _]} ->
            case OrgId of
                null -> {ok, undefined};
                _ -> {ok, OrgId}
            end;
        {ok, []} ->
            {ok, undefined};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc submission 资源链：submission→assignment→task→group→workspace→organization
%% 供 ACL/业务共用；任一跳缺失即 not_found（deny-by-default）。
%% 注意 assignment.task_id 是 HashID varchar（引用 group_task.task_id）。
-spec submission_scope(integer()) -> {ok, map() | undefined} | {error, term()}.
submission_scope(SubmissionId) ->
    Sql =
        <<
            "SELECT hs.id AS submission_id, hs.assignment_id, hs.learner_id, "
            "hs.status AS submission_status, hs.attempt_no, "
            "a.task_id, a.learner_id AS assignment_learner_id, "
            "gt.group_id, w.organization_id AS org_id "
            "FROM ",
            (tb(homework_submission))/binary,
            " hs "
            "JOIN ",
            (tb(group_task_assignment))/binary,
            " a ON a.id = hs.assignment_id "
            "JOIN ",
            (tb(group_task))/binary,
            " gt ON gt.task_id = a.task_id "
            "JOIN ",
            (tb(group))/binary,
            " g ON g.id = gt.group_id "
            "JOIN ",
            (tb(workspace))/binary,
            " w ON w.id = g.workspace_id "
            "WHERE hs.id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Sql, [SubmissionId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc assignment 资源链：assignment→task→group→workspace→organization（Step 9 用）
%% 含 gt.deadline / gt.title（提交开放判定 5442 与详情回显；group_task 先例：
%% 开放 = task.status =/= 3 且 deadline 未过，见 group_task_logic:check_deadline/1）
-spec assignment_scope(integer()) -> {ok, map() | undefined} | {error, term()}.
assignment_scope(AssignmentId) ->
    Sql =
        <<
            "SELECT a.id AS assignment_id, a.task_id, a.learner_id, a.user_id AS assignee_uid, "
            "a.status AS assignment_status, gt.id AS task_gid, "
            "gt.group_id, gt.status AS task_status, gt.title AS task_title, "
            "gt.deadline AS task_deadline, g.title AS group_title, "
            "w.organization_id AS org_id "
            "FROM ",
            (tb(group_task_assignment))/binary,
            " a "
            "JOIN ",
            (tb(group_task))/binary,
            " gt ON gt.task_id = a.task_id "
            "JOIN ",
            (tb(group))/binary,
            " g ON g.id = gt.group_id "
            "JOIN ",
            (tb(workspace))/binary,
            " w ON w.id = g.workspace_id "
            "WHERE a.id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Sql, [AssignmentId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec tb(atom()) -> binary().
%% GROUP 是保留字：只引末段 → public."group"（整段加引号 → 42P01）。
tb(group) ->
    elib_pg_sql:public_tablename_quoted(<<"group">>);
tb(Tb) ->
    tablename(ec_cnv:to_binary(Tb)).
