-module(enterprise_directory_member_repo).
-moduledoc "Source-owned member state and bounded offboarding counts; no account or role changes.".
-export([target_tx/4, blockers_tx/3, status_tx/6, remember_tx/5]).

target_tx(C, App, Org, External) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT s.external_user_id,s.managed,s.status AS membership_status,"
                " s.suspension_member_version,p.user_id,p.status AS source_status,p.binding_retired,"
                " s.projected_user_id,m.role,m.status AS member_status,"
                " COALESCE(u.status=1 AND u.account_type=0,false) AS valid_user,"
                " (extract(epoch FROM m.updated_at)*1000000)::bigint AS member_version"
                " FROM enterprise_directory_membership_source s"
                " JOIN enterprise_source_person p USING(application_id,external_user_id)"
                " LEFT JOIN \"user\" u ON u.id=p.user_id"
                " LEFT JOIN organization_member m ON m.organization_id=s.organization_id AND m.user_id=p.user_id"
                " WHERE s.application_id=$1 AND s.organization_id=$2 AND s.external_user_id=$3"
            >>,
            [App, Org, External]
        )
    of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, resource_not_found};
        {error, _} -> {error, internal_error}
    end.

blockers_tx(C, Org, User) ->
    %% Mirror removal dependencies without returning names, task bodies or an
    %% unbounded list. Advisory only: the actual removal rechecks under locks.
    case
        elib_pg:query(
            C,
            <<
                "WITH ws AS (SELECT w.id,w.owner_id FROM workspace w JOIN workspace_member wm"
                " ON wm.workspace_id=w.id WHERE w.organization_id=$1 AND wm.user_id=$2 AND wm.status='active')"
                " SELECT (SELECT count(*) FROM ws WHERE owner_id=$2) AS owned_workspaces,"
                " (SELECT count(*) FROM project p JOIN ws ON ws.id=p.workspace_id WHERE p.owner_id=$2) AS "
                "owned_projects,"
                " (SELECT count(*) FROM project_task t JOIN project p ON p.id=t.project_id"
                " JOIN ws ON ws.id=p.workspace_id WHERE t.assignee_id=$2 AND t.status<>'done') AS unfinished_tasks,"
                " (SELECT count(*) FROM \"group\" g JOIN ws ON ws.id=g.workspace_id"
                " WHERE g.scope='workspace' AND g.owner_uid=$2 AND g.status=1) AS owned_groups,"
                " (SELECT count(*) FROM channel ch JOIN ws ON ws.id=ch.workspace_id"
                " WHERE ch.scope='workspace' AND ch.creator_uid=$2 AND ch.status=1) AS owned_channels, "
                " (SELECT count(*) FROM organization_business_identity_assignment a"
                " WHERE a.organization_id=$1 AND a.user_id=$2 AND a.status='active') AS active_assignments"
            >>,
            [Org, User]
        )
    of
        {ok, [Row]} -> {ok, Row};
        _ -> {error, internal_error}
    end.

status_tx(C, Org, User, From, To, Version) ->
    case
        elib_pg:query(
            C,
            <<
                "UPDATE organization_member SET status=$4,updated_at=clock_timestamp()"
                " WHERE organization_id=$1 AND user_id=$2 AND status=$3 AND role='member'"
                " AND (extract(epoch FROM updated_at)*1000000)::bigint=$5"
                " RETURNING (extract(epoch FROM updated_at)*1000000)::bigint AS member_version"
            >>,
            [Org, User, From, To, Version]
        )
    of
        {ok, [#{<<"member_version">> := New}]} -> {ok, New};
        {ok, []} -> {error, version_conflict};
        {error, _} -> {error, internal_error}
    end.

remember_tx(C, App, Org, External, Version) ->
    case
        elib_pg:execute(
            C,
            <<
                "UPDATE enterprise_directory_membership_source SET suspension_member_version=$4"
                " WHERE application_id=$1 AND organization_id=$2 AND external_user_id=$3"
            >>,
            [App, Org, External, Version]
        )
    of
        {ok, 1} -> ok;
        _ -> {error, internal_error}
    end.
