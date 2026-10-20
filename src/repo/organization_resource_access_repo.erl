-module(organization_resource_access_repo).
-moduledoc "Live parent eligibility for Human workspace resources; no membership mutation or cached decisions.".
-export([
    workspace_predicate_sql/2,
    resource_predicate_sql/3,
    group_predicate_sql/2,
    workspace_parent_allowed/2,
    channel_predicate_sql/2,
    channel_allowed/2
]).

%% Expressions are fixed identifiers/placeholders from source, never user input.
%% Match the existing attachment policy: personal workspaces keep their own
%% membership model; organization resources require a live organization member.
%% Archived workspaces remain readable. Normal resource roles still apply.
workspace_predicate_sql(WorkspaceExpr, UidExpr) ->
    <<
        "EXISTS (SELECT 1 FROM public.workspace ra_ws "
        "LEFT JOIN public.organization ra_org ON ra_org.id=ra_ws.organization_id "
        "LEFT JOIN public.organization_member ra_om ON ra_om.organization_id=ra_org.id "
        "AND ra_om.user_id=",
        UidExpr/binary,
        " AND ra_om.status='active' "
        "WHERE ra_ws.id=",
        WorkspaceExpr/binary,
        " AND ra_ws.status IN ('active','archived') "
        "AND (ra_ws.organization_id IS NULL OR (ra_org.status='active' AND ra_om.user_id=",
        UidExpr/binary,
        ")) AND (EXISTS (SELECT 1 FROM public.workspace_member ra_wm "
        "WHERE ra_wm.workspace_id=ra_ws.id AND ra_wm.user_id=",
        UidExpr/binary,
        " AND ra_wm.status='active') OR ra_om.role IN ('owner','admin')))"
    >>.

resource_predicate_sql(ScopeExpr, WorkspaceExpr, UidExpr) ->
    Workspace = workspace_predicate_sql(WorkspaceExpr, UidExpr),
    <<"((", ScopeExpr/binary, "='personal' AND ", WorkspaceExpr/binary, " IS NULL) OR (",
        ScopeExpr/binary, "='workspace' AND ", Workspace/binary, "))">>.

group_predicate_sql(GroupExpr, UidExpr) ->
    Resource = resource_predicate_sql(<<"ra_group.scope">>, <<"ra_group.workspace_id">>, UidExpr),
    <<"EXISTS (SELECT 1 FROM public.\"group\" ra_group WHERE ra_group.id=", GroupExpr/binary,
        " AND ", Resource/binary, ")">>.

%% Join codes may add workspace membership, so they check the parent without
%% requiring a pre-existing workspace role. Existing relations stay untouched.
workspace_parent_allowed(WsId, Uid) ->
    Sql = <<
        "SELECT EXISTS(SELECT 1 FROM public.workspace w "
        "LEFT JOIN public.organization o ON o.id=w.organization_id "
        "LEFT JOIN public.organization_member m ON m.organization_id=o.id "
        "AND m.user_id=$2 AND m.status='active' WHERE w.id=$1 "
        "AND (w.organization_id IS NULL OR (o.status='active' AND m.user_id=$2))) AS allowed"
    >>,
    case elib_pg:one(Sql, [WsId, Uid]) of
        {ok, #{<<"allowed">> := true}} -> true;
        _ -> false
    end.

channel_predicate_sql(ChannelExpr, UidExpr) ->
    Resource = resource_predicate_sql(
        <<"ra_channel.scope">>, <<"ra_channel.workspace_id">>, UidExpr
    ),
    <<"EXISTS (SELECT 1 FROM public.channel ra_channel WHERE ra_channel.id=", ChannelExpr/binary,
        " AND ", Resource/binary, ")">>.

channel_allowed(ChannelId, Uid) ->
    Predicate = channel_predicate_sql(<<"$1">>, <<"$2">>),
    case elib_pg:one(<<"SELECT ", Predicate/binary, " AS allowed">>, [ChannelId, Uid]) of
        {ok, #{<<"allowed">> := true}} -> true;
        _ -> false
    end.
