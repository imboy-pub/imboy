-module(enterprise_internal_access_repo).
-moduledoc "HMAC application resource attribution and live lifecycle checks; no Scope/Grant lookup.".
-export([organization_tx/2, workspace_tx/3, resolve_organization_tx/3]).

organization_tx(Conn, #{
    authentication := hmac,
    organization_id := Org,
    application_id := App,
    credential_id := Cred
}) when
    is_integer(Org), Org > 0, is_integer(App), App > 0, is_integer(Cred), Cred > 0
->
    Sql = <<
        "SELECT o.id FROM organization o,enterprise_application a,"
        "enterprise_application_credential c WHERE o.id=$1 AND a.id=$2 AND c.id=$3 "
        "AND c.application_id=a.id AND o.status='active' AND a.status='active' "
        "AND c.status='active' AND (c.expires_at IS NULL OR c.expires_at>clock_timestamp()) "
        "AND (a.organization_id=o.id OR EXISTS(SELECT 1 FROM enterprise_directory_organization_source s "
        "WHERE s.application_id=a.id AND s.organization_id=o.id AND s.enabled "
        "AND s.authorization_expires_at>clock_timestamp())) FOR SHARE OF o,a,c"
    >>,
    allowed(elib_pg:query(Conn, Sql, [Org, App, Cred]));
organization_tx(_, _) ->
    {error, organization_boundary_violation}.

workspace_tx(Conn, Ctx, WorkspaceId) when is_integer(WorkspaceId), WorkspaceId > 0 ->
    case organization_tx(Conn, Ctx) of
        ok ->
            Sql = <<
                "SELECT id FROM workspace WHERE id=$1 AND organization_id=$2 "
                "AND status IN ('active','archived') FOR SHARE"
            >>,
            allowed(elib_pg:query(Conn, Sql, [WorkspaceId, maps:get(organization_id, Ctx)]));
        Error ->
            Error
    end;
workspace_tx(_, _, _) ->
    {error, organization_boundary_violation}.

allowed({ok, [_]}) -> ok;
allowed({ok, []}) -> {error, organization_boundary_violation};
allowed({error, _}) -> {error, security_gate_closed}.

%% Organization is business addressing, not a tenant. A signed client may pick
%% only the application's attribution or a live, Admin-configured source map.
resolve_organization_tx(Conn, Ctx, undefined) ->
    case maps:get(application_organization_id, Ctx) of
        Org when is_integer(Org) -> {ok, Ctx#{organization_id => Org}};
        null ->
            case
                elib_pg:query(
                    Conn,
                    <<
                        "SELECT DISTINCT s.organization_id FROM enterprise_directory_organization_source s "
                        "JOIN organization o ON o.id=s.organization_id AND o.status='active' "
                        "WHERE s.application_id=$1 AND s.enabled AND s.authorization_expires_at>clock_timestamp() "
                        "ORDER BY s.organization_id LIMIT 2"
                    >>,
                    [maps:get(application_id, Ctx)]
                )
            of
                {ok, [#{<<"organization_id">> := Org}]} -> {ok, Ctx#{organization_id => Org}};
                {ok, _} -> {ok, Ctx#{organization_id => null}};
                {error, _} -> {error, security_gate_closed}
            end
    end;
resolve_organization_tx(Conn, Ctx, Target) when is_binary(Target) ->
    case elib_tsid:from_binary(Target) of
        {ok, Org} ->
            Bound = Ctx#{organization_id => Org},
            case organization_tx(Conn, Bound) of
                ok -> {ok, Bound};
                Error -> Error
            end;
        error ->
            {error, invalid_request}
    end.
