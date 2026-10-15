-module(enterprise_directory_source_repo).
-export([
    bootstrap_tx/4,
    pending_projections_tx/3,
    login_projections_tx/3,
    lock_source_tx/2,
    authorize_tx/2,
    organization_tx/3,
    save_organization_tx/5,
    department_tx/4,
    save_department_tx/6,
    membership_tx/4,
    save_membership_tx/7,
    save_offboarding_tx/5,
    authorize_member_tx/4,
    project_member_tx/6,
    department_members_tx/5,
    retire_person_tx/3,
    confirmed_owner_tx/2,
    archive_department_tx/3,
    restore_department_tx/5
]).

lock_source_tx(Conn, #{application_id := AppId, credential_id := CredId}) when
    is_integer(AppId), AppId > 0, is_integer(CredId), CredId > 0
->
    one(
        elib_pg:query(
            Conn,
            <<
                "SELECT a.id,a.allowed_scopes FROM enterprise_application a"
                " JOIN enterprise_application_credential c ON c.application_id=a.id"
                " WHERE a.id=$1 AND c.id=$2 AND a.status='active' AND c.status='active'"
                " AND (c.expires_at IS NULL OR c.expires_at>clock_timestamp())"
                " AND a.allowed_scopes ? 'identities:write' FOR UPDATE OF a FOR SHARE OF c"
            >>,
            [AppId, CredId]
        ),
        insufficient_scope
    );
lock_source_tx(_, _) ->
    {error, invalid_source_context}.

authorize_tx(Conn, #{application_id := AppId, organization_id := OrgId} = Ctx) when
    is_integer(OrgId), OrgId > 0
->
    case lock_source_tx(Conn, Ctx) of
        {ok, App} -> authorize_grants_tx(Conn, AppId, OrgId, App, Ctx);
        Error -> Error
    end;
authorize_tx(_, _) ->
    {error, invalid_source_context}.

authorize_grants_tx(Conn, AppId, OrgId, App, Ctx) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT id FROM enterprise_application_grant"
                " WHERE application_id=$1 AND organization_id=$2 ORDER BY id FOR SHARE"
            >>,
            [AppId, OrgId]
        )
    of
        {ok, Rows} ->
            GrantIds = [maps:get(<<"id">>, Row) || Row <- Rows],
            Scopes = app_scopes(maps:get(<<"allowed_scopes">>, App)),
            case enterprise_application_grant_logic:context_tx(Conn, OrgId, AppId, Scopes) of
                {ok, #{effective_scopes := Effective}} ->
                    case
                        enterprise_application_grant_logic:require_org_tx(
                            Conn,
                            Ctx#{organization_id => OrgId, granted_scopes => Effective},
                            <<"identities:write">>
                        )
                    of
                        ok -> current_grant_tx(Conn, AppId, OrgId, GrantIds);
                        Error -> Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

app_scopes(Value) ->
    try
        case Value of
            Json when is_binary(Json) -> jsone:decode(Json);
            List when is_list(List) -> List
        end
    of
        Scopes when is_list(Scopes) -> [Scope || Scope <- Scopes, is_binary(Scope)];
        _ -> []
    catch
        _:_ -> []
    end.

current_grant_tx(Conn, AppId, OrgId, GrantIds) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT EXISTS (SELECT 1 FROM enterprise_application_grant g"
                " JOIN enterprise_application_grant_scope s ON s.grant_id=g.id"
                " WHERE g.application_id=$1 AND g.organization_id=$2 AND g.id=ANY($3::bigint[])"
                " AND g.workspace_scope_kind='none' AND g.status='active' AND s.scope='identities:write'"
                " AND g.valid_from<=clock_timestamp() AND g.expires_at>clock_timestamp()"
                " AND EXISTS (SELECT 1 FROM enterprise_directory_organization_source o"
                " WHERE o.application_id=$1 AND o.organization_id=$2 AND o.enabled"
                " AND o.authorization_expires_at>clock_timestamp())) AS allowed"
            >>,
            [AppId, OrgId, GrantIds]
        )
    of
        {ok, [#{<<"allowed">> := true}]} -> {ok, #{organization_id => OrgId}};
        {ok, [#{<<"allowed">> := false}]} -> {error, organization_boundary_violation};
        {error, Reason} -> {error, Reason};
        _ -> {error, security_gate_closed}
    end.

confirmed_owner_tx(Conn, Owner) ->
    one(
        elib_pg:query(
            Conn,
            <<
                "SELECT id FROM \"user\""
                " WHERE id=$1 AND account_type=0 AND status=1 FOR SHARE"
            >>,
            [Owner]
        ),
        confirmed_human_owner_required
    ).

bootstrap_tx(Conn, Ctx, Source, Spec) ->
    App =
        case lock_source_tx(Conn, Ctx) of
            {ok, Row} -> Row;
            {error, Reason} -> throw({abort_tx, Reason})
        end,
    Scopes = app_scopes(maps:get(<<"allowed_scopes">>, App)),
    case
        lists:member(<<"identities:write">>, Scopes) andalso
            Scopes -- [<<"identities:write">>, <<"identities:read">>] =:= []
    of
        true -> bootstrap_authorized_tx(Conn, Ctx, Source, Spec, Scopes);
        false -> {error, directory_only_application_required}
    end.

bootstrap_authorized_tx(Conn, Ctx, _Source, Spec, Scopes) ->
    AppId = maps:get(application_id, Ctx),
    ExternalId = maps:get(<<"external_id">>, Spec),
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT authorization_expires_at,confirmed_owner_user_id FROM enterprise_directory_organization_source"
                " WHERE application_id=$1 AND external_organization_id=$2 AND organization_id IS NULL"
                " AND enabled AND authorization_expires_at>clock_timestamp() FOR UPDATE"
            >>,
            [AppId, ExternalId]
        )
    of
        {ok, [#{<<"authorization_expires_at">> := Until, <<"confirmed_owner_user_id">> := Owner}]} ->
            bootstrap_create_tx(Conn, Ctx, AppId, ExternalId, Owner, Spec, Scopes, Until);
        {ok, []} ->
            {error, directory_bootstrap_not_authorized};
        Error ->
            Error
    end.

bootstrap_create_tx(Conn, Ctx, AppId, ExternalId, Owner, Spec, Scopes, Until) ->
    Bootstrap = fun
        ({ok, Value}) -> Value;
        ({error, Reason}) -> throw({abort_tx, Reason})
    end,
    _ = Bootstrap(confirmed_owner_tx(Conn, Owner)),
    _ = Bootstrap(lock_source_tx(Conn, Ctx)),
    case
        Bootstrap(
            elib_pg:query(
                Conn,
                <<
                    "SELECT authorization_expires_at,confirmed_owner_user_id"
                    " FROM enterprise_directory_organization_source WHERE application_id=$1"
                    " AND external_organization_id=$2 AND organization_id IS NULL AND enabled"
                    " AND authorization_expires_at>clock_timestamp() FOR UPDATE"
                >>,
                [AppId, ExternalId]
            )
        )
    of
        [_] -> ok;
        _ -> throw({abort_tx, directory_bootstrap_not_authorized})
    end,
    Org = Bootstrap(
        organization_repo:create_tx(Conn, Owner, maps:get(<<"name">>, Spec), <<"active">>)
    ),
    OrgId = maps:get(<<"id">>, Org),
    case organization_member_repo:upsert_active_tx(Conn, OrgId, Owner, <<"owner">>, null) of
        {ok, changed, _} -> ok;
        {ok, unchanged, _} -> ok;
        {error, Reason} -> throw({abort_tx, Reason});
        _ -> throw({abort_tx, owner_membership_not_created})
    end,
    Grant = Bootstrap(
        enterprise_application_grant_repo:create_tx(
            Conn,
            OrgId,
            AppId,
            #{
                scopes => Scopes,
                workspace_scope_kind => none,
                workspace_ids => [],
                expires_at => Until,
                provisioning_source => <<"directory_bootstrap">>,
                idempotency_key =>
                    <<"directory-bootstrap-", (digest(#{<<"external_id">> => ExternalId}))/binary>>
            }
        )
    ),
    case
        write(
            elib_pg:execute(
                Conn,
                <<
                    "UPDATE enterprise_directory_organization_source"
                    " SET organization_id=$3,bootstrap_grant_id=$4 WHERE application_id=$1 AND external_organization_id=$2"
                    " AND organization_id IS NULL"
                >>,
                [AppId, ExternalId, OrgId, maps:get(<<"id">>, Grant)]
            )
        )
    of
        ok -> {ok, Org};
        {error, WriteReason} -> throw({abort_tx, WriteReason})
    end.

archive_department_tx(Conn, OrgId, DeptId) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT id FROM organization_department"
                " WHERE organization_id=$1 ORDER BY id FOR UPDATE"
            >>,
            [OrgId]
        )
    of
        {ok, _} -> archive_empty_department_tx(Conn, OrgId, DeptId);
        Error -> Error
    end.

archive_empty_department_tx(Conn, OrgId, DeptId) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT id FROM organization_department"
                " WHERE organization_id=$1 AND parent_id=$2 AND status='active'"
                " UNION ALL SELECT user_id FROM organization_department_member WHERE department_id=$2"
            >>,
            [OrgId, DeptId]
        )
    of
        {ok, []} ->
            write(
                elib_pg:execute(
                    Conn,
                    <<
                        "UPDATE organization_department SET status='archived',"
                        " version=version+1,updated_at=CURRENT_TIMESTAMP WHERE id=$1 AND organization_id=$2"
                    >>,
                    [DeptId, OrgId]
                )
            );
        {ok, _} ->
            {error, department_not_empty};
        Error ->
            Error
    end.

restore_department_tx(Conn, OrgId, DeptId, ParentId, Name) ->
    write(
        elib_pg:execute(
            Conn,
            <<
                "UPDATE organization_department SET status='active',"
                " parent_id=$3,name=$4,version=version+1,updated_at=CURRENT_TIMESTAMP"
                " WHERE organization_id=$1 AND id=$2 AND status='archived'"
            >>,
            [OrgId, DeptId, ParentId, Name]
        )
    ).

organization_tx(Conn, AppId, ExternalId) ->
    one(
        elib_pg:query(
            Conn,
            <<
                "SELECT * FROM enterprise_directory_organization_source"
                " WHERE application_id=$1 AND external_organization_id=$2"
                " AND enabled AND authorization_expires_at>clock_timestamp() FOR UPDATE"
            >>,
            [AppId, ExternalId]
        ),
        directory_source_not_configured
    ).

save_organization_tx(Conn, AppId, ExternalId, OrgId, Spec) ->
    write(
        elib_pg:execute(
            Conn,
            <<
                "UPDATE enterprise_directory_organization_source"
                " SET organization_id=$3,source_version=$4,payload_digest=$5"
                " WHERE application_id=$1 AND external_organization_id=$2"
            >>,
            [AppId, ExternalId, OrgId, maps:get(<<"version">>, Spec), digest(Spec)]
        )
    ).

department_tx(Conn, AppId, OrgId, ExternalId) ->
    one(
        elib_pg:query(
            Conn,
            <<
                "SELECT * FROM enterprise_directory_department_source"
                " WHERE application_id=$1 AND organization_id=$2 AND external_department_id=$3"
                " FOR UPDATE"
            >>,
            [AppId, OrgId, ExternalId]
        ),
        not_found
    ).

save_department_tx(Conn, AppId, OrgId, ExternalId, DeptId, Spec) ->
    write(
        elib_pg:execute(
            Conn,
            <<
                "INSERT INTO enterprise_directory_department_source"
                " (application_id,organization_id,external_department_id,department_id,source_version,payload_digest)"
                " VALUES ($1,$2,$3,$4,$5,$6) ON CONFLICT (application_id,organization_id,external_department_id)"
                " DO UPDATE SET source_version=EXCLUDED.source_version,payload_digest=EXCLUDED.payload_digest"
            >>,
            [AppId, OrgId, ExternalId, DeptId, maps:get(<<"version">>, Spec), digest(Spec)]
        )
    ).

membership_tx(Conn, AppId, OrgId, ExternalId) ->
    one(
        elib_pg:query(
            Conn,
            <<
                "SELECT * FROM enterprise_directory_membership_source"
                " WHERE application_id=$1 AND organization_id=$2 AND external_user_id=$3 FOR UPDATE"
            >>,
            [AppId, OrgId, ExternalId]
        ),
        not_found
    ).

%% Login must distinguish an unavailable configured source from missing write scope.
login_projections_tx(Conn, AppId, ExternalId) ->
    elib_pg:query(
        Conn,
        <<
            "SELECT m.*, (o.enabled AND o.authorization_expires_at>clock_timestamp()) "
            "AS source_current FROM enterprise_directory_membership_source m "
            "JOIN enterprise_directory_organization_source o USING(application_id,organization_id) "
            "WHERE m.application_id=$1 AND m.external_user_id=$2 AND m.status='active' "
            "AND m.projected_user_id IS NULL ORDER BY m.organization_id FOR UPDATE OF o,m"
        >>,
        [AppId, ExternalId]
    ).

pending_projections_tx(Conn, AppId, ExternalId) ->
    elib_pg:query(
        Conn,
        <<
            "SELECT m.* FROM enterprise_directory_membership_source m"
            " JOIN enterprise_directory_organization_source o USING(application_id,organization_id)"
            " WHERE m.application_id=$1 AND m.external_user_id=$2 AND o.enabled"
            " AND o.authorization_expires_at>clock_timestamp()"
            " AND m.status='active' AND m.projected_user_id IS NULL"
            " ORDER BY m.organization_id FOR UPDATE OF o,m"
        >>,
        [AppId, ExternalId]
    ).

save_membership_tx(Conn, AppId, OrgId, ExternalId, Managed, UserId, Spec) ->
    write(
        elib_pg:execute(
            Conn,
            <<
                "INSERT INTO enterprise_directory_membership_source"
                " (application_id,organization_id,external_user_id,managed,source_version,payload_digest,status,projected_user_id,department_external_ids)"
                " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9::jsonb) ON CONFLICT (application_id,organization_id,external_user_id)"
                " DO UPDATE SET managed=EXCLUDED.managed,source_version=EXCLUDED.source_version,"
                " offboarding_member_updated_at=CASE WHEN EXCLUDED.status='removed'"
                " THEN enterprise_directory_membership_source.offboarding_member_updated_at ELSE NULL END,"
                " payload_digest=EXCLUDED.payload_digest,status=EXCLUDED.status,projected_user_id=EXCLUDED.projected_user_id,"
                " department_external_ids=EXCLUDED.department_external_ids"
            >>,
            [
                AppId,
                OrgId,
                ExternalId,
                Managed,
                maps:get(<<"version">>, Spec),
                digest(Spec),
                maps:get(<<"status">>, Spec),
                UserId,
                jsone:encode(maps:get(<<"department_external_ids">>, Spec))
            ]
        )
    ).

authorize_member_tx(Conn, AppId, OrgId, UserId) ->
    case
        one(
            elib_pg:query(
                Conn,
                <<
                    "SELECT m.external_user_id"
                    " FROM enterprise_directory_membership_source m"
                    " JOIN enterprise_directory_organization_source o USING(application_id,organization_id)"
                    " JOIN enterprise_source_person p USING(application_id,external_user_id)"
                    " WHERE m.application_id=$1 AND m.organization_id=$2 AND p.user_id=$3"
                    " AND m.managed AND o.enabled FOR UPDATE OF o,m"
                >>,
                [AppId, OrgId, UserId]
            ),
            directory_membership_not_managed
        )
    of
        {ok, _} -> ok;
        Error -> Error
    end.

save_offboarding_tx(Conn, AppId, OrgId, ExternalId, actual_removed) ->
    write(
        elib_pg:execute(
            Conn,
            <<
                "UPDATE enterprise_directory_membership_source s"
                " SET offboarding_member_updated_at=m.updated_at FROM organization_member m"
                " WHERE s.application_id=$1 AND s.organization_id=$2 AND s.external_user_id=$3"
                " AND m.organization_id=s.organization_id AND m.user_id=s.projected_user_id"
                " AND m.status='removed' AND m.updated_at IS NOT NULL"
            >>,
            [AppId, OrgId, ExternalId]
        )
    );
save_offboarding_tx(Conn, AppId, OrgId, ExternalId, RemovedAt) ->
    write(
        elib_pg:execute(
            Conn,
            <<
                "UPDATE enterprise_directory_membership_source"
                " SET offboarding_member_updated_at=$4::timestamptz"
                " WHERE application_id=$1 AND organization_id=$2 AND external_user_id=$3"
            >>,
            [AppId, OrgId, ExternalId, RemovedAt]
        )
    ).

project_member_tx(Conn, OrgId, UserId, Status, Managed, PreviousSource) ->
    PreviousSourceStatus = maps:get(<<"status">>, PreviousSource, undefined),
    RemovedAt = maps:get(<<"offboarding_member_updated_at">>, PreviousSource, null),
    case organization_member_repo:find_for_update_tx(Conn, OrgId, UserId, <<"role,status">>) of
        {ok, #{<<"status">> := <<"suspended">>}} ->
            {error, local_membership_conflict};
        {error, not_found} when Status =:= <<"active">> ->
            case
                organization_member_repo:upsert_active_tx(Conn, OrgId, UserId, <<"member">>, null)
            of
                {ok, changed, _} -> {ok, true};
                {ok, unchanged, _} -> {ok, false};
                {ok, role_conflict, _} -> {error, local_membership_conflict};
                Error -> Error
            end;
        {error, not_found} ->
            {ok, Managed};
        {ok, #{<<"status">> := <<"active">>}} when Status =:= <<"active">> -> {ok, Managed};
        {ok, #{<<"role">> := <<"member">>, <<"status">> := <<"removed">>}} when
            Status =:= <<"active">>,
            Managed =:= true,
            PreviousSourceStatus =:= <<"removed">>,
            RemovedAt =/= null
        ->
            case
                elib_pg:execute(
                    Conn,
                    <<
                        "UPDATE organization_member SET status='active',"
                        " updated_at=CURRENT_TIMESTAMP WHERE organization_id=$1 AND user_id=$2"
                        " AND role='member' AND status='removed' AND updated_at=$3::timestamptz"
                    >>,
                    [OrgId, UserId, RemovedAt]
                )
            of
                {ok, 1} -> {ok, true};
                {error, Reason} -> {error, Reason};
                _ -> {error, local_membership_conflict}
            end;
        {ok, #{<<"status">> := <<"removed">>}} when Status =:= <<"removed">> -> {ok, Managed};
        {ok, _} when Status =:= <<"removed">>, Managed -> {ok, remove};
        {ok, _} ->
            {error, local_membership_conflict};
        Error ->
            Error
    end.

department_members_tx(Conn, AppId, OrgId, UserId, DeptIds) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT department_id FROM enterprise_directory_department_member_source"
                " WHERE application_id=$1 AND organization_id=$2 AND user_id=$3 ORDER BY department_id FOR UPDATE"
            >>,
            [AppId, OrgId, UserId]
        )
    of
        {ok, Rows} ->
            Existing = [maps:get(<<"department_id">>, Row) || Row <- Rows],
            reconcile_departments(Conn, AppId, OrgId, UserId, Existing, DeptIds);
        Error ->
            Error
    end.

reconcile_departments(Conn, AppId, OrgId, UserId, Existing, Desired) ->
    lists:foreach(
        fun(DeptId) ->
            case organization_department_pg:delete_member_tx(Conn, DeptId, UserId) of
                {ok, removed} -> ok;
                {ok, not_present} -> ensure_no_admin(Conn, DeptId, UserId);
                {error, Reason} -> throw({abort_tx, Reason})
            end,
            {ok, _} = elib_pg:execute(
                Conn,
                <<
                    "DELETE FROM enterprise_directory_department_member_source"
                    " WHERE application_id=$1 AND department_id=$2 AND user_id=$3"
                >>,
                [AppId, DeptId, UserId]
            )
        end,
        Existing -- Desired
    ),
    lists:foreach(
        fun(DeptId) ->
            case organization_department_pg:insert_member_tx(Conn, OrgId, DeptId, UserId, null) of
                {ok, _, added} ->
                    ok = write(
                        elib_pg:execute(
                            Conn,
                            <<
                                "INSERT INTO enterprise_directory_department_member_source"
                                " (application_id,organization_id,department_id,user_id) VALUES ($1,$2,$3,$4)"
                                " ON CONFLICT (application_id,department_id,user_id) DO UPDATE"
                                " SET organization_id=EXCLUDED.organization_id"
                            >>,
                            [AppId, OrgId, DeptId, UserId]
                        )
                    );
                {ok, _, already_member} ->
                    ok;
                {error, Reason} ->
                    throw({abort_tx, Reason})
            end
        end,
        Desired
    ),
    ok.

ensure_no_admin(Conn, DeptId, UserId) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT user_id FROM organization_department_member"
                " WHERE department_id=$1 AND user_id=$2 AND is_admin FOR UPDATE"
            >>,
            [DeptId, UserId]
        )
    of
        {ok, []} -> ok;
        {ok, _} -> throw({abort_tx, local_department_admin_conflict});
        {error, Reason} -> throw({abort_tx, Reason})
    end.

retire_person_tx(Conn, AppId, ExternalId) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT organization_id FROM enterprise_directory_membership_source"
                " WHERE application_id=$1 AND external_user_id=$2 AND status='active'"
            >>,
            [AppId, ExternalId]
        )
    of
        {ok, []} -> ok;
        {ok, _} -> {error, active_source_memberships};
        Error -> Error
    end.

digest(Spec) ->
    {ok, Digest} = enterprise_internal_idempotency:request_digest(
        <<"POST">>, <<"directory">>, Spec
    ),
    Digest.

one({ok, [Row]}, _) -> {ok, Row};
one({ok, []}, Reason) -> {error, Reason};
one({error, Reason}, _) -> {error, Reason}.

write({ok, 1}) -> ok;
write({error, Reason}) -> {error, Reason};
write(Other) -> {error, {unexpected_write_result, Other}}.
