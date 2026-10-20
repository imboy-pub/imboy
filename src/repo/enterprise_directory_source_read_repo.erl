-module(enterprise_directory_source_read_repo).
-moduledoc "Parameterized source reconciliation queries. Every statement checks live read grants, source, application and credential authority; no write permission is required.".
-export([organization_tx/3, organization_tx/4, page_tx/6]).

%% This fragment has only static SQL. $1/$2 always come from authenticated context.
authority(Ctx) -> authority(Ctx, <<"'identities:read'">>).

authority(#{authentication := hmac}, _ScopeSql) ->
    <<
        " o.application_id=$1 AND o.enabled AND o.authorization_expires_at>clock_timestamp()"
        " AND EXISTS(SELECT 1 FROM organization org WHERE org.id=o.organization_id AND org.status='active')"
        " AND EXISTS(SELECT 1 FROM enterprise_application a JOIN enterprise_application_credential c "
        "ON c.application_id=a.id WHERE a.id=o.application_id AND c.id=$2 AND a.status='active' "
        "AND c.status='active' AND (c.expires_at IS NULL OR c.expires_at>clock_timestamp()))"
    >>;
authority(_Ctx, ScopeSql) ->
    legacy_authority(ScopeSql).

legacy_authority(ScopeSql) ->
    <<
        " o.application_id=$1 AND o.enabled AND o.authorization_expires_at>clock_timestamp()"
        " AND EXISTS (SELECT 1 FROM organization org WHERE org.id=o.organization_id AND org.status='active')"
        " AND EXISTS (SELECT 1 FROM enterprise_application a"
        " JOIN enterprise_application_credential c ON c.application_id=a.id"
        " WHERE a.id=o.application_id AND c.id=$2 AND a.status='active' AND c.status='active'"
        " AND (c.expires_at IS NULL OR c.expires_at>clock_timestamp())"
        " AND a.allowed_scopes ? ",
        ScopeSql/binary,
        ")"
        " AND EXISTS (SELECT 1 FROM enterprise_application_grant g"
        " JOIN enterprise_application_grant_scope gs ON gs.grant_id=g.id"
        " WHERE g.application_id=o.application_id AND g.organization_id=o.organization_id"
        " AND g.workspace_scope_kind='none' AND g.status='active'"
        " AND g.valid_from<=clock_timestamp() AND g.expires_at>clock_timestamp()"
        " AND gs.scope=",
        ScopeSql/binary,
        ")"
    >>.

organization_tx(Conn, Ctx, External) ->
    organization_tx(Conn, Ctx, External, <<"identities:read">>).

organization_tx(Conn, Ctx, External, Scope) when
    Scope =:= <<"identities:read">>; Scope =:= <<"members:write">>
->
    Params =
        case maps:get(authentication, Ctx, legacy) of
            hmac -> identity(Ctx) ++ [External];
            _ -> identity(Ctx) ++ [External, Scope]
        end,
    case
        elib_pg:query(
            Conn,
            <<"SELECT o.organization_id FROM enterprise_directory_organization_source o WHERE ",
                (authority(Ctx, <<"$4">>))/binary, " AND o.external_organization_id=$3">>,
            Params
        )
    of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, resource_not_found};
        {error, _} -> {error, internal_error}
    end.

page_tx(Conn, organizations, Ctx, _, After, Limit) ->
    elib_pg:query(
        Conn,
        <<
            "SELECT o.external_organization_id,o.organization_id,o.source_version,o.enabled"
            " FROM enterprise_directory_organization_source o WHERE ",
            (authority(Ctx))/binary,
            " AND ($3::text IS NULL OR o.external_organization_id COLLATE \"C\">$3 COLLATE \"C\")"
            " ORDER BY o.external_organization_id COLLATE \"C\" LIMIT $4"
        >>,
        identity(Ctx) ++ [After, Limit]
    );
page_tx(Conn, departments, Ctx, Filter, After, Limit) ->
    elib_pg:query(
        Conn,
        <<
            "SELECT * FROM (SELECT d.external_department_id,d.department_id,p.external_department_id"
            " AS parent_external_id,n.name,d.source_version,"
            " CASE WHEN n.status='active' THEN 'active' ELSE 'removed' END AS status"
            " FROM enterprise_directory_organization_source o"
            " JOIN enterprise_directory_department_source d USING(application_id,organization_id)"
            " JOIN organization_department n ON n.organization_id=d.organization_id AND n.id=d.department_id"
            " LEFT JOIN enterprise_directory_department_source p ON p.application_id=d.application_id"
            " AND p.organization_id=d.organization_id AND p.department_id=n.parent_id WHERE ",
            (authority(Ctx))/binary,
            " AND o.external_organization_id=$3) rows"
            " WHERE ($4::text IS NULL OR external_department_id COLLATE \"C\">$4 COLLATE \"C\")"
            " AND ($6::text IS NULL OR external_department_id=$6) AND ($7::text IS NULL OR status=$7)"
            " ORDER BY external_department_id COLLATE \"C\" LIMIT $5"
        >>,
        query_params(Ctx, Filter, After, Limit) ++
            [value(<<"external_department_id">>, Filter), value(<<"status">>, Filter)]
    );
page_tx(Conn, persons, Ctx, Filter, After, Limit) ->
    elib_pg:query(
        Conn,
        person_query(Ctx),
        query_params(Ctx, Filter, After, Limit) ++
            [
                value(<<"external_user_id">>, Filter),
                value(<<"source_status">>, Filter),
                value(<<"membership_status">>, Filter),
                value(<<"projection_status">>, Filter)
            ]
    ).

person_query(Ctx) ->
    <<
        "SELECT * FROM (SELECT p.external_user_id,p.source_version,p.status AS source_status,"
        " CASE WHEN p.binding_retired THEN 'retired' WHEN p.user_id IS NULL THEN 'unlinked'"
        " ELSE 'linked' END AS binding_status,"
        " CASE WHEN NOT p.binding_retired AND u.status=1 AND u.account_type=0 THEN p.user_id"
        " ELSE NULL END AS user_id,m.source_version AS membership_version,m.status AS membership_status,"
        " m.department_external_ids,",
        (projection())/binary,
        " AS projection_status"
        " FROM enterprise_directory_organization_source o"
        " JOIN enterprise_directory_membership_source m USING(application_id,organization_id)"
        " JOIN enterprise_source_person p USING(application_id,external_user_id)"
        " LEFT JOIN \"user\" u ON u.id=p.user_id WHERE ",
        (authority(Ctx))/binary,
        " AND o.external_organization_id=$3) rows"
        " WHERE ($4::text IS NULL OR external_user_id COLLATE \"C\">$4 COLLATE \"C\")"
        " AND ($6::text IS NULL OR external_user_id=$6) AND ($7::text IS NULL OR source_status=$7)"
        " AND ($8::text IS NULL OR membership_status=$8) AND ($9::text IS NULL OR projection_status=$9)"
        " ORDER BY external_user_id COLLATE \"C\" LIMIT $5"
    >>.

projection() ->
    <<
        "CASE WHEN p.binding_retired THEN 'retired'"
        " WHEN m.status='removed' OR p.status='removed' THEN 'removed'"
        " WHEN p.user_id IS NULL THEN 'unlinked'"
        " WHEN m.projected_user_id=p.user_id AND u.status=1 AND u.account_type=0"
        " AND EXISTS (SELECT 1 FROM organization_member om WHERE om.organization_id=m.organization_id"
        " AND om.user_id=p.user_id AND om.status='active')"
        " AND NOT EXISTS (SELECT 1 FROM jsonb_array_elements_text(m.department_external_ids) x(external_id)"
        " WHERE NOT EXISTS (SELECT 1 FROM enterprise_directory_department_source ds"
        " JOIN organization_department d ON d.organization_id=ds.organization_id AND d.id=ds.department_id"
        " JOIN organization_department_member dm ON dm.department_id=d.id AND dm.user_id=p.user_id"
        " WHERE ds.application_id=m.application_id AND ds.organization_id=m.organization_id"
        " AND ds.external_department_id=x.external_id AND d.status='active'))"
        " THEN 'projected' ELSE 'pending' END"
    >>.

identity(Ctx) -> [maps:get(application_id, Ctx), maps:get(credential_id, Ctx)].
value(Key, Filter) -> maps:get(Key, Filter, null).
query_params(Ctx, Filter, After, Limit) ->
    identity(Ctx) ++ [maps:get(<<"external_organization_id">>, Filter), After, Limit].
