-module(enterprise_directory_sync_pg_tests).
-export([all_rows_snapshot/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% 独立 VM 拥有 TSID 初始化，避免改写其他套件正在使用的节点身份。
%% The standalone fixture owns its TSID identity in a separate VM.
directory_pg_test_() -> {timeout, 900, fun() -> isolated_pg_fixture:run(fun run/0) end}.

run() ->
    ok = require_server_env(fun os:getenv/1),
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"DIRECTORY_SYNC_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    %% Provisioning loads application env; install the peer-owned AEAD key after it.
    application:set_env(imboy, postgre_aes_key, crypto:strong_rand_bytes(32)),
    Conn = maps:get(conn, State),
    try
        schema(Conn),
        empty_down_up(Conn),
        Ctx = seed(Conn),
        trusted_bootstrap(Conn, Ctx),
        applied_and_replay(Conn, Ctx),
        rejected_atomicity(Conn, Ctx),
        pending_claim_projection(Conn, Ctx),
        enterprise_directory_source_read_pg_checks:exercise(Conn, Ctx),
        enterprise_directory_member_pg_checks:exercise(Conn, Ctx),
        local_membership_governance(Conn, Ctx),
        enterprise_directory_user_delete_pg_checks:exercise(State, Ctx),
        concurrent_revocations(State, Ctx),
        actual_target_and_revocation(Conn, Ctx),
        source_restore(Conn, Ctx),
        populated_down_rejected(Conn)
    after
        inttest_marker_db:release(State)
    end.

%% An explicitly configured trust password may be empty; missing credentials and
%% missing server coordinates remain distinct errors. Authentication is performed
%% by the real PG connection, never by this presence check.
require_server_env(Getenv) ->
    Names = [
        "DIRECTORY_SYNC_INTTEST_PG_HOST",
        "DIRECTORY_SYNC_INTTEST_PG_PORT",
        "DIRECTORY_SYNC_INTTEST_PG_USER",
        "DIRECTORY_SYNC_INTTEST_PG_PASSWORD"
    ],
    lists:foreach(
        fun(Name) ->
            case Getenv(Name) of
                false ->
                    error({directory_fixture_requires_explicit_server, Name});
                "" when Name =/= "DIRECTORY_SYNC_INTTEST_PG_PASSWORD" ->
                    error({directory_fixture_requires_explicit_server, Name});
                _ ->
                    ok
            end
        end,
        Names
    ).

directory_server_env_contract_test() ->
    Password = "DIRECTORY_SYNC_INTTEST_PG_PASSWORD",
    Base = #{
        "DIRECTORY_SYNC_INTTEST_PG_HOST" => "127.0.0.1",
        "DIRECTORY_SYNC_INTTEST_PG_PORT" => "55432",
        "DIRECTORY_SYNC_INTTEST_PG_USER" => "fixture"
    },
    Read = fun(Map) -> fun(Name) -> maps:get(Name, Map, false) end end,
    ?assertEqual(ok, require_server_env(Read(Base#{Password => ""}))),
    ?assertEqual(ok, require_server_env(Read(Base#{Password => "synthetic-nonempty"}))),
    ?assertError(
        {directory_fixture_requires_explicit_server, Password},
        require_server_env(Read(Base))
    ),
    [
        begin
            ?assertError(
                {directory_fixture_requires_explicit_server, Name},
                require_server_env(Read((Base#{Password => ""})#{Name => ""}))
            ),
            ?assertError(
                {directory_fixture_requires_explicit_server, Name},
                require_server_env(Read(maps:remove(Name, Base#{Password => ""})))
            )
        end
     || Name <- maps:keys(Base)
    ],
    ok.

schema(Conn) ->
    {ok, [#{<<"present">> := Present}]} = elib_pg:query(
        Conn,
        <<
            "SELECT to_regclass('enterprise_directory_organization_source') IS NOT NULL"
            " AND to_regclass('enterprise_directory_department_source') IS NOT NULL"
            " AND to_regclass('enterprise_directory_membership_source') IS NOT NULL"
            " AND to_regclass('enterprise_directory_department_member_source') IS NOT NULL AS present"
        >>,
        []
    ),
    ?assertEqual(true, Present).

empty_down_up(Conn) ->
    Before = schema_snapshot(Conn),
    BeforeRows = all_rows_snapshot(Conn),
    lists:foreach(
        fun(Table) ->
            ?assertEqual(0, scalar(Conn, <<"SELECT count(*) AS value FROM ", Table/binary>>))
        end,
        source_tables()
    ),
    migration_tx(Conn, down),
    ?assertEqual(
        null,
        scalar(
            Conn,
            <<"SELECT to_regclass('enterprise_directory_organization_source')::text AS value">>
        )
    ),
    migration_tx(Conn, up),
    schema(Conn),
    ?assertEqual(Before, schema_snapshot(Conn)),
    ?assertEqual(BeforeRows, all_rows_snapshot(Conn)).

populated_down_rejected(Conn) ->
    lists:foreach(
        fun(Table) ->
            ?assert(scalar(Conn, <<"SELECT count(*) AS value FROM ", Table/binary>>) > 0)
        end,
        source_tables()
    ),
    ?assert(
        scalar(Conn, <<
            "SELECT count(*) AS value FROM enterprise_application_grant"
            " WHERE provisioning_source='directory_bootstrap'"
        >>) > 0
    ),
    BeforeSchema = schema_snapshot(Conn),
    BeforeRows = all_rows_snapshot(Conn),
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    try
        Results = sql_results(epgsql:squery(Conn, migration_sql(down))),
        Errors = [Code || {error, #error{code = Code}} <- Results],
        ?assertEqual([<<"23514">>], Errors)
    after
        {ok, _, _} = epgsql:squery(Conn, "ROLLBACK")
    end,
    schema(Conn),
    ?assertEqual(BeforeSchema, schema_snapshot(Conn)),
    ?assertEqual(BeforeRows, all_rows_snapshot(Conn)).

migration_tx(Conn, Direction) ->
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    try
        lists:foreach(
            fun(Result) -> ?assertNotMatch({error, _}, Result) end,
            sql_results(epgsql:squery(Conn, migration_sql(Direction)))
        ),
        {ok, _, _} = epgsql:squery(Conn, "COMMIT")
    catch
        Class:Reason:Stack ->
            _ = epgsql:squery(Conn, "ROLLBACK"),
            erlang:raise(Class, Reason, Stack)
    end.

migration_sql(Direction) ->
    Path =
        "priv/migrations/00000177_enterprise_directory_sync." ++ atom_to_list(Direction) ++ ".sql",
    {ok, Sql} = file:read_file(Path),
    {ok, Suspension} = file:read_file(
        "priv/migrations/00000208_directory_member_suspension." ++ atom_to_list(Direction) ++ ".sql"
    ),
    case Direction of
        down -> <<Suspension/binary, "\n", Sql/binary>>;
        up -> <<Sql/binary, "\n", Suspension/binary>>
    end.

sql_results(Results) when is_list(Results) -> Results;
sql_results(Result) -> [Result].

source_tables() ->
    [
        <<"enterprise_directory_organization_source">>,
        <<"enterprise_directory_department_source">>,
        <<"enterprise_directory_membership_source">>,
        <<"enterprise_directory_department_member_source">>
    ].

all_rows_snapshot(Conn) ->
    Tables =
        source_tables() ++
            [
                <<"enterprise_application">>,
                <<"enterprise_application_credential">>,
                <<"enterprise_application_grant">>,
                <<"enterprise_application_grant_scope">>,
                <<"enterprise_application_grant_workspace">>,
                <<"enterprise_source_person">>,
                <<"organization">>,
                <<"organization_department">>,
                <<"organization_member">>,
                <<"organization_department_member">>,
                <<"enterprise_audit_event">>,
                <<"enterprise_internal_idempotency">>
            ],
    [
        {Table,
            scalar(Conn, <<
                "SELECT COALESCE(jsonb_agg(to_jsonb(rows)"
                " ORDER BY to_jsonb(rows)::text),'[]'::jsonb)::text AS value FROM ",
                Table/binary,
                " rows"
            >>)}
     || Table <- Tables
    ].

schema_snapshot(Conn) ->
    scalar(Conn, <<
        "SELECT COALESCE(jsonb_agg(item ORDER BY item::text),'[]'::jsonb)::text AS value FROM ("
        " SELECT to_jsonb(c) AS item FROM information_schema.columns c WHERE table_schema='public'"
        " UNION ALL SELECT jsonb_build_object('table',r.relname,'constraint',c.conname,"
        " 'definition',pg_get_constraintdef(c.oid)) FROM pg_constraint c"
        " JOIN pg_class r ON r.oid=c.conrelid JOIN pg_namespace n ON n.oid=r.relnamespace WHERE n.nspname='public'"
        " UNION ALL SELECT jsonb_build_object('index',indexname,'definition',indexdef) FROM pg_indexes WHERE schemaname='public'"
        " UNION ALL SELECT jsonb_build_object('trigger',t.tgname,'definition',pg_get_triggerdef(t.oid))"
        " FROM pg_trigger t JOIN pg_class r ON r.oid=t.tgrelid JOIN pg_namespace n ON n.oid=r.relnamespace"
        " WHERE n.nspname='public' AND NOT t.tgisinternal"
        " UNION ALL SELECT jsonb_build_object('function',p.proname,'definition',pg_get_functiondef(p.oid))"
        " FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace"
        " WHERE n.nspname='public' AND p.proname='fn_enterprise_directory_source_version_guard'"
        " ) definitions"
    >>).

seed(Conn) ->
    {ok, _} = elib_pg:query(
        Conn,
        <<
            "INSERT INTO \"user\""
            " (id,password,account,account_type,status,reg_ip,reg_cosv)"
            " VALUES (977001,'x','directory-fixture-owner',0,1,'127.0.0.1','x'),"
            " (977002,'x','directory-fixture-person',0,1,'127.0.0.1','x')"
        >>,
        []
    ),
    lists:foreach(
        fun(OrgId) ->
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "INSERT INTO organization"
                    " (id,name,owner_id,status,branding,settings) VALUES ($1,'original',977001,'active','{}','{}')"
                >>,
                [OrgId]
            )
        end,
        [977100, 977101]
    ),
    {ok, App} = enterprise_application_repo:create_tx(
        Conn,
        977100,
        <<"directory-fixture">>,
        <<"Directory">>,
        {null, [<<"identities:write">>]}
    ),
    AppId = maps:get(<<"id">>, App),
    {ok, Credential} = enterprise_application_credential_repo:create_tx(
        Conn,
        977100,
        AppId,
        <<"directory-fixture">>,
        <<"synthetic-fixture-secret">>
    ),
    {ok, _} = elib_pg:query(
        Conn,
        <<
            "INSERT INTO enterprise_application_grant"
            " (id,organization_id,application_id,workspace_scope_kind,status,valid_from,expires_at,idempotency_key)"
            " VALUES (977200,977101,$1,'none','active',clock_timestamp()-interval '1 day',"
            " clock_timestamp()+interval '1 day','directory-fixture')"
        >>,
        [AppId]
    ),
    {ok, _} = elib_pg:query(
        Conn,
        <<
            "INSERT INTO enterprise_application_grant_scope"
            " (grant_id,scope) VALUES (977200,'identities:write')"
        >>,
        []
    ),
    {ok, _} = elib_pg:query(
        Conn,
        <<
            "INSERT INTO enterprise_directory_organization_source"
            " (application_id,external_organization_id,organization_id,enabled,authorization_expires_at)"
            " VALUES ($1,'fixture',977101,true,clock_timestamp()+interval '1 day')"
        >>,
        [AppId]
    ),
    #{
        application_id => AppId,
        credential_id => maps:get(<<"id">>, Credential),
        organization_id => 977100,
        granted_scopes => [<<"identities:write">>]
    }.

applied_and_replay(Conn, Ctx) ->
    ?assertMatch({ok, applied, _}, tx(Conn, Ctx, <<"fixture-first">>, body(1))),
    ?assertMatch({ok, replay, _}, tx(Conn, Ctx, <<"fixture-first">>, body(1))),
    ?assertEqual(
        1,
        scalar(Conn, <<
            "SELECT source_version AS value FROM"
            " enterprise_directory_organization_source WHERE external_organization_id='fixture'"
        >>)
    ).

rejected_atomicity(Conn, Ctx) ->
    Before = snapshot(Conn),
    Dept = #{
        <<"external_id">> => <<"bad">>,
        <<"parent_external_id">> => <<"missing">>,
        <<"name">> => <<"Bad">>,
        <<"version">> => 1,
        <<"status">> => <<"active">>
    },
    ?assertMatch(
        {error, _}, tx(Conn, Ctx, <<"fixture-fail">>, (body(2))#{<<"departments">> => [Dept]})
    ),
    ?assertEqual(Before, snapshot(Conn)).

actual_target_and_revocation(Conn, Ctx) ->
    Before = snapshot(Conn),
    {ok, _} = elib_pg:query(
        Conn,
        <<
            "UPDATE enterprise_application_grant SET"
            " status='revoked',revoked_at=clock_timestamp(),revoked_by_user_id=977001 WHERE id=977200"
        >>,
        []
    ),
    ?assertMatch({error, _}, tx(Conn, Ctx, <<"fixture-revoked">>, body(2))),
    ?assertEqual(Before, snapshot(Conn)),
    ?assertMatch({error, _}, tx(Conn, Ctx, <<"fixture-first">>, body(1))).

pending_claim_projection(Conn, Ctx) ->
    Dept = #{
        <<"external_id">> => <<"claim-dept">>,
        <<"parent_external_id">> => null,
        <<"name">> => <<"Claim Dept">>,
        <<"version">> => 1,
        <<"status">> => <<"active">>
    },
    Person = #{
        <<"external_user_id">> => <<"pending">>,
        <<"display_name">> => <<"Pending">>,
        <<"source_status">> => <<"active">>,
        <<"source_version">> => 1,
        <<"membership">> => #{
            <<"version">> => 1,
            <<"status">> => <<"active">>,
            <<"department_external_ids">> => [<<"claim-dept">>]
        }
    },
    ?assertMatch(
        {ok, applied, _},
        tx(
            Conn,
            Ctx,
            <<"fixture-pending">>,
            (body(2))#{<<"departments">> => [Dept], <<"persons">> => [Person]}
        )
    ),
    ?assertEqual(
        0,
        scalar(Conn, <<
            "SELECT count(*) AS value FROM organization_member"
            " WHERE organization_id=977101 AND user_id=977002"
        >>)
    ),
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    try
        ?assertMatch(
            {ok, _},
            enterprise_directory_sync_logic:claim_and_project_tx(
                Conn, Ctx, <<"pending">>, 977002
            )
        ),
        ?assertEqual(
            1,
            scalar(Conn, <<
                "SELECT count(*) AS value FROM organization_member"
                " WHERE organization_id=977101 AND user_id=977002 AND role='member' AND status='active'"
            >>)
        ),
        ?assertEqual(
            1,
            scalar(Conn, <<
                "SELECT count(*) AS value FROM organization_department_member m"
                " JOIN enterprise_directory_department_source s ON s.department_id=m.department_id"
                " WHERE s.organization_id=977101 AND s.external_department_id='claim-dept' AND m.user_id=977002"
            >>)
        ),
        {ok, _, _} = epgsql:squery(Conn, "COMMIT")
    catch
        Class:Reason:Stack ->
            _ = epgsql:squery(Conn, "ROLLBACK"),
            erlang:raise(Class, Reason, Stack)
    end.

local_membership_governance(Conn, Ctx) ->
    lists:foreach(
        fun(Status) ->
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "UPDATE organization_member SET status=$1"
                    " WHERE organization_id=977101 AND user_id=977002"
                >>,
                [Status]
            ),
            Before = all_rows_snapshot(Conn),
            ?assertEqual(
                {error, local_membership_conflict},
                tx(
                    Conn,
                    Ctx,
                    <<"fixture-local-", Status/binary>>,
                    pending_body(2, <<"active">>)
                )
            ),
            case Status of
                <<"suspended">> ->
                    ?assertEqual(
                        {error, local_membership_conflict},
                        tx(
                            Conn,
                            Ctx,
                            <<"fixture-suspended-departure">>,
                            pending_body(2, <<"removed">>)
                        )
                    );
                _ ->
                    ok
            end,
            ?assertEqual(Before, all_rows_snapshot(Conn))
        end,
        [<<"suspended">>, <<"removed">>]
    ),
    ?assertMatch(
        {ok, applied, _},
        tx(
            Conn,
            Ctx,
            <<"fixture-local-removed-source-removed">>,
            pending_body(2, <<"removed">>)
        )
    ),
    ?assertEqual(
        null,
        scalar(Conn, <<
            "SELECT offboarding_member_updated_at AS value"
            " FROM enterprise_directory_membership_source WHERE external_user_id='pending'"
        >>)
    ),
    BeforeLocalRestore = all_rows_snapshot(Conn),
    ?assertEqual(
        {error, local_membership_conflict},
        tx(
            Conn,
            Ctx,
            <<"fixture-local-removed-source-active">>,
            pending_body(3, <<"active">>)
        )
    ),
    ?assertEqual(BeforeLocalRestore, all_rows_snapshot(Conn)),
    {ok, _} = elib_pg:query(
        Conn,
        <<
            "UPDATE organization_member SET status='active'"
            " WHERE organization_id=977101 AND user_id=977002"
        >>,
        []
    ),
    {ok, _, _} = organization_member_repo:upsert_active_tx(
        Conn, 977100, 977002, <<"member">>, null
    ),
    OtherOrg = scalar(Conn, <<
        "SELECT to_jsonb(m)::text AS value FROM organization_member m"
        " WHERE organization_id=977100 AND user_id=977002"
    >>),
    Owner = scalar(Conn, <<
        "SELECT to_jsonb(m)::text AS value FROM organization_member m"
        " WHERE organization_id=977101 AND user_id=977001"
    >>),
    ?assertMatch(
        {ok, applied, _},
        tx(Conn, Ctx, <<"fixture-source-departure">>, pending_body(3, <<"removed">>))
    ),
    ?assertEqual(
        true,
        scalar(Conn, <<
            "SELECT s.offboarding_member_updated_at=m.updated_at AS value"
            " FROM enterprise_directory_membership_source s JOIN organization_member m"
            " ON m.organization_id=s.organization_id AND m.user_id=s.projected_user_id"
            " WHERE s.external_user_id='pending'"
        >>)
    ),
    ?assertEqual(
        <<"active">>,
        scalar(Conn, <<
            "SELECT status AS value FROM enterprise_source_person"
            " WHERE external_user_id='pending'"
        >>)
    ),
    ?assertEqual(
        OtherOrg,
        scalar(Conn, <<
            "SELECT to_jsonb(m)::text AS value FROM organization_member m"
            " WHERE organization_id=977100 AND user_id=977002"
        >>)
    ),
    ?assertEqual(
        Owner,
        scalar(Conn, <<
            "SELECT to_jsonb(m)::text AS value FROM organization_member m"
            " WHERE organization_id=977101 AND user_id=977001"
        >>)
    ),
    ?assertMatch(
        {ok, applied, _}, tx(Conn, Ctx, <<"fixture-source-restore">>, pending_body(4, <<"active">>))
    ),
    ?assertEqual(
        null,
        scalar(Conn, <<
            "SELECT offboarding_member_updated_at AS value"
            " FROM enterprise_directory_membership_source WHERE external_user_id='pending'"
        >>)
    ),
    ?assertEqual(
        <<"active">>,
        scalar(Conn, <<
            "SELECT status AS value FROM organization_member"
            " WHERE organization_id=977101 AND user_id=977002"
        >>)
    ).

pending_body(Version, Status) ->
    Person = #{
        <<"external_user_id">> => <<"pending">>,
        <<"display_name">> => <<"Pending">>,
        <<"source_status">> => <<"active">>,
        <<"source_version">> => 1,
        <<"membership">> => #{
            <<"version">> => Version,
            <<"status">> => Status,
            <<"department_external_ids">> =>
                case Status of
                    <<"active">> -> [<<"claim-dept">>];
                    <<"removed">> -> []
                end
        }
    },
    (body(2))#{<<"persons">> => [Person]}.

source_restore(Conn, Ctx) ->
    AppId = maps:get(application_id, Ctx),
    {ok, _} = enterprise_source_person_repo:sync_tx(
        Conn, AppId, <<"restore">>, <<"Person">>, <<"removed">>, 1
    ),
    ?assertMatch(
        {ok, #{<<"status">> := <<"active">>, <<"user_id">> := null}},
        enterprise_source_person_repo:sync_tx(
            Conn, AppId, <<"restore">>, <<"Person">>, <<"active">>, 2
        )
    ).

tx(Conn, Ctx, Key, Body) ->
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    try enterprise_directory_sync_logic:sync_tx(Conn, Ctx, Key, Body) of
        Result ->
            {ok, _, _} = epgsql:squery(Conn, "COMMIT"),
            Result
    catch
        throw:{abort_tx, Reason} ->
            {ok, _, _} = epgsql:squery(Conn, "ROLLBACK"),
            {error, Reason};
        Class:Reason:Stack ->
            _ = epgsql:squery(Conn, "ROLLBACK"),
            erlang:raise(Class, Reason, Stack)
    end.

snapshot(Conn) ->
    {ok, Rows} = elib_pg:query(
        Conn,
        <<
            "SELECT name,"
            " (SELECT source_version FROM enterprise_directory_organization_source WHERE external_organization_id='fixture') AS version,"
            " (SELECT count(*) FROM enterprise_directory_department_source) AS departments,"
            " (SELECT count(*) FROM enterprise_directory_membership_source) AS members,"
            " (SELECT count(*) FROM enterprise_audit_event) AS audits,"
            " (SELECT count(*) FROM enterprise_internal_idempotency) AS requests"
            " FROM organization WHERE id=977101"
        >>,
        []
    ),
    Rows.

scalar(Conn, Sql) ->
    {ok, [#{<<"value">> := Value}]} = elib_pg:query(Conn, Sql, []),
    Value.

body(Version) ->
    #{
        <<"organization">> => #{
            <<"external_id">> => <<"fixture">>,
            <<"name">> => <<"Synced">>,
            <<"version">> => Version
        },
        <<"departments">> => [],
        <<"persons">> => []
    }.

trusted_bootstrap(Conn, Ctx) ->
    AppId = maps:get(application_id, Ctx),
    %% A combined OA source is allowed, but bootstrap must not grant OA powers.
    {ok, _} = elib_pg:query(
        Conn,
        <<"UPDATE enterprise_application SET allowed_scopes=$2::jsonb WHERE id=$1">>,
        [
            AppId,
            <<"[\"identities:write\",\"identities:read\",\"oa:login:issue\",\"sso:exchange\"]">>
        ]
    ),
    {ok, _} = elib_pg:query(
        Conn,
        <<
            "INSERT INTO adm_user(id,account,password,status)"
            " VALUES (977010,'directory-fixture-admin','synthetic',1)"
        >>,
        []
    ),
    {ok, [#{<<"expires_at">> := Until}]} = elib_pg:query(
        Conn,
        <<"SELECT clock_timestamp()+interval '1 day' AS expires_at">>,
        []
    ),
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    try
        ?assertMatch(
            {ok, _},
            enterprise_directory_source_config_logic:configure_source_tx(
                Conn,
                #{adm_user_id => 977010},
                AppId,
                <<"bootstrap">>,
                #{
                    confirmed_owner_user_id => 977001,
                    enabled => true,
                    authorization_expires_at => Until,
                    expected_version => 0
                }
            )
        ),
        {ok, _, _} = epgsql:squery(Conn, "COMMIT")
    catch
        Class:Reason:Stack ->
            _ = epgsql:squery(Conn, "ROLLBACK"),
            erlang:raise(Class, Reason, Stack)
    end,
    UserCount = scalar(Conn, <<"SELECT count(*) AS value FROM \"user\"">>),
    WorkspaceCount = scalar(Conn, <<"SELECT count(*) AS value FROM workspace">>),
    Body = (body(1))#{
        <<"organization">> => #{
            <<"external_id">> => <<"bootstrap">>,
            <<"name">> => <<"Bootstrapped">>,
            <<"version">> => 1
        }
    },
    ?assertMatch({ok, applied, _}, tx(Conn, Ctx, <<"fixture-bootstrap">>, Body)),
    ?assertMatch({ok, replay, _}, tx(Conn, Ctx, <<"fixture-bootstrap">>, Body)),
    ?assertEqual(
        1,
        scalar(Conn, <<
            "SELECT count(*) AS value FROM enterprise_directory_organization_source s"
            " JOIN organization_member m ON m.organization_id=s.organization_id"
            " JOIN enterprise_application_grant g ON g.id=s.bootstrap_grant_id"
            " WHERE s.external_organization_id='bootstrap' AND m.user_id=977001 AND m.role='owner'"
            " AND g.provisioning_source='directory_bootstrap' AND g.workspace_scope_kind='none'"
        >>)
    ),
    ?assertEqual(
        1,
        scalar(Conn, <<
            "SELECT count(*) AS value FROM enterprise_application_grant_scope gs"
            " JOIN enterprise_directory_organization_source s ON gs.grant_id=s.bootstrap_grant_id"
            " WHERE s.external_organization_id='bootstrap' AND gs.scope='identities:write'"
        >>)
    ),
    ?assertEqual(UserCount, scalar(Conn, <<"SELECT count(*) AS value FROM \"user\"">>)),
    ?assertEqual(
        0,
        scalar(Conn, <<
            "SELECT count(*) AS value FROM enterprise_application_grant_scope gs"
            " JOIN enterprise_directory_organization_source s ON gs.grant_id=s.bootstrap_grant_id"
            " WHERE s.external_organization_id='bootstrap'"
            " AND gs.scope IN ('oa:login:issue','sso:exchange')"
        >>)
    ),
    ?assertEqual(WorkspaceCount, scalar(Conn, <<"SELECT count(*) AS value FROM workspace">>)),
    combined_login_permission(Conn, Ctx),
    disable_bootstrap(Conn, Ctx, AppId, Until, Body).

combined_login_permission(Conn, #{application_id := App, credential_id := Cred}) ->
    ?assertMatch({error, _}, enterprise_oa_login_repo:authority_tx(Conn, App, Cred)),
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    try
        {ok, _} = enterprise_application_grant_repo:create_tx(Conn, 977100, App, #{
            scopes => [<<"oa:login:issue">>],
            workspace_scope_kind => none,
            workspace_ids => [],
            expires_at => <<"2099-01-01T00:00:00Z">>,
            idempotency_key => <<"explicit-oa-source-login">>
        }),
        ?assertMatch({ok, _}, enterprise_oa_login_repo:authority_tx(Conn, App, Cred))
    after
        {ok, _, _} = epgsql:squery(Conn, "ROLLBACK")
    end,
    ?assertMatch({error, _}, enterprise_oa_login_repo:authority_tx(Conn, App, Cred)).

disable_bootstrap(Conn, Ctx, AppId, Until, Body) ->
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    try
        ?assertMatch(
            {ok, _},
            enterprise_directory_source_config_logic:configure_source_tx(
                Conn,
                #{adm_user_id => 977010},
                AppId,
                <<"bootstrap">>,
                #{
                    confirmed_owner_user_id => 977001,
                    enabled => false,
                    authorization_expires_at => Until,
                    expected_version => 1
                }
            )
        ),
        {ok, _, _} = epgsql:squery(Conn, "COMMIT")
    catch
        DisableClass:DisableReason:DisableStack ->
            _ = epgsql:squery(Conn, "ROLLBACK"),
            erlang:raise(DisableClass, DisableReason, DisableStack)
    end,
    ?assertMatch({error, _}, tx(Conn, Ctx, <<"fixture-bootstrap">>, Body)),
    ?assertEqual(
        1,
        scalar(Conn, <<
            "SELECT count(*) AS value FROM enterprise_directory_organization_source s"
            " JOIN enterprise_application_grant g ON g.id=s.bootstrap_grant_id"
            " WHERE s.external_organization_id='bootstrap' AND NOT s.enabled AND g.status='revoked'"
        >>)
    ).

concurrent_revocations(State, Ctx) ->
    CredId = maps:get(credential_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    Cases = [
        {<<"UPDATE enterprise_application_credential SET status='revoked',revoked_at=clock_timestamp() WHERE id=$1">>,
            <<"UPDATE enterprise_application_credential SET status='active',revoked_at=NULL WHERE id=$1">>,
            [CredId]},
        {<<"UPDATE enterprise_directory_organization_source SET enabled=false WHERE application_id=$1 AND external_organization_id='fixture'">>,
            <<"UPDATE enterprise_directory_organization_source SET enabled=true WHERE application_id=$1 AND external_organization_id='fixture'">>,
            [AppId]},
        {<<"UPDATE enterprise_application_grant SET status='revoked',revoked_at=clock_timestamp(),revoked_by_user_id=977001 WHERE id=$1">>,
            <<"UPDATE enterprise_application_grant SET status='active',revoked_at=NULL,revoked_by_user_id=NULL WHERE id=$1">>,
            [977200]}
    ],
    lists:foreach(
        fun({Revoke, Restore, Params}) ->
            revocation_wait(State, Ctx, Revoke, Restore, Params)
        end,
        Cases
    ).

revocation_wait(State, Ctx, Revoke, Restore, Params) ->
    Conn = maps:get(conn, State),
    Before = snapshot(Conn),
    Server = maps:get(server, State),
    Options = maps:merge(
        maps:with([host, port, username, password], Server),
        maps:get(connect_extra, Server, #{})
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Options#{database => maps:get(db, State)}),
    {ok, _, _} = epgsql:squery(Other, "SET statement_timeout='10s'"),
    Parent = self(),
    Ref = make_ref(),
    try
        {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
        {ok, _} = elib_pg:query(Conn, Revoke, Params),
        Worker = spawn(fun() ->
            Result =
                try
                    tx(Other, Ctx, <<"fixture-concurrent-revoke">>, body(3))
                catch
                    Class:Reason -> {worker_exception, Class, Reason}
                end,
            Parent ! {Ref, Result}
        end),
        try
            await_blocked(Conn, epgsql:get_backend_pid(Other), 250),
            {ok, _, _} = epgsql:squery(Conn, "COMMIT"),
            receive
                {Ref, Result} -> ?assertMatch({error, _}, Result)
            after 12000 -> error(directory_revocation_worker_timeout)
            end,
            ?assertEqual(Before, snapshot(Conn)),
            {ok, _} = elib_pg:query(Conn, Restore, Params)
        after
            exit(Worker, kill)
        end
    after
        _ = epgsql:squery(Conn, "ROLLBACK"),
        epgsql:close(Other)
    end.

await_blocked(_, _, 0) ->
    error(directory_sync_did_not_wait_on_revocation);
await_blocked(Conn, BackendPid, Attempts) ->
    {ok, [#{<<"blocked">> := Blocked}]} = elib_pg:query(
        Conn,
        <<"SELECT pg_backend_pid()=ANY(pg_blocking_pids($1)) AS blocked">>,
        [BackendPid]
    ),
    case Blocked of
        true -> ok;
        false -> receive
            after 20 -> await_blocked(Conn, BackendPid, Attempts - 1)
            end
    end.
