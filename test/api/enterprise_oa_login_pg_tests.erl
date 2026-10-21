-module(enterprise_oa_login_pg_tests).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

oa_login_pg_test_() ->
    {timeout, 600, fun() ->
        S = intbe02_http_support:setup_all(),
        try
            run(S)
        after
            intbe02_http_support:teardown_all(S),
            inttest_marker_db:release(S)
        end
    end}.

run(S) ->
    CacheOwner =
        case whereis(imboy_cache_sync) of
            undefined ->
                {ok, CachePid} = imboy_cache_sync:start_link(),
                CachePid;
            _ ->
                undefined
        end,
    try
        run_with_cache(S)
    after
        case CacheOwner of
            undefined -> ok;
            _ -> gen_server:stop(CacheOwner)
        end
    end.

run_with_cache(S) ->
    migration_roundtrip(),
    %% Match production startup: the marker database has no runtime account sequence.
    ok = account_ds:init(),
    [#{<<"exists">> := true}] = rows(
        maps:get(conn, S),
        <<"SELECT to_regclass('public.imboy_account_id_seq') IS NOT NULL AS exists">>,
        []
    ),
    Owned =
        case whereis(imboy_account_allocator) of
            undefined ->
                case imboy_account_allocator:start_link() of
                    {ok, Pid} -> Pid;
                    {error, {already_started, _}} -> undefined
                end;
            _ ->
                undefined
        end,
    try
        run_checks(S)
    after
        case Owned of
            undefined -> ok;
            _ -> gen_server:stop(Owned)
        end
    end.

run_checks(S) ->
    C = maps:get(conn, S),
    App = maps:get(app_sso, S),
    [#{<<"id">> := Cred}] = rows(
        C,
        <<
            "SELECT id FROM enterprise_application_credential "
            "WHERE application_id=$1 AND status='active'"
        >>,
        [App]
    ),
    Ctx = #{application_id => App, credential_id => Cred},
    exec(C, <<"UPDATE enterprise_application_credential SET organization_id=NULL WHERE id=$1">>, [
        Cred
    ]),
    source_mixup(S, Ctx),
    pending_authority_expiry(S, Ctx),
    deadlines(S, Ctx),
    login_existing(S, Ctx),
    explicit_link(S, Ctx),
    explicit_register(S, Ctx),
    concurrent_register(S, Ctx),
    projected_after_authorization(S, Ctx),
    issuer_revocation(S, Ctx),
    migration_preserves_data(S),
    ok.

migration_roundtrip() ->
    {ok, Down} = file:read_file("priv/migrations/00000181_enterprise_oa_login.down.sql"),
    {ok, Up} = file:read_file("priv/migrations/00000181_enterprise_oa_login.up.sql"),
    ok = elib_pg:with_tx(fun(C) ->
        exec(
            C,
            <<"ALTER TABLE enterprise_application ADD COLUMN allowed_scopes jsonb NOT NULL DEFAULT '[]'">>,
            []
        ),
        [?assertEqual(ok, element(1, Result)) || Result <- epgsql:squery(C, Down)],
        [?assertEqual(ok, element(1, Result)) || Result <- epgsql:squery(C, Up)],
        exec(C, <<"ALTER TABLE enterprise_application DROP COLUMN allowed_scopes">>, []),
        ok
    end).

migration_preserves_data(S) ->
    C = maps:get(conn, S),
    Sql = <<"SELECT to_jsonb(t) AS row FROM enterprise_oa_login_ticket t ORDER BY digest">>,
    Before = rows(C, Sql, []),
    ?assert(Before =/= []),
    {ok, Down} = file:read_file("priv/migrations/00000181_enterprise_oa_login.down.sql"),
    exec(C, <<"BEGIN">>, []),
    try
        exec(
            C,
            <<"ALTER TABLE enterprise_application ADD COLUMN allowed_scopes jsonb NOT NULL DEFAULT '[]'">>,
            []
        ),
        Results = epgsql:squery(C, Down),
        Errors = [Error || {error, Error} <- Results],
        ?assert(lists:any(fun(Error) -> eb_pg_exec:error_of(Error) =:= <<"23514">> end, Errors))
    after
        exec(C, <<"ROLLBACK">>, [])
    end,
    ?assertEqual(Before, rows(C, Sql, [])).

pending_authority_expiry(S, Ctx) ->
    C = maps:get(conn, S),
    Cred = maps:get(credential_id, Ctx),
    P = ticket(Ctx, <<"pending-authority-expiry">>, <<"login">>, <<"expiry-device">>),
    PendingAudits = rows(
        C,
        <<"SELECT id FROM enterprise_audit_event WHERE action='oa.login_pending' ORDER BY id">>,
        []
    ),
    exec(
        C,
        <<
            "CREATE FUNCTION oa_pending_delay() RETURNS trigger LANGUAGE plpgsql AS $$ "
            "BEGIN IF NEW.action='oa.login_pending' THEN PERFORM pg_sleep(2); END IF; RETURN NEW; END $$"
        >>,
        []
    ),
    exec(
        C,
        <<
            "CREATE TRIGGER oa_pending_delay BEFORE INSERT ON enterprise_audit_event "
            "FOR EACH ROW EXECUTE FUNCTION oa_pending_delay()"
        >>,
        []
    ),
    exec(
        C,
        <<
            "UPDATE enterprise_application_credential "
            "SET expires_at=clock_timestamp()+interval '1 second' WHERE id=$1"
        >>,
        [Cred]
    ),
    try
        Result = enterprise_oa_login_logic:exchange(P),
        ?assert(
            case Result of
                {error, _} -> true;
                _ -> false
            end
        ),
        [#{<<"stage">> := <<"issued">>}] = rows(
            C,
            <<"SELECT stage FROM enterprise_oa_login_ticket WHERE digest=$1">>,
            [crypto:hash(sha256, maps:get(<<"ticket">>, P))]
        ),
        ?assertEqual(
            PendingAudits,
            rows(
                C,
                <<
                    "SELECT id FROM enterprise_audit_event "
                    "WHERE action='oa.login_pending' ORDER BY id"
                >>,
                []
            )
        )
    after
        exec(C, <<"DROP TRIGGER oa_pending_delay ON enterprise_audit_event">>, []),
        exec(C, <<"DROP FUNCTION oa_pending_delay()">>, []),
        exec(C, <<"UPDATE enterprise_application_credential SET expires_at=NULL WHERE id=$1">>, [
            Cred
        ])
    end.

login_existing(S, Ctx) ->
    Ext = intbe02_http_support:fixture(ext_alice, S),
    P = ticket(Ctx, Ext, <<"login">>, <<"oa-device-login">>),
    ?assertMatch(
        {error, _}, enterprise_oa_login_logic:exchange(P#{<<"code_verifier">> => verifier_bad()})
    ),
    {ok, Session} = enterprise_oa_login_logic:exchange(P),
    Uid = intbe02_http_support:fixture(alice, S),
    ?assertEqual(Uid, maps:get(<<"user_id">>, Session)),
    ?assertMatch(
        {ok, Uid, _, <<"tk">>, <<"oa-device-login">>, _},
        token_ds:decrypt_token(maps:get(<<"token">>, Session))
    ),
    ?assertMatch({error, _}, enterprise_oa_login_logic:exchange(P)),
    C = maps:get(conn, S),
    ?assertEqual(
        1,
        length(
            rows(
                C,
                <<
                    "SELECT id FROM user_device WHERE user_id=$1 "
                    "AND device_id='oa-device-login' AND status=1"
                >>,
                [Uid]
            )
        )
    ),
    ?assertEqual(
        1,
        length(
            rows(
                C,
                <<
                    "SELECT id FROM enterprise_audit_event WHERE resource_id=$1 "
                    "AND action='oa.login_completed'"
                >>,
                [maps:get(application_id, Ctx)]
            )
        )
    ).

explicit_link(S, Ctx) ->
    C = maps:get(conn, S),
    Uid = intbe02_http_support:fixture(h_a1, S),
    {ok, _} = elib_pg:with_tx(fun(Tx) ->
        enterprise_oa_login_repo:device_tx(Tx, Uid, <<"oa-link-device">>, #{})
    end),
    {ok, #{<<"epoch">> := Epoch}} = elib_pg:with_tx(fun(Tx) ->
        enterprise_oa_login_repo:epoch_tx(Tx, Uid)
    end),
    seed_pending(S, Ctx),
    Members = rows(
        C,
        <<"SELECT to_jsonb(m) AS row FROM organization_member m ORDER BY organization_id,user_id">>,
        []
    ),
    P = ticket(Ctx, <<"new-link">>, <<"link">>, <<"oa-link-device">>),
    {ok, Pending} = enterprise_oa_login_logic:exchange(P),
    ?assertNot(maps:is_key(<<"token">>, Pending)),
    Complete = maps:merge(P, Pending),
    ?assertMatch(
        {error, _}, enterprise_oa_login_logic:register(Complete#{<<"confirm_create">> => true})
    ),
    {ok, Session} = enterprise_oa_login_logic:link(
        Uid, <<"oa-link-device">>, {Epoch, erlang:system_time(second) + 60}, Complete
    ),
    ?assertEqual(Uid, maps:get(<<"user_id">>, Session)),
    ?assertEqual(false, maps:get(<<"projection_pending">>, Session)),
    AfterMembers = rows(
        C,
        <<"SELECT to_jsonb(m) AS row FROM organization_member m ORDER BY organization_id,user_id">>,
        []
    ),
    ?assertEqual([], Members -- AfterMembers),
    [#{<<"user_id">> := Uid}] = rows(
        C,
        <<
            "SELECT user_id FROM enterprise_source_person "
            "WHERE application_id=$1 AND external_user_id='new-link'"
        >>,
        [maps:get(application_id, Ctx)]
    ),
    Conflict = ticket(Ctx, <<"new-link">>, <<"link">>, <<"oa-device-login">>),
    {ok, Pending2} = enterprise_oa_login_logic:exchange(Conflict),
    Other = intbe02_http_support:fixture(alice, S),
    ?assertMatch(
        {error, _},
        enterprise_oa_login_logic:link(
            Other,
            <<"oa-device-login">>,
            {1, erlang:system_time(second) + 60},
            maps:merge(Conflict, Pending2)
        )
    ).

explicit_register(S, Ctx) ->
    P = ticket(Ctx, <<"new-register">>, <<"login">>, <<"oa-register-device">>),
    {ok, Pending} = enterprise_oa_login_logic:exchange(P),
    Complete = maps:merge(P, Pending),
    ?assertEqual(
        {error, explicit_confirmation_required}, enterprise_oa_login_logic:register(Complete)
    ),
    C = maps:get(conn, S),
    Before = user_count(C),
    exec(
        C,
        <<
            "CREATE FUNCTION oa_audit_reject() RETURNS trigger LANGUAGE plpgsql AS $$ "
            "BEGIN IF NEW.action='oa.identity_linked' THEN RAISE EXCEPTION 'synthetic'; END IF; RETURN NEW; END $$"
        >>,
        []
    ),
    exec(
        C,
        <<
            "CREATE TRIGGER oa_audit_reject BEFORE INSERT ON enterprise_audit_event "
            "FOR EACH ROW EXECUTE FUNCTION oa_audit_reject()"
        >>,
        []
    ),
    try
        ?assertMatch(
            {error, _}, enterprise_oa_login_logic:register(Complete#{<<"confirm_create">> => true})
        ),
        ?assertEqual(Before, user_count(C)),
        ?assertEqual(
            [],
            rows(
                C,
                <<
                    "SELECT user_id FROM enterprise_source_person "
                    "WHERE application_id=$1 AND external_user_id='new-register'"
                >>,
                [maps:get(application_id, Ctx)]
            )
        )
    after
        exec(C, <<"DROP TRIGGER oa_audit_reject ON enterprise_audit_event">>, []),
        exec(C, <<"DROP FUNCTION oa_audit_reject()">>, [])
    end,
    {ok, Created} = enterprise_oa_login_logic:register(Complete#{<<"confirm_create">> => true}),
    ?assertEqual(Before + 1, user_count(C)),
    ?assertMatch(
        {error, _}, enterprise_oa_login_logic:register(Complete#{<<"confirm_create">> => true})
    ),
    Again = ticket(Ctx, <<"new-register">>, <<"login">>, <<"oa-register-device">>),
    {ok, Repeated} = enterprise_oa_login_logic:exchange(Again),
    ?assertEqual(maps:get(<<"user_id">>, Created), maps:get(<<"user_id">>, Repeated)),
    Token = maps:get(<<"token">>, Repeated),
    Uid = maps:get(<<"user_id">>, Repeated),
    ?assertMatch(
        {ok, Uid, <<"oa-register-device">>}, auth_ds:verify_token(<<"Bearer ", Token/binary>>)
    ),
    ok = user_device_logic:delete(Uid, <<"oa-register-device">>),
    ?assertMatch({error, _, _}, auth_ds:verify_token(<<"Bearer ", Token/binary>>)).

issuer_revocation(S, Ctx) ->
    P = ticket(
        Ctx, intbe02_http_support:fixture(ext_alice, S), <<"login">>, <<"oa-revoked-device">>
    ),
    C = maps:get(conn, S),
    App = maps:get(application_id, Ctx),
    exec(
        C,
        <<
            "UPDATE enterprise_application SET status='disabled' WHERE id=$1"
        >>,
        [App]
    ),
    ?assertMatch({error, _}, enterprise_oa_login_logic:exchange(P)),
    exec(C, <<"UPDATE enterprise_application SET status='active' WHERE id=$1">>, [App]),
    Expired = ticket(
        Ctx, intbe02_http_support:fixture(ext_alice, S), <<"login">>, <<"oa-expired-device">>
    ),
    exec(
        C,
        <<
            "UPDATE enterprise_oa_login_ticket SET expires_at=clock_timestamp()-interval '1 second' "
            "WHERE digest=$1"
        >>,
        [crypto:hash(sha256, maps:get(<<"ticket">>, Expired))]
    ),
    ?assertMatch({error, _}, enterprise_oa_login_logic:exchange(Expired)),
    exec(C, <<"UPDATE enterprise_application SET status='disabled' WHERE id=$1">>, [
        maps:get(application_id, Ctx)
    ]),
    ?assertMatch({error, _}, enterprise_oa_login_logic:exchange(P)),
    ?assertEqual(
        [], rows(C, <<"SELECT id FROM user_device WHERE device_id='oa-revoked-device'">>, [])
    ).

ticket(Ctx, Ext, Intent, Did) ->
    {ok, #{<<"ticket">> := Ticket}} = enterprise_oa_login_logic:issue(Ctx, params(Ext, Intent)),
    {ok, [#{<<"application_key">> := Key}]} = elib_pg:query(
        <<"SELECT application_key FROM enterprise_application WHERE id=$1">>,
        [maps:get(application_id, Ctx)]
    ),
    #{
        <<"ticket">> => Ticket,
        <<"code_verifier">> => verifier(),
        <<"did">> => Did,
        <<"application_key">> => Key
    }.
params(Ext, Intent) ->
    #{
        <<"external_user_id">> => Ext,
        <<"intent">> => Intent,
        <<"code_challenge_method">> => <<"S256">>,
        <<"code_challenge">> => enterprise_oa_login_logic:pkce(verifier())
    }.
verifier() -> <<"dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk">>.
verifier_bad() -> <<"eBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk">>.
rows(C, Sql, P) ->
    {ok, R} = elib_pg:query(C, Sql, P),
    R.
exec(C, Sql, P) ->
    case elib_pg:execute(C, Sql, P) of
        {ok, _} -> ok;
        Other -> erlang:error(Other)
    end.
user_count(C) ->
    [#{<<"n">> := N}] = rows(C, <<"SELECT count(*) AS n FROM \"user\"">>, []),
    N.

seed_pending(S, Ctx) ->
    C = maps:get(conn, S),
    App = maps:get(application_id, Ctx),
    {ok, _} = enterprise_source_person_repo:sync_tx(
        C, App, <<"new-link">>, <<"Synthetic">>, <<"active">>, 1
    ),
    lists:foreach(
        fun(Org) ->
            exec(
                C,
                <<
                    "INSERT INTO enterprise_directory_organization_source "
                    "(application_id,external_organization_id,organization_id,enabled,authorization_expires_at) "
                    "VALUES($1,$2,$3,true,'2099-01-01')"
                >>,
                [App, integer_to_binary(Org), Org]
            ),
            ok = enterprise_directory_source_repo:save_membership_tx(
                C,
                App,
                Org,
                <<"new-link">>,
                false,
                null,
                #{
                    <<"version">> => 1,
                    <<"status">> => <<"active">>,
                    <<"department_external_ids">> => []
                }
            )
        end,
        [intbe02_http_support:fixture(org_a, S), intbe02_http_support:fixture(org_b, S)]
    ).

projected_after_authorization(S, Ctx) ->
    C = maps:get(conn, S),
    App = maps:get(application_id, Ctx),
    Before = user_count(C),
    {ok, Session} = enterprise_oa_login_logic:exchange(
        ticket(Ctx, <<"new-link">>, <<"login">>, <<"oa-link-device">>)
    ),
    ?assertEqual(false, maps:get(<<"projection_pending">>, Session)),
    ?assertEqual(Before, user_count(C)),
    [A, B] = rows(
        C,
        <<
            "SELECT organization_id,projected_user_id FROM enterprise_directory_membership_source "
            "WHERE application_id=$1 AND external_user_id='new-link' ORDER BY organization_id"
        >>,
        [App]
    ),
    ?assertEqual(intbe02_http_support:fixture(h_a1, S), maps:get(<<"projected_user_id">>, A)),
    ?assertEqual(intbe02_http_support:fixture(h_a1, S), maps:get(<<"projected_user_id">>, B)).

concurrent_register(S, Ctx) ->
    C = maps:get(conn, S),
    Before = user_count(C),
    Parent = self(),
    Inputs = [
        begin
            P = ticket(Ctx, <<"parallel-register">>, <<"login">>, Did),
            {ok, Pending} = enterprise_oa_login_logic:exchange(P),
            (maps:merge(P, Pending))#{<<"confirm_create">> => true}
        end
     || Did <- [<<"oa-race-a">>, <<"oa-race-b">>]
    ],
    Workers = [
        spawn(fun() ->
            receive
                go ->
                    Parent ! {self(), enterprise_oa_login_logic:register(P)}
            end
        end)
     || P <- Inputs
    ],
    lists:foreach(fun(Pid) -> Pid ! go end, Workers),
    Results = [
        receive
            {Pid, Result} -> Result
        after 60000 -> erlang:error(concurrent_timeout)
        end
     || Pid <- Workers
    ],
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(1, length([error || {error, _} <- Results])),
    ?assertEqual(Before + 1, user_count(C)).

deadlines(S, Ctx) ->
    C = maps:get(conn, S),
    P = ticket(Ctx, <<"deadline-person">>, <<"login">>, <<"oa-deadline-device">>),
    D = crypto:hash(sha256, maps:get(<<"ticket">>, P)),
    [#{<<"seconds">> := Seconds}] = rows(
        C,
        <<
            "SELECT extract(epoch FROM expires_at-created_at)::int AS seconds "
            "FROM enterprise_oa_login_ticket WHERE digest=$1"
        >>,
        [D]
    ),
    ?assertEqual(60, Seconds),
    {ok, Pending} = enterprise_oa_login_logic:exchange(P),
    ?assertEqual(300, maps:get(<<"expires_in">>, Pending)),
    PD = crypto:hash(sha256, maps:get(<<"pending_token">>, Pending)),
    [#{<<"remaining">> := Remaining, <<"expires_at">> := Deadline}] = rows(
        C,
        <<
            "SELECT extract(epoch FROM expires_at-clock_timestamp())::int AS remaining,expires_at "
            "FROM enterprise_oa_login_ticket WHERE digest=$1"
        >>,
        [PD]
    ),
    ?assert(Remaining > 290 andalso Remaining =< 300),
    Complete = maps:merge(P, Pending),
    ?assertMatch({error, _}, enterprise_oa_login_logic:register(Complete)),
    [#{<<"expires_at">> := Deadline}] = rows(
        C, <<"SELECT expires_at FROM enterprise_oa_login_ticket WHERE digest=$1">>, [PD]
    ),
    exec(
        C,
        <<"UPDATE enterprise_oa_login_ticket SET expires_at=clock_timestamp()-interval '1 second' WHERE digest=$1">>,
        [PD]
    ),
    Before = user_count(C),
    ?assertMatch(
        {error, _}, enterprise_oa_login_logic:register(Complete#{<<"confirm_create">> => true})
    ),
    ?assertEqual(Before, user_count(C)),
    Expired = ticket(Ctx, <<"expired-issued">>, <<"login">>, <<"oa-expired-issued">>),
    ED = crypto:hash(sha256, maps:get(<<"ticket">>, Expired)),
    exec(
        C,
        <<"UPDATE enterprise_oa_login_ticket SET expires_at=clock_timestamp()-interval '1 second' WHERE digest=$1">>,
        [ED]
    ),
    ?assertMatch({error, _}, enterprise_oa_login_logic:exchange(Expired)),
    [#{<<"stage">> := <<"issued">>}] = rows(
        C, <<"SELECT stage FROM enterprise_oa_login_ticket WHERE digest=$1">>, [ED]
    ).

source_mixup(S, CtxA) ->
    C = maps:get(conn, S),
    AppB = maps:get(app_ro, S),
    [#{<<"id">> := CredB}] = rows(
        C,
        <<"SELECT id FROM enterprise_application_credential WHERE application_id=$1 AND status='active'">>,
        [AppB]
    ),
    PA = ticket(CtxA, <<"same-source-person">>, <<"login">>, <<"mixup-device">>),
    PB = ticket(
        #{application_id => AppB, credential_id => CredB},
        <<"same-source-person">>,
        <<"login">>,
        <<"mixup-device">>
    ),
    ?assertEqual(
        {error, invalid_request},
        enterprise_oa_login_logic:exchange(
            PB#{<<"application_key">> => maps:get(<<"application_key">>, PA)}
        )
    ),
    {ok, PendingB} = enterprise_oa_login_logic:exchange(PB),
    ?assert(maps:is_key(<<"pending_token">>, PendingB)),
    ?assertEqual(
        {error, invalid_request},
        enterprise_oa_login_logic:register(
            (maps:merge(PB, PendingB))#{
                <<"confirm_create">> => true,
                <<"application_key">> => maps:get(<<"application_key">>, PA)
            }
        )
    ).

%% Focused gap coverage only: no migration roundtrip or account allocator path.
oa_login_three_gap_test_() ->
    isolated_pg_fixture:test(fun oa_login_three_gap_fixture/0).

oa_login_three_gap_fixture() ->
    {timeout, 900,
        {setup,
            fun() ->
                isolated_pg_fixture:setup_single_node_cache(
                    fun enterprise_oa_login_three_gap_pg_checks:setup/0,
                    fun enterprise_oa_login_three_gap_pg_checks:teardown/1
                )
            end,
            fun enterprise_oa_login_three_gap_pg_checks:teardown/1, fun(S) ->
                Ctx = maps:get(oa_gap_context, S),
                Ticket = fun(Ext, Intent, Did) -> ticket(Ctx, Ext, Intent, Did) end,
                Issue = fun() ->
                    enterprise_oa_login_logic:issue(
                        Ctx, params(<<"three-gap-issue-person">>, <<"login">>)
                    )
                end,
                {inorder, [
                    {"link wrong application key rejects without effects",
                        {timeout, 900, fun() ->
                            enterprise_oa_login_three_gap_pg_checks:link_wrong_key(S, Ticket)
                        end}},
                    {"issue authority expires before ticket return",
                        {timeout, 900, fun() ->
                            enterprise_oa_login_three_gap_pg_checks:issue_deadline(S, Issue)
                        end}},
                    {"authority expires before final session return",
                        {timeout, 900, fun() ->
                            enterprise_oa_login_three_gap_pg_checks:session_deadline(S, Ticket)
                        end}}
                ]}
            end}}.

%% Explicit source removal is distinct from organization exit/account tombstones.
source_retirement_test_() ->
    isolated_pg_fixture:test(fun source_retirement_fixture/0).

source_retirement_fixture() ->
    {timeout, 900,
        {setup, fun enterprise_oa_login_source_retirement_pg_checks:setup/0,
            fun enterprise_oa_login_source_retirement_pg_checks:cleanup/1, fun(H) ->
                Ticket = fun(Ctx, Ext, Did) -> ticket(Ctx, Ext, <<"login">>, Did) end,
                {timeout, 900, fun() ->
                    enterprise_oa_login_source_retirement_pg_checks:run(H, Ticket)
                end}
            end}}.
