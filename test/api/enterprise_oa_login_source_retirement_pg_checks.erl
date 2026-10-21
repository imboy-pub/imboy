-module(enterprise_oa_login_source_retirement_pg_checks).
-export([setup/0, cleanup/1, run/2]).
-include_lib("eunit/include/eunit.hrl").
-define(H, intbe02_http_support).
-define(UID, 995011).
-define(EXT_A, <<"intbe02-ext-h1">>).
-define(EXT_B, <<"c09-retire-source-b">>).
-define(DID_A, <<"c09-source-A-held">>).
-define(DID_B, <<"c09-source-B-held">>).

setup() ->
    quiet(),
    ?assertEqual(undefined, whereis(imboy_cache)),
    try
        setup_marker()
    catch
        Class:Reason:Stack ->
            %% Unknown marker before original H returns is preserved for the parent.
            cleanup_step(fun() -> stop_cache(whereis(imboy_cache)) end),
            erlang:raise(Class, Reason, Stack)
    end.

setup_marker() ->
    H = ?H:setup_all(),
    Cache = whereis(imboy_cache),
    try
        ?assert(is_pid(Cache)),
        H#{source_removal_cache => Cache}
    catch
        Class:Reason:Stack ->
            cleanup_step(fun() -> cleanup(H#{source_removal_cache => Cache}) end),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(H) ->
    Results = [
        cleanup_step(fun() -> ?H:teardown_all(H) end),
        cleanup_step(fun() -> inttest_marker_db:release(H) end),
        cleanup_step(fun() -> stop_cache(maps:get(source_removal_cache, H)) end)
    ],
    io:format("C09_SOURCE_REMOVAL_CLEANUP ~p~n", [Results]),
    ?assertEqual([true, true, true], Results),
    quiet().

cleanup_step(F) ->
    try
        ok = F(),
        true
    catch
        Class:_ ->
            io:format("C09_SOURCE_REMOVAL_CLEANUP_FAILED_CLASS ~p~n", [Class]),
            false
    end.

stop_cache(undefined) ->
    ok;
stop_cache(Pid) when is_pid(Pid) ->
    ?assertEqual(Pid, whereis(imboy_cache)),
    Ref = erlang:monitor(process, Pid),
    ok = gen_server:stop(Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 -> erlang:error(own_cache_not_stopped)
    end,
    ?assertEqual(undefined, whereis(imboy_cache)).

run(H, Ticket) ->
    quiet(),
    C = maps:get(conn, H),
    A = authorize(C, maps:get(app_a, H)),
    B = authorize(C, maps:get(app_sso, H)),
    prepare_directory(C, A),
    prepare_b(C, B),
    assert_person(C, A, ?EXT_A, <<"active">>, 0),
    assert_no_membership(C, A),
    sync(A, 1, <<"active">>),
    assert_person(C, A, ?EXT_A, <<"active">>, 1),
    assert_projection(C, A, 1),
    assert_no_membership(C, A),
    Control = Ticket(A, ?EXT_A, <<"c09-source-A-positive">>),
    assert_session(exchange(Control), <<"c09-source-A-positive">>),
    HeldA = Ticket(A, ?EXT_A, ?DID_A),
    HeldB = Ticket(B, ?EXT_B, ?DID_B),
    Headers = headers(),
    assert_live_issuer(C, A, HeldA),
    assert_live_issuer(C, B, HeldB),
    Preserved = unrelated(C, B),
    sync(A, 2, <<"removed">>),
    assert_person(C, A, ?EXT_A, <<"removed">>, 2),
    assert_projection(C, A, 2),
    assert_no_membership(C, A),
    ?assertEqual(Preserved, unrelated(C, B)),
    assert_live_issuer(C, A, HeldA),
    assert_live_issuer(C, B, HeldB),
    assert_denials(H, A, B, HeldA, HeldB, Headers),
    quiet(),
    ?assertEqual(maps:get(source_removal_cache, H), whereis(imboy_cache)),
    io:format("C09_SOURCE_REMOVAL_SCOPE removed_status_only_no_tombstone_no_org_exit~n"),
    ok.

authorize(C, App) ->
    [#{<<"id">> := Cred}] = rows(
        C,
        <<
            "SELECT id FROM enterprise_application_credential "
            "WHERE application_id=$1 AND status='active'"
        >>,
        [App]
    ),
    [#{<<"id">> := Cred}] = rows(
        C,
        <<
            "UPDATE enterprise_application_credential "
            "SET organization_id=NULL WHERE id=$1 RETURNING id"
        >>,
        [Cred]
    ),
    Ctx = #{application_id => App, credential_id => Cred, authentication => hmac},
    {ok, _} = elib_pg:with_tx(fun(Tx) -> enterprise_oa_login_repo:authority_tx(Tx, App, Cred) end),
    Ctx.

prepare_directory(C, #{application_id := App}) ->
    exec(
        C,
        <<
            "INSERT INTO organization(id,name,owner_id,status) "
            "VALUES(995903,'Synthetic source target',995021,'active')"
        >>,
        []
    ),
    exec(
        C,
        <<
            "INSERT INTO enterprise_directory_organization_source "
            "(application_id,external_organization_id,organization_id,enabled,authorization_expires_at) "
            "VALUES($1,'c09-retire-source',995903,true,clock_timestamp()+interval '1 day')"
        >>,
        [App]
    ).

prepare_b(C, #{application_id := App}) ->
    {ok, #{
        <<"user_id">> := ?UID,
        <<"status">> := <<"active">>,
        <<"binding_retired">> := false
    }} = elib_pg:with_tx(fun(Tx) ->
        enterprise_source_person_repo:claim_tx(Tx, App, ?EXT_B, ?UID)
    end),
    ?assertEqual(
        [],
        rows(
            C,
            <<
                "SELECT organization_id FROM enterprise_directory_membership_source "
                "WHERE application_id=$1 AND external_user_id=$2"
            >>,
            [App, ?EXT_B]
        )
    ).

sync(Ctx, Version, Status) ->
    quiet(),
    Body = #{
        <<"organization">> => #{
            <<"external_id">> => <<"c09-retire-source">>,
            <<"name">> => <<"Synthetic source target">>,
            <<"version">> => Version
        },
        <<"departments">> => [],
        <<"persons">> => [
            #{
                <<"external_user_id">> => ?EXT_A,
                <<"display_name">> => <<"Synthetic source person">>,
                <<"source_status">> => Status,
                <<"source_version">> => Version,
                <<"membership">> => #{
                    <<"version">> => Version,
                    <<"status">> => <<"removed">>,
                    <<"department_external_ids">> => []
                }
            }
        ]
    },
    ?assertEqual(ok, enterprise_directory_sync_logic:validate(Body)),
    {ok, applied, #{response_code := 200}} = enterprise_directory_sync_logic:sync(
        Ctx, <<"c09-source-removal-v", (integer_to_binary(Version))/binary>>, Body
    ),
    quiet().

assert_person(C, #{application_id := App}, Ext, Status, Version) ->
    ?assertEqual(
        [
            #{
                <<"user_id">> => ?UID,
                <<"status">> => Status,
                <<"binding_retired">> => false,
                <<"source_version">> => Version
            }
        ],
        rows(
            C,
            <<
                "SELECT user_id,status,binding_retired,source_version FROM enterprise_source_person "
                "WHERE application_id=$1 AND external_user_id=$2"
            >>,
            [App, Ext]
        )
    ),
    ?assertEqual(
        [#{<<"status">> => 1, <<"account_type">> => 0}],
        rows(C, <<"SELECT status,account_type FROM \"user\" WHERE id=995011">>, [])
    ).

assert_no_membership(C, #{application_id := App}) ->
    ?assertEqual(
        [],
        rows(
            C,
            <<
                "SELECT user_id FROM organization_member "
                "WHERE organization_id=995903 AND user_id=995011"
            >>,
            []
        )
    ),
    ?assertEqual(
        [],
        rows(
            C,
            <<
                "SELECT organization_id FROM enterprise_directory_membership_source "
                "WHERE application_id=$1 AND external_user_id=$2 AND status='active'"
            >>,
            [App, ?EXT_A]
        )
    ),
    ?assertEqual(
        [],
        rows(
            C,
            <<
                "SELECT d.user_id FROM enterprise_directory_department_member_source d "
                "WHERE d.application_id=$1 AND d.user_id=995011"
            >>,
            [App]
        )
    ).

assert_projection(C, #{application_id := App}, Version) ->
    ?assertEqual(
        [
            #{
                <<"source_version">> => Version,
                <<"status">> => <<"removed">>,
                <<"managed">> => false,
                <<"projected_user_id">> => ?UID,
                <<"empty_departments">> => true
            }
        ],
        rows(
            C,
            <<
                "SELECT source_version,status,managed,projected_user_id,"
                "department_external_ids='[]'::jsonb AS empty_departments "
                "FROM enterprise_directory_membership_source WHERE application_id=$1 "
                "AND organization_id=995903 AND external_user_id=$2"
            >>,
            [App, ?EXT_A]
        )
    ),
    ?assertEqual(
        [#{<<"source_version">> => Version, <<"enabled">> => true}],
        rows(
            C,
            <<
                "SELECT source_version,enabled FROM enterprise_directory_organization_source "
                "WHERE application_id=$1 AND organization_id=995903"
            >>,
            [App]
        )
    ).

assert_live_issuer(C, #{application_id := App, credential_id := Cred}, P) ->
    {ok, Authority} = elib_pg:with_tx(fun(Tx) ->
        enterprise_oa_login_repo:authority_tx(Tx, App, Cred)
    end),
    ?assertEqual(maps:get(<<"application_key">>, Authority), maps:get(<<"application_key">>, P)),
    ?assertEqual(
        [
            #{
                <<"stage">> => <<"issued">>,
                <<"live">> => true,
                <<"application_id">> => App,
                <<"credential_id">> => Cred
            }
        ],
        rows(
            C,
            <<
                "SELECT stage,expires_at>clock_timestamp() AS live,application_id,credential_id "
                "FROM enterprise_oa_login_ticket WHERE digest=$1"
            >>,
            [digest(P)]
        )
    ),
    ?assertEqual(
        [
            #{
                <<"challenge">> => enterprise_oa_login_logic:pkce(maps:get(<<"code_verifier">>, P)),
                <<"intent">> => <<"login">>
            }
        ],
        rows(
            C,
            <<
                "SELECT challenge,intent "
                "FROM enterprise_oa_login_ticket WHERE digest=$1"
            >>,
            [digest(P)]
        )
    ).

assert_denials(H, A, B, HeldA, HeldB, Headers) ->
    C = maps:get(conn, H),
    Before = snapshot(C),
    ?assertEqual(
        {error, identity_conflict}, safe_result(enterprise_oa_login_logic:exchange(HeldA))
    ),
    quiet(),
    ?assertEqual(Before, snapshot(C)),
    assert_live_issuer(C, A, HeldA),
    R = ?H:http(maps:get(port, H), <<"POST">>, <<"/api/v1/oa/login/exchange">>, HeldA, Headers),
    Json = jsone:decode(maps:get(body, R)),
    io:format(
        "C09_SOURCE_REMOVAL_HTTP status=~p code=~p~n",
        [maps:get(status, R), maps:get(<<"code">>, Json, undefined)]
    ),
    ?assertEqual(409, maps:get(status, R)),
    ?assertEqual(
        #{<<"code">> => 409, <<"msg">> => <<"identity_conflict">>, <<"payload">> => #{}},
        maps:with([<<"code">>, <<"msg">>, <<"payload">>], Json)
    ),
    ?assert(is_integer(maps:get(<<"sv_ts">>, Json))),
    ?assertEqual(4, map_size(Json)),
    quiet(),
    ?assertEqual(Before, snapshot(C)),
    assert_live_issuer(C, A, HeldA),
    assert_live_issuer(C, B, HeldB),
    Stable = unaffected_after_b(C, A, B, HeldA),
    assert_session(exchange(HeldB), ?DID_B),
    ?assertEqual(Stable, unaffected_after_b(C, A, B, HeldA)),
    ?assertEqual(
        [#{<<"stage">> => <<"consumed">>}],
        rows(
            C,
            <<"SELECT stage FROM enterprise_oa_login_ticket WHERE digest=$1">>,
            [digest(HeldB)]
        )
    ),
    ?assertEqual(
        [#{<<"n">> => 1}],
        rows(
            C,
            <<"SELECT count(*) AS n FROM user_device WHERE user_id=995011 AND device_id=$1 AND status=1">>,
            [?DID_B]
        )
    ).

exchange(P) ->
    quiet(),
    {ok, Session} = enterprise_oa_login_logic:exchange(P),
    quiet(),
    Session.

assert_session(Session, Did) ->
    ?assertEqual(?UID, maps:get(<<"user_id">>, Session)),
    ?assertEqual(false, maps:get(<<"projection_pending">>, Session)),
    {ok, ?UID, _, <<"tk">>, Did, Epoch} = token_ds:decrypt_token(maps:get(<<"token">>, Session)),
    ?assert(is_integer(Epoch) andalso Epoch >= 1),
    ?assertEqual(
        {ok, ?UID, Did},
        auth_ds:verify_token(<<"Bearer ", (maps:get(<<"token">>, Session))/binary>>)
    ),
    {ok, EpochRows} = elib_pg:query(
        <<"SELECT epoch FROM user_auth_epoch WHERE user_id=995011">>, []
    ),
    ?assertEqual([#{<<"epoch">> => Epoch}], EpochRows).

headers() ->
    ok = app_version_ds:set_sign_key(
        <<"synthetic">>,
        <<"c09-removal-r1">>,
        <<"synthetic.c09">>,
        <<"synthetic-c09-source-removal-key">>
    ),
    #{
        <<"cos">> => <<"synthetic">>,
        <<"vsn">> => <<"c09-removal-r1">>,
        <<"pkg">> => <<"synthetic.c09">>,
        <<"did">> => ?DID_A,
        <<"method">> => <<"sha256">>,
        <<"sign">> => elib_hasher:hmac_sha256(
            <<?DID_A/binary, "|c09-removal-r1|synthetic|synthetic.c09">>,
            <<"synthetic-c09-source-removal-key">>
        )
    }.

unrelated(C, #{application_id := B}) ->
    Queries = [
        {<<"SELECT to_jsonb(t) AS row FROM \"user\" t ORDER BY id">>, []},
        {<<"SELECT to_jsonb(t) AS row FROM user_device t ORDER BY id">>, []},
        {<<"SELECT to_jsonb(t) AS row FROM user_auth_epoch t ORDER BY user_id">>, []},
        {<<"SELECT to_jsonb(t) AS row FROM enterprise_external_identity t ORDER BY id">>, []},
        {<<"SELECT to_jsonb(t) AS row FROM enterprise_source_person t WHERE application_id=$1 ORDER BY external_user_id">>,
            [B]},
        {<<"SELECT to_jsonb(t) AS row FROM enterprise_oa_login_ticket t ORDER BY digest">>, []},
        {<<"SELECT to_jsonb(t) AS row FROM organization_member t ORDER BY organization_id,user_id">>,
            []},
        {<<"SELECT to_jsonb(t) AS row FROM organization t WHERE id<>995903 ORDER BY id">>, []}
    ],
    [rows(C, Q, P) || {Q, P} <- Queries].

unaffected_after_b(C, #{application_id := A}, #{application_id := B}, HeldA) ->
    Queries = [
        {<<"SELECT to_jsonb(t) AS row FROM \"user\" t ORDER BY id">>, []},
        {<<"SELECT to_jsonb(t) AS row FROM enterprise_external_identity t ORDER BY id">>, []},
        {<<"SELECT to_jsonb(t) AS row FROM enterprise_source_person t WHERE application_id=ANY($1::bigint[]) ORDER BY application_id,external_user_id">>,
            [[A, B]]},
        {<<"SELECT to_jsonb(t) AS row FROM enterprise_oa_login_ticket t WHERE digest=$1">>, [
            digest(HeldA)
        ]},
        {<<"SELECT to_jsonb(t) AS row FROM organization_member t ORDER BY organization_id,user_id">>,
            []},
        {<<"SELECT to_jsonb(t) AS row FROM workspace_member t ORDER BY workspace_id,user_id">>, []},
        {<<"SELECT to_jsonb(t) AS row FROM enterprise_directory_membership_source t ORDER BY application_id,organization_id,external_user_id">>,
            []}
    ],
    [rows(C, Q, P) || {Q, P} <- Queries].

snapshot(C) ->
    Tables = [
        <<"\"user\"">>,
        <<"user_device">>,
        <<"user_auth_epoch">>,
        <<"enterprise_source_person">>,
        <<"enterprise_external_identity">>,
        <<"enterprise_oa_login_ticket">>,
        <<"organization">>,
        <<"organization_member">>,
        <<"workspace_member">>,
        <<"organization_department">>,
        <<"organization_department_member">>,
        <<"enterprise_directory_organization_source">>,
        <<"enterprise_directory_membership_source">>,
        <<"enterprise_directory_department_source">>,
        <<"enterprise_directory_department_member_source">>,
        <<"enterprise_audit_event">>,
        <<"enterprise_internal_idempotency">>
    ],
    [
        {T,
            rows(
                C,
                <<"SELECT to_jsonb(t) AS row FROM ", T/binary, " t ORDER BY to_jsonb(t)::text">>,
                []
            )}
     || T <- Tables
    ].

digest(P) -> crypto:hash(sha256, maps:get(<<"ticket">>, P)).
safe_result({error, R}) -> {error, R};
safe_result({ok, _}) -> {ok, secret_payload_withheld};
safe_result(_) -> unexpected_result.
rows(C, Sql, Params) ->
    {ok, R} = elib_pg:query(C, Sql, Params),
    R.
exec(C, Sql, Params) ->
    {ok, N} = elib_pg:execute(C, Sql, Params),
    ?assert(N > 0),
    ok.

quiet() ->
    ?assertEqual(undefined, whereis(imboy_cache_sync)),
    ?assertEqual(undefined, whereis(imboy_domain_event)),
    ?assertEqual(undefined, whereis(imboy_sup)),
    ?assertNot(lists:keymember(imboy, 1, application:which_applications())).
