-module(enterprise_internal_self_auth_tests).

%% Real authentication/Grant/scope orchestration, mocked persistence and rate I/O.
%% These unit tests do not establish HTTP or PostgreSQL business acceptance.
-include_lib("eunit/include/eunit.hrl").

-define(MOCKS, [
    enterprise_application_credential_repo,
    enterprise_application_repo,
    enterprise_application_grant_repo,
    enterprise_internal_rate,
    elib_pg,
    elib_log,
    elib_dt
]).
-define(SCOPES, [<<"application:read">>, <<"webhooks:manage">>, <<"sso:exchange">>]).

self_auth_test_() ->
    Positive = [fun() -> archived_self_allowed(Id) end || Id <- self_ids()],
    Cases =
        Positive ++
            [
                fun revoked/0,
                fun expired/0,
                fun bad_secret/0,
                fun disabled_application/0,
                fun no_grants/0,
                fun failed_grant_read/0,
                fun missing_scope/0,
                fun allowed_scopes_intersection/0,
                fun rate_limited/0,
                fun missing_idempotency/0,
                fun resource_scope_retained/0,
                fun direct_auth_ignores_org/0,
                fun exception_propagates/0,
                fun directory_admission_only/0,
                fun directory_admission_fail_closed/0
            ],
    {foreach, fun setup/0, fun cleanup/1, [fun(_) -> ?_test(Case()) end || Case <- Cases]}.

self_ids() ->
    [maps:get(id, R) || R <- enterprise_internal_routes:routes()].

setup() ->
    ok = meck:new(?MOCKS, [non_strict, no_link]),
    credential(#{}),
    meck:expect(
        enterprise_application_credential_repo,
        digest_hex,
        fun(S) -> crypto:hash(sha256, S) end
    ),
    meck:expect(enterprise_application_credential_repo, touch_last_used_tx, 2, ok),
    meck:expect(enterprise_application_repo, find_by_id_tx, 2, {ok, app()}),
    meck:expect(enterprise_application_grant_repo, grant_governed_tx, 3, {ok, true}),
    grants({ok, ?SCOPES}),
    meck:expect(enterprise_internal_rate, check, 2, {ok, 1}),
    meck:expect(elib_pg, query, 3, {ok, [#{<<"status">> => <<"archived">>}]}),
    meck:expect(elib_log, internal_log, 4, ok),
    meck:expect(elib_dt, now, 0, <<"2026-10-14T00:00:00Z">>),
    meck:expect(elib_dt, compare_rfc3339, 3, false),
    ok.

cleanup(_) ->
    meck:unload(?MOCKS).

credential(Changes) ->
    Row = #{
        <<"id">> => 1,
        <<"organization_id">> => 2,
        <<"application_id">> => 3,
        <<"status">> => <<"active">>,
        <<"expires_at">> => null,
        <<"secret_digest">> => crypto:hash(sha256, <<"synthetic-secret">>)
    },
    meck:expect(
        enterprise_application_credential_repo,
        find_by_prefix_tx,
        2,
        {ok, maps:merge(Row, Changes)}
    ).

app() ->
    #{
        <<"organization_id">> => 2,
        <<"status">> => <<"active">>,
        <<"application_key">> => <<"fixture">>,
        <<"allowed_scopes">> => <<"[\"application:read\",\"webhooks:manage\",\"sso:exchange\"]">>
    }.

grants(Result) ->
    meck:expect(enterprise_application_grant_repo, effective_scopes_tx, 3, Result).

request(Id) ->
    request(Id, #{<<"idempotency-key">> => <<"fixture-key">>}).

request(Id, Extra) ->
    [Route] = [R || R <- enterprise_internal_routes:routes(), maps:get(id, R) =:= Id],
    Headers = Extra#{<<"authorization">> => <<"Bearer ib_int_1.synthetic-secret">>},
    Result = enterprise_internal_auth:decide(
        maps:get(method, Route),
        maps:get(path, Route),
        Headers,
        fun() ->
            enterprise_internal_auth:authenticate_tx(
                fixture_conn, <<"ib_int_1">>, <<"synthetic-secret">>
            )
        end
    ),
    Result.

archived_self_allowed(Id) ->
    Scopes = enterprise_internal_scope:all(),
    grants({ok, Scopes}),
    meck:expect(
        enterprise_application_repo,
        find_by_id_tx,
        2,
        {ok, (app())#{<<"allowed_scopes">> => jsone:encode(Scopes)}}
    ),
    ?assertMatch({ok, #{route_id := Id, organization_id := 2}}, request(Id)),
    ?assertEqual(0, meck:num_calls(elib_pg, query, '_')).

revoked() ->
    credential(#{<<"status">> => <<"revoked">>}),
    ?assertEqual({error, invalid_credential}, request(<<"INT-01">>)).

expired() ->
    credential(#{<<"expires_at">> => <<"2020-01-01T00:00:00Z">>}),
    meck:expect(elib_dt, compare_rfc3339, 3, true),
    ?assertEqual({error, credential_expired}, request(<<"INT-01">>)).

bad_secret() ->
    credential(#{<<"secret_digest">> => crypto:hash(sha256, <<"other-secret">>)}),
    ?assertEqual({error, invalid_credential}, request(<<"INT-01">>)).

disabled_application() ->
    meck:expect(
        enterprise_application_repo,
        find_by_id_tx,
        2,
        {ok, (app())#{<<"status">> => <<"disabled">>}}
    ),
    ?assertEqual({error, application_disabled}, request(<<"INT-01">>)).

no_grants() ->
    grants({ok, []}),
    ?assertEqual({error, insufficient_scope}, request(<<"INT-01">>)).

failed_grant_read() ->
    grants({error, unavailable}),
    ?assertEqual({error, security_gate_closed}, request(<<"INT-01">>)).

missing_scope() ->
    grants({ok, [<<"application:read">>]}),
    ?assertEqual({error, insufficient_scope}, request(<<"INT-12">>)).

rate_limited() ->
    meck:expect(enterprise_internal_rate, check, 2, {limited, 1000}),
    ?assertEqual({error, rate_limited}, request(<<"INT-01">>)),
    ?assertEqual(1, enterprise_internal_auth:take_retry_after_seconds()).

missing_idempotency() ->
    ?assertEqual({error, invalid_request}, request(<<"INT-12">>, #{})).

resource_scope_retained() ->
    ?assertMatch({ok, _}, request(<<"INT-01">>)),
    ?assertEqual({error, insufficient_scope}, request(<<"INT-04">>)),
    ?assertEqual(0, meck:num_calls(elib_pg, query, '_')).

direct_auth_ignores_org() ->
    ?assertMatch(
        {ok, #{application_id := 3}},
        enterprise_internal_auth:authenticate_tx(
            fixture_conn, <<"ib_int_1">>, <<"synthetic-secret">>
        )
    ).

exception_propagates() ->
    meck:expect(
        enterprise_application_credential_repo,
        find_by_prefix_tx,
        fun(_, _) -> error(injected) end
    ),
    ?assertException(error, injected, request(<<"INT-01">>)).

allowed_scopes_intersection() ->
    meck:expect(
        enterprise_application_repo,
        find_by_id_tx,
        2,
        {ok, (app())#{<<"allowed_scopes">> => <<"[]">>}}
    ),
    ?assertEqual({error, insufficient_scope}, request(<<"INT-01">>)).

directory_admission_only() ->
    grants({ok, []}),
    meck:expect(
        enterprise_application_repo,
        find_by_id_tx,
        2,
        {ok, (app())#{
            <<"organization_id">> => null,
            <<"allowed_scopes">> => <<"[\"identities:write\"]">>
        }}
    ),
    ?assertMatch({ok, #{granted_scopes := [], route_id := <<"INT-43">>}}, request(<<"INT-43">>)),
    ?assertEqual({error, insufficient_scope}, request(<<"INT-02">>)),
    ?assertEqual({error, invalid_request}, request(<<"INT-43">>, #{})),
    meck:expect(enterprise_internal_rate, check, 2, {limited, 1000}),
    ?assertEqual({error, rate_limited}, request(<<"INT-43">>)).

directory_admission_fail_closed() ->
    grants({ok, []}),
    ?assertEqual({error, insufficient_scope}, request(<<"INT-43">>)),
    meck:expect(
        enterprise_application_repo,
        find_by_id_tx,
        2,
        {ok, (app())#{<<"allowed_scopes">> => <<"[\"identities:write\"]">>}}
    ),
    credential(#{<<"status">> => <<"revoked">>}),
    ?assertEqual({error, invalid_credential}, request(<<"INT-43">>)).
