-module(enterprise_internal_signature_http_tests).
-include_lib("eunit/include/eunit.hrl").

signature_http_test_() ->
    {timeout, 900, fun() -> isolated_pg_fixture:run(fun run/0) end}.

run() ->
    S = intbe02_http_support:setup_all(),
    try
        C = maps:get(conn, S),
        enterprise_internal_audit_http_checks:reset(),
        transport(S),
        credential_lock_order(S),
        _ = enterprise_internal_wiring_http_tests:signature_positive_chain(S),
        webhook_application_boundary(S),
        ok = enterprise_internal_audit_http_checks:directory_sync(S),
        enterprise_directory_member_http_checks:run(S),
        directory_reads(S),
        bootstrap(S),
        ticket(S),
        credential_lifecycle(S),
        ?assertMatch(
            {ok, [#{<<"n">> := 0}]},
            elib_pg:query(
                C,
                <<"SELECT count(*) AS n FROM information_schema.columns WHERE table_name='enterprise_application' AND column_name='allowed_scopes'">>,
                []
            )
        ),
        ?assertMatch(
            {ok, [#{<<"n">> := 0}]},
            elib_pg:query(
                C,
                <<"SELECT count(*) AS n FROM enterprise_application_grant WHERE status='active'">>,
                []
            )
        )
    after
        intbe02_http_support:teardown_all(S),
        inttest_marker_db:release(S)
    end.

credential_lock_order(#{conn := C, port := Port, app_a := App, cred_a := Credential}) ->
    Path = <<"/api/internal/v1/application">>,
    Headers = signed(
        Credential,
        <<"GET">>,
        Path,
        <<>>,
        #{<<"x-imboy-organization">> => <<"995101">>},
        sha256
    ),
    exec(C, <<"BEGIN">>, []),
    try
        {ok, [_]} = elib_pg:query(
            C,
            <<"SELECT id FROM enterprise_application WHERE id=$1 FOR UPDATE">>,
            [App]
        ),
        Parent = self(),
        Ref = make_ref(),
        spawn(fun() ->
            Parent ! {Ref, intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, Headers)}
        end),
        await_application_lock(C, erlang:monotonic_time(millisecond) + 10000),
        exec(C, <<"SET LOCAL lock_timeout='2s'">>, []),
        %% A real administrator holding App can update its credential while
        %% authentication waits for App. Credential -> App would deadlock here.
        exec(
            C,
            <<"UPDATE enterprise_application_credential SET last_used_at=clock_timestamp() WHERE application_id=$1">>,
            [App]
        ),
        exec(C, <<"COMMIT">>, []),
        receive
            {Ref, Reply} -> ?assertEqual(200, maps:get(status, Reply))
        after 20000 -> error(credential_lock_order_timeout)
        end
    after
        exec(C, <<"ROLLBACK">>, [])
    end.

await_application_lock(C, Deadline) ->
    {ok, _} = elib_pg:query(C, <<"SELECT pg_stat_clear_snapshot()">>, []),
    {ok, [#{<<"waiting">> := Waiting}]} = elib_pg:query(
        C,
        <<"SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock' AND query LIKE '%FOR SHARE OF o,a,c%') AS waiting">>,
        []
    ),
    case Waiting of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    timer:sleep(20),
                    await_application_lock(C, Deadline);
                false ->
                    error(authentication_did_not_wait_for_application)
            end
    end.

webhook_application_boundary(#{conn := C, port := Port, cred_c := Credential, app_c := App}) ->
    {ok, [#{<<"same_principal">> := true}]} = elib_pg:query(
        C,
        <<"SELECT a.principal_user_id=d.bot_id::text::bigint AS same_principal FROM enterprise_application a JOIN (SELECT substring(bot_id from 6) AS bot_id FROM bot_delivery WHERE delivery_id='intbe02-dlv-0001') d ON true WHERE a.id=$1">>,
        [App]
    ),
    Reply = post(
        Port,
        Credential,
        <<"/api/internal/v1/webhook/deliveries/intbe02-dlv-0001/replay">>,
        #{},
        <<"signed-foreign-app-replay">>
    ),
    ?assertEqual(404, maps:get(status, Reply)).

transport(#{port := Port, cred_a := Credential} = S) ->
    Path = <<"/api/internal/v1/application">>,
    H = signed(Credential, <<"GET">>, Path, <<>>, #{}, sha256),
    [_, Secret] = binary:split(Credential, <<".">>),
    {ok, Parsed} = enterprise_internal_signature:parse(H, erlang:system_time(second)),
    Canonical = enterprise_internal_signature:canonical(<<"GET">>, Path, <<>>, <<>>, Parsed#{
        headers => H
    }),
    UnsupportedCanonical = binary:replace(Canonical, <<"\nsha256\n">>, <<"\nsha512\n">>),
    UnsupportedHex = binary:encode_hex(
        crypto:mac(hmac, sha512, Secret, UnsupportedCanonical), lowercase
    ),
    Unsupported = H#{<<"x-imboy-signature">> => <<"sha512=", UnsupportedHex/binary>>},
    err(
        intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, Unsupported),
        <<"invalid_credential">>
    ),
    Missing = intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, #{}),
    err(Missing, <<"invalid_credential">>),
    Bearer = #{<<"authorization">> => <<"Bearer ", Credential/binary>>},
    err(intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, Bearer), <<"invalid_credential">>),
    err(
        intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, maps:merge(H, Bearer)),
        <<"invalid_credential">>
    ),
    First = intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, H),
    ?assertEqual(200, maps:get(status, First)),
    Payload = jsone:decode(maps:get(body, First)),
    ?assertEqual(<<"hmac">>, maps:get(<<"authentication">>, Payload)),
    ?assertNot(maps:is_key(<<"granted_scopes">>, Payload)),
    err(intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, H), <<"invalid_credential">>),
    H2 = signed(Credential, <<"GET">>, Path, <<>>, #{}, sha256),
    err(
        intbe02_http_support:http(Port, <<"GET">>, <<Path/binary, "?altered=1">>, <<>>, H2),
        <<"invalid_credential">>
    ),
    BadOrg = signed(
        Credential,
        <<"GET">>,
        Path,
        <<>>,
        #{<<"x-imboy-organization">> => <<"995102">>},
        sha256
    ),
    err(
        intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, BadOrg),
        <<"organization_boundary_violation">>
    ),
    Concurrent = signed(Credential, <<"GET">>, Path, <<>>, #{}, sha256),
    concurrent(Port, Path, Concurrent),
    mutate_body(S, Credential),
    lifecycle(S, Credential).

concurrent(Port, Path, Headers) ->
    Parent = self(),
    Tag = make_ref(),
    Pids = [
        spawn(fun() ->
            receive
                go ->
                    R = intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, Headers),
                    Parent ! {Tag, self(), maps:get(status, R)}
            end
        end)
     || _ <- lists:seq(1, 8)
    ],
    [P ! go || P <- Pids],
    Statuses = [
        receive
            {Tag, P, Code} -> Code
        after 20000 -> error(nonce_timeout)
        end
     || P <- Pids
    ],
    ?assertEqual([200 | lists:duplicate(7, 401)], lists:sort(Statuses)).

mutate_body(#{port := Port}, Credential) ->
    Path = <<"/api/internal/v1/directory/sync">>,
    H = signed(
        Credential,
        <<"POST">>,
        Path,
        <<"{}">>,
        #{<<"idempotency-key">> => <<"tampered-body">>},
        sha256
    ),
    err(
        intbe02_http_support:http(Port, <<"POST">>, Path, <<"{\"changed\":true}">>, H),
        <<"invalid_credential">>
    ).

lifecycle(#{conn := C, port := Port, app_a := App}, Credential) ->
    Path = <<"/api/internal/v1/application">>,
    exec(C, <<"UPDATE enterprise_application SET status='disabled' WHERE id=$1">>, [App]),
    H = signed(Credential, <<"GET">>, Path, <<>>, #{}, sha256),
    err(intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, H), <<"application_disabled">>),
    exec(C, <<"UPDATE enterprise_application SET status='active' WHERE id=$1">>, [App]).

directory_reads(#{port := Port, cred_a := Credential}) ->
    [
        ok_json(
            intbe02_http_support:http(
                Port,
                <<"GET">>,
                Path,
                <<>>,
                intbe02_http_support:auth(Credential)
            )
        )
     || Path <- [
            <<"/api/internal/v1/directory/organizations">>,
            <<"/api/internal/v1/directory/departments?external_organization_id=audit-directory-org">>,
            <<"/api/internal/v1/directory/persons?external_organization_id=audit-directory-org">>
        ]
    ].

bootstrap(#{conn := C, port := Port}) ->
    {ok, AppRow} = elib_pg:with_tx(fun(Conn) ->
        enterprise_internal_ops:create_application_tx(
            Conn, null, <<"synthetic-signature-connection">>, <<"Synthetic OA signature">>
        )
    end),
    App = maps:get(<<"id">>, AppRow),
    {ok, #{credential := Credential}} = elib_pg:with_tx(fun(Conn) ->
        enterprise_internal_ops:issue_credential_tx(Conn, null, App, undefined)
    end),
    exec(
        C,
        <<"INSERT INTO enterprise_directory_organization_source(application_id,external_organization_id,confirmed_owner_user_id,enabled,authorization_expires_at) VALUES($1,'signed-initial-org',995001,true,'2099-01-01')">>,
        [App]
    ),
    Init = #{
        <<"organization">> => #{
            <<"external_id">> => <<"signed-initial-org">>,
            <<"name">> => <<"Synthetic signed organization">>,
            <<"version">> => 1
        },
        <<"departments">> => [],
        <<"persons">> => []
    },
    Reply = post(
        Port, Credential, <<"/api/internal/v1/directory/sync">>, Init, <<"signed-initial">>
    ),
    #{<<"organization_id">> := Org} = ok_json(Reply),
    List = ok_json(
        intbe02_http_support:http(
            Port,
            <<"GET">>,
            <<"/api/internal/v1/workspaces">>,
            <<>>,
            intbe02_http_support:auth(Credential)
        )
    ),
    ?assertMatch(#{<<"items">> := []}, List),
    New = ok_json(
        post(
            Port,
            Credential,
            <<"/api/internal/v1/workspaces">>,
            #{<<"owner_user_id">> => 995001, <<"name">> => <<"Signed workspace">>},
            <<"signed-workspace">>
        )
    ),
    ?assert(is_integer(maps:get(<<"workspace_id">>, New))),
    ?assertMatch(
        {ok, [#{<<"n">> := 0}]},
        elib_pg:query(
            C,
            <<"SELECT count(*) AS n FROM enterprise_application_grant WHERE application_id=$1">>,
            [App]
        )
    ),
    ?assert(is_integer(Org)),
    multi_source(C, Port, App, Credential, Org).

ticket(#{port := Port, cred_sso := Credential, app_sso := App}) ->
    Verifier = <<"dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk">>,
    Result = post(
        Port,
        Credential,
        <<"/api/internal/v1/oa/login/tickets">>,
        #{
            <<"external_user_id">> => <<"signed-pending-person">>,
            <<"intent">> => <<"login">>,
            <<"code_challenge_method">> => <<"S256">>,
            <<"code_challenge">> => enterprise_oa_login_logic:pkce(Verifier)
        },
        undefined
    ),
    #{<<"code">> := 0, <<"payload">> := #{<<"ticket">> := Ticket}} = ok_json(Result),
    {ok, [#{<<"application_key">> := Key}]} = elib_pg:query(
        <<"SELECT application_key FROM enterprise_application WHERE id=$1">>, [App]
    ),
    ?assertMatch(
        {ok, #{<<"requires_account_choice">> := true}},
        enterprise_oa_login_logic:exchange(#{
            <<"ticket">> => Ticket,
            <<"application_key">> => Key,
            <<"code_verifier">> => Verifier,
            <<"did">> => <<"signed-synthetic-device">>
        })
    ).

post(Port, Credential, Path, Body, Key) ->
    H = intbe02_http_support:auth(Credential),
    Headers =
        case Key of
            undefined -> H;
            _ -> H#{<<"idempotency-key">> => Key}
        end,
    intbe02_http_support:http(Port, <<"POST">>, Path, Body, Headers).
signed(Credential, M, P, B, H, A) ->
    intbe02_http_support:signed_headers(
        Credential,
        M,
        P,
        B,
        H#{<<"content-type">> => <<"application/json">>},
        A
    ).
ok_json(Reply) ->
    ?assertEqual(200, maps:get(status, Reply), Reply),
    jsone:decode(maps:get(body, Reply)).
err(Reply, Code) ->
    ?assertEqual(enterprise_internal_error:http_status(Code), maps:get(status, Reply)),
    ?assertMatch(#{<<"error">> := #{<<"code">> := Code}}, jsone:decode(maps:get(body, Reply))).
exec(C, Sql, Params) -> intbe02_http_support:sql_exec(C, Sql, Params).

multi_source(C, Port, App, Credential, Org) ->
    exec(
        C,
        <<"INSERT INTO enterprise_directory_organization_source(application_id,external_organization_id,organization_id,enabled,authorization_expires_at) VALUES($1,'signed-second-org',995102,true,'2099-01-01')">>,
        [App]
    ),
    Path = <<"/api/internal/v1/workspaces">>,
    err(
        intbe02_http_support:http(
            Port, <<"GET">>, Path, <<>>, intbe02_http_support:auth(Credential)
        ),
        <<"organization_boundary_violation">>
    ),
    H = signed(
        Credential,
        <<"GET">>,
        Path,
        <<>>,
        #{<<"x-imboy-organization">> => integer_to_binary(Org)},
        sha256
    ),
    #{<<"items">> := Items} = ok_json(intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, H)),
    ?assertEqual(1, length(Items)),
    Other = signed(
        Credential,
        <<"GET">>,
        Path,
        <<>>,
        #{<<"x-imboy-organization">> => <<"995101">>},
        sha256
    ),
    err(
        intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, Other),
        <<"organization_boundary_violation">>
    ),
    Tampered = H#{<<"x-imboy-organization">> => <<"995102">>},
    err(intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, Tampered), <<"invalid_credential">>).

credential_lifecycle(#{conn := C, port := Port, app_a := App}) ->
    {ok, #{credential := Old, credential_id := OldId}} = elib_pg:with_tx(fun(Conn) ->
        enterprise_internal_ops:issue_credential_tx(Conn, 995101, App, undefined)
    end),
    {ok, #{credential := New, credential_id := NewId}} = elib_pg:with_tx(fun(Conn) ->
        enterprise_internal_ops:rotate_credential_tx(Conn, 995101, OldId)
    end),
    Path = <<"/api/internal/v1/application">>,
    err(
        intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, intbe02_http_support:auth(Old)),
        <<"invalid_credential">>
    ),
    _ = ok_json(
        intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, intbe02_http_support:auth(New))
    ),
    ok = elib_pg:with_tx(fun(Conn) ->
        enterprise_internal_ops:revoke_credential_tx(Conn, 995101, NewId)
    end),
    err(
        intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, intbe02_http_support:auth(New)),
        <<"invalid_credential">>
    ),
    ?assertMatch(
        {ok, [#{<<"n">> := 0}]},
        elib_pg:query(
            C,
            <<"SELECT count(*) AS n FROM enterprise_internal_request_nonce WHERE credential_id=$1">>,
            [OldId]
        )
    ).
