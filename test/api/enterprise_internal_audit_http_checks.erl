%% Audit insertion failures must roll back the same real HTTP transaction.
-module(enterprise_internal_audit_http_checks).
-export([reset/0, assert_complete/0, http/6, request/5, directory_sync/1, oa_login/1, seats/1]).
-include_lib("eunit/include/eunit.hrl").

reset() ->
    put(?MODULE, []),
    ok.

assert_complete() ->
    Required = enterprise_internal_audit_policy:required_ids(),
    ?assertEqual(28, length(Required)),
    ?assertEqual(
        lists:sort(Required),
        lists:sort(seen())
    ),
    io:format("INTERNAL_REQUIRED_AUDIT_RESULT=ok operations=~B~n", [length(Required)]).

seats(S) ->
    Path = <<"/api/internal/v1/customer-service/seats">>,
    Workspace = intbe02_http_support:fixture(ws_a1, S),
    Created = request(
        S,
        <<"POST">>,
        Path,
        #{<<"workspace_id">> => Workspace, <<"business_identity_id">> => 995701},
        <<"audit-seat-create">>
    ),
    #{<<"business_identity_id">> := 995701, <<"version">> := Version} =
        jsone:decode(maps:get(body, Created)),
    Updated = request(
        S,
        <<"PATCH">>,
        <<Path/binary, "/995701">>,
        #{
            <<"workspace_id">> => Workspace,
            <<"expected_version">> => Version,
            <<"enabled">> => false
        },
        <<"audit-seat-update">>
    ),
    #{<<"enabled">> := false, <<"version">> := Next} = jsone:decode(maps:get(body, Updated)),
    ?assertEqual(Version + 1, Next).

directory_sync(#{conn := C, app_a := App} = S) ->
    Org = intbe02_http_support:fixture(org_a, S),
    ok = intbe02_http_support:sql_exec(
        C,
        <<
            "INSERT INTO enterprise_directory_organization_source "
            "(application_id,external_organization_id,organization_id,enabled,authorization_expires_at) "
            "VALUES($1,'audit-directory-org',$2,true,'2099-01-01T00:00:00Z')"
        >>,
        [App, Org]
    ),
    Reply = request(
        S,
        <<"POST">>,
        <<"/api/internal/v1/directory/sync">>,
        #{
            <<"organization">> => #{
                <<"external_id">> => <<"audit-directory-org">>,
                <<"name">> => <<"Synthetic audited directory">>,
                <<"version">> => 1
            },
            <<"departments">> => [],
            <<"persons">> => []
        },
        <<"audit-directory-sync">>
    ),
    ?assertEqual(200, maps:get(status, Reply)),
    ?assertEqual(
        #{<<"organization_id">> => Org, <<"synced">> => true},
        jsone:decode(maps:get(body, Reply))
    ),
    intbe02_http_support:cover(<<"INT-43">>, ok).

oa_login(#{conn := C, port := Port, app_sso := App, cred_sso := Credential}) ->
    Path = <<"/api/internal/v1/oa/login/tickets">>,
    Auth = intbe02_http_support:auth(Credential),
    Params = #{
        <<"external_user_id">> => <<"audit-oa-login">>,
        <<"intent">> => <<"login">>,
        <<"code_challenge_method">> => <<"S256">>,
        <<"code_challenge">> => enterprise_oa_login_logic:pkce(
            <<"dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk">>
        )
    },
    Post = fun(Body, Headers) ->
        intbe02_http_support:http(Port, <<"POST">>, Path, Body, Headers)
    end,
    assert_error(Post(Params, #{}), <<"invalid_credential">>),
    assert_error(Post(Params, Auth), <<"insufficient_scope">>),
    ok = intbe02_http_support:sql_exec(
        C,
        <<
            "UPDATE enterprise_application SET allowed_scopes=allowed_scopes || "
            "'[\"oa:login:issue\"]'::jsonb WHERE id=$1"
        >>,
        [App]
    ),
    assert_error(Post(Params, Auth), <<"insufficient_scope">>),
    ok = intbe02_http_support:sql_exec(
        C,
        <<
            "INSERT INTO enterprise_application_grant_scope(grant_id,scope) "
            "SELECT id,'oa:login:issue' FROM enterprise_application_grant WHERE application_id=$1"
        >>,
        [App]
    ),
    Before = snapshot(C),
    assert_error(Post(Params#{<<"intent">> => <<"invalid">>}, Auth), <<"invalid_request">>),
    ?assertEqual(Before, snapshot(C)),
    _ = http(C, Port, <<"POST">>, Path, Params, Auth),
    intbe02_http_support:cover(<<"INT-44">>, ok).

assert_error(Reply, Code) ->
    ?assertEqual(enterprise_internal_error:http_status(Code), maps:get(status, Reply)),
    #{<<"error">> := #{<<"code">> := Actual}} = jsone:decode(maps:get(body, Reply)),
    ?assertEqual(Code, Actual).

request(S, M, P, B, K) ->
    H = maps:merge(
        intbe02_http_support:auth(maps:get(cred_a, S)),
        intbe02_http_support:idem(K)
    ),
    http(maps:get(conn, S), maps:get(port, S), M, P, B, H).

http(C, Port, M, P, B, H) ->
    case enterprise_internal_routes:match(M, P) of
        {ok, #{id := Id}} ->
            case
                lists:member(Id, enterprise_internal_audit_policy:required_ids()) andalso
                    not lists:member(Id, seen())
            of
                true -> audited(C, Port, M, P, B, H, Id);
                false -> intbe02_http_support:http(Port, M, P, B, H)
            end;
        _ ->
            intbe02_http_support:http(Port, M, P, B, H)
    end.

seen() ->
    case get(?MODULE) of
        undefined -> [];
        L -> L
    end.

audited(C, Port, M, P, B, H, Id) ->
    Before = snapshot(C),
    BeforeTickets = ticket_count(C),
    AuditCount = audit_count(C, Id),
    reject_audits(C, true),
    try
        Failed = intbe02_http_support:http(Port, M, P, B, H),
        ?assertEqual(500, maps:get(status, Failed), {audit_failure, Id}),
        Json = jsone:decode(maps:get(body, Failed)),
        ?assertEqual(<<"internal_error">>, maps:get(<<"code">>, maps:get(<<"error">>, Json))),
        ?assertEqual(Before, snapshot(C), {audit_rollback, Id})
    after
        reject_audits(C, false)
    end,
    First = intbe02_http_support:http(Port, M, P, B, H),
    ?assertEqual(200, maps:get(status, First), {audit_restored, Id}),
    ?assertEqual(AuditCount + 1, audit_count(C, Id), {required_audit_inserted, Id}),
    After = snapshot(C),
    TicketCount = ticket_count(C),
    case Id of
        <<"INT-44">> -> ?assertEqual(BeforeTickets + 1, TicketCount);
        _ -> ok
    end,
    Replay = intbe02_http_support:http(Port, M, P, B, H),
    assert_replay(C, Id, First, Replay, {After, TicketCount, AuditCount + 1}),
    put(?MODULE, [Id | seen()]),
    record(Id),
    First.

assert_replay(C, <<"INT-44">> = Id, First, Replay, {_After, TicketCount, AuditCount}) ->
    ?assertNotEqual(ticket(First), ticket(Replay)),
    ?assertNotEqual(
        <<"true">>, intbe02_http_support:json_header_val(Replay, <<"idempotent-replayed">>)
    ),
    ?assertEqual(TicketCount + 1, ticket_count(C)),
    ?assertEqual(AuditCount + 1, audit_count(C, Id));
assert_replay(C, Id, First, Replay, {After, _TicketCount, _AuditCount}) ->
    case Id of
        <<"INT-14">> ->
            ?assertEqual(404, maps:get(status, Replay)),
            ?assertEqual(
                <<"resource_not_found">>,
                maps:get(<<"code">>, maps:get(<<"error">>, jsone:decode(maps:get(body, Replay))))
            );
        _ ->
            ?assertEqual(200, maps:get(status, Replay)),
            ?assertEqual(maps:get(body, First), maps:get(body, Replay)),
            ?assertEqual(
                <<"true">>, intbe02_http_support:json_header_val(Replay, <<"idempotent-replayed">>)
            )
    end,
    ?assertEqual(After, snapshot(C), {replay_no_write, Id}).

ticket(Reply) ->
    ?assertEqual(200, maps:get(status, Reply)),
    #{<<"code">> := 0, <<"payload">> := #{<<"ticket">> := Ticket, <<"expires_in">> := 60}} =
        jsone:decode(maps:get(body, Reply)),
    ?assert(is_binary(Ticket)),
    ?assertEqual(43, byte_size(Ticket)),
    Ticket.

ticket_count(C) ->
    #{<<"n">> := Count} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM enterprise_oa_login_ticket">>
    ),
    Count.

audit_count(C, Id) ->
    Table =
        case Id of
            I when I =:= <<"INT-35">>; I =:= <<"INT-36">> -> <<"customer_service_event">>;
            _ -> <<"enterprise_audit_event">>
        end,
    Filter =
        case Id of
            <<"INT-44">> -> <<"action=$1 AND resource_type='enterprise_application'">>;
            <<"INT-43">> -> <<"action=$1 AND resource_type='directory_sync'">>;
            _ -> <<"organization_id=995101 AND action=$1">>
        end,
    {ok, [Row]} = elib_pg:query(
        C,
        <<"SELECT count(*) AS n FROM ", Table/binary, " WHERE ", Filter/binary>>,
        [enterprise_internal_audit_policy:audit_action(Id)]
    ),
    maps:get(<<"n">>, Row).

reject_audits(C, true) ->
    lists:foreach(
        fun(T) ->
            ok = intbe02_http_support:sql_exec(
                C,
                <<"ALTER TABLE ", T/binary,
                    " ADD CONSTRAINT synthetic_matrix_audit_reject CHECK(false) NOT VALID">>
            )
        end,
        [<<"enterprise_audit_event">>, <<"customer_service_event">>]
    );
reject_audits(C, false) ->
    lists:foreach(
        fun(T) ->
            ok = intbe02_http_support:sql_exec(
                C,
                <<"ALTER TABLE ", T/binary, " DROP CONSTRAINT synthetic_matrix_audit_reject">>
            )
        end,
        [<<"enterprise_audit_event">>, <<"customer_service_event">>]
    ).

snapshot(C) ->
    {ok, Tables} = elib_pg:query(
        C,
        <<"SELECT tablename FROM pg_tables WHERE schemaname='public' ORDER BY tablename">>,
        []
    ),
    [
        begin
            T = maps:get(<<"tablename">>, Row),
            %% Credential use time legitimately changes on every authenticated request.
            Expr =
                case T of
                    <<"enterprise_application_credential">> -> <<"to_jsonb(t)-'last_used_at'">>;
                    _ -> <<"to_jsonb(t)">>
                end,
            Safe = binary:replace(T, <<"\"">>, <<"\"\"">>, [global]),
            Sql =
                <<"SELECT md5(COALESCE(jsonb_agg(", Expr/binary, " ORDER BY ", Expr/binary,
                    "::text)::text,'[]')) AS digest FROM public.\"", Safe/binary, "\" t">>,
            {ok, [Digest]} = elib_pg:query(C, Sql, []),
            {T, maps:get(<<"digest">>, Digest)}
        end
     || Row <- Tables
    ].

record(Id) ->
    case os:getenv("IMBOY_GATE_RUN_DIR") of
        false ->
            ok;
        Dir ->
            ok = file:write_file(
                filename:join(Dir, "internal-audit-matrix.jsonl"),
                [
                    jsone:encode(#{
                        id => Id,
                        audit_failure_status => 500,
                        rollback => true,
                        retry_status => 200,
                        replay_no_write => Id =/= <<"INT-44">>,
                        replay_semantics =>
                            case Id of
                                <<"INT-44">> -> fresh_ticket;
                                <<"INT-14">> -> consumed_code_rejected;
                                _ -> idempotent_replay
                            end
                    }),
                    "\n"
                ],
                [append]
            )
    end.
