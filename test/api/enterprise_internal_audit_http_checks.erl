%% Audit insertion failures must roll back the same real HTTP transaction.
-module(enterprise_internal_audit_http_checks).
-export([reset/0, assert_complete/0, http/6, request/5]).
-include_lib("eunit/include/eunit.hrl").

reset() ->
    put(?MODULE, []),
    ok.

assert_complete() ->
    ?assertEqual(
        lists:sort(enterprise_internal_audit_policy:required_ids()),
        lists:sort(get(?MODULE))
    ),
    io:format("INTERNAL_REQUIRED_AUDIT_RESULT=ok operations=25~n").

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
    Replay = intbe02_http_support:http(Port, M, P, B, H),
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
    ?assertEqual(After, snapshot(C), {replay_no_write, Id}),
    put(?MODULE, [Id | seen()]),
    record(Id),
    First.

audit_count(C, Id) ->
    Table =
        case Id of
            I when I =:= <<"INT-35">>; I =:= <<"INT-36">> -> <<"customer_service_event">>;
            _ -> <<"enterprise_audit_event">>
        end,
    {ok, [Row]} = elib_pg:query(
        C,
        <<"SELECT count(*) AS n FROM ", Table/binary,
            " WHERE organization_id=995101 AND action=$1">>,
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
                        replay_no_write => true
                    }),
                    "\n"
                ],
                [append]
            )
    end.
