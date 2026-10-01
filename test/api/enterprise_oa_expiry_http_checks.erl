-module(enterprise_oa_expiry_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(S) ->
    C = maps:get(conn, S),
    {Digest, Body} = issue(S),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE enterprise_oa_sso_code SET expires_at=clock_timestamp()+interval '3 seconds' WHERE code_digest=$1">>,
        [Digest]
    ),
    Before = audit_count(C),
    [R] = hold_exchange(S, Digest, Body, 1, true),
    save("oa-expiry-response.json", [R]),
    ?assertEqual(404, maps:get(status, R)),
    ?assertEqual(<<"resource_not_found">>, error_code(R)),
    {ok, [Row]} = enterprise_query(C, Digest),
    ?assertEqual(null, maps:get(<<"consumed_at">>, Row)),
    ?assertEqual(Before, audit_count(C)),
    concurrent_once(S),
    audit_failure_retry(S),
    identity_revocation(S),
    authority_revocation(S),
    credential_deadline(S),
    authority_deadline(S),
    grant_deadline(S),
    new_grant_race(S).

issue(S) ->
    Redirect = <<"https://oa.customer.example.com/sso/cb">>,
    Nonce = <<"expiry_http_nonce_0123456789">>,
    {ok, #{<<"code">> := Code}} = enterprise_oa_sso_logic:issue_code_tx(
        maps:get(conn, S), 995017, #{
            <<"application_key">> => <<"intbe02-oa-sso">>,
            <<"redirect_uri">> => Redirect,
            <<"nonce">> => Nonce
        }
    ),
    {enterprise_oa_sso_code_repo:digest_hex(Code), #{
        <<"code">> => Code, <<"redirect_uri">> => Redirect, <<"nonce">> => Nonce
    }}.

concurrent_once(S) ->
    C = maps:get(conn, S),
    {Digest, Body} = issue(S),
    Before = audit_count(C),
    Results = hold_exchange(S, Digest, Body, 2, false),
    save("oa-concurrent-responses.json", Results),
    ?assertEqual([200, 404], lists:sort([maps:get(status, R) || R <- Results])),
    [Denied] = [R || R <- Results, maps:get(status, R) =:= 404],
    ?assertEqual(<<"resource_not_found">>, error_code(Denied)),
    {ok, [Row]} = enterprise_query(C, Digest),
    ?assertNotEqual(null, maps:get(<<"consumed_at">>, Row)),
    ?assertEqual(Before + 1, audit_count(C)).

audit_failure_retry(S) ->
    C = maps:get(conn, S),
    {Digest, Body} = issue(S),
    Before = audit_count(C),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"CREATE FUNCTION gate_drop_oa_audit() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.action='oa.sso.exchanged' THEN RETURN NULL; END IF; RETURN NEW; END $$">>
    ),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"CREATE TRIGGER gate_drop_oa_audit BEFORE INSERT ON enterprise_audit_event FOR EACH ROW EXECUTE FUNCTION gate_drop_oa_audit()">>
    ),
    try
        Failed = exchange(S, Body),
        save("oa-audit-failure-response.json", [Failed]),
        ?assertEqual(500, maps:get(status, Failed)),
        ?assertEqual(<<"internal_error">>, error_code(Failed)),
        {ok, [Row]} = enterprise_query(C, Digest),
        ?assertEqual(null, maps:get(<<"consumed_at">>, Row)),
        ?assertEqual(Before, audit_count(C))
    after
        ok = intbe02_http_support:sql_exec(
            C,
            <<"DROP TRIGGER gate_drop_oa_audit ON enterprise_audit_event">>
        ),
        ok = intbe02_http_support:sql_exec(C, <<"DROP FUNCTION gate_drop_oa_audit()">>)
    end,
    Retry = exchange(S, Body),
    Replay = exchange(S, Body),
    save("oa-audit-retry-responses.json", [Retry, Replay]),
    ?assertEqual(200, maps:get(status, Retry)),
    ?assertEqual(404, maps:get(status, Replay)),
    {ok, [Consumed]} = enterprise_query(C, Digest),
    ?assertNotEqual(null, maps:get(<<"consumed_at">>, Consumed)),
    ?assertEqual(Before + 1, audit_count(C)).

identity_revocation(S) ->
    Facts = [
        {"mapping", <<"enterprise_external_identity">>, <<"status">>, <<"removed">>, <<"active">>,
            <<"application_id=(SELECT application_id FROM enterprise_oa_sso_code WHERE code_digest=$1) AND user_id=995017">>},
        {"member", <<"organization_member">>, <<"status">>, <<"suspended">>, <<"active">>,
            <<"organization_id=(SELECT organization_id FROM enterprise_oa_sso_code WHERE code_digest=$1) AND user_id=995017">>},
        {"account", <<"\"user\"">>, <<"status">>, 0, 1,
            <<"id=(SELECT user_id FROM enterprise_oa_sso_code WHERE code_digest=$1)">>}
    ],
    lists:foreach(
        fun(Fact) -> revoke_during_exchange(S, Fact, 422, <<"identity_not_mapped">>) end, Facts
    ).

revoke_during_exchange(S, {Name, Table, Column, Revoked, Active, Where}, Status, Code) ->
    C = maps:get(conn, S),
    {Digest, Body} = issue(S),
    Sql = fact_update_sql(Table, Column, Where),
    Before = audit_count(C),
    ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
    try
        {ok, 1} = write_fact(C, Sql, Digest, Revoked),
        Parent = self(),
        Ref = make_ref(),
        spawn(fun() -> Parent ! {Ref, exchange(S, Body)} end),
        Blocked = wait(
            C,
            <<"SELECT count(*)>0 AS ready FROM pg_stat_activity WHERE cardinality(pg_blocking_pids(pid))>0 AND (query LIKE '%enterprise_external_identity eei%' OR query LIKE '%organization_member%' OR query LIKE '%enterprise_application%' OR query LIKE '%organization o%')">>,
            [],
            100
        ),
        case Name of
            "application-expired" -> await_deadline(C, Digest, credential);
            _ -> ok
        end,
        ok = intbe02_http_support:sql_exec(C, <<"COMMIT">>),
        R =
            receive
                {Ref, Result} -> Result
            after 10000 -> error(exchange_timeout)
            end,
        save("oa-revoke-" ++ Name ++ "-response.json", [R]),
        ?assertEqual(Status, maps:get(status, R)),
        ?assertEqual(Code, error_code(R)),
        ?assert(Blocked),
        {ok, [Row]} = enterprise_query(C, Digest),
        ?assertEqual(null, maps:get(<<"consumed_at">>, Row)),
        ?assertEqual(Before, audit_count(C))
    after
        intbe02_http_support:sql_exec(C, <<"ROLLBACK">>),
        {ok, 1} = write_fact(C, Sql, Digest, Active)
    end.

authority_revocation(S) ->
    AppWhere = <<"id=(SELECT application_id FROM enterprise_oa_sso_code WHERE code_digest=$1)">>,
    OrgWhere = <<"id=(SELECT organization_id FROM enterprise_oa_sso_code WHERE code_digest=$1)">>,
    CredWhere =
        <<"application_id=(SELECT application_id FROM enterprise_oa_sso_code WHERE code_digest=$1)">>,
    Facts = [
        {
            {"application-scopes", <<"enterprise_application">>, <<"allowed_scopes">>, <<"[]">>,
                <<"[\"sso:exchange\"]">>, AppWhere},
            403,
            <<"insufficient_scope">>
        },
        {
            {"grant-revoked", <<"enterprise_application_grant">>, <<"status">>, <<"revoked">>,
                <<"active">>, CredWhere},
            403,
            <<"insufficient_scope">>
        },
        {
            {"grant-scopes", <<"enterprise_application_grant">>, <<"scopes">>,
                [<<"application:read">>], [<<"sso:exchange">>], CredWhere},
            403,
            <<"insufficient_scope">>
        },

        {
            {"application", <<"enterprise_application">>, <<"status">>, <<"disabled">>,
                <<"active">>, AppWhere},
            403,
            <<"application_disabled">>
        },
        {
            {"organization", <<"organization">>, <<"status">>, <<"archived">>, <<"active">>,
                OrgWhere},
            403,
            <<"organization_disabled">>
        },
        {
            {"credential", <<"enterprise_application_credential">>, <<"status">>, <<"revoked">>,
                <<"active">>, CredWhere},
            401,
            <<"invalid_credential">>
        }
    ],
    lists:foreach(
        fun({Fact, Status, Code}) -> revoke_during_exchange(S, Fact, Status, Code) end, Facts
    ).

write_fact(C, {grant_scopes, Where}, Digest, Scopes) ->
    {ok, [G]} = elib_pg:query(
        C,
        <<"SELECT id, organization_id, application_id, version FROM enterprise_application_grant WHERE ",
            Where/binary>>,
        [Digest]
    ),
    ok = enterprise_application_grant_repo:replace_scopes_tx(
        C,
        maps:get(<<"organization_id">>, G),
        maps:get(<<"application_id">>, G),
        maps:get(<<"id">>, G),
        maps:get(<<"version">>, G),
        Scopes
    ),
    {ok, 1};
write_fact(C, Sql, Digest, Value) ->
    elib_pg:execute(C, Sql, [Digest, Value]).

fact_update_sql(<<"enterprise_application_grant">>, <<"scopes">>, Where) ->
    {grant_scopes, Where};
fact_update_sql(<<"enterprise_application_grant">> = Table, Column, Where) ->
    <<"UPDATE ", Table/binary, " SET ", Column/binary,
        "=$2, revoked_at=CASE WHEN $2='revoked' THEN clock_timestamp() ELSE NULL END,"
        " revoked_by_user_id=CASE WHEN $2='revoked' THEN 995001 ELSE NULL END WHERE ",
        Where/binary>>;
fact_update_sql(<<"enterprise_application_credential">> = Table, Column, Where) ->
    <<"UPDATE ", Table/binary, " SET ", Column/binary,
        "=$2, revoked_at=CASE WHEN $2='revoked' THEN clock_timestamp() ELSE NULL END WHERE ",
        Where/binary>>;
fact_update_sql(Table, Column, Where) ->
    <<"UPDATE ", Table/binary, " SET ", Column/binary, "=$2 WHERE ", Where/binary>>.

credential_deadline(S) ->
    C = maps:get(conn, S),
    {Digest, Body} = issue(S),
    AppId = maps:get(app_sso, S),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE enterprise_application_credential SET expires_at=clock_timestamp()+interval '3 seconds' WHERE application_id=$1">>,
        [AppId]
    ),
    Before = audit_count(C),
    try
        [R] = hold_exchange(S, Digest, Body, 1, credential),
        save("oa-credential-deadline-response.json", [R]),
        ?assertEqual(401, maps:get(status, R)),
        ?assertEqual(<<"credential_expired">>, error_code(R)),
        {ok, [Row]} = enterprise_query(C, Digest),
        ?assertEqual(null, maps:get(<<"consumed_at">>, Row)),
        ?assertEqual(Before, audit_count(C))
    after
        {ok, 1} = elib_pg:execute(
            C,
            <<"UPDATE enterprise_application_credential SET expires_at=NULL WHERE application_id=$1">>,
            [AppId]
        )
    end.

authority_deadline(S) ->
    C = maps:get(conn, S),
    AppId = maps:get(app_sso, S),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE enterprise_application_credential SET expires_at=clock_timestamp()+interval '3 seconds' WHERE application_id=$1">>,
        [AppId]
    ),
    try
        Fact =
            {"application-expired", <<"enterprise_application">>, <<"status">>, <<"disabled">>,
                <<"active">>,
                <<"id=(SELECT application_id FROM enterprise_oa_sso_code WHERE code_digest=$1)">>},
        revoke_during_exchange(S, Fact, 401, <<"credential_expired">>)
    after
        {ok, 1} = elib_pg:execute(
            C,
            <<"UPDATE enterprise_application_credential SET expires_at=NULL WHERE application_id=$1">>,
            [AppId]
        )
    end.

grant_deadline(S) ->
    C = maps:get(conn, S),
    {Digest, Body} = issue(S),
    AppId = maps:get(app_sso, S),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE enterprise_application_grant SET expires_at=clock_timestamp()+interval '3 seconds' WHERE application_id=$1">>,
        [AppId]
    ),
    Before = audit_count(C),
    try
        [R] = hold_exchange(S, Digest, Body, 1, grant),
        save("oa-grant-deadline-response.json", [R]),
        ?assertEqual(403, maps:get(status, R)),
        ?assertEqual(<<"insufficient_scope">>, error_code(R)),
        {ok, [Row]} = enterprise_query(C, Digest),
        ?assertEqual(null, maps:get(<<"consumed_at">>, Row)),
        ?assertEqual(Before, audit_count(C))
    after
        {ok, 1} = elib_pg:execute(
            C,
            <<"UPDATE enterprise_application_grant SET expires_at=clock_timestamp()+interval '1 day' WHERE application_id=$1">>,
            [AppId]
        )
    end.

new_grant_race(S) ->
    C = maps:get(conn, S),
    {Digest, Body} = issue(S),
    AppId = maps:get(app_sso, S),
    Where =
        <<"idempotency_key='intbe02-g-sso' AND application_id=(SELECT application_id FROM enterprise_oa_sso_code WHERE code_digest=$1)">>,
    Before = audit_count(C),
    ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
    try
        {ok, 1} = write_fact(C, {grant_scopes, Where}, Digest, [<<"application:read">>]),
        Parent = self(),
        Ref = make_ref(),
        spawn(fun() -> Parent ! {Ref, exchange(S, Body)} end),
        await_grant_waiter(C),
        {ok, _} = enterprise_application_grant_repo:create_tx(C, 995101, AppId, #{
            scopes => [<<"sso:exchange">>],
            workspace_scope_kind => none,
            expires_at => elib_dt:to_rfc3339(erlang:system_time(second) + 86400, second),
            idempotency_key => <<"oa-sso-grant-phantom">>
        }),
        ok = intbe02_http_support:sql_exec(C, <<"COMMIT">>),
        R =
            receive
                {Ref, Result} -> Result
            after 10000 -> error(exchange_timeout)
            end,
        save("oa-new-grant-response.json", [R]),
        ?assertEqual(403, maps:get(status, R)),
        ?assertEqual(<<"insufficient_scope">>, error_code(R)),
        {ok, [Row]} = enterprise_query(C, Digest),
        ?assertEqual(null, maps:get(<<"consumed_at">>, Row)),
        Retry = exchange(S, Body),
        save("oa-new-grant-retry-response.json", [Retry]),
        ?assertEqual(200, maps:get(status, Retry)),
        ?assertEqual(Before + 1, audit_count(C))
    after
        intbe02_http_support:sql_exec(C, <<"ROLLBACK">>),
        {ok, 1} = write_fact(C, {grant_scopes, Where}, Digest, [<<"sso:exchange">>]),
        {ok, _} = elib_pg:execute(
            C,
            <<"UPDATE enterprise_application_grant SET status='revoked', revoked_at=clock_timestamp(), revoked_by_user_id=995001 WHERE application_id=$1 AND idempotency_key='oa-sso-grant-phantom' AND status='active'">>,
            [AppId]
        )
    end.

await_grant_waiter(C) ->
    ?assert(
        wait(
            C,
            <<"SELECT count(*)>0 AS ready FROM pg_stat_activity WHERE cardinality(pg_blocking_pids(pid))>0 AND query LIKE 'SELECT id FROM enterprise_application_grant%'">>,
            [],
            100
        )
    ).

hold_exchange(S, Digest, Body, Count, Expire) ->
    C = maps:get(conn, S),
    ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
    try
        {ok, [_]} = elib_pg:query(
            C, <<"SELECT id FROM enterprise_oa_sso_code WHERE code_digest=$1 FOR UPDATE">>, [Digest]
        ),
        Parent = self(),
        Ref = make_ref(),
        lists:foreach(
            fun(_) -> spawn(fun() -> Parent ! {Ref, exchange(S, Body)} end) end, lists:seq(1, Count)
        ),
        await_waiters(C, Count),
        await_deadline(C, Digest, Expire),
        ok = intbe02_http_support:sql_exec(C, <<"COMMIT">>),
        [
            receive
                {Ref, R} -> R
            after 10000 -> error(exchange_timeout)
            end
         || _ <- lists:seq(1, Count)
        ]
    after
        intbe02_http_support:sql_exec(C, <<"ROLLBACK">>)
    end.

await_deadline(_C, _Digest, false) ->
    ok;
await_deadline(C, Digest, Expire) ->
    Sql =
        case Expire of
            true ->
                <<"SELECT expires_at<=clock_timestamp() AS ready FROM enterprise_oa_sso_code WHERE code_digest=$1">>;
            grant ->
                <<"SELECT expires_at<=clock_timestamp() AS ready FROM enterprise_application_grant WHERE application_id=(SELECT application_id FROM enterprise_oa_sso_code WHERE code_digest=$1)">>;
            credential ->
                <<"SELECT expires_at<=clock_timestamp() AS ready FROM enterprise_application_credential WHERE application_id=(SELECT application_id FROM enterprise_oa_sso_code WHERE code_digest=$1)">>
        end,
    ?assert(wait(C, Sql, [Digest], 100)).

await_waiters(C, Count) ->
    %% Queued contenders can block on each other, not directly on the holder.
    ?assert(
        wait(
            C,
            <<"SELECT count(*) >= $1 AS ready FROM pg_stat_activity WHERE cardinality(pg_blocking_pids(pid))>0 AND query LIKE '%enterprise_oa_sso_code%'">>,
            [Count],
            100
        )
    ).

exchange(S, Body) ->
    intbe02_http_support:http(
        maps:get(port, S),
        <<"POST">>,
        <<"/api/internal/v1/oa/sso/exchange">>,
        Body,
        intbe02_http_support:auth(maps:get(cred_sso, S))
    ).
wait(_C, _Sql, _Params, 0) ->
    false;
wait(C, Sql, Params, Left) ->
    %% Refresh transaction-cached activity columns; lock dependencies remain native.
    {ok, _} = elib_pg:query(C, <<"SELECT pg_stat_clear_snapshot()">>, []),
    case elib_pg:query(C, Sql, Params) of
        {ok, [#{<<"ready">> := true}]} ->
            true;
        _ ->
            timer:sleep(50),
            wait(C, Sql, Params, Left - 1)
    end.
enterprise_query(C, Digest) ->
    elib_pg:query(C, <<"SELECT consumed_at FROM enterprise_oa_sso_code WHERE code_digest=$1">>, [
        Digest
    ]).
audit_count(C) ->
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        C,
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE action='oa.sso.exchanged'">>,
        []
    ),
    N.
error_code(R) ->
    maps:get(<<"code">>, maps:get(<<"error">>, jsone:decode(maps:get(body, R)))).
save(Name, Responses) ->
    case os:getenv("IMBOY_GATE_RUN_DIR") of
        false ->
            ok;
        Directory ->
            file:write_file(
                filename:join(Directory, Name),
                jsone:encode([
                    #{status => maps:get(status, R), body => jsone:decode(maps:get(body, R))}
                 || R <- Responses
                ])
            )
    end.
