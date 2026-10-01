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
    concurrent_once(S).

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
        case Expire of
            true ->
                ?assert(
                    wait(
                        C,
                        <<"SELECT expires_at<=clock_timestamp() AS ready FROM enterprise_oa_sso_code WHERE code_digest=$1">>,
                        [Digest],
                        100
                    )
                );
            false ->
                ok
        end,
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
