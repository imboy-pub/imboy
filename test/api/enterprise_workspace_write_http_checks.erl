-module(enterprise_workspace_write_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").
-define(BASE, <<"/api/internal/v1/workspaces">>).

run(S) ->
    B37 = #{<<"owner_user_id">> => 995001, <<"name">> => <<"Internal workspace">>},
    R37 = audited_req(S, <<"POST">>, ?BASE, B37, <<"ws-37">>),
    #{<<"workspace_id">> := W, <<"version">> := 1} = ok_json(R37),
    cover(37),
    Path = path(W),
    B38 = #{<<"expected_version">> => 1, <<"name">> => <<"Updated workspace">>},
    R38 = audited_req(S, <<"PATCH">>, Path, B38, <<"ws-38">>),
    #{<<"version">> := 2} = ok_json(R38),
    cover(38),
    negatives(S, Path, B37, B38),
    audit_failure(S, Path, W),
    concurrent(S, Path),
    B39 = #{<<"expected_version">> => 3},
    R39 = audited_req(S, <<"DELETE">>, Path, B39, <<"ws-39">>),
    #{<<"version">> := 4, <<"status">> := <<"archived">>} = ok_json(R39),
    cover(39),
    lists:foreach(
        fun({M, P, B, K, Original}) ->
            Replay = req(S, M, P, B, K),
            ?assertEqual(200, maps:get(status, Replay)),
            ?assertEqual(maps:get(body, Original), maps:get(body, Replay)),
            ?assertEqual(
                <<"true">>, intbe02_http_support:json_header_val(Replay, <<"idempotent-replayed">>)
            )
        end,
        [
            {<<"POST">>, ?BASE, B37, <<"ws-37">>, R37},
            {<<"PATCH">>, Path, B38, <<"ws-38">>, R38},
            {<<"DELETE">>, Path, B39, <<"ws-39">>, R39}
        ]
    ),
    assert_audits(S, W),
    revoked_replay(S, Path, B39).

negatives(S, Path, B37, B38) ->
    lists:foreach(
        fun({M, P, B, K}) ->
            Missing = request(S, cred_ro, M, P, B, K),
            err(Missing, <<"insufficient_scope">>),
            err(req(S, M, P, B, undefined), <<"invalid_request">>),
            err(req(S, M, P, B, binary:copy(<<"x">>, 129)), <<"invalid_request">>),
            err(req(S, M, P, B#{<<"unknown">> => 1}, K), <<"invalid_request">>)
        end,
        [
            {<<"POST">>, ?BASE, B37, <<"negative-post">>},
            {<<"PATCH">>, Path, B38, <<"negative-patch">>},
            {<<"DELETE">>, Path, #{<<"expected_version">> => 2}, <<"negative-delete">>}
        ]
    ),
    err(
        request(S, cred_w, <<"POST">>, ?BASE, B37, <<"narrow-create">>),
        <<"organization_boundary_violation">>
    ),
    err(
        request(S, cred_w, <<"PATCH">>, Path, B38, <<"narrow-update">>),
        <<"organization_boundary_violation">>
    ),
    err(req(S, <<"PATCH">>, Path, B38, <<"stale-version">>), <<"version_conflict">>),
    err(
        req(S, <<"POST">>, ?BASE, B37#{<<"name">> => <<"different">>}, <<"ws-37">>),
        <<"idempotency_conflict">>
    ),
    err(
        req(S, <<"PATCH">>, Path, B38#{<<"name">> => <<"different">>}, <<"ws-38">>),
        <<"idempotency_conflict">>
    ),
    err(req(S, <<"PATCH">>, path(995211), B38, <<"foreign">>), <<"resource_not_found">>),
    err(
        req(S, <<"POST">>, ?BASE, B37#{<<"owner_user_id">> => 995021}, <<"foreign-owner">>),
        <<"organization_boundary_violation">>
    ).

audit_failure(S, Path, W) ->
    C = maps:get(conn, S),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"ALTER TABLE enterprise_audit_event ADD CONSTRAINT synthetic_workspace_audit_reject CHECK(resource_type <> 'workspace') NOT VALID">>
    ),
    try
        err(
            req(
                S,
                <<"PATCH">>,
                Path,
                #{<<"expected_version">> => 2, <<"name">> => <<"Must roll back">>},
                <<"ws-audit-reject">>
            ),
            <<"internal_error">>
        ),
        #{<<"version">> := 2, <<"name">> := <<"Updated workspace">>} = intbe02_http_support:one(
            C,
            <<"SELECT version,name FROM workspace WHERE id=$1">>,
            [W]
        ),
        #{<<"n">> := 0} = intbe02_http_support:one(
            C,
            <<"SELECT count(*) AS n FROM enterprise_internal_idempotency WHERE idempotency_key='ws-audit-reject'">>
        )
    after
        ok = intbe02_http_support:sql_exec(
            C,
            <<"ALTER TABLE enterprise_audit_event DROP CONSTRAINT synthetic_workspace_audit_reject">>
        )
    end.

concurrent(S, Path) ->
    Parent = self(),
    Pids = [
        spawn(fun() ->
            receive
                go ->
                    Parent !
                        {
                            self(),
                            req(
                                S,
                                <<"PATCH">>,
                                Path,
                                #{<<"expected_version">> => 2, <<"name">> => Name},
                                Name
                            )
                        }
            end
        end)
     || Name <- [<<"race-a">>, <<"race-b">>]
    ],
    [Pid ! go || Pid <- Pids],
    Rs = [
        receive
            {Pid, R} -> R
        after 10000 -> error(workspace_patch_timeout)
        end
     || Pid <- Pids
    ],
    ?assertEqual([200, 409], lists:sort([maps:get(status, R) || R <- Rs])),
    [Conflict] = [R || R <- Rs, maps:get(status, R) =:= 409],
    err(Conflict, <<"version_conflict">>).

assert_audits(S, W) ->
    C = maps:get(conn, S),
    #{<<"n">> := 4} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE resource_type='workspace' AND resource_id=$1 AND actor_user_id IS NULL AND actor_role='application' AND detail ? 'application_id' AND detail ? 'correlation_id'">>,
        [W]
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM \"group\" WHERE workspace_id=$1 AND scope='workspace'">>,
        [W]
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel WHERE workspace_id=$1 AND scope='workspace'">>,
        [W]
    ).

revoked_replay(S, Path, Body) ->
    C = maps:get(conn, S),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"DELETE FROM enterprise_application_grant_scope WHERE scope='workspaces:write'">>
    ),
    err(req(S, <<"DELETE">>, Path, Body, <<"ws-39">>), <<"insufficient_scope">>).

path(W) -> <<?BASE/binary, "/", (integer_to_binary(W))/binary>>.
audited_req(S, M, P, B, K) ->
    enterprise_internal_audit_http_checks:request(S, M, P, B, K).
req(S, M, P, B, K) -> request(S, cred_a, M, P, B, K).
request(S, Credential, M, P, B, K) ->
    A = intbe02_http_support:auth(maps:get(Credential, S)),
    H =
        case K of
            undefined -> A;
            _ -> maps:merge(A, intbe02_http_support:idem(K))
        end,
    intbe02_http_support:http(maps:get(port, S), M, P, B, H).
ok_json(R) ->
    ?assertEqual(200, maps:get(status, R), maps:get(body, R)),
    jsone:decode(maps:get(body, R)).
err(R, Code) ->
    ?assertEqual(
        enterprise_internal_error:http_status(Code), maps:get(status, R), maps:get(body, R)
    ),
    ?assertEqual(
        Code, maps:get(<<"code">>, maps:get(<<"error">>, jsone:decode(maps:get(body, R))))
    ).
cover(N) -> intbe02_http_support:cover(<<"INT-", (integer_to_binary(N))/binary>>, ok).
