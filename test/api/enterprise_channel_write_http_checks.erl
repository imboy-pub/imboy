-module(enterprise_channel_write_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").
-define(BASE, <<"/api/internal/v1/channels">>).

run(S) ->
    B40 = #{
        <<"workspace_id">> => 995202,
        <<"creator_user_id">> => 995012,
        <<"name">> => <<"Internal channel">>
    },
    R40 = req(S, <<"POST">>, ?BASE, B40, <<"ch-40">>),
    #{<<"channel_id">> := W, <<"version">> := 1} = ok_json(R40),
    cover(40),
    Path = path(W),
    B41 = #{<<"expected_version">> => 1, <<"name">> => <<"Updated channel">>},
    R41 = req(S, <<"PATCH">>, Path, B41, <<"ch-41">>),
    #{<<"version">> := 2} = ok_json(R41),
    ?assertNotEqual(
        maps:get(<<"updated_at">>, ok_json(R40)), maps:get(<<"updated_at">>, ok_json(R41))
    ),
    cover(41),
    negatives(S, Path, B40, B41),
    creator_negatives(S, B40),
    resource_negatives(S, Path, B40),
    quota_limit(S, B40),
    concurrent_create(S, B40),
    audit_failure(S, Path, W),
    concurrent(S, Path),
    archive_and_replay(S, W, Path, B40, R40, B41, R41).

archive_and_replay(S, W, Path, B40, R40, B41, R41) ->
    {ok, 1} = elib_pg:query(
        maps:get(conn, S),
        <<"INSERT INTO channel_message(id,channel_id,author_id,content) VALUES(9959502,$1,995012,'synthetic-retained-history')">>,
        [W]
    ),
    B42 = #{<<"expected_version">> => 3},
    R42 = req(S, <<"DELETE">>, Path, B42, <<"ch-42">>),
    #{<<"version">> := 4, <<"status">> := 0} = ok_json(R42),
    cover(42),
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
            {<<"POST">>, ?BASE, B40, <<"ch-40">>, R40},
            {<<"PATCH">>, Path, B41, <<"ch-41">>, R41},
            {<<"DELETE">>, Path, B42, <<"ch-42">>, R42}
        ]
    ),
    err(
        req(S, <<"DELETE">>, Path, B42#{<<"expected_version">> => 2}, <<"ch-42">>),
        <<"idempotency_conflict">>
    ),
    assert_audits(S, W),
    revoked_replay(S, Path, B42).

negatives(S, Path, B40, B41) ->
    lists:foreach(
        fun({M, P, B, K}) ->
            Missing = request(S, cred_ro, M, P, B, K),
            err(Missing, <<"insufficient_scope">>),
            err(req(S, M, P, B, undefined), <<"invalid_request">>),
            err(req(S, M, P, B, binary:copy(<<"x">>, 129)), <<"invalid_request">>),
            err(req(S, M, P, B#{<<"unknown">> => 1}, K), <<"invalid_request">>)
        end,
        [
            {<<"POST">>, ?BASE, B40, <<"negative-post">>},
            {<<"PATCH">>, Path, B41, <<"negative-patch">>},
            {<<"DELETE">>, Path, #{<<"expected_version">> => 2}, <<"negative-delete">>}
        ]
    ),
    err(
        request(S, cred_w, <<"POST">>, ?BASE, B40, <<"narrow-create">>),
        <<"organization_boundary_violation">>
    ),
    err(
        request(S, cred_w, <<"PATCH">>, Path, B41, <<"narrow-update">>),
        <<"organization_boundary_violation">>
    ),
    err(req(S, <<"PATCH">>, Path, B41, <<"stale-version">>), <<"version_conflict">>),
    err(
        req(S, <<"POST">>, ?BASE, B40#{<<"name">> => <<"different">>}, <<"ch-40">>),
        <<"idempotency_conflict">>
    ),
    err(
        req(S, <<"PATCH">>, Path, B41#{<<"name">> => <<"different">>}, <<"ch-41">>),
        <<"idempotency_conflict">>
    ),
    err(req(S, <<"PATCH">>, path(995999999), B41, <<"foreign">>), <<"resource_not_found">>),
    err(
        req(S, <<"POST">>, ?BASE, B40#{<<"creator_user_id">> => 995021}, <<"foreign-owner">>),
        <<"organization_boundary_violation">>
    ).

audit_failure(S, Path, W) ->
    C = maps:get(conn, S),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"ALTER TABLE enterprise_audit_event ADD CONSTRAINT synthetic_channel_audit_reject CHECK(resource_type <> 'channel') NOT VALID">>
    ),
    try
        err(
            req(
                S,
                <<"PATCH">>,
                Path,
                #{<<"expected_version">> => 2, <<"name">> => <<"Must roll back">>},
                <<"ch-audit-reject">>
            ),
            <<"internal_error">>
        ),
        #{<<"version">> := 2, <<"name">> := <<"Updated channel">>} = intbe02_http_support:one(
            C,
            <<"SELECT version,name FROM channel WHERE id=$1">>,
            [W]
        ),
        #{<<"n">> := 0} = intbe02_http_support:one(
            C,
            <<"SELECT count(*) AS n FROM enterprise_internal_idempotency WHERE idempotency_key='ch-audit-reject'">>
        )
    after
        ok = intbe02_http_support:sql_exec(
            C,
            <<"ALTER TABLE enterprise_audit_event DROP CONSTRAINT synthetic_channel_audit_reject">>
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
     || Name <- [<<"channel-race-a">>, <<"channel-race-b">>]
    ],
    [Pid ! go || Pid <- Pids],
    Rs = [
        receive
            {Pid, R} -> R
        after 10000 -> error(channel_patch_timeout)
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
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE resource_type='channel' AND resource_id=$1 AND actor_user_id IS NULL AND actor_role='application' AND detail ? 'application_id' AND detail ? 'correlation_id'">>,
        [W]
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel_subscription WHERE channel_id=$1 AND user_id=995012 AND status=1">>,
        [W]
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel_admin WHERE channel_id=$1 AND user_id=995012 AND role=3">>,
        [W]
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel_message WHERE channel_id=$1 AND content='synthetic-retained-history'">>,
        [W]
    ).

revoked_replay(S, Path, Body) ->
    C = maps:get(conn, S),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"DELETE FROM enterprise_application_grant_scope WHERE scope='channels:write'">>
    ),
    err(req(S, <<"DELETE">>, Path, Body, <<"ch-42">>), <<"insufficient_scope">>).

path(W) -> <<?BASE/binary, "/", (integer_to_binary(W))/binary>>.
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

creator_negatives(S, Body) ->
    C = maps:get(conn, S),
    lists:foreach(
        fun({Set, Restore}) ->
            ok = intbe02_http_support:sql_exec(C, Set),
            try
                err(
                    req(S, <<"POST">>, ?BASE, Body, <<"creator-ineligible">>),
                    <<"organization_boundary_violation">>
                )
            after
                ok = intbe02_http_support:sql_exec(C, Restore)
            end
        end,
        [
            {<<"UPDATE workspace_member SET role='guest' WHERE workspace_id=995202 AND user_id=995012">>,
                <<"UPDATE workspace_member SET role='member' WHERE workspace_id=995202 AND user_id=995012">>},
            {<<"UPDATE organization_member SET status='removed' WHERE organization_id=995101 AND user_id=995012">>,
                <<"UPDATE organization_member SET status='active' WHERE organization_id=995101 AND user_id=995012">>}
        ]
    ),
    err(
        req(S, <<"POST">>, ?BASE, Body#{<<"visibility">> => 0}, <<"public-forbidden">>),
        <<"invalid_request">>
    ),
    err(
        req(S, <<"POST">>, ?BASE, Body#{<<"creator_user_id">> => 995001}, <<"nonmember">>),
        <<"organization_boundary_violation">>
    ),
    err(
        req(
            S,
            <<"PATCH">>,
            path(995501),
            #{<<"expected_version">> => 1, <<"scope">> => <<"personal">>},
            <<"immutable-scope">>
        ),
        <<"invalid_request">>
    ).

resource_negatives(S, Path, Body) ->
    C = maps:get(conn, S),
    ok = intbe02_http_support:sql_exec(
        C,
        <<
            "INSERT INTO channel(id,name,creator_uid,scope,workspace_id) VALUES "
            "(9959503,'synthetic-personal',995012,'personal',NULL),"
            "(9959504,'synthetic-foreign',995021,'workspace',995211)"
        >>
    ),
    lists:foreach(
        fun(Id) ->
            lists:foreach(
                fun(M) ->
                    B =
                        case M of
                            <<"PATCH">> ->
                                #{<<"expected_version">> => 1, <<"name">> => <<"denied">>};
                            _ ->
                                #{<<"expected_version">> => 1}
                        end,
                    err(req(S, M, path(Id), B, <<"foreign-resource">>), <<"resource_not_found">>)
                end,
                [<<"PATCH">>, <<"DELETE">>]
            )
        end,
        [9959503, 9959504]
    ),
    err(
        request(
            S, cred_w, <<"DELETE">>, Path, #{<<"expected_version">> => 2}, <<"narrow-archive">>
        ),
        <<"organization_boundary_violation">>
    ),
    ok = intbe02_http_support:sql_exec(
        C, <<"UPDATE workspace SET status='archived' WHERE id=995202">>
    ),
    try
        err(req(S, <<"POST">>, ?BASE, Body, <<"parent-archived">>), <<"resource_conflict">>)
    after
        ok = intbe02_http_support:sql_exec(
            C, <<"UPDATE workspace SET status='active' WHERE id=995202">>
        )
    end,
    #{<<"channel_id">> := _} = ok_json(
        req(
            S,
            <<"POST">>,
            ?BASE,
            Body#{<<"workspace_id">> => 995201, <<"creator_user_id">> => 995001},
            <<"owner-create">>
        )
    ).

quota_limit(S, Body) ->
    C = maps:get(conn, S),
    {ok, Count} = channel_repo:count_managed_tx(C, 995012),
    {ok, _} = elib_pg:query(
        C,
        <<
            "INSERT INTO channel(id,name,creator_uid,scope,workspace_id) "
            "SELECT 9959600+n,'synthetic-quota-'||n,995012,'workspace',995202 FROM generate_series(1,$1::integer) n"
        >>,
        [20 - Count]
    ),
    ok = intbe02_http_support:sql_exec(
        C,
        <<
            "INSERT INTO channel_admin(id,channel_id,user_id,role) "
            "SELECT 9959700+id-9959600,id,995012,3 FROM channel WHERE id BETWEEN 9959601 AND 9959620"
        >>
    ),
    try
        err(req(S, <<"POST">>, ?BASE, Body, <<"application-quota-full">>), <<"resource_conflict">>),
        {ok, 20} = channel_repo:count_managed_tx(C, 995012),
        #{<<"n">> := 0} = intbe02_http_support:one(
            C,
            <<"SELECT count(*) AS n FROM enterprise_internal_idempotency WHERE idempotency_key='application-quota-full'">>
        )
    after
        ok = intbe02_http_support:sql_exec(
            C, <<"DELETE FROM channel_admin WHERE channel_id BETWEEN 9959601 AND 9959620">>
        ),
        ok = intbe02_http_support:sql_exec(
            C, <<"DELETE FROM channel WHERE id BETWEEN 9959601 AND 9959620">>
        )
    end.

concurrent_create(S, Body) ->
    Parent = self(),
    B = Body#{<<"name">> => <<"Concurrent internal channel">>},
    Workers = [
        spawn(fun() ->
            receive
                go ->
                    Parent ! {self(), req(S, <<"POST">>, ?BASE, B, <<"channel-concurrent-create">>)}
            end
        end)
     || _ <- [1, 2]
    ],
    [P ! go || P <- Workers],
    [R1, R2] = [
        receive
            {P, R} -> R
        after 10000 -> error(channel_create_timeout)
        end
     || P <- Workers
    ],
    #{<<"channel_id">> := Id} = ok_json(R1),
    #{<<"channel_id">> := Id} = ok_json(R2),
    ?assertEqual(maps:get(body, R1), maps:get(body, R2)),
    ?assertEqual(
        1,
        length([
            R
         || R <- [R1, R2],
            intbe02_http_support:json_header_val(R, <<"idempotent-replayed">>) =:= <<"true">>
        ])
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        maps:get(conn, S),
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE resource_type='channel' AND resource_id=$1">>,
        [Id]
    ).
