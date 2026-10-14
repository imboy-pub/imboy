-module(enterprise_directory_source_repo_tests).
-include_lib("eunit/include/eunit.hrl").

bootstrap_creates_only_org_owner_and_minimal_grant_test() ->
    bootstrap_mocks(fun() ->
        bootstrap_rows(<<"[\"identities:write\",\"identities:read\"]">>, true),
        meck:expect(organization_repo, create_tx, fun(conn, 99, <<"Org">>, <<"active">>) ->
            {ok, #{<<"id">> => 10, <<"status">> => <<"active">>}}
        end),
        meck:expect(
            organization_member_repo,
            upsert_active_tx,
            fun(conn, 10, 99, <<"owner">>, null) -> {ok, changed, #{<<"role">> => <<"owner">>}} end
        ),
        meck:expect(
            enterprise_application_grant_repo,
            create_tx,
            fun(
                conn,
                10,
                1,
                #{
                    scopes := Scopes,
                    workspace_scope_kind := none,
                    workspace_ids := [],
                    provisioning_source := <<"directory_bootstrap">>,
                    expires_at := <<"2099-01-01T00:00:00Z">>
                }
            ) ->
                ?assertEqual([<<"identities:write">>, <<"identities:read">>], Scopes),
                {ok, #{<<"id">> => 30}}
            end
        ),
        ?assertMatch({ok, #{<<"id">> := 10}}, bootstrap()),
        ?assertEqual(1, meck:num_calls(elib_pg, execute, '_')),
        meck:expect(
            organization_member_repo,
            upsert_active_tx,
            fun(conn, 10, 99, <<"owner">>, null) -> {ok, unchanged, #{}} end
        ),
        ?assertMatch({ok, #{<<"id">> := 10}}, bootstrap())
    end).

bootstrap_requires_precreation_capability_test() ->
    bootstrap_mocks(fun() ->
        bootstrap_rows(<<"[\"identities:write\"]">>, false),
        ?assertEqual({error, directory_bootstrap_not_authorized}, bootstrap()),
        ?assertEqual(0, meck:num_calls(organization_repo, create_tx, '_')),
        bootstrap_rows(<<"[\"identities:write\",\"messages:send\"]">>, true),
        ?assertEqual({error, directory_only_application_required}, bootstrap()),
        ?assertEqual(0, meck:num_calls(organization_repo, create_tx, '_'))
    end).

bootstrap_owner_failure_aborts_before_grant_test() ->
    bootstrap_mocks(fun() ->
        bootstrap_rows(<<"[\"identities:write\"]">>, true),
        meck:expect(organization_repo, create_tx, fun(conn, 99, _, _) -> {ok, #{<<"id">> => 10}} end),
        meck:expect(
            organization_member_repo,
            upsert_active_tx,
            fun(conn, 10, 99, <<"owner">>, null) -> {error, owner_failed} end
        ),
        ?assertThrow({abort_tx, owner_failed}, bootstrap()),
        ?assertEqual(0, meck:num_calls(enterprise_application_grant_repo, create_tx, '_'))
    end).

bootstrap() ->
    enterprise_directory_source_repo:bootstrap_tx(
        conn,
        #{application_id => 1, credential_id => 2},
        #{<<"confirmed_owner_user_id">> => 99},
        #{<<"external_id">> => <<"org">>, <<"name">> => <<"Org">>}
    ).

bootstrap_rows(Scopes, Authorized) ->
    meck:expect(elib_pg, query, fun(conn, Sql, Params) ->
        case Params of
            [1, 2] ->
                {ok, [#{<<"allowed_scopes">> => Scopes}]};
            [1, <<"org">>] ->
                ?assertNotEqual(
                    nomatch, binary:match(Sql, <<"authorization_expires_at>clock_timestamp()">>)
                ),
                {ok,
                    case Authorized of
                        true ->
                            [
                                #{
                                    <<"authorization_expires_at">> => <<"2099-01-01T00:00:00Z">>,
                                    <<"confirmed_owner_user_id">> => 99
                                }
                            ];
                        false ->
                            []
                    end};
            [99] ->
                {ok, [#{<<"id">> => 99}]}
        end
    end),
    meck:expect(enterprise_internal_idempotency, request_digest, fun(_, _, _) ->
        {ok, <<"digest">>}
    end),
    meck:expect(elib_pg, execute, fun(conn, _, [1, <<"org">>, 10, 30]) -> {ok, 1} end).

bootstrap_mocks(Fun) ->
    Modules = [
        elib_pg,
        organization_repo,
        organization_member_repo,
        enterprise_application_grant_repo,
        enterprise_internal_idempotency
    ],
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict, no_link]) end, Modules),
    try
        Fun()
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

revoked_credential_and_source_lock_test() ->
    with_pg(fun() ->
        meck:expect(elib_pg, query, fun(conn, Sql, [1, 2]) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"FOR UPDATE OF a">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"c.status='active'">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"c.expires_at>clock_timestamp()">>)),
            {ok, []}
        end),
        ?assertEqual(
            {error, insufficient_scope},
            enterprise_directory_source_repo:authorize_tx(
                conn,
                #{
                    application_id => 1,
                    credential_id => 2,
                    organization_id => 10,
                    granted_scopes => [<<"identities:write">>]
                }
            )
        ),
        ?assertEqual(
            {error, insufficient_scope},
            enterprise_directory_source_repo:authorize_tx(
                conn,
                #{
                    application_id => 1,
                    credential_id => 2,
                    organization_id => 10,
                    granted_scopes => [<<"messages:send">>]
                }
            )
        ),
        ?assertEqual(2, meck:num_calls(elib_pg, query, '_'))
    end).

stale_middleware_grant_is_recomputed_test() ->
    with_pg(fun() ->
        meck:new(enterprise_application_grant_logic, [non_strict, no_link]),
        try
            authority_rows(),
            meck:expect(
                enterprise_application_grant_logic,
                context_tx,
                fun(conn, 10, 1, [<<"identities:write">>]) ->
                    {ok, #{effective_scopes => []}}
                end
            ),
            meck:expect(
                enterprise_application_grant_logic,
                require_org_tx,
                fun(conn, #{organization_id := 10, granted_scopes := []}, <<"identities:write">>) ->
                    {error, insufficient_scope}
                end
            ),
            ?assertEqual(
                {error, insufficient_scope},
                enterprise_directory_source_repo:authorize_tx(
                    conn,
                    #{
                        application_id => 1,
                        credential_id => 2,
                        organization_id => 10,
                        granted_scopes => [<<"identities:write">>]
                    }
                )
            )
        after
            meck:unload(enterprise_application_grant_logic)
        end
    end).

target_org_and_locked_expiry_test() ->
    with_pg(fun() ->
        meck:new(enterprise_application_grant_logic, [non_strict, no_link]),
        try
            authority_rows(),
            meck:expect(
                enterprise_application_grant_logic,
                context_tx,
                fun(conn, 10, 1, _) -> {ok, #{effective_scopes => [<<"identities:write">>]}} end
            ),
            meck:expect(
                enterprise_application_grant_logic,
                require_org_tx,
                fun(conn, #{organization_id := 10}, <<"identities:write">>) -> ok end
            ),
            ?assertEqual(
                {error, organization_boundary_violation},
                enterprise_directory_source_repo:authorize_tx(
                    conn,
                    #{
                        application_id => 1,
                        credential_id => 2,
                        organization_id => 10,
                        granted_scopes => []
                    }
                )
            )
        after
            meck:unload(enterprise_application_grant_logic)
        end
    end).

authority_rows() ->
    meck:expect(elib_pg, query, fun
        (conn, _, [1, 2]) ->
            {ok, [#{<<"id">> => 1, <<"allowed_scopes">> => <<"[\"identities:write\"]">>}]};
        (conn, Sql, [1, 10]) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"ORDER BY id FOR SHARE">>)),
            {ok, [#{<<"id">> => 3}]};
        (conn, Sql, [1, 10, [3]]) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"g.id=ANY($3::bigint[])">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"g.expires_at>clock_timestamp()">>)),
            {ok, [#{<<"allowed">> => false}]}
    end).

source_cannot_claim_local_member_test() ->
    with_pg(fun() ->
        meck:new(organization_member_repo, [non_strict, no_link]),
        try
            meck:expect(organization_member_repo, find_for_update_tx, fun(conn, 10, 42, _) ->
                {ok, #{<<"role">> => <<"admin">>, <<"status">> => <<"active">>}}
            end),
            ?assertEqual(
                {ok, false},
                enterprise_directory_source_repo:project_member_tx(
                    conn, 10, 42, <<"active">>, false, #{}
                )
            ),
            ?assertEqual(
                {error, local_membership_conflict},
                enterprise_directory_source_repo:project_member_tx(
                    conn, 10, 42, <<"removed">>, false, #{}
                )
            ),
            ?assertEqual(0, meck:num_calls(elib_pg, execute, '_'))
        after
            meck:unload(organization_member_repo)
        end
    end).

restore_changes_only_status_test() ->
    with_pg(fun() ->
        meck:new(organization_member_repo, [non_strict, no_link]),
        try
            meck:expect(organization_member_repo, find_for_update_tx, fun(conn, 10, 42, _) ->
                {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"removed">>}}
            end),
            meck:expect(elib_pg, execute, fun(conn, Sql, [10, 42, <<"stamp">>]) ->
                ?assertEqual(nomatch, binary:match(Sql, <<"SET role">>)),
                ?assertEqual(nomatch, binary:match(Sql, <<"suspended">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"updated_at=$3::timestamptz">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"status='removed'">>)),
                {ok, 1}
            end),
            ?assertEqual(
                {ok, true},
                enterprise_directory_source_repo:project_member_tx(
                    conn,
                    10,
                    42,
                    <<"active">>,
                    true,
                    #{
                        <<"status">> => <<"removed">>,
                        <<"offboarding_member_updated_at">> => <<"stamp">>
                    }
                )
            )
        after
            meck:unload(organization_member_repo)
        end
    end).

local_suspension_and_removal_are_not_restored_test() ->
    with_pg(fun() ->
        meck:new(organization_member_repo, [non_strict, no_link]),
        try
            lists:foreach(
                fun({LocalStatus, Managed, PreviousStatus}) ->
                    meck:expect(organization_member_repo, find_for_update_tx, fun(conn, 10, 42, _) ->
                        {ok, #{<<"role">> => <<"member">>, <<"status">> => LocalStatus}}
                    end),
                    ?assertEqual(
                        {error, local_membership_conflict},
                        enterprise_directory_source_repo:project_member_tx(
                            conn, 10, 42, <<"active">>, Managed, #{<<"status">> => PreviousStatus}
                        )
                    )
                end,
                [
                    {<<"suspended">>, true, <<"removed">>},
                    {<<"suspended">>, true, <<"active">>},
                    {<<"removed">>, true, <<"active">>},
                    {<<"removed">>, true, undefined},
                    {<<"removed">>, false, <<"removed">>},
                    {<<"removed">>, true, <<"removed">>}
                ]
            ),
            ?assertEqual(0, meck:num_calls(elib_pg, execute, '_')),
            ?assertEqual(0, meck:num_calls(organization_member_repo, upsert_active_tx, '_')),
            meck:expect(organization_member_repo, find_for_update_tx, fun(conn, 10, 42, _) ->
                {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"suspended">>}}
            end),
            ?assertEqual(
                {error, local_membership_conflict},
                enterprise_directory_source_repo:project_member_tx(
                    conn, 10, 42, <<"removed">>, true, #{<<"status">> => <<"active">>}
                )
            )
        after
            meck:unload(organization_member_repo)
        end
    end).

offboarding_evidence_must_match_member_revision_test() ->
    with_pg(fun() ->
        meck:new(organization_member_repo, [non_strict, no_link]),
        try
            meck:expect(organization_member_repo, find_for_update_tx, fun(conn, 10, 42, _) ->
                {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"removed">>}}
            end),
            meck:expect(elib_pg, execute, fun(conn, Sql, [10, 42, <<"old-stamp">>]) ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"updated_at=$3::timestamptz">>)),
                {ok, 0}
            end),
            ?assertEqual(
                {error, local_membership_conflict},
                enterprise_directory_source_repo:project_member_tx(
                    conn,
                    10,
                    42,
                    <<"active">>,
                    true,
                    #{
                        <<"status">> => <<"removed">>,
                        <<"offboarding_member_updated_at">> => <<"old-stamp">>
                    }
                )
            )
        after
            meck:unload(organization_member_repo)
        end
    end).

offboarding_stamp_is_bound_to_projected_member_test() ->
    with_pg(fun() ->
        meck:expect(elib_pg, execute, fun(conn, Sql, [1, 10, <<"person">>]) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"m.user_id=s.projected_user_id">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"m.organization_id=s.organization_id">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"m.status='removed'">>)),
            {ok, 1}
        end),
        ?assertEqual(
            ok,
            enterprise_directory_source_repo:save_offboarding_tx(
                conn, 1, 10, <<"person">>, actual_removed
            )
        )
    end).

nonempty_department_is_not_cascaded_test() ->
    with_pg(fun() ->
        meck:expect(elib_pg, query, fun
            (conn, _, [10]) -> {ok, [#{<<"id">> => 20}]};
            (conn, _, [10, 20]) -> {ok, [#{<<"id">> => 21}]}
        end),
        ?assertEqual(
            {error, department_not_empty},
            enterprise_directory_source_repo:archive_department_tx(conn, 10, 20)
        ),
        ?assertEqual(0, meck:num_calls(elib_pg, execute, '_'))
    end).

source_department_admin_is_preserved_test() ->
    with_pg(fun() ->
        meck:new(organization_department_pg, [non_strict, no_link]),
        try
            meck:expect(elib_pg, query, fun
                (conn, _, [1, 10, 42]) -> {ok, [#{<<"department_id">> => 20}]};
                (conn, _, [20, 42]) -> {ok, [#{<<"user_id">> => 42}]}
            end),
            meck:expect(organization_department_pg, delete_member_tx, fun(conn, 20, 42) ->
                {ok, not_present}
            end),
            ?assertThrow(
                {abort_tx, local_department_admin_conflict},
                enterprise_directory_source_repo:department_members_tx(conn, 1, 10, 42, [])
            ),
            ?assertEqual(0, meck:num_calls(elib_pg, execute, '_'))
        after
            meck:unload(organization_department_pg)
        end
    end).

with_pg(Fun) ->
    meck:new(elib_pg, [non_strict, no_link]),
    try
        Fun()
    after
        meck:unload(elib_pg)
    end.
