-module(enterprise_directory_sync_logic_tests).
-export([
    with_sync_mocks/1,
    with_mocks/2,
    prepare_person/0,
    ctx/0,
    body/0,
    department/0
]).
-include_lib("eunit/include/eunit.hrl").

validation_test() ->
    Body = body(),
    ?assertEqual(ok, enterprise_directory_sync_logic:validate(Body)),
    ?assertEqual(
        {error, invalid_request},
        enterprise_directory_sync_logic:validate(Body#{<<"user_id">> => 42})
    ),
    Person = person(),
    ?assertEqual(
        {error, invalid_request},
        enterprise_directory_sync_logic:validate(
            Body#{<<"persons">> => [Person#{<<"role">> => <<"owner">>}]}
        )
    ),
    ?assertEqual(
        {error, invalid_request},
        enterprise_directory_sync_logic:validate(
            Body#{<<"persons">> => [Person, Person]}
        )
    ),
    ?assertEqual(
        {error, invalid_request},
        enterprise_directory_sync_logic:validate(
            Body#{<<"persons">> => [Person#{<<"source_version">> => 0}]}
        )
    ).

version_test() ->
    with_mocks([enterprise_internal_idempotency], fun() ->
        meck:expect(enterprise_internal_idempotency, request_digest, fun(_, _, _) ->
            {ok, <<"digest">>}
        end),
        Source = #{<<"source_version">> => 2, <<"payload_digest">> => <<"digest">>},
        ?assertEqual(
            replay, enterprise_directory_sync_logic:version(Source, #{<<"version">> => 2})
        ),
        ?assertEqual(
            advance, enterprise_directory_sync_logic:version(Source, #{<<"version">> => 3})
        ),
        ?assertEqual(
            {error, stale_source_version},
            enterprise_directory_sync_logic:version(Source, #{<<"version">> => 1})
        ),
        ?assertEqual(
            {error, source_version_conflict},
            enterprise_directory_sync_logic:version(
                Source#{<<"payload_digest">> => <<"other">>}, #{<<"version">> => 2}
            )
        )
    end).

replay_and_conflict_test() ->
    with_sync_mocks(fun() ->
        Snapshot = #{response_code => 200, response_body => <<"original">>},
        meck:expect(enterprise_internal_idempotency, begin_tx, fun(conn, _, _, _, _) ->
            {ok, replay, Snapshot}
        end),
        ?assertEqual(
            {ok, replay, Snapshot},
            enterprise_directory_sync_logic:sync_tx(conn, ctx(), <<"key">>, body())
        ),
        ?assertEqual(0, meck:num_calls(enterprise_audit_event_repo, append_tx, '_')),
        ?assertEqual(
            0, meck:num_calls(enterprise_directory_source_repo, save_organization_tx, '_')
        ),
        meck:expect(enterprise_internal_idempotency, begin_tx, fun(_, _, _, _, _) ->
            {error, digest_conflict}
        end),
        ?assertThrow(
            {abort_tx, digest_conflict},
            enterprise_directory_sync_logic:sync_tx(conn, ctx(), <<"key">>, body())
        )
    end).

same_connection_audit_failure_test() ->
    with_sync_mocks(fun() ->
        meck:expect(enterprise_internal_idempotency, begin_tx, fun(conn, _, _, _, _) ->
            {ok, inserted}
        end),
        meck:expect(organization_repo, update_tx, fun(conn, 10, _, null, null) -> {ok, #{}} end),
        meck:expect(enterprise_directory_source_repo, save_organization_tx, fun(conn, 1, _, 10, _) ->
            ok
        end),
        meck:expect(enterprise_audit_event_repo, append_tx, fun(conn, 10, _) ->
            {error, audit_failed}
        end),
        ?assertThrow(
            {abort_tx, audit_failed},
            enterprise_directory_sync_logic:sync_tx(conn, ctx(), <<"key">>, body())
        ),
        ?assertEqual(0, meck:num_calls(enterprise_internal_idempotency, complete_tx, '_'))
    end).

claim_projects_pending_membership_and_departments_test() ->
    with_sync_mocks(fun() ->
        meck:new(organization_department_pg, [non_strict, no_link]),
        try
            meck:expect(enterprise_source_person_repo, claim_tx, fun(conn, 1, <<"person">>, 42) ->
                {ok, #{<<"user_id">> => 42}}
            end),
            Pending = #{
                <<"organization_id">> => 10,
                <<"source_version">> => 1,
                <<"payload_digest">> => <<"digest">>,
                <<"managed">> => false,
                <<"projected_user_id">> => null,
                <<"department_external_ids">> => <<"[\"dept\"]">>
            },
            meck:expect(
                enterprise_directory_source_repo,
                pending_projections_tx,
                fun(conn, 1, <<"person">>) -> {ok, [Pending]} end
            ),
            meck:expect(
                enterprise_directory_source_repo,
                project_member_tx,
                fun(conn, 10, 42, <<"active">>, false, _) -> {ok, true} end
            ),
            meck:expect(
                enterprise_directory_source_repo,
                department_tx,
                fun(conn, 1, 10, <<"dept">>) -> {ok, #{<<"department_id">> => 20}} end
            ),
            meck:expect(
                organization_department_pg,
                fetch_department_tx,
                fun(conn, 10, 20) -> {ok, #{status => active}} end
            ),
            meck:expect(
                enterprise_directory_source_repo,
                department_members_tx,
                fun(conn, 1, 10, 42, [20]) -> ok end
            ),
            meck:expect(
                enterprise_directory_source_repo,
                save_membership_tx,
                fun(conn, 1, 10, <<"person">>, true, 42, #{<<"version">> := 1}) -> ok end
            ),
            meck:expect(enterprise_audit_event_repo, append_tx, fun(conn, 10, _) -> {ok, #{}} end),
            ?assertMatch(
                {ok, #{<<"user_id">> := 42}},
                enterprise_directory_sync_logic:claim_and_project_tx(conn, ctx(), <<"person">>, 42)
            ),
            ?assertEqual(
                1, meck:num_calls(enterprise_directory_source_repo, department_members_tx, '_')
            )
        after
            meck:unload(organization_department_pg)
        end
    end).

claim_projection_requires_current_target_grant_test() ->
    with_sync_mocks(fun() ->
        meck:expect(enterprise_source_person_repo, claim_tx, fun(conn, 1, _, 42) ->
            {ok, #{<<"user_id">> => 42}}
        end),
        meck:expect(
            enterprise_directory_source_repo,
            pending_projections_tx,
            fun(conn, 1, _) -> {ok, [#{<<"organization_id">> => 10}]} end
        ),
        meck:expect(
            enterprise_directory_source_repo,
            authorize_tx,
            fun(conn, #{organization_id := 10}) -> {error, organization_boundary_violation} end
        ),
        ?assertThrow(
            {abort_tx, organization_boundary_violation},
            enterprise_directory_sync_logic:claim_and_project_tx(conn, ctx(), <<"person">>, 42)
        ),
        ?assertEqual(0, meck:num_calls(enterprise_directory_source_repo, project_member_tx, '_'))
    end).

repeated_claim_does_not_reproject_governance_removal_test() ->
    with_sync_mocks(fun() ->
        meck:expect(enterprise_source_person_repo, claim_tx, fun(conn, 1, _, 42) ->
            {ok, #{<<"user_id">> => 42}}
        end),
        meck:expect(
            enterprise_directory_source_repo,
            pending_projections_tx,
            fun(conn, 1, _) -> {ok, []} end
        ),
        ?assertMatch(
            {ok, _},
            enterprise_directory_sync_logic:claim_and_project_tx(conn, ctx(), <<"person">>, 42)
        ),
        ?assertEqual(0, meck:num_calls(enterprise_directory_source_repo, project_member_tx, '_'))
    end).

actual_target_is_authorized_before_mutation_test() ->
    with_sync_mocks(fun() ->
        meck:expect(
            enterprise_directory_source_repo,
            authorize_tx,
            fun(conn, #{organization_id := 10}) -> {error, organization_boundary_violation} end
        ),
        ?assertThrow(
            {abort_tx, organization_boundary_violation},
            enterprise_directory_sync_logic:sync_tx(
                conn, (ctx())#{organization_id => 999}, <<"key">>, body()
            )
        ),
        ?assertEqual(0, meck:num_calls(enterprise_internal_idempotency, begin_tx, '_')),
        ?assertEqual(0, meck:num_calls(organization_repo, update_tx, '_'))
    end).

authorization_expiry_prevents_completion_test() ->
    with_sync_mocks(fun() ->
        meck:expect(
            enterprise_directory_source_repo,
            authorize_tx,
            2,
            meck:seq([{ok, #{}}, {error, organization_boundary_violation}])
        ),
        meck:expect(enterprise_internal_idempotency, begin_tx, fun(conn, _, _, _, _) ->
            {ok, inserted}
        end),
        meck:expect(organization_repo, update_tx, fun(conn, 10, _, null, null) -> {ok, #{}} end),
        meck:expect(enterprise_directory_source_repo, save_organization_tx, fun(conn, 1, _, 10, _) ->
            ok
        end),
        meck:expect(enterprise_audit_event_repo, append_tx, fun(conn, 10, _) -> {ok, #{}} end),
        ?assertThrow(
            {abort_tx, organization_boundary_violation},
            enterprise_directory_sync_logic:sync_tx(conn, ctx(), <<"key">>, body())
        ),
        ?assertEqual(0, meck:num_calls(enterprise_internal_idempotency, complete_tx, '_'))
    end).

source_authorization_failure_test() ->
    with_sync_mocks(fun() ->
        meck:expect(enterprise_directory_source_repo, lock_source_tx, fun(conn, _) ->
            {error, organization_boundary_violation}
        end),
        ?assertThrow(
            {abort_tx, organization_boundary_violation},
            enterprise_directory_sync_logic:sync_tx(conn, ctx(), <<"key">>, body())
        ),
        ?assertEqual(0, meck:num_calls(enterprise_directory_source_repo, organization_tx, '_'))
    end).

org_departure_does_not_retire_source_test() ->
    with_sync_mocks(fun() ->
        prepare_person(),
        Spec = (person())#{
            <<"membership">> => #{
                <<"version">> => 2,
                <<"status">> => <<"removed">>,
                <<"department_external_ids">> => []
            }
        },
        meck:expect(
            enterprise_directory_source_repo,
            project_member_tx,
            fun(conn, 10, 42, <<"removed">>, true, #{<<"status">> := <<"active">>}) ->
                {ok, remove}
            end
        ),
        meck:expect(
            organization_member_logic,
            directory_remove_tx,
            fun(conn, #{application_id := 1}, 10, 42) -> {ok, #{}} end
        ),
        ?assertMatch(
            {ok, applied, _},
            enterprise_directory_sync_logic:sync_tx(
                conn, ctx(), <<"key">>, (body())#{<<"persons">> => [Spec]}
            )
        ),
        ?assertEqual(0, meck:num_calls(enterprise_directory_source_repo, retire_person_tx, '_')),
        ?assertEqual(
            1,
            meck:num_calls(
                enterprise_directory_source_repo,
                save_offboarding_tx,
                [conn, 1, 10, '_', actual_removed]
            )
        ),
        ?assertEqual([{<<"active">>, 1}], [
            {Status, Version}
         || {_, {enterprise_source_person_repo, sync_tx, [conn, 1, _, _, Status, Version]}, _} <-
                meck:history(enterprise_source_person_repo)
        ])
    end).

restore_preserves_local_role_test() ->
    with_mocks([organization_member_repo, elib_pg], fun() ->
        meck:expect(
            organization_member_repo,
            find_for_update_tx,
            fun(conn, 10, 42, _) ->
                {ok, #{<<"status">> => <<"removed">>, <<"role">> => <<"admin">>}}
            end
        ),
        ?assertEqual(
            {error, local_membership_conflict},
            enterprise_directory_source_repo:project_member_tx(
                conn,
                10,
                42,
                <<"active">>,
                true,
                #{<<"status">> => <<"removed">>, <<"offboarding_member_updated_at">> => <<"stamp">>}
            )
        ),
        ?assertEqual(0, meck:num_calls(elib_pg, execute, '_'))
    end).

restore_passes_previous_source_status_test() ->
    with_sync_mocks(fun() ->
        prepare_person(),
        meck:expect(
            enterprise_directory_source_repo,
            membership_tx,
            fun(conn, 1, 10, _) ->
                {ok, #{
                    <<"managed">> => true,
                    <<"source_version">> => 1,
                    <<"status">> => <<"removed">>,
                    <<"projected_user_id">> => 42,
                    <<"offboarding_member_updated_at">> => <<"stamp">>
                }}
            end
        ),
        meck:expect(
            enterprise_directory_source_repo,
            project_member_tx,
            fun(
                conn,
                10,
                42,
                <<"active">>,
                true,
                #{
                    <<"status">> := <<"removed">>,
                    <<"offboarding_member_updated_at">> := <<"stamp">>
                }
            ) ->
                {ok, true}
            end
        ),
        Spec = (person())#{
            <<"membership">> => #{
                <<"version">> => 2,
                <<"status">> => <<"active">>,
                <<"department_external_ids">> => []
            }
        },
        ?assertMatch(
            {ok, applied, _},
            enterprise_directory_sync_logic:sync_tx(
                conn, ctx(), <<"key">>, (body())#{<<"persons">> => [Spec]}
            )
        )
    end).

owner_and_admin_guard_test() ->
    with_mocks(
        [
            enterprise_directory_source_repo,
            organization_repo,
            organization_member_repo,
            workspace_ds
        ],
        fun() ->
            meck:expect(enterprise_directory_source_repo, authorize_tx, fun(conn, _) ->
                {ok, #{}}
            end),
            meck:expect(enterprise_directory_source_repo, lock_source_tx, fun(conn, _) ->
                {ok, #{}}
            end),
            meck:expect(enterprise_directory_source_repo, authorize_member_tx, fun(conn, 1, 10, 42) ->
                ok
            end),
            meck:expect(
                organization_repo,
                find_for_update_tx,
                fun(conn, 10) -> {ok, #{<<"owner_id">> => 42, <<"status">> => <<"active">>}} end
            ),
            lists:foreach(
                fun(Role) ->
                    meck:expect(
                        organization_member_repo,
                        find_for_update_tx,
                        fun(conn, 10, 42, _) ->
                            {ok, #{<<"role">> => Role, <<"status">> => <<"active">>}}
                        end
                    ),
                    try organization_member_logic:directory_remove_tx(conn, ctx(), 10, 42) of
                        _ -> ?assert(false)
                    catch
                        throw:{abort_tx, {Code, _}} -> ?assert(lists:member(Code, [403, 409]))
                    end
                end,
                [<<"owner">>, <<"admin">>]
            ),
            ?assertEqual(0, meck:num_calls(workspace_ds, remove_organization_memberships_tx, '_'))
        end
    ).

machine_departure_reuses_cleanup_test() ->
    with_mocks(
        [
            enterprise_directory_source_repo,
            organization_repo,
            organization_member_repo,
            workspace_ds,
            elib_pg
        ],
        fun() ->
            meck:expect(enterprise_directory_source_repo, authorize_tx, fun(conn, _) ->
                {ok, #{}}
            end),
            meck:expect(enterprise_directory_source_repo, authorize_member_tx, fun(conn, 1, 10, 42) ->
                ok
            end),
            meck:expect(
                organization_repo,
                find_for_update_tx,
                fun(conn, 10) -> {ok, #{<<"owner_id">> => 99, <<"status">> => <<"active">>}} end
            ),
            meck:expect(
                organization_member_repo,
                find_for_update_tx,
                fun(conn, 10, 42, _) ->
                    {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}}
                end
            ),
            meck:expect(workspace_ds, remove_organization_memberships_tx, fun(conn, 10, 42) ->
                [#{workspace_id => 5}]
            end),
            meck:expect(organization_member_repo, remove_tx, fun(conn, 10, 42) -> ok end),
            meck:expect(elib_pg, query, fun(conn, _Sql, [42, 10]) ->
                ?assertEqual(
                    1, meck:num_calls(organization_member_repo, remove_tx, [conn, 10, 42])
                ),
                case get(directory_projection_error) of
                    true -> {error, custody_projection_failed};
                    _ -> {ok, []}
                end
            end),
            meck:expect(elib_pg, after_commit, fun(conn, _, Callback) ->
                put(directory_commit_callback, Callback),
                ok
            end),
            meck:expect(workspace_ds, member_removed, fun(_) -> ok end),
            ?assertMatch(
                {ok, _}, organization_member_logic:directory_remove_tx(conn, ctx(), 10, 42)
            ),
            ?assertEqual(0, meck:num_calls(workspace_ds, member_removed, '_')),
            Callback = erase(directory_commit_callback),
            ok = Callback(),
            ?assertEqual(1, meck:num_calls(workspace_ds, member_removed, '_')),
            ?assertEqual(1, meck:num_calls(elib_pg, query, [conn, '_', [42, 10]])),
            %% Projection failure aborts before any new commit callback is registered.
            meck:reset(organization_member_repo),
            put(directory_projection_error, true),
            try
                ?assertThrow(
                    {abort_tx, {internal, custody_projection_failed}},
                    organization_member_logic:directory_remove_tx(conn, ctx(), 10, 42)
                ),
                ?assertEqual(2, meck:num_calls(elib_pg, query, [conn, '_', [42, 10]])),
                ?assertEqual(1, meck:num_calls(elib_pg, after_commit, '_')),
                ?assertEqual(1, meck:num_calls(workspace_ds, member_removed, '_')),
                ?assertEqual(undefined, get(directory_commit_callback))
            after
                erase(directory_projection_error)
            end
        end
    ).

department_connection_test() ->
    with_mocks([elib_pg], fun() ->
        meck:expect(elib_pg, query, fun(conn, _, [10, 20, 42, null]) ->
            {ok, [#{<<"user_id">> => 42}]}
        end),
        meck:expect(elib_pg, execute, fun(conn, _, [20, 42]) -> {ok, 1} end),
        ?assertMatch(
            {ok, _, added}, organization_department_pg:insert_member_tx(conn, 10, 20, 42, null)
        ),
        ?assertEqual({ok, removed}, organization_department_pg:delete_member_tx(conn, 20, 42)),
        ?assertEqual(0, meck:num_calls(elib_pg, query, ['_', '_']))
    end).

department_create_and_parent_boundary_test() ->
    with_sync_mocks(fun() ->
        prepare_person(),
        meck:new(organization_department_pg, [non_strict, no_link]),
        try
            meck:expect(
                enterprise_directory_source_repo,
                department_tx,
                fun
                    (conn, 1, 10, <<"dept">>) -> {error, not_found};
                    (conn, 1, 10, <<"foreign">>) -> {error, not_found}
                end
            ),
            meck:expect(
                organization_department_pg,
                insert_department_tx,
                fun(conn, 10, null, <<"Dept">>, null, _) ->
                    {ok, #{id => 20, status => active, version => 1}}
                end
            ),
            meck:expect(
                enterprise_directory_source_repo,
                save_department_tx,
                fun(conn, 1, 10, <<"dept">>, 20, _) -> ok end
            ),
            Spec = department(),
            ?assertMatch(
                {ok, applied, _},
                enterprise_directory_sync_logic:sync_tx(
                    conn, ctx(), <<"key">>, (body())#{<<"departments">> => [Spec]}
                )
            ),
            ?assertThrow(
                {abort_tx, not_found},
                enterprise_directory_sync_logic:sync_tx(
                    conn, ctx(), <<"new-key">>, (body())#{
                        <<"departments">> =>
                            [Spec#{<<"parent_external_id">> => <<"foreign">>}]
                    }
                )
            )
        after
            meck:unload(organization_department_pg)
        end
    end).

department_cycle_failure_test() ->
    with_sync_mocks(fun() ->
        prepare_person(),
        meck:new(organization_department_pg, [non_strict, no_link]),
        try
            meck:expect(
                enterprise_directory_source_repo,
                department_tx,
                fun(conn, 1, 10, _) ->
                    {ok, #{<<"department_id">> => 20, <<"source_version">> => 1}}
                end
            ),
            meck:expect(
                organization_department_pg,
                fetch_department_tx,
                fun(conn, 10, 20) -> {ok, #{id => 20, status => active, version => 1}} end
            ),
            meck:expect(organization_department_pg, move_in_tx, fun(conn, 10, 20, 20, null, 1) ->
                {error, cycle}
            end),
            Spec = (department())#{<<"version">> => 2, <<"parent_external_id">> => <<"dept">>},
            ?assertThrow(
                {abort_tx, cycle},
                enterprise_directory_sync_logic:sync_tx(
                    conn, ctx(), <<"key">>, (body())#{<<"departments">> => [Spec]}
                )
            ),
            ?assertEqual(0, meck:num_calls(enterprise_audit_event_repo, append_tx, '_'))
        after
            meck:unload(organization_department_pg)
        end
    end).

unlinked_person_and_later_binding_test() ->
    with_sync_mocks(fun() ->
        prepare_person(),
        meck:expect(
            enterprise_source_person_repo,
            sync_tx,
            fun(conn, 1, _, _, _, _) -> {ok, #{<<"user_id">> => null}} end
        ),
        meck:expect(enterprise_directory_source_repo, membership_tx, fun(conn, 1, 10, _) ->
            {error, not_found}
        end),
        meck:expect(
            enterprise_directory_source_repo,
            save_membership_tx,
            fun(conn, 1, 10, _, false, null, _) -> ok end
        ),
        ?assertMatch(
            {ok, applied, _},
            enterprise_directory_sync_logic:sync_tx(
                conn, ctx(), <<"key">>, (body())#{<<"persons">> => [person()]}
            )
        ),
        ?assertEqual(0, meck:num_calls(enterprise_directory_source_repo, project_member_tx, '_')),
        meck:expect(
            enterprise_source_person_repo,
            sync_tx,
            fun(conn, 1, _, _, _, _) -> {ok, #{<<"user_id">> => 42}} end
        ),
        meck:expect(enterprise_directory_source_repo, membership_tx, fun(conn, 1, 10, _) ->
            {ok, #{
                <<"managed">> => false,
                <<"projected_user_id">> => null,
                <<"source_version">> => 1,
                <<"payload_digest">> => <<"digest">>
            }}
        end),
        meck:expect(
            enterprise_directory_source_repo,
            project_member_tx,
            fun(conn, 10, 42, <<"active">>, false, _) -> {ok, true} end
        ),
        meck:expect(
            enterprise_directory_source_repo,
            save_membership_tx,
            fun(conn, 1, 10, _, true, 42, _) -> ok end
        ),
        ?assertMatch(
            {ok, applied, _},
            enterprise_directory_sync_logic:sync_tx(
                conn, ctx(), <<"new-key">>, (body())#{<<"persons">> => [person()]}
            )
        )
    end).

same_version_does_not_restore_governance_removal_test() ->
    with_sync_mocks(fun() ->
        prepare_person(),
        meck:expect(enterprise_directory_source_repo, membership_tx, fun(conn, 1, 10, _) ->
            {ok, #{
                <<"managed">> => true,
                <<"projected_user_id">> => 42,
                <<"source_version">> => 1,
                <<"payload_digest">> => <<"digest">>
            }}
        end),
        ?assertMatch(
            {ok, applied, _},
            enterprise_directory_sync_logic:sync_tx(
                conn, ctx(), <<"key">>, (body())#{<<"persons">> => [person()]}
            )
        ),
        ?assertEqual(0, meck:num_calls(enterprise_directory_source_repo, project_member_tx, '_'))
    end).

retirement_is_explicit_and_other_orgs_block_it_test() ->
    with_sync_mocks(fun() ->
        prepare_person(),
        Spec = (person())#{
            <<"source_status">> => <<"removed">>,
            <<"source_version">> => 2,
            <<"membership">> => #{
                <<"version">> => 2,
                <<"status">> => <<"removed">>,
                <<"department_external_ids">> => []
            }
        },
        meck:expect(
            enterprise_directory_source_repo,
            project_member_tx,
            fun(conn, 10, 42, <<"removed">>, true, #{<<"status">> := <<"active">>}) ->
                {ok, true}
            end
        ),
        meck:expect(
            enterprise_directory_source_repo,
            retire_person_tx,
            fun(conn, 1, _) -> {error, active_source_memberships} end
        ),
        ?assertThrow(
            {abort_tx, active_source_memberships},
            enterprise_directory_sync_logic:sync_tx(
                conn, ctx(), <<"key">>, (body())#{<<"persons">> => [Spec]}
            )
        ),
        ?assertEqual(0, meck:num_calls(enterprise_source_person_repo, sync_tx, '_')),
        meck:expect(enterprise_directory_source_repo, retire_person_tx, fun(conn, 1, _) -> ok end),
        ?assertMatch(
            {ok, applied, _},
            enterprise_directory_sync_logic:sync_tx(
                conn, ctx(), <<"new-key">>, (body())#{<<"persons">> => [Spec]}
            )
        ),
        ?assertEqual(
            1,
            meck:num_calls(
                enterprise_source_person_repo,
                sync_tx,
                [conn, 1, '_', '_', <<"removed">>, 2]
            )
        )
    end).

prepare_person() ->
    meck:expect(enterprise_internal_idempotency, begin_tx, fun(conn, _, _, _, _) ->
        {ok, inserted}
    end),
    meck:expect(organization_repo, update_tx, fun(conn, 10, _, null, null) -> {ok, #{}} end),
    meck:expect(enterprise_directory_source_repo, save_organization_tx, fun(conn, 1, _, 10, _) ->
        ok
    end),
    meck:expect(enterprise_source_person_repo, find_tx, fun(conn, 1, _) ->
        {ok, #{<<"user_id">> => 42}}
    end),
    meck:expect(enterprise_source_person_repo, sync_tx, fun(conn, 1, _, _, _, _) ->
        {ok, #{<<"user_id">> => 42}}
    end),
    meck:expect(
        enterprise_directory_source_repo,
        membership_tx,
        fun(conn, 1, 10, _) ->
            {ok, #{
                <<"managed">> => true,
                <<"source_version">> => 1,
                <<"status">> => <<"active">>
            }}
        end
    ),
    meck:expect(enterprise_directory_source_repo, department_members_tx, fun(conn, 1, 10, 42, []) ->
        ok
    end),
    meck:expect(enterprise_directory_source_repo, save_membership_tx, fun(
        conn, 1, 10, _, true, 42, _
    ) ->
        ok
    end),
    meck:expect(enterprise_audit_event_repo, append_tx, fun(conn, 10, _) -> {ok, 123} end),
    meck:expect(enterprise_internal_idempotency, complete_tx, fun(conn, _, _, _, 10, 200, _) ->
        ok
    end).

with_sync_mocks(Fun) ->
    with_mocks(
        [
            enterprise_internal_idempotency,
            enterprise_directory_source_repo,
            organization_repo,
            enterprise_audit_event_repo,
            enterprise_source_person_repo,
            organization_member_logic
        ],
        fun() ->
            meck:expect(enterprise_internal_idempotency, serialize_tx, fun(_, _, _) -> ok end),
            meck:expect(enterprise_internal_idempotency, valid_key, fun(_) -> true end),
            meck:expect(enterprise_internal_idempotency, request_digest, fun(_, _, _) ->
                {ok, <<"digest">>}
            end),
            meck:expect(enterprise_directory_source_repo, authorize_tx, fun(conn, _) ->
                {ok, #{}}
            end),
            meck:expect(enterprise_directory_source_repo, lock_source_tx, fun(conn, _) ->
                {ok, #{}}
            end),
            meck:expect(enterprise_directory_source_repo, save_offboarding_tx, fun(
                conn, 1, 10, _, _
            ) ->
                ok
            end),
            meck:expect(
                enterprise_directory_source_repo,
                organization_tx,
                fun(conn, 1, _) ->
                    {ok, #{<<"organization_id">> => 10, <<"source_version">> => 0}}
                end
            ),
            meck:expect(
                organization_repo,
                find_for_update_tx,
                fun(conn, 10) -> {ok, #{<<"id">> => 10, <<"status">> => <<"active">>}} end
            ),
            Fun()
        end
    ).

with_mocks(Modules, Fun) ->
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict, no_link]) end, Modules),
    try
        Fun()
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

ctx() -> #{application_id => 1, credential_id => 2, authentication => hmac}.
body() ->
    #{
        <<"organization">> => #{
            <<"external_id">> => <<"org">>,
            <<"name">> => <<"Org">>,
            <<"version">> => 1
        },
        <<"departments">> => [],
        <<"persons">> => []
    }.
person() ->
    #{
        <<"external_user_id">> => <<"person">>,
        <<"display_name">> => <<"Name">>,
        <<"source_status">> => <<"active">>,
        <<"source_version">> => 1,
        <<"membership">> => #{
            <<"version">> => 1,
            <<"status">> => <<"active">>,
            <<"department_external_ids">> => []
        }
    }.
department() ->
    #{
        <<"external_id">> => <<"dept">>,
        <<"parent_external_id">> => null,
        <<"name">> => <<"Dept">>,
        <<"status">> => <<"active">>,
        <<"version">> => 1
    }.
