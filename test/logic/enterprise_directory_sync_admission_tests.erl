-module(enterprise_directory_sync_admission_tests).
-include_lib("eunit/include/eunit.hrl").

transaction_wrapper_rolls_back_on_audit_failure_test() ->
    enterprise_directory_sync_logic_tests:with_sync_mocks(fun() ->
        meck:new(elib_pg, [non_strict, no_link]),
        try
            meck:expect(elib_pg, with_tx, fun(Callback) ->
                put(directory_test_writes, []),
                try
                    Callback(conn)
                catch
                    throw:{abort_tx, Reason} ->
                        erase(directory_test_writes),
                        {error, Reason}
                end
            end),
            meck:expect(enterprise_internal_idempotency, begin_tx, fun(conn, _, _, _, _) ->
                {ok, inserted}
            end),
            meck:expect(organization_repo, update_tx, fun(conn, 10, _, null, null) ->
                put(directory_test_writes, [organization]),
                {ok, #{}}
            end),
            meck:expect(enterprise_directory_source_repo, save_organization_tx, fun(
                conn, 1, _, 10, _
            ) ->
                put(directory_test_writes, [source | get(directory_test_writes)]),
                ok
            end),
            meck:expect(enterprise_audit_event_repo, append_tx, fun(conn, 10, _) ->
                {error, audit_failed}
            end),
            ?assertEqual(
                {error, audit_failed},
                enterprise_directory_sync_logic:sync(
                    enterprise_directory_sync_logic_tests:ctx(),
                    <<"key">>,
                    enterprise_directory_sync_logic_tests:body()
                )
            ),
            ?assertEqual(undefined, get(directory_test_writes))
        after
            meck:unload(elib_pg)
        end
    end).

human_governance_is_separate_test() ->
    enterprise_directory_sync_logic_tests:with_mocks(
        [
            elib_pg,
            organization_member_repo,
            workspace_ds,
            elib_log,
            enterprise_directory_source_repo
        ],
        fun() ->
            meck:expect(elib_pg, with_tx, fun(Callback) ->
                try
                    Callback(conn)
                catch
                    throw:{abort_tx, Reason} -> {error, Reason}
                end
            end),
            meck:expect(
                organization_member_repo,
                find_organization_for_share_tx,
                fun(conn, 10, _) ->
                    {ok, #{<<"id">> => 10, <<"owner_id">> => 99, <<"status">> => <<"active">>}}
                end
            ),
            meck:expect(
                organization_member_repo,
                find_active_for_share_tx,
                fun
                    (conn, 10, 99, _) -> {ok, #{<<"role">> => <<"owner">>}};
                    (conn, 10, 98, _) -> {ok, #{<<"role">> => <<"admin">>}}
                end
            ),
            meck:expect(
                organization_member_repo,
                find_for_update_tx,
                fun(conn, 10, 42, _) ->
                    {ok, #{<<"role">> => <<"admin">>, <<"status">> => <<"active">>}}
                end
            ),
            meck:expect(workspace_ds, remove_organization_memberships_tx, fun(conn, 10, 42) ->
                []
            end),
            meck:expect(organization_member_repo, remove_tx, fun(conn, 10, 42) -> ok end),
            meck:expect(elib_pg, query, fun(conn, _Sql, [42, 10]) -> {ok, []} end),
            meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
            ?assertMatch({ok, _}, organization_member_logic:remove(99, 10, 42)),
            ?assertEqual(1, meck:num_calls(elib_pg, query, [conn, '_', [42, 10]])),
            ?assertMatch({error, {403, _}}, organization_member_logic:remove(98, 10, 42)),
            ?assertEqual(1, meck:num_calls(elib_pg, query, [conn, '_', [42, 10]])),
            meck:expect(elib_pg, query, fun(conn, _Sql, [42, 10]) ->
                {error, custody_projection_failed}
            end),
            ?assertMatch({error, {500, _}}, organization_member_logic:remove(99, 10, 42)),
            ?assertEqual(2, meck:num_calls(elib_pg, query, [conn, '_', [42, 10]])),
            ?assertEqual(0, meck:num_calls(enterprise_directory_source_repo, authorize_tx, '_'))
        end
    ).

initial_owner_cannot_come_from_body_test() ->
    enterprise_directory_sync_logic_tests:with_sync_mocks(fun() ->
        enterprise_directory_sync_logic_tests:prepare_person(),
        meck:expect(enterprise_directory_source_repo, organization_tx, fun(conn, 1, _) ->
            {ok, #{
                <<"organization_id">> => null,
                <<"confirmed_owner_user_id">> => 99,
                <<"source_version">> => 0
            }}
        end),
        meck:expect(
            enterprise_directory_source_repo,
            bootstrap_tx,
            fun(conn, _, #{<<"confirmed_owner_user_id">> := 99}, _) ->
                {error, confirmed_human_owner_required}
            end
        ),
        meck:expect(organization_repo, create_tx, fun(conn, 99, <<"Org">>, <<"active">>) ->
            {ok, #{<<"id">> => 10, <<"status">> => <<"active">>}}
        end),
        ?assertThrow(
            {abort_tx, confirmed_human_owner_required},
            enterprise_directory_sync_logic:sync_tx(
                conn,
                enterprise_directory_sync_logic_tests:ctx(),
                <<"key">>,
                enterprise_directory_sync_logic_tests:body()
            )
        ),
        ?assertEqual(0, meck:num_calls(organization_repo, create_tx, '_')),
        Org = maps:get(<<"organization">>, enterprise_directory_sync_logic_tests:body()),
        ?assertEqual(
            {error, invalid_request},
            enterprise_directory_sync_logic:validate(
                (enterprise_directory_sync_logic_tests:body())#{
                    <<"organization">> => Org#{<<"owner_id">> => 42}
                }
            )
        )
    end).

bootstrap_continues_sync_and_audits_same_transaction_test() ->
    enterprise_directory_sync_logic_tests:with_sync_mocks(fun() ->
        enterprise_directory_sync_logic_tests:prepare_person(),
        meck:expect(
            enterprise_directory_source_repo,
            organization_tx,
            fun(conn, 1, _) -> {ok, #{<<"organization_id">> => null, <<"source_version">> => 0}} end
        ),
        meck:expect(
            enterprise_directory_source_repo,
            bootstrap_tx,
            fun(conn, _, _, #{<<"name">> := <<"Org">>}) ->
                {ok, #{<<"id">> => 10, <<"owner_id">> => 99, <<"status">> => <<"active">>}}
            end
        ),
        ?assertMatch(
            {ok, applied, _},
            enterprise_directory_sync_logic:sync_tx(
                conn,
                enterprise_directory_sync_logic_tests:ctx(),
                <<"key">>,
                enterprise_directory_sync_logic_tests:body()
            )
        ),
        ?assertEqual(2, meck:num_calls(enterprise_audit_event_repo, append_tx, '_')),
        ?assertEqual(1, meck:num_calls(enterprise_internal_idempotency, complete_tx, '_'))
    end).

child_before_parent_input_is_ordered_test() ->
    enterprise_directory_sync_logic_tests:with_sync_mocks(fun() ->
        enterprise_directory_sync_logic_tests:prepare_person(),
        meck:new(organization_department_pg, [non_strict, no_link]),
        try
            meck:expect(
                enterprise_directory_source_repo,
                department_tx,
                fun
                    (conn, 1, 10, <<"dept">>) ->
                        case get(directory_parent_created) of
                            true -> {ok, #{<<"department_id">> => 20}};
                            _ -> {error, not_found}
                        end;
                    (conn, 1, 10, <<"child">>) ->
                        {error, not_found}
                end
            ),
            meck:expect(
                organization_department_pg,
                insert_department_tx,
                fun
                    (conn, 10, null, <<"Dept">>, null, _) -> {ok, #{id => 20}};
                    (conn, 10, 20, <<"Child">>, null, _) -> {ok, #{id => 21}}
                end
            ),
            meck:expect(
                organization_department_pg,
                fetch_department_tx,
                fun(conn, 10, 20) -> {ok, #{id => 20, status => active}} end
            ),
            meck:expect(
                enterprise_directory_source_repo,
                save_department_tx,
                fun
                    (conn, 1, 10, <<"dept">>, 20, _) ->
                        put(directory_parent_created, true),
                        ok;
                    (conn, 1, 10, <<"child">>, 21, _) ->
                        ok
                end
            ),
            Child = (enterprise_directory_sync_logic_tests:department())#{
                <<"external_id">> => <<"child">>,
                <<"name">> => <<"Child">>,
                <<"parent_external_id">> => <<"dept">>
            },
            ?assertMatch(
                {ok, applied, _},
                enterprise_directory_sync_logic:sync_tx(
                    conn,
                    enterprise_directory_sync_logic_tests:ctx(),
                    <<"key">>,
                    (enterprise_directory_sync_logic_tests:body())#{
                        <<"departments">> => [
                            Child, enterprise_directory_sync_logic_tests:department()
                        ]
                    }
                )
            )
        after
            erase(directory_parent_created),
            meck:unload(organization_department_pg)
        end
    end).
