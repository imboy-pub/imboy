-module(enterprise_directory_source_config_logic_tests).
-include_lib("eunit/include/eunit.hrl").

configuration_wrapper_uses_transaction_test() ->
    mocks(fun() ->
        rows([], row(), <<"[\"identities:write\"]">>),
        meck:expect(elib_pg, with_tx, fun(Fun) -> Fun(conn) end),
        ?assertMatch(
            {ok, _},
            enterprise_directory_source_config_logic:configure_source(
                #{adm_user_id => 7}, 1, <<"org">>, spec()
            )
        ),
        meck:expect(enterprise_audit_event_repo, append_tx, fun(_, _, _) ->
            {error, audit_failed}
        end),
        ?assertThrow(
            {abort_tx, audit_failed},
            enterprise_directory_source_config_logic:configure_source(
                #{adm_user_id => 7}, 1, <<"org">>, spec()
            )
        )
    end).

connection_is_instance_directory_only_test() ->
    mocks(fun() ->
        connection_mocks(),
        ?assertMatch(
            {ok, #{<<"id">> := 1}},
            enterprise_directory_source_config_logic:create_connection(
                connection_input(), #{adm_user_id => 7}
            )
        ),
        ?assertEqual(1, meck:num_calls(enterprise_application_repo, create_tx, '_')),
        ?assertEqual(1, meck:num_calls(enterprise_audit_event_repo, append_tx, '_')),
        ?assertEqual(0, meck:num_calls(enterprise_application_grant_repo, '_', '_'))
    end).

connection_rejects_untrusted_fields_test() ->
    lists:foreach(
        fun(Field) ->
            ?assertEqual(
                {error, invalid_directory_connection},
                enterprise_directory_source_config_logic:create_connection(
                    (connection_input())#{Field => [<<"messages:send">>]}, #{adm_user_id => 7}
                )
            )
        end,
        [allowed_scopes, organization_id, target_organization_ids, owner_id, principal_user_id]
    ),
    ?assertEqual(
        {error, invalid_admin},
        enterprise_directory_source_config_logic:create_connection(connection_input(), #{})
    ).

connection_validates_existing_fields_test() ->
    lists:foreach(
        fun({Field, Value, Reason}) ->
            ?assertEqual(
                {error, Reason},
                enterprise_directory_source_config_logic:create_connection(
                    (connection_input())#{Field => Value}, #{adm_user_id => 7}
                )
            )
        end,
        [
            {application_key, <<"short">>, invalid_application_key},
            {application_key, <<"directory\nkey">>, invalid_application_key},
            {name, <<"  ">>, invalid_name},
            {redirect_uris, [<<"http://example.test">>], invalid_redirect_uris},
            {redirect_uris, [123], invalid_redirect_uris},
            {redirect_uris, [<<"https://example.test#x">>], invalid_redirect_uris},
            {redirect_uris, [<<"https://example.test">>, <<"https://example.test">>],
                invalid_redirect_uris}
        ]
    ).

connection_failures_abort_transaction_test() ->
    mocks(fun() ->
        connection_mocks(),
        meck:expect(elib_pg, query, fun(_, _, _) -> {ok, []} end),
        ?assertThrow(
            {abort_tx, invalid_admin},
            enterprise_directory_source_config_logic:create_connection(
                connection_input(), #{adm_user_id => 7}
            )
        ),
        ?assertEqual(0, meck:num_calls(enterprise_application_repo, create_tx, '_')),
        connection_mocks(),
        meck:expect(enterprise_application_repo, create_tx, fun(_, _, _, _, _, _) ->
            {error, key_conflict}
        end),
        ?assertThrow(
            {abort_tx, key_conflict},
            enterprise_directory_source_config_logic:create_connection(
                connection_input(), #{adm_user_id => 7}
            )
        ),
        connection_mocks(),
        meck:expect(enterprise_audit_event_repo, append_tx, fun(_, _, _) ->
            {error, audit_failed}
        end),
        ?assertThrow(
            {abort_tx, audit_failed},
            enterprise_directory_source_config_logic:create_connection(
                connection_input(), #{adm_user_id => 7}
            )
        )
    end).

connection_input() ->
    #{
        application_key => <<"directory-app">>,
        name => <<"Directory">>,
        redirect_uris => [<<"https://example.test/callback">>]
    }.

connection_mocks() ->
    meck:expect(elib_pg, with_tx, fun(Fun) -> Fun(conn) end),
    meck:expect(elib_pg, query, fun(conn, _, [7]) -> {ok, [#{<<"id">> => 7}]} end),
    meck:expect(
        enterprise_application_repo,
        create_tx,
        fun(
            conn,
            null,
            <<"directory-app">>,
            <<"Directory">>,
            {null, [<<"identities:write">>, <<"identities:read">>]},
            [<<"https://example.test/callback">>]
        ) ->
            {ok, #{<<"id">> => 1, <<"organization_id">> => null}}
        end
    ),
    meck:expect(
        enterprise_audit_event_repo,
        append_tx,
        fun(
            conn,
            null,
            #{
                action := <<"directory.connection_created">>,
                actor_role := <<"platform_admin">>,
                detail := #{<<"adm_user_id">> := 7}
            }
        ) ->
            {ok, #{}}
        end
    ).

configuration_rejects_client_targets_test() ->
    ?assertThrow(
        {abort_tx, invalid_source_configuration},
        enterprise_directory_source_config_logic:configure_source_tx(
            conn,
            #{adm_user_id => 7},
            1,
            <<"org">>,
            (spec())#{organization_id => 99}
        )
    ),
    ?assertThrow(
        {abort_tx, invalid_source_configuration},
        enterprise_directory_source_config_logic:configure_source_tx(
            conn, #{}, 1, <<"org">>, spec()
        )
    ).

trusted_configuration_is_audited_test() ->
    mocks(fun() ->
        rows([], row(), <<"[\"identities:write\"]">>),
        ?assertMatch(
            {ok, #{<<"configuration_version">> := 1}},
            enterprise_directory_source_config_logic:configure_source_tx(
                conn,
                #{adm_user_id => 7},
                1,
                <<"org">>,
                spec()
            )
        ),
        ?assertEqual(1, meck:num_calls(enterprise_audit_event_repo, append_tx, '_'))
    end).

wide_application_cannot_enable_source_test() ->
    mocks(fun() ->
        rows([], row(), <<"[\"identities:write\",\"messages:send\"]">>),
        ?assertThrow(
            {abort_tx, directory_only_application_required},
            enterprise_directory_source_config_logic:configure_source_tx(
                conn,
                #{adm_user_id => 7},
                1,
                <<"org">>,
                spec()
            )
        ),
        ?assertEqual(0, meck:num_calls(enterprise_audit_event_repo, append_tx, '_'))
    end).

configuration_cas_failure_test() ->
    mocks(fun() ->
        rows([row()], row(), <<"[\"identities:write\"]">>),
        ?assertThrow(
            {abort_tx, configuration_conflict},
            enterprise_directory_source_config_logic:configure_source_tx(
                conn,
                #{adm_user_id => 7},
                1,
                <<"org">>,
                spec()
            )
        )
    end).

configuration_audit_failure_aborts_test() ->
    mocks(fun() ->
        rows([], row(), <<"[\"identities:write\"]">>),
        meck:expect(enterprise_audit_event_repo, append_tx, fun(conn, _, _) ->
            {error, audit_failed}
        end),
        ?assertThrow(
            {abort_tx, audit_failed},
            enterprise_directory_source_config_logic:configure_source_tx(
                conn, #{adm_user_id => 7}, 1, <<"org">>, spec()
            )
        )
    end).

disable_revokes_only_linked_bootstrap_grant_test() ->
    mocks(fun() ->
        Previous = (row())#{<<"organization_id">> => 10, <<"bootstrap_grant_id">> => 30},
        rows([Previous], Previous#{<<"enabled">> => false}, <<"[\"identities:write\"]">>),
        meck:expect(
            enterprise_application_grant_repo,
            revoke_admin_tx,
            fun(conn, 10, 1, 30, 2, 7) -> ok end
        ),
        ?assertMatch(
            {ok, _},
            enterprise_directory_source_config_logic:configure_source_tx(
                conn,
                #{adm_user_id => 7},
                1,
                <<"org">>,
                (spec())#{expected_version => 1, enabled => false}
            )
        ),
        ?assertEqual(1, meck:num_calls(enterprise_application_grant_repo, revoke_admin_tx, '_'))
    end).

enable_requires_existing_bootstrap_authorization_test() ->
    mocks(fun() ->
        Previous = (row())#{<<"organization_id">> => 10, <<"bootstrap_grant_id">> => 30},
        lists:foreach(
            fun(Allowed) ->
                rows([Previous], Previous, <<"[\"identities:write\"]">>, Allowed),
                case Allowed of
                    true ->
                        ?assertMatch(
                            {ok, _},
                            enterprise_directory_source_config_logic:configure_source_tx(
                                conn, #{adm_user_id => 7}, 1, <<"org">>, (spec())#{
                                    expected_version => 1
                                }
                            )
                        );
                    false ->
                        ?assertThrow(
                            {abort_tx, bootstrap_grant_conflict},
                            enterprise_directory_source_config_logic:configure_source_tx(
                                conn, #{adm_user_id => 7}, 1, <<"org">>, (spec())#{
                                    expected_version => 1
                                }
                            )
                        )
                end
            end,
            [true, false]
        )
    end).

bound_source_cannot_change_initial_owner_test() ->
    mocks(fun() ->
        Previous = (row())#{<<"organization_id">> => 10, <<"confirmed_owner_user_id">> => 98},
        rows([Previous], Previous, <<"[\"identities:write\"]">>),
        ?assertThrow(
            {abort_tx, initial_owner_immutable},
            enterprise_directory_source_config_logic:configure_source_tx(
                conn,
                #{adm_user_id => 7},
                1,
                <<"org">>,
                (spec())#{expected_version => 1}
            )
        ),
        ?assertEqual(0, meck:num_calls(enterprise_audit_event_repo, append_tx, '_'))
    end).

rows(Previous, Updated, Scopes) ->
    rows(Previous, Updated, Scopes, true).

rows(Previous, Updated, Scopes, Allowed) ->
    meck:expect(elib_pg, query, fun(conn, Sql, Params) ->
        case Params of
            [7] ->
                {ok, [#{<<"id">> => 7}]};
            [1] ->
                {ok, [#{<<"id">> => 1, <<"allowed_scopes">> => Scopes}]};
            [<<"2099-01-01T00:00:00Z">>] ->
                {ok, [#{<<"expires_at">> => <<"2099-01-01T00:00:00Z">>}]};
            [1, <<"org">>] ->
                {ok, Previous};
            [30, 1, 10, _] ->
                {ok, [#{<<"allowed">> => Allowed}]};
            [30, 1, 10] ->
                ?assertNotEqual(
                    nomatch, binary:match(Sql, <<"provisioning_source='directory_bootstrap'">>)
                ),
                {ok, [#{<<"version">> => 2, <<"status">> => <<"active">>}]};
            [1, <<"org">>, 99, _, _, 7] ->
                {ok, [Updated]}
        end
    end),
    meck:expect(enterprise_directory_source_repo, confirmed_owner_tx, fun(conn, 99) -> {ok, #{}} end),
    meck:expect(
        enterprise_audit_event_repo,
        append_tx,
        fun(conn, _, #{actor_role := <<"platform_admin">>, detail := #{<<"adm_user_id">> := 7}}) ->
            {ok, #{}}
        end
    ).

row() ->
    #{
        <<"organization_id">> => null,
        <<"confirmed_owner_user_id">> => 99,
        <<"authorization_expires_at">> => <<"2099-01-01T00:00:00Z">>,
        <<"configuration_version">> => 1,
        <<"enabled">> => true,
        <<"bootstrap_grant_id">> => null
    }.
spec() ->
    #{
        confirmed_owner_user_id => 99,
        enabled => true,
        authorization_expires_at => <<"2099-01-01T00:00:00Z">>,
        expected_version => 0
    }.
mocks(Fun) ->
    Modules = [
        elib_pg,
        enterprise_directory_source_repo,
        enterprise_audit_event_repo,
        enterprise_application_grant_repo,
        enterprise_application_repo
    ],
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict, no_link]) end, Modules),
    try
        Fun()
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.
