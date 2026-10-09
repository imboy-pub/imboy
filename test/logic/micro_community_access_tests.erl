-module(micro_community_access_tests).
-include_lib("eunit/include/eunit.hrl").
-include("micro_community_test.hrl").

enabled_predicate_preserves_permanent_paid_and_wall_clock_test() ->
    Sql = micro_community_access_repo:predicate_sql(<<"g.id">>, <<"$2">>, true),
    [
        ?assertNotEqual(nomatch, binary:match(Sql, Fragment))
     || Fragment <-
            [
                <<"NOT EXISTS">>,
                <<"mo.subscription_end_at IS NULL">>,
                <<"clock_timestamp()">>,
                <<"mo.status = 1">>,
                <<"ms.status = 1">>,
                <<"mg.owner_uid = mc.creator_uid">>
            ]
    ],
    ?assertEqual(nomatch, binary:match(Sql, <<"NOW()">>)).

disabled_predicate_denies_bound_but_retains_unbound_test() ->
    Sql = micro_community_access_repo:predicate_sql(<<"$1">>, <<"$2">>, false),
    ?assertEqual(
        <<"NOT EXISTS (SELECT 1 FROM public.channel mc WHERE mc.discussion_group_id = $1)">>, Sql
    ).

delivery_formats_test_() ->
    Formats = [
        {c2g_to, #{<<"type">> => <<"C2G">>, <<"to">> => <<"20">>}},
        {c2g_to_id, #{<<"type">> => <<"C2G">>, <<"to_id">> => 20}},
        {s2c_gid, #{<<"type">> => <<"S2C">>, <<"payload">> => #{<<"gid">> => <<"20">>}}},
        {s2c_group_id, #{<<"type">> => <<"S2C">>, <<"payload">> => #{<<"group_id">> => 20}}},
        {json_envelope, <<"{\"type\":\"C2G\",\"to\":\"20\"}">>}
    ],
    [
        {
            atom_to_list(Name),
            ?MC_WITH_MECKS(
                [{micro_community_access_repo, [{delivery_allowed, 2, fun(20, 7) -> true end}]}],
                fun() ->
                    ?assert(micro_community_access_ds:delivery_allowed(7, Msg)),
                    ?assertEqual(
                        1, meck:num_calls(micro_community_access_repo, delivery_allowed, [20, 7])
                    )
                end
            )
        }
     || {Name, Msg} <- Formats
    ].

list_delivery_requires_every_group_message_test_() ->
    ?MC_WITH_MECKS(
        [
            {micro_community_access_repo, [
                {delivery_allowed, 2, fun
                    (20, 7) -> true;
                    (21, 7) -> false
                end}
            ]}
        ],
        fun() ->
            Allowed = #{<<"type">> => <<"C2G">>, <<"to">> => 20},
            Denied = #{<<"type">> => <<"S2C">>, <<"payload">> => #{<<"gid">> => 21}},
            ?assert(micro_community_access_ds:delivery_allowed(7, [ordinary(), Allowed])),
            ?assertNot(micro_community_access_ds:delivery_allowed(7, [Allowed, Denied]))
        end
    ).

ordinary_messages_do_not_gain_a_new_acl_test_() ->
    ?MC_WITH_MECKS(
        [
            {micro_community_access_repo, [
                {delivery_allowed, 2, fun(_, _) -> erlang:error(unexpected_acl) end}
            ]}
        ],
        fun() ->
            ?assert(micro_community_access_ds:delivery_allowed(7, ordinary())),
            ?assert(
                micro_community_access_ds:delivery_allowed(
                    7,
                    #{<<"type">> => <<"S2C">>, <<"payload">> => #{<<"notice">> => <<"hello">>}}
                )
            ),
            ?assertEqual(0, meck:num_calls(micro_community_access_repo, delivery_allowed, '_'))
        end
    ).

delivery_query_exception_fails_closed_test_() ->
    ?MC_WITH_MECKS(
        [
            {micro_community_access_repo, [
                {delivery_allowed, 2, fun(_, _) -> erlang:error(db_unavailable) end}
            ]}
        ],
        fun() ->
            ?assertNot(
                micro_community_access_ds:delivery_allowed(
                    7,
                    #{<<"type">> => <<"C2G">>, <<"to">> => 20}
                )
            )
        end
    ).

malformed_json_delivery_denied_test() ->
    ?assertNot(micro_community_access_ds:delivery_allowed(7, <<"{invalid">>)),
    ?assertNot(micro_community_access_ds:delivery_allowed(7, null)).

delivery_repo_rechecks_active_membership_with_bound_parameters_test_() ->
    with_live_policy(
        [
            {elib_pg, [
                {one, 2, fun(Sql, Params) ->
                    ?assertEqual([20, 7], Params),
                    [
                        ?assertNotEqual(nomatch, binary:match(Sql, Fragment))
                     || Fragment <-
                            [
                                <<"gm.group_id=$1">>,
                                <<"gm.user_id=$2">>,
                                <<"gm.status=1">>,
                                <<"g.status=1">>
                            ]
                    ],
                    {ok, #{<<"allowed">> => true}}
                end}
            ]}
        ],
        fun() -> ?assert(micro_community_access_repo:delivery_allowed(20, 7)) end
    ).

delivery_repo_database_error_denied_test_() ->
    with_live_policy(
        [{elib_pg, [{one, 2, fun(_, _) -> {error, disconnected} end}]}],
        fun() -> ?assertNot(micro_community_access_repo:delivery_allowed(20, 7)) end
    ).

eligible_database_error_denied_test_() ->
    with_live_policy(
        [{elib_pg, [{one, 2, fun(_, _) -> {error, disconnected} end}]}],
        fun() -> ?assertNot(micro_community_access_repo:eligible(20, 7)) end
    ).

unbound_lock_does_not_touch_group_or_sequence_test_() ->
    ?MC_WITH_MECKS(
        [
            {elib_pg, [
                {query, 3, fun(conn, Sql, Params) ->
                    case binary:match(Sql, <<"LOCK TABLE public.channel">>) of
                        {0, _} ->
                            ?assertEqual([], Params),
                            {ok, []};
                        nomatch ->
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"WHERE discussion_group_id=$1">>)
                            ),
                            ?assertEqual([20], Params),
                            ?assertEqual(nomatch, binary:match(Sql, <<"msg_store_seq">>)),
                            {ok, []}
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, micro_community_access_repo:lock_group_tx(conn, 20)),
            ?assertEqual(2, meck:num_calls(elib_pg, query, '_'))
        end
    ).

join_transaction_failure_does_not_create_membership_test_() ->
    ?MC_WITH_MECKS(
        [
            {elib_pg, [{with_tx, 1, fun(_) -> throw({abort_tx, deadlock_detected}) end}]},
            {group_member_ds, [
                {join_group, 5, fun(_, _, _, _, _) -> erlang:error(unexpected_join) end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, {503, _}}, micro_community_access_ds:join(7, 10)),
            ?assertEqual(0, meck:num_calls(group_member_ds, join_group, '_'))
        end
    ).

join_closed_profile_and_feature_have_zero_ds_calls_test_() ->
    [
        {Name,
            ?MC_WITH_MECKS(
                [
                    {imboy_policy, [{current_profile, 0, fun() -> Profile end}]},
                    {imboy_feature, [{enabled, 1, fun(channel) -> Enabled end}]},
                    {micro_community_access_ds, [
                        {join, 2, fun(_, _) -> erlang:error(unexpected_join) end}
                    ]}
                ],
                fun() ->
                    ?assertMatch({error, {403, _}}, micro_community_access_logic:join(7, 10)),
                    ?assertEqual(0, meck:num_calls(micro_community_access_ds, join, '_'))
                end
            )}
     || {Name, Profile, Enabled} <- [
            {"phase1 blocks even enabled channel", phase1_delivery, true},
            {"disabled channel blocks community", community, false}
        ]
    ].

invalid_join_input_has_zero_ds_calls_test_() ->
    ?MC_WITH_MECKS(
        [
            {imboy_policy, [{current_profile, 0, fun() -> community end}]},
            {imboy_feature, [{enabled, 1, fun(channel) -> true end}]},
            {micro_community_access_ds, [{join, 2, fun(_, _) -> erlang:error(unexpected_join) end}]}
        ],
        fun() ->
            [
                ?assertMatch({error, _}, micro_community_access_logic:join(Uid, Cid))
             || {Uid, Cid} <-
                    [{0, 10}, {7, 0}, {7, -1}, {7, <<"not-an-id">>}, {7, 9223372036854775808}]
            ],
            ?assertEqual(0, meck:num_calls(micro_community_access_ds, join, '_'))
        end
    ).

transaction_lock_exception_must_propagate_test_() ->
    ?MC_WITH_MECKS(
        [
            {micro_community_access_repo, [
                {eligible_tx, 3, fun(_, _, _) -> throw({abort_tx, lock_failed}) end}
            ]}
        ],
        fun() ->
            ?assertThrow(
                {abort_tx, lock_failed}, micro_community_access_ds:eligible_tx(conn, 20, 7)
            )
        end
    ).

revoke_lock_failure_has_no_side_effects_test_() ->
    revoke_mocks(fun() ->
        meck:expect(
            micro_community_access_repo,
            lock_channel_tx,
            fun(_, _) -> throw({abort_tx, lock_failed}) end
        ),
        ?assertThrow(
            {abort_tx, lock_failed}, micro_community_access_ds:revoke_channel_tx(conn, 10, 7)
        ),
        ?assertEqual(0, meck:num_calls(group_member_ds, close_history_generation, '_')),
        ?assertEqual(0, meck:num_calls(micro_community_access_repo, deactivate_tx, '_'))
    end).

revoke_generation_failure_does_not_deactivate_test_() ->
    revoke_mocks(fun() ->
        meck:expect(
            group_member_ds,
            close_history_generation,
            fun(_, _, _, _) -> throw({abort_tx, generation_failed}) end
        ),
        ?assertThrow(
            {abort_tx, generation_failed}, micro_community_access_ds:revoke_channel_tx(conn, 10, 7)
        ),
        ?assertEqual(0, meck:num_calls(micro_community_access_repo, deactivate_tx, '_'))
    end).

revoke_deactivation_failure_propagates_for_caller_rollback_test_() ->
    revoke_mocks(fun() ->
        meck:expect(
            micro_community_access_repo,
            deactivate_tx,
            fun(_, _, _) -> throw({abort_tx, deactivation_failed}) end
        ),
        ?assertThrow(
            {abort_tx, deactivation_failed},
            micro_community_access_ds:revoke_channel_tx(conn, 10, 7)
        )
    end).

unbound_revoke_remains_noop_test_() ->
    revoke_mocks(fun() ->
        meck:expect(micro_community_access_repo, lock_channel_tx, fun(_, _) -> null end),
        ?assertEqual(ok, micro_community_access_ds:revoke_channel_tx(conn, 10, 7)),
        ?assertEqual(0, meck:num_calls(group_member_ds, close_history_generation, '_'))
    end).

retained_entitlement_does_not_remove_member_test_() ->
    revoke_mocks(fun() ->
        meck:expect(micro_community_access_repo, eligible_tx, fun(_, _, _) -> true end),
        ?assertEqual(ok, micro_community_access_ds:revoke_channel_tx(conn, 10, 7)),
        ?assertEqual(0, meck:num_calls(micro_community_access_repo, deactivate_tx, '_'))
    end).

revoke_mocks(Fun) ->
    ?MC_WITH_MECKS(
        [
            {micro_community_access_repo, [
                {lock_channel_tx, 2, fun(conn, 10) -> 20 end},
                {eligible_tx, 3, fun(conn, 20, 7) -> false end},
                {deactivate_tx, 3, fun(conn, 20, 7) -> ok end}
            ]},
            {group_member_ds, [
                {close_history_generation, 4, fun(
                    conn,
                    20,
                    7,
                    <<"channel_entitlement_revoked">>
                ) ->
                    ok
                end}
            ]}
        ],
        Fun
    ).

with_live_policy(Extra, Fun) ->
    ?MC_WITH_MECKS(
        [
            {imboy_profile_preset, [{current, 0, fun() -> community end}]},
            {imboy_feature, [{enabled, 1, fun(channel) -> true end}]}
        ] ++ Extra,
        Fun
    ).

ordinary() -> #{<<"type">> => <<"C2C">>, <<"to">> => 8, <<"payload">> => <<"hello">>}.

binary_key_proplist_uses_group_authorization_test_() ->
    ?MC_WITH_MECKS(
        [{micro_community_access_repo, [{delivery_allowed, 2, fun(20, 7) -> false end}]}], fun() ->
            ?assertNot(
                micro_community_access_ds:delivery_allowed(
                    7,
                    [{<<"type">>, <<"C2G">>}, {<<"to">>, 20}]
                )
            ),
            ?assertEqual(1, meck:num_calls(micro_community_access_repo, delivery_allowed, [20, 7])),
            ?assertNot(
                micro_community_access_ds:delivery_allowed(
                    7,
                    [{<<"type">>, <<"S2C">>}, {<<"payload">>, [{<<"gid">>, 20}]}]
                )
            ),
            ?assert(
                micro_community_access_ds:delivery_allowed(
                    7,
                    [{<<"type">>, <<"C2C">>}, {<<"to">>, 8}]
                )
            ),
            ?assertNot(
                micro_community_access_ds:delivery_allowed(
                    7,
                    [{<<"type">>, <<"C2G">>}, {<<"type">>, <<"C2C">>}]
                )
            ),
            ?assertNot(micro_community_access_ds:delivery_allowed(7, [{type, <<"C2C">>}]))
        end
    ).

direct_s2c_payload_shapes_cannot_bypass_authorization_test_() ->
    ?MC_WITH_MECKS(
        [
            {micro_community_access_repo, [
                {delivery_allowed, 2, fun
                    (20, 7) -> false;
                    (21, 7) -> true
                end}
            ]}
        ],
        fun() ->
            Envelope = #{<<"type">> => <<"S2C">>},
            ?assertNot(
                micro_community_access_ds:delivery_allowed(
                    7,
                    Envelope#{<<"payload">> => [{<<"gid">>, 20}]}
                )
            ),
            ?assert(
                micro_community_access_ds:delivery_allowed(
                    7,
                    Envelope#{<<"payload">> => [{<<"group_id">>, 21}]}
                )
            ),
            ?assertNot(
                micro_community_access_ds:delivery_allowed(
                    7,
                    Envelope#{<<"payload">> => [{<<"gid">>, 20}, {<<"gid">>, 21}]}
                )
            ),
            ?assertNot(
                micro_community_access_ds:delivery_allowed(
                    7,
                    Envelope#{<<"payload">> => [{gid, 21}]}
                )
            ),
            ?assertNot(
                micro_community_access_ds:delivery_allowed(
                    7,
                    Envelope#{<<"payload">> => <<"malformed">>}
                )
            ),
            ?assertNot(micro_community_access_ds:delivery_allowed(7, Envelope)),
            ?assert(
                micro_community_access_ds:delivery_allowed(
                    7,
                    Envelope#{<<"payload">> => [{<<"notify_type">>, <<"ordinary">>}]}
                )
            ),
            ?assertEqual(1, meck:num_calls(micro_community_access_repo, delivery_allowed, [20, 7])),
            ?assertEqual(1, meck:num_calls(micro_community_access_repo, delivery_allowed, [21, 7]))
        end
    ).

%% Real handler -> logic -> DS -> repo; only transport, legacy detail transfer,
%% workspace boundary and the SQL adapter are fixtures. No authorization mock.
discussion_detail_guard_regression_test_() ->
    [
        {Name, detail_guard_case(Guard, Gid, Allowed, Expected, Queries)}
     || {Name, Guard, Gid, Allowed, Expected, Queries} <- [
            {"revoked discussion denies detail", ok, <<"20">>, false, 403, 1},
            {"current discussion member retains detail", ok, <<"20">>, true, 0, 1},
            {"unbound legacy detail remains allowed", ok, <<"20">>, true, 0, 1},
            {"query exception fails closed", ok, <<"20">>, exception, 403, 1},
            {"workspace denial wins first", {error, {403, <<"workspace denied">>}}, <<"20">>, true,
                403, 0},
            {"workspace unavailable wins first", {error, {503, <<"workspace unavailable">>}},
                <<"20">>, true, 503, 0},
            {"invalid group preserves validation", ok, <<"bad">>, true, 400, 0}
        ]
    ].

detail_guard_case(Guard, Gid, Allowed, Expected, Queries) ->
    with_live_policy(detail_guard_mocks(Guard, Gid, Allowed), fun() ->
        ?assertEqual(
            {response, Expected}, group_handler:handle_action(detail, req, #{current_uid => 7})
        ),
        ?assertEqual(Queries, meck:num_calls(elib_pg, one, '_')),
        ?assertEqual(
            case Expected of
                0 -> 1;
                _ -> 0
            end,
            meck:num_calls(group_logic, find_by_id, '_')
        )
    end).

detail_guard_mocks(Guard, Gid, Allowed) ->
    [
        {cowboy_req, [{parse_qs, 1, fun(req) -> [{<<"gid">>, Gid}] end}]},
        {workspace_resolver, [
            {guard_group_gid, 2, fun(7, Gid0) ->
                ?assertEqual(Gid, Gid0),
                Guard
            end}
        ]},
        {elib_response, [
            {error, 3, fun(req, _, Code) -> {response, Code} end},
            {success, 3, fun(req, _, _) -> {response, 0} end}
        ]},
        {imboy_error, [
            {validate_id, 2, fun(req, Id) ->
                case Id of
                    <<"bad">> -> {error, {response, 400}};
                    <<"20">> -> {ok, 20}
                end
            end}
        ]},
        {group_logic, [
            {find_by_id, 2, fun(20, _) -> #{<<"id">> => 20} end},
            {group_transfer, 1, fun(G) -> G end}
        ]},
        {elib_pg, [
            {one, 2, fun(Sql, Params) ->
                ?assertEqual([20, 7], Params),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"gm.status=1">>)),
                case Allowed of
                    exception -> erlang:error(db_unavailable);
                    _ -> {ok, #{<<"allowed">> => Allowed}}
                end
            end}
        ]}
    ].

discussion_detail_logic_revocation_regression_test_() ->
    with_live_policy(
        [
            {elib_pg, [
                {one, 2, fun(_, [20, 7]) ->
                    {ok, #{<<"allowed">> => false}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, {403, _}}, micro_community_access_logic:guard_group_detail(7, 20))
        end
    ).
