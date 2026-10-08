-module(adm_micro_community_tests).
-include_lib("eunit/include/eunit.hrl").
-include("micro_community_test.hrl").

platform_actor_never_becomes_personal_owner_test_() ->
    with_platform(fun() ->
        meck:expect(
            micro_community_binding_repo,
            write_platform_tx,
            fun(conn, 900, 10, {null, 20}) -> ok end
        ),
        ?assertEqual({ok, #{discussion_group_id => 20}}, adm_micro_community_ds:set(900, 10, 20)),
        ?assertEqual(
            1, meck:num_calls(micro_community_binding_repo, platform_group_tx, [conn, 20, 1])
        ),
        ?assertEqual(
            1,
            meck:num_calls(micro_community_binding_repo, write_platform_tx, [
                conn, 900, 10, {null, 20}
            ])
        )
    end).

missing_group_permission_denies_test_() ->
    with_platform(fun() ->
        meck:expect(adm_acl, role_permissions, fun(1) -> [<<"channels:update">>] end),
        ?assertEqual({error, forbidden}, adm_micro_community_ds:set(900, 10, 20)),
        ?assertEqual(0, meck:num_calls(micro_community_binding_repo, platform_channel_tx, '_'))
    end).

revoked_platform_actor_denies_test_() ->
    with_platform(fun() ->
        meck:expect(adm_micro_community_repo, authority_tx, fun(_, _) ->
            throw({rollback, forbidden})
        end),
        ?assertEqual({error, forbidden}, adm_micro_community_ds:get(900, 10))
    end).

same_value_rechecks_without_audit_test_() ->
    with_platform(fun() ->
        meck:expect(micro_community_binding_repo, platform_channel_tx, fun(_, _) -> channel(20) end),
        ?assertEqual({ok, #{discussion_group_id => 20}}, adm_micro_community_ds:set(900, 10, 20)),
        ?assertEqual(1, meck:num_calls(adm_micro_community_repo, authority_tx, '_')),
        ?assertEqual(1, meck:num_calls(micro_community_binding_repo, platform_group_tx, '_')),
        ?assertEqual(0, meck:num_calls(micro_community_binding_repo, write_platform_tx, '_'))
    end).

platform_audit_failure_rolls_back_test_() ->
    with_platform(fun() ->
        meck:expect(
            micro_community_binding_repo,
            write_platform_tx,
            fun(_, _, _, _) -> throw({rollback, binding_audit_failed}) end
        ),
        ?assertEqual({error, binding_audit_failed}, adm_micro_community_ds:set(900, 10, 20))
    end).

nonempty_platform_unbind_denied_test_() ->
    with_platform(fun() ->
        meck:expect(micro_community_binding_repo, platform_channel_tx, fun(_, _) -> channel(20) end),
        meck:expect(
            micro_community_binding_repo,
            pristine_tx,
            fun(_, _, _) -> throw({rollback, group_not_pristine}) end
        ),
        ?assertEqual({error, group_not_pristine}, adm_micro_community_ds:set(900, 10, null))
    end).

phase1_platform_entry_denied_test_() ->
    ?MC_WITH_MECKS(
        [
            {imboy_policy, [{current_profile, 0, fun() -> phase1_delivery end}]},
            {imboy_feature, [{enabled, 1, fun(channel) -> true end}]}
        ],
        fun() ->
            ?assertMatch({error, {403, _}}, adm_micro_community_logic:get(900, 10)),
            ?assertMatch({error, {403, _}}, adm_micro_community_logic:essence(900, 10, 30, true))
        end
    ).

platform_candidate_pagination_test_() ->
    with_platform(fun() ->
        meck:expect(
            micro_community_binding_repo,
            platform_candidates_tx,
            fun(conn, 1, 2, 10) -> #{list => [], total => 11, page => 2, size => 10} end
        ),
        ?assertEqual(
            {ok, #{list => [], total => 11, page => 2, size => 10}},
            adm_micro_community_ds:candidates(900, 10, 2, 10)
        )
    end).

with_platform(Fun) ->
    ?MC_WITH_MECKS(
        [
            {adm_micro_community_repo, [
                {transaction, 1, fun(F) -> F(conn) end},
                {authority_tx, 2, fun(conn, 900) -> [1] end}
            ]},
            {adm_acl, [
                {normalize_role_ids, 1, fun(Roles) -> Roles end},
                {role_permissions, 1, fun(1) -> [<<"channels:update">>, <<"groups:update">>] end}
            ]},
            {micro_community_binding_repo, [
                {lock_tables_tx, 1, fun(conn) -> ok end},
                {platform_channel_tx, 2, fun(conn, 10) -> channel(null) end},
                {platform_group_tx, 3, fun(conn, 20, 1) -> #{} end},
                {pristine_tx, 3, fun(_, _, _) -> ok end}
            ]}
        ],
        Fun
    ).

channel(Gid) -> #{<<"creator_uid">> => 1, <<"discussion_group_id">> => Gid}.
