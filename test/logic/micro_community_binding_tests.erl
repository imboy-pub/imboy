-module(micro_community_binding_tests).
-include_lib("eunit/include/eunit.hrl").
-include("micro_community_test.hrl").

set_validates_before_write_test_() ->
    repo_test(fun() ->
        meck:expect(micro_community_binding_repo, write_tx, fun(conn, 1, 10, {null, 20}) -> ok end),
        ?assertEqual({ok, #{discussion_group_id => 20}}, micro_community_binding_ds:set(1, 10, 20)),
        ?assertEqual(1, meck:num_calls(micro_community_binding_repo, pristine_tx, '_')),
        ?assertEqual(1, meck:num_calls(micro_community_binding_repo, write_tx, '_'))
    end).

same_binding_revalidates_without_audit_test_() ->
    repo_test(fun() ->
        meck:expect(micro_community_binding_repo, channel_tx, fun(_, _, _) -> channel(20) end),
        ?assertEqual({ok, #{discussion_group_id => 20}}, micro_community_binding_ds:set(1, 10, 20)),
        ?assertEqual(1, meck:num_calls(micro_community_binding_repo, group_tx, '_')),
        ?assertEqual(0, meck:num_calls(micro_community_binding_repo, write_tx, '_')),
        ?assertEqual(0, meck:num_calls(micro_community_binding_repo, pristine_tx, '_'))
    end).

same_binding_rejects_lost_permission_test_() ->
    repo_test(fun() ->
        meck:expect(micro_community_binding_repo, channel_tx, fun(_, _, _) -> channel(20) end),
        meck:expect(
            micro_community_binding_repo,
            group_tx,
            fun(_, _, _, _) -> throw({rollback, invalid_group}) end
        ),
        ?assertEqual({error, invalid_group}, micro_community_binding_ds:set(1, 10, 20)),
        ?assertEqual(0, meck:num_calls(micro_community_binding_repo, write_tx, '_'))
    end).

occupied_unbind_preserves_binding_test_() ->
    repo_test(fun() ->
        meck:expect(micro_community_binding_repo, channel_tx, fun(_, _, _) -> channel(20) end),
        meck:expect(
            micro_community_binding_repo,
            pristine_tx,
            fun(_, _, _) -> throw({rollback, group_not_pristine}) end
        ),
        ?assertEqual({error, group_not_pristine}, micro_community_binding_ds:set(1, 10, null)),
        ?assertEqual(0, meck:num_calls(micro_community_binding_repo, write_tx, '_'))
    end).

empty_unbind_writes_once_test_() ->
    repo_test(fun() ->
        meck:expect(micro_community_binding_repo, channel_tx, fun(_, _, _) -> channel(20) end),
        meck:expect(micro_community_binding_repo, write_tx, fun(conn, 1, 10, {20, null}) -> ok end),
        ?assertEqual(
            {ok, #{discussion_group_id => null}}, micro_community_binding_ds:set(1, 10, null)
        )
    end).

query_failure_denies_test_() ->
    repo_test(fun() ->
        meck:expect(
            micro_community_binding_repo,
            read_tx,
            fun(_, _, _) -> throw({rollback, binding_query_failed}) end
        ),
        ?assertEqual({error, binding_query_failed}, micro_community_binding_ds:get(1, 10))
    end).

workspace_channel_denied_test_() ->
    repo_test(fun() ->
        meck:expect(
            micro_community_binding_repo,
            channel_tx,
            fun(_, _, _) -> (channel(null))#{<<"scope">> => <<"workspace">>} end
        ),
        ?assertEqual({error, forbidden}, micro_community_binding_ds:set(1, 10, 20))
    end).

phase1_delivery_denies_entry_test_() ->
    ?MC_WITH_MECKS(
        [
            {imboy_policy, [{current_profile, 0, fun() -> phase1_delivery end}]},
            {imboy_feature, [{enabled, 1, fun(channel) -> true end}]}
        ],
        fun() ->
            ?assertMatch({error, {403, _}}, micro_community_binding_logic:get(1, 10)),
            ?assertMatch({error, {403, _}}, micro_community_binding_logic:set(1, 10, 20)),
            ?assertMatch({error, {403, _}}, micro_community_binding_logic:candidates(1, 10, 1, 10))
        end
    ).

pristine_covers_persistent_content_test() ->
    Sql = micro_community_binding_repo:pristine_sql(),
    [
        ?assertNotEqual(nomatch, binary:match(Sql, Name))
     || Name <-
            [
                <<"msg_c2g">>,
                <<"msg_store_staging">>,
                <<"msg_store">>,
                <<"group_notice">>,
                <<"group_file">>,
                <<"group_album_photo">>,
                <<"group_task">>,
                <<"group_schedule">>,
                <<"group_vote">>,
                <<"attachment">>
            ]
    ].

ordinary_subscriber_get_test_() ->
    repo_test(fun() ->
        meck:expect(
            micro_community_binding_repo,
            read_tx,
            fun(conn, 2, 10) -> {ok, #{discussion_group_id => 20}} end
        ),
        ?assertEqual({ok, #{discussion_group_id => 20}}, micro_community_binding_ds:get(2, 10))
    end).

paid_permanent_order_read_sql_test() ->
    Sql = micro_community_binding_repo:read_sql(),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"co.subscription_end_at IS NULL OR">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"cs.status=1">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"co.status=1">>)).

group_constraints_reject_test_() ->
    Valid = #{
        <<"manages">> => true,
        <<"owner_uid">> => 1,
        <<"scope">> => <<"personal">>,
        <<"type">> => 2,
        <<"join_limit">> => 3,
        <<"status">> => 1,
        <<"e2ee_mode">> => 0
    },
    [
        {
            atom_to_list(Label),
            ?MC_WITH_MECKS(
                [{elib_pg, [{query, 3, fun(_, _, _) -> {ok, [Valid#{Key => Value}]} end}]}],
                fun() ->
                    ?assertThrow(
                        {rollback, invalid_group},
                        micro_community_binding_repo:group_tx(conn, 1, 20, 1)
                    )
                end
            )
        }
     || {Label, Key, Value} <- [
            {permission, <<"manages">>, false},
            {owner, <<"owner_uid">>, 2},
            {workspace, <<"scope">>, <<"workspace">>},
            {public, <<"type">>, 1},
            {join_policy, <<"join_limit">>, 1},
            {disabled, <<"status">>, 0},
            {encrypted, <<"e2ee_mode">>, 1}
        ]
    ].

audit_failure_propagates_test_() ->
    repo_test(fun() ->
        meck:expect(
            micro_community_binding_repo,
            write_tx,
            fun(_, _, _, _) -> throw({rollback, binding_audit_failed}) end
        ),
        ?assertEqual({error, binding_audit_failed}, micro_community_binding_ds:set(1, 10, 20))
    end).

creator_with_role_one_cannot_manage_test_() ->
    repo_test(fun() ->
        meck:expect(
            micro_community_binding_repo,
            channel_tx,
            fun(_, _, _) -> (channel(null))#{<<"manages">> => false} end
        ),
        ?assertEqual({error, forbidden}, micro_community_binding_ds:set(1, 10, 20)),
        ?assertEqual(0, meck:num_calls(micro_community_binding_repo, group_tx, '_'))
    end).

channel_role_precedence_sql_test() ->
    Sql = micro_community_binding_repo:channel_manager_sql(),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"ca.role>0">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"COALESCE((SELECT ca.role">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"CASE WHEN c.creator_uid=$2">>)).

table_lock_failure_propagates_test_() ->
    repo_test(fun() ->
        meck:expect(
            micro_community_binding_repo,
            lock_tables_tx,
            fun(_) -> throw({rollback, {binding_lock_failed, deadlock_detected}}) end
        ),
        ?assertEqual(
            {error, {binding_lock_failed, deadlock_detected}},
            micro_community_binding_ds:set(1, 10, 20)
        ),
        ?assertEqual(0, meck:num_calls(micro_community_binding_repo, channel_tx, '_'))
    end).

repo_test(Fun) ->
    ?MC_WITH_MECKS(
        [
            {micro_community_binding_repo, [
                {transaction, 1, fun(F) -> F(conn) end},
                {lock_tables_tx, 1, fun(conn) -> ok end},
                {channel_tx, 3, fun(_, _, _) -> channel(null) end},
                {group_tx, 4, fun(_, _, _, _) -> #{} end},
                {pristine_tx, 3, fun(_, _, _) -> ok end}
            ]}
        ],
        Fun
    ).

channel(Gid) ->
    #{
        <<"status">> => 1,
        <<"scope">> => <<"personal">>,
        <<"manages">> => true,
        <<"creator_uid">> => 1,
        <<"discussion_group_id">> => Gid
    }.
