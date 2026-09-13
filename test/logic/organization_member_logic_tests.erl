-module(organization_member_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 101).
-define(OWNER, 201).
-define(ADMIN, 202).
-define(MEMBER, 203).

admin_can_invite_and_remove_member_test_() ->
    ?WITH_MECKS(
        common_mocks(?ADMIN, <<"admin">>) ++
            [
                {user_repo, [
                    {'find_by_id', 2, fun(?MEMBER, <<"id">>) -> #{<<"id">> => ?MEMBER} end}
                ]},
                {user_denylist_logic, [
                    {'blocked_between', 2, fun(?ADMIN, ?MEMBER) -> false end}
                ]}
            ],
        fun() ->
            ?assertMatch(
                {ok, changed, #{<<"role">> := <<"member">>}},
                organization_member_logic:invite(?ADMIN, ?ORG_ID, ?MEMBER, <<"member">>)
            ),
            ?assertMatch(
                {ok, #{status := <<"removed">>}},
                organization_member_logic:remove(?ADMIN, ?ORG_ID, ?MEMBER)
            )
        end
    ).

admin_cannot_manage_admin_role_test_() ->
    ?WITH_MECKS(
        common_mocks(?ADMIN, <<"admin">>) ++
            [
                {user_repo, [
                    {'find_by_id', 2, fun(?MEMBER, <<"id">>) -> #{<<"id">> => ?MEMBER} end}
                ]},
                {user_denylist_logic, [
                    {'blocked_between', 2, fun(?ADMIN, ?MEMBER) -> false end}
                ]}
            ],
        fun() ->
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:invite(?ADMIN, ?ORG_ID, ?MEMBER, <<"admin">>)
            ),
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:change_role(
                    ?ADMIN, ?ORG_ID, ?MEMBER, <<"admin">>
                )
            ),
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:remove(?ADMIN, ?ORG_ID, ?ADMIN)
            )
        end
    ).

primary_owner_can_promote_and_remove_admin_test_() ->
    ?WITH_MECKS(
        common_mocks(?OWNER, <<"owner">>),
        fun() ->
            ?assertMatch(
                {ok, changed, #{role := <<"admin">>}},
                organization_member_logic:change_role(
                    ?OWNER, ?ORG_ID, ?MEMBER, <<"admin">>
                )
            ),
            ?assertMatch(
                {ok, #{status := <<"removed">>}},
                organization_member_logic:remove(?OWNER, ?ORG_ID, ?ADMIN)
            )
        end
    ).

primary_owner_is_protected_test_() ->
    ?WITH_MECKS(
        common_mocks(?OWNER, <<"owner">>),
        fun() ->
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:remove(?OWNER, ?ORG_ID, ?OWNER)
            ),
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:change_role(
                    ?OWNER, ?ORG_ID, ?OWNER, <<"member">>
                )
            )
        end
    ).

ordinary_member_cannot_list_test_() ->
    ?WITH_MECKS(
        [
            {organization_member_repo, [
                {'find_active', 3, fun(?ORG_ID, ?MEMBER, <<"role">>) ->
                    {ok, #{<<"role">> => <<"member">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {403, _}}, organization_member_logic:list(?MEMBER, ?ORG_ID, 1, 10)
            )
        end
    ).

primary_owner_can_transfer_to_active_member_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun run_tx/1}
            ]},
            {organization_repo, [
                {'find_for_update_tx', 2, fun(fake_conn, ?ORG_ID) ->
                    {ok, #{
                        <<"id">> => ?ORG_ID,
                        <<"owner_id">> => ?OWNER,
                        <<"status">> => <<"active">>
                    }}
                end},
                {'update_owner_tx', 3, fun(fake_conn, ?ORG_ID, ?MEMBER) ->
                    put(t_org_owner_updated, true),
                    {ok, #{<<"id">> => ?ORG_ID, <<"owner_id">> => ?MEMBER}}
                end}
            ]},
            {organization_member_repo, [
                {'find_for_update_tx', 4, fun
                    (fake_conn, ?ORG_ID, ?OWNER, <<"role,status">>) ->
                        {ok, #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}};
                    (fake_conn, ?ORG_ID, ?MEMBER, <<"role,status">>) ->
                        {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}}
                end},
                {'update_role_tx', 4, fun(fake_conn, ?ORG_ID, ?OWNER, <<"admin">>) ->
                    put(t_previous_owner_demoted, true),
                    ok
                end},
                {'find_active_tx', 4, fun(fake_conn, ?ORG_ID, ?MEMBER, <<"role">>) ->
                    {ok, #{<<"role">> => <<"owner">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{
                    organization_id := ?ORG_ID,
                    owner_id := ?MEMBER,
                    previous_owner_id := ?OWNER,
                    previous_owner_role := <<"admin">>
                }},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(true, erase(t_org_owner_updated)),
            ?assertEqual(true, erase(t_previous_owner_demoted))
        end
    ).

non_owner_cannot_transfer_owner_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun run_tx/1}
            ]},
            {organization_repo, [
                {'find_for_update_tx', 2, fun(fake_conn, ?ORG_ID) ->
                    {ok, #{
                        <<"id">> => ?ORG_ID,
                        <<"owner_id">> => ?OWNER,
                        <<"status">> => <<"active">>
                    }}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:transfer_owner(?ADMIN, ?ORG_ID, ?MEMBER)
            )
        end
    ).

transfer_owner_validation_and_sync_failures_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun run_tx/1}
            ]},
            {organization_repo, [
                {'find_for_update_tx', 2, fun(fake_conn, ?ORG_ID) ->
                    Status =
                        case get(t_owner_transfer_case) of
                            archived -> <<"archived">>;
                            _ -> <<"active">>
                        end,
                    {ok, #{
                        <<"id">> => ?ORG_ID,
                        <<"owner_id">> => ?OWNER,
                        <<"status">> => Status
                    }}
                end},
                {'update_owner_tx', 3, fun(fake_conn, ?ORG_ID, ?MEMBER) ->
                    put(t_owner_transfer_updated, true),
                    {ok, #{<<"id">> => ?ORG_ID, <<"owner_id">> => ?MEMBER}}
                end}
            ]},
            {organization_member_repo, [
                {'find_for_update_tx', 4, fun
                    (fake_conn, ?ORG_ID, ?OWNER, <<"role,status">>) ->
                        {ok, #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}};
                    (fake_conn, ?ORG_ID, ?MEMBER, <<"role,status">>) ->
                        case get(t_owner_transfer_case) of
                            inactive_target ->
                                {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"removed">>}};
                            _ ->
                                {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}}
                        end
                end},
                {'update_role_tx', 4, fun(fake_conn, ?ORG_ID, ?OWNER, <<"admin">>) -> ok end},
                {'find_active_tx', 4, fun(fake_conn, ?ORG_ID, ?MEMBER, <<"role">>) ->
                    {ok, #{<<"role">> => <<"member">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {400, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?OWNER)
            ),
            put(t_owner_transfer_case, archived),
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(undefined, erase(t_owner_transfer_updated)),
            put(t_owner_transfer_case, inactive_target),
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(undefined, erase(t_owner_transfer_updated)),
            put(t_owner_transfer_case, sync_failure),
            ?assertMatch(
                {error, {500, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(true, erase(t_owner_transfer_updated)),
            erase(t_owner_transfer_case),
            ok
        end
    ).

common_mocks(ActorUid, ActorRole) ->
    [
        {elib_pg, [
            {'with_tx', 1, fun run_tx/1}
        ]},
        {organization_member_repo, [
            {'find_organization_for_share_tx', 3, fun(_, ?ORG_ID, <<"id,owner_id,status">>) ->
                {ok, #{
                    <<"id">> => ?ORG_ID,
                    <<"owner_id">> => ?OWNER,
                    <<"status">> => <<"active">>
                }}
            end},
            {'find_active_for_share_tx', 4, fun(_, ?ORG_ID, ActualUid, <<"role">>) ->
                ?assertEqual(ActorUid, ActualUid),
                {ok, #{<<"role">> => ActorRole}}
            end},
            {'upsert_active_tx', 5, fun(_, ?ORG_ID, ?MEMBER, Role, InvitedBy) ->
                ?assertEqual(ActorUid, InvitedBy),
                {ok, changed, #{<<"role">> => Role}}
            end},
            {'find_active_tx', 4, fun(_, ?ORG_ID, ?MEMBER, _) ->
                {ok, #{
                    <<"organization_id">> => ?ORG_ID,
                    <<"user_id">> => ?MEMBER,
                    <<"role">> => <<"member">>,
                    <<"status">> => <<"active">>
                }}
            end},
            {'find_for_update_tx', 4, fun(_, ?ORG_ID, TargetUid, <<"role,status">>) ->
                case TargetUid of
                    ?OWNER -> {ok, #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}};
                    ?ADMIN -> {ok, #{<<"role">> => <<"admin">>, <<"status">> => <<"active">>}};
                    ?MEMBER -> {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}}
                end
            end},
            {'update_role_tx', 4, fun(_, ?ORG_ID, ?MEMBER, _Role) -> ok end},
            {'remove_tx', 3, fun(_, ?ORG_ID, TargetUid) when
                TargetUid =:= ?MEMBER; TargetUid =:= ?ADMIN
            ->
                ok
            end}
        ]}
    ].

run_tx(Tx) ->
    try Tx(fake_conn) of
        Result -> Result
    catch
        throw:{abort_tx, Reason} -> {error, Reason}
    end.
