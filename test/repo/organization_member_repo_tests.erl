-module(organization_member_repo_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 101).
-define(UID, 202).

find_active_tx_uses_scoped_membership_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"organization_member">>)),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"status = 'active'">>)),
                    ?assertEqual([101, 202], Params),
                    {ok, [#{<<"role">> => <<"admin">>}]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{<<"role">> => <<"admin">>}},
                organization_member_repo:find_active_tx(fake_conn, 101, 202, <<"role">>)
            )
        end
    ).

find_active_tx_preserves_not_found_and_db_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun
                    (_Conn, _Sql, [101, 404]) -> {ok, []};
                    (_Conn, _Sql, [101, 503]) -> {error, unavailable}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, not_found},
                organization_member_repo:find_active_tx(fake_conn, 101, 404, <<"role">>)
            ),
            ?assertEqual(
                {error, unavailable},
                organization_member_repo:find_active_tx(fake_conn, 101, 503, <<"role">>)
            )
        end
    ).

migration_keeps_workspace_membership_independent_test() ->
    {ok, Up} = file:read_file("priv/migrations/00000113_organization_member.up.sql"),
    ?assertNotEqual(
        nomatch, binary:match(Up, <<"CREATE TABLE IF NOT EXISTS organization_member">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Up, <<"ON CONFLICT (organization_id, user_id)">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"trg_organization_owner_member_sync">>)),
    ?assertNotEqual(
        nomatch, binary:match(Up, <<"trg_organization_primary_owner_member_guard">>)
    ),
    ?assertEqual(nomatch, binary:match(Up, <<"REFERENCES workspace_member">>)),
    ?assertEqual(nomatch, binary:match(Up, <<"REFERENCES workspace(">>)).

write_membership_lookup_takes_share_lock_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, _Params) ->
                    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Sql), <<"FOR SHARE">>)),
                    {ok, [#{<<"role">> => <<"owner">>}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                organization_member_repo:find_active_for_share_tx(
                    fake_conn, 101, 202, <<"role">>
                )
            )
        end
    ).

upsert_active_is_organization_only_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, [?ORG_ID, ?UID]) ->
                    ?assertEqual(nomatch, binary:match(Sql, <<"workspace_member">>)),
                    {ok, []}
                end},
                {'execute', 3, fun(_Conn, Sql, Params) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"organization_member">>)),
                    ?assertEqual(nomatch, binary:match(Sql, <<"workspace_member">>)),
                    ?assertEqual([?ORG_ID, ?UID, <<"member">>, 303], lists:sublist(Params, 4)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, changed, _},
                organization_member_repo:upsert_active_tx(
                    fake_conn, ?ORG_ID, ?UID, <<"member">>, 303
                )
            )
        end
    ).

page_propagates_count_failure_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, [?ORG_ID]) -> {error, unavailable} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, unavailable},
                organization_member_repo:page_by_organization(
                    ?ORG_ID, 1, 10, <<"om.user_id">>
                )
            )
        end
    ).
