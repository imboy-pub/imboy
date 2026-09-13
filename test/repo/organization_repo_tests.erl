-module(organization_repo_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

create_tx_uses_database_defaults_and_returns_row_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(organization) -> 101 end}
            ]},
            {elib_pg, [
                {'query', 3, fun(fake_conn, Sql, Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"INSERT INTO">>)),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"CURRENT_TIMESTAMP">>)),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"RETURNING id,name">>)),
                    ?assertEqual([101, <<"Acme">>, 201], Params),
                    {ok, [#{<<"id">> => 101, <<"name">> => <<"Acme">>}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{<<"id">> := 101}},
                organization_repo:create_tx(fake_conn, 201, <<"Acme">>)
            )
        end
    ).

page_by_member_is_independent_of_workspace_membership_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, [201]) ->
                    ?assertEqual(nomatch, binary:match(Sql, <<"workspace_member">>)),
                    {ok, #{<<"count">> => 1}}
                end},
                {'query', 2, fun(Sql, [201, 10, 0]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"organization_member">>)),
                    ?assertEqual(nomatch, binary:match(Sql, <<"workspace_member">>)),
                    {ok, [#{<<"id">> => 101, <<"member_role">> => <<"member">>}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{total := 1, total_page := 1, list := [_]}},
                organization_repo:page_by_member(201, 1, 10)
            )
        end
    ).

update_tx_uses_parameterized_shallow_json_merge_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(fake_conn, Sql, Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"branding || $2::jsonb">>)),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"settings || $3::jsonb">>)),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"CURRENT_TIMESTAMP">>)),
                    ?assertEqual([<<"Renamed">>, <<"{\"logo\":\"x\"}">>, null, 101], Params),
                    {ok, [#{<<"id">> => 101, <<"name">> => <<"Renamed">>}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{<<"name">> := <<"Renamed">>}},
                organization_repo:update_tx(
                    fake_conn, 101, <<"Renamed">>, <<"{\"logo\":\"x\"}">>, null
                )
            )
        end
    ).

update_owner_tx_updates_anchor_and_returns_row_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(fake_conn, Sql, Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"SET owner_id = $1">>)),
                    ?assertNotEqual(nomatch, binary:match(SqlBin, <<"CURRENT_TIMESTAMP">>)),
                    ?assertEqual([202, 101], Params),
                    {ok, [#{<<"id">> => 101, <<"owner_id">> => 202}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{<<"owner_id">> := 202}},
                organization_repo:update_owner_tx(fake_conn, 101, 202)
            )
        end
    ).
