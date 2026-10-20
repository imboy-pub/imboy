-module(enterprise_directory_source_read_logic_tests).
-include_lib("eunit/include/eunit.hrl").

query_contract_test() ->
    ?assertMatch(
        {ok, #{page_size := 50}},
        enterprise_directory_source_read_logic:validate(organizations, #{})
    ),
    ?assertMatch(
        {error, invalid_request},
        enterprise_directory_source_read_logic:validate(persons, #{})
    ),
    lists:foreach(
        fun(Query) ->
            ?assertMatch(
                {error, invalid_request},
                enterprise_directory_source_read_logic:validate(persons, Query)
            )
        end,
        [
            #{<<"external_organization_id">> => <<"o">>, <<"page_size">> => <<"101">>},
            #{<<"external_organization_id">> => <<"o">>, <<"user_id">> => <<"1">>},
            #{<<"external_organization_id">> => <<"o">>, <<"projection_status">> => <<"bad">>}
        ]
    ).

cursor_binding_test() ->
    Modules = [config_ds, enterprise_directory_source_read_repo],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        Key = <<0:256>>,
        meck:expect(config_ds, env, fun(imboy, enterprise_internal_cursor_signing_key, _) -> Key end),
        meck:expect(
            enterprise_directory_source_read_repo,
            organization_tx,
            fun(_, _, _) -> {ok, #{<<"organization_id">> => 100}} end
        ),
        meck:expect(
            enterprise_directory_source_read_repo,
            page_tx,
            fun
                (_, persons, _, _, null, 2) ->
                    {ok, [person(<<"a">>), person(<<"b">>)]};
                (_, persons, _, _, <<"a">>, 2) ->
                    {ok, [person(<<"b">>)]}
            end
        ),
        Ctx = #{application_id => 20, credential_id => 30},
        Query = #{<<"external_organization_id">> => <<"org/a">>, <<"page_size">> => <<"1">>},
        {ok, Page} = enterprise_directory_source_read_logic:page_tx(conn, Ctx, persons, Query),
        ?assertEqual(true, maps:get(<<"has_more">>, Page)),
        Cursor = maps:get(<<"next_cursor">>, Page),
        Next = Query#{<<"cursor">> => Cursor},
        ?assertMatch(
            {ok, #{<<"has_more">> := false}},
            enterprise_directory_source_read_logic:page_tx(conn, Ctx, persons, Next)
        ),
        ?assertEqual(
            {error, invalid_request},
            enterprise_directory_source_read_logic:page_tx(
                conn, Ctx#{application_id => 21}, persons, Next
            )
        ),
        ?assertEqual(
            {error, invalid_request},
            enterprise_directory_source_read_logic:page_tx(
                conn,
                Ctx,
                persons,
                Next#{<<"external_organization_id">> => <<"other">>}
            )
        ),
        ?assertEqual(
            {error, invalid_request},
            enterprise_directory_source_read_logic:page_tx(conn, Ctx, departments, Next)
        ),
        ?assertEqual(
            {error, invalid_request},
            enterprise_directory_source_read_logic:page_tx(
                conn,
                Ctx,
                persons,
                Next#{<<"projection_status">> => <<"pending">>}
            )
        ),
        meck:expect(
            enterprise_directory_source_read_repo,
            organization_tx,
            fun(_, _, _) -> {error, resource_not_found} end
        ),
        ?assertEqual(
            {error, resource_not_found},
            enterprise_directory_source_read_logic:page_tx(conn, Ctx, persons, Next)
        ),
        meck:expect(config_ds, env, fun(_, _, _) -> undefined end),
        ?assertEqual(
            {error, security_gate_closed},
            enterprise_directory_source_read_logic:page_tx(conn, Ctx, persons, Query)
        )
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

person(Id) -> #{<<"external_user_id">> => Id, <<"department_external_ids">> => <<"[]">>}.
