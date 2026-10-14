-module(eb_contact_transaction_tests).
-include_lib("eunit/include/eunit.hrl").
-export([subject_hmac/4, seal_scoped/3, ensure_contact_in/5, append_in/3]).

same_connection_and_failure_propagation_test() ->
    pure_mock_fixture:run(fun same_connection_and_failure_propagation/0).

same_connection_and_failure_propagation() ->
    Params = #{
        workspace_id => 2,
        channel => <<"other">>,
        subject => <<"visitor">>,
        crypto => ?MODULE,
        store => ?MODULE,
        audit => ?MODULE
    },
    try
        put(store_result, success),
        put(audit_result, {ok, 99}),
        {ok, #{reused := false, audit_id := 99}} =
            enterprise_business_facade:ensure_contact_in(test_conn, 1, Params),
        ?assertEqual(test_conn, get(store_conn)),
        ?assertEqual(test_conn, get(audit_conn)),
        put(audit_result, {error, audit_down}),
        ?assertEqual(
            {error, {audit_append_failed, audit_down}},
            enterprise_business_facade:ensure_contact_in(test_conn, 1, Params)
        ),
        put(store_result, {error, identity_failed}),
        erase(audit_conn),
        ?assertEqual(
            {error, identity_failed},
            enterprise_business_facade:ensure_contact_in(test_conn, 1, Params)
        ),
        ?assertEqual(undefined, get(audit_conn))
    after
        [erase(K) || K <- [store_result, audit_result, store_conn, audit_conn]]
    end.

reused_contact_does_not_append_create_audit_test() ->
    pure_mock_fixture:run(fun() ->
        Params = #{
            workspace_id => 2,
            channel => <<"other">>,
            subject => <<"visitor">>,
            crypto => ?MODULE,
            store => ?MODULE,
            audit => ?MODULE
        },
        try
            put(store_result, reused),
            put(audit_result, {error, audit_must_not_be_called}),
            erase(audit_conn),
            {ok, Result} = enterprise_business_facade:ensure_contact_in(test_conn, 1, Params),
            ?assertEqual(true, maps:get(reused, Result)),
            ?assert(maps:is_key(contact, Result)),
            ?assert(maps:is_key(contact_identity, Result)),
            ?assertNot(maps:is_key(audit_id, Result)),
            ?assertEqual(undefined, get(audit_conn))
        after
            [erase(K) || K <- [store_result, audit_result, store_conn, audit_conn]]
        end
    end).

conflict_is_verified_on_same_connection_test() ->
    pure_mock_fixture:run(fun conflict_is_verified_on_same_connection/0).

conflict_is_verified_on_same_connection() ->
    meck:new(elib_pg, [passthrough, no_link]),
    Identity = #{id => 7, contact_id => 5, channel => <<"other">>, subject_hmac => <<"hmac">>},
    ContactRow = #{
        <<"id">> => 5,
        <<"organization_id">> => 1,
        <<"workspace_id">> => 2,
        <<"status">> => <<"active">>
    },
    IdentityRow = #{
        <<"id">> => 7,
        <<"organization_id">> => 1,
        <<"workspace_id">> => 2,
        <<"contact_id">> => 5,
        <<"channel">> => <<"other">>,
        <<"subject_hmac">> => <<"hmac">>
    },
    try
        meck:expect(elib_pg, execute, fun(test_conn, Sql, _) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"ON CONFLICT DO NOTHING">>)),
            {ok, 0, []}
        end),
        meck:expect(elib_pg, query, fun
            (test_conn, _, [1, 2, 5]) -> {ok, [ContactRow]};
            (test_conn, _, [1, 2, 7]) -> {ok, [IdentityRow]}
        end),
        ?assertMatch(
            {ok, #{reused := true}},
            eb_pg_store:ensure_contact_in(test_conn, 1, 2, #{id => 5}, Identity)
        ),
        lists:foreach(
            fun({Key, Value}) ->
                Bad = IdentityRow#{Key => Value},
                meck:expect(elib_pg, query, fun
                    (test_conn, _, [1, 2, 5]) -> {ok, [ContactRow]};
                    (test_conn, _, [1, 2, 7]) -> {ok, [Bad]}
                end),
                ?assertEqual(
                    {error, contact_identity_conflict},
                    eb_pg_store:ensure_contact_in(test_conn, 1, 2, #{id => 5}, Identity)
                )
            end,
            [
                {<<"organization_id">>, 3},
                {<<"workspace_id">>, 3},
                {<<"contact_id">>, 8},
                {<<"channel">>, <<"email">>},
                {<<"subject_hmac">>, <<"different">>}
            ]
        ),
        meck:expect(elib_pg, query, fun
            (test_conn, _, [1, 2, 5]) -> {ok, [ContactRow]};
            (test_conn, _, [1, 2, 7]) -> {ok, []}
        end),
        ?assertMatch(
            {error, _},
            eb_pg_store:ensure_contact_in(test_conn, 1, 2, #{id => 5}, Identity)
        )
    after
        meck:unload(elib_pg)
    end.

identity_insert_failure_is_not_success_test() ->
    pure_mock_fixture:run(fun identity_insert_failure_is_not_success/0).

identity_insert_failure_is_not_success() ->
    meck:new(elib_pg, [passthrough, no_link]),
    try
        meck:expect(elib_pg, execute, fun(test_conn, Sql, _) ->
            case binary:match(Sql, <<"INSERT INTO enterprise_contact_identity">>) of
                nomatch -> {ok, 1, [#{<<"id">> => 5}]};
                _ -> {error, identity_write_failed}
            end
        end),
        Identity = #{
            id => 7,
            contact_id => 5,
            channel => <<"other">>,
            subject_hmac => <<"hmac">>
        },
        ?assertMatch(
            {error, {contact_identity_not_created, _}},
            eb_pg_store:ensure_contact_in(test_conn, 1, 2, #{id => 5}, Identity)
        ),
        ?assertEqual(0, meck:num_calls(elib_pg, query, '_'))
    after
        meck:unload(elib_pg)
    end.

subject_hmac(_, _, _, _) -> {ok, binary:encode_hex(<<1:256>>)}.
seal_scoped(_, _, _) -> {error, unused}.
ensure_contact_in(Conn, _, _, Contact, Identity) ->
    put(store_conn, Conn),
    case get(store_result) of
        success -> {ok, #{contact => Contact, contact_identity => Identity, reused => false}};
        reused -> {ok, #{contact => Contact, contact_identity => Identity, reused => true}};
        Error -> Error
    end.
append_in(Conn, _, _) ->
    put(audit_conn, Conn),
    get(audit_result).
