-module(olm_shared_group_authorization_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

%% Only an empty, controller-owned marker DB is allowed; no runtime/config fallback.
-spec run(map()) -> ok.
run(ConnectOpts) when is_map(ConnectOpts) ->
    Required = [database, host, password, port, username],
    ?assertEqual(Required, lists:sort(maps:keys(maps:with(Required, ConnectOpts)))),
    ExpectedDb = unicode:characters_to_binary(maps:get(database, ConnectOpts)),
    ?assertEqual(
        match,
        re:run(
            ExpectedDb,
            <<"\\Aimboy_olm_authorization_[a-z0-9_]+\\z">>,
            [{capture, none}]
        )
    ),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, Conn} = epgsql:connect(ConnectOpts),
    try
        run_conn(Conn, ExpectedDb)
    after
        epgsql:close(Conn)
    end.

run_conn(Conn, ExpectedDb) ->
    ?assertEqual(
        {ok, [#{<<"db">> => ExpectedDb, <<"empty">> => true}]},
        elib_pg:query(
            Conn,
            <<
                "SELECT current_database() AS db, "
                "to_regclass('public.group_member') IS NULL AS empty"
            >>,
            []
        )
    ),
    {ok, []} = elib_pg:query(Conn, <<"BEGIN">>, []),
    try
        create_fixture(Conn),
        assert_authorization(Conn)
    after
        {ok, []} = elib_pg:query(Conn, <<"ROLLBACK">>, []),
        ?assertEqual(
            {ok, [#{<<"absent">> => true}]},
            elib_pg:query(
                Conn,
                <<"SELECT to_regclass('public.group_member') IS NULL AS absent">>,
                []
            )
        )
    end.

create_fixture(Conn) ->
    %% Types/status domain: foundation migration; only status=1 is active.
    {ok, []} = elib_pg:query(
        Conn,
        <<
            "CREATE TABLE public.group_member (group_id bigint NOT NULL, "
            "user_id bigint NOT NULL, status smallint NOT NULL "
            "CHECK(status IN (-1,0,1,2)))"
        >>,
        []
    ),
    {ok, []} = elib_pg:query(
        Conn,
        <<
            "INSERT INTO public.group_member(group_id,user_id,status) VALUES "
            "(1,100,1),(1,200,1),(2,201,1),(1,202,0),(1,203,2),"
            "(1,204,-1),(1,205,0),(1,206,2),(1,207,-1)"
        >>,
        []
    ),
    ok.

assert_authorization(Conn) ->
    Modules = [friend_ds, olm_identity_ds, elib_pg, elib_metric],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(friend_ds, is_friend, fun(_, _) -> false end),
        meck:expect(elib_metric, increment, fun(_) -> ok end),
        %% Authorization success stops before real identity/prekey access.
        meck:expect(olm_identity_ds, find_identity, fun(_, _) -> {ok, not_found} end),
        meck:expect(elib_pg, query, fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end),
        Cases = [
            {100, 200, true},
            {100, 201, false},
            {100, 202, false},
            {100, 203, false},
            {100, 204, false},
            {205, 200, false},
            {206, 200, false},
            {207, 200, false}
        ],
        [
            assert_claim(A, B, Allowed, Arity)
         || {A, B, Allowed} <- Cases, Arity <- [3, 4]
        ],
        [assert_database_failure(Conn, Arity) || Arity <- [3, 4]],
        assert_claim(100, 200, true, 3),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, claim_one_time_key, '_')),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, claim_fallback_key, '_')),
        ok
    after
        meck:unload(Modules)
    end.

assert_claim(A, B, Allowed, Arity) ->
    IdentityCalls = meck:num_calls(olm_identity_ds, find_identity, '_'),
    Result = apply(
        olm_identity_logic,
        claim_keys,
        [A, B, <<"synthetic-device">>] ++
            case Arity of
                3 -> [];
                4 -> [<<"synthetic-request">>]
            end
    ),
    Expected =
        case Allowed of
            true -> {error, <<"device_not_registered">>};
            false -> {error, <<"claim_not_authorized">>}
        end,
    ?assertEqual(Expected, Result),
    ?assertEqual(
        IdentityCalls +
            case Allowed of
                true -> 1;
                false -> 0
            end,
        meck:num_calls(olm_identity_ds, find_identity, '_')
    ).

assert_database_failure(Conn, Arity) ->
    {ok, []} = elib_pg:query(Conn, <<"SAVEPOINT missing_relation">>, []),
    try
        {ok, []} = elib_pg:query(Conn, <<"DROP TABLE public.group_member">>, []),
        assert_claim(100, 200, false, Arity)
    after
        {ok, []} = elib_pg:query(Conn, <<"ROLLBACK TO SAVEPOINT missing_relation">>, []),
        {ok, []} = elib_pg:query(Conn, <<"RELEASE SAVEPOINT missing_relation">>, [])
    end.

claim_shared_group_authorization_test_() ->
    Cases = [
        {active_shared_group, {ok, [#{<<"?column?">> => 1}]}, true},
        {no_active_shared_group, {ok, []}, false},
        {database_error, {error, unavailable}, false},
        {database_exception, throw, false},
        {unexpected_query_shape, {ok, [], [], [{1}]}, false}
    ],
    [
        {atom_to_list(Name) ++ " arity " ++ integer_to_list(Arity), fun() ->
            assert_shared_group_claim(Arity, Reply, Allowed)
        end}
     || {Name, Reply, Allowed} <- Cases, Arity <- [3, 4]
    ].

assert_shared_group_claim(Arity, Reply, Allowed) ->
    Modules = [olm_identity_ds, friend_ds, elib_pg, elib_metric],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(friend_ds, is_friend, fun(100, 200) -> false end),
        meck:expect(elib_metric, increment, fun(_) -> ok end),
        meck:expect(elib_pg, query, fun(_, _) ->
            case Reply of
                throw -> error(synthetic_database_failure);
                _ -> Reply
            end
        end),
        %% A successful authorization stops before any real key can be consumed.
        meck:expect(olm_identity_ds, find_identity, fun(200, <<"dev-B">>) ->
            {ok, not_found}
        end),
        Result = apply(
            olm_identity_logic,
            claim_keys,
            [100, 200, <<"dev-B">>] ++
                case Arity of
                    3 -> [];
                    4 -> [<<"request-1">>]
                end
        ),
        Expected =
            case Allowed of
                true -> {error, <<"device_not_registered">>};
                false -> {error, <<"claim_not_authorized">>}
            end,
        ?assertEqual(Expected, Result),
        ?assertEqual(1, meck:num_calls(elib_pg, query, '_')),
        assert_shared_group_query(),
        ?assertEqual(
            case Allowed of
                true -> 1;
                false -> 0
            end,
            meck:num_calls(olm_identity_ds, find_identity, '_')
        ),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, claim_one_time_key, '_'))
    after
        meck:unload(Modules)
    end.

assert_shared_group_query() ->
    Sql = meck:capture(first, elib_pg, query, ['_', '_'], 1),
    ?assertEqual([100, 200], meck:capture(first, elib_pg, query, ['_', '_'], 2)),
    ?assertEqual(
        <<
            "SELECT 1 FROM group_member a "
            "JOIN group_member b ON a.group_id = b.group_id "
            "WHERE a.user_id = $1 AND b.user_id = $2 AND a.status = 1 AND b.status = 1 "
            "LIMIT 1"
        >>,
        iolist_to_binary(Sql)
    ).
