%%% Pure fault injection: verifies connection and abort propagation, not PG rollback.
-module(cs_widget_atomic_unit_tests).
-include_lib("eunit/include/eunit.hrl").

atomic_faults_test_() ->
    [
        ?_test(pure_mock_fixture:run(fun() -> run(Operation, Fault) end))
     || Operation <- [bootstrap, exchange],
        Fault <- [none, contact, write, event]
    ].

run(Operation, Fault) ->
    Mods = [elib_pg, enterprise_business_facade, cs_pg_widget, cs_pg_seat],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link, non_strict]) end, Mods),
    put(steps, []),
    try
        meck:expect(elib_pg, with_tx, fun(Fun, Opts) ->
            ?assertEqual([{reraise, true}, {ensure_committed, true}], Opts),
            try
                Fun(conn)
            catch
                throw:{abort_tx, R} -> {error, R}
            end
        end),
        meck:expect(
            enterprise_business_facade,
            ensure_contact_in,
            fun(1, #{workspace_id := 2, contact_conn := conn}) ->
                step(contact),
                result(contact, Fault, #{contact => #{id => 3}, reused => true})
            end
        ),
        meck:expect(
            cs_pg_widget,
            insert_widget_bootstrap_token_in,
            fun(conn, 1, #{contact_id := 3} = Token) ->
                step(write),
                result(write, Fault, Token)
            end
        ),
        meck:expect(
            cs_pg_widget,
            record_widget_nonce_in,
            fun(conn, 1, 4, <<"digest">>, 9) ->
                step(write),
                case Fault of
                    write -> {error, write_failed};
                    _ -> ok
                end
            end
        ),
        meck:expect(
            cs_pg_seat,
            insert_event_in,
            fun(conn, 1, #{detail := #{<<"contact_id">> := 3}}) ->
                step(event),
                result(event, Fault, #{id => 5})
            end
        ),
        Command = #{
            contact_params => #{workspace_id => 2},
            token => #{id => 6},
            installation_id => 4,
            jti_digest => <<"digest">>,
            expires_at => 9,
            event => #{detail => #{}}
        },
        Actual =
            case Operation of
                bootstrap -> cs_pg_store:insert_widget_bootstrap_audited(1, Command);
                exchange -> cs_pg_store:exchange_widget_identity_audited(1, Command)
            end,
        Expected =
            case Fault of
                none when Operation =:= bootstrap -> {ok, #{id => 6, contact_id => 3}};
                none -> {ok, #{contact_id => 3, reused => true}};
                event -> {error, {audit_append_failed, event_failed}};
                contact -> {error, contact_failed};
                write -> {error, write_failed}
            end,
        ?assertEqual(Expected, Actual),
        Order =
            case Operation of
                bootstrap -> [contact, write, event];
                exchange -> [write, contact, event]
            end,
        Steps = lists:reverse(get(steps)),
        ExpectedSteps =
            case Fault of
                none -> Order;
                _ -> lists:takewhile(fun(X) -> X =/= Fault end, Order) ++ [Fault]
            end,
        ?assertEqual(ExpectedSteps, Steps)
    after
        erase(steps),
        lists:foreach(fun meck:unload/1, Mods)
    end.

step(Name) -> put(steps, [Name | get(steps)]).
result(Name, Name, _) -> {error, list_to_atom(atom_to_list(Name) ++ "_failed")};
result(_, _, Value) -> {ok, Value}.

bootstrap_failure_hides_secret_test_() ->
    [
        ?_test(pure_mock_fixture:run(fun() -> bootstrap_failure(Reason) end))
     || Reason <- [
            contact_failed,
            identity_failed,
            token_failed,
            {audit_append_failed, event_failed},
            missing
        ]
    ].

bootstrap_failure(Reason) ->
    meck:new(cs_widget_support, [passthrough, no_link]),
    meck:new(cs_fake_store, [passthrough, no_link]),
    try
        Store =
            case Reason of
                missing -> cs_fake_id;
                _ -> cs_fake_store
            end,
        code:ensure_loaded(Store),
        meck:expect(cs_widget_support, with_store, fun(_, Fun) -> Fun(Store) end),
        meck:expect(cs_widget_support, resolve_default_workspace, fun(1, _) -> {ok, 2} end),
        meck:expect(cs_widget_support, new_id, fun(cs_visit_token, _) -> {ok, 6} end),
        meck:expect(
            cs_fake_store,
            fetch_widget_installation_by_public_id_global,
            fun(_) ->
                {ok, #{
                    id => 4,
                    organization_id => 1,
                    status => active,
                    public_widget_id => <<"widget">>,
                    allowed_origins => [<<"https://example.test">>]
                }}
            end
        ),
        meck:expect(cs_widget_support, new_token_secret, fun(_) -> <<"private-secret">> end),
        meck:expect(cs_widget_support, token_digest, fun(_, _) -> <<"digest">> end),
        meck:expect(cs_fake_store, insert_widget_bootstrap_audited, fun(1, Command) ->
            ?assertNot(maps:is_key(secret, maps:get(token, Command))),
            ?assertEqual(nomatch, binary:match(term_to_binary(Command), <<"private-secret">>)),
            {error, Reason}
        end),
        %% Missing capability still needs an installation read, without a legacy write fallback.
        case Reason of
            missing ->
                meck:new(cs_fake_id, [passthrough, no_link, non_strict]),
                meck:expect(
                    cs_fake_id,
                    fetch_widget_installation_by_public_id_global,
                    fun(_) ->
                        {ok, #{
                            id => 4,
                            organization_id => 1,
                            status => active,
                            public_widget_id => <<"widget">>,
                            allowed_origins => [<<"https://example.test">>]
                        }}
                    end
                );
            _ ->
                ok
        end,
        Actual = cs_widget_app:bootstrap(0, #{
            public_widget_id => <<"widget">>,
            origin => <<"https://example.test">>,
            subject_id => <<"subject">>,
            subject_key => <<"key">>,
            at => 1
        }),
        Expected =
            case Reason of
                missing -> {error, {missing_store_capability, insert_widget_bootstrap_audited}};
                _ -> {error, Reason}
            end,
        ?assertEqual(Expected, Actual),
        ?assertEqual(nomatch, binary:match(term_to_binary(Actual), <<"private-secret">>))
    after
        case lists:member(cs_fake_id, meck:mocked()) of
            true -> meck:unload(cs_fake_id);
            false -> ok
        end,
        meck:unload(cs_fake_store),
        meck:unload(cs_widget_support)
    end.

commit_confirmation_test_() ->
    [
        ?_test(pure_mock_fixture:run(fun() -> commit_confirmation(Operation, Status) end))
     || Operation <- [bootstrap, exchange],
        Status <- [{ok, commit}, {ok, rollback}, {error, disconnected}]
    ].

commit_confirmation(Operation, Status) ->
    Conn = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Mods = [pooler, config_ds, epgsql_sock, enterprise_business_facade, cs_pg_widget, cs_pg_seat],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link, non_strict]) end, Mods),
    try
        meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
        meck:expect(pooler, return_member, fun(pgsql, Conn0) ->
            ?assertEqual(Conn, Conn0),
            ok
        end),
        meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
        %% Keep real epgsql:with_transaction/3; mock only the socket boundary.
        meck:expect(epgsql_sock, sync_command, fun(C, epgsql_cmd_squery, _) ->
            ?assertEqual(Conn, C),
            {ok, [], []}
        end),
        meck:expect(epgsql_sock, get_cmd_status, fun(C) ->
            ?assertEqual(Conn, C),
            Status
        end),
        meck:expect(enterprise_business_facade, ensure_contact_in, fun(1, #{contact_conn := C}) ->
            ?assertEqual(Conn, C),
            {ok, #{contact => #{id => 3}, reused => true}}
        end),
        meck:expect(cs_pg_widget, insert_widget_bootstrap_token_in, fun(C, 1, Token) ->
            ?assertEqual(Conn, C),
            {ok, Token}
        end),
        meck:expect(cs_pg_widget, record_widget_nonce_in, fun(C, 1, 4, <<"digest">>, 9) ->
            ?assertEqual(Conn, C),
            ok
        end),
        meck:expect(cs_pg_seat, insert_event_in, fun(C, 1, _) ->
            ?assertEqual(Conn, C),
            {ok, #{id => 5}}
        end),
        Command = #{
            contact_params => #{workspace_id => 2},
            token => #{id => 6},
            installation_id => 4,
            jti_digest => <<"digest">>,
            expires_at => 9,
            event => #{detail => #{}}
        },
        BodyResult =
            case Operation of
                bootstrap -> {ok, #{id => 6, contact_id => 3}};
                exchange -> {ok, #{contact_id => 3, reused => true}}
            end,
        Actual =
            case Operation of
                bootstrap -> cs_pg_store:insert_widget_bootstrap_audited(1, Command);
                exchange -> cs_pg_store:exchange_widget_identity_audited(1, Command)
            end,
        Expected =
            case Status of
                {ok, commit} ->
                    BodyResult;
                {ok, rollback} ->
                    {error, {db_exception, error, {ensure_committed_failed, rollback}}};
                {error, disconnected} ->
                    {error, {db_exception, error, {badmatch, Status}}}
            end,
        ?assertEqual(Expected, Actual),
        ?assertEqual(1, meck:num_calls(epgsql_sock, get_cmd_status, 1)),
        ?assertEqual(1, meck:num_calls(pooler, return_member, 2))
    after
        lists:foreach(fun meck:unload/1, Mods),
        Conn ! stop
    end.
