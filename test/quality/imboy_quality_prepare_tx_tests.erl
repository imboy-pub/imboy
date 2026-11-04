-module(imboy_quality_prepare_tx_tests).
-include_lib("eunit/include/eunit.hrl").

connection_failure_records_original_error_test() ->
    with_journal(fun(J) ->
        put(cleanup_calls, []),
        try
            ?assertException(
                error,
                {prepare_failed, simulated_connection_failure, _},
                imboy_quality_env_cleanup:with_transaction(
                    J,
                    cleanup(),
                    fun() -> ok end,
                    fun() -> error(simulated_connection_failure) end
                )
            ),
            ?assertEqual([], get(cleanup_calls)),
            ?assert(has_event(J, <<"prepare_failed">>, undefined))
        after
            erase(cleanup_calls)
        end
    end).

clone_failure_cleans_only_confirmed_resources_test() ->
    with_journal(fun(J) ->
        put(cleanup_calls, []),
        try
            ?assertException(
                error,
                {prepare_failed, simulated_clone_failure, _},
                imboy_quality_env_cleanup:with_transaction(
                    J,
                    cleanup(),
                    fun() -> ok end,
                    fun() ->
                        imboy_quality_env_cleanup:transaction_intent(clone, <<"db_1">>),
                        imboy_quality_env_cleanup:transaction_confirm(clone, <<"db_1">>, "db_1"),
                        imboy_quality_env_cleanup:transaction_intent(clone, <<"db_2">>),
                        error(simulated_clone_failure)
                    end
                )
            ),
            ?assertEqual([{clone, "db_1"}], get(cleanup_calls)),
            ?assert(has_event(J, <<"intent">>, <<"db_2">>)),
            ?assertNot(has_event(J, <<"confirmed">>, <<"db_2">>))
        after
            erase(cleanup_calls)
        end
    end).

garage_and_clone_are_recovered_in_reverse_order_test() ->
    with_journal(fun(J) ->
        put(cleanup_calls, []),
        Garage = #{
            dir => "/tmp/fake-garage",
            os_pid => 123,
            dir_identity => {1, 2, 3},
            dir_token => <<"fake">>,
            process_identity => #{start => <<"fake">>}
        },
        try
            ?assertException(
                error,
                {prepare_failed, simulated_after_garage, _},
                imboy_quality_env_cleanup:with_transaction(
                    J,
                    cleanup(),
                    fun() -> ok end,
                    fun() ->
                        imboy_quality_env_cleanup:transaction_intent(clone, <<"db_1">>),
                        imboy_quality_env_cleanup:transaction_confirm(clone, <<"db_1">>, "db_1"),
                        imboy_quality_env_cleanup:transaction_intent(garage, <<"garage_1">>),
                        imboy_quality_env_cleanup:transaction_confirm(
                            garage, <<"garage_1">>, Garage
                        ),
                        error(simulated_after_garage)
                    end
                )
            ),
            ?assertEqual([{garage, Garage}, {clone, "db_1"}], get(cleanup_calls)),
            ?assert(has_event(J, <<"confirmed">>, <<"garage_1">>))
        after
            erase(cleanup_calls)
        end
    end).

cleanup() ->
    fun(Type, Resource) ->
        put(cleanup_calls, get(cleanup_calls) ++ [{Type, Resource}]),
        ok
    end.

has_event(J, Event, Name) ->
    {ok, Bytes} = file:read_file(maps:get(path, J)),
    Lines = [L || L <- binary:split(Bytes, <<"\n">>, [global]), L =/= <<>>],
    lists:any(
        fun(Line) ->
            E = json:decode(Line),
            maps:get(<<"event">>, E, undefined) =:= Event andalso
                (Name =:= undefined orelse maps:get(<<"name">>, E, undefined) =:= Name)
        end,
        Lines
    ).

with_journal(Fun) ->
    Root = filename:join("/tmp", "imboy-journal-test-" ++ os:getpid()),
    Evidence = filename:join(Root, "evidence"),
    ok = file:make_dir(Root),
    ok = file:make_dir(Evidence),
    PG = #{<<"pid">> => 123, <<"port">> => 25496, <<"system_identifier">> => <<"fake">>},
    J = imboy_quality_env_cleanup:journal_open(Root, PG),
    try
        Fun(J)
    after
        _ = imboy_quality_env_cleanup:journal_close(J),
        file:delete(maps:get(path, J)),
        file:del_dir(Evidence),
        file:del_dir(Root)
    end.
