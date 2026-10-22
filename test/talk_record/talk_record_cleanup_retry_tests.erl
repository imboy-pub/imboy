-module(talk_record_cleanup_retry_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    [
        ?_assertEqual(
            {ok, 0, <<>>}, talk_record_cleanup_retry_logic:parse(#{<<"cleanup_generation">> => 0})
        ),
        ?_assertEqual(
            {ok, 8, <<"network fixed">>},
            talk_record_cleanup_retry_logic:parse(
                #{<<"cleanup_generation">> => 8, <<"reason">> => <<"network fixed">>}
            )
        )
    ] ++
        [
            ?_assertEqual({error, invalid_body}, talk_record_cleanup_retry_logic:parse(B))
         || B <- [
                #{},
                #{<<"cleanup_generation">> => -1},
                #{<<"cleanup_generation">> => null},
                #{<<"cleanup_generation">> => 0, <<"path">> => <<"foreign">>},
                #{<<"cleanup_generation">> => 0, <<"attempts">> => 0},
                #{<<"cleanup_generation">> => 0, <<"reason">> => binary:copy(<<"x">>, 201)}
            ]
        ].

legacy_actorless_path_rejected_test() ->
    ?assertEqual({error, cleanup_actor_required}, talk_record_delete_logic:retry_purge(1)),
    ?assertEqual({error, cleanup_actor_required}, talk_record_delete_logic:retry_purge_tx(conn, 1)).

repo_authenticated_arguments_test() ->
    with_retry_mocks(fun() ->
        meck:expect(elib_tsid, generate, fun(talk_record) -> 99 end),
        meck:expect(elib_pg, query, fun(
            conn, Sql, [1, 2, 3, 4, <<"key">>, <<"hash">>, <<"reason">>, 99]
        ) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"fn_talk_record_cleanup_retry">>)),
            {ok, [#{<<"result">> => #{<<"job_id">> => 99, <<"reopened_resources">> => 1}}]}
        end),
        ?assertMatch(
            {ok, #{<<"job_id">> := 99}},
            talk_record_cleanup_retry_repo:retry_tx(
                conn, 1, 2, 3, 4, <<"key">>, <<"hash">>, <<"reason">>
            )
        ),
        ?assert(meck:validate(elib_pg))
    end).

no_fake_accepted_test() ->
    with_retry_mocks(fun() ->
        meck:expect(elib_tsid, generate, fun(_) -> 99 end),
        lists:foreach(
            fun({Response, Expected}) ->
                meck:expect(elib_pg, query, fun(_, _, _) -> {ok, [#{<<"result">> => Response}]} end),
                ?assertEqual(
                    {error, Expected},
                    talk_record_cleanup_retry_repo:retry_tx(
                        conn, 1, 2, 3, 4, <<"key">>, <<"hash">>, <<>>
                    )
                )
            end,
            [
                {#{<<"job_id">> => 99, <<"reopened_resources">> => 0}, db_error},
                {#{<<"error">> => <<"cleanup_protected">>}, cleanup_protected},
                {#{<<"error">> => <<"cleanup_not_retryable">>}, cleanup_not_retryable},
                {#{<<"error">> => <<"not_found">>}, not_found},
                {#{<<"error">> => <<"cleanup_generation_conflict">>}, cleanup_generation_conflict}
            ]
        )
    end).

readonly_capability_fail_closed_test() ->
    meck:new(elib_pg, [non_strict]),
    try
        lists:foreach(
            fun(Value) ->
                meck:expect(elib_pg, query, fun(_, _, _) -> {ok, [#{<<"capability">> => Value}]} end),
                P = talk_record_cleanup_retry_repo:capability_tx(conn, 1, 2),
                ?assertEqual(false, maps:get(<<"allowed">>, P))
            end,
            [
                null,
                #{},
                #{
                    <<"allowed">> => true,
                    <<"generation">> => null,
                    <<"exhausted_resources">> => 1,
                    <<"reason">> => <<"eligible">>
                }
            ]
        ),
        meck:expect(elib_pg, query, fun(_, _, _) -> erlang:error(unavailable) end),
        ?assertEqual(
            false, maps:get(<<"allowed">>, talk_record_cleanup_retry_repo:capability_tx(conn, 1, 2))
        )
    after
        meck:unload(elib_pg)
    end.

%% Each successful acquisition is protected before the next can fail.
with_retry_mocks(Test) ->
    ok = meck:new(elib_pg, [non_strict]),
    try
        ok = meck:new(elib_tsid, [non_strict]),
        try
            Test()
        after
            meck:unload(elib_tsid)
        end
    after
        meck:unload(elib_pg)
    end.

partial_mock_setup_preserves_existing_mock_test() ->
    pure_mock_fixture:run(fun() ->
        ok = meck:new(elib_tsid, [non_strict]),
        Existing = whereis(elib_tsid_meck),
        try
            ?assertException(
                error,
                {already_started, _},
                with_retry_mocks(fun() ->
                    error(test_body_must_not_run)
                end)
            ),
            ?assertNot(lists:member(elib_pg, meck:mocked())),
            ?assertEqual(Existing, whereis(elib_tsid_meck)),
            ?assert(is_process_alive(Existing))
        after
            meck:unload(elib_tsid)
        end
    end).
