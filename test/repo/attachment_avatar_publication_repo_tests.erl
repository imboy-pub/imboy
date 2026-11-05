-module(attachment_avatar_publication_repo_tests).
-include_lib("eunit/include/eunit.hrl").

invalid_reservation_test() ->
    ?assertEqual(
        {error, invalid_avatar_publication},
        attachment_avatar_publication_repo:reserve(<<>>, <<"public">>)
    ).

publication_guards_test_() ->
    {setup,
        fun() ->
            meck:new(elib_pg, [passthrough, no_link]),
            meck:new(attachment_repo, [passthrough, no_link]),
            meck:expect(elib_pg, with_tx, fun(F, _) -> F(test_conn) end),
            meck:expect(attachment_repo, find_by_path_for_update_tx, fun(_, _) ->
                {ok, #{
                    <<"id">> => 10,
                    <<"path">> => <<"app1/avatar/test">>,
                    <<"status">> => 1,
                    <<"lifecycle_state">> => <<"live">>,
                    <<"storage_container">> => <<"private">>
                }}
            end),
            meck:expect(attachment_repo, verify_direct_winner_tx, fun(_, _) -> ok end)
        end,
        fun(_) ->
            meck:unload(attachment_repo),
            meck:unload(elib_pg)
        end,
        fun(_) ->
            [
                ?_assertEqual(
                    {error, avatar_bucket_not_private},
                    attachment_avatar_publication_repo:reserve(
                        <<"app1/avatar/test">>, <<"private">>
                    )
                ),
                ?_test(begin
                    meck:expect(elib_pg, query, fun(test_conn, Sql, [10]) ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"p.state='reserved'">>)),
                        {ok, []}
                    end),
                    ?assertEqual(
                        {error, avatar_publication_not_retryable},
                        attachment_avatar_publication_repo:start_copy(10)
                    )
                end),
                ?_test(begin
                    meck:reset(elib_pg),
                    ?assertEqual(
                        {error, timeout},
                        attachment_avatar_publication_repo:finish_copy(
                            10, <<"00000000-0000-4000-8000-000000000001">>, {error, timeout}
                        )
                    ),
                    ?assertEqual(0, meck:num_calls(elib_pg, query, '_'))
                end)
            ]
        end}.

not_dispatched_releases_only_current_token_test_() ->
    {setup, fun() -> meck:new(elib_pg, [passthrough, no_link]) end,
        fun(_) -> meck:unload(elib_pg) end, fun(_) ->
            ?_test(begin
                T = <<"00000000-0000-4000-8000-000000000001">>,
                meck:expect(elib_pg, query, fun(Sql, [10, T0, State]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"dispatch_token=$2::uuid">>)),
                    ?assertEqual(T, T0),
                    {ok, [#{<<"state">> => State}]}
                end),
                ?assertEqual(
                    {error, {not_dispatched, location_changed}},
                    attachment_avatar_publication_repo:finish_copy(
                        10, T, {error, {not_dispatched, location_changed}}
                    )
                ),
                meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
                ?assertEqual(
                    {error, avatar_publication_unsettled},
                    attachment_avatar_publication_repo:finish_copy(10, T, {ok, #{}})
                )
            end)
        end}.
