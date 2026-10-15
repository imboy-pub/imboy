-module(cache_permission_authority_tests).
-include_lib("eunit/include/eunit.hrl").

permission_errors_test_() ->
    [
        {timeout, 35,
            ?_test(
                pure_mock_fixture:run(fun() ->
                    State = setup(),
                    try
                        ?assertEqual(ok, eunit:test(Test(State), [verbose]))
                    after
                        cleanup(State)
                    end
                end)
            )}
     || Test <- [
            fun(_) -> ?_test(denylist_query_failure_is_checked()) end,
            fun(_) -> ?_test(denylist_failure_is_not_cached()) end,
            fun(_) -> ?_test(invitation_failure_is_denied()) end,
            fun(_) -> ?_test(moment_failure_is_denied()) end,
            fun(_) -> ?_test(relationship_failure_is_not_cached()) end,
            fun(_) -> ?_test(relationship_malformed_is_denied()) end,
            fun(_) -> ?_test(pending_status_failure_is_blocked()) end,
            fun(_) -> ?_test(forward_failure_is_denied()) end,
            fun(_) -> ?_test(c2c_failure_is_denied()) end,
            fun(_) -> ?_test(read_receipt_failure_is_denied()) end,
            fun(_) -> ?_test(webrtc_failure_is_denied()) end
        ]
    ].
setup() ->
    Saved = [
        {K, application:get_env(imboy, K)}
     || K <- [sql_driver, dsync_enabled, cache_fenced_namespaces]
    ],
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, dsync_enabled, false),
    application:set_env(imboy, cache_fenced_namespaces, []),
    {ok, D} = depcache:start_link(imboy_cache, #{memory_max => 64}),
    unlink(D),
    meck:new(elib_pg, [no_link, passthrough]),
    meck:expect(elib_pg, pluck_value, fun(_, _, _, _, _) -> 0 end),
    meck:expect(elib_pg, pluck, fun(_, _, _, _) -> {error, unavailable} end),
    meck:expect(elib_pg, query, fun(_, _) -> {error, unavailable} end),
    {D, Saved}.
cleanup({D, Saved}) ->
    try
        meck:unload(elib_pg)
    after
        gen_server:stop(D),
        [
            case V of
                undefined -> application:unset_env(imboy, K);
                {ok, X} -> application:set_env(imboy, K, X)
            end
         || {K, V} <- Saved
        ]
    end.
denylist_query_failure_is_checked() ->
    ?assertError(denylist_read_unavailable, user_denylist_repo:in_denylist(1, 2)).
denylist_failure_is_not_cached() ->
    ?assertEqual(1, user_denylist_logic:in_denylist(1, 2)),
    ?assertEqual(undefined, depcache:get({in_denylist, 1, 2}, imboy_cache)),
    meck:expect(elib_pg, pluck, fun(_, _, _, _) -> {ok, 0} end),
    ?assertEqual(0, user_denylist_logic:in_denylist(1, 2)).
invitation_failure_is_denied() ->
    ?assertEqual(true, user_denylist_logic:blocked_between(1, 2)).
moment_failure_is_denied() ->
    Post = #{<<"id">> => 7, <<"status">> => 1, <<"author_uid">> => 1, <<"visibility">> => 0},
    ?assertEqual(false, moment_ds:can_view_post(2, Post)).
relationship_failure_is_not_cached() ->
    ?assertEqual({false, true}, friend_ds:check_relationship(1, 2)),
    ?assertEqual(undefined, depcache:get({check_relationship3, 1, 2}, imboy_cache)),
    meck:expect(elib_pg, query, fun(_, _) ->
        {ok, [#{<<"is_friend">> => true, <<"in_denylist">> => false}]}
    end),
    ?assertEqual({true, false}, friend_ds:check_relationship(1, 2)).
relationship_malformed_is_denied() ->
    meck:expect(elib_pg, query, fun(_, _) ->
        {ok, [#{<<"is_friend">> => not_boolean, <<"in_denylist">> => false}]}
    end),
    ?assertEqual({false, true}, friend_ds:check_relationship(1, 2)).
pending_status_failure_is_blocked() ->
    ?assertEqual(blocked, friend_ds:pending_status(1, 2)).
forward_failure_is_denied() ->
    ?assertMatch(
        {error, {in_denylist, _}}, msg_forward_logic:forward([<<"fixture">>], 1, 2, <<"c2c">>)
    ).

with_message_mocks(F) ->
    meck:new(message_ds, [no_link, passthrough]),
    meck:expect(message_ds, assemble_s2c, fun(_, Action, _) -> #{<<"action">> => Action} end),
    meck:new(msg_rate_logic, [no_link, passthrough]),
    meck:expect(msg_rate_logic, check_and_record, fun(_) -> ok end),
    meck:new(ai_agent_ds, [no_link, passthrough]),
    meck:expect(ai_agent_ds, is_agent, fun(_) -> {true, #{}} end),
    try
        F()
    after
        [meck:unload(M) || M <- [ai_agent_ds, msg_rate_logic, message_ds]]
    end.
c2c_failure_is_denied() ->
    with_message_mocks(fun() ->
        ?assertMatch(
            {reply, #{<<"action">> := <<"in_denylist">>}},
            msg_c2c_logic:c2c(<<"fixture">>, 1, #{<<"to">> => <<"2">>})
        )
    end).
read_receipt_failure_is_denied() ->
    with_message_mocks(fun() ->
        ?assertMatch(
            {reply, #{<<"action">> := <<"in_denylist">>}},
            msg_c2c_logic:c2c_read(<<"fixture">>, 1, #{<<"to">> => <<"2">>, <<"from">> => <<"1">>})
        )
    end).

webrtc_failure_is_denied() ->
    with_message_mocks(fun() ->
        ?assertMatch(
            {reply, #{<<"action">> := <<"in_denylist">>}},
            webrtc_ws_logic:event(1, 2, <<"fixture">>, <<"{}">>)
        )
    end).
