-module(enterprise_webhook_transport_tests).
-include_lib("eunit/include/eunit.hrl").

%% Isolate settlement writers; the public executor and pinned URL guard are real.
transport_error_preserves_cause_and_latency_test() ->
    Parent = self(),
    Previous = application:get_env(imboy, bot_webhook_sender_mod),
    Modules = [
        enterprise_webhook_repo,
        bot_webhook_delivery_repo,
        bot_webhook_delivery_sender,
        elib_metric
    ],
    with_mocks(Modules, fun() ->
        try
            application:set_env(imboy, bot_webhook_sender_mod, bot_webhook_delivery_sender),
            configure_stubs(Parent),
            Delivery = #{
                <<"delivery_id">> => <<"synthetic-transport-error">>,
                <<"bot_id">> => <<"eapp:995011">>,
                <<"webhook_url">> => <<"https://example.com/hook">>,
                <<"pinned_ip">> => <<"8.8.8.8">>,
                <<"payload">> => <<"{}">>
            },
            ?assertEqual(ok, enterprise_webhook_logic:execute_delivery(Delivery)),
            receive
                {attempt, A} ->
                    ?assertEqual(<<"error">>, maps:get(status_class, A)),
                    ?assertEqual(null, maps:get(http_status, A)),
                    ?assert(maps:get(latency_ms, A) >= 20),
                    ?assertEqual(<<"{recv,timeout}">>, maps:get(error_trunc, A))
            after 1000 -> erlang:error(attempt_missing)
            end,
            receive
                {retry, 5, 1, <<>>} -> ok
            after 1000 -> erlang:error(retry_missing)
            end,
            ?assertEqual(1, meck:num_calls(bot_webhook_delivery_repo, insert_attempt, 2)),
            ?assertEqual(1, meck:num_calls(bot_webhook_delivery_sender, post, 7))
        after
            restore_sender(Previous)
        end
    end).

with_mocks([], Fun) ->
    Fun();
with_mocks([Module | Rest], Fun) ->
    meck:new(Module, [non_strict, no_link]),
    try
        with_mocks(Rest, Fun)
    after
        meck:unload(Module)
    end.

configure_stubs(Parent) ->
    meck:expect(enterprise_webhook_repo, principal_of_delivery, fun(_) -> {ok, 995011} end),
    meck:expect(enterprise_webhook_repo, get_secret, fun(_) -> {ok, <<"synthetic-only">>} end),
    meck:expect(elib_metric, record, fun(_, _) -> ok end),
    meck:expect(elib_metric, increment, fun(_, _, _) -> ok end),
    meck:expect(
        bot_webhook_delivery_repo,
        insert_attempt,
        fun(_, A) ->
            Parent ! {attempt, A},
            ok
        end
    ),
    meck:expect(
        bot_webhook_delivery_repo,
        mark_retry,
        fun(_, After, Number, Reason) ->
            Parent ! {retry, After, Number, Reason},
            {ok, 1}
        end
    ),
    meck:expect(
        bot_webhook_delivery_sender,
        post,
        fun(_, _, _, _, _, _, _) ->
            timer:sleep(25),
            {error, {recv, timeout}}
        end
    ).

restore_sender(undefined) -> application:unset_env(imboy, bot_webhook_sender_mod);
restore_sender({ok, M}) -> application:set_env(imboy, bot_webhook_sender_mod, M).
