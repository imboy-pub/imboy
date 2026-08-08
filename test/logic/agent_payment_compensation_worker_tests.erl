-module(agent_payment_compensation_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% 补偿 worker 只依赖持久化 outbox，不依赖进程内队列。

clear() ->
    lists:foreach(fun erase/1, [released, retried]),
    ok.

releases_claimed_compensation_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_compensation_ds, [
                {claim_pending, 1, fun(20) ->
                    {ok, [#{<<"id">> => 1001, <<"attempts">> => 1}]}
                end},
                {release, 1, fun(1001) ->
                    put(released, true),
                    ok
                end},
                {mark_retry, 3, fun(_, _, _) ->
                    put(retried, true),
                    ok
                end}
            ]}
        ],
        fun() ->
            clear(),
            ?assertEqual(ok, agent_payment_compensation_worker:process_once()),
            ?assertEqual(true, get(released)),
            ?assertEqual(undefined, get(retried))
        end
    ).

failed_release_returns_claim_to_retry_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_compensation_ds, [
                {claim_pending, 1, fun(20) ->
                    {ok, [#{<<"id">> => 1002, <<"attempts">> => 4}]}
                end},
                {release, 1, fun(1002) -> {error, timeout} end},
                {mark_retry, 3, fun(1002, DelaySecs, timeout) ->
                    put(retried, {DelaySecs, timeout}),
                    ok
                end}
            ]}
        ],
        fun() ->
            clear(),
            ?assertEqual(ok, agent_payment_compensation_worker:process_once()),
            ?assertEqual({16, timeout}, get(retried))
        end
    ).
