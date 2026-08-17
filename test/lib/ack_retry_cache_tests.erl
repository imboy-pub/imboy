-module(ack_retry_cache_tests).

-include_lib("eunit/include/eunit.hrl").

reset_table() ->
    ok = ack_retry_cache:init_table(),
    ets:delete_all_objects(ack_retry_cache_ets),
    ok.

set_get_test() ->
    ok = reset_table(),
    Key = {timer, make_ref()},
    ?assertEqual(ok, ack_retry_cache:set(Key, value, 1000)),
    ?assertEqual({ok, value}, ack_retry_cache:get(Key)).

expired_entry_is_not_returned_test() ->
    ok = reset_table(),
    Key = {ack, make_ref()},
    ?assertEqual(ok, ack_retry_cache:set(Key, true, 0)),
    ?assertEqual(undefined, ack_retry_cache:get(Key)),
    ?assertEqual([], ets:lookup(ack_retry_cache_ets, Key)).

cleanup_removes_expired_entries_test() ->
    ok = reset_table(),
    Key = {cleanup, make_ref()},
    ok = ack_retry_cache:set(Key, value, 0),
    ?assertEqual(ok, ack_retry_cache:cleanup()),
    ?assertEqual([], ets:lookup(ack_retry_cache_ets, Key)).

delete_if_value_preserves_replaced_timer_test() ->
    ok = reset_table(),
    Key = {timer, make_ref()},
    OldRef = make_ref(),
    NewRef = make_ref(),
    ok = ack_retry_cache:set(Key, NewRef, 1000),
    ?assertEqual(false, ack_retry_cache:delete_if_value(Key, OldRef)),
    ?assertEqual({ok, NewRef}, ack_retry_cache:get(Key)),
    ?assertEqual(true, ack_retry_cache:delete_if_value(Key, NewRef)),
    ?assertEqual(undefined, ack_retry_cache:get(Key)).

table_survives_transient_caller_test() ->
    ok = reset_table(),
    Owner = ets:info(ack_retry_cache_ets, owner),
    Key = {ack, make_ref()},
    {Pid, MonitorRef} = spawn_monitor(fun() ->
        ok = ack_retry_cache:set(Key, true, 1000)
    end),
    receive
        {'DOWN', MonitorRef, process, Pid, normal} -> ok
    after 2000 ->
        erlang:error(transient_caller_timeout)
    end,
    ?assertNotEqual(undefined, ets:whereis(ack_retry_cache_ets)),
    ?assertEqual(Owner, ets:info(ack_retry_cache_ets, owner)),
    ?assertEqual({ok, true}, ack_retry_cache:get(Key)).

concurrent_read_write_test() ->
    ok = reset_table(),
    ?assertEqual(true, ets:info(ack_retry_cache_ets, write_concurrency)),
    ?assertEqual(true, ets:info(ack_retry_cache_ets, read_concurrency)),
    Parent = self(),
    Count = 200,
    lists:foreach(
        fun(N) ->
            spawn(fun() ->
                Key = {concurrent, N},
                ok = ack_retry_cache:set(Key, N, 1000),
                Parent ! {ack_cache_result, N, ack_retry_cache:get(Key)}
            end)
        end,
        lists:seq(1, Count)
    ),
    Results = receive_results(Count, []),
    ?assertEqual(lists:seq(1, Count), lists:sort(Results)).

receive_results(0, Acc) ->
    Acc;
receive_results(Remaining, Acc) ->
    receive
        {ack_cache_result, N, {ok, N}} ->
            receive_results(Remaining - 1, [N | Acc])
    after 5000 ->
        erlang:error({concurrent_result_timeout, Remaining})
    end.
