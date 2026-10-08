-module(elib_tsid_reinit_fence_tests).
-include_lib("eunit/include/eunit.hrl").

reinitialization_preserves_fence_test() ->
    try
        Opts = #{dc_id => 1, node_id => 1, dc_bits => 3},
        elib_tsid:reset_for_test(),
        ok = elib_tsid:init(Opts),
        {ok, Initial} = elib_tsid:runtime_handle(),
        {Pid, Ref} = spawn_monitor(fun() -> ok end),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 1000 -> error(timeout)
        end,
        Fence = atomics:new(2, []),
        atomics:put(Fence, 1, 1),
        atomics:put(Fence, 2, 0),
        ok = elib_tsid:guarded_publish(#{
            combined_node => maps:get(combined_node, Initial),
            dc_bits => 3,
            names => [guarded_name],
            max_logical_lead_ms => 512,
            capacity_wait_timeout_ms => 100,
            max_batch_chunk => 1050624,
            cursor_floor_ts => erlang:system_time(millisecond) - 1735689600000,
            guard_ref => Fence,
            guard_pid => Pid
        }),
        {ok, Before} = elib_tsid:runtime_handle(),
        ok = elib_tsid:init(Opts#{names => [added_name]}),
        {ok, After} = elib_tsid:runtime_handle(),
        ?assertEqual(Fence, maps:get(guard_ref, After)),
        ?assertEqual(Pid, maps:get(guard_pid, After)),
        ?assertEqual(maps:get(cursor, Before), maps:get(cursor, After)),
        ?assert(lists:member(guarded_name, elib_tsid:registered())),
        ?assert(lists:member(added_name, elib_tsid:registered())),
        ?assertError(
            {elib_tsid_fenced, #{phase := renew_unreachable}},
            elib_tsid:generate(guarded_name)
        )
    after
        elib_tsid:reset_for_test()
    end.
