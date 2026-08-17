-module(message_ds_ack_retry_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

log_mocks() ->
    [
        {elib_log, [
            {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) -> ok end},
            {'internal_log', 5, fun(_Level, _Fmt, _Args, _Module, _Line) -> ok end}
        ]}
    ].

start_receiver(Tag, Parent) ->
    spawn(fun Loop() ->
        receive
            Msg ->
                Parent ! {Tag, Msg},
                Loop()
        end
    end).

send_next_immediate_publish_targets_only_unacked_devices_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {imboy_syn, [
                    {'list_by_uid', 1, fun(1) ->
                        Parent = self(),
                        PidAck = start_receiver(ack_pid, Parent),
                        PidUnacked = start_receiver(unacked_pid, Parent),
                        put(test_pids, [PidAck, PidUnacked]),
                        [
                            {PidAck, {<<"ios">>, <<"did_ack">>}},
                            {PidUnacked, {<<"ios">>, <<"did_unacked">>}}
                        ]
                    end},
                    {'publish', 3, fun(_, _, _) -> erlang:error(unexpected_publish_call) end}
                ]},
                {ack_retry_cache, [
                    {'get', 1, fun
                        ({ack_received, 1, <<"did_ack">>, <<"msg_1">>}) -> {ok, true};
                        ({ack_received, 1, <<"did_unacked">>, <<"msg_1">>}) -> undefined;
                        (_) -> undefined
                    end}
                ]}
            ],
        fun() ->
            ok = message_ds:send_next(1, <<"msg_1">>, <<"raw_msg_1">>, [0]),
            receive
                {unacked_pid, {timeout, _Ref, <<"raw_msg_1">>}} -> ok
            after 300 ->
                ?assert(false)
            end,
            receive
                {ack_pid, _} -> ?assert(false)
            after 150 ->
                ok
            end,
            ?assertEqual(0, meck:num_calls(imboy_syn, publish, 3)),
            [PidAck, PidUnacked] = get(test_pids),
            exit(PidAck, kill),
            exit(PidUnacked, kill),
            erase(test_pids)
        end
    ).

send_next_immediate_publish_skips_all_acked_devices_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {imboy_syn, [
                    {'list_by_uid', 1, fun(1) ->
                        Parent = self(),
                        PidAck1 = start_receiver(ack_pid_1, Parent),
                        PidAck2 = start_receiver(ack_pid_2, Parent),
                        put(test_pids, [PidAck1, PidAck2]),
                        [
                            {PidAck1, {<<"ios">>, <<"did_ack_1">>}},
                            {PidAck2, {<<"android">>, <<"did_ack_2">>}}
                        ]
                    end},
                    {'publish', 3, fun(_, _, _) -> erlang:error(unexpected_publish_call) end}
                ]},
                {ack_retry_cache, [
                    {'get', 1, fun
                        ({ack_received, 1, <<"did_ack_1">>, <<"msg_2">>}) -> {ok, true};
                        ({ack_received, 1, <<"did_ack_2">>, <<"msg_2">>}) -> {ok, true};
                        (_) -> undefined
                    end}
                ]}
            ],
        fun() ->
            ok = message_ds:send_next(1, <<"msg_2">>, <<"raw_msg_2">>, [0]),
            receive
                {ack_pid_1, _} -> ?assert(false)
            after 150 ->
                ok
            end,
            receive
                {ack_pid_2, _} -> ?assert(false)
            after 150 ->
                ok
            end,
            ?assertEqual(0, meck:num_calls(imboy_syn, publish, 3)),

            [PidAck1, PidAck2] = get(test_pids),
            exit(PidAck1, kill),
            exit(PidAck2, kill),
            erase(test_pids)
        end
    ).

send_next_delayed_publish_replaces_old_timer_for_unacked_device_test_() ->
    OldRef = erlang:start_timer(5000, self(), old_timer_should_be_canceled),
    ?WITH_MECKS(
        log_mocks() ++
            [
                {imboy_syn, [
                    {'list_by_uid', 1, fun(1) ->
                        Parent = self(),
                        Pid1 = start_receiver(delayed_pid_1, Parent),
                        put(test_pids, [Pid1]),
                        [{Pid1, {<<"ios">>, <<"did_1">>}}]
                    end}
                ]},
                {ack_retry_cache, [
                    {'get', 1, fun
                        ({ack_received, 1, <<"did_1">>, <<"msg_3">>}) -> undefined;
                        ({1, <<"did_1">>, <<"msg_3">>}) -> {ok, OldRef};
                        (_) -> undefined
                    end},
                    {'set', 3, fun({1, <<"did_1">>, <<"msg_3">>}, Ref, TTL) ->
                        ?assert(is_reference(Ref)),
                        %% ACK ETS 使用毫秒 TTL：Delay=100 + 5000ms 竞态余量。
                        ?assertEqual(5100, TTL),
                        ok
                    end}
                ]}
            ],
        fun() ->
            ok = message_ds:send_next(1, <<"msg_3">>, <<"raw_msg_3">>, [100]),
            receive
                {delayed_pid_1,
                    {timeout, _Ref, {[], {1, <<"did_1">>, <<"msg_3">>}, <<"raw_msg_3">>}}} ->
                    ok
            after 400 ->
                ?assert(false)
            end,
            ?assertEqual(1, meck:num_calls(ack_retry_cache, set, 3)),
            [Pid1] = get(test_pids),
            exit(Pid1, kill),
            erase(test_pids)
        end
    ).

send_next_delayed_publish_skips_already_acked_device_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {imboy_syn, [
                    {'list_by_uid', 1, fun(1) ->
                        Parent = self(),
                        Pid1 = start_receiver(delayed_pid_1, Parent),
                        put(test_pids, [Pid1]),
                        [{Pid1, {<<"ios">>, <<"did_1">>}}]
                    end}
                ]},
                {ack_retry_cache, [
                    {'get', 1, fun
                        ({ack_received, 1, <<"did_1">>, <<"msg_4">>}) -> {ok, true};
                        (_) -> undefined
                    end},
                    {'set', 3, fun(_, _, _) -> erlang:error(unexpected_set_call) end}
                ]}
            ],
        fun() ->
            ok = message_ds:send_next(1, <<"msg_4">>, <<"raw_msg_4">>, [100]),
            receive
                {delayed_pid_1, _} -> ?assert(false)
            after 300 ->
                ok
            end,
            ?assertEqual(0, meck:num_calls(ack_retry_cache, set, 3)),
            [Pid1] = get(test_pids),
            exit(Pid1, kill),
            erase(test_pids)
        end
    ).

send_next_race_ack_after_immediate_publish_only_retries_unacked_device_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {imboy_syn, [
                    {'list_by_uid', 1, fun(1) ->
                        Parent = self(),
                        PidAckRace = start_receiver(race_ack_pid, Parent),
                        PidPending = start_receiver(race_pending_pid, Parent),
                        put(test_pids, [PidAckRace, PidPending]),
                        [
                            {PidAckRace, {<<"ios">>, <<"did_ack_race">>}},
                            {PidPending, {<<"android">>, <<"did_pending">>}}
                        ]
                    end}
                ]},
                {ack_retry_cache, [
                    {'get', 1, fun
                        ({ack_received, 1, <<"did_ack_race">>, <<"msg_5">>} = Key) ->
                            Cnt =
                                case get({ack_lookup_count, Key}) of
                                    undefined -> 0;
                                    N -> N
                                end,
                            put({ack_lookup_count, Key}, Cnt + 1),
                            case Cnt of
                                0 -> undefined;
                                _ -> {ok, true}
                            end;
                        ({ack_received, 1, <<"did_pending">>, <<"msg_5">>}) ->
                            undefined;
                        ({1, <<"did_ack_race">>, <<"msg_5">>}) ->
                            undefined;
                        ({1, <<"did_pending">>, <<"msg_5">>}) ->
                            undefined;
                        (_) ->
                            undefined
                    end},
                    {'set', 3, fun(Key, Ref, TTL) ->
                        TimerSets =
                            case get(test_timer_sets) of
                                undefined -> [];
                                V -> V
                            end,
                        put(test_timer_sets, [Key | TimerSets]),
                        ?assert(is_reference(Ref)),
                        %% ACK ETS 使用毫秒 TTL：Delay=100 + 5000ms 竞态余量。
                        ?assertEqual(5100, TTL),
                        ok
                    end}
                ]}
            ],
        fun() ->
            ok = message_ds:send_next(1, <<"msg_5">>, <<"raw_msg_5">>, [0, 100]),
            receive
                {race_ack_pid, {timeout, _Ref1, <<"raw_msg_5">>}} -> ok
            after 300 ->
                ?assert(false)
            end,
            receive
                {race_pending_pid, {timeout, _Ref2, <<"raw_msg_5">>}} -> ok
            after 300 ->
                ?assert(false)
            end,
            receive
                {race_pending_pid,
                    {timeout, _Ref3, {[], {1, <<"did_pending">>, <<"msg_5">>}, <<"raw_msg_5">>}}} ->
                    ok
            after 500 ->
                ?assert(false)
            end,
            receive
                {race_ack_pid,
                    {timeout, _Ref4, {[], {1, <<"did_ack_race">>, <<"msg_5">>}, <<"raw_msg_5">>}}} ->
                    ?assert(false)
            after 250 ->
                ok
            end,
            ?assertEqual(
                2, get({ack_lookup_count, {ack_received, 1, <<"did_ack_race">>, <<"msg_5">>}})
            ),
            ?assertEqual(1, meck:num_calls(ack_retry_cache, set, 3)),
            ?assertEqual([{1, <<"did_pending">>, <<"msg_5">>}], get(test_timer_sets)),
            [PidAckRace, PidPending] = get(test_pids),
            exit(PidAckRace, kill),
            exit(PidPending, kill),
            erase(test_pids),
            erase(test_timer_sets),
            erase({ack_lookup_count, {ack_received, 1, <<"did_ack_race">>, <<"msg_5">>}})
        end
    ).
