-module(talk_record_playback_parts_limit_tests).
-include_lib("eunit/include/eunit.hrl").

bounded_and_release_test_() ->
    {timeout, 10, fun() ->
        Parent = self(),
        Hold = fun() ->
            talk_record_playback_parts_limit:with_slot(fun() ->
                Parent ! {acquired, self()},
                receive
                    release -> ok
                end
            end)
        end,
        Pids = [spawn(Hold) || _ <- lists:seq(1, 4)],
        try
            lists:foreach(
                fun(_) ->
                    receive
                        {acquired, _} -> ok
                    after 2000 -> error(timeout)
                    end
                end,
                Pids
            ),
            ?assertEqual(
                {error, playback_busy},
                talk_record_playback_parts_limit:with_slot(fun() -> leaked end)
            ),
            [Killed | Others] = Pids,
            Ref = monitor(process, Killed),
            exit(Killed, kill),
            receive
                {'DOWN', Ref, process, Killed, _} -> ok
            after 2000 -> error(timeout)
            end,
            ?assertEqual(ok, eventually_available(50)),
            ?assertError(
                test_failure,
                talk_record_playback_parts_limit:with_slot(fun() -> error(test_failure) end)
            ),
            ?assertEqual(ok, talk_record_playback_parts_limit:with_slot(fun() -> ok end)),
            [P ! release || P <- Others]
        after
            [exit(P, kill) || P <- Pids, is_process_alive(P)]
        end
    end}.

eventually_available(0) ->
    timeout;
eventually_available(N) ->
    case talk_record_playback_parts_limit:with_slot(fun() -> ok end) of
        {error, playback_busy} ->
            timer:sleep(20),
            eventually_available(N - 1);
        ok ->
            ok
    end.
