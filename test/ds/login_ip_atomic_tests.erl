-module(login_ip_atomic_tests).
-include_lib("eunit/include/eunit.hrl").

ip_atomic_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(concurrent_counts_are_not_lost()) end,
        fun(_) -> ?_test(concurrent_limit_rejects_excess()) end,
        fun(_) -> ?_test(direct_failure_caller_stops_at_ip_limit()) end
    ]}.
setup() ->
    Old = application:get_env(imboy, login_max_ip_attempts_per_hour),
    application:set_env(imboy, login_max_ip_attempts_per_hour, 100),
    {ok, D} = depcache:start_link(c06_ip_cache, #{memory_max => 64}),
    unlink(D),
    {ok, A} = login_attempt_ds:start_link(),
    unlink(A),
    meck:new(imboy_cache, [no_link, passthrough]),
    meck:expect(imboy_cache, get, fun(Key) ->
        Value = depcache:get(Key, c06_ip_cache),
        timer:sleep(5),
        Value
    end),
    meck:expect(imboy_cache, set, fun(Key, Value, TTL) ->
        ?assertEqual(3600, TTL),
        depcache:set(Key, Value, TTL, [], c06_ip_cache)
    end),
    {D, A, Old}.
cleanup({D, A, Old}) ->
    try
        meck:unload(imboy_cache)
    after
        gen_server:stop(A),
        gen_server:stop(D),
        case Old of
            undefined -> application:unset_env(imboy, login_max_ip_attempts_per_hour);
            {ok, V} -> application:set_env(imboy, login_max_ip_attempts_per_hour, V)
        end
    end.
parallel(N, Ip) ->
    Parent = self(),
    Pids = [
        spawn(fun() ->
            receive
                go ->
                    Parent ! {self(), login_attempt_ds:check_ip_rate_limit(Ip)}
            end
        end)
     || _ <- lists:seq(1, N)
    ],
    [P ! go || P <- Pids],
    [
        receive
            {P, R} -> R
        after 3000 -> error(worker_timeout)
        end
     || P <- Pids
    ].
concurrent_counts_are_not_lost() ->
    Ip = <<"c06_parallel_ip">>,
    Results = parallel(40, Ip),
    ?assertEqual([{ok, I} || I <- lists:seq(1, 40)], lists:sort(Results)),
    ?assertEqual({ok, 40}, depcache:get({login_attempt_ip_limit, Ip}, c06_ip_cache)).
concurrent_limit_rejects_excess() ->
    application:set_env(imboy, login_max_ip_attempts_per_hour, 10),
    Results = parallel(40, <<"c06_limited_ip">>),
    ?assertEqual(10, length([ok || {ok, _} <- Results])),
    ?assertEqual(30, length([denied || {error, rate_limited} <- Results])),
    ?assertEqual([{ok, I} || I <- lists:seq(1, 10)], lists:sort([R || R = {ok, _} <- Results])),
    ?assertEqual(10, meck:num_calls(imboy_cache, set, 3)).
direct_failure_caller_stops_at_ip_limit() ->
    Ip = <<"c06_caller_ip">>,
    depcache:set({login_attempt_ip_limit, Ip}, 100, 3600, [], c06_ip_cache),
    ?assertEqual(
        {error, rate_limited},
        login_security_logic:record_login_failure(<<"c06_identifier">>, Ip)
    ),
    ?assertEqual({ok, 0}, login_attempt_ds:get_attempts(<<"c06_identifier">>, Ip)),
    ?assertEqual(0, meck:num_calls(imboy_cache, set, 3)).
