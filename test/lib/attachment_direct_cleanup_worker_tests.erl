-module(attachment_direct_cleanup_worker_tests).
-include_lib("eunit/include/eunit.hrl").

unavailable_database_keeps_worker_alive_test() ->
    OldConf = application:get_env(imboy, pg_conf),
    true = os:putenv("IMBOY_DIRECT_CLEANUP_PG_USER", "cleanup_test"),
    true = os:putenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD", "fixture-only"),
    ok = application:set_env(imboy, pg_conf, #{start_mfa => {epgsql, connect, [#{}]}}),
    meck:new(epgsql, [non_strict, no_link, passthrough]),
    try
        meck:expect(epgsql, connect, fun(_) -> error(noproc) end),
        {ok, Pid} = attachment_direct_cleanup_worker:start_link(),
        Pid ! tick,
        timer:sleep(50),
        ?assert(is_process_alive(Pid)),
        ?assertEqual(1, meck:num_calls(epgsql, connect, '_')),
        gen_server:stop(Pid)
    after
        meck:unload(epgsql),
        os:unsetenv("IMBOY_DIRECT_CLEANUP_PG_USER"),
        os:unsetenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD"),
        case OldConf of
            {ok, Conf} -> application:set_env(imboy, pg_conf, Conf);
            _ -> application:unset_env(imboy, pg_conf)
        end
    end.

missing_or_ambiguous_credentials_fail_closed_test() ->
    Keys = [
        "IMBOY_DIRECT_CLEANUP_PG_USER",
        "IMBOY_DIRECT_CLEANUP_PG_PASSWORD",
        "IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE"
    ],
    Old = [{Key, os:getenv(Key)} || Key <- Keys],
    try
        [os:unsetenv(Key) || Key <- Keys],
        ?assertEqual(
            {error, worker_credentials_missing}, attachment_direct_cleanup_worker:run_batch()
        ),
        os:putenv("IMBOY_DIRECT_CLEANUP_PG_USER", "fixture"),
        os:putenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD", "fixture"),
        os:putenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE", "/nonexistent"),
        ?assertEqual(
            {error, worker_credentials_missing}, attachment_direct_cleanup_worker:run_batch()
        )
    after
        [
            case Value of
                false -> os:unsetenv(Key);
                _ -> os:putenv(Key, Value)
            end
         || {Key, Value} <- Old
        ]
    end.
