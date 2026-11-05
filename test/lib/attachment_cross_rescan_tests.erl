-module(attachment_cross_rescan_tests).
-include_lib("eunit/include/eunit.hrl").

rescan_ack_and_retry_test_() ->
    [
        ?_test(run_case(Result, Expected))
     || {Result, Expected} <- [{ok, true}, {{error, not_found}, true}, {{error, down}, false}]
    ].

run_case(Result, Expected) ->
    meck:new(elib_pg, [non_strict, no_link]),
    meck:new(attachment_storage, [non_strict, no_link]),
    put(claims, 0),
    try
        meck:expect(elib_pg, query, fun(_, Sql, Args) ->
            case binary:match(Sql, <<"claim">>) of
                {_, _} ->
                    case get(claims) of
                        0 ->
                            put(claims, 1),
                            [Token] = Args,
                            put(claim_token, Token),
                            {ok, [
                                #{
                                    <<"out_id">> => 7,
                                    <<"out_physical_key">> => <<"key">>,
                                    <<"out_driver">> => <<"garage">>,
                                    <<"out_namespace">> => <<"n">>,
                                    <<"out_container">> => <<"b">>,
                                    <<"out_locator">> => <<"l">>
                                }
                            ]};
                        _ ->
                            {ok, []}
                    end;
                nomatch ->
                    ?assertEqual([7, get(claim_token), Expected], Args),
                    {ok, [#{<<"state">> => <<"ready">>}]}
            end
        end),
        meck:expect(attachment_storage, frozen_location, fun(<<"key">>, _) -> {ok, #{}} end),
        ExpectedResult =
            case Expected of
                true -> {ok, 1};
                false -> {error, delete_failed}
            end,
        ?assertEqual(ExpectedResult, attachment_cross_rescan:run_batch(conn, fun(_) -> Result end))
    after
        erase(claims),
        erase(claim_token),
        meck:unload([elib_pg, attachment_storage])
    end.

claim_failure_does_not_delete_test() ->
    meck:new(elib_pg, [non_strict, no_link]),
    try
        meck:expect(elib_pg, query, fun(_, _, _) -> {error, unavailable} end),
        ?assertEqual(
            {error, unavailable},
            attachment_cross_rescan:run_batch(conn, fun(_) -> error(deleted) end)
        )
    after
        meck:unload(elib_pg)
    end.

supervisor_tick_dispatch_test() ->
    Env = application:get_env(imboy, pg_conf),
    User = os:getenv("IMBOY_DIRECT_CLEANUP_PG_USER"),
    Password = os:getenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD"),
    PasswordFile = os:getenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE"),
    Enabled = application:get_env(imboy, attachment_delete_worker_enabled),
    Parent = self(),
    meck:new(epgsql, [non_strict, no_link, passthrough]),
    meck:new(elib_pg, [non_strict, no_link]),
    meck:new(attachment_cross_rescan, [non_strict, no_link]),
    try
        os:putenv("IMBOY_DIRECT_CLEANUP_PG_USER", "fixture"),
        os:putenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD", "fixture"),
        os:unsetenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE"),
        application:set_env(imboy, pg_conf, #{start_mfa => {epgsql, connect, [#{}]}}),
        application:set_env(imboy, attachment_delete_worker_enabled, false),
        meck:expect(epgsql, connect, fun(_) -> {ok, conn} end),
        meck:expect(epgsql, close, fun(_) -> ok end),
        meck:expect(epgsql, squery, fun(conn, _) -> {ok, [], []} end),
        meck:expect(elib_pg, query, fun(conn, Sql, _) ->
            case binary:match(Sql, <<"AS safe">>) of
                {_, _} -> {ok, [#{<<"safe">> => true}]};
                nomatch -> {ok, []}
            end
        end),
        meck:expect(attachment_cross_rescan, run_batch, fun(conn, _) ->
            Parent ! reached,
            {ok, 0}
        end),
        [Spec] = attachment_delete_reaper:child_specs(),
        {Mod, Fun, Args} = maps:get(start, Spec),
        {ok, Pid} = apply(Mod, Fun, Args),
        try
            Pid ! tick,
            receive
                reached -> ok
            after 5000 -> error(tick_did_not_dispatch)
            end
        after
            gen_server:stop(Pid)
        end
    after
        meck:unload([epgsql, elib_pg, attachment_cross_rescan]),
        restore_env("IMBOY_DIRECT_CLEANUP_PG_USER", User),
        restore_env("IMBOY_DIRECT_CLEANUP_PG_PASSWORD", Password),
        restore_env("IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE", PasswordFile),
        restore_app_env(pg_conf, Env),
        restore_app_env(attachment_delete_worker_enabled, Enabled)
    end.

restore_env(Key, false) -> os:unsetenv(Key);
restore_env(Key, Value) -> os:putenv(Key, Value).
restore_app_env(Key, undefined) -> application:unset_env(imboy, Key);
restore_app_env(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).
