-module(olm_identity_log_privacy_tests).
-include_lib("eunit/include/eunit.hrl").

database_failure_logs_do_not_expose_identity_or_reason_test_() ->
    Cases = [
        {find_identity, 2,
            fun() -> olm_identity_logic:get_identity(100, <<"synthetic-device">>) end,
            olm_get_identity_error},
        {list_devices_with_identity, 1, fun() -> olm_identity_logic:list_devices(100) end,
            olm_list_devices_error},
        {count_one_time_keys, 2,
            fun() -> olm_identity_logic:count_one_time_keys(100, <<"synthetic-device">>) end,
            olm_count_otk_error},
        {cleanup_consumed_one_time_keys, 1,
            fun() -> olm_identity_logic:cleanup_consumed_one_time_keys(30) end,
            olm_cleanup_otk_error}
    ],
    [
        ?_test(check_failure_log(Function, Arity, Call, Event))
     || {Function, Arity, Call, Event} <- Cases
    ].

check_failure_log(Function, Arity, Call, Event) ->
    Modules = [olm_identity_ds, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        Canary = #{uid => 100, device => <<"synthetic-device">>, key => <<"synthetic-key-canary">>},
        Stub =
            case Arity of
                1 -> fun(_) -> {error, Canary} end;
                2 -> fun(_, _) -> {error, Canary} end
            end,
        meck:expect(olm_identity_ds, Function, Arity, Stub),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        ?assertEqual({error, <<"internal_error">>}, Call()),
        ?assertEqual(1, meck:num_calls(elib_log, internal_log, [error, Event, '_', '_'])),
        ?assertEqual(1, meck:num_calls(elib_log, internal_log, 4)),
        ?assertEqual(1, meck:num_calls(olm_identity_ds, Function, Arity))
    after
        meck:unload(Modules)
    end.
