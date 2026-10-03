-module(olm_identity_log_privacy_tests).
-include_lib("eunit/include/eunit.hrl").

database_failure_logs_do_not_expose_identity_or_reason_test_() ->
    Cases = [
        {upsert_one_time_keys, 4,
            fun() ->
                olm_identity_logic:report_one_time_keys(
                    100,
                    <<"synthetic-device">>,
                    [{<<"synthetic-key-id">>, <<"synthetic-key">>}],
                    100
                )
            end,
            olm_report_otk_error},
        {upsert_fallback_key, 4,
            fun() ->
                olm_identity_logic:report_fallback_key(
                    100,
                    <<"synthetic-device">>,
                    <<"synthetic-key-id">>,
                    <<"synthetic-key">>
                )
            end,
            olm_report_fallback_error},
        {find_identity, 2,
            fun() ->
                olm_identity_logic:report_fallback_key(
                    100,
                    <<"synthetic-device">>,
                    <<"synthetic-key-id">>,
                    <<"synthetic-key">>,
                    <<"synthetic-signature">>
                )
            end,
            olm_fallback_identity_error},
        {find_identity, 2,
            fun() -> olm_identity_logic:claim_keys(100, 100, <<"synthetic-device">>) end,
            olm_claim_identity_error},
        {find_identity, 2,
            fun() ->
                olm_identity_logic:claim_keys(
                    100, 100, <<"synthetic-device">>, <<"synthetic-request">>
                )
            end,
            olm_claim_identity_error},
        {find_identity, 2,
            fun() ->
                olm_identity_logic:report_identity(
                    100,
                    <<"synthetic-device">>,
                    <<"synthetic-ed">>,
                    <<"synthetic-curve">>,
                    <<"synthetic-signature">>,
                    <<"android">>
                )
            end,
            olm_report_identity_lookup_error},
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
                2 -> fun(_, _) -> {error, Canary} end;
                4 -> fun(_, _, _, _) -> {error, Canary} end
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

identity_write_failure_logs_do_not_expose_identity_or_reason_test_() ->
    [
        ?_test(check_identity_write_log(Stage, Event))
     || {Stage, Event} <- [
            {bump, olm_identity_bump_error},
            {upsert, olm_report_identity_error},
            {audit, olm_rotation_event_failed}
        ]
    ].

check_identity_write_log(Stage, Event) ->
    Modules = [olm_identity_ds, user_device_ds, trust_audit_ds, elib_metric, elib_tsid, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
        Ed = base64:encode(Pub),
        Curve = base64:encode(<<"synthetic-new-curve">>),
        Sig = base64:encode(crypto:sign(eddsa, none, Curve, [Priv, ed25519])),
        Canary = #{uid => 100, key => <<"synthetic-key-canary">>},
        configure_identity_write_failure(Stage, Ed, Canary),
        Expected =
            case Stage of
                audit -> ok;
                _ -> {error, <<"internal_error">>}
            end,
        ?assertEqual(
            Expected,
            olm_identity_logic:report_identity(
                100, <<"synthetic-device">>, Ed, Curve, Sig, <<"android">>
            )
        ),
        ?assertEqual(1, meck:num_calls(elib_log, internal_log, [error, Event, '_', '_'])),
        ?assertEqual(1, meck:num_calls(elib_log, internal_log, 4)),
        Metrics =
            case Stage of
                audit -> 1;
                _ -> 0
            end,
        ?assertEqual(
            Metrics, meck:num_calls(elib_metric, increment, [olm_rotation_event_failed_total])
        )
    after
        meck:unload(Modules)
    end.

configure_identity_write_failure(Stage, Ed, Canary) ->
    meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
        {ok, #{<<"ed25519_key">> => Ed, <<"curve25519_key">> => <<"synthetic-old-curve">>}}
    end),
    meck:expect(user_device_ds, bump_identity_version, 2, fun(_, _) ->
        case Stage of
            bump -> {error, Canary};
            _ -> {ok, 2, 1}
        end
    end),
    meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) ->
        case Stage of
            upsert -> {error, Canary};
            _ -> {ok, 1}
        end
    end),
    meck:expect(trust_audit_ds, insert_event, 1, fun(_) -> {error, Canary} end),
    meck:expect(elib_tsid, generate, 1, fun(_) -> 990003 end),
    meck:expect(elib_metric, increment, 1, fun(_) -> ok end),
    meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end).
