-module(e2ee_failure_metric_exposition_tests).
-include_lib("eunit/include/eunit.hrl").

failure_metrics_reach_exporter_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(check_metric(Name, Call, Expected))
         || {Name, Call, Expected} <- [
                {e2ee_recovery_failed_total,
                    fun() ->
                        e2ee_recovery_logic:start_auto_recovery(
                            100, <<"synthetic-device">>, <<"server_backup">>
                        )
                    end,
                    {error, <<"internal_error">>}},
                {group_e2ee_check_failed_total,
                    fun() ->
                        msg_c2g_logic:group_e2ee_gate(
                            42, <<"text">>, <<>>, null, <<"synthetic-body">>
                        )
                    end,
                    {error, <<"group_e2ee_check_failed">>}},
                {olm_identity_pop_rejected_total,
                    fun() ->
                        olm_identity_logic:report_identity(
                            100,
                            <<"synthetic-device">>,
                            <<"invalid-public-key">>,
                            <<"synthetic-curve">>,
                            <<"invalid-signature">>,
                            <<"android">>
                        )
                    end,
                    {error, <<"invalid_signature">>}},
                {olm_identity_rotation_rejected_total, fun rotation_rejected/0,
                    {error, <<"key_rotation_requires_old_key_proof">>}},
                {olm_rotation_event_failed_total, fun rotation_audit_failed/0, ok}
            ]
        ]
    end}.

setup() ->
    %% This isolated VM owns the real metric process; no metric mock or reset.
    {ok, Pid} = elib_metric:start_link(),
    unlink(Pid),
    Modules = [
        e2ee_backup_ds,
        group_ds,
        olm_identity_ds,
        user_device_ds,
        trust_audit_ds,
        elib_tsid,
        elib_log
    ],
    ok = meck:new(Modules, [passthrough, no_link]),
    meck:expect(e2ee_backup_ds, latest, 1, fun(_) -> {error, synthetic_db_failure} end),
    meck:expect(group_ds, e2ee_mode, 1, fun(_) -> {error, synthetic_db_failure} end),
    meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) -> {ok, not_found} end),
    meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
    {Pid, Modules}.

cleanup({Pid, Modules}) ->
    meck:unload(Modules),
    gen_server:stop(Pid).

check_metric(Name, Call, Expected) ->
    Before = counter(Name),
    ?assertEqual(Expected, Call()),
    Metrics = elib_metric:get_all_metrics(),
    ?assertEqual(Before + 1, maps:get(Name, maps:get(counters, Metrics))),
    Text = iolist_to_binary(metrics_handler:format_prometheus(Metrics)),
    TypeLine = iolist_to_binary(["# TYPE ", atom_to_binary(Name), " counter"]),
    ?assert(lists:member(TypeLine, binary:split(Text, <<"\n">>, [global]))),
    ExpectedLine = iolist_to_binary([atom_to_binary(Name), " ", integer_to_binary(Before + 1)]),
    ?assert(lists:member(ExpectedLine, binary:split(Text, <<"\n">>, [global]))),
    ?assertEqual(nomatch, binary:match(Text, <<"synthetic-device">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"synthetic-body">>)).

counter(Name) ->
    maps:get(Name, maps:get(counters, elib_metric:get_all_metrics()), 0).

rotation_rejected() ->
    meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
        {ok, #{
            <<"ed25519_key">> => <<"synthetic-old-root">>,
            <<"curve25519_key">> => <<"synthetic-old-curve">>
        }}
    end),
    try
        olm_identity_logic:report_identity(
            100,
            <<"synthetic-device">>,
            <<"synthetic-new-root">>,
            <<"synthetic-new-curve">>,
            <<"synthetic-signature">>,
            <<>>,
            <<"android">>
        )
    after
        restore_identity_lookup()
    end.

rotation_audit_failed() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    Ed = base64:encode(Pub),
    Curve = base64:encode(<<"synthetic-new-curve">>),
    Sig = base64:encode(crypto:sign(eddsa, none, Curve, [Priv, ed25519])),
    meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
        {ok, #{<<"ed25519_key">> => Ed, <<"curve25519_key">> => <<"synthetic-old-curve">>}}
    end),
    meck:expect(user_device_ds, bump_identity_version, 2, fun(_, _) -> {ok, 2, 1} end),
    meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) -> {ok, 1} end),
    meck:expect(trust_audit_ds, insert_event, 1, fun(_) -> {error, synthetic_db_failure} end),
    meck:expect(elib_tsid, generate, 1, fun(_) -> 990003 end),
    try
        Result = olm_identity_logic:report_identity(
            100,
            <<"synthetic-device">>,
            Ed,
            Curve,
            Sig,
            <<"android">>
        ),
        ?assertEqual(1, meck:num_calls(trust_audit_ds, insert_event, 1)),
        Result
    after
        restore_identity_lookup()
    end.

restore_identity_lookup() ->
    meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) -> {ok, not_found} end).
