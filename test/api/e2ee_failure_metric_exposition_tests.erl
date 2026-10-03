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
                    {error, <<"invalid_signature">>}}
            ]
        ]
    end}.

setup() ->
    %% This isolated VM owns the real metric process; no metric mock or reset.
    {ok, Pid} = elib_metric:start_link(),
    unlink(Pid),
    Modules = [e2ee_backup_ds, group_ds, olm_identity_ds, elib_log],
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
    ExpectedLine = iolist_to_binary([atom_to_binary(Name), " ", integer_to_binary(Before + 1)]),
    ?assert(lists:member(ExpectedLine, binary:split(Text, <<"\n">>, [global]))),
    ?assertEqual(nomatch, binary:match(Text, <<"synthetic-device">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"synthetic-body">>)).

counter(Name) ->
    maps:get(Name, maps:get(counters, elib_metric:get_all_metrics()), 0).
