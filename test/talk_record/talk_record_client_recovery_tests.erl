-module(talk_record_client_recovery_tests).
-include_lib("eunit/include/eunit.hrl").
whole_job_any_side_effect_fails_closed_test() ->
    ?assert(talk_record_client_recovery_contract:safe(facts())),
    lists:foreach(
        fun(K) -> ?assertNot(talk_record_client_recovery_contract:safe((facts())#{K => true})) end,
        maps:keys(facts())
    ),
    ?assertNot(talk_record_client_recovery_contract:safe(#{})).
latest_lost_claim_is_discoverable_without_secrets_test() ->
    V = talk_record_client_recovery_contract:view(run(), [], null, ctx(), facts()),
    ?assertEqual([<<"recover_unprepared">>], maps:get(<<"allowed_actions">>, V)),
    ?assertNot(maps:is_key(<<"run_token">>, V)),
    ?assertNot(maps:is_key(<<"lease_token_hash">>, maps:get(<<"run">>, V))).
foreign_active_device_cannot_recover_test() ->
    V = talk_record_client_recovery_contract:view(
        run(), [], null, #{uid => 10, did => <<"other">>}, facts()
    ),
    ?assertEqual(<<"device_busy">>, maps:get(<<"recovery_status">>, V)),
    ?assertEqual([], maps:get(<<"allowed_actions">>, V)).
prepare_response_lost_returns_existing_task_test() ->
    T = task(),
    V = talk_record_client_recovery_contract:view(run(), [], T, ctx(), (facts())#{
        <<"has_task">> => true
    }),
    ?assertEqual(50, maps:get(<<"id">>, maps:get(<<"task">>, V))),
    ?assertEqual(<<"task_available">>, maps:get(<<"recovery_status">>, V)),
    ?assertNot(lists:member(<<"recover_unprepared">>, maps:get(<<"allowed_actions">>, V))).
query_run_origin_is_distinct_test() ->
    R = (run())#{
        <<"id">> => 40, <<"execution_purpose">> => <<"query_only">>, <<"provider_task_id">> => 50
    },
    V = talk_record_client_recovery_contract:view(R, [], task(), ctx(), facts()),
    ?assertEqual(40, maps:get(<<"id">>, maps:get(<<"run">>, V))),
    ?assertEqual(30, maps:get(<<"source_run_id">>, maps:get(<<"task">>, V))),
    ?assertEqual(50, maps:get(<<"provider_task_id">>, maps:get(<<"run">>, V))).
terminal_tasks_remain_readonly_test() ->
    lists:foreach(
        fun(T) ->
            V = talk_record_client_recovery_contract:view(run(), [], T, ctx(), facts()),
            ?assertEqual([], maps:get(<<"allowed_actions">>, V))
        end,
        [(task())#{<<"state">> => <<"published">>}, (task())#{<<"cancelled_at">> => 1}]
    ).
expired_safe_is_recoverable_but_unknown_never_is_test() ->
    R = (run())#{<<"lease_valid">> => false},
    ?assertEqual(
        [<<"recover_unprepared">>],
        maps:get(
            <<"allowed_actions">>,
            talk_record_client_recovery_contract:view(R, [], null, ctx(), facts())
        )
    ),
    ?assertEqual(
        [],
        maps:get(
            <<"allowed_actions">>,
            talk_record_client_recovery_contract:view(R, [], null, ctx(), (facts())#{
                <<"has_uncertain_run">> => true
            })
        )
    ).
route_literals_precede_run_id_test() ->
    Routes = talk_record_routes:routes(),
    Names = [P || {P, _, _} <- Routes],
    Base = "/api/v1/talk-records/:id/jobs/:job_id/client-executions/",
    Pos = fun(P) -> length(lists:takewhile(fun(X) -> X =/= P end, Names)) end,
    ?assert(Pos(Base ++ "latest") < Pos(Base ++ ":run_id")),
    ?assert(Pos(Base ++ "recover-unprepared") < Pos(Base ++ ":run_id")).
facts() ->
    #{<<"has_task">> => false, <<"has_provider_fact">> => false, <<"has_uncertain_run">> => false}.
ctx() -> #{uid => 10, did => <<"device">>}.
run() ->
    #{
        <<"id">> => 30,
        <<"revision">> => 1,
        <<"actor_user_id">> => 10,
        <<"device_session_id">> => <<"device">>,
        <<"state">> => <<"active">>,
        <<"lease_valid">> => true,
        <<"lease_expires_at_unix">> => 500,
        <<"execution_purpose">> => <<"submit">>,
        <<"provider_task_id">> => null,
        <<"lease_token_hash">> => <<"secret">>
    }.
task() ->
    #{
        <<"id">> => 50,
        <<"source_run_id">> => 30,
        <<"job_id">> => 20,
        <<"state">> => <<"prepared">>,
        <<"revision">> => 1,
        <<"provider_task_id">> => null,
        <<"request_profile">> => talk_record_provider_contract:profile(),
        <<"assembly">> => #{},
        <<"source_authorization_id">> => null,
        <<"source_issued_at">> => null,
        <<"source_expires_at">> => null,
        <<"policy_version">> => <<"p1">>,
        <<"config_version">> => binary:copy(<<"a">>, 64)
    }.
