-module(talk_record_provider_legacy_guard_tests).
-include_lib("eunit/include/eunit.hrl").
legacy_actions_blocked_before_idempotency_replay_test() ->
    Ms = [
        elib_pg,
        talk_record_client_gate,
        talk_record_client_repo,
        talk_record_client_policy,
        talk_record_idempotency_repo,
        talk_record_client_operations
    ],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Ms),
    try
        meck:expect(elib_pg, with_tx, fun(F, _) ->
            try
                F(conn)
            catch
                throw:E -> {rollback, E}
            end
        end),
        R = #{<<"id">> => 7, <<"scope">> => <<"personal">>},
        J = #{<<"id">> => 20, <<"record_id">> => 7},
        Run = #{
            <<"provider">> => <<"doubao">>,
            <<"policy_version">> => <<"p1">>,
            <<"config_version">> => hash()
        },
        meck:expect(talk_record_client_gate, record_tx, fun(_, _, 7, write) -> R end),
        meck:expect(talk_record_client_gate, mode, fun(_, _) -> ok end),
        meck:expect(talk_record_client_gate, run_tx, fun(_, _, _, _, _) -> ok end),
        meck:expect(talk_record_client_repo, lock_job_tx, fun(_, 7, 20) -> {ok, J} end),
        meck:expect(talk_record_client_repo, latest_tx, fun(_, 20) -> {error, not_found} end),
        meck:expect(talk_record_client_repo, lock_run_tx, fun(_, 7, 20, 30) -> {ok, Run} end),
        meck:expect(talk_record_client_policy, get, fun() ->
            {ok, #{public => #{<<"policy_version">> => <<"p1">>, <<"config_version">> => hash()}}}
        end),
        lists:foreach(
            fun(A) ->
                ?assertEqual(
                    {error, async_protocol_required},
                    talk_record_client_ds:execute(
                        A, #{uid => 10, did => <<"device">>}, 7, 20, 30, 0, {#{}, #{}}
                    )
                )
            end,
            [plan, dispatch, result, complete, settle]
        ),
        ?assertEqual(0, meck:num_calls(talk_record_idempotency_repo, record_tx, '_')),
        ?assertEqual(0, meck:num_calls(talk_record_client_operations, perform, '_'))
    after
        lists:foreach(fun meck:unload/1, Ms)
    end.
hash() -> binary:copy(<<"a">>, 64).
