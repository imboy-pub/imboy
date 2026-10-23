-module(talk_record_client_unprepared_tests).
-include_lib("eunit/include/eunit.hrl").
lost_claim_response_reissues_token_without_renewal_test() ->
    with_tx(fun() ->
        {ok, created, P} = recover(),
        ?assert(is_binary(maps:get(<<"run_token">>, P))),
        ?assertEqual(500, maps:get(<<"lease_expires_at_unix">>, maps:get(<<"run">>, P))),
        ?assertEqual(false, maps:get(<<"may_submit">>, P)),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, insert_run_tx, '_')),
        ?assertEqual(0, meck:num_calls(talk_record_client_repo, heartbeat_tx, '_'))
    end).
expired_safe_run_reaches_new_claim_only_after_expiry_test() ->
    with_tx(fun() ->
        put(r, (get(r))#{<<"lease_valid">> => false}),
        meck:expect(talk_record_client_repo, expire_tx, fun(conn, _) ->
            put(expired, true),
            {expired, <<"expired">>}
        end),
        meck:expect(talk_record_client_admission, claim_tx, fun(_, _, _) ->
            ?assertEqual(true, get(expired)),
            ok
        end),
        meck:expect(talk_record_client_repo, consent_tx, fun(_, _, _, _, _) ->
            throw({talk_client_abort, new_claim_boundary})
        end),
        ?assertEqual({error, new_claim_boundary}, recover())
    end).
any_durable_fact_blocks_before_new_claim_test() ->
    lists:foreach(
        fun(K) ->
            with_tx(fun() ->
                put(facts, (get(facts))#{K => true}),
                ?assertEqual({error, client_execution_conflict}, recover()),
                ?assertEqual(0, meck:num_calls(talk_record_client_repo, insert_run_tx, '_'))
            end)
        end,
        [<<"has_task">>, <<"has_provider_fact">>, <<"has_uncertain_run">>]
    ).
foreign_active_device_rejected_test() ->
    with_tx(fun() ->
        put(r, (get(r))#{<<"device_session_id">> => <<"foreign">>}),
        ?assertEqual({error, client_execution_conflict}, recover())
    end).
stale_etag_rejected_before_token_test() ->
    with_tx(fun() ->
        put(etag, <<"client-run:30:9">>),
        ?assertEqual({error, precondition_failed}, recover())
    end).
prepare_response_loss_discovers_task_without_new_grant_test() ->
    with_tx(fun() ->
        T = #{
            <<"id">> => 50,
            <<"source_run_id">> => 30,
            <<"job_id">> => 20,
            <<"state">> => <<"prepared">>,
            <<"revision">> => 1,
            <<"request_profile">> => talk_record_provider_contract:profile(),
            <<"assembly">> => #{},
            <<"source_authorization_id">> => null,
            <<"source_issued_at">> => null,
            <<"source_expires_at">> => null
        },
        meck:expect(talk_record_provider_repo, latest_task_tx, fun(conn, 20) -> {ok, T} end),
        {ok, read, P} = talk_record_client_recovery_ds:execute(latest, ctx(), 7, 20, {#{}, #{}}),
        ?assertEqual(50, maps:get(<<"id">>, maps:get(<<"task">>, P))),
        ?assertNot(maps:is_key(<<"run_token">>, P)),
        ?assertEqual({error, client_execution_conflict}, recover())
    end).
recover() ->
    talk_record_client_recovery_ds:execute(
        recover_unprepared,
        ctx(),
        7,
        20,
        {#{if_match => get(etag), idempotency_key => <<"fresh-recovery-action-1">>}, #{
            <<"processing_consent">> => consent()
        }}
    ).
with_tx(F) ->
    Mods = [
        talk_record_client_gate,
        talk_record_client_repo,
        talk_record_provider_repo,
        talk_record_client_recovery_repo,
        talk_record_client_admission,
        talk_record_idempotency_repo,
        elib_pg,
        elib_tsid
    ],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Mods),
    try
        setup(),
        F()
    after
        lists:foreach(fun meck:unload/1, Mods),
        lists:foreach(fun erase/1, [r, facts, etag, expired])
    end.
setup() ->
    Config = #{
        public => #{
            <<"policy_version">> => <<"p1">>,
            <<"config_version">> => hash(),
            <<"providers">> => #{
                <<"transcribe">> => #{
                    <<"provider">> => <<"doubao">>, <<"model">> => <<"volc.seedasr.auc">>
                }
            }
        },
        signing_key => binary:copy(<<"k">>, 32)
    },
    R = (talk_record_client_recovery_tests_run())#{
        <<"lease_token_hash">> => binary:encode_hex(
            crypto:hash(
                sha256,
                talk_record_client_policy:token(
                    maps:get(signing_key, Config), 30, 1, 10, <<"device">>
                )
            ),
            lowercase
        )
    },
    put(r, R),
    put(etag, <<"client-run:30:1">>),
    put(facts, #{
        <<"has_task">> => false, <<"has_provider_fact">> => false, <<"has_uncertain_run">> => false
    }),
    meck:expect(elib_pg, with_tx, fun(F, _) ->
        try
            F(conn)
        catch
            throw:{talk_client_abort, E} -> {rollback, {talk_client_abort, E}}
        end
    end),
    meck:expect(talk_record_client_gate, record_tx, fun(_, _, 7, _) -> #{<<"id">> => 7} end),
    meck:expect(talk_record_client_gate, pins_tx, fun(_, _, _) -> Config end),
    meck:expect(talk_record_client_gate, run_tx, fun(_, _, _, _, _) -> ok end),
    meck:expect(talk_record_client_repo, lock_job_tx, fun(conn, 7, 20) -> {ok, job()} end),
    meck:expect(talk_record_client_repo, latest_tx, fun(conn, 20) -> {ok, get(r)} end),
    meck:expect(talk_record_client_repo, lock_run_tx, fun(conn, 7, 20, 30) -> {ok, get(r)} end),
    meck:expect(talk_record_client_repo, parts_tx, fun(conn, 30) -> {ok, []} end),
    meck:expect(talk_record_provider_repo, latest_task_tx, fun(conn, 20) -> {error, not_found} end),
    meck:expect(talk_record_provider_repo, snapshot_tx, fun(conn, 7) ->
        {ok, #{<<"source_kind">> => <<"assembly">>}}
    end),
    meck:expect(talk_record_client_recovery_repo, facts_tx, fun(conn, 20) -> {ok, get(facts)} end),
    meck:expect(talk_record_idempotency_repo, record_tx, fun(_, _, _, _, _, _, _) ->
        {ok, inserted}
    end),
    meck:expect(talk_record_idempotency_repo, complete_tx, fun(_, _, _, _, _, _) -> ok end),
    meck:expect(elib_tsid, generate, fun(_) -> 100 end).
talk_record_client_recovery_tests_run() ->
    maps:merge(
        maps:with(
            [
                <<"input_manifest_hash">>,
                <<"input_content_revision">>,
                <<"input_transcript_version">>
            ],
            job()
        ),
        (maps:remove(<<"confirmed">>, consent()))#{
            <<"id">> => 30,
            <<"revision">> => 1,
            <<"attempt">> => 1,
            <<"actor_user_id">> => 10,
            <<"device_session_id">> => <<"device">>,
            <<"lease_valid">> => true,
            <<"state">> => <<"active">>,
            <<"lease_expires_at_unix">> => 500,
            <<"input_snapshot">> => #{<<"source_kind">> => <<"assembly">>}
        }
    ).
job() ->
    #{
        <<"id">> => 20,
        <<"attempt">> => 1,
        <<"kind">> => <<"transcribe">>,
        <<"execution_mode">> => <<"client_byok">>,
        <<"state">> => <<"running">>,
        <<"input_manifest_hash">> => hash(),
        <<"input_content_revision">> => 1,
        <<"input_transcript_version">> => null,
        <<"processor_input">> => maps:remove(<<"confirmed">>, consent())
    }.
consent() ->
    #{
        <<"provider">> => <<"doubao">>,
        <<"model">> => <<"volc.seedasr.auc">>,
        <<"execution_mode">> => <<"client_byok">>,
        <<"confirmed">> => true,
        <<"policy_version">> => <<"p1">>,
        <<"config_version">> => hash()
    }.
ctx() -> #{uid => 10, did => <<"device">>}.
hash() -> binary:copy(<<"a">>, 64).
