-module(talk_record_retry_outcome_tests).
-include_lib("eunit/include/eunit.hrl").

outcome_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(check(<<"export">>, {error, transient}, {error, db_down}, {error, db_down})),
            ?_test(
                check(
                    <<"export">>,
                    {error, transient},
                    {rollback, db_down},
                    {error, {retry_not_confirmed, db_down}}
                )
            ),
            ?_test(
                check(
                    <<"export">>,
                    {error, transient},
                    {error, lease_lost},
                    {ok, #{outcome => late_result_rejected}}
                )
            ),
            ?_test(
                check(
                    <<"export">>,
                    {error, transient},
                    {ok, retry_wait, <<"later">>},
                    {ok, #{outcome => requeued, kind => <<"export">>}}
                )
            ),
            ?_test(check(<<"assemble">>, {error, transient}, {error, db_down}, {error, db_down})),
            ?_test(
                check(
                    <<"assemble">>,
                    {error, transient},
                    {ok, retry_wait, <<"later">>},
                    {ok, #{outcome => requeued, reason => <<"transient">>}}
                )
            ),
            ?_test(check_terminal({error, db_down}, {error, db_down})),
            ?_test(check_terminal({error, lease_lost}, {ok, #{outcome => late_result_rejected}})),
            ?_test(check_terminal(ok, {ok, #{outcome => failed, error_code => forbidden}})),
            ?_test(check_exhausted({rollback, db_down}, {error, {finish_not_confirmed, db_down}})),
            ?_test(
                check_exhausted(
                    {error, {commit_not_confirmed, closed}}, {error, {commit_not_confirmed, closed}}
                )
            ),
            ?_test(check_purge({ok, cleaned}, {error, db_down}, {error, db_down})),
            ?_test(
                check_purge(
                    {ok, cleaned},
                    ok,
                    {ok, #{outcome => succeeded, kind => <<"purge">>, detail => cleaned}}
                )
            ),
            ?_test(check_purge({error, record_not_deleted}, {error, db_down}, {error, db_down})),
            ?_test(check_cancel({error, db_down}, {error, db_down})),
            ?_test(check_cancel({ok, already_terminal}, {ok, #{outcome => late_result_rejected}})),
            ?_test(
                check(
                    <<"assemble">>,
                    {error, artifact_write_unknown},
                    ok,
                    {ok, #{outcome => suspended, reason => artifact_write_unknown}}
                )
            ),
            ?_test(
                check(
                    <<"assemble">>,
                    {error, artifact_write_quarantined},
                    {error, db_down},
                    {error, db_down}
                )
            ),
            ?_test(
                check(
                    <<"assemble">>,
                    {error, {commit_not_confirmed, closed}},
                    ok,
                    {error, {commit_not_confirmed, closed}}
                )
            ),
            ?_test(
                check(
                    <<"assemble">>,
                    {error, {rollback_not_confirmed, busy, closed}},
                    ok,
                    {error, {rollback_not_confirmed, busy, closed}}
                )
            ),
            ?_test(
                check(
                    <<"assemble">>,
                    {error, legacy_staging_unresolved},
                    ok,
                    {ok, #{outcome => suspended, reason => legacy_staging_unresolved}}
                )
            )
        ]
    end}.

setup() ->
    meck:new([elib_pg, talk_record_export_job, talk_record_assembly_job], [passthrough, no_link]),
    ok.
cleanup(_) -> meck:unload([elib_pg, talk_record_export_job, talk_record_assembly_job]).

check(Kind, WorkResult, FinishResult, Expected) ->
    Job = #{
        <<"kind">> => Kind,
        <<"id">> => 77,
        <<"attempt">> => 1,
        <<"record_id">> => 88,
        <<"lease_token">> => <<"test">>
    },
    put(tx_calls, 0),
    meck:expect(elib_pg, with_tx, fun(_, _) ->
        case get(tx_calls) of
            0 ->
                put(tx_calls, 1),
                {ok, Job};
            _ ->
                FinishResult
        end
    end),
    meck:expect(talk_record_export_job, run, fun(_) -> WorkResult end),
    meck:expect(talk_record_assembly_job, run, fun(_) -> WorkResult end),
    ?assertEqual(Expected, talk_record_job_worker:run_once()).

check_terminal(FinishResult, Expected) ->
    Job = #{
        <<"kind">> => <<"export">>,
        <<"id">> => 77,
        <<"attempt">> => 1,
        <<"record_id">> => 88,
        <<"lease_token">> => <<"test">>
    },
    meck:expect(elib_pg, with_tx, fun(_, _) -> {ok, Job} end),
    meck:expect(talk_record_export_job, run, fun(_) -> {error, forbidden} end),
    meck:expect(talk_record_export_job, fail, fun(_, _) -> FinishResult end),
    ?assertEqual(Expected, talk_record_job_worker:run_once()).

check_exhausted(FinishResult, Expected) ->
    Job = #{
        <<"kind">> => <<"export">>,
        <<"id">> => 77,
        <<"attempt">> => 999,
        <<"record_id">> => 88,
        <<"lease_token">> => <<"test">>
    },
    meck:expect(elib_pg, with_tx, fun(_, _) -> {ok, Job} end),
    meck:expect(talk_record_export_job, fail, fun(_, _) -> FinishResult end),
    ?assertEqual(Expected, talk_record_job_worker:run_once()),
    ?assertEqual(Expected, talk_record_job_worker:run_batch(1)).

check_purge(PurgeResult, FinishResult, Expected) ->
    Job = #{
        <<"kind">> => <<"purge">>,
        <<"id">> => 77,
        <<"attempt">> => 1,
        <<"record_id">> => 88,
        <<"lease_token">> => <<"test">>
    },
    put(tx_calls, 0),
    meck:expect(elib_pg, with_tx, fun(_, _) ->
        case get(tx_calls) of
            0 ->
                put(tx_calls, 1),
                {ok, Job};
            1 ->
                put(tx_calls, 2),
                PurgeResult;
            _ ->
                FinishResult
        end
    end),
    ?assertEqual(Expected, talk_record_job_worker:run_once()).

check_cancel(FinishResult, Expected) ->
    Job = #{
        <<"kind">> => <<"assemble">>,
        <<"id">> => 77,
        <<"attempt">> => 1,
        <<"record_id">> => 88,
        <<"lease_token">> => <<"test">>
    },
    put(tx_calls, 0),
    meck:expect(elib_pg, with_tx, fun(_, _) ->
        case get(tx_calls) of
            0 ->
                put(tx_calls, 1),
                {ok, Job};
            _ ->
                FinishResult
        end
    end),
    meck:expect(talk_record_assembly_job, run, fun(_) -> {error, record_deleted} end),
    ?assertEqual(Expected, talk_record_job_worker:run_once()).
