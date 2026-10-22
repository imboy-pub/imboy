-module(talk_record_job_execution_mode_tests).
-include_lib("eunit/include/eunit.hrl").

%% Pure SQL boundary tests: these do not claim PostgreSQL routing acceptance.
enqueue_modes_test_() ->
    [
        {binary_to_list(Kind), fun() ->
            with_mocks(fun() ->
                expect_insert(<<"server_local">>),
                ?assertEqual({ok, created, 31}, talk_record_job_repo:enqueue_tx(conn, job(Kind)))
            end)
        end}
     || Kind <- [<<"assemble">>, <<"export">>, <<"purge">>, <<"transcribe">>, <<"summarize">>]
    ] ++
        [
            {"device " ++ binary_to_list(Kind), fun() ->
                with_mocks(fun() ->
                    expect_insert(<<"client_byok">>),
                    ?assertEqual(
                        {ok, created, 31},
                        talk_record_job_repo:enqueue_tx(
                            conn, (job(Kind))#{execution_mode => <<"client_byok">>}
                        )
                    )
                end)
            end}
         || Kind <- [<<"transcribe">>, <<"summarize">>]
        ].

reject_unsupported_routes_before_storage_test_() ->
    [
        {lists:flatten(io_lib:format("~p/~p", [Kind, Mode])), fun() ->
            with_mocks(fun() ->
                ?assertEqual(
                    {error, invalid_job},
                    talk_record_job_repo:enqueue_tx(
                        conn, (job(Kind))#{execution_mode => Mode}
                    )
                ),
                ?assertEqual(0, meck:num_calls(elib_pg, query, '_')),
                ?assertEqual(0, meck:num_calls(elib_pg_sql, insert, '_'))
            end)
        end}
     || {Kind, Mode} <- [
            {<<"assemble">>, <<"client_byok">>},
            {<<"export">>, <<"client_byok">>},
            {<<"purge">>, <<"client_byok">>},
            {<<"transcribe">>, <<"unknown">>},
            {<<"transcribe">>, null},
            {<<"transcribe">>, client_byok},
            {<<"unknown">>, <<"server_local">>}
        ]
    ].

dedupe_cannot_reinterpret_execution_custody_test_() ->
    [
        {binary_to_list(Stored), fun() ->
            with_mocks(fun() ->
                meck:expect(elib_pg_sql, insert, fun(_, Data) ->
                    ?assertEqual(<<"client_byok">>, maps:get(<<"execution_mode">>, Data)),
                    {<<"INSERT">>, []}
                end),
                Row = #{<<"id">> => 31, <<"execution_mode">> => Stored},
                meck:expect(elib_pg, query, fun
                    (conn, <<"INSERT", _/binary>>, []) -> {ok, []};
                    (conn, <<"SELECT", _/binary>>, [<<"key">>]) -> {ok, [Row]}
                end),
                Expected =
                    case Stored of
                        <<"client_byok">> -> {ok, existing, Row};
                        _ -> {error, dedupe_execution_mode_conflict}
                    end,
                ?assertEqual(
                    Expected,
                    talk_record_job_repo:enqueue_tx(
                        conn, (job(<<"transcribe">>))#{execution_mode => <<"client_byok">>}
                    )
                )
            end)
        end}
     || Stored <- [<<"server_local">>, <<"client_byok">>]
    ].

server_worker_sql_fences_test_() ->
    Job = #{<<"id">> => 31, <<"attempt">> => 1, <<"lease_token">> => <<"token">>},
    Calls = [
        {claim, fun() ->
            talk_record_job_repo:claim_next_tx(conn, <<"worker">>, <<"token">>, 120)
        end},
        {renew, fun() -> talk_record_job_repo:renew_lease_tx(conn, Job, 120) end},
        {success, fun() -> talk_record_job_repo:finish_success_tx(conn, 31, <<"token">>) end},
        {failed, fun() ->
            talk_record_job_repo:finish_failed_tx(conn, 31, <<"token">>, <<"timeout">>)
        end},
        {retry, fun() ->
            talk_record_job_repo:finish_retry_tx(conn, 31, <<"token">>, <<"timeout">>, 5)
        end},
        {suspend, fun() ->
            talk_record_job_repo:finish_suspended_tx(conn, 31, <<"token">>, <<"waiting">>)
        end},
        {manual_retry, fun() -> talk_record_job_repo:retry_failed_tx(conn, 31) end},
        {unsuspend, fun() -> talk_record_job_repo:unsuspend_kind_tx(conn, <<"transcribe">>) end},
        {reap, fun() -> talk_record_job_repo:reap_expired_tx(conn) end}
    ],
    [
        {atom_to_list(Name), fun() ->
            with_mocks(fun() ->
                meck:expect(elib_pg, query, fun(conn, Sql, _Params) ->
                    case Sql of
                        <<"UPDATE", _/binary>> ->
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"execution_mode='server_local'">>)
                            );
                        <<"SELECT", _/binary>> ->
                            ok
                    end,
                    {ok, []}
                end),
                _ = Call(),
                ?assert(meck:validate(elib_pg))
            end)
        end}
     || {Name, Call} <- Calls
    ].

reaper_uses_current_time_after_transaction_start_test() ->
    with_mocks(fun() ->
        meck:expect(elib_pg, query, fun(conn, Sql, []) ->
            ?assertNotEqual(
                nomatch,
                binary:match(
                    Sql,
                    <<"lease_until <= clock_timestamp()">>
                )
            ),
            {ok, [#{<<"id">> => 31}]}
        end),
        ?assertEqual({ok, 1}, talk_record_job_repo:reap_expired_tx(conn))
    end).

cleanup_unblock_preserves_device_custody_test() ->
    with_mocks(fun() ->
        meck:expect(elib_pg, execute, fun(conn, Sql, []) ->
            ?assertNotEqual(
                nomatch, binary:match(Sql, <<"WHERE j.execution_mode='server_local'">>)
            ),
            ?assertNotEqual(
                nomatch, binary:match(Sql, <<"j.error_code='processor_cleanup_pending'">>)
            ),
            {ok, 0}
        end),
        ?assertEqual({ok, 0}, talk_record_processor_run_repo:unblock_waiting_tx(conn))
    end).

governance_cancellation_still_covers_both_modes_test_() ->
    Calls = [
        fun() -> talk_record_job_repo:cancel_job_tx(conn, 31) end,
        fun() -> talk_record_job_repo:cancel_record_jobs_tx(conn, 7) end
    ],
    [
        ?_test(
            with_mocks(fun() ->
                meck:expect(elib_pg, query, fun(conn, Sql, _) ->
                    ?assertEqual(nomatch, binary:match(Sql, <<"execution_mode='server_local'">>)),
                    {ok, [#{<<"id">> => 31}]}
                end),
                _ = Call()
            end)
        )
     || Call <- Calls
    ].

job(Kind) -> #{id => 31, record_id => 7, kind => Kind, dedupe_key => <<"key">>}.

expect_insert(Mode) ->
    meck:expect(elib_pg_sql, insert, fun(_, Data) ->
        ?assertEqual(Mode, maps:get(<<"execution_mode">>, Data)),
        {<<"INSERT">>, []}
    end),
    meck:expect(elib_pg, query, fun(conn, <<"INSERT", _/binary>>, []) ->
        {ok, [#{<<"id">> => 31}]}
    end).

with_mocks(Fun) ->
    Modules = [elib_pg, elib_pg_sql, elib_tsid, elib_dt],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(elib_pg_sql, public_tablename, fun(T) -> T end),
        meck:expect(elib_tsid, generate, fun(attachment) -> 31 end),
        meck:expect(elib_dt, now, fun() -> <<"synthetic-time">> end),
        Fun()
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.
