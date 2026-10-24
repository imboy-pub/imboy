-module(talk_record_wiring_tests).
-include_lib("eunit/include/eunit.hrl").

production_routes_test() ->
    Routes = talk_record_routes:routes(),
    ?assertEqual(length(Routes), length(lists:usort([P || {P, _, _} <- Routes]))),
    ?assertMatch([_], cowboy_router:compile([{'_', Routes}])),
    Expected = [
        {"complete", complete},
        {"jobs", jobs},
        {"jobs/:job_id/retry", job_retry},
        {"processing-requests", processing_requests},
        {"exports", exports_collection},
        {"exports/:export_id", export_item},
        {"exports/:export_id/download", export_download},
        {"ownership-transfers", ownership_transfers}
    ],
    lists:foreach(
        fun({Suffix, Action}) ->
            ?assertEqual(
                {"/api/v1/talk-records/:id/" ++ Suffix, talk_record_handler, #{action => Action}},
                lists:keyfind("/api/v1/talk-records/:id/" ++ Suffix, 1, Routes)
            )
        end,
        Expected
    ),
    ?assertEqual(Routes, 'talk_record_routes.fragment':routes()),
    %% Only this exact signed-capability source tuple bypasses JWT. Its actual
    %% capability/expiry/identity protections are verified by the real HTTP suite.
    CapabilitySource =
        {"/api/v1/talk-records/client-audio-source", talk_record_client_audio_handler, #{}},
    Open = imboy_router:open(),
    ?assertEqual([CapabilitySource], [
        Route
     || {P, _, _} = Route <- Routes,
        lists:member(list_to_binary(P), Open)
    ]),
    ?assertEqual([], [
        P
     || {P, _, _} = Route <- Routes,
        Route =/= CapabilitySource,
        lists:member(list_to_binary(P), Open)
    ]).

processing_dispatch_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(begin
                ok = meck:expect(
                    talk_record_job_logic,
                    complete,
                    fun(7, 10, <<"record:1">>, <<"key">>, #{}) ->
                        {ok, created, #{<<"job_id">> => 11}}
                    end
                ),
                R = request(complete, <<"POST">>),
                ?assertEqual(202, maps:get(status, R))
            end),
            ?_test(begin
                ok = meck:expect(
                    talk_record_job_logic,
                    complete,
                    fun(_, _, _, _, _) ->
                        {error,
                            {missing_sequences, #{
                                <<"capture_id">> => 20, <<"missing">> => [2], secret => ignored
                            }}}
                    end
                ),
                R = request(complete, <<"POST">>),
                ?assertEqual(409, maps:get(status, R)),
                ?assertEqual(#{<<"capture_id">> => 20, <<"missing">> => [2]}, maps:get(body, R))
            end),
            ?_test(begin
                ok = meck:expect(
                    talk_record_job_logic,
                    list_jobs,
                    fun(7, 10) -> {ok, #{<<"jobs">> => []}} end
                ),
                ?assertEqual(200, maps:get(status, request(jobs, <<"GET">>)))
            end),
            ?_test(begin
                ok = meck:expect(
                    talk_record_job_logic,
                    retry_job,
                    fun(7, 10, 12, <<"record:1">>, <<"key">>) ->
                        {ok, replay, #{<<"job_id">> => 12}}
                    end
                ),
                R = request(job_retry, <<"POST">>),
                ?assertEqual(202, maps:get(status, R)),
                ?assertEqual(<<"true">>, maps:get(<<"idempotent-replayed">>, R))
            end),
            ?_test(begin
                ok = meck:expect(
                    talk_record_job_logic,
                    processing_request,
                    fun(7, 10, _, _, _) -> {error, processing_unavailable} end
                ),
                ?assertEqual(503, maps:get(status, request(processing_requests, <<"POST">>)))
            end),
            ?_test(?assertEqual(405, maps:get(status, request(complete, <<"GET">>))))
        ]
    end}.

request(Action, Method) ->
    talk_record_handler:handle_action(Action, #{method => Method}, #{current_uid => 7}).
setup() ->
    lists:foreach(
        fun(M) -> ok = meck:new(M, [non_strict, no_link]) end,
        [cowboy_req, elib_response, elib_param, elib_tsid, elib_dt, elib_cnv, talk_record_job_logic]
    ),
    meck:expect(cowboy_req, method, fun(#{method := M}) -> M end),
    meck:expect(cowboy_req, binding, fun(id, _, _) -> <<"10">> end),
    meck:expect(cowboy_req, bindings, fun(_) -> #{id => <<"10">>, job_id => <<"12">>} end),
    meck:expect(cowboy_req, header, fun
        (<<"if-match">>, _) -> <<"record:1">>;
        (<<"idempotency-key">>, _) -> <<"key">>
    end),
    meck:expect(cowboy_req, set_resp_header, fun(K, V, R) -> R#{K => V} end),
    meck:expect(cowboy_req, reply, fun(Status, _, Body, R) -> R#{status => Status, body => Body} end),
    meck:expect(elib_response, success, fun(R, B) -> R#{status => 200, body => B} end),
    meck:expect(elib_response, error_with_status, fun(R, Status, _, _) -> R#{status => Status} end),
    meck:expect(elib_response, error_with_status, fun(R, Status, _, Body, _) ->
        R#{status => Status, body => Body}
    end),
    meck:expect(elib_dt, millisecond, fun() -> 1 end),
    meck:expect(elib_cnv, convert_at_timestamps, fun(B) -> B end),
    meck:expect(elib_param, post, fun(_) -> #{} end),
    meck:expect(elib_tsid, from_binary, fun(B) -> {ok, binary_to_integer(B)} end),
    ok.
cleanup(_) ->
    lists:foreach(
        fun meck:unload/1,
        [cowboy_req, elib_response, elib_param, elib_tsid, elib_dt, elib_cnv, talk_record_job_logic]
    ).
