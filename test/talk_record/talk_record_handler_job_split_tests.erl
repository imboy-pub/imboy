-module(talk_record_handler_job_split_tests).
-include_lib("eunit/include/eunit.hrl").

%% Reject wrong HTTP methods before any DB/domain operation across moved domains.
method_dispatch_test_() ->
    {setup,
        fun() ->
            ok = meck:new(cowboy_req, [passthrough, unstick, no_link]),
            meck:expect(cowboy_req, reply, fun(Status, _Headers, _Body, Req) ->
                Req#{response_status => Status}
            end)
        end,
        fun(_) -> meck:unload(cowboy_req) end, fun(_) ->
            ?_test(begin
                Actions = [
                    collection,
                    capabilities,
                    creation_contexts,
                    item,
                    captures_collection,
                    capture_events,
                    capture_audio_parts,
                    capture_audio_part_confirm,
                    capture_recover,
                    capture_abandon,
                    marks_collection,
                    marks_item,
                    segments_collection,
                    segments_item,
                    speakers_collection,
                    speakers_item,
                    speaker_claim,
                    summary_item,
                    grants_collection,
                    grants_item,
                    share_card,
                    playback,
                    share_candidates,
                    transfer_candidates,
                    record_share_candidates,
                    exports_collection,
                    export_item,
                    export_download,
                    ownership_transfers,
                    complete,
                    jobs,
                    job_retry,
                    processing_requests
                ],
                lists:foreach(
                    fun(Action) ->
                        Req = talk_record_handler:handle_action(
                            Action, #{method => <<"OPTIONS">>}, #{current_uid => 700001}
                        ),
                        ?assertEqual(405, maps:get(response_status, Req))
                    end,
                    Actions
                ),
                {ok, InitReq, State} = talk_record_handler:init(
                    #{method => <<"OPTIONS">>},
                    #{action => complete, current_uid => 700001}
                ),
                ?assertEqual(405, maps:get(response_status, InitReq)),
                ?assertEqual(#{current_uid => 700001}, State)
            end)
        end}.

complete_parse_contract_test() ->
    Body = #{<<"captures">> => [#{<<"capture_id">> => 123, <<"last_seq">> => 0}]},
    ?assertEqual(
        {ok, #{captures => [#{capture_id => 123, last_seq => 0}], consent => undefined}},
        talk_record_job_logic:parse_complete(Body)
    ),
    ?assertEqual(
        {error, invalid_last_seq},
        talk_record_job_logic:parse_complete(
            #{<<"captures">> => [#{<<"capture_id">> => 123}]}
        )
    ).

processing_parse_contract_test() ->
    Body = #{
        <<"mode">> => <<"transcribe">>,
        <<"processing_consent">> =>
            #{
                <<"policy_version">> => <<"notice-v1">>,
                <<"processor_config_version">> => <<"provider-v1">>
            }
    },
    Expected =
        {ok, #{
            mode => <<"transcribe">>,
            consent =>
                #{policy_version => <<"notice-v1">>, processor_config_version => <<"provider-v1">>}
        }},
    ?assertEqual(Expected, talk_record_job_logic:parse_processing_request(Body)),
    ?assertEqual(Expected, talk_record_job_requests:parse_processing_request(Body)),
    ?assertEqual(
        {error, processing_consent_required},
        talk_record_job_logic:parse_processing_request(#{<<"mode">> => <<"summarize">>})
    ).

retry_policy_contract_test() ->
    ?assert(talk_record_job_logic:retryable_error(<<"timeout">>)),
    ?assertNot(talk_record_job_logic:retryable_error(<<"input_stale">>)),
    ?assert(talk_record_job_logic:kind_allowed_for_role(<<"assemble">>, owner)),
    ?assertNot(talk_record_job_logic:kind_allowed_for_role(<<"assemble">>, editor)).

module_size_test() ->
    Modules = [
        {api, [
            talk_record_handler,
            talk_record_capture_http,
            talk_record_content_http,
            talk_record_sharing_http,
            talk_record_processing_http,
            talk_record_http
        ]},
        {logic, [talk_record_job_logic, talk_record_job_requests, talk_record_job_support]}
    ],
    lists:foreach(
        fun({Dir, Names}) ->
            lists:foreach(
                fun(Name) ->
                    {ok, Body} = file:read_file(
                        filename:join(["src", atom_to_list(Dir), atom_to_list(Name) ++ ".erl"])
                    ),
                    ?assert(length(binary:split(Body, <<"\n">>, [global])) < 800)
                end,
                Names
            )
        end,
        Modules
    ).
