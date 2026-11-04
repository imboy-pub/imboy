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
    %% no-backend-ffmpeg V1：manifest_hash 与 final_audio 必填。
    Hash = hash64(),
    Body = #{
        <<"captures">> => [
            #{<<"capture_id">> => 123, <<"last_seq">> => 0, <<"manifest_hash">> => Hash}
        ],
        <<"final_audio">> => final_decl()
    },
    ?assertEqual(
        {ok, #{
            captures => [#{capture_id => 123, last_seq => 0, manifest_hash => Hash}],
            final_audio => final_parsed(),
            consent => undefined
        }},
        talk_record_job_logic:parse_complete(Body)
    ),
    ?assertEqual(
        {error, invalid_last_seq},
        talk_record_job_logic:parse_complete(
            #{<<"captures">> => [#{<<"capture_id">> => 123}]}
        )
    ).

complete_parse_mandatory_test() ->
    Hash = hash64(),
    CapH = #{<<"capture_id">> => 1, <<"last_seq">> => 0, <<"manifest_hash">> => Hash},
    %% manifest_hash 缺失/非 hex64 → invalid_manifest_hash。
    ?assertEqual(
        {error, invalid_manifest_hash},
        talk_record_job_logic:parse_complete(
            #{
                <<"captures">> => [#{<<"capture_id">> => 1, <<"last_seq">> => 0}],
                <<"final_audio">> => final_decl()
            }
        )
    ),
    ?assertEqual(
        {error, invalid_manifest_hash},
        talk_record_job_logic:parse_complete(
            #{
                <<"captures">> => [CapH#{<<"manifest_hash">> => <<"zz">>}],
                <<"final_audio">> => final_decl()
            }
        )
    ),
    %% final_audio 缺失/形态非法。
    ?assertEqual(
        {error, final_audio_required},
        talk_record_job_logic:parse_complete(#{<<"captures">> => [CapH]})
    ),
    ?assertEqual(
        {error, invalid_final_audio},
        talk_record_job_logic:parse_complete(
            #{
                <<"captures">> => [CapH],
                <<"final_audio">> => maps:remove(<<"channels">>, final_decl())
            }
        )
    ),
    %% channels 非 1（V1 单声道合同）与 client_upload_id 非 UUID。
    ?assertEqual(
        {error, invalid_final_audio},
        talk_record_job_logic:parse_complete(
            #{
                <<"captures">> => [CapH],
                <<"final_audio">> => final_decl(#{<<"channels">> => 2})
            }
        )
    ),
    ?assertEqual(
        {error, invalid_final_audio},
        talk_record_job_logic:parse_complete(
            #{
                <<"captures">> => [CapH],
                <<"final_audio">> =>
                    final_decl(#{<<"client_upload_id">> => <<"not-a-uuid">>})
            }
        )
    ).

final_decl() -> final_decl(#{}).
final_decl(Override) ->
    maps:merge(
        #{
            <<"object_key">> => <<"u1/talk_record/r1/final.aac">>,
            <<"client_upload_id">> => <<"0f0e0d0c-0b0a-49f8-8a7b-6c5d4e3f2a1b">>,
            <<"sha256">> => hash64(),
            <<"byte_size">> => 1024,
            <<"sample_rate">> => 16000,
            <<"channels">> => 1,
            <<"sample_count">> => 1024,
            <<"duration_ms">> => 64,
            %% PLAN T3 准入证明：attachment_id 为 parse_final_audio 必填字段。
            <<"attachment_id">> => 420001
        },
        Override
    ).

final_parsed() ->
    #{
        object_key => <<"u1/talk_record/r1/final.aac">>,
        client_upload_id => <<"0f0e0d0c-0b0a-49f8-8a7b-6c5d4e3f2a1b">>,
        sha256 => hash64(),
        byte_size => 1024,
        sample_rate => 16000,
        channels => 1,
        sample_count => 1024,
        duration_ms => 64,
        attachment_id => 420001
    }.

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

hash64() -> list_to_binary(lists:duplicate(64, $a)).
