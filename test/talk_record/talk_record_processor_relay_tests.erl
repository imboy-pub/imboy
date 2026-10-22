-module(talk_record_processor_relay_tests).
-include_lib("eunit/include/eunit.hrl").

%% Executable fixtures prove local protocol/transactions only, never ASR,
%% diarization, summary factuality or model-quality acceptance.
processor_protocol_test_() ->
    {timeout, 180,
        {setup, fun setup/0, fun cleanup/1, fun(S) -> {timeout, 60, ?_test(run(S))} end}}.
setup() ->
    S = talk_record_t19_support:setup_all(),
    talk_record_setup_fixture:prepare(
        S,
        fun() ->
            {ok, _} = elib_pg:execute(
                maps:get(conn, S), <<"DELETE FROM talk_record_processor_run">>, []
            ),
            talk_record_t19_support:seed(S),
            Exe = fixture_executable(),
            Bundle = #{
                enabled => true,
                approved => true,
                version => <<"synthetic-protocol-v1">>,
                transcribe => #{
                    executable => Exe,
                    model_version => <<"synthetic-asr-not-model">>,
                    diarization => true
                },
                summarize => #{
                    executable => Exe, model_version => <<"synthetic-summary-not-model">>
                }
            },
            application:set_env(imboy, talk_record_processor, Bundle),
            S#{exe => Exe}
        end,
        fun talk_record_t19_support:teardown_all/1
    ).
cleanup(S) ->
    application:unset_env(imboy, talk_record_processor),
    file:delete(maps:get(exe, S)),
    talk_record_t19_support:teardown_all(S).

run(S) ->
    C = maps:get(conn, S),
    ?assertEqual(true, maps:get(<<"asr_enabled">>, talk_record_ds:capabilities())),
    {Rid, Assembly} = talk_record_assembly_relay_tests:fixture(
        C, talk_record_assembly_relay_tests:tone(), 971
    ),
    {ok, published} = talk_record_assembly_job:run(Assembly),
    {ok, J} = model_job(C, Rid, <<"transcribe">>, <<"summarize">>, 1),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='queued',lease_token=NULL,lease_owner=NULL,lease_until=NULL WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ),
    ?assertMatch(
        {ok, #{outcome := succeeded, kind := <<"transcribe">>}}, talk_record_job_worker:run_once()
    ),
    {ok, T} = talk_record_transcript_repo:find_current_tx(C, Rid, <<"t.id,t.version,t.status">>),
    ?assertEqual(1, maps:get(<<"version">>, T)),
    ?assertEqual(<<"ready">>, maps:get(<<"status">>, T)),
    {ok, [Speaker]} = talk_record_speaker_repo:list_by_record_tx(C, Rid),
    ?assertEqual(<<"anonymous">>, maps:get(<<"identity_source">>, Speaker)),
    ?assertEqual(null, maps:get(<<"matched_user_id">>, Speaker)),
    ?assertMatch(
        {ok, #{outcome := succeeded, kind := <<"summarize">>}}, talk_record_job_worker:run_once()
    ),
    {ok, Summary} = talk_record_summary_repo:find_by_record_tx(C, Rid),
    ?assertEqual(<<"draft">>, maps:get(<<"status">>, Summary)),
    [Decision] = maps:get(<<"decisions">>, Summary),
    ?assertMatch([_], maps:get(<<"segment_ids">>, Decision)),
    {ok, J2} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, 2),
    {ok, published} = talk_record_model_job:run(<<"transcribe">>, J2),
    {ok, T2} = talk_record_transcript_repo:find_current_tx(C, Rid, <<"t.version">>),
    ?assertEqual(2, maps:get(<<"version">>, T2)),
    {ok, Stale} = talk_record_summary_repo:find_by_record_tx(C, Rid),
    ?assertEqual(<<"stale">>, maps:get(<<"status">>, Stale)),
    rejects(C, Rid),
    runner_limits(),
    schema_contracts(),
    model_fences(C, Rid),
    no_speech(C, Rid),
    bad_summary(C, Rid),
    processing_entry(C, Rid),
    publish_cas(C, Rid),
    summary_edit_fence(C, Rid),
    actor_fence(C, Rid),
    heartbeat_fences(C, Rid),
    durable_unknown(C, Rid),
    ok.

model_job(C, Rid, Kind, Mode, N) ->
    Consent = #{
        policy_version => <<"talk-record-notice-v1">>,
        processor_config_version => talk_record_processor_config:version()
    },
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    {ok, Pin} = elib_pg:with_tx(fun(X) ->
        {ok, Id} = talk_record_job_support:record_consent_tx(X, 700001, Rid, Consent, Mode),
        {ok, talk_record_job_support:processor_pin(Id, Consent, Mode)}
    end),
    {ok, [#{<<"content_revision">> := Rev}]} = elib_pg:query(
        C, <<"SELECT content_revision FROM talk_record WHERE id=$1">>, [Rid]
    ),
    TV =
        case Kind of
            <<"summarize">> ->
                {ok, T} = talk_record_transcript_repo:find_current_tx(C, Rid, <<"t.version">>),
                maps:get(<<"version">>, T);
            _ ->
                null
        end,
    {ok, created, _} = elib_pg:with_tx(fun(X) ->
        talk_record_job_support:enqueue_job_tx(
            X,
            #{
                record_id => Rid,
                kind => Kind,
                processor_input => Pin,
                input_content_revision => Rev,
                input_transcript_version => TV,
                input_manifest_hash => maps:get(<<"manifest_hash">>, A),
                dedupe_key => <<"model-protocol:", (integer_to_binary(N))/binary>>
            }
        )
    end),
    claim().
claim() ->
    elib_pg:with_tx(fun(C) ->
        talk_record_job_repo:claim_next_tx(
            C,
            <<"synthetic-model-protocol">>,
            <<"11111111-2222-4333-8444-555555555555">>,
            300
        )
    end).

rejects(C, Rid) ->
    {ok, J} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, 3),
    {ok, Config} = talk_record_processor_config:get(<<"transcribe">>),
    {ok, _} = elib_pg:with_tx(fun(X) -> talk_record_model_job:prepare_tx(X, J, Config) end),
    {ok, 1} = elib_pg:execute(
        C, <<"UPDATE talk_record_job SET lease_until=now()-interval '1s' WHERE id=$1">>, [
            maps:get(<<"id">>, J)
        ]
    ),
    ?assertEqual({error, lease_lost}, talk_record_model_job:run(<<"transcribe">>, J)),
    cancel(C, J),
    {ok, Historical} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, 4),
    {ok, 1} = elib_pg:execute(
        C, <<"UPDATE talk_record_job SET processor_input=NULL WHERE id=$1">>, [
            maps:get(<<"id">>, Historical)
        ]
    ),
    ?assertEqual({error, input_stale}, talk_record_model_job:run(<<"transcribe">>, Historical)),
    {ok, [Old]} = elib_pg:query(C, <<"SELECT * FROM talk_record_job WHERE id=$1">>, [
        maps:get(<<"id">>, Historical)
    ]),
    ?assertEqual(
        {error, processing_consent_required}, talk_record_model_job:run(<<"transcribe">>, Old)
    ),
    {ok, 1} = elib_pg:execute(
        C, <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>, [
            maps:get(<<"id">>, Old)
        ]
    ).

runner_limits() ->
    {ok, Cfg} = talk_record_processor_config:get(<<"transcribe">>),
    ?assertEqual(
        {error, processor_timeout},
        talk_record_processor_runner:run(Cfg#{timeout_ms => 20}, #{<<"test_mode">> => <<"timeout">>})
    ),
    ?assertEqual(
        {error, processor_output_limit},
        talk_record_processor_runner:run(Cfg#{max_output_bytes => 100}, #{
            <<"test_mode">> => <<"large">>
        })
    ).
schema_contracts() ->
    {ok, C} = talk_record_processor_config:get(<<"summarize">>),
    Bad = #{
        <<"model_version">> => maps:get(model_version, C),
        <<"overview">> => <<"fixture">>,
        <<"decisions">> => [#{<<"text">> => <<"wrong record">>, <<"segment_ids">> => [999]}],
        <<"actions">> => []
    },
    ?assertEqual(
        {error, processor_schema_invalid},
        talk_record_processor_result:validate(<<"summarize">>, Bad, C, #{segment_ids => [1]})
    ).

model_fences(C, Rid) ->
    {ok, J} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, 5),
    {ok, Cfg} = talk_record_processor_config:get(<<"transcribe">>),
    {ok, _} = elib_pg:with_tx(fun(X) -> talk_record_model_job:prepare_tx(X, J, Cfg) end),
    {ok, Bundle} = application:get_env(imboy, talk_record_processor),
    application:set_env(imboy, talk_record_processor, Bundle#{version => <<"changed">>}),
    ?assertEqual(
        {error, processing_consent_required}, talk_record_model_job:run(<<"transcribe">>, J)
    ),
    application:set_env(imboy, talk_record_processor, Bundle),
    {ok, 1} = elib_pg:execute(
        C, <<"UPDATE talk_record SET content_revision=content_revision+1 WHERE id=$1">>, [Rid]
    ),
    ?assertEqual({error, input_stale}, talk_record_model_job:run(<<"transcribe">>, J)),
    cancel(C, J),
    {ok, J2} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, 6),
    {ok, 1} = elib_pg:execute(C, <<"UPDATE talk_record SET deleted_at=now() WHERE id=$1">>, [Rid]),
    ?assertEqual({error, record_deleted}, talk_record_model_job:run(<<"transcribe">>, J2)),
    {ok, 1} = elib_pg:execute(C, <<"UPDATE talk_record SET deleted_at=NULL WHERE id=$1">>, [Rid]),
    cancel(C, J2).

no_speech(C, Rid) ->
    {ok, B} = application:get_env(imboy, talk_record_processor),
    P = maps:get(transcribe, B),
    application:set_env(imboy, talk_record_processor, B#{
        transcribe => P#{model_version => <<"synthetic-nospeech">>}
    }),
    {ok, J} = model_job(C, Rid, <<"transcribe">>, <<"summarize">>, 7),
    ?assertEqual({ok, published}, talk_record_model_job:run(<<"transcribe">>, J)),
    {ok, T} = talk_record_transcript_repo:find_current_tx(C, Rid, <<"t.status">>),
    ?assertEqual(<<"no_speech">>, maps:get(<<"status">>, T)),
    {ok, [#{<<"summary_status">> := <<"not_requested">>}]} = elib_pg:query(
        C,
        <<"SELECT summary_status FROM talk_record WHERE id=$1">>,
        [Rid]
    ),
    application:set_env(imboy, talk_record_processor, B).

bad_summary(C, Rid) ->
    {ok, J} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, 8),
    {ok, published} = talk_record_model_job:run(<<"transcribe">>, J),
    {ok, B} = application:get_env(imboy, talk_record_processor),
    P = maps:get(summarize, B),
    application:set_env(imboy, talk_record_processor, B#{
        summarize => P#{model_version => <<"synthetic-badrefs">>}
    }),
    {ok, S} = model_job(C, Rid, <<"summarize">>, <<"summarize">>, 9),
    ?assertEqual({error, processor_schema_invalid}, talk_record_model_job:run(<<"summarize">>, S)),
    ok = talk_record_model_job:finish_failed(S, <<"processor_schema_invalid">>),
    {ok, [#{<<"state">> := <<"failed">>}]} = elib_pg:query(
        C, <<"SELECT state FROM talk_record_job WHERE id=$1">>, [maps:get(<<"id">>, S)]
    ),
    application:set_env(imboy, talk_record_processor, B).

cancel(C, J) ->
    %% These direct-phase fixture calls did not start an executor/Port.
    _ = talk_record_processor_run_repo:finish(J, <<"stopped">>),
    {ok, 1} = elib_pg:execute(
        C, <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>, [
            maps:get(<<"id">>, J)
        ]
    ),
    ok.

processing_entry(C, Rid) ->
    {ok, [#{<<"revision">> := Rev}]} = elib_pg:query(
        C, <<"SELECT revision FROM talk_record WHERE id=$1">>, [Rid]
    ),
    Body = #{
        <<"mode">> => <<"summarize">>,
        <<"processing_consent">> => #{
            <<"policy_version">> => <<"talk-record-notice-v1">>,
            <<"processor_config_version">> => talk_record_processor_config:version()
        }
    },
    {ok, created, P} = talk_record_job_logic:processing_request(
        700001, Rid, integer_to_binary(Rev), <<"processing-protocol-entry">>, Body
    ),
    Jid = maps:get(<<"job_id">>, P),
    {ok, [Row]} = elib_pg:query(
        C,
        <<"SELECT kind,processor_input,input_transcript_version FROM talk_record_job WHERE id=$1">>,
        [Jid]
    ),
    ?assertEqual(<<"summarize">>, maps:get(<<"kind">>, Row)),
    ?assert(is_integer(maps:get(<<"input_transcript_version">>, Row))),
    Pin = jsone:decode(maps:get(<<"processor_input">>, Row)),
    ?assert(is_integer(maps:get(<<"consent_event_id">>, Pin))),
    ?assertMatch(
        {ok, #{outcome := succeeded, kind := <<"summarize">>}}, talk_record_job_worker:run_once()
    ).

publish_cas(C, Rid) ->
    {ok, J} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, 10),
    {ok, Cfg} = talk_record_processor_config:get(<<"transcribe">>),
    {ok, P} = elib_pg:with_tx(fun(X) -> talk_record_model_job:prepare_tx(X, J, Cfg) end),
    Raw = #{
        <<"model_version">> => maps:get(model_version, Cfg),
        <<"status">> => <<"ready">>,
        <<"speakers">> => [#{<<"label">> => <<"S1">>}],
        <<"segments">> => [
            #{
                <<"start_ms">> => 0,
                <<"end_ms">> => 800,
                <<"speaker_label">> => <<"S1">>,
                <<"text">> => <<"synthetic CAS data">>
            }
        ]
    },
    {ok, V} = talk_record_processor_result:validate(<<"transcribe">>, Raw, Cfg, P),
    {ok, Before} = talk_record_transcript_repo:find_current_tx(C, Rid, <<"t.id,t.version">>),
    ok = meck:new(talk_record_job_repo, [passthrough, no_link]),
    meck:expect(talk_record_job_repo, finish_success_tx, fun(_, _, _) -> {error, lease_lost} end),
    try
        ?assertEqual(
            {rollback, {talk_model_abort, lease_lost}},
            elib_pg:with_tx(
                fun(X) -> talk_record_model_job:publish_tx(X, J, Cfg, V) end, [{reraise, false}]
            )
        )
    after
        meck:unload(talk_record_job_repo)
    end,
    ?assertEqual(
        {ok, Before}, talk_record_transcript_repo:find_current_tx(C, Rid, <<"t.id,t.version">>)
    ),
    cancel(C, J).

summary_edit_fence(C, Rid) ->
    {ok, J} = model_job(C, Rid, <<"summarize">>, <<"summarize">>, 11),
    {ok, Cfg} = talk_record_processor_config:get(<<"summarize">>),
    {ok, _} = elib_pg:with_tx(fun(X) -> talk_record_model_job:prepare_tx(X, J, Cfg) end),
    {ok, S} = talk_record_summary_repo:find_by_record_tx(C, Rid),
    {ok, _} = talk_record_summary_repo:update_tx(
        C,
        Rid,
        maps:get(<<"id">>, S),
        maps:get(<<"revision">>, S),
        #{<<"overview">> => <<"synthetic manual edit during model IO">>}
    ),
    ?assertEqual({error, input_stale}, talk_record_model_job:run(<<"summarize">>, J)),
    {ok, After} = talk_record_summary_repo:find_by_record_tx(C, Rid),
    ?assertEqual(<<"synthetic manual edit during model IO">>, maps:get(<<"overview">>, After)),
    cancel(C, J).

actor_fence(C, Rid) ->
    {ok, J} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, 12),
    {ok, 1} = elib_pg:execute(C, <<"UPDATE talk_record SET owner_user_id=700002 WHERE id=$1">>, [
        Rid
    ]),
    ?assertEqual(
        {error, processing_consent_required}, talk_record_model_job:run(<<"transcribe">>, J)
    ),
    {ok, 1} = elib_pg:execute(C, <<"UPDATE talk_record SET owner_user_id=700001 WHERE id=$1">>, [
        Rid
    ]),
    cancel(C, J).

durable_unknown(C, Rid) ->
    {ok, J} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, 23),
    {ok, Cfg} = talk_record_processor_config:get(<<"transcribe">>),
    {ok, _} = elib_pg:with_tx(fun(X) -> talk_record_model_job:prepare_tx(X, J, Cfg) end),
    ?assertEqual(
        {ok, <<"processor_cleanup_pending">>}, talk_record_delete_repo:write_blocker_tx(C, Rid)
    ),
    ok = talk_record_processor_run_repo:finish(J, <<"unknown">>),
    ?assertEqual(
        {ok, <<"processor_stop_unconfirmed">>}, talk_record_delete_repo:write_blocker_tx(C, Rid)
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='queued',lease_token=NULL,lease_until=NULL WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ),
    {ok, New} = claim(),
    ?assertEqual({error, lease_lost}, talk_record_job_repo:renew_lease_tx(C, J, 900)),
    ?assertEqual(
        {error, processor_cleanup_pending}, talk_record_model_job:run(<<"transcribe">>, New)
    ),
    %% Fixture never launched an OS process: explicitly acknowledge only that fixture attempt.
    ok = talk_record_processor_run_repo:finish(J, <<"stopped">>),
    ?assertEqual({ok, published}, talk_record_model_job:run(<<"transcribe">>, New)).

heartbeat_fences(C, Rid) ->
    {ok, B} = application:get_env(imboy, talk_record_processor),
    Root = os:getenv("TALK_UPLOAD_DISK_ROOT"),
    lists:foreach(
        fun({N, Action}) ->
            Barrier = filename:join(Root, "processor-barrier-" ++ integer_to_list(N)),
            ok = file:make_dir(Barrier),
            P = maps:get(transcribe, B),
            application:set_env(imboy, talk_record_processor, B#{
                transcribe => P#{args => [Barrier], heartbeat_ms => 50}
            }),
            {ok, J} = model_job(C, Rid, <<"transcribe">>, <<"transcribe">>, N),
            Parent = self(),
            Worker = spawn(fun() ->
                Parent ! {model_done, self(), talk_record_model_job:run(<<"transcribe">>, J)}
            end),
            Enter = filename:join(Barrier, "entered"),
            ok = await(fun() -> filelib:is_file(Enter) end),
            {ok, Data} = file:read_file(Enter),
            Meta = jsone:decode(Data),
            {ok, 1} = elib_pg:execute(
                C, <<"UPDATE talk_record_job SET lease_until=now()+interval '3s' WHERE id=$1">>, [
                    maps:get(<<"id">>, J)
                ]
            ),
            ok = await(fun() ->
                {ok, [#{<<"renewed">> := V}]} = elib_pg:query(
                    C,
                    <<"SELECT lease_until>now()+interval '800s' renewed FROM talk_record_job WHERE id=$1">>,
                    [maps:get(<<"id">>, J)]
                ),
                V
            end),
            case Action of
                expire ->
                    {ok, 1} = elib_pg:execute(
                        C,
                        <<"UPDATE talk_record_job SET lease_until=now()-interval '1s' WHERE id=$1">>,
                        [maps:get(<<"id">>, J)]
                    ),
                    receive
                        {model_done, Worker, R} -> ?assertEqual({error, lease_lost}, R)
                    after 5000 -> error(cancel_timeout)
                    end;
                parent_down ->
                    exit(Worker, kill)
            end,
            ok = await(fun() ->
                {ok, [#{<<"state">> := State}]} = elib_pg:query(
                    C,
                    <<"SELECT state FROM talk_record_processor_run WHERE job_id=$1 AND attempt=$2">>,
                    [maps:get(<<"id">>, J), maps:get(<<"attempt">>, J)]
                ),
                State =:= <<"stopped">>
            end),
            ?assertEqual(false, filelib:is_file(maps:get(<<"audio_path">>, Meta))),
            cancel(C, J),
            ok = file:delete(Enter),
            ok = file:del_dir(Barrier)
        end,
        [{21, expire}, {22, parent_down}]
    ),
    application:set_env(imboy, talk_record_processor, B).
await(Predicate) -> await(Predicate, erlang:monotonic_time(millisecond) + 5000).
await(Predicate, Deadline) ->
    case Predicate() of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true -> receive
                    after 10 -> await(Predicate, Deadline)
                    end;
                false -> error(observation_timeout)
            end
    end.

fixture_executable() ->
    Path = filename:join(os:getenv("TALK_UPLOAD_DISK_ROOT"), "synthetic-processor-protocol.py"),
    Script =
        <<"#!/usr/bin/python3\nimport sys,json,time,os\nx=json.loads(sys.stdin.readline())\nif len(sys.argv)>1:\n b=sys.argv[1]\n with open(os.path.join(b,'entered'),'w') as f: json.dump({'pid':os.getpid(),'audio_path':x.get('audio_path')},f)\n while not os.path.exists(os.path.join(b,'release')): time.sleep(0.01)\nif x.get('test_mode')=='timeout': time.sleep(30)\nif x.get('test_mode')=='large': print('x'*1000);sys.exit(0)\nif x['kind']=='transcribe':\n p=x['audio_path'];assert os.path.getsize(p)>0\n r={'model_version':x['model_version'],'status':'ready','speakers':[{'label':'S1'}],'segments':[{'start_ms':0,'end_ms':800,'speaker_label':'S1','text':'synthetic protocol data'}]}\n if 'nospeech' in x['model_version']: r.update(status='no_speech',speakers=[],segments=[])\nelse:\n r={'model_version':x['model_version'],'overview':'synthetic protocol summary','decisions':[{'text':'synthetic draft','segment_ids':[999 if 'badrefs' in x['model_version'] else x['segments'][0]['id']]}],'actions':[]}\nprint(json.dumps(r))\n">>,
    ok = file:write_file(Path, Script),
    ok = file:change_mode(Path, 8#700),
    Path.
