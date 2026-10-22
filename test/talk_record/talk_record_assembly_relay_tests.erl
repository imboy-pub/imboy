-module(talk_record_assembly_relay_tests).
-include_lib("eunit/include/eunit.hrl").
-export([fixture/3, tone/0]).

assembly_pipeline_test_() ->
    {timeout, 180,
        {setup, fun setup/0, fun talk_record_t19_support:teardown_all/1, fun(S) ->
            ?_test(run(S))
        end}}.
setup() ->
    S = talk_record_t19_support:setup_all(),
    talk_record_setup_fixture:prepare(
        S,
        fun() ->
            talk_record_t19_support:seed(S),
            application:set_env(
                imboy,
                talk_record_render_tmp_dir,
                filename:join(os:getenv("TALK_UPLOAD_DISK_ROOT"), "render")
            ),
            S
        end,
        fun talk_record_t19_support:teardown_all/1
    ).

run(S) ->
    C = maps:get(conn, S),
    Bytes = tone(),
    {Rid, J} = fixture(C, Bytes, 951),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_job SET state='queued',lease_token=NULL,"
            "lease_owner=NULL,lease_until=NULL WHERE id=$1"
        >>,
        [maps:get(<<"id">>, J)]
    ),
    ?assertEqual({ok, #{outcome => succeeded}}, talk_record_job_worker:run_once()),
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    Aid = maps:get(<<"audio_attachment_id">>, A),
    ?assert(is_integer(Aid)),
    {ok, [R]} = elib_pg:query(
        C,
        <<
            "SELECT media_status,audio_manifest_attachment_id"
            " FROM talk_record WHERE id=$1"
        >>,
        [Rid]
    ),
    ?assertEqual(<<"available">>, maps:get(<<"media_status">>, R)),
    Mid = maps:get(<<"audio_manifest_attachment_id">>, R),
    ?assert(is_integer(Mid)),
    {ok, Mb} = attachment_derived_artifact:read(Mid),
    ?assertEqual(maps:get(<<"manifest_hash">>, A), maps:get(<<"manifest_hash">>, jsone:decode(Mb))),
    ?assertEqual(
        {ok, true},
        talk_record_assembly_artifact:with_audio(Aid, fun(Source) ->
            ?assert(attachment_upload_spool:size(Source) > 0),
            {ok, true}
        end)
    ),
    {ok, [#{<<"n">> := 2}]} = elib_pg:query(
        C,
        <<
            "SELECT count(*) n FROM talk_record_attachment_ref"
            " WHERE record_id=$1 AND role IN ('manifest','transcoded_audio')"
            " AND state='bound' AND lease_until IS NULL"
        >>,
        [Rid]
    ),
    ?assertEqual({error, lease_lost}, talk_record_assembly_job:run(J)),
    late_publish(C, Bytes, 952, lease),
    late_publish(C, Bytes, 953, delete),
    invalid_source(C, 954),
    finish_cas(C, Bytes, 955),
    duration_mismatch(C, Bytes, 956),
    corrupt_source(C, Bytes, 957),
    mixed_bad_frames(C, Bytes, 958),
    capabilities_limits(),
    failure_fences(C, Bytes, 960, lease),
    failure_fences(C, Bytes, 961, delete),
    failure_fences(C, Bytes, 962, input),
    attempt_retry(C, Bytes, 963),
    cancel_attempts(C, Bytes, 964),
    {ok, []} = file:list_dir(config_ds:env(imboy, talk_record_render_tmp_dir, "/tmp")),
    ok.

fixture(C, Bytes, N) ->
    {ok, Rid} = talk_record_t15_support:create_record(700001, N, <<"audio assembly">>),
    Cid = elib_tsid:generate(attachment),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO talk_record_capture(id,record_id,device_session_id,"
            "device_id,started_by,status) VALUES($1,$2,'11111111-2222-4333-8444-555555555555',"
            "'assembly-relay',700001,'ended')"
        >>,
        [Cid, Rid]
    ),
    {ok, Aid} = elib_pg:with_tx(fun(X) ->
        Nonce = binary:encode_hex(crypto:strong_rand_bytes(12), lowercase),
        attachment_derived_artifact:reserve_tx(
            X,
            #{job_id => N, attempt => 1, actor_id => 700001},
            #{
                <<"path">> => <<"assembly-source/", Nonce/binary>>,
                <<"url">> => <<>>,
                <<"name">> => <<"source.aac">>,
                <<"size">> => byte_size(Bytes),
                <<"mime_type">> => <<"audio/aac">>,
                <<"scope">> => <<"talk_record">>,
                <<"scope_ref">> => integer_to_binary(Rid),
                <<"origin">> => <<"test_audio">>,
                <<"file_hash256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase)
            }
        )
    end),
    ok = attachment_derived_artifact:write(Aid, Bytes),
    Pid = elib_tsid:generate(attachment),
    Hash = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO talk_record_audio_part(id,record_id,capture_id,seq,"
            "start_ms,end_ms,attachment_id,sha256,byte_size,codec,upload_status)"
            " VALUES($1,$2,$3,0,0,1000,$4,$5,$6,'aac','confirmed')"
        >>,
        [Pid, Rid, Cid, Aid, Hash, byte_size(Bytes)]
    ),
    {ok, Parts} = talk_record_assembly_logic:confirmed_parts_tx(C, Rid),
    Manifest = talk_record_assembly_logic:build_manifest(Cid, Parts),
    {ok, _, _} = elib_pg:with_tx(fun(X) ->
        talk_record_assembly_logic:persist_tx(X, Rid, Cid, Manifest)
    end),
    {ok, created, _} = elib_pg:with_tx(fun(X) ->
        talk_record_job_repo:enqueue_tx(
            X,
            #{
                record_id => Rid,
                kind => <<"assemble">>,
                input_manifest_hash => maps:get(<<"manifest_hash">>, Manifest),
                input_content_revision => 1,
                dedupe_key => <<"assembly-", (integer_to_binary(N))/binary>>
            }
        )
    end),
    {ok, J} = elib_pg:with_tx(fun(X) ->
        talk_record_job_repo:claim_next_tx(
            X,
            <<"assembly-relay">>,
            <<"11111111-2222-4333-8444-555555555555">>,
            300
        )
    end),
    {Rid, J}.

late_publish(C, B, N, Mode) ->
    {Rid, J} = fixture(C, B, N),
    {ok, P} = elib_pg:with_tx(fun(X) -> talk_record_assembly_job:prepare_tx(X, J) end),
    ok = talk_record_audio_render:with_audio(maps:get(parts, P), fun(Source) ->
        Mb = jsone:encode(maps:get(manifest, P)),
        {ok, Ready} = elib_pg:with_tx(fun(X) ->
            talk_record_assembly_job:reserve_tx(X, J, P, {Source, Mb})
        end),
        ok = talk_record_assembly_artifact:write(Ready, Source, Mb),
        Expected =
            case Mode of
                lease ->
                    {ok, 1} = elib_pg:execute(
                        C,
                        <<"UPDATE talk_record_job SET lease_until=now()-interval '1s' WHERE id=$1">>,
                        [maps:get(<<"id">>, J)]
                    ),
                    lease_lost;
                delete ->
                    {ok, 1} = elib_pg:execute(
                        C, <<"UPDATE talk_record SET deleted_at=now() WHERE id=$1">>, [Rid]
                    ),
                    record_deleted
            end,
        ?assertEqual({error, Expected}, talk_record_assembly_job:run(J)),
        Result = elib_pg:with_tx(fun(X) -> talk_record_assembly_job:publish_tx(X, J, Ready) end, [
            {reraise, false}
        ]),
        ?assertEqual({rollback, {talk_assembly_abort, Expected}}, Result),
        ok
    end),
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    ?assertEqual(null, maps:get(<<"audio_attachment_id">>, A)),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ).

invalid_source(C, N) ->
    {Rid, J} = fixture(C, <<"invalid AAC">>, N),
    ?assertEqual({error, audio_render_failed}, talk_record_assembly_job:run(J)),
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    ?assertEqual(null, maps:get(<<"audio_attachment_id">>, A)),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='queued',lease_token=NULL,lease_owner=NULL,lease_until=NULL WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ),
    ?assertEqual(
        {ok, #{outcome => failed, error_code => <<"audio_render_failed">>}},
        talk_record_job_worker:run_once()
    ),
    {ok, [#{<<"media_status">> := Status}]} = elib_pg:query(
        C, <<"SELECT media_status FROM talk_record WHERE id=$1">>, [Rid]
    ),
    ?assertEqual(<<"retryable_failed">>, Status).

finish_cas(C, B, N) ->
    {Rid, J} = fixture(C, B, N),
    ok = meck:new(talk_record_job_repo, [passthrough, no_link]),
    meck:expect(talk_record_job_repo, finish_success_tx, fun(_, _, _) -> {error, lease_lost} end),
    try
        ?assertEqual({error, lease_lost}, talk_record_assembly_job:run(J))
    after
        meck:unload(talk_record_job_repo)
    end,
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    ?assertEqual(null, maps:get(<<"audio_attachment_id">>, A)),
    {ok, [R]} = elib_pg:query(
        C,
        <<
            "SELECT audio_manifest_attachment_id,media_status"
            " FROM talk_record WHERE id=$1"
        >>,
        [Rid]
    ),
    ?assertEqual(null, maps:get(<<"audio_manifest_attachment_id">>, R)),
    ?assertNotEqual(<<"available">>, maps:get(<<"media_status">>, R)),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        C,
        <<
            "SELECT count(*) n FROM talk_record_attachment_ref"
            " WHERE record_id=$1 AND role IN ('manifest','transcoded_audio')"
            " AND lease_until IS NULL"
        >>,
        [Rid]
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ).

duration_mismatch(C, B, N) ->
    {Rid, J} = fixture(C, B, N),
    {ok, 1} = elib_pg:execute(
        C, <<"UPDATE talk_record_audio_part SET end_ms=5000 WHERE record_id=$1">>, [Rid]
    ),
    {ok, Parts} = talk_record_assembly_logic:confirmed_parts_tx(C, Rid),
    ?assertEqual(
        {error, audio_duration_mismatch},
        talk_record_audio_render:with_audio(Parts, fun(_) -> ok end)
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ).

corrupt_source(C, B, N) ->
    {Rid, J} = fixture(C, B, N),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "UPDATE attachment SET file_hash256=repeat('0',64)"
            " WHERE id=(SELECT attachment_id FROM talk_record_audio_part WHERE record_id=$1)"
        >>,
        [Rid]
    ),
    ?assertEqual({error, artifact_integrity_mismatch}, talk_record_assembly_job:run(J)),
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    ?assertEqual(null, maps:get(<<"audio_attachment_id">>, A)).

mixed_bad_frames(C, B, N) ->
    <<Head:100/binary, _:20/binary, Tail/binary>> = B,
    Damaged = <<Head/binary, (binary:copy(<<255>>, 20))/binary, Tail/binary>>,
    {_Rid, J} = fixture(C, Damaged, N),
    ?assertEqual({error, audio_render_failed}, talk_record_assembly_job:run(J)).

capabilities_limits() ->
    talk_record_t19_support:with_env(
        talk_record_capabilities,
        #{<<"max_duration_ms">> => 99999999, <<"max_recording_bytes">> => 9999999999},
        fun() ->
            Cap = talk_record_ds:capabilities(),
            ?assertEqual(7200000, maps:get(<<"max_duration_ms">>, Cap)),
            ?assertEqual(2147483648, maps:get(<<"max_recording_bytes">>, Cap))
        end
    ),
    talk_record_t19_support:with_env(
        talk_record_capabilities,
        #{<<"max_duration_ms">> => null},
        fun() ->
            ?assertEqual(false, maps:get(<<"recording_enabled">>, talk_record_ds:capabilities()))
        end
    ).

failure_fences(C, B, N, Mode) ->
    {Rid, J} = fixture(C, B, N),
    {ok, 1} = elib_pg:execute(
        C, <<"UPDATE talk_record SET media_status='assembling' WHERE id=$1">>, [Rid]
    ),
    Expected =
        case Mode of
            lease ->
                {ok, 1} = elib_pg:execute(
                    C,
                    <<"UPDATE talk_record_job SET lease_until=now()-interval '1s' WHERE id=$1">>,
                    [maps:get(<<"id">>, J)]
                ),
                {error, lease_lost};
            delete ->
                {ok, 1} = elib_pg:execute(
                    C, <<"UPDATE talk_record SET deleted_at=now() WHERE id=$1">>, [Rid]
                ),
                {ok, failed};
            input ->
                {ok, 1} = elib_pg:execute(
                    C, <<"UPDATE talk_record SET content_revision=2 WHERE id=$1">>, [Rid]
                ),
                {ok, failed}
        end,
    ?assertEqual(Expected, talk_record_assembly_job:fail(J, <<"audio_render_failed">>)),
    {ok, [#{<<"media_status">> := Status}]} = elib_pg:query(
        C, <<"SELECT media_status FROM talk_record WHERE id=$1">>, [Rid]
    ),
    ?assertEqual(<<"assembling">>, Status),
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ).

attempt_retry(C, B, N) ->
    {Rid, J1} = fixture(C, B, N),
    {ok, #{parts := Parts}} = elib_pg:with_tx(fun(X) ->
        talk_record_assembly_job:prepare_tx(X, J1)
    end),
    ok = elib_pg:with_tx(fun(X) ->
        talk_record_assembly_logic:release_job_leases_tx(X, J1, Parts)
    end),
    requeue(C, J1),
    {ok, J2} = claim(),
    ?assertEqual(maps:get(<<"attempt">>, J1) + 1, maps:get(<<"attempt">>, J2)),
    {ok, _} = elib_pg:with_tx(fun(X) -> talk_record_assembly_job:prepare_tx(X, J2) end),
    %% A late attempt-1 cleanup cannot release attempt-2 references.
    ok = elib_pg:with_tx(fun(X) ->
        talk_record_assembly_logic:release_job_leases_tx(X, J1, Parts)
    end),
    [P] = Parts,
    Key2 = talk_record_assembly_logic:lease_business_key(
        maps:get(<<"id">>, J2),
        maps:get(<<"attempt">>, J2),
        maps:get(<<"id">>, P)
    ),
    {ok, #{<<"state">> := <<"bound">>}} = talk_record_attachment_ref_repo:find_tx(
        C, Rid, <<"audio_part">>, Key2
    ),
    ok = elib_pg:with_tx(fun(X) ->
        talk_record_assembly_logic:release_job_leases_tx(X, J2, Parts)
    end),
    requeue(C, J2),
    ?assertEqual({ok, #{outcome => succeeded}}, talk_record_job_worker:run_once()),
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    ?assert(is_integer(maps:get(<<"audio_attachment_id">>, A))),
    Prefix = <<"assemble_job:", (integer_to_binary(maps:get(<<"id">>, J1)))/binary, ":%">>,
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        C,
        <<
            "SELECT count(*) n FROM talk_record_attachment_ref"
            " WHERE record_id=$1 AND business_key LIKE $2 AND state='bound'"
        >>,
        [Rid, Prefix]
    ).

cancel_attempts(C, B, N) ->
    {Rid, J1} = fixture(C, B, N),
    {ok, _} = elib_pg:with_tx(fun(X) -> talk_record_assembly_job:prepare_tx(X, J1) end),
    requeue(C, J1),
    {ok, J2} = claim(),
    {ok, _} = elib_pg:with_tx(fun(X) -> talk_record_assembly_job:prepare_tx(X, J2) end),
    ?assertEqual({ok, 1}, talk_record_job_logic:cancel_record_jobs(Rid)),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        C,
        <<
            "SELECT count(*) n FROM talk_record_attachment_ref"
            " WHERE record_id=$1 AND role='audio_part' AND lease_until IS NOT NULL AND state='bound'"
        >>,
        [Rid]
    ).

requeue(C, J) ->
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_job SET state='queued',lease_token=NULL,"
            " lease_owner=NULL,lease_until=NULL WHERE id=$1"
        >>,
        [maps:get(<<"id">>, J)]
    ),
    ok.
claim() ->
    elib_pg:with_tx(fun(X) ->
        talk_record_job_repo:claim_next_tx(
            X,
            <<"assembly-retry">>,
            <<"11111111-2222-4333-8444-555555555555">>,
            300
        )
    end).

tone() ->
    Dir = os:getenv("TALK_UPLOAD_DISK_ROOT"),
    Path = filename:join(Dir, "test-tone.aac"),
    P = open_port({spawn_executable, "/usr/local/bin/ffmpeg"}, [
        binary,
        exit_status,
        {args, [
            "-nostdin",
            "-v",
            "error",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=440:duration=1",
            "-ac",
            "1",
            "-ar",
            "16000",
            "-c:a",
            "aac",
            "-f",
            "adts",
            "-y",
            Path
        ]}
    ]),
    receive
        {P, {exit_status, 0}} -> ok
    after 10000 -> error(tone_timeout)
    end,
    {ok, B} = file:read_file(Path),
    ok = file:delete(Path),
    B.
