-module(talk_record_assembly_relay_tests).
-include_lib("eunit/include/eunit.hrl").
-export([fixture/3, tone/0]).

assembly_pipeline_test_() ->
    {timeout, 600,
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
            %% V3-18 P2 三段式合同：audio_render part_fetch_item 只为
            %% garage 位置签 Worker GET（disk 位置 historical_location_
            %% unverified fail-closed）。relay 的 part 附件因此挂 garage
            %% registry（A0_ENDPOINT=v3run1），与 upload_namespaces 对齐。
            PrevGarage = application:get_env(imboy, garage),
            PrevRegistry = application:get_env(imboy, attachment_storage_registry),
            PrevPortTimeout = application:get_env(imboy, attachment_worker_port_timeout_ms),
            PrevInProcess = application:get_env(imboy, attachment_worker_in_process),
            ok = garage_registry_install(),
            %% W5b fork 楔死旁路：eunit VM 下 fork/exec 偶发楔死调度线程
            %% （取证见 attachment_worker_port:run_io_worker 注释），IO 段
            %% 切进程内等价路径；fork 类真链路由 G18 gate 承载。
            application:set_env(imboy, attachment_worker_in_process, true),
            %% 收割界保留（防御任意仍走 escript 的调用：端口等待 45s <
            %% 外层 eunit timeout 600s，挂死转化为有界成败判定）。
            application:set_env(imboy, attachment_worker_port_timeout_ms, 45000),
            S#{
                relay_prev_garage => PrevGarage,
                relay_prev_registry => PrevRegistry,
                relay_prev_port_timeout => PrevPortTimeout,
                relay_prev_in_process => PrevInProcess
            }
        end,
        fun relay_teardown/1
    ).

garage_registry_install() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    Ep = list_to_binary(os:getenv("A0_ENDPOINT")),
    Ak = list_to_binary(os:getenv("A0_AK")),
    Sk = list_to_binary(os:getenv("A0_SK")),
    ok = application:set_env(imboy, garage, #{
        endpoint => Ep,
        bucket => <<"imboy">>,
        region => <<"garage">>,
        access_key => Ak,
        secret_key => Sk
    }),
    Base = attachment_storage:registry(),
    application:set_env(imboy, attachment_storage_registry, Base#{
        roots => (maps:get(roots, Base, #{}))#{
            <<"talk-relay-garage">> => #{
                driver => <<"garage">>,
                namespace => <<"talk-relay-garage">>,
                endpoint => Ep,
                container => <<"imboy">>,
                access_key => Ak,
                secret_key => Sk,
                region => <<"garage">>
            }
        },
        upload_namespaces =>
            (maps:get(upload_namespaces, Base, #{}))#{<<"talk_record">> => <<"talk-relay-garage">>}
    }),
    ok.

relay_teardown(S) ->
    case maps:get(relay_prev_registry, S, undefined) of
        undefined -> application:unset_env(imboy, attachment_storage_registry);
        {ok, R} -> application:set_env(imboy, attachment_storage_registry, R)
    end,
    case maps:get(relay_prev_garage, S, undefined) of
        undefined -> application:unset_env(imboy, garage);
        {ok, G} -> application:set_env(imboy, garage, G)
    end,
    case maps:get(relay_prev_port_timeout, S, undefined) of
        undefined -> application:unset_env(imboy, attachment_worker_port_timeout_ms);
        {ok, T} -> application:set_env(imboy, attachment_worker_port_timeout_ms, T)
    end,
    case maps:get(relay_prev_in_process, S, undefined) of
        undefined -> application:unset_env(imboy, attachment_worker_in_process);
        {ok, V} -> application:set_env(imboy, attachment_worker_in_process, V)
    end,
    talk_record_t19_support:teardown_all(S).

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
    %% V3-18 P2 三段式迁移：中间段与 run/1 同构（Worker staging 上传拿
    %% proof → reserve_tx({Proof, ManifestBytes}) → write/2），旧 write/3
    %% + BEAM 字节哈希形态已退役。竞态注入不需要第二遍真渲染——gate 在
    %% prepare 期即拒绝（prepare_tx 已取，后续 run(J) 直接失败）；本体
    %% 字节复用 fixture 的 tone 文件（staging/copy 不参与后续断言）。
    {ok, PartsRows} = talk_record_assembly_logic:confirmed_parts_tx(
        C, maps:get(<<"record_id">>, J)
    ),
    _ = PartsRows,
    %% tone() 生成即删源文件（fixture 只留字节）；late_publish 本体字节
    %% 自落盘（内容不参与竞态断言，仅满足 Worker staging 上传的形态链）。
    OutPath = filename:join(os:getenv("TALK_UPLOAD_DISK_ROOT"), "late-publish-body.aac"),
    ok = file:write_file(OutPath, B),
    Mb = jsone:encode(maps:get(manifest, P)),
    ManifestFile = filename:join(os:getenv("TALK_UPLOAD_DISK_ROOT"), "manifest-relay.json"),
    ok = file:write_file(ManifestFile, Mb),
    AudioStaging = relay_staging_key(J, <<"audio">>),
    ManifestStaging = relay_staging_key(J, <<"manifest">>),
    T0 = erlang:monotonic_time(millisecond),
    {ok, #{sha256 := ASha, size := ASize}} =
        relay_staging_upload(OutPath, <<"audio/aac">>, AudioStaging),
    T1 = erlang:monotonic_time(millisecond),
    io:format("RELAY_T stage_upload_audio=~pms~n", [T1 - T0]),
    {ok, _} = relay_staging_upload(ManifestFile, <<"application/json">>, ManifestStaging),
    T2 = erlang:monotonic_time(millisecond),
    io:format("RELAY_T stage_upload_manifest=~pms~n", [T2 - T1]),
    {ok, Ready} = elib_pg:with_tx(fun(X) ->
        talk_record_assembly_job:reserve_tx(X, J, P, {{ASha, ASize}, Mb})
    end),
    ok = talk_record_assembly_artifact:write(
        Ready,
        #{
            audio_staging => AudioStaging,
            manifest_staging => ManifestStaging,
            audio_file => OutPath,
            manifest_file => ManifestFile
        }
    ),
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
    _ = file:delete(ManifestFile),
    _ = file:delete(OutPath),
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    ?assertEqual(null, maps:get(<<"audio_attachment_id">>, A)),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ).

%% 与 talk_record_assembly_job:staging_key/staging_upload 同构（未导出，
%% 测试本地复刻；键随机一次性，sweep 收敛残留）。
relay_staging_key(Job, Role) ->
    Jid = maps:get(<<"id">>, Job),
    Nonce = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"assembly-staging/", (integer_to_binary(Jid))/binary, "/", Role/binary, "/", Nonce/binary>>.

relay_staging_upload(Path, Mime, StagingKey) ->
    Cfg = application:get_env(imboy, garage, #{}),
    Parent = self(),
    Pid = spawn(fun() ->
        R =
            (catch attachment_worker_port:upload_file(#{
                src_path => Path,
                endpoint => maps:get(endpoint, Cfg),
                ak => maps:get(access_key, Cfg),
                sk => maps:get(secret_key, Cfg),
                bucket => maps:get(bucket, Cfg, <<"imboy">>),
                region => maps:get(region, Cfg, <<"garage">>),
                object_key => StagingKey,
                mime => Mime
            })),
        io:format("RELAY_T staged ~ts -> ~200p~n", [StagingKey, R]),
        Parent ! {staged, R}
    end),
    receive
        {staged, R} -> R
    after 45000 -> erlang:error({staging_timeout, StagingKey, Pid})
    end.

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
    %% V3-18 P2 三段式：字节完整性锚 = talk_record_audio_part.sha256
    %% （Worker fetch 逐项 size+sha256 校验，任一不符 fail-closed；
    %% attachment.file_hash256 只是元数据，不再作 fetch 锚）。锚值破坏
    %% 在 prepare 期即被 fence（manifest 重验不一致 → input_stale，
    %% 不再进 fetch 段）；fetch 段错 sha fail-closed 由
    %% assembly_io_worker_tests（真 Garage）覆盖。
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_audio_part SET sha256=repeat('0',64) WHERE record_id=$1">>,
        [Rid]
    ),
    ?assertEqual({error, input_stale}, talk_record_assembly_job:run(J)),
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
