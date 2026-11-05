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
            %% no-backend-ffmpeg V1：事务外校验经 frozen_location 真读成品
            %% 对象（garage 位置）。relay 的 part/final 附件因此挂 garage
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
    %% 成品 = 真实客户端直传附件（audio_attachment_id 指向它，且建
    %% role='transcoded_audio' 永久引用（既有类别，指向直传成品）），服务端不再产出派生音频副本。
    ?assertEqual(Aid, final_attachment_id(C, Rid)),
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
    final_asset_mismatch(C, Bytes, 956),
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

final_attachment_id(C, Rid) ->
    %% attachment 表无 origin 列（夹具 meta 的 origin 键不落库）——以
    %% derived_attachment 的物理 path 前缀标识 final 成品行。
    {ok, [#{<<"id">> := Aid}]} = elib_pg:query(
        C,
        <<
            "SELECT id FROM attachment WHERE scope='talk_record' AND scope_ref=$1"
            " AND path LIKE 'assembly-final/%'"
        >>,
        [integer_to_binary(Rid)]
    ),
    Aid.

%% 组装链 fixture：源段附件 + 客户端成品直传附件（同字节流、独立物理
%% 对象、origin='test_final'）+ job 冻结 final_audio 输入（complete 事务
%% 准入形态）。Bytes 即成品字节（单段：源段明文 == 成品）。
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
    {ok, Aid} = derived_attachment(N, Rid, <<"assembly-source">>, <<"test_audio">>, Bytes),
    {ok, AidFinal} = derived_attachment(N, Rid, <<"assembly-final">>, <<"test_final">>, Bytes),
    %% 派生附件行 reserve 即默认 'live'（00000178 列默认）——UPDATE 固化意图并断言两行。
    {ok, 2} = elib_pg:execute(
        C,
        <<"UPDATE attachment SET lifecycle_state='live' WHERE id IN ($1,$2)">>,
        [Aid, AidFinal]
    ),
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
    {ok, [#{<<"path">> := FinalPath}]} = elib_pg:query(
        C, <<"SELECT path FROM attachment WHERE id=$1">>, [AidFinal]
    ),
    Final = final_decl(FinalPath, AidFinal, Bytes),
    {ok, created, _} = elib_pg:with_tx(fun(X) ->
        talk_record_job_repo:enqueue_tx(
            X,
            #{
                record_id => Rid,
                kind => <<"assemble">>,
                input_manifest_hash => maps:get(<<"manifest_hash">>, Manifest),
                input_content_revision => 1,
                dedupe_key => <<"assembly-", (integer_to_binary(N))/binary>>,
                %% enqueue_tx 对 processor_input 自行 jsone:encode（repo 134-137 行）——
                %% 传已编码 binary 会双重编码成 JSON string，ck_trj_processor_input 即拒。
                processor_input => #{<<"final_audio">> => Final}
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

derived_attachment(N, Rid, Prefix, Origin, Bytes) ->
    Nonce = binary:encode_hex(crypto:strong_rand_bytes(12), lowercase),
    %% reserve_tx 返回 {ok, Aid}——直接当 Aid 用会让 write 的 SQL 参数
    %% 携带 {ok, N} 元组（epgsql int8 encode 即 integer_overflow）。
    %% write/2 自带事务（elib_pg 嵌套 tx 不受嵌套拒绝保护），必须在 reserve
    %% 事务提交后调用——生产 run_manifest 即此次序。
    {ok, Aid} =
        elib_pg:with_tx(fun(X) ->
            attachment_derived_artifact:reserve_tx(
                X,
                #{job_id => N, attempt => 1, actor_id => 700001},
                #{
                    <<"path">> => <<Prefix/binary, "/", Nonce/binary>>,
                    <<"url">> => <<>>,
                    <<"name">> => <<"audio.aac">>,
                    <<"size">> => byte_size(Bytes),
                    <<"mime_type">> => <<"audio/aac">>,
                    <<"scope">> => <<"talk_record">>,
                    <<"scope_ref">> => integer_to_binary(Rid),
                    <<"origin">> => Origin,
                    <<"file_hash256">> =>
                        binary:encode_hex(crypto:hash(sha256, Bytes), lowercase)
                }
            )
        end),
    ok = attachment_derived_artifact:write(Aid, Bytes),
    {ok, Aid}.

final_decl(Path, AidFinal, Bytes) ->
    Frames = 64,
    #{
        <<"object_key">> => Path,
        <<"client_upload_id">> => <<"0f0e0d0c-0b0a-49f8-8a7b-6c5d4e3f2a1b">>,
        <<"sha256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
        <<"byte_size">> => byte_size(Bytes),
        <<"sample_rate">> => 16000,
        <<"channels">> => 1,
        <<"sample_count">> => Frames * 1024,
        <<"duration_ms">> => (Frames * 1024 * 1000 + 8000) div 16000,
        <<"attachment_id">> => AidFinal
    }.

late_publish(C, B, N, Mode) ->
    {Rid, J} = fixture(C, B, N),
    {ok, P} = elib_pg:with_tx(fun(X) -> talk_record_assembly_job:prepare_tx(X, J) end),
    %% no-backend-ffmpeg V1：中间段仅 manifest（成品不再服务端产副本）。
    %% 竞态注入不需要第二遍真校验——gate 在 prepare 期即拒绝（prepare_tx
    %% 已取，后续 run(J) 直接失败）。
    Mb = jsone:encode(maps:get(manifest, P)),
    ManifestFile = filename:join(os:getenv("TALK_UPLOAD_DISK_ROOT"), "manifest-relay.json"),
    ok = file:write_file(ManifestFile, Mb),
    ManifestStaging = relay_staging_key(J, <<"manifest">>),
    {ok, _} = relay_staging_upload(ManifestFile, <<"application/json">>, ManifestStaging),
    {ok, Ready} = elib_pg:with_tx(fun(X) ->
        talk_record_assembly_job:reserve_tx(X, J, P, Mb)
    end),
    ok = talk_record_assembly_artifact:write(
        Ready,
        #{
            manifest_staging => ManifestStaging,
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
    ?assertEqual({error, audio_frame_invalid}, talk_record_assembly_job:run(J)),
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    ?assertEqual(null, maps:get(<<"audio_attachment_id">>, A)),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='queued',lease_token=NULL,lease_owner=NULL,lease_until=NULL WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ),
    ?assertEqual(
        {ok, #{outcome => failed, error_code => <<"audio_frame_invalid">>}},
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

%% 成品附件字节身份被篡改（与冻结声明不符）→ prepare 期数据库事实复核
%% final_asset_mismatch，不进事务外校验段。
final_asset_mismatch(C, B, N) ->
    {Rid, J} = fixture(C, B, N),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "UPDATE attachment SET file_hash256=repeat('f',64)"
            " WHERE scope='talk_record' AND scope_ref=$1"
            " AND path LIKE 'assembly-final/%'"
        >>,
        [integer_to_binary(Rid)]
    ),
    ?assertEqual({error, final_asset_mismatch}, talk_record_assembly_job:run(J)),
    {ok, A} = talk_record_assembly_logic:find_tx(C, Rid),
    ?assertEqual(null, maps:get(<<"audio_attachment_id">>, A)),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ).

corrupt_source(C, B, N) ->
    {Rid, J} = fixture(C, B, N),
    %% 字节完整性锚 = talk_record_audio_part.sha256（manifest 重算比对，
    %% 任一不符 fail-closed：锚值破坏在 prepare 期即被 fence → input_stale）。
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
    ?assertEqual({error, audio_frame_invalid}, talk_record_assembly_job:run(J)).

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
    ?assertEqual(Expected, talk_record_assembly_job:fail(J, <<"audio_frame_invalid">>)),
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

%% 固定合法 ADTS 字节（no-backend-ffmpeg V1：与
%% talk_record_audio_validate_tests 同构的 16kHz 单声道 39 字节帧 ×64），
%% 替换旧 /usr/local/bin/ffmpeg 动态生成——无 FFmpeg 环境仍走真实完成链。
tone() ->
    Length = 39,
    B3 = 16#40 bor (Length bsr 11),
    B4 = (Length bsr 3) band 16#FF,
    B5 = (Length band 7) bsl 5,
    Frame = <<16#FF, 16#F1, 16#60, B3, B4, B5, 16#00, 0:256>>,
    iolist_to_binary(lists:duplicate(64, Frame)).
