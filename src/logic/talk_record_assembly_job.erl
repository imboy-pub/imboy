-module(talk_record_assembly_job).
-moduledoc "真实音频组装：短事务准备、受控IO、重验与原子发布。".
-export([run/1, prepare_tx/2, reserve_tx/4, publish_tx/3, fail/2]).

run(Job) ->
    %% V3-18 P2 三段式：prepare（tx 权益）→ 外部处理（Worker fetch parts +
    %% ffmpeg 解码/拼接/编码 + Worker staging 上传拿 proof）→ tx reserve
    %% （hash/size 真值建行）→ 服务端 Copy + finish（零 BEAM 字节）→
    %% tx publish。staging 键随机一次性；残留由 sweep 收敛。
    case tx(fun(C) -> prepare_tx(C, Job) end) of
        {ok, P} ->
            talk_record_audio_render:with_audio(maps:get(parts, P), fun(OutPath) ->
                Bytes = jsone:encode(maps:get(manifest, P)),
                ManifestFile = filename:join(filename:dirname(OutPath), "manifest.json"),
                ok = file:write_file(ManifestFile, Bytes),
                AudioStaging = staging_key(Job, <<"audio">>),
                ManifestStaging = staging_key(Job, <<"manifest">>),
                case staging_upload(OutPath, <<"audio/aac">>, AudioStaging) of
                    {ok, #{sha256 := ASha, size := ASize}} ->
                        case
                            staging_upload(ManifestFile, <<"application/json">>, ManifestStaging)
                        of
                            {ok, _} ->
                                case
                                    tx(fun(C) -> reserve_tx(C, Job, P, {{ASha, ASize}, Bytes}) end)
                                of
                                    {ok, Ready} ->
                                        case
                                            talk_record_assembly_artifact:write(
                                                Ready,
                                                #{
                                                    audio_staging => AudioStaging,
                                                    manifest_staging => ManifestStaging,
                                                    audio_file => OutPath,
                                                    manifest_file => ManifestFile
                                                }
                                            )
                                        of
                                            %% worker 合同 {ok, published}：
                                            %% publish 成功统一收敛为 tagged
                                            %% 结果（publish_tx 的内部视图
                                            %% 不透传到 dispatch case）。
                                            ok ->
                                                case tx(fun(C) -> publish_tx(C, Job, Ready) end) of
                                                    {ok, _} -> {ok, published};
                                                    E -> E
                                                end;
                                            Error ->
                                                Error
                                        end;
                                    Error ->
                                        Error
                                end;
                            {error, _} = E ->
                                E
                        end;
                    {error, _} = E ->
                        E
                end
            end);
        Error ->
            Error
    end.

staging_key(Job, Role) ->
    Jid = maps:get(<<"id">>, Job),
    Nonce = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"assembly-staging/", (integer_to_binary(Jid))/binary, "/", Role/binary, "/", Nonce/binary>>.

staging_upload(Path, Mime, StagingKey) ->
    Cfg = application:get_env(imboy, garage, #{}),
    Task = #{
        src_path => Path,
        endpoint => maps:get(endpoint, Cfg),
        ak => maps:get(access_key, Cfg),
        sk => maps:get(secret_key, Cfg),
        bucket => maps:get(bucket, Cfg, <<"imboy">>),
        region => maps:get(region, Cfg, <<"garage">>),
        object_key => StagingKey,
        mime => Mime
    },
    attachment_worker_port:upload_file(Task).

prepare_tx(C, Job) ->
    ok = gate(C, Job),
    require(talk_record_assembly_logic:prepare_tx(C, Job)).

reserve_tx(C, Job, P, {AudioProof, Bytes}) ->
    %% V3-18 P2：AudioProof={Sha256Hex,Size}（Worker staging 上传返回值）
    ok = gate(C, Job),
    {ok, talk_record_assembly_artifact:reserve_tx(C, Job, P, AudioProof, Bytes)}.

publish_tx(C, Job, P) ->
    ok = gate(C, Job),
    require(talk_record_assembly_logic:finish_tx(C, Job, P)).

gate(C, Job) ->
    Rid = maps:get(<<"record_id">>, Job),
    case talk_record_export_repo:lock_record_tx(C, Rid) of
        {ok, Record} ->
            {ok, Locked} =
                case talk_record_export_repo:lock_job_tx(C, Job) of
                    {error, not_found} -> abort(lease_lost);
                    V -> require(V)
                end,
            Keys = [
                <<"id">>,
                <<"record_id">>,
                <<"kind">>,
                <<"attempt">>,
                <<"lease_token">>,
                <<"input_manifest_hash">>,
                <<"input_content_revision">>
            ],
            case
                maps:with(Keys, Locked) =:= maps:with(Keys, Job) andalso
                    maps:get(<<"kind">>, Job) =:= <<"assemble">> andalso
                    maps:get(<<"content_revision">>, Record) =:=
                        maps:get(<<"input_content_revision">>, Job)
            of
                true -> check_manifest(C, Rid, Job);
                false -> abort(input_stale)
            end;
        {error, not_found} ->
            abort(record_deleted);
        {error, R} ->
            abort(R)
    end.

check_manifest(C, Rid, Job) ->
    {ok, A} = require(talk_record_assembly_logic:find_tx(C, Rid)),
    case maps:get(<<"manifest_hash">>, A) =:= maps:get(<<"input_manifest_hash">>, Job) of
        true -> ok;
        false -> abort(input_stale)
    end.

%% Always settle this current leased job; only its still-current live record
%% input may receive a failed media projection. A newer input is left intact.
fail(Job, Code) ->
    tx(fun(C) ->
        Rid = maps:get(<<"record_id">>, Job),
        {ok, Rows} = require(
            elib_pg:query(
                C,
                <<
                    "SELECT deleted_at,content_revision FROM talk_record"
                    " WHERE id=$1 FOR UPDATE"
                >>,
                [Rid]
            )
        ),
        {ok, Locked} =
            case talk_record_export_repo:lock_job_tx(C, Job) of
                {error, not_found} -> abort(lease_lost);
                V -> require(V)
            end,
        Keys = [<<"id">>, <<"record_id">>, <<"attempt">>, <<"lease_token">>, <<"kind">>],
        case maps:with(Keys, Locked) =:= maps:with(Keys, Job) of
            true -> ok;
            false -> abort(lease_lost)
        end,
        ok = maybe_failed_media(C, Rid, Rows, Locked),
        ok = require(
            talk_record_job_repo:finish_failed_tx(
                C,
                maps:get(<<"id">>, Job),
                maps:get(<<"lease_token">>, Job),
                Code
            )
        ),
        {ok, failed}
    end).

maybe_failed_media(C, Rid, [#{<<"deleted_at">> := null, <<"content_revision">> := Rev}], Job) ->
    case talk_record_assembly_logic:find_tx(C, Rid) of
        {ok, A} ->
            Current =
                maps:get(<<"input_content_revision">>, Job) =:= Rev andalso
                    maps:get(<<"input_manifest_hash">>, Job) =:= maps:get(<<"manifest_hash">>, A),
            case Current of
                true ->
                    {ok, 1} = require(
                        elib_pg:execute(
                            C,
                            <<
                                "UPDATE talk_record SET"
                                " media_status='retryable_failed',updated_at=now() WHERE id=$1"
                            >>,
                            [Rid]
                        )
                    ),
                    ok;
                false ->
                    ok
            end;
        {error, not_found} ->
            ok;
        {error, R} ->
            abort(R)
    end;
maybe_failed_media(_, _, _, _) ->
    ok.

tx(Fun) ->
    case elib_pg:with_tx(Fun, [{reraise, false}]) of
        {rollback, {talk_assembly_abort, R}} -> {error, R};
        V -> V
    end.
require({error, R}) -> abort(R);
require(V) -> V.
abort(R) -> throw({talk_assembly_abort, R}).
