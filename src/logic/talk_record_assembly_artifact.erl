-module(talk_record_assembly_artifact).
-moduledoc "合成音频及JSON清单复用派生附件与现有引用生命周期。".
-export([reserve_tx/5, write/2, publish_tx/3, with_audio/2]).

reserve_tx(C, Job, Prepared, AudioProof, ManifestBytes) ->
    %% V3-18 P2：Audio 的 hash/size 来自 Worker staging 上传的返回值
    %% （{Sha256Hex, Size}）——BEAM 不再读成品字节算哈希。
    Rid = maps:get(<<"record_id">>, Job),
    {ok, [#{<<"owner_user_id">> := Uid}]} = elib_pg:query(
        C,
        <<"SELECT owner_user_id FROM talk_record WHERE id=$1">>,
        [Rid]
    ),
    {AudioSha, AudioSize} = AudioProof,
    Audio = reserve(
        C,
        Job,
        Uid,
        <<"transcoded_audio">>,
        <<"audio/aac">>,
        AudioSize,
        AudioSha
    ),
    Manifest = reserve(
        C,
        Job,
        Uid,
        <<"manifest">>,
        <<"application/json">>,
        byte_size(ManifestBytes),
        binary:encode_hex(crypto:hash(sha256, ManifestBytes), lowercase)
    ),
    Prepared#{artifacts => [Audio, Manifest]}.

reserve(C, Job, Uid, Role, Mime, Size, Hash) ->
    Rid = maps:get(<<"record_id">>, Job),
    Jid = maps:get(<<"id">>, Job),
    Nonce = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    Key = <<"talk-record/assembly/", Nonce/binary>>,
    Meta = #{
        <<"path">> => Key,
        <<"url">> => <<>>,
        <<"name">> => Role,
        <<"size">> => Size,
        <<"mime_type">> => Mime,
        <<"file_hash256">> => Hash,
        <<"scope">> => <<"talk_record">>,
        <<"scope_ref">> => integer_to_binary(Rid),
        <<"origin">> => <<"server_assembly">>
    },
    {ok, Aid} = require(
        attachment_derived_artifact:reserve_tx(
            C,
            #{job_id => Jid, attempt => maps:get(<<"attempt">>, Job), actor_id => Uid},
            Meta
        )
    ),
    Lease = <<"assembly_pending:", (integer_to_binary(Aid))/binary>>,
    {ok, _, _} = require(
        talk_record_attachment_adapter:grant_processing_lease_tx(
            C, Rid, Aid, Role, Lease, 900
        )
    ),
    #{attachment_id => Aid, role => Role, lease_key => Lease}.

%% V3-18 P2：Worker 落链——staging 上传（音频/manifest 临时文件，Worker
%% 自签 PUT+checksum）→ 服务端 CopyObject（staging→工件物理键）→
%% derived_write 状态收口。BEAM 全程零工件字节。
write(#{artifacts := [#{attachment_id := Audio}, #{attachment_id := Manifest}]}, #{
    audio_staging := AudioStaging,
    manifest_staging := ManifestStaging,
    audio_file := AudioFile,
    manifest_file := ManifestFile
}) ->
    %% land_artifact 成功返回 finish_write 的 Worker 结果（{ok, #{sha256,
    %% size}}）；write/2 对外合同统一收敛为 ok / {error, _}（runner 与
    %% relay 测试只关心成败，不透传上传视图——历史 case_clause 源）。
    case land_artifact(Audio, AudioFile, AudioStaging) of
        {ok, _} ->
            case land_artifact(Manifest, ManifestFile, ManifestStaging) of
                {ok, _} -> ok;
                Error -> Error
            end;
        Error ->
            Error
    end.

land_artifact(Aid, Path, StagingKey) ->
    case attachment_derived_artifact:begin_write(Aid) of
        {ok, I = #{namespace := _, container := C}} ->
            Cfg = application:get_env(imboy, garage, #{}),
            Task = #{
                src_path => Path,
                endpoint => maps:get(endpoint, Cfg),
                ak => maps:get(access_key, Cfg),
                sk => maps:get(secret_key, Cfg),
                bucket => C,
                region => maps:get(region, Cfg, <<"garage">>),
                object_key => StagingKey,
                mime => maps:get(mime, I, <<"application/octet-stream">>)
            },
            case attachment_worker_port:upload_file(Task) of
                {ok, _} = OkUp ->
                    case attachment_derived_artifact:copy_staging(I, StagingKey) of
                        {ok, _} ->
                            attachment_derived_artifact:finish_write(Aid, OkUp);
                        {error, _} = CopyErr ->
                            _ = attachment_derived_artifact:finish_write(Aid, CopyErr),
                            CopyErr
                    end;
                {error, _} = UpErr ->
                    _ = attachment_derived_artifact:finish_write(Aid, UpErr),
                    UpErr
            end;
        Error ->
            Error
    end.

publish_tx(C, Rid, #{artifacts := Artifacts}) ->
    lists:foreach(
        fun(#{attachment_id := Aid, role := Role, lease_key := Lease}) ->
            ok = require(attachment_derived_artifact:complete_tx(C, Aid)),
            Key = <<"assembly:", Role/binary, ":", (integer_to_binary(Aid))/binary>>,
            {ok, _, _} = require(
                talk_record_attachment_adapter:bind_artifact_tx(C, Rid, Aid, Role, Key)
            ),
            {ok, _} = require(
                talk_record_attachment_adapter:cancel_processing_lease_tx(C, Rid, Role, Lease)
            )
        end,
        Artifacts
    ),
    [#{attachment_id := Audio}, #{attachment_id := Manifest}] = Artifacts,
    {ok, 1} = require(
        elib_pg:execute(
            C,
            <<
                "UPDATE talk_record_assembly SET audio_attachment_id=$2"
                " WHERE record_id=$1"
            >>,
            [Rid, Audio]
        )
    ),
    {ok, 1} = require(
        elib_pg:execute(
            C,
            <<
                "UPDATE talk_record SET audio_manifest_attachment_id=$2"
                " WHERE id=$1 AND deleted_at IS NULL"
            >>,
            [Rid, Manifest]
        )
    ),
    ok;
publish_tx(_, _, _) ->
    {error, assembly_artifacts_missing}.

with_audio(Aid, Fun) -> attachment_derived_artifact:with_spool(Aid, Fun).
require({error, R}) -> throw({talk_assembly_abort, R});
require(V) -> V.
