-module(talk_record_assembly_artifact).
-moduledoc "成品直传绑定与小JSON清单派生附件复用现有引用生命周期。".
%% no-backend-ffmpeg V1：音频不再由服务端 reserve/write 派生副本——
%% 客户端合成的成品经 direct-v1 已 CONFIRMED 为正式附件（冻结于
%% job.processor_input.final_audio），本模块只负责：
%%   1. 小 JSON manifest（拼装清单）的派生附件 reserve/land（保留既有
%%      内部作业/引用类别，通用 spool 与 Worker 自签 PUT 不涉 FFmpeg）；
%%   2. 发布事务：成品附件以 role='final_audio' 建永久业务引用，
%%      talk_record_assembly.audio_attachment_id 指向真实客户端成品。
-export([reserve_tx/4, write/2, publish_tx/3]).

reserve_tx(C, Job, Prepared, ManifestBytes) ->
    Rid = maps:get(<<"record_id">>, Job),
    {ok, [#{<<"owner_user_id">> := Uid}]} = elib_pg:query(
        C,
        <<"SELECT owner_user_id FROM talk_record WHERE id=$1">>,
        [Rid]
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
    Prepared#{artifacts => [Manifest]}.

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

%% Worker 落链（仅 manifest）：staging 上传（小 JSON 临时文件，Worker
%% 自签 PUT+checksum）→ 服务端 CopyObject（staging→工件物理键）→
%% derived_write 状态收口。BEAM 全程零成品音频字节。
write(#{artifacts := [#{attachment_id := Manifest}]}, #{
    manifest_staging := ManifestStaging,
    manifest_file := ManifestFile
}) ->
    case land_artifact(Manifest, ManifestFile, ManifestStaging) of
        {ok, _} -> ok;
        Error -> Error
    end.

land_artifact(Aid, Path, StagingKey) ->
    case attachment_derived_artifact:begin_write(Aid, StagingKey) of
        {ok,
            I = #{
                driver := garage,
                namespace := _,
                container := C,
                endpoint := Endpoint,
                garage_config := #{
                    endpoint := Endpoint, access_key := AccessKey, secret_key := SecretKey
                } = Cfg
            }} ->
            Task = #{
                src_path => Path,
                endpoint => Endpoint,
                ak => AccessKey,
                sk => SecretKey,
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
                            record_unknown(Aid, CopyErr)
                    end;
                {error, _} = UpErr ->
                    record_unknown(Aid, UpErr)
            end;
        {ok, _} ->
            record_unknown(Aid, {error, location_unverified});
        Error ->
            Error
    end.

record_unknown(Aid, Error) ->
    case attachment_derived_artifact:finish_write(Aid, Error) of
        {error, artifact_write_quarantined} = Unconfirmed -> Unconfirmed;
        {error, _} -> {error, artifact_write_unknown};
        Other -> {error, {unknown_write_unconfirmed, Other}}
    end.

publish_tx(
    C,
    Rid,
    #{artifacts := [#{attachment_id := Manifest, role := Role, lease_key := Lease}]} = Prepared
) ->
    #{final_audio := Final} = Prepared,
    %% 冻结投影为 JSON 形（binary 键）；attachment_id 由 complete 准入冻结
    %% （正整数），publish 时直接取用。
    FinalAttachment = maps:get(<<"attachment_id">>, Final),
    ok = require(attachment_derived_artifact:complete_tx(C, Manifest)),
    Key = <<"assembly:", Role/binary, ":", (integer_to_binary(Manifest))/binary>>,
    {ok, _, _} = require(
        talk_record_attachment_adapter:bind_artifact_tx(C, Rid, Manifest, Role, Key)
    ),
    {ok, _} = require(
        talk_record_attachment_adapter:cancel_processing_lease_tx(C, Rid, Role, Lease)
    ),
    %% 成品：真实客户端直传附件。引用角色沿用既有 'transcoded_audio' 类别
    %% （消费链回听/AI 按该类别+business_key 查询；T5 切读取权威时一并
    %% 收敛），指向对象从派生副本改为直传附件——不重建派生副本，不二次
    %% Copy；audio_attachment_id 同步指向真实成品。
    FinalRole = <<"transcoded_audio">>,
    FinalKey = <<"assembly:transcoded_audio:", (integer_to_binary(FinalAttachment))/binary>>,
    {ok, _, _} = require(
        talk_record_attachment_adapter:bind_artifact_tx(
            C, Rid, FinalAttachment, FinalRole, FinalKey
        )
    ),
    {ok, 1} = require(
        elib_pg:execute(
            C,
            <<
                "UPDATE talk_record_assembly SET audio_attachment_id=$2"
                " WHERE record_id=$1"
            >>,
            [Rid, FinalAttachment]
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
    ok.

require({error, R}) -> throw({talk_assembly_abort, R});
require(V) -> V.
