-module(talk_record_assembly_artifact).
-moduledoc "合成音频及JSON清单复用派生附件与现有引用生命周期。".
-export([reserve_tx/5, write/3, publish_tx/3, with_audio/2]).

reserve_tx(C, Job, Prepared, Source, ManifestBytes) ->
    Rid = maps:get(<<"record_id">>, Job),
    {ok, [#{<<"owner_user_id">> := Uid}]} = elib_pg:query(
        C,
        <<"SELECT owner_user_id FROM talk_record WHERE id=$1">>,
        [Rid]
    ),
    Audio = reserve(
        C,
        Job,
        Uid,
        <<"transcoded_audio">>,
        <<"audio/aac">>,
        attachment_upload_spool:size(Source),
        attachment_upload_spool:sha256(Source)
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

write(#{artifacts := [#{attachment_id := Audio}, #{attachment_id := Manifest}]}, Source, Bytes) ->
    case attachment_derived_artifact:write(Audio, Source) of
        ok -> attachment_derived_artifact:write(Manifest, Bytes);
        Error -> Error
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
