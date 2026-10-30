-module(talk_record_playback_parts).
-moduledoc "Authenticated playback of completed, confirmed source parts.".
-export([open/2, metadata/4, with_source/5, recheck/4, recheck_part/5]).

-define(AUD, <<"talk_record_playback_parts">>).

open(Uid, Rid) ->
    case snapshot(Uid, Rid) of
        {ok, Acl, Hash, Parts} ->
            case secret() of
                {ok, Key} ->
                    Now = os:system_time(second),
                    Exp = Now + talk_record_grant_playback:ttl_seconds(),
                    Claims = #{
                        <<"aud">> => ?AUD,
                        <<"sub">> => ?AUD,
                        <<"uid">> => Uid,
                        <<"rid">> => Rid,
                        <<"acl">> => Acl,
                        <<"manifest">> => Hash,
                        <<"iat">> => Now,
                        <<"exp">> => Exp
                    },
                    Ticket = imboy_jwt:sign(Claims, Key),
                    {ok, #{
                        <<"record_id">> => integer_to_binary(Rid),
                        <<"expiry">> => Exp,
                        <<"expires_at">> => elib_dt:to_rfc3339(Exp),
                        <<"parts">> => [part_view(Rid, P, Ticket) || P <- Parts]
                    }};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

part_view(Rid, P, Ticket) ->
    Seq = maps:get(<<"seq">>, P),
    #{
        <<"seq">> => Seq,
        <<"offset_ms">> => maps:get(<<"offset_ms">>, P),
        <<"duration_ms">> => maps:get(<<"duration_ms">>, P),
        <<"url">> =>
            <<"/api/v1/talk-records/", (integer_to_binary(Rid))/binary, "/playback/parts/",
                (integer_to_binary(Seq))/binary, "?ticket=", Ticket/binary>>
    }.

metadata(Uid, Rid, Ticket, Seq) ->
    case verified(Uid, Rid, Ticket) of
        {ok, Parts} ->
            case [P || P <- Parts, maps:get(<<"seq">>, P) =:= Seq] of
                [P] -> {ok, P};
                _ -> {error, playback_media_unavailable}
            end;
        Error ->
            Error
    end.

recheck(Uid, Rid, Ticket, Seq) ->
    case metadata(Uid, Rid, Ticket, Seq) of
        {ok, P} ->
            case
                elib_pg:query(
                    <<
                        "SELECT id FROM attachment WHERE id=$1 AND size=$2 "
                        "AND lifecycle_state='live'"
                    >>,
                    [maps:get(<<"attachment_id">>, P), maps:get(<<"byte_size">>, P)]
                )
            of
                {ok, [_]} -> ok;
                _ -> {error, playback_media_unavailable}
            end;
        Error ->
            Error
    end.

with_source(Uid, Rid, Ticket, Seq, Fun) ->
    case metadata(Uid, Rid, Ticket, Seq) of
        {ok, P} ->
            Aid = maps:get(<<"attachment_id">>, P),
            Size = maps:get(<<"byte_size">>, P),
            Hash = maps:get(<<"sha256">>, P),
            Max = maps:get(<<"max_part_bytes">>, talk_record_ds:capabilities(), 0),
            talk_record_playback_parts_limit:with_slot(fun() ->
                attachment_derived_artifact:with_live_spool(Aid, Max, fun(Source) ->
                    case
                        attachment_upload_spool:size(Source) =:= Size andalso
                            attachment_upload_spool:sha256(Source) =:= Hash andalso
                            recheck_part(Uid, Rid, Ticket, Seq, P) =:= ok
                    of
                        true -> Fun(Source, Size, P);
                        false -> {error, playback_media_unavailable}
                    end
                end)
            end);
        Error ->
            Error
    end.

verified(Uid, Rid, Ticket) when is_binary(Ticket) ->
    case secret() of
        {ok, Key} ->
            case imboy_jwt:verify(Ticket, Key) of
                {ok, #{
                    <<"aud">> := ?AUD,
                    <<"sub">> := ?AUD,
                    <<"uid">> := Uid,
                    <<"rid">> := Rid,
                    <<"acl">> := Acl,
                    <<"manifest">> := Hash
                }} ->
                    case snapshot(Uid, Rid) of
                        {ok, Acl, Hash, Parts} -> {ok, Parts};
                        _ -> {error, playback_ticket_invalid}
                    end;
                _ ->
                    {error, playback_ticket_invalid}
            end;
        Error ->
            Error
    end;
verified(_, _, _) ->
    {error, playback_ticket_invalid}.

%% The initial request checks the full manifest. Each block rechecks only the
%% signed manifest identity and the selected part, keeping work bounded.
recheck_part(Uid, Rid, Ticket, Seq, P) ->
    case secret() of
        {ok, Key} ->
            case imboy_jwt:verify(Ticket, Key) of
                {ok, #{
                    <<"aud">> := ?AUD,
                    <<"sub">> := ?AUD,
                    <<"uid">> := Uid,
                    <<"rid">> := Rid,
                    <<"acl">> := Acl,
                    <<"manifest">> := Hash
                }} ->
                    case talk_record_content_common:visible_or_404(Uid, Rid) of
                        {ok, #{<<"acl_revision">> := Acl, <<"media_status">> := Status}} when
                            Status =:= <<"assembling">>;
                            Status =:= <<"retryable_failed">>;
                            Status =:= <<"available">>
                        ->
                            part_still_bound(Rid, Hash, Seq, P);
                        _ ->
                            {error, playback_ticket_invalid}
                    end;
                _ ->
                    {error, playback_ticket_invalid}
            end;
        Error ->
            Error
    end.

part_still_bound(Rid, Hash, Seq, P) ->
    Sql = <<
        "SELECT p.id FROM talk_record_assembly a "
        "JOIN talk_record_audio_part p ON p.record_id=a.record_id AND p.capture_id=a.capture_id "
        "JOIN attachment x ON x.id=p.attachment_id "
        "WHERE a.record_id=$1 AND a.manifest_hash=$2 AND p.seq=$3 AND p.id=$4 "
        "AND p.attachment_id=$5 AND p.sha256=$6 AND p.byte_size=$7 "
        "AND p.upload_status='confirmed' AND x.lifecycle_state='live' AND x.size=p.byte_size "
        "AND EXISTS (SELECT 1 FROM talk_record_attachment_ref r "
        "WHERE r.record_id=p.record_id AND r.capture_id=p.capture_id "
        "AND r.attachment_id=p.attachment_id AND r.role='audio_part' AND r.state='bound' "
        "AND r.business_key=('audio_part:' || p.id::text)) "
        "AND (a.segments -> $3 ->> 'part_id')=p.id::text "
        "AND (a.segments -> $3 ->> 'attachment_id')=p.attachment_id::text "
        "AND (a.segments -> $3 ->> 'sha256')=p.sha256 "
        "AND (a.segments -> $3 ->> 'byte_size')=p.byte_size::text"
    >>,
    Args = [
        Rid,
        Hash,
        Seq,
        maps:get(<<"part_id">>, P),
        maps:get(<<"attachment_id">>, P),
        maps:get(<<"sha256">>, P),
        maps:get(<<"byte_size">>, P)
    ],
    case elib_pg:query(Sql, Args) of
        {ok, [_]} -> ok;
        _ -> {error, playback_media_unavailable}
    end.

snapshot(Uid, Rid) ->
    case talk_record_content_common:visible_or_404(Uid, Rid) of
        {ok, #{<<"acl_revision">> := Acl, <<"media_status">> := Status}} when
            Status =:= <<"assembling">>;
            Status =:= <<"retryable_failed">>;
            Status =:= <<"available">>
        ->
            case elib_pg:with_conn(fun(C) -> snapshot_tx(C, Rid) end) of
                {ok, Hash, Parts} -> {ok, Acl, Hash, Parts};
                Error -> Error
            end;
        {ok, _} ->
            {error, playback_media_unavailable};
        Error ->
            Error
    end.

snapshot_tx(C, Rid) ->
    case
        {
            talk_record_assembly_logic:find_tx(C, Rid),
            talk_record_assembly_logic:confirmed_parts_tx(C, Rid)
        }
    of
        {
            {ok, #{
                <<"capture_id">> := Capture,
                <<"manifest_hash">> := Hash,
                <<"segments">> := Stored
            }},
            {ok, [_ | _] = Rows}
        } ->
            Manifest = talk_record_assembly_logic:build_manifest(Capture, Rows),
            Parts = maps:get(<<"parts">>, Manifest),
            Seqs = [maps:get(<<"seq">>, P) || P <- Parts],
            MaxBytes = maps:get(<<"max_part_bytes">>, talk_record_ds:capabilities(), 0),
            ValidParts = lists:all(
                fun(P) ->
                    maps:get(<<"duration_ms">>, P) > 0 andalso
                        maps:get(<<"byte_size">>, P) > 0 andalso
                        maps:get(<<"byte_size">>, P) =< MaxBytes andalso
                        maps:get(<<"attachment_id">>, P) > 0
                end,
                Parts
            ),
            case
                maps:get(<<"manifest_hash">>, Manifest) =:= Hash andalso
                    talk_record_client_gate:json(Stored) =:= Parts andalso
                    length(Parts) =< 2000 andalso ValidParts andalso
                    Seqs =:= lists:seq(0, length(Seqs) - 1)
            of
                true -> {ok, Hash, Parts};
                false -> {error, playback_media_unavailable}
            end;
        _ ->
            {error, playback_media_unavailable}
    end.

secret() ->
    case application:get_env(imboy, jwt_key) of
        {ok, Key} when is_binary(Key), byte_size(Key) >= 32 -> {ok, Key};
        _ -> {error, playback_signing_key_unavailable}
    end.
