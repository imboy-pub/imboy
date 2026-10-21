-module(talk_record_part_upload).
-moduledoc "Capture parts reuse ordinary V5 upload sessions; declarations are not assets.".
-export([prepare/3, verify_upload/3, confirmed_asset_tx/4, client_upload_id/1]).

%% The UUID is derived from a globally unique part ID, not seq/hash. Retries and
%% later captures therefore cannot accidentally mint/reuse another object key.
client_upload_id(PartId) when is_integer(PartId), PartId > 0 ->
    <<A:48, _:4, B:12, _:2, C:62, _/binary>> =
        crypto:hash(
            sha,
            <<16#6ba7b8129dad11d180b400c04fd430c8:128, "imboy:talk_record:audio_part:",
                (integer_to_binary(PartId))/binary>>
        ),
    Hex = binary:encode_hex(<<A:48, 5:4, B:12, 2:2, C:62>>, lowercase),
    <<H1:8/binary, H2:4/binary, H3:4/binary, H4:4/binary, H5:12/binary>> = Hex,
    <<H1/binary, "-", H2/binary, "-", H3/binary, "-", H4/binary, "-", H5/binary>>.

prepare(Uid, RecordId, Part) ->
    case talk_record_capture_input:valid_part_codec(maps:get(<<"codec">>, Part, undefined)) of
        true -> prepare_aac(Uid, RecordId, Part);
        false -> {error, invalid_codec}
    end.

prepare_aac(Uid, RecordId, Part) ->
    Pid = part_id(Part),
    Ref = integer_to_binary(RecordId),
    {Mime, Ext} = codec(maps:get(<<"codec">>, Part, null)),
    Name = <<"part-", (integer_to_binary(Pid))/binary, Ext/binary>>,
    Cid = client_upload_id(Pid),
    case attach_logic:presign(Uid, Name, Mime, <<"talk_record">>, Ref, Cid) of
        {ok, Session} -> {ok, Session};
        {error, session_expired} -> renew(Uid, Cid, Mime);
        Error -> Error
    end.

%% Renewal reuses the frozen location and repository deadline guard. Signing is
%% performed only after the new expiry commits; a dead session is never replaced.
renew(Uid, Cid, Mime) ->
    case attach_pending_repo:status_lookup(Cid) of
        {ok, #{<<"object_key">> := Key} = Row} ->
            case
                {
                    attach_logic:reauthorize_upload_session(Uid, Row),
                    attachment_storage:session_location(Key)
                }
            of
                {ok, {ok, Location}} when is_map(Location) ->
                    case renew_expiry(Key, Uid, Row) of
                        {ok, #{committed_expires_at := Expiry}} ->
                            {ok, Cap} = imboy_jwt:issue_upload_capability(Key, Mime, Expiry),
                            {ok, #{
                                <<"object_key">> => Key,
                                <<"client_upload_id">> => Cid,
                                <<"put_url">> =>
                                    <<"/api/v1/attachment/gateway/", Cid/binary, "?capability=",
                                        Cap/binary>>,
                                <<"expires_at">> => Expiry,
                                <<"upload_capability">> => Cap,
                                <<"gateway">> => true,
                                <<"session_state">> => <<"registered">>
                            }};
                        Error ->
                            Error
                    end;
                {{error, _} = Error, _} ->
                    Error;
                {_, _} ->
                    {error, historical_location_unverified}
            end;
        Error ->
            Error
    end.

renew_expiry(Key, Uid, Row) ->
    Now = erlang:system_time(second),
    Deadline = unix_seconds(maps:get(<<"session_deadline">>, Row, null)),
    case Now < Deadline of
        true -> attach_pending_repo:renew_capability(Key, Uid, min(Now + 3600, Deadline));
        false -> {error, session_expired}
    end.

unix_seconds(Bin) when is_binary(Bin) ->
    try
        calendar:rfc3339_to_system_time(binary_to_list(Bin), [{unit, second}])
    catch
        _:_ -> 0
    end;
unix_seconds(_) ->
    0.

%% Authorization is checked by the caller before any storage I/O and again when
%% binding. No user-provided key or attachment ID is accepted in the receipt.
verify_upload(Uid, RecordId, Part) ->
    Cid = client_upload_id(part_id(Part)),
    case read_session(Cid) of
        {ok, Row} -> verify_session(Uid, RecordId, Part, Row);
        {error, not_found} -> {error, upload_not_confirmed};
        Error -> Error
    end.

read_session(Cid) ->
    Sql = <<
        "SELECT object_key,creator_user_id,scope,session_state,"
        " upload_registration,upload_content FROM attach_pending"
        " WHERE client_upload_id=$1::uuid"
    >>,
    case elib_pg:query(Sql, [Cid]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        Error -> Error
    end.

verify_session(Uid, RecordId, Part, Row) ->
    case session_matches(Uid, RecordId, Row) of
        false ->
            {error, session_mismatch};
        true ->
            case maps:get(<<"session_state">>, Row) of
                <<"confirmed">> -> ok;
                <<"registered">> -> verify_bytes(Uid, RecordId, Part, Row);
                _ -> {error, session_not_renewable}
            end
    end.

verify_bytes(Uid, RecordId, Part, Row) ->
    %% Gateway pin is server-computed from uploaded bytes, not a client claim.
    case pin_matches(Part, maps:get(<<"upload_content">>, Row, null)) of
        false ->
            {error, audio_part_verification_failed};
        true ->
            case
                attach_logic:confirm(
                    Uid,
                    maps:get(<<"object_key">>, Row),
                    <<"talk_record">>,
                    integer_to_binary(RecordId),
                    #{}
                )
            of
                {ok, _} -> ok;
                Error -> Error
            end
    end.

confirmed_asset_tx(Conn, Uid, RecordId, Part) ->
    Cid = client_upload_id(part_id(Part)),
    Sql = <<"SELECT object_key FROM attach_pending WHERE client_upload_id=$1::uuid">>,
    case elib_pg:query(Conn, Sql, [Cid]) of
        {ok, [#{<<"object_key">> := Key}]} ->
            %% Use the existing V5 object lock order before pending/asset locks.
            case attach_pending_repo:lock_session_tx(Conn, Key) of
                {ok, not_found} -> {error, upload_not_confirmed};
                {ok, Session} -> locked_asset(Conn, Uid, RecordId, Part, Cid, Session);
                Error -> Error
            end;
        {ok, []} ->
            {error, upload_not_confirmed};
        Error ->
            Error
    end.

locked_asset(Conn, Uid, RecordId, Part, Cid, Session) ->
    case
        session_matches(Uid, RecordId, Session) andalso
            maps:get(<<"session_state">>, Session, null) =:= <<"confirmed">> andalso
            pin_matches(Part, maps:get(<<"upload_content">>, Session, null))
    of
        false ->
            {error, audio_part_verification_failed};
        true ->
            Sql = <<
                "SELECT a.id,a.file_hash256,a.size FROM attachment a"
                " JOIN attach_pending p ON p.confirmed_attachment_id=a.id AND p.object_key=a.path"
                " WHERE p.client_upload_id=$1::uuid AND p.session_state='confirmed'"
                " AND a.scope='talk_record' AND a.scope_ref=$2 AND a.creator_user_id=$3"
                " AND a.lifecycle_state='live' AND a.status>=0 FOR UPDATE OF a"
            >>,
            case elib_pg:query(Conn, Sql, [Cid, integer_to_binary(RecordId), Uid]) of
                {ok, [#{<<"id">> := Id, <<"file_hash256">> := Sha, <<"size">> := Size}]} ->
                    case
                        Sha =:= maps:get(<<"sha256">>, Part) andalso
                            Size =:= maps:get(<<"byte_size">>, Part)
                    of
                        true -> {ok, Id};
                        false -> {error, audio_part_verification_failed}
                    end;
                {ok, []} ->
                    {error, audio_part_verification_failed};
                Error ->
                    Error
            end
    end.

session_matches(Uid, RecordId, #{
    <<"creator_user_id">> := Uid,
    <<"scope">> := <<"talk_record">>,
    <<"upload_registration">> := Reg0
}) ->
    Reg = json(Reg0),
    case Reg of
        #{
            <<"subject">> := #{<<"kind">> := <<"user">>, <<"principal">> := Principal},
            <<"declaration">> := #{<<"scope_ref">> := Ref, <<"scope">> := <<"talk_record">>}
        } ->
            Principal =:= integer_to_binary(Uid) andalso Ref =:= integer_to_binary(RecordId);
        _ ->
            false
    end;
session_matches(_, _, _) ->
    false.

pin_matches(Part, Pin0) ->
    case json(Pin0) of
        #{<<"sha256">> := Sha, <<"size_bytes">> := Size} ->
            Sha =:= maps:get(<<"sha256">>, Part) andalso
                integer_value(Size) =:= maps:get(<<"byte_size">>, Part);
        _ ->
            false
    end.

json(Bin) when is_binary(Bin) ->
    try
        jsone:decode(Bin)
    catch
        _:_ -> null
    end;
json(Value) ->
    Value.
integer_value(N) when is_integer(N) -> N;
integer_value(Bin) when is_binary(Bin) ->
    try
        binary_to_integer(Bin)
    catch
        _:_ -> undefined
    end;
integer_value(_) ->
    undefined.
part_id(#{<<"part_id">> := Id}) -> Id;
part_id(#{<<"id">> := Id}) -> Id.
codec(<<"aac">>) -> {<<"audio/aac">>, <<".aac">>}.
