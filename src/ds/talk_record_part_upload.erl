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
    %% V3-18：录音分片上传切 direct s3_put——正文不过 BEAM（原 V5 presign +
    %% gateway put_url 通道随 V3-20 退役）。字节证明链：presign 绑定
    %% x-amz-checksum-sha256（PUT 时 S3 验内容）→ finalize copy proof——
    %% 强于旧 verify_and_save_v5 的 BEAM 读回复核。Cid 幂等保持（T02）：
    %% 重试/续录同 part 恒同 object_key，不会铸造/复用错对象。
    Pid = part_id(Part),
    {Mime, Ext} = codec(maps:get(<<"codec">>, Part, null)),
    Name = <<"part-", (integer_to_binary(Pid))/binary, Ext/binary>>,
    Cid = client_upload_id(Pid),
    Sha = maps:get(<<"sha256">>, Part, <<>>),
    Size = maps:get(<<"byte_size">>, Part, 0),
    case
        attach_direct_logic:register(
            Uid, <<"talk_record">>, integer_to_binary(RecordId), Cid, Name
        )
    of
        {ok, #{object_key := Key}} ->
            case attach_direct_logic:declare(Key, Sha, Size, Mime) of
                ok -> grant_descriptor(Key, Sha);
                %% 同 part 重试（expected 已冻结同值）：幂等直接再取 descriptor
                {error, expected_conflict} -> grant_descriptor(Key, Sha);
                Error -> Error
            end;
        Error ->
            Error
    end.

%% GRANTED/FINALIZING（票据过期/前次 attempt 未收口）→ renew_grant 新
%% attempt（I05：新 staging/target，一次授权一次请求）；CONFIRMED 重试
%% 返回原会话投影（幂等）。
grant_descriptor(Key, Sha) ->
    case attach_direct_logic:grant(Key, Sha) of
        {ok, #{grant := _G, attempt_id := _No} = Granted} ->
            {ok, Granted};
        {error, illegal_state} ->
            attach_direct_logic:renew_grant(Key, Sha);
        Error ->
            Error
    end.

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

verify_bytes(_Uid, _RecordId, _Part, Row) ->
    %% V3-18：registered 会话按 direct 收口最新 attempt——字节证明来自
    %% staging PUT 的 S3 checksum（presign 绑定）与 finalize copy proof，
    %% 不再 BEAM 读回复核（verify_and_save_v5 退役面）。declared expected
    %% 与 proof 的一致性由 direct 状态机保证；会话/主体匹配已在上层核过。
    Key = maps:get(<<"object_key">>, Row),
    case latest_attempt(Key) of
        {ok, No} ->
            case attach_direct_logic:finalize(Key, No) of
                {ok, #{state := <<"CONFIRMED">>}} ->
                    ok;
                {ok, #{outcome := <<"UNKNOWN_QUARANTINED">>}} ->
                    %% UNKNOWN 不晋升（I13）：对调用方保持“未确认可重试”
                    {error, upload_not_confirmed};
                {ok, _Rejected} ->
                    {error, audio_part_verification_failed};
                {error, attempt_not_dispatchable} ->
                    %% attempt 已收口但会话未 confirm（proof 拒绝）或已
                    %% CONFIRMED 竞态——重读会话态裁决
                    case session_confirmed(Key) of
                        true -> ok;
                        false -> {error, audio_part_verification_failed}
                    end;
                Error ->
                    Error
            end;
        none ->
            %% 无 attempt：会话未取 descriptor（客户端未走 grant）
            {error, upload_not_confirmed}
    end.

latest_attempt(Key) ->
    case attach_direct_attempt_repo:list_for_session(Key) of
        {ok, Attempts} when Attempts =/= [] ->
            #{<<"attempt_no">> := No} = lists:last(Attempts),
            {ok, No};
        _ ->
            none
    end.

session_confirmed(Key) ->
    case
        elib_pg:query(
            <<"SELECT session_state FROM attach_pending WHERE object_key=$1">>,
            [Key]
        )
    of
        {ok, [#{<<"session_state">> := <<"confirmed">>}]} -> true;
        _ -> false
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
