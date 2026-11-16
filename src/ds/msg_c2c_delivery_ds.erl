-module(msg_c2c_delivery_ds).
-moduledoc "C2C 消息投递数据服务 —— SenderUid/SenderDid 必须来自认证连接态，受保护密文字节不可变。".
-export([send_other_devices/4, send_other_devices/5]).

%% SenderUid and SenderDid must come from authenticated connection state.
%% Keep logical recipient and all protected ciphertext bytes unchanged.
-spec send_other_devices(pos_integer(), binary(), binary(), map()) -> ok.
send_other_devices(SenderUid, SenderDid, MsgId, Msg) ->
    send_other_devices(SenderUid, SenderDid, MsgId, Msg, false).

%% AllowPlain is server-owned input provenance, never a client envelope field.
-spec send_other_devices(pos_integer(), binary(), binary(), map(), boolean()) -> ok.
send_other_devices(SenderUid, SenderDid, MsgId, Msg, AllowPlain) ->
    case eligible_metadata(SenderUid, SenderDid, Msg, AllowPlain) of
        {ok, Devices} ->
            Dids = lists:usort([
                DID
             || {_Pid, {_Platform, DID}} <- imboy_syn:list_by_uid(SenderUid),
                DID =/= SenderDid,
                maps:is_key(DID, Devices)
            ]),
            case Dids of
                [] ->
                    ok;
                _ ->
                    message_ds:send_next(
                        SenderUid,
                        MsgId,
                        imboy_message_helper:encode_json(Msg),
                        elib_retry_config:intervals(<<"c2c">>),
                        Dids,
                        true
                    )
            end;
        plain ->
            message_ds:send_next(
                SenderUid,
                MsgId,
                imboy_message_helper:encode_json(Msg),
                elib_retry_config:intervals(<<"c2c">>),
                [SenderDid],
                false
            );
        skip ->
            ok
    end.

eligible_metadata(SenderUid, SenderDid, Msg, AllowPlain) ->
    From = maps:get(<<"from">>, Msg, undefined),
    case
        {
            maps:get(<<"type">>, Msg, undefined),
            maps:get(<<"sender_did">>, Msg, undefined),
            maps:find(<<"e2ee">>, Msg)
        }
    of
        {<<"C2C">>, SenderDid, {ok, #{
            <<"meta_version">> := 3,
            <<"fan_out">> := <<"per_device">>,
            <<"devices">> := Devices
        }}} when
            is_map(Devices), is_binary(SenderDid), byte_size(SenderDid) > 0
        ->
            case From =:= SenderUid orelse From =:= integer_to_binary(SenderUid) of
                true -> {ok, Devices};
                false -> skip
            end;
        {<<"C2C">>, SenderDid, {ok, null}} when
            AllowPlain =:= true, is_binary(SenderDid), byte_size(SenderDid) > 0
        ->
            case From =:= SenderUid orelse From =:= integer_to_binary(SenderUid) of
                true -> plain;
                false -> skip
            end;
        _ ->
            skip
    end.
