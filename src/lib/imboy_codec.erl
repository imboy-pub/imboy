%%%-------------------------------------------------------------------
%%% @doc
%%% IMBoy 消息编解码器抽象层
%%%
%%% 提供统一的编解码接口，支持 JSON 和 Protocol Buffers 两种协议。
%%% WebSocket 连接建立时通过子协议协商确定编码格式，后续通信
%%% 全部通过本模块进行编解码，业务层无需关心底层协议。
%%%
%%% 设计原则：
%%%   - 统一内部表示：所有消息在 Erlang 内部统一为 binary-key map
%%%   - 协议透明：Logic/DS/Repo 层不感知传输协议
%%%   - 渐进迁移：支持 JSON/protobuf 双协议并行
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(imboy_codec).

-include("log.hrl").

%% API
-export([
    encode/2,
    decode/2,
    encode_payload/3,
    decode_payload/3,
    encode_ws_frame/2,
    protocol_atom/1
]).

%% 协议类型
-type protocol() :: json | protobuf.
-export_type([protocol/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 编码消息为传输格式
%%
%% JSON 协议：Erlang map → JSON binary
%% Protobuf 协议：Erlang map → protobuf binary
%%
%% @param Protocol 协议类型
%% @param Msg Erlang map（binary key for JSON, atom key for protobuf）
%% @returns 编码后的二进制数据
-spec encode(protocol(), map()) -> binary().
encode(json, Msg) when is_map(Msg) ->
    try jsone:encode(Msg, [native_utf8])
    catch Class:Reason ->
        ok = ?WARN_LOG({codec_json_encode_error, Class, Reason}),
        <<"{}">>
    end;
encode(protobuf, Msg) when is_map(Msg) ->
    try
        PbMsg = to_pb_map(Msg),
        imboy_pb:encode_msg(PbMsg, 'IMBoyMessage')
    catch Class:Reason ->
        ok = ?WARN_LOG({codec_pb_encode_error, Class, Reason}),
        <<>>
    end.

%% @doc 解码传输数据为 Erlang map
%%
%% 解码后统一返回 binary-key map，与现有代码兼容。
%%
%% @param Protocol 协议类型
%% @param Data 二进制数据
%% @returns 解码后的 Erlang map（binary keys）
-spec decode(protocol(), binary()) -> map().
decode(json, Data) when is_binary(Data) ->
    try jsone:decode(Data, [{object_format, map}])
    catch Class:Reason ->
        ok = ?WARN_LOG({codec_json_decode_error, Class, Reason}),
        #{}
    end;
decode(protobuf, Data) when is_binary(Data) ->
    try
        PbMsg = imboy_pb:decode_msg(Data, 'IMBoyMessage'),
        from_pb_map(PbMsg)
    catch Class:Reason ->
        ok = ?WARN_LOG({codec_pb_decode_error, Class, Reason}),
        #{}
    end.

%% @doc 编码 payload 子消息
%%
%% 根据 msg_type 将 payload map 编码为对应的 protobuf 子消息。
%% JSON 协议下直接返回 map（由 encode/2 统一编码）。
%%
%% @param Protocol 协议类型
%% @param MsgType 消息内容类型 (binary)
%% @param Payload payload map
%% @returns 编码后的 payload（JSON: map, protobuf: binary）
-spec encode_payload(protocol(), binary(), map() | binary()) -> map() | binary().
encode_payload(json, _MsgType, Payload) ->
    Payload;
encode_payload(protobuf, MsgType, Payload) when is_map(Payload) ->
    PbType = payload_pb_type(MsgType),
    case PbType of
        undefined -> Payload;
        _ -> imboy_pb:encode_msg(Payload, PbType)
    end;
encode_payload(_Protocol, _MsgType, Payload) ->
    Payload.

%% @doc 解码 payload 子消息
%%
%% @param Protocol 协议类型
%% @param MsgType 消息内容类型 (binary)
%% @param PayloadBin payload 二进制数据
%% @returns 解码后的 payload map
-spec decode_payload(protocol(), binary(), binary() | map()) -> map().
decode_payload(json, _MsgType, Payload) when is_map(Payload) ->
    Payload;
decode_payload(json, _MsgType, Payload) when is_binary(Payload) ->
    try jsone:decode(Payload, [{object_format, map}])
    catch _:_ -> #{<<"raw">> => Payload}
    end;
decode_payload(protobuf, MsgType, PayloadBin) when is_binary(PayloadBin), byte_size(PayloadBin) > 0 ->
    PbType = payload_pb_type(MsgType),
    case PbType of
        undefined -> #{<<"raw">> => PayloadBin};
        _ ->
            try imboy_pb:decode_msg(PayloadBin, PbType)
            catch Class:Reason ->
                ok = ?WARN_LOG({codec_pb_decode_payload_error, Class, Reason, MsgType}),
                #{<<"raw">> => PayloadBin}
            end
    end;
decode_payload(_Protocol, _MsgType, _Payload) ->
    #{}.

%% @doc 将编码后的消息包装为 WebSocket 帧
%%
%% JSON → text frame, protobuf → binary frame
%%
%% @param Protocol 协议类型
%% @param EncodedMsg 已编码的消息二进制
%% @returns Cowboy WebSocket 帧 tuple
-spec encode_ws_frame(protocol(), binary()) -> {text, binary()} | {binary, binary()}.
encode_ws_frame(json, EncodedMsg) ->
    {text, EncodedMsg};
encode_ws_frame(protobuf, EncodedMsg) ->
    {binary, EncodedMsg}.

%% @doc 将子协议字符串转换为协议原子
%%
%% @param SubProtocol WebSocket 子协议字符串
%% @returns 协议原子
-spec protocol_atom(binary() | undefined) -> protocol().
protocol_atom(<<"imboy-protobuf">>) -> protobuf;
protocol_atom(<<"imboy-json">>) -> json;
protocol_atom(<<"text">>) -> json;
protocol_atom(_) -> json.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc 将 binary-key map 转换为 protobuf atom-key map
%%
%% 核心转换：现有代码使用 binary keys（如 <<"type">>），
%% gpb 生成的代码使用 atom keys（如 type）。
%% 同时处理类型转换（string UID → integer, string type → enum atom）。
-spec to_pb_map(map()) -> map().
to_pb_map(Msg) ->
    #{
        id => maps:get(<<"id">>, Msg, <<>>),
        type => msg_direction_to_enum(maps:get(<<"type">>, Msg, <<>>)),
        from => to_sint64(maps:get(<<"from">>, Msg, maps:get(<<"from_id">>, Msg, 0))),
        to => to_sint64(maps:get(<<"to">>, Msg, maps:get(<<"to_id">>, Msg, 0))),
        msg_type => content_type_to_enum(maps:get(<<"msg_type">>, Msg, <<>>)),
        action => to_binary(maps:get(<<"action">>, Msg, <<>>)),
        e2ee => e2ee_to_pb(maps:get(<<"e2ee">>, Msg, null)),
        payload => ensure_binary(maps:get(<<"payload">>, Msg, <<>>)),
        created_at => to_int64(maps:get(<<"created_at">>, Msg, 0)),
        server_ts => to_int64(maps:get(<<"server_ts">>, Msg, 0)),
        expire_secs => to_int32(maps:get(<<"expire_secs">>, Msg, 0)),
        conv_seq => to_int64(maps:get(<<"conv_seq">>, Msg, 0))
    }.

%% @doc 将 protobuf atom-key map 转换为 binary-key map
%%
%% 逆向转换：gpb 解码后的 atom-key map → 现有代码使用的 binary-key map。
-spec from_pb_map(map()) -> map().
from_pb_map(PbMsg) ->
    E2EE = e2ee_from_pb(maps:get(e2ee, PbMsg, undefined)),
    #{
        <<"id">> => maps:get(id, PbMsg, <<>>),
        <<"type">> => msg_direction_from_enum(maps:get(type, PbMsg, 'MSG_DIRECTION_UNSPECIFIED')),
        <<"from">> => maps:get(from, PbMsg, 0),
        <<"to">> => maps:get(to, PbMsg, 0),
        <<"msg_type">> => content_type_from_enum(maps:get(msg_type, PbMsg, 'CONTENT_TYPE_UNSPECIFIED')),
        <<"action">> => maps:get(action, PbMsg, <<>>),
        <<"e2ee">> => E2EE,
        <<"payload">> => maps:get(payload, PbMsg, <<>>),
        <<"created_at">> => maps:get(created_at, PbMsg, 0),
        <<"server_ts">> => maps:get(server_ts, PbMsg, 0),
        <<"expire_secs">> => maps:get(expire_secs, PbMsg, 0),
        <<"conv_seq">> => maps:get(conv_seq, PbMsg, 0)
    }.


%% --- MsgDirection enum conversion ---

msg_direction_to_enum(<<"C2C">>) -> 'C2C';
msg_direction_to_enum(<<"C2G">>) -> 'C2G';
msg_direction_to_enum(<<"C2S">>) -> 'C2S';
msg_direction_to_enum(<<"S2C">>) -> 'S2C';
msg_direction_to_enum(<<"C2C_SERVER_ACK">>) -> 'C2C_SERVER_ACK';
msg_direction_to_enum(<<"C2G_SERVER_ACK">>) -> 'C2G_SERVER_ACK';
msg_direction_to_enum(<<"CLIENT_ACK">>) -> 'CLIENT_ACK';
msg_direction_to_enum(<<"CLIENT_ACK_CONFIRM">>) -> 'CLIENT_ACK_CONFIRM';
msg_direction_to_enum(<<"webrtc_offer">>) -> 'WEBRTC_OFFER';
msg_direction_to_enum(<<"webrtc_answer">>) -> 'WEBRTC_ANSWER';
msg_direction_to_enum(<<"webrtc_candidate">>) -> 'WEBRTC_CANDIDATE';
msg_direction_to_enum(<<"webrtc_bye">>) -> 'WEBRTC_BYE';
msg_direction_to_enum(Atom) when is_atom(Atom) -> Atom;
msg_direction_to_enum(_) -> 'MSG_DIRECTION_UNSPECIFIED'.

msg_direction_from_enum('C2C') -> <<"C2C">>;
msg_direction_from_enum('C2G') -> <<"C2G">>;
msg_direction_from_enum('C2S') -> <<"C2S">>;
msg_direction_from_enum('S2C') -> <<"S2C">>;
msg_direction_from_enum('C2C_SERVER_ACK') -> <<"C2C_SERVER_ACK">>;
msg_direction_from_enum('C2G_SERVER_ACK') -> <<"C2G_SERVER_ACK">>;
msg_direction_from_enum('CLIENT_ACK') -> <<"CLIENT_ACK">>;
msg_direction_from_enum('CLIENT_ACK_CONFIRM') -> <<"CLIENT_ACK_CONFIRM">>;
msg_direction_from_enum('WEBRTC_OFFER') -> <<"webrtc_offer">>;
msg_direction_from_enum('WEBRTC_ANSWER') -> <<"webrtc_answer">>;
msg_direction_from_enum('WEBRTC_CANDIDATE') -> <<"webrtc_candidate">>;
msg_direction_from_enum('WEBRTC_BYE') -> <<"webrtc_bye">>;
msg_direction_from_enum(_) -> <<>>.


%% --- ContentType enum conversion ---

content_type_to_enum(<<"text">>) -> 'TEXT';
content_type_to_enum(<<"image">>) -> 'IMAGE';
content_type_to_enum(<<"video">>) -> 'VIDEO';
content_type_to_enum(<<"audio">>) -> 'AUDIO';
content_type_to_enum(<<"file">>) -> 'FILE';
content_type_to_enum(<<"location">>) -> 'LOCATION';
content_type_to_enum(<<"custom">>) -> 'CUSTOM';
content_type_to_enum(<<"e2ee">>) -> 'E2EE';
content_type_to_enum(Atom) when is_atom(Atom) -> Atom;
content_type_to_enum(_) -> 'CONTENT_TYPE_UNSPECIFIED'.

content_type_from_enum('TEXT') -> <<"text">>;
content_type_from_enum('IMAGE') -> <<"image">>;
content_type_from_enum('VIDEO') -> <<"video">>;
content_type_from_enum('AUDIO') -> <<"audio">>;
content_type_from_enum('FILE') -> <<"file">>;
content_type_from_enum('LOCATION') -> <<"location">>;
content_type_from_enum('CUSTOM') -> <<"custom">>;
content_type_from_enum('E2EE') -> <<"e2ee">>;
content_type_from_enum(_) -> <<>>.


%% --- Payload type mapping ---

payload_pb_type(<<"text">>) -> 'PayloadText';
payload_pb_type(<<"image">>) -> 'PayloadImage';
payload_pb_type(<<"video">>) -> 'PayloadVideo';
payload_pb_type(<<"audio">>) -> 'PayloadAudio';
payload_pb_type(<<"file">>) -> 'PayloadFile';
payload_pb_type(<<"location">>) -> 'PayloadLocation';
payload_pb_type(<<"client_ack">>) -> 'PayloadClientAck';
payload_pb_type(<<"client_ack_confirm">>) -> 'PayloadClientAckConfirm';
payload_pb_type(_) -> undefined.


%% --- E2EE conversion ---

e2ee_to_pb(null) -> undefined;
e2ee_to_pb(undefined) -> undefined;
e2ee_to_pb(E2EE) when is_map(E2EE) ->
    Keys = [begin
        #{
            did => maps:get(<<"did">>, K, <<>>),
            kid => maps:get(<<"kid">>, K, <<>>),
            wrap_alg => maps:get(<<"wrap_alg">>, K, <<>>),
            ek => maps:get(<<"ek">>, K, <<>>)
        }
    end || K <- maps:get(<<"keys">>, E2EE, [])],
    #{
        ver => maps:get(<<"e2ee_ver">>, E2EE, 1),
        suite => maps:get(<<"e2ee_suite">>, E2EE, <<>>),
        nonce => maps:get(<<"nonce">>, E2EE, <<>>),
        keys => Keys
    };
e2ee_to_pb(_) -> undefined.

e2ee_from_pb(undefined) -> null;
e2ee_from_pb(#{ver := Ver, suite := Suite, nonce := Nonce, keys := Keys}) ->
    KeysList = [#{
        <<"did">> => maps:get(did, K, <<>>),
        <<"kid">> => maps:get(kid, K, <<>>),
        <<"wrap_alg">> => maps:get(wrap_alg, K, <<>>),
        <<"ek">> => maps:get(ek, K, <<>>)
    } || K <- Keys],
    #{
        <<"e2ee">> => true,
        <<"e2ee_ver">> => Ver,
        <<"e2ee_suite">> => Suite,
        <<"nonce">> => Nonce,
        <<"keys">> => KeysList
    };
e2ee_from_pb(_) -> null.


%% --- Type conversion helpers ---

to_sint64(V) when is_integer(V) -> V;
to_sint64(V) when is_binary(V) ->
    try binary_to_integer(V)
    catch _:_ -> 0
    end;
to_sint64(_) -> 0.

to_int64(V) when is_integer(V) -> V;
to_int64(V) when is_binary(V) ->
    try binary_to_integer(V)
    catch _:_ -> 0
    end;
to_int64(_) -> 0.

to_int32(V) when is_integer(V) -> V;
to_int32(V) when is_binary(V) ->
    try binary_to_integer(V)
    catch _:_ -> 0
    end;
to_int32(undefined) -> 0;
to_int32(_) -> 0.

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(_) -> <<>>.

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_map(V) ->
    try jsone:encode(V, [native_utf8])
    catch _:_ -> <<>>
    end;
ensure_binary(V) when is_list(V) ->
    try jsone:encode(V, [native_utf8])
    catch _:_ -> <<>>
    end;
ensure_binary(_) -> <<>>.
