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
-include("imboy_frame.hrl").

%% API
-export([
    encode/2,
    decode/2,
    encode_payload/3,
    decode_payload/3,
    encode_ws_frame/2,
    encode_ws_msg/4,
    e2ee_pb_lossless/1,
    protocol_atom/1,
    framing_atom/1,
    wrap_v2_frame/3,
    unwrap_v2_frame/1,
    is_json_channel_type/1
]).

%% 协议类型
-type protocol() :: json | protobuf.
-type framing() :: none | v2.
-export_type([protocol/0, framing/0]).

%% protobuf E2EEMeta（proto/imboy.proto §128）能承载的 JSON 字段全集。
%% 与 e2ee_to_pb/1 + e2ee_from_pb/1 的读写字段一一对应，改动其一必须同步改这里。
-define(E2EE_PB_FIELDS, [
    <<"e2ee">>,
    <<"e2ee_ver">>,
    <<"e2ee_suite">>,
    <<"nonce">>,
    <<"keys">>,
    <<"protocol">>,
    <<"version">>,
    <<"peer_uid">>,
    <<"peer_device_id">>,
    <<"message_type">>,
    <<"session_id">>,
    <<"message_index">>,
    <<"group_id">>,
    <<"meta_version">>
]).

%% protobuf E2EEDeviceKey 能承载的字段全集
-define(E2EE_PB_KEY_FIELDS, [<<"did">>, <<"kid">>, <<"wrap_alg">>, <<"ek">>]).

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
    try
        jsone:encode(Msg, [native_utf8])
    catch
        Class:Reason ->
            ok = ?WARN_LOG({codec_json_encode_error, Class, Reason}),
            <<"{}">>
    end;
encode(protobuf, Msg) when is_map(Msg) ->
    case e2ee_pb_lossless(Msg) of
        false ->
            %% ADR 15 §10 fail-closed：protobuf 的 E2EEMeta schema 装不下该
            %% e2ee 信封（PFv3 的 protected_header/header_hash/ciphertext/
            %% protocol_metadata/devices 均无对应字段）。宁可拒绝编码，也不
            %% 投递被裁剪的信封——服务端不持有密钥，接收端一旦收到裁剪结果
            %% 即永久不可解密，且无法事后修复。调用方须退回 JSON。
            ok = ?WARN_LOG({codec_pb_encode_refused_lossy_e2ee, maps:get(<<"id">>, Msg, unknown)}),
            <<>>;
        true ->
            try
                PbMsg = to_pb_map(Msg),
                imboy_pb:encode_msg(PbMsg, 'IMBoyMessage')
            catch
                Class:Reason ->
                    ok = ?WARN_LOG({codec_pb_encode_error, Class, Reason}),
                    <<>>
            end
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
    try
        jsone:decode(Data, [{object_format, map}])
    catch
        Class:Reason ->
            ok = ?WARN_LOG({codec_json_decode_error, Class, Reason}),
            #{}
    end;
decode(protobuf, Data) when is_binary(Data) ->
    try
        PbMsg = imboy_pb:decode_msg(Data, 'IMBoyMessage'),
        from_pb_map(PbMsg)
    catch
        Class:Reason ->
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
    try
        jsone:decode(Payload, [{object_format, map}])
    catch
        _:Err ->
            ok = ?WARN_LOG({codec_json_decode_fallback, Err}),
            #{<<"raw">> => Payload}
    end;
decode_payload(protobuf, MsgType, PayloadBin) when
    is_binary(PayloadBin), byte_size(PayloadBin) > 0
->
    PbType = payload_pb_type(MsgType),
    case PbType of
        undefined ->
            #{<<"raw">> => PayloadBin};
        _ ->
            try
                imboy_pb:decode_msg(PayloadBin, PbType)
            catch
                Class:Reason ->
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

%% @doc 按连接的协议与 framing 把消息编成 WebSocket 帧，并保证 E2EE 信封无损
%%
%% protobuf 的 E2EEMeta schema 无法表达 PFv3 外层信封。遇到这类消息一律
%% 退回 byte-preserving 的 JSON：v2 framing 的 payload 本就允许 JSON 原文
%% （见 websocket_handler:encode_delivery_frame_v2/1），非 v2 的 protobuf
%% 连接同样已有 JSON text 帧回退先例（encode_delivery_frame_protobuf/1）。
%%
%% @param FrameType v2 framing 的帧类型；非 v2 时忽略
-spec encode_ws_msg(protocol(), framing(), 0..255, map()) ->
    {text, binary()} | {binary, binary()}.
encode_ws_msg(protobuf, v2, FrameType, Msg) when is_map(Msg) ->
    Encoded =
        case pb_lossless(Msg) of
            true -> encode(protobuf, Msg);
            false -> encode(json, Msg)
        end,
    {binary, wrap_v2_frame(FrameType, 0, Encoded)};
encode_ws_msg(protobuf, _Framing, _FrameType, Msg) when is_map(Msg) ->
    case pb_lossless(Msg) of
        true -> encode_ws_frame(protobuf, encode(protobuf, Msg));
        false -> encode_ws_frame(json, encode(json, Msg))
    end;
encode_ws_msg(Protocol, _Framing, _FrameType, Msg) when is_map(Msg) ->
    encode_ws_frame(Protocol, encode(Protocol, Msg)).

%% @doc IMBoyMessage schema 能否无损承载整条消息
%%
%% 除 e2ee 信封外还有两个漏斗，都会让客户端收到无法关联的空壳帧：
%%   1. type 不在 MsgDirection 枚举内（如 CLIENT_ACK_ERROR）→ to_pb_map
%%      压成 MSG_DIRECTION_UNSPECIFIED，客户端认不出这是什么响应；
%%   2. to_pb_map 只搬 schema 里的字段，控制帧的 in_reply_to / reason
%%      被静默丢弃 → 客户端无法把 ACK_ERROR 关联回原消息，确认超时
%%      重发，形成刷屏死循环。
%% 命中任一即退回 JSON：v2 frame 的 payload 本就允许 JSON 原文，客户端
%% protobuf 解码失败后按 UTF-8 JSON 解析（与 e2ee 超纲同款回退）。
-spec pb_lossless(map()) -> boolean().
pb_lossless(Msg) when is_map(Msg) ->
    e2ee_pb_lossless(Msg) andalso
        direction_pb_lossless(Msg) andalso
        ctrl_fields_pb_lossless(Msg);
pb_lossless(_) ->
    true.

%% @private type 非空且不落在 MsgDirection 枚举内 → 不可无损表达
direction_pb_lossless(Msg) ->
    case maps:get(<<"type">>, Msg, <<>>) of
        <<>> -> true;
        Type -> msg_direction_to_enum(Type) =/= 'MSG_DIRECTION_UNSPECIFIED'
    end.

%% @private to_pb_map 没有对应字段、会被丢弃的控制帧键
%% ponytail: 只列当前已知被丢的两个键；将来 to_pb_map 增字段时同步维护，
%% 若丢字段的场景变多，改成对 schema 字段做白名单更省心。
ctrl_fields_pb_lossless(Msg) ->
    not (maps:is_key(<<"in_reply_to">>, Msg) orelse maps:is_key(<<"reason">>, Msg)).

%% @doc protobuf 的 E2EEMeta 能否无损承载该消息的 e2ee 信封
%%
%% 判据是白名单：e2ee map（含 keys 列表元素）出现任何 E2EEMeta 没有的字段，
%% 即视为不可无损表达。这样 PFv3 字段与一切未来扩展字段都被自动覆盖，
%% 不需要随协议演进逐个补判断。
-spec e2ee_pb_lossless(map()) -> boolean().
e2ee_pb_lossless(Msg) when is_map(Msg) ->
    case maps:get(<<"e2ee">>, Msg, null) of
        E2EE when is_map(E2EE) ->
            no_extra_keys(E2EE, ?E2EE_PB_FIELDS) andalso
                keys_pb_lossless(maps:get(<<"keys">>, E2EE, []));
        _ ->
            true
    end;
e2ee_pb_lossless(_) ->
    true.

%% @private
keys_pb_lossless(Keys) when is_list(Keys) ->
    lists:all(
        fun(K) -> is_map(K) andalso no_extra_keys(K, ?E2EE_PB_KEY_FIELDS) end,
        Keys
    );
keys_pb_lossless(_) ->
    false.

%% @private
no_extra_keys(M, Allowed) ->
    maps:size(maps:without(Allowed, M)) =:= 0.

%% @doc 将子协议字符串转换为协议原子
%%
%% @param SubProtocol WebSocket 子协议字符串
%% @returns 协议原子
-spec protocol_atom(binary() | undefined) -> protocol().
protocol_atom(<<"imboy.v2">>) -> protobuf;
protocol_atom(<<"imboy-protobuf">>) -> protobuf;
protocol_atom(<<"imboy-json">>) -> json;
protocol_atom(<<"text">>) -> json;
protocol_atom(_) -> json.

%% @doc 将子协议字符串转换为 framing 原子
%%
%% v2 分层二进制协议对应的 framing=v2，其余为 none
-spec framing_atom(binary() | undefined) -> framing().
framing_atom(<<"imboy.v2">>) -> v2;
framing_atom(_) -> none.

%% @doc v2 framing 封包：调用 imboy_frame:encode/3 包裹 payload
-spec wrap_v2_frame(0..255, 0..255, binary()) -> binary().
wrap_v2_frame(Type, Flags, Payload) when
    is_integer(Type), is_integer(Flags), is_binary(Payload)
->
    imboy_frame:encode(Type, Flags, Payload).

%% @doc v2 framing 解包：调用 imboy_frame:decode/1，丢弃 Rest 字节
%%
%% 返回 {ok, Frame} 或 {error, Reason}
-spec unwrap_v2_frame(binary()) ->
    {ok, imboy_frame:frame()} | {error, atom()}.
unwrap_v2_frame(Bin) when is_binary(Bin) ->
    case imboy_frame:decode(Bin) of
        {ok, Frame, _Rest} -> {ok, Frame};
        {more, _} -> {error, incomplete_frame};
        {error, Reason} -> {error, Reason}
    end.

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
        <<"msg_type">> => content_type_from_enum(
            maps:get(msg_type, PbMsg, 'CONTENT_TYPE_UNSPECIFIED')
        ),
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

%% @doc JSON 通道专属 msg_type 识别
%%
%% agent_task / a2a_task_update / stream_delta 均为 JSON 通道消息类型，
%% 不进入 protobuf enum（无 proto 变更），在 JSON 通道由 msg_type binary
%% 原样透传。业务层可据此判定是否为 JSON-only 内容类型（渲染/路由 gate）。
-spec is_json_channel_type(binary()) -> boolean().
is_json_channel_type(<<"agent_task">>) -> true;
is_json_channel_type(<<"a2a_task_update">>) -> true;
is_json_channel_type(<<"stream_delta">>) -> true;
is_json_channel_type(_) -> false.

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

e2ee_to_pb(null) ->
    undefined;
e2ee_to_pb(undefined) ->
    undefined;
e2ee_to_pb(E2EE) when is_map(E2EE) ->
    Keys = [
        begin
            #{
                did => maps:get(<<"did">>, K, <<>>),
                kid => maps:get(<<"kid">>, K, <<>>),
                wrap_alg => maps:get(<<"wrap_alg">>, K, <<>>),
                ek => maps:get(<<"ek">>, K, <<>>)
            }
        end
     || K <- maps:get(<<"keys">>, E2EE, [])
    ],
    Base = #{
        ver => maps:get(<<"e2ee_ver">>, E2EE, 1),
        suite => maps:get(<<"e2ee_suite">>, E2EE, <<>>),
        nonce => maps:get(<<"nonce">>, E2EE, <<>>),
        keys => Keys
    },
    %% v2 Olm/Megolm/v3 扩展字段（仅非默认值写入，gpb omitted 语义）
    e2ee_to_pb_ext(Base, E2EE);
e2ee_to_pb(_) ->
    undefined.

%% @doc 条件写入 v2 扩展字段（仅非默认值）
e2ee_to_pb_ext(M, E2EE) ->
    M1 =
        case maps:get(<<"protocol">>, E2EE, <<>>) of
            <<>> -> M;
            V1 -> M#{protocol => V1}
        end,
    M2 =
        case maps:get(<<"version">>, E2EE, 0) of
            0 -> M1;
            V2 -> M1#{protocol_version => V2}
        end,
    M3 =
        case maps:get(<<"peer_uid">>, E2EE, <<>>) of
            <<>> -> M2;
            V3 -> M2#{peer_uid => V3}
        end,
    M4 =
        case maps:get(<<"peer_device_id">>, E2EE, <<>>) of
            <<>> -> M3;
            V4 -> M3#{peer_device_id => V4}
        end,
    M5 =
        case maps:get(<<"message_type">>, E2EE, 0) of
            0 -> M4;
            V5 -> M4#{message_type => V5}
        end,
    M6 =
        case maps:get(<<"session_id">>, E2EE, <<>>) of
            <<>> -> M5;
            V6 -> M5#{session_id => V6}
        end,
    M7 =
        case maps:get(<<"message_index">>, E2EE, 0) of
            0 -> M6;
            V7 -> M6#{message_index => V7}
        end,
    M8 =
        case maps:get(<<"group_id">>, E2EE, <<>>) of
            <<>> -> M7;
            V8 -> M7#{group_id => V8}
        end,
    case maps:get(<<"meta_version">>, E2EE, 0) of
        0 -> M8;
        V9 -> M8#{meta_version => V9}
    end.

e2ee_from_pb(undefined) ->
    null;
e2ee_from_pb(Pb) when is_map(Pb) ->
    Keys = maps:get(keys, Pb, []),
    KeysList = [
        #{
            <<"did">> => maps:get(did, K, <<>>),
            <<"kid">> => maps:get(kid, K, <<>>),
            <<"wrap_alg">> => maps:get(wrap_alg, K, <<>>),
            <<"ek">> => maps:get(ek, K, <<>>)
        }
     || K <- Keys
    ],
    Base = #{
        <<"e2ee">> => true,
        <<"e2ee_ver">> => maps:get(ver, Pb, 0),
        <<"e2ee_suite">> => maps:get(suite, Pb, <<>>),
        <<"nonce">> => maps:get(nonce, Pb, <<>>),
        <<"keys">> => KeysList
    },
    %% v2 扩展字段（仅非默认值写回 JSON map）
    e2ee_from_pb_ext(Base, Pb);
e2ee_from_pb(_) ->
    null.

%% @doc 条件回写 v2 扩展字段到 JSON binary-key map
e2ee_from_pb_ext(M, Pb) ->
    M1 =
        case maps:get(protocol, Pb, <<>>) of
            <<>> -> M;
            V1 -> M#{<<"protocol">> => V1}
        end,
    M2 =
        case maps:get(protocol_version, Pb, 0) of
            0 -> M1;
            V2 -> M1#{<<"version">> => V2}
        end,
    M3 =
        case maps:get(peer_uid, Pb, <<>>) of
            <<>> -> M2;
            V3 -> M2#{<<"peer_uid">> => V3}
        end,
    M4 =
        case maps:get(peer_device_id, Pb, <<>>) of
            <<>> -> M3;
            V4 -> M3#{<<"peer_device_id">> => V4}
        end,
    M5 =
        case maps:get(message_type, Pb, 0) of
            0 -> M4;
            V5 -> M4#{<<"message_type">> => V5}
        end,
    M6 =
        case maps:get(session_id, Pb, <<>>) of
            <<>> -> M5;
            V6 -> M5#{<<"session_id">> => V6}
        end,
    M7 =
        case maps:get(message_index, Pb, 0) of
            0 -> M6;
            V7 -> M6#{<<"message_index">> => V7}
        end,
    M8 =
        case maps:get(group_id, Pb, <<>>) of
            <<>> -> M7;
            V8 -> M7#{<<"group_id">> => V8}
        end,
    case maps:get(meta_version, Pb, 0) of
        0 -> M8;
        V9 -> M8#{<<"meta_version">> => V9}
    end.

%% --- Type conversion helpers ---

to_sint64(V) when is_integer(V) -> V;
to_sint64(V) when is_binary(V) ->
    try
        binary_to_integer(V)
    catch
        _:Err ->
            ok = ?WARN_LOG({to_sint64_fallback, V, Err}),
            0
    end;
to_sint64(_) ->
    0.

to_int64(V) when is_integer(V) -> V;
to_int64(V) when is_binary(V) ->
    try
        binary_to_integer(V)
    catch
        _:Err ->
            ok = ?WARN_LOG({to_int64_fallback, V, Err}),
            0
    end;
to_int64(_) ->
    0.

to_int32(V) when is_integer(V) -> V;
to_int32(V) when is_binary(V) ->
    try
        binary_to_integer(V)
    catch
        _:Err ->
            ok = ?WARN_LOG({to_int32_fallback, V, Err}),
            0
    end;
to_int32(undefined) ->
    0;
to_int32(_) ->
    0.

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(_) -> <<>>.

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_map(V) ->
    try
        jsone:encode(V, [native_utf8])
    catch
        _:Err ->
            ok = ?WARN_LOG({ensure_binary_map_fallback, Err}),
            <<>>
    end;
ensure_binary(V) when is_list(V) ->
    try
        jsone:encode(V, [native_utf8])
    catch
        _:Err ->
            ok = ?WARN_LOG({ensure_binary_list_fallback, Err}),
            <<>>
    end;
ensure_binary(_) ->
    <<>>.
