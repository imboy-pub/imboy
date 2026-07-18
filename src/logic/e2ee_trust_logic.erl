-module(e2ee_trust_logic).
%%%
%% e2ee_trust_logic — 设备信任决策事件业务逻辑（ADR 06 §8.2.2 + §8.3）。
%%
%% 服务端职责边界（ADR 06 §8.1，零算法）：**不**计算 Safety Number、**不**验证 QR、
%% **不**判断 verification 是否成功。只做三件事：
%%   1. 校验状态转换合法（§3.2 白名单，非法抛 invalid_transition）；
%%   2. 验 actor_signature（Ed25519，防 T7 伪造他人 trust 事件；签名验证≠解密，不违反 ADR 02 §6）；
%%   3. 写 trust_audit（append-only）+ 广播 e2ee_trust_changed（§8.3）。
%%
%% 不写 user_device.trust_state：那是设备 owner 对**自身**设备的状态声明（ADR 03 §6 设备生命周期），
%% 与「A 信任 B」的事件流是两件事（ADR 06 §3.4/§8.2.1 显式区分）。本模块只记事件流。
%%%

-include("log.hrl").

-export([record_trust_event/10]).
-export([canonical_payload/6]).

%% 合法 method（ADR 06 §8.2.2）
-define(VALID_METHODS, [
    <<"qr_scan">>, <<"manual_number">>, <<"revoke">>, <<"device_destroyed">>
]).

%% ===================================================================
%% 记录信任决策事件
%% ===================================================================

%% @doc 记录一条信任决策事件。ActorUid = 决策发起方（current_uid）；
%%  ActorDeviceId = actor 用于签名的设备（查其 ed25519 公钥验签，不入审计表）。
%%  TsBin = 客户端签名时间戳（binary，纳入签名负载防重放）。
%%  ActorSignatureB64 = actor ed25519 私钥对 canonical_payload 的签名（base64）。
-spec record_trust_event(
    integer(),
    binary(),
    integer(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary()
) -> ok | {error, binary()}.
record_trust_event(
    ActorUid,
    ActorDeviceId,
    TargetUid,
    TargetDeviceId,
    TargetEd25519,
    FromState,
    ToState,
    Method,
    TsBin,
    ActorSignatureB64
) when
    is_integer(ActorUid), is_integer(TargetUid)
->
    case
        valid_nonempty([
            ActorDeviceId, TargetDeviceId, TargetEd25519, TsBin, ActorSignatureB64
        ]) andalso
            lists:member(Method, ?VALID_METHODS)
    of
        false ->
            {error, <<"bad_request">>};
        true ->
            case valid_transition(FromState, ToState) of
                false ->
                    {error, <<"invalid_transition">>};
                true ->
                    verify_and_record(
                        ActorUid,
                        ActorDeviceId,
                        TargetUid,
                        TargetDeviceId,
                        TargetEd25519,
                        FromState,
                        ToState,
                        Method,
                        TsBin,
                        ActorSignatureB64
                    )
            end
    end;
record_trust_event(_, _, _, _, _, _, _, _, _, _) ->
    {error, <<"bad_request">>}.

-spec verify_and_record(
    integer(),
    binary(),
    integer(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary()
) -> ok | {error, binary()}.
verify_and_record(
    ActorUid,
    ActorDeviceId,
    TargetUid,
    TargetDeviceId,
    TargetEd25519,
    FromState,
    ToState,
    Method,
    TsBin,
    ActorSignatureB64
) ->
    %% 取 actor 该设备的 ed25519 公钥（验签用）
    case olm_identity_ds:find_identity(ActorUid, ActorDeviceId) of
        {ok, not_found} ->
            {error, <<"actor_device_not_registered">>};
        {ok, #{<<"ed25519_key">> := ActorEd25519B64}} ->
            Canonical = canonical_payload(
                TargetUid, TargetDeviceId, TargetEd25519, FromState, ToState, TsBin
            ),
            case verify_signature(ActorEd25519B64, Canonical, ActorSignatureB64) of
                false ->
                    %% 验签失败：拒写 + 拒广播（ADR 06 §9.4 T-06-12，防 T7 伪造）
                    {error, <<"invalid_signature">>};
                true ->
                    write_and_broadcast(
                        ActorUid,
                        TargetUid,
                        TargetDeviceId,
                        TargetEd25519,
                        FromState,
                        ToState,
                        Method,
                        ActorSignatureB64,
                        TsBin
                    )
            end;
        {error, Reason} ->
            _ = ?ERROR_LOG({trust_actor_identity_error, ActorUid, ActorDeviceId, Reason}),
            {error, <<"internal_error">>}
    end.

-spec write_and_broadcast(
    integer(),
    integer(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary()
) -> ok | {error, binary()}.
write_and_broadcast(
    ActorUid,
    TargetUid,
    TargetDeviceId,
    TargetEd25519,
    FromState,
    ToState,
    Method,
    ActorSignatureB64,
    TsBin
) ->
    case
        trust_audit_ds:insert_event(
            ActorUid,
            TargetUid,
            TargetDeviceId,
            TargetEd25519,
            FromState,
            ToState,
            Method,
            ActorSignatureB64
        )
    of
        {ok, _} ->
            broadcast_trust_changed(
                ActorUid, TargetUid, TargetDeviceId, ToState, Method, TsBin
            ),
            ok;
        {error, Reason} ->
            _ = ?ERROR_LOG({trust_audit_insert_error, ActorUid, TargetUid, Reason}),
            {error, <<"internal_error">>}
    end.

%% ===================================================================
%% 广播 e2ee_trust_changed（ADR 06 §8.3）
%% ===================================================================

%% @doc 广播信任变更。to_state=revoked 时同时广播给对端用户（§8.3），
%%  其余仅同步给本账号其他设备（多设备同步）。payload 服务端不解释，透传。
-spec broadcast_trust_changed(integer(), integer(), binary(), binary(), binary(), binary()) -> ok.
broadcast_trust_changed(ActorUid, TargetUid, TargetDeviceId, ToState, Method, TsBin) ->
    Payload = #{
        <<"actor_uid">> => ActorUid,
        <<"target_uid">> => TargetUid,
        <<"target_device_id">> => TargetDeviceId,
        <<"to_state">> => ToState,
        <<"method">> => Method,
        <<"ts">> => TsBin
    },
    ToUids =
        case ToState of
            <<"revoked">> -> [ActorUid, TargetUid];
            _ -> [ActorUid]
        end,
    _ = msg_s2c_ds:send(ActorUid, ToUids, <<"e2ee_trust_changed">>, <<>>, null, Payload, save),
    ok.

%% ===================================================================
%% 冻结的 canonical 签名负载（ADR 03 §4.1 约定：字段字典序，确定性字节流）
%% ===================================================================

%% @doc 构造 actor 签名覆盖的规范字节流。字段按键字典序、`key=value` 换行拼接，
%%  确定性、无库依赖（避免 JSON 编码器键序不定）。客户端须用同格式签名。
%%  签名字段：from_state / target_device_id / target_ed25519 / target_uid / to_state / ts。
-spec canonical_payload(integer(), binary(), binary(), binary(), binary(), binary()) -> binary().
canonical_payload(TargetUid, TargetDeviceId, TargetEd25519, FromState, ToState, TsBin) ->
    <<
        "from_state=",
        FromState/binary,
        "\n",
        "target_device_id=",
        TargetDeviceId/binary,
        "\n",
        "target_ed25519=",
        TargetEd25519/binary,
        "\n",
        "target_uid=",
        (integer_to_binary(TargetUid))/binary,
        "\n",
        "to_state=",
        ToState/binary,
        "\n",
        "ts=",
        TsBin/binary
    >>.

%% ===================================================================
%% 内部：状态转换白名单 + 验签 + 校验
%% ===================================================================

%% ADR 06 §3.2 合法转换白名单；非法（含 revoked→verified 绕过、同态跳变、未知态）返回 false。
-spec valid_transition(binary(), binary()) -> boolean().
valid_transition(<<"unverified">>, <<"verified">>) -> true;
valid_transition(<<"verified">>, <<"unverified">>) -> true;
valid_transition(<<"unverified">>, <<"revoked">>) -> true;
valid_transition(<<"verified">>, <<"revoked">>) -> true;
%% revoked→unverified：仅 identity 键变更时合法（客户端职责，服务端只放行该对）
valid_transition(<<"revoked">>, <<"unverified">>) -> true;
valid_transition(_, _) -> false.

-spec verify_signature(binary(), binary(), binary()) -> boolean().
verify_signature(Ed25519B64, Canonical, SignatureB64) ->
    try
        PubKey = base64:decode(Ed25519B64),
        Sig = base64:decode(SignatureB64),
        crypto:verify(eddsa, none, Canonical, Sig, [PubKey, ed25519])
    catch
        _:_ -> false
    end.

-spec valid_nonempty([binary()]) -> boolean().
valid_nonempty(List) ->
    lists:all(fun(B) -> is_binary(B) andalso byte_size(B) > 0 end, List).
