-module(e2ee_trust_logic).
%%%
%% e2ee_trust_logic — 设备信任决策事件业务逻辑（ADR 06 §8.2.2 + ADR 16 §3.3.1）。
%%
%% 服务端职责边界（ADR 06 §8.1，零算法）：**不**计算 Safety Number、**不**验证 QR、
%% **不**判断 verification 是否成功。只做：
%%   1. 校验状态转换合法（§3.2 白名单，非法抛 invalid_transition）；
%%   2. E2EE-014 freshness：issued_at/expires_at 时间窗，拒绝过期/未来超窗（防重放）；
%%   3. 验 actor_signature（Ed25519，签名覆盖 §3.3.1 全部 11 字段，防 T7 伪造）；
%%   4. 校验 actor 设备 active + device_generation 匹配（撤销/旧设备重放拒绝）；
%%   5. 校验 target_identity_version 单调不回退（防对端身份键回退攻击）；
%%   6. event_id 幂等写入（同 event_id 重放返回原语义结果，不重复审计/广播）；
%%   7. 写 trust_audit（append-only）+ 广播 e2ee_trust_changed（§8.3）。
%%
%% 签名验证是身份认证级密码学（≠ E2EE payload 解密），不违反 ADR 02 §6 零密码学原则。
%% 不写 user_device.trust_state：那是设备 owner 对自身设备的状态声明（ADR 03 §6），
%% 与「A 信任 B」的事件流是两件事（ADR 06 §3.4）。本模块只记事件流。
%%%

-include("log.hrl").

-export([record_trust_event/2]).
-export([canonical_payload/1]).

%% 合法 method（ADR 06 §8.2.2）
-define(VALID_METHODS, [
    <<"qr_scan">>, <<"manual_number">>, <<"revoke">>, <<"device_destroyed">>
]).

%% E2EE-014 freshness 时间窗（ms，ADR 16 §3.3.1：TTL 5min、skew ±2min）
-define(FRESH_PAST_MS, 300000).
-define(FRESH_FUTURE_MS, 120000).
-define(MAX_TTL_MS, 300000).

%% ===================================================================
%% 记录信任决策事件（ADR 16 §3.3.1）
%% ===================================================================

%% @doc 记录一条信任决策事件。ActorUid = 决策发起方（已认证 current_uid，权威）。
%%  F = 请求体字段 map（见 §3.3.1）：actor_device_id / target_uid / target_device_id /
%%  target_ed25519 / from_state / to_state / method / event_id / issued_at / expires_at /
%%  actor_device_generation / target_identity_version / actor_signature。
%%  数值字段接受 binary 或 integer，内部统一为 integer。
-spec record_trust_event(integer(), map()) -> ok | {error, binary()}.
record_trust_event(ActorUid, F) when is_integer(ActorUid), is_map(F) ->
    case normalize(ActorUid, F) of
        {error, Reason} ->
            {error, Reason};
        {ok, N} ->
            with_transition(N)
    end;
record_trust_event(_, _) ->
    {error, <<"bad_request">>}.

%% 规范化 + 基础校验（非空、method、数值、target_uid>0）
-spec normalize(integer(), map()) -> {ok, map()} | {error, binary()}.
normalize(ActorUid, F) ->
    ActorDeviceId = maps:get(<<"actor_device_id">>, F, <<>>),
    TargetUid = to_int(maps:get(<<"target_uid">>, F, 0)),
    TargetDeviceId = maps:get(<<"target_device_id">>, F, <<>>),
    TargetEd25519 = maps:get(<<"target_ed25519">>, F, <<>>),
    FromState = maps:get(<<"from_state">>, F, <<>>),
    ToState = maps:get(<<"to_state">>, F, <<>>),
    Method = maps:get(<<"method">>, F, <<>>),
    EventId = maps:get(<<"event_id">>, F, <<>>),
    IssuedAt = to_int(maps:get(<<"issued_at">>, F, -1)),
    ExpiresAt = to_int(maps:get(<<"expires_at">>, F, -1)),
    ActorGen = to_int(maps:get(<<"actor_device_generation">>, F, -1)),
    TargetVer = to_int(maps:get(<<"target_identity_version">>, F, -1)),
    ActorSig = maps:get(<<"actor_signature">>, F, <<>>),
    NonEmptyOk = valid_nonempty([
        ActorDeviceId, TargetDeviceId, TargetEd25519, EventId, ActorSig
    ]),
    IntsOk =
        is_integer(TargetUid) andalso TargetUid > 0 andalso
            is_integer(IssuedAt) andalso IssuedAt >= 0 andalso
            is_integer(ExpiresAt) andalso ExpiresAt >= 0 andalso
            is_integer(ActorGen) andalso ActorGen >= 0 andalso
            is_integer(TargetVer) andalso TargetVer >= 0,
    case
        NonEmptyOk andalso IntsOk andalso lists:member(Method, ?VALID_METHODS) andalso
            byte_size(EventId) =< 64
    of
        false ->
            {error, <<"bad_request">>};
        true ->
            {ok, #{
                actor_uid => ActorUid,
                actor_device_id => ActorDeviceId,
                target_uid => TargetUid,
                target_device_id => TargetDeviceId,
                target_ed25519 => TargetEd25519,
                from_state => FromState,
                to_state => ToState,
                method => Method,
                event_id => EventId,
                issued_at => IssuedAt,
                expires_at => ExpiresAt,
                actor_device_generation => ActorGen,
                target_identity_version => TargetVer,
                actor_signature => ActorSig
            }}
    end.

-spec with_transition(map()) -> ok | {error, binary()}.
with_transition(N) ->
    case valid_transition(maps:get(from_state, N), maps:get(to_state, N)) of
        false ->
            {error, <<"invalid_transition">>};
        true ->
            case fresh(maps:get(issued_at, N), maps:get(expires_at, N)) of
                false ->
                    %% 单一通用错误，不区分过期/未来/TTL，避免时间 oracle
                    {error, <<"stale_event">>};
                true ->
                    verify_and_record(N)
            end
    end.

-spec verify_and_record(map()) -> ok | {error, binary()}.
verify_and_record(N) ->
    ActorUid = maps:get(actor_uid, N),
    ActorDeviceId = maps:get(actor_device_id, N),
    case olm_identity_ds:find_identity(ActorUid, ActorDeviceId) of
        {ok, not_found} ->
            {error, <<"actor_device_not_registered">>};
        {ok, #{<<"ed25519_key">> := ActorEd25519B64}} ->
            case
                actor_device_active(ActorUid, ActorDeviceId, maps:get(actor_device_generation, N))
            of
                false ->
                    %% 撤销/禁用 actor 或 device_generation 失配（旧设备重放）
                    {error, <<"actor_device_revoked">>};
                true ->
                    verify_sig_then_write(N, ActorEd25519B64)
            end;
        {error, Reason} ->
            _ = ?ERROR_LOG({trust_actor_identity_error, ActorUid, ActorDeviceId, Reason}),
            {error, <<"internal_error">>}
    end.

-spec verify_sig_then_write(map(), binary()) -> ok | {error, binary()}.
verify_sig_then_write(N, ActorEd25519B64) ->
    Canonical = canonical_payload(signed_fields(N)),
    case verify_signature(ActorEd25519B64, Canonical, maps:get(actor_signature, N)) of
        false ->
            {error, <<"invalid_signature">>};
        true ->
            write_and_broadcast(N)
    end.

%% 原子写入：版本单调校验（防身份键回退）+ event_id 幂等 + 冲突归属核对，
%% 全部在 trust_audit_repo:insert_event/1 的单事务内完成（advisory 锁串行化，
%% 闭合 TOCTOU 与 event_id 抢占——见 repo 层文档）。
%% 业务语义错误（binary）直接透传给 handler；DB 层错误（其它）折成 internal_error。
-spec write_and_broadcast(map()) -> ok | {error, binary()}.
write_and_broadcast(N) ->
    case trust_audit_ds:insert_event(N) of
        {ok, inserted} ->
            broadcast_trust_changed(N),
            ok;
        {ok, duplicate} ->
            %% 同 event_id 合法重放：幂等返回原语义结果，不重复审计、不重播（E2EE-014）
            ok;
        {error, Reason} when is_binary(Reason) ->
            %% identity_version_rollback / event_id_conflict：语义拒绝，不泄漏细节
            {error, Reason};
        {error, Reason} ->
            _ = ?ERROR_LOG({trust_audit_insert_error, maps:get(actor_uid, N), Reason}),
            {error, <<"internal_error">>}
    end.

%% ===================================================================
%% 广播 e2ee_trust_changed（ADR 06 §8.3）
%% ===================================================================

-spec broadcast_trust_changed(map()) -> ok.
broadcast_trust_changed(N) ->
    ActorUid = maps:get(actor_uid, N),
    TargetUid = maps:get(target_uid, N),
    ToState = maps:get(to_state, N),
    Payload = #{
        <<"actor_uid">> => ActorUid,
        <<"target_uid">> => TargetUid,
        <<"target_device_id">> => maps:get(target_device_id, N),
        <<"to_state">> => ToState,
        <<"method">> => maps:get(method, N),
        <<"event_id">> => maps:get(event_id, N),
        <<"issued_at">> => maps:get(issued_at, N)
    },
    ToUids =
        case ToState of
            <<"revoked">> -> [ActorUid, TargetUid];
            _ -> [ActorUid]
        end,
    _ = msg_s2c_ds:send(ActorUid, ToUids, <<"e2ee_trust_changed">>, <<>>, null, Payload, save),
    ok.

%% ===================================================================
%% 冻结的 canonical 签名负载（ADR 16 §3.3.1：字段字典序，key=value\n，确定性字节流）
%% ===================================================================

%% @doc 构造 actor 签名覆盖的规范字节流。11 字段按 ASCII 字典序、`key=value` 换行拼接，
%%  末字段无尾随换行；整数十进制。客户端须用同格式签名。编码收敛见 ADR 16 §3.3.1
%%  （沿用 key=value\n，不采用 CBOR）。
-spec canonical_payload(map()) -> binary().
canonical_payload(#{
    <<"actor_device_generation">> := ADG,
    <<"actor_uid">> := AU,
    <<"event_id">> := EID,
    <<"expires_at">> := EX,
    <<"from_state">> := FS,
    <<"issued_at">> := IA,
    <<"target_device_id">> := TD,
    <<"target_ed25519">> := TE,
    <<"target_identity_version">> := TIV,
    <<"target_uid">> := TU,
    <<"to_state">> := TS
}) ->
    <<
        "actor_device_generation=",
        (i2b(ADG))/binary,
        "\n",
        "actor_uid=",
        (i2b(AU))/binary,
        "\n",
        "event_id=",
        EID/binary,
        "\n",
        "expires_at=",
        (i2b(EX))/binary,
        "\n",
        "from_state=",
        FS/binary,
        "\n",
        "issued_at=",
        (i2b(IA))/binary,
        "\n",
        "target_device_id=",
        TD/binary,
        "\n",
        "target_ed25519=",
        TE/binary,
        "\n",
        "target_identity_version=",
        (i2b(TIV))/binary,
        "\n",
        "target_uid=",
        (i2b(TU))/binary,
        "\n",
        "to_state=",
        TS/binary
    >>.

%% 从规范化事件抽取 canonical 签名字段（binary key，供 canonical_payload/1）
-spec signed_fields(map()) -> map().
signed_fields(N) ->
    #{
        <<"actor_device_generation">> => maps:get(actor_device_generation, N),
        <<"actor_uid">> => maps:get(actor_uid, N),
        <<"event_id">> => maps:get(event_id, N),
        <<"expires_at">> => maps:get(expires_at, N),
        <<"from_state">> => maps:get(from_state, N),
        <<"issued_at">> => maps:get(issued_at, N),
        <<"target_device_id">> => maps:get(target_device_id, N),
        <<"target_ed25519">> => maps:get(target_ed25519, N),
        <<"target_identity_version">> => maps:get(target_identity_version, N),
        <<"target_uid">> => maps:get(target_uid, N),
        <<"to_state">> => maps:get(to_state, N)
    }.

%% ===================================================================
%% 内部：状态转换白名单 + freshness + 验签 + actor 校验
%% ===================================================================

%% ADR 06 §3.2 合法转换白名单；非法（含 revoked→verified 绕过、同态跳变、未知态）返回 false。
-spec valid_transition(binary(), binary()) -> boolean().
valid_transition(<<"unverified">>, <<"verified">>) -> true;
valid_transition(<<"verified">>, <<"unverified">>) -> true;
valid_transition(<<"unverified">>, <<"revoked">>) -> true;
valid_transition(<<"verified">>, <<"revoked">>) -> true;
valid_transition(<<"revoked">>, <<"unverified">>) -> true;
valid_transition(_, _) -> false.

%% E2EE-014 freshness：issued_at 在 [now-past, now+future]，expires_at 在 (issued_at, issued_at+ttl] 且未过期
-spec fresh(integer(), integer()) -> boolean().
fresh(IssuedAt, ExpiresAt) ->
    Now = elib_dt:millisecond(),
    IssuedAt >= Now - ?FRESH_PAST_MS andalso
        IssuedAt =< Now + ?FRESH_FUTURE_MS andalso
        ExpiresAt > IssuedAt andalso
        ExpiresAt =< IssuedAt + ?MAX_TTL_MS andalso
        Now =< ExpiresAt.

%% actor 设备 active（status=1）且 device_generation 匹配（旧设备重放拒绝）
-spec actor_device_active(integer(), binary(), integer()) -> boolean().
actor_device_active(ActorUid, ActorDeviceId, ClaimedGen) ->
    case trust_audit_ds:actor_device_state(ActorUid, ActorDeviceId) of
        {ok, #{<<"status">> := Status, <<"device_generation">> := Gen}} ->
            Status =:= 1 andalso Gen =:= ClaimedGen;
        _ ->
            false
    end.

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

-spec to_int(term()) -> integer() | invalid.
to_int(I) when is_integer(I) -> I;
to_int(B) when is_binary(B) ->
    try
        binary_to_integer(B)
    catch
        _:_ -> invalid
    end;
to_int(_) ->
    invalid.

-spec i2b(integer() | binary()) -> binary().
i2b(I) when is_integer(I) -> integer_to_binary(I);
i2b(B) when is_binary(B) -> B.
