-module(olm_identity_logic).
%%%
%%% olm_identity_logic — Olm (X3DH + Double Ratchet) 设备密钥业务逻辑层。
%%%
%%% 职责：参数校验、claim 授权（仅好友/同群成员可领 OTK）、
%%%       claim 聚合（OTK 优先，耗尽回退 fallback）、跨设备批量查询。
%%% 零信任不变量：服务端只存/转公钥侧，无私钥；不做任何加解密。
%%%

-include("log.hrl").

-export([report_identity/6]).
-export([report_one_time_keys/4]).
-export([report_fallback_key/4]).
-export([report_fallback_key/5]).
-export([get_identity/2]).
-export([list_devices/1]).
-export([claim_keys/3]).
-export([claim_keys/4]).
-export([batch_claim_keys/3]).
-export([batch_claim_keys/4]).
-export([count_one_time_keys/2]).
-export([cleanup_consumed_one_time_keys/1]).

%% one-time keys 上报上限（防存储放大 DoS）
-define(MAX_OTK_PER_REPORT, 100).
%% batch claim 单请求设备数上限（防一次请求 claim 过多设备放大 DB 负载）
-define(MAX_BATCH_CLAIM_DEVICES, 20).
%% 天→秒换算：cleanup 保留期配置层用 days，Repo 层用 seconds，本层换算
-define(SECONDS_PER_DAY, 86400).
%% 换钥版本事件的 freshness 窗口（ms）：服务端即时事件，窗口仅防 event 重放
-define(ROTATION_EVENT_TTL_MS, 300000).

%% ===================================================================
%% 上报身份键
%% ===================================================================

%% @doc 上报设备 Olm 身份键（ed25519 + curve25519 + 签名）
%% 所有字段非空校验；签名由客户端用 Ed25519 私钥对 curve25519_key 生成，
%% 其他端可验签以防服务端篡改。
%%
%% C01（E2EE 计划 run-20261003-094804）换钥治理——E2EE-013 PoP 用「本次上传的
%% 新 ed25519 公钥」验「本次上传的签名」，只证明持有新私钥；盗 token 者自生成
%% 密钥对即可通过 PoP 并静默覆盖设备身份根键（ON CONFLICT DO UPDATE），对端
%% TOFU 无从感知。本层在写前按「现存身份」分流：
%%
%%   1. 无现存身份（not_found）→ 首次注册：PoP 即授权，直接写入。
%%   2. 现存身份同根（ed25519 相同）→ PoP（= 根钥签新子键）即授权；
%%      子键（curve25519）有变化时走 rotate_identity/6 产生版本事件；
%%      完全一致为幂等重报，不产生版本事件。
%%   3. 现存身份换根（ed25519 不同）→ 必须携带**旧钥过渡签名**：signature
%%      须由已注册旧 ed25519 私钥对本 curve25519_key 签署（持旧钥者授权换根），
%%      否则拒绝 key_rotation_requires_old_key_proof。盗 token 者无旧私钥，
%%      换根被拒；真重装（旧私钥丢失）用户同样被拒——重装恢复请先在设备管理
%%      移除旧设备记录（user_device_logic:delete 触发吊销级联清掉 olm 行），
%%      协议级 signed key transition 属后续升级（见 C01 报告）。
%%   4. find_identity 查询失败 → fail-closed：无法确认旧身份状态时拒绝写入
%%      （防「查不到旧身份即绕过分流」）。
%%
%% 版本事件（rotate_identity/6）：user_device.identity_version 单调 +1
%% （migration 00000047 写侧实现，撤销设备 0 行命中 → device_revoked，兼作
%% 撤销复活门），并写 trust_audit 事件 method=identity_rotated（版本/代数快照，
%% 复用 trust_audit_repo 的防回退 advisory 锁与 event_id 幂等）。
-spec report_identity(integer(), binary(), binary(), binary(), binary(), binary()) ->
    ok | {error, binary()}.
report_identity(UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType) when
    is_integer(UserId), is_binary(DeviceId)
->
    case
        byte_size(Ed25519Key) > 0 andalso
            byte_size(Curve25519Key) > 0 andalso
            byte_size(Signature) > 0
    of
        true ->
            case olm_identity_ds:find_identity(UserId, DeviceId) of
                {ok, not_found} ->
                    %% 首次注册：PoP（新钥自签）即授权
                    pop_then_upsert(
                        UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType
                    );
                {ok, #{<<"ed25519_key">> := OldEd, <<"curve25519_key">> := OldCurve}} ->
                    dispatch_existing_identity(
                        UserId,
                        DeviceId,
                        Ed25519Key,
                        Curve25519Key,
                        Signature,
                        DeviceType,
                        OldEd,
                        OldCurve
                    );
                {error, Reason} ->
                    _ = ?ERROR_LOG({olm_report_identity_lookup_error, UserId, DeviceId, Reason}),
                    {error, <<"internal_error">>}
            end;
        false ->
            {error, <<"invalid_identity_keys">>}
    end;
report_identity(_, _, _, _, _, _) ->
    {error, <<"bad_request">>}.

%% @private PoP（本次上传的 ed25519 验签）通过后直接 upsert——
%% 覆盖「首次注册」与「幂等重报」两条无需版本事件的路径。
-spec pop_then_upsert(integer(), binary(), binary(), binary(), binary(), binary()) ->
    ok | {error, binary()}.
pop_then_upsert(UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType) ->
    case verify_ed25519(Ed25519Key, Curve25519Key, Signature) of
        true ->
            do_upsert_identity(UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType);
        false ->
            _ = elib_metric:increment(olm_identity_pop_rejected_total),
            {error, <<"invalid_signature">>}
    end.

%% @private 按现存身份分流（见 report_identity/6 文档）。
-spec dispatch_existing_identity(
    integer(), binary(), binary(), binary(), binary(), binary(), binary(), binary()
) ->
    ok | {error, binary()}.
dispatch_existing_identity(
    UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType, OldEd, OldCurve
) ->
    case OldEd =:= Ed25519Key of
        true ->
            %% 同根：PoP（根钥对新子键签名）即授权
            case verify_ed25519(Ed25519Key, Curve25519Key, Signature) of
                true ->
                    case OldCurve =:= Curve25519Key of
                        true ->
                            %% 幂等重报：键完全一致，无版本事件
                            do_upsert_identity(
                                UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType
                            );
                        false ->
                            %% 子键轮换（根不变）
                            rotate_identity(
                                UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType
                            )
                    end;
                false ->
                    _ = elib_metric:increment(olm_identity_pop_rejected_total),
                    {error, <<"invalid_signature">>}
            end;
        false ->
            %% 换根：必须旧 ed25519 私钥对本 curve25519_key 的过渡签名
            case verify_ed25519(OldEd, Curve25519Key, Signature) of
                true ->
                    rotate_identity(
                        UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType
                    );
                false ->
                    _ = elib_metric:increment(olm_identity_rotation_rejected_total),
                    {error, <<"key_rotation_requires_old_key_proof">>}
            end
    end.

%% @private 有变化的身份写入：先过撤销联合门 + 版本 bump（原子单条 UPDATE，
%% WHERE status=1），成功后 upsert，最后追加 trust_audit 版本事件。
-spec rotate_identity(integer(), binary(), binary(), binary(), binary(), binary()) ->
    ok | {error, binary()}.
rotate_identity(UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType) ->
    case user_device_ds:bump_identity_version(UserId, DeviceId) of
        {ok, 0} ->
            %% user_device 无活跃行：撤销/硬删设备（含 cleanup 失败残留 olm 行的
            %% 复活路径）——身份写入必须拒绝
            {error, <<"device_revoked">>};
        {ok, NewVer, DeviceGen} ->
            case
                do_upsert_identity(
                    UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType
                )
            of
                ok ->
                    _ = emit_rotation_event(
                        UserId, DeviceId, Ed25519Key, Signature, NewVer, DeviceGen
                    ),
                    ok;
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_identity_bump_error, UserId, DeviceId, Reason}),
            {error, <<"internal_error">>}
    end.

-spec do_upsert_identity(integer(), binary(), binary(), binary(), binary(), binary()) ->
    ok | {error, binary()}.
do_upsert_identity(UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType) ->
    case
        olm_identity_ds:upsert_identity(
            UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType
        )
    of
        {ok, _} ->
            ok;
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_report_identity_error, UserId, DeviceId, Reason}),
            {error, <<"internal_error">>}
    end.

%% @private 追加换钥版本事件到 trust_audit（append-only）。
%%
%% 事件复用 trust_audit_repo:insert_event 的基础设施：per-target advisory 锁
%% 串行化、target_identity_version 快照防回退、event_id 幂等。actor 是设备
%% 本人（self-rotation），actor_signature 即本次上传的身份认证级签名——
%% 同根路径为新根对 curve 的签名（PoP），换根路径为旧根对 curve 的过渡签名，
%% 两者都密码学绑定到授权本次换钥的私钥。
%%
%% from_state/to_state 置 <<"unverified">>：服务端不追踪信任现值（trust_audit
%% 是事件流非现值表），换钥的语义就是「对端 TOFU 信任重置」；该事件不走
%% e2ee_trust_logic 的状态机校验（那是客户端信任决策路径），版本历史由
%% method=identity_rotated + target_identity_version 快照承载。
%% 事件写入失败不阻断换钥（换钥授权已验证、数据已落库），仅 ERROR 日志 +
%% 指标计数供对账告警。
-spec emit_rotation_event(integer(), binary(), binary(), binary(), pos_integer(), pos_integer()) ->
    ok.
emit_rotation_event(UserId, DeviceId, Ed25519B64, SignatureB64, NewVer, DeviceGen) ->
    NowMs = os:system_time(millisecond),
    Event = #{
        actor_uid => UserId,
        target_uid => UserId,
        target_device_id => DeviceId,
        target_ed25519 => Ed25519B64,
        from_state => <<"unverified">>,
        to_state => <<"unverified">>,
        method => <<"identity_rotated">>,
        actor_signature => SignatureB64,
        event_id => integer_to_binary(elib_tsid:generate(trust_audit)),
        issued_at => NowMs,
        expires_at => NowMs + ?ROTATION_EVENT_TTL_MS,
        actor_device_generation => DeviceGen,
        target_identity_version => NewVer
    },
    case trust_audit_ds:insert_event(Event) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_rotation_event_failed, UserId, DeviceId, NewVer, Reason}),
            _ = elib_metric:increment(olm_rotation_event_failed_total),
            ok
    end.

%% ===================================================================
%% 上报 one-time keys（批量）
%% ===================================================================

%% @doc 全量替换式上报 one-time keys（先删后插）
%% Keys: [{KeyId, KeyBase64}, ...]，上限 100 条。
-spec report_one_time_keys(integer(), binary(), [{binary(), binary()}], pos_integer()) ->
    {ok, non_neg_integer()} | {error, binary()}.
report_one_time_keys(UserId, DeviceId, Keys, MaxKeys) when
    is_integer(UserId), is_binary(DeviceId), is_list(Keys)
->
    Len = length(Keys),
    case Len =:= 0 orelse Len > ?MAX_OTK_PER_REPORT of
        true ->
            {error, <<"invalid_key_count">>};
        false ->
            Validated = [
                {K, V}
             || {K, V} <- Keys,
                is_binary(K) andalso byte_size(K) > 0 andalso is_binary(V) andalso byte_size(V) > 0
            ],
            case length(Validated) =:= Len of
                false ->
                    {error, <<"invalid_key_format">>};
                true ->
                    case
                        olm_identity_ds:upsert_one_time_keys(UserId, DeviceId, Validated, MaxKeys)
                    of
                        {ok, N} ->
                            {ok, N};
                        {error, Reason} ->
                            _ = ?ERROR_LOG({olm_report_otk_error, UserId, DeviceId, Reason}),
                            {error, <<"internal_error">>}
                    end
            end
    end;
report_one_time_keys(_, _, _, _) ->
    {error, <<"bad_request">>}.

%% ===================================================================
%% 上报 fallback key
%%
%%  /4 是无签名底层 upsert：仅由 /5 验签成功路径与既有测试复用，
%%  HTTP 层（olm_handler）只走 /5——无签名上传已在 /5 被拒。
%% ===================================================================

-spec report_fallback_key(integer(), binary(), binary(), binary()) ->
    ok | {error, binary()}.
report_fallback_key(UserId, DeviceId, KeyId, KeyB64) when
    is_integer(UserId), is_binary(DeviceId), is_binary(KeyId), is_binary(KeyB64)
->
    case byte_size(KeyId) > 0 andalso byte_size(KeyB64) > 0 of
        true ->
            case olm_identity_ds:upsert_fallback_key(UserId, DeviceId, KeyId, KeyB64) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    _ = ?ERROR_LOG({olm_report_fallback_error, UserId, DeviceId, Reason}),
                    {error, <<"internal_error">>}
            end;
        false ->
            {error, <<"invalid_fallback_key">>}
    end;
report_fallback_key(_, _, _, _) ->
    {error, <<"bad_request">>}.

%% @doc E2EE-062：带签名的 fallback prekey 上报（playbook E2EE-025：
%%  「OTK 耗尽只使用协议允许且**身份验证通过的** signed fallback prekey，或拒发」）。
%%
%%  威胁：E2EE-013 用 token 绑定设备所有权，但 **token 在网络上传输、identity 私钥
%%  不会**——盗 token 远比盗设备 ed25519 私钥容易。持被盗 token 的攻击者今天可以
%%  给该设备上传**自己控制的** fallback prekey；此后凡该设备 OTK 耗尽、对端回退
%%  fallback 的会话，用的都是攻击者的预密钥。要求由**已注册的 ed25519 身份键**
%%  签名，就把 fallback key 绑到了 token 窃取者拿不到的秘密上。
%%
%%  ⚠️ 空签名已改为**拒绝**（E2EE-062 第二阶段，2026-08-27 红队 RT-P1-01 落地）：
%%  第一阶段期间运行时攻击实证——盗 token 者可用空签名把设备 fallback prekey
%%  覆盖为自己控制的公钥；受害者 OTK 耗尽后对端新会话即落在攻击者预密钥上，
%%  且 identity 未变、TOFU 不告警。客户端（imboyapp olm_session_service）自
%%  E2EE-062 起已随上传携带 Ed25519 签名，必填不再阻断现役客户端。
%%  空签名仍计数（olm_fallback_unsigned_total），让旧客户端兼容缺口在运维侧可见。
-spec report_fallback_key(integer(), binary(), binary(), binary(), binary()) ->
    ok | {error, binary()}.
report_fallback_key(_UserId, _DeviceId, _KeyId, _KeyB64, <<>>) ->
    _ = elib_metric:increment(olm_fallback_unsigned_total),
    {error, <<"fallback_signature_required">>};
report_fallback_key(UserId, DeviceId, KeyId, KeyB64, Signature) when
    is_integer(UserId),
    is_binary(DeviceId),
    is_binary(KeyId),
    is_binary(KeyB64),
    is_binary(Signature)
->
    case no_ctrl_chars([DeviceId, KeyId, KeyB64]) of
        false ->
            %% canonical 用 `key=value\n`，值内含 \n/\r 会让编码非单射——
            %% 同一串字节可对应多组字段拆分，等价于签名伪造。fail-closed 拒收。
            {error, <<"invalid_fallback_key">>};
        true ->
            verify_then_report_fallback(UserId, DeviceId, KeyId, KeyB64, Signature)
    end;
report_fallback_key(_, _, _, _, _) ->
    {error, <<"bad_request">>}.

-spec verify_then_report_fallback(integer(), binary(), binary(), binary(), binary()) ->
    ok | {error, binary()}.
verify_then_report_fallback(UserId, DeviceId, KeyId, KeyB64, Signature) ->
    case olm_identity_ds:find_identity(UserId, DeviceId) of
        {ok, not_found} ->
            %% 无从验证即拒绝：若「验不了就放行」，攻击者只需先让 identity 查不到
            %% 即可绕开整道验签。
            {error, <<"device_not_registered">>};
        {ok, #{<<"ed25519_key">> := Ed25519B64}} ->
            Canonical = fallback_canonical(UserId, DeviceId, KeyId, KeyB64),
            case verify_ed25519(Ed25519B64, Canonical, Signature) of
                true ->
                    report_fallback_key(UserId, DeviceId, KeyId, KeyB64);
                false ->
                    {error, <<"invalid_signature">>}
            end;
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_fallback_identity_error, UserId, DeviceId, Reason}),
            {error, <<"internal_error">>}
    end.

%% @private fallback key 的 canonical 签名载荷。
%%  `key=value\n`、ASCII 字典序、末字段无尾随换行——与
%%  `e2ee_trust_logic:canonical_payload/1` 同一方案（项目既有、双语言对齐）。
%%  字段序 device_id < key_base64 < key_id < user_id 已是字典序。
-spec fallback_canonical(integer(), binary(), binary(), binary()) -> binary().
fallback_canonical(UserId, DeviceId, KeyId, KeyB64) ->
    <<"device_id=", DeviceId/binary, "\n", "key_base64=", KeyB64/binary, "\n", "key_id=",
        KeyId/binary, "\n", "user_id=", (integer_to_binary(UserId))/binary>>.

%% @private canonical 单射守卫：字段值不得含 \n/\r（唯一记录分隔符）。
-spec no_ctrl_chars([binary()]) -> boolean().
no_ctrl_chars(List) ->
    lists:all(
        fun(B) -> is_binary(B) andalso binary:match(B, [<<"\n">>, <<"\r">>]) =:= nomatch end,
        List
    ).

%% @private Ed25519 验签（公钥与签名均为 base64）。
%%  与 `e2ee_trust_logic:verify_signature/3` 是同一段原语的两份拷贝：
%%  那边是私有函数，而 `imboy_plugin_signature` 标注 FROZEN（v2 动态加载子系统冻结），
%%  两者都不宜从本路径依赖。若日后要合并，两处都在本注释可检索到。
-spec verify_ed25519(binary(), binary(), binary()) -> boolean().
verify_ed25519(Ed25519B64, Canonical, SignatureB64) ->
    try
        %% vodozemac 的 toBase64() 输出无尾随 '='；Erlang 的
        %% base64:decode/1 要求标准填充。只补齐缺失的填充，不改变
        %% 字节内容，也不放宽 Ed25519 验签本身。
        PubKey = decode_base64(Ed25519B64),
        Sig = decode_base64(SignatureB64),
        crypto:verify(eddsa, none, Canonical, Sig, [PubKey, ed25519])
    catch
        _:_ -> false
    end.

%% @private 接受标准 base64 与 vodozemac 的无填充 base64。
-spec decode_base64(binary()) -> binary().
decode_base64(B64) when is_binary(B64) ->
    Padding =
        case byte_size(B64) rem 4 of
            0 -> <<>>;
            2 -> <<"==">>;
            3 -> <<"=">>;
            _ -> erlang:error(invalid_base64_padding)
        end,
    base64:decode(<<B64/binary, Padding/binary>>).

%% ===================================================================
%% 查询身份键
%% ===================================================================

-spec get_identity(integer(), binary()) -> {ok, map()} | {error, binary()}.
get_identity(UserId, DeviceId) when is_integer(UserId), is_binary(DeviceId) ->
    case olm_identity_ds:find_identity(UserId, DeviceId) of
        {ok, not_found} ->
            {error, <<"not_found">>};
        {ok, Row} ->
            {ok, Row};
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_get_identity_error, UserId, DeviceId, Reason}),
            {error, <<"internal_error">>}
    end;
get_identity(_, _) ->
    {error, <<"bad_request">>}.

%% ===================================================================
%% 统一设备列表（ADR 03 §8.1）：对端全部活跃 olm 设备
%% ===================================================================

%% @doc 列出对端用户全部活跃 olm 设备（多设备发现，供 X3DH fan-out）。
%%  返回 {ok, #{<<"user_id">> => Uid, <<"devices">> => [DeviceMap]}}；
%%  DeviceMap 含 device_id/device_type/capabilities/trust_state/identity_blob/
%%  identity_signature/ed25519_key/curve25519_key/signature（ADR 03 §8.1 形状）。
-spec list_devices(integer()) -> {ok, map()} | {error, binary()}.
list_devices(TargetUid) when is_integer(TargetUid), TargetUid > 0 ->
    case olm_identity_ds:list_devices_with_identity(TargetUid) of
        {ok, Devices} when is_list(Devices) ->
            {ok, #{<<"user_id">> => TargetUid, <<"devices">> => Devices}};
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_list_devices_error, TargetUid, Reason}),
            {error, <<"internal_error">>}
    end;
list_devices(_) ->
    {error, <<"bad_request">>}.

%% ===================================================================
%% claim 聚合：OTK 优先，耗尽回退 fallback（X3DH 标准语义）
%% ===================================================================

%% @doc 领取目标用户某设备的一个 prekey。
%% 返回 {ok, #{type => one_time|fallback, key_id, key_base64, identity}}，
%% identity 含对端身份键供 X3DH 协商。
-spec claim_keys(integer(), integer(), binary()) ->
    {ok, map()} | {error, binary()}.
claim_keys(CurrentUid, TargetUid, DeviceId) when
    is_integer(CurrentUid), is_integer(TargetUid), is_binary(DeviceId)
->
    case ensure_claim_authorized(CurrentUid, TargetUid) of
        ok ->
            %% 先查身份键（claim 必须附带身份键供客户端 createOutboundSession）
            case olm_identity_ds:find_identity(TargetUid, DeviceId) of
                {ok, not_found} ->
                    {error, <<"device_not_registered">>};
                {ok, Identity} ->
                    claim_with_identity(CurrentUid, TargetUid, DeviceId, Identity);
                {error, Reason} ->
                    _ = ?ERROR_LOG({olm_claim_identity_error, TargetUid, DeviceId, Reason}),
                    {error, <<"internal_error">>}
            end;
        {error, _} = Err ->
            Err
    end;
claim_keys(_, _, _) ->
    {error, <<"bad_request">>}.

%% @doc E2EE-062：带幂等租约的 claim。RequestId 非空时，同一领取方重放同一
%%  RequestId 只消费一条 OTK 并恒返回同一条 key；为空时语义等同 claim_keys/3。
-spec claim_keys(integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, binary()}.
claim_keys(CurrentUid, TargetUid, DeviceId, RequestId) when
    is_integer(CurrentUid), is_integer(TargetUid), is_binary(DeviceId), is_binary(RequestId)
->
    case ensure_claim_authorized(CurrentUid, TargetUid) of
        ok ->
            case olm_identity_ds:find_identity(TargetUid, DeviceId) of
                {ok, not_found} ->
                    {error, <<"device_not_registered">>};
                {ok, Identity} ->
                    claim_with_identity(CurrentUid, TargetUid, DeviceId, Identity, RequestId);
                {error, Reason} ->
                    _ = ?ERROR_LOG({olm_claim_identity_error, TargetUid, DeviceId, Reason}),
                    {error, <<"internal_error">>}
            end;
        {error, _} = Err ->
            Err
    end;
claim_keys(_, _, _, _) ->
    {error, <<"bad_request">>}.

%% 撤销联合门说明（C01，run-20261003-094804）：目标设备的活跃性检查在
%% olm_identity_ds 的 claim 入口（ensure_device_active），不在本层——claim 的
%% ds 层是被 mock 的边界，门放 ds 层可被任何调用方（含 handler 直调场景）
%% 统一覆盖；本层只把 ds 返回的 {error, device_revoked} 原样冒泡，不落入
%% fallback 兜底（撤销设备的 fallback 同样在 ds 层被拦）。

-spec claim_with_identity(integer(), integer(), binary(), map()) ->
    {ok, map()} | {error, binary()}.
claim_with_identity(CurrentUid, TargetUid, DeviceId, Identity) ->
    %% 保留对 olm_identity_ds:claim_one_time_key/3 的原调用形状——
    %% 既有测试按 arity 挂 meck 期望，换 arity 会让它们静默穿透（A2-a 实证过）
    case olm_identity_ds:claim_one_time_key(TargetUid, DeviceId, CurrentUid) of
        {ok, OtkRow} ->
            {ok, #{
                <<"type">> => <<"one_time">>,
                <<"key_id">> => maps:get(<<"key_id">>, OtkRow),
                <<"key_base64">> => maps:get(<<"key_base64">>, OtkRow),
                <<"identity">> => Identity
            }};
        {error, device_revoked} ->
            %% 撤销设备：OTK 与 fallback 一并拒绝，转为 wire 契约的 binary 冒泡
            {error, <<"device_revoked">>};
        {error, exhausted} ->
            %% OTK 耗尽 → fallback 兜底。
            %% 这一刻就是前向保密降级的瞬间：该对端此后的新会话都复用同一条
            %% fallback prekey。运维侧必须能看见它，否则耗尽攻击是完全静默的。
            _ = elib_metric:increment(olm_otk_exhausted_total),
            case olm_identity_ds:claim_fallback_key(TargetUid, DeviceId) of
                {ok, FbRow} ->
                    {ok, #{
                        <<"type">> => <<"fallback">>,
                        <<"key_id">> => maps:get(<<"key_id">>, FbRow),
                        <<"key_base64">> => maps:get(<<"key_base64">>, FbRow),
                        <<"identity">> => Identity
                    }};
                {error, device_revoked} ->
                    {error, <<"device_revoked">>};
                {error, exhausted} ->
                    %% 连 fallback 都没有：比「池空」更严重，单独计数以便告警分级。
                    _ = elib_metric:increment(olm_prekey_unavailable_total),
                    {error, <<"no_prekey_available">>}
            end
    end.

-spec claim_with_identity(integer(), integer(), binary(), map(), binary()) ->
    {ok, map()} | {error, binary()}.
claim_with_identity(CurrentUid, TargetUid, DeviceId, Identity, <<>>) ->
    claim_with_identity(CurrentUid, TargetUid, DeviceId, Identity);
claim_with_identity(CurrentUid, TargetUid, DeviceId, Identity, RequestId) ->
    case olm_identity_ds:claim_one_time_key(TargetUid, DeviceId, CurrentUid, RequestId) of
        {ok, OtkRow} ->
            {ok, #{
                <<"type">> => <<"one_time">>,
                <<"key_id">> => maps:get(<<"key_id">>, OtkRow),
                <<"key_base64">> => maps:get(<<"key_base64">>, OtkRow),
                <<"identity">> => Identity
            }};
        {error, device_revoked} ->
            %% 撤销设备：OTK 与 fallback 一并拒绝，转为 wire 契约的 binary 冒泡
            {error, <<"device_revoked">>};
        {error, exhausted} ->
            %% OTK 耗尽 → fallback 兜底（非破坏性，重复领取同一条是协议允许的）。
            %% 幂等路径与 claim_with_identity/4 是两个函数子句，埋点必须各插一次；
            %% 合并成「旧的委托新的」会让按 arity 挂 meck 的既有测试静默穿透。
            _ = elib_metric:increment(olm_otk_exhausted_total),
            case olm_identity_ds:claim_fallback_key(TargetUid, DeviceId) of
                {ok, FbRow} ->
                    {ok, #{
                        <<"type">> => <<"fallback">>,
                        <<"key_id">> => maps:get(<<"key_id">>, FbRow),
                        <<"key_base64">> => maps:get(<<"key_base64">>, FbRow),
                        <<"identity">> => Identity
                    }};
                {error, device_revoked} ->
                    {error, <<"device_revoked">>};
                {error, exhausted} ->
                    %% 连 fallback 都没有：比「池空」更严重，单独计数以便告警分级。
                    _ = elib_metric:increment(olm_prekey_unavailable_total),
                    {error, <<"no_prekey_available">>}
            end
    end.

%% ===================================================================
%% batch claim（ADR 03 §8.2 多设备 fan-out）
%% ===================================================================

%% @doc 批量领取对端多设备 prekey。逐设备复用 claim_keys/3（每设备一条原子
%%  SKIP LOCKED + UPDATE 消费，保留 OTK 审计语义，见 repo claim_one_time_key/3）。
%%  返回 {ok, #{<<"claimed">> => #{DeviceId => KeyPayload},
%%             <<"failed">>  => #{DeviceId => Reason}}}；部分失败不中断其他设备。
%%  ponytail: 逐设备串行 claim，典型多设备 N<=5、单请求上限 20；若未来 N 很大，
%%  升级路径=单条 CTE 批量 claim（VALUES + LATERAL join），当前收益不抵复杂度。
-spec batch_claim_keys(integer(), integer(), [binary()]) ->
    {ok, map()} | {error, binary()}.
batch_claim_keys(CurrentUid, TargetUid, DeviceIds) when
    is_integer(CurrentUid), is_integer(TargetUid), is_list(DeviceIds)
->
    case normalize_device_ids(DeviceIds) of
        {error, Reason} ->
            {error, Reason};
        {ok, Uniq} ->
            %% 保留对 claim_keys/3 的原调用形状（既有测试按 arity 挂 meck 期望）
            {ok,
                fan_out(Uniq, fun(DeviceId) ->
                    claim_keys(CurrentUid, TargetUid, DeviceId)
                end)}
    end;
batch_claim_keys(_, _, _) ->
    {error, <<"bad_request">>}.

%% @doc E2EE-062 第三刀：带幂等租约的批量领取。
%%  多设备 fan-out 是幂等缺口的**放大器**——一次重试消费 N 条 OTK，抽干速度是
%%  单设备路径的 N 倍。此处把 RequestId 逐设备传给 `claim_keys/4`。
%%
%%  RequestId **不按设备派生**（不拼 device_id）：迁移 49 的部分唯一索引
%%  `uk_olm_otk_claim_request` 的键已经是
%%  `(claimed_by, user_id, device_id, claim_request_id)`，device_id 本就在键里，
%%  同一 RequestId 在不同设备上天然不互相命中。派生反而会把长度推过
%%  `claim_request_id varchar(64)` 而在 DB 层报错——即把可选的幂等优化变成
%%  一条新的失败路径。
-spec batch_claim_keys(integer(), integer(), [binary()], binary()) ->
    {ok, map()} | {error, binary()}.
batch_claim_keys(CurrentUid, TargetUid, DeviceIds, <<>>) ->
    batch_claim_keys(CurrentUid, TargetUid, DeviceIds);
batch_claim_keys(CurrentUid, TargetUid, DeviceIds, RequestId) when
    is_integer(CurrentUid), is_integer(TargetUid), is_list(DeviceIds), is_binary(RequestId)
->
    case normalize_device_ids(DeviceIds) of
        {error, Reason} ->
            {error, Reason};
        {ok, Uniq} ->
            {ok,
                fan_out(Uniq, fun(DeviceId) ->
                    claim_keys(CurrentUid, TargetUid, DeviceId, RequestId)
                end)}
    end;
batch_claim_keys(_, _, _, _) ->
    {error, <<"bad_request">>}.

%% @private 逐设备串行 claim，部分失败不中断其他设备。
-spec fan_out([binary()], fun((binary()) -> {ok, map()} | {error, binary()})) -> map().
fan_out(DeviceIds, ClaimFun) ->
    {Claimed, Failed} = lists:foldl(
        fun(DeviceId, {AccOk, AccErr}) ->
            case ClaimFun(DeviceId) of
                {ok, Payload} ->
                    {maps:put(DeviceId, Payload, AccOk), AccErr};
                {error, Reason} ->
                    {AccOk, maps:put(DeviceId, Reason, AccErr)}
            end
        end,
        {#{}, #{}},
        DeviceIds
    ),
    #{<<"claimed">> => Claimed, <<"failed">> => Failed}.

%% @private 去重 + 上限校验（batch_claim_keys/3 与 /4 共用同一判据）
-spec normalize_device_ids([binary()]) -> {ok, [binary()]} | {error, binary()}.
normalize_device_ids(DeviceIds) ->
    case lists:usort([D || D <- DeviceIds, is_binary(D), byte_size(D) > 0]) of
        [] -> {error, <<"no_device_ids">>};
        Uniq when length(Uniq) > ?MAX_BATCH_CLAIM_DEVICES -> {error, <<"too_many_devices">>};
        Uniq -> {ok, Uniq}
    end.

%% ===================================================================
%% OTK 余量查询（E2EE-062：低水位补传的信号来源）
%% ===================================================================

%% @doc 查询某用户某设备**尚未被领取**的 one-time key 数量。
%%  调用方（handler）必须保证 UserId/DeviceId 取自 token 而非请求入参——
%%  否则该能力就是「探测谁的池快空了」的接口，正好给耗尽攻击择时。
%%  查询失败返回 {error, _}，**不得**降级为 0：0 是「该补传了」的有效信号，
%%  用它掩盖故障会让真正的池见底与数据库故障无法区分。
-spec count_one_time_keys(integer(), binary()) ->
    {ok, non_neg_integer()} | {error, binary()}.
count_one_time_keys(UserId, DeviceId) when
    is_integer(UserId), UserId > 0, is_binary(DeviceId), DeviceId =/= <<>>
->
    case olm_identity_ds:count_one_time_keys(UserId, DeviceId) of
        {ok, N} when is_integer(N) ->
            {ok, N};
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_count_otk_error, UserId, DeviceId, Reason}),
            {error, <<"internal_error">>}
    end;
count_one_time_keys(_, _) ->
    {error, <<"bad_request">>}.

%% ===================================================================
%% cleanup 已消费 OTK 审计行（olm_otk_cleanup_worker 调用）
%% ===================================================================

%% @doc 清理已消费（claimed）且超保留期的 one-time key 审计行。
%%  入参 RetentionDays（运维配置单位 days）；本层换算为 seconds 传下层。
%%  安全门：RetentionDays 必须为正整数，否则拒绝下探——防 retention<=0 时
%%  `consumed_at < now()-0` 删光全部 claimed 审计行。
-spec cleanup_consumed_one_time_keys(pos_integer()) ->
    {ok, non_neg_integer()} | {error, binary()}.
cleanup_consumed_one_time_keys(RetentionDays) when
    is_integer(RetentionDays), RetentionDays > 0
->
    RetentionSeconds = RetentionDays * ?SECONDS_PER_DAY,
    case olm_identity_ds:cleanup_consumed_one_time_keys(RetentionSeconds) of
        {ok, N} ->
            {ok, N};
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_cleanup_otk_error, RetentionDays, Reason}),
            {error, <<"internal_error">>}
    end;
cleanup_consumed_one_time_keys(_) ->
    {error, <<"invalid_retention">>}.

%% ===================================================================
%% OTK claim 授权检查（E2EE-013：仅好友/同群可领 OTK，防耗尽攻击）
%% ===================================================================

%% @doc 检查当前用户是否有权领取目标用户的 OTK。
%% 允许的情况：
%%  - 自身（多设备同步，self-claim）
%%  - 好友关系
%%  - 同群成员（共享群组）
%% 否则拒接，防任意用户抽干他人 OTK 池。
-spec ensure_claim_authorized(integer(), integer()) -> ok | {error, binary()}.
ensure_claim_authorized(CurrentUid, TargetUid) when
    is_integer(CurrentUid), is_integer(TargetUid)
->
    case CurrentUid =:= TargetUid of
        true ->
            ok;
        false ->
            case friend_ds:is_friend(CurrentUid, TargetUid) of
                true ->
                    ok;
                false ->
                    case share_common_group(CurrentUid, TargetUid) of
                        true ->
                            ok;
                        false ->
                            _ = elib_metric:increment(olm_claim_unauthorized_total),
                            {error, <<"claim_not_authorized">>}
                    end
            end
    end;
ensure_claim_authorized(_, _) ->
    {error, <<"bad_request">>}.

%% @private 检查两个用户是否同属至少一个群组。
-spec share_common_group(integer(), integer()) -> boolean().
share_common_group(Uid1, Uid2) ->
    try
        {ok, _, _, Rows} = elib_pg:query(
            "SELECT 1 FROM group_member a "
            "JOIN group_member b ON a.group_id = b.group_id "
            "WHERE a.uid = $1 AND b.uid = $2 AND a.status = 'active' AND b.status = 'active' "
            "LIMIT 1",
            [Uid1, Uid2]
        ),
        Rows =/= []
    catch
        _:_ -> false
    end.
