-module(olm_identity_logic).
%%%
%%% olm_identity_logic — Olm (X3DH + Double Ratchet) 设备密钥业务逻辑层。
%%%
%%% 职责：参数校验、claim 聚合（OTK 优先，耗尽回退 fallback）、跨设备批量查询。
%%% 零信任不变量：服务端只存/转公钥侧，无私钥；不做任何加解密。
%%%

-include("log.hrl").

-export([report_identity/6]).
-export([report_one_time_keys/4]).
-export([report_fallback_key/4]).
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

%% ===================================================================
%% 上报身份键
%% ===================================================================

%% @doc 上报设备 Olm 身份键（ed25519 + curve25519 + 签名）
%% 所有字段非空校验；签名由客户端用 Ed25519 私钥对 curve25519_key 生成，
%% 其他端可验签以防服务端篡改。
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
            end;
        false ->
            {error, <<"invalid_identity_keys">>}
    end;
report_identity(_, _, _, _, _, _) ->
    {error, <<"bad_request">>}.

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
claim_keys(_, _, _) ->
    {error, <<"bad_request">>}.

%% @doc E2EE-062：带幂等租约的 claim。RequestId 非空时，同一领取方重放同一
%%  RequestId 只消费一条 OTK 并恒返回同一条 key；为空时语义等同 claim_keys/3。
-spec claim_keys(integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, binary()}.
claim_keys(CurrentUid, TargetUid, DeviceId, RequestId) when
    is_integer(CurrentUid), is_integer(TargetUid), is_binary(DeviceId), is_binary(RequestId)
->
    case olm_identity_ds:find_identity(TargetUid, DeviceId) of
        {ok, not_found} ->
            {error, <<"device_not_registered">>};
        {ok, Identity} ->
            claim_with_identity(CurrentUid, TargetUid, DeviceId, Identity, RequestId);
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_claim_identity_error, TargetUid, DeviceId, Reason}),
            {error, <<"internal_error">>}
    end;
claim_keys(_, _, _, _) ->
    {error, <<"bad_request">>}.

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
        {error, exhausted} ->
            %% OTK 耗尽 → fallback 兜底
            case olm_identity_ds:claim_fallback_key(TargetUid, DeviceId) of
                {ok, FbRow} ->
                    {ok, #{
                        <<"type">> => <<"fallback">>,
                        <<"key_id">> => maps:get(<<"key_id">>, FbRow),
                        <<"key_base64">> => maps:get(<<"key_base64">>, FbRow),
                        <<"identity">> => Identity
                    }};
                {error, exhausted} ->
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
        {error, exhausted} ->
            %% OTK 耗尽 → fallback 兜底（非破坏性，重复领取同一条是协议允许的）
            case olm_identity_ds:claim_fallback_key(TargetUid, DeviceId) of
                {ok, FbRow} ->
                    {ok, #{
                        <<"type">> => <<"fallback">>,
                        <<"key_id">> => maps:get(<<"key_id">>, FbRow),
                        <<"key_base64">> => maps:get(<<"key_base64">>, FbRow),
                        <<"identity">> => Identity
                    }};
                {error, exhausted} ->
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
