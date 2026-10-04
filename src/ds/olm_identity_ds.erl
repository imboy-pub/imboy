-module(olm_identity_ds).
-moduledoc "Olm 设备密钥数据服务层（thin pass-through to repo）。".
%%%
%% olm_identity_ds — Olm 设备密钥数据服务层（thin pass-through to repo）。
%% 与 compliance_key_ds 同模式：仅做 G3 治理（handler 不直调 repo）。
%%%

-export([upsert_identity/6]).
-export([find_identity/2]).
-export([list_identity_by_uids/1]).
-export([list_devices_with_identity/1]).
-export([upsert_one_time_keys/4]).
-export([count_one_time_keys/2]).
-export([claim_one_time_key/3]).
-export([claim_one_time_key/4]).
-export([upsert_fallback_key/4]).
-export([claim_fallback_key/2]).
-export([cleanup_consumed_one_time_keys/1]).

-spec upsert_identity(integer(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, term()} | {error, term()}.
upsert_identity(UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType) ->
    olm_identity_repo:upsert_identity(
        UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType
    ).

-spec find_identity(integer(), binary()) -> {ok, map() | not_found} | {error, term()}.
find_identity(UserId, DeviceId) ->
    olm_identity_repo:find_identity(UserId, DeviceId).

-spec list_identity_by_uids([integer()]) -> {ok, [map()]} | {error, term()}.
list_identity_by_uids(Uids) ->
    olm_identity_repo:list_identity_by_uids(Uids).

-spec list_devices_with_identity(integer()) -> {ok, [map()]} | {error, term()}.
list_devices_with_identity(UserId) ->
    olm_identity_repo:list_devices_with_identity(UserId).

-spec upsert_one_time_keys(integer(), binary(), [{binary(), binary()}], pos_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
upsert_one_time_keys(UserId, DeviceId, Keys, MaxKeys) ->
    olm_identity_repo:upsert_one_time_keys(UserId, DeviceId, Keys, MaxKeys).

-spec count_one_time_keys(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_one_time_keys(UserId, DeviceId) ->
    olm_identity_repo:count_one_time_keys(UserId, DeviceId).

-spec claim_one_time_key(integer(), binary(), integer()) ->
    {ok, map()} | {error, exhausted | device_revoked}.
claim_one_time_key(UserId, DeviceId, ClaimedBy) ->
    case ensure_device_active(UserId, DeviceId) of
        ok ->
            olm_identity_repo:claim_one_time_key(UserId, DeviceId, ClaimedBy);
        {error, device_revoked} = Err ->
            Err
    end.

%% @doc E2EE-062：带幂等租约的 claim。RequestId 为 <<>> 时语义等同 /3。
-spec claim_one_time_key(integer(), binary(), integer(), binary()) ->
    {ok, map()} | {error, exhausted | device_revoked}.
claim_one_time_key(UserId, DeviceId, ClaimedBy, RequestId) ->
    case ensure_device_active(UserId, DeviceId) of
        ok ->
            olm_identity_repo:claim_one_time_key(UserId, DeviceId, ClaimedBy, RequestId);
        {error, device_revoked} = Err ->
            Err
    end.

-spec upsert_fallback_key(integer(), binary(), binary(), binary()) ->
    {ok, term()} | {error, term()}.
upsert_fallback_key(UserId, DeviceId, KeyId, KeyB64) ->
    olm_identity_repo:upsert_fallback_key(UserId, DeviceId, KeyId, KeyB64).

-spec claim_fallback_key(integer(), binary()) -> {ok, map()} | {error, exhausted | device_revoked}.
claim_fallback_key(UserId, DeviceId) ->
    case ensure_device_active(UserId, DeviceId) of
        ok ->
            olm_identity_repo:claim_fallback_key(UserId, DeviceId);
        {error, device_revoked} = Err ->
            Err
    end.

%% @doc 撤销联合门（C01，E2EE 计划 run-20261003-094804）：OTK/fallback 的领取
%% 前确认目标设备在 user_device 白名单中仍活跃。
%%
%% 为什么在 ds 层而不是 logic 层：claim 的 ds 入口是全系统（handler/logic/
%% 未来调用方）抵达 olm 表的唯一通道，门放这里覆盖一切路径；且 is_active
%% 本身 fail-closed（DB 故障即 false），对 claim 这类安全操作宁可拒绝。
%% 覆盖 cleanup_olm_material 失败残留路径：撤销后 olm 三表残留行
%% （OTK/fallback/identity）领不走；self-claim 同样拦截。
%% 语义上 is_active 的正结果缓存 60s 有轻微时延窗口，撤销广播（跨节点
%% flush）+ 60s TTL 是既有吊销传播语义，本门沿用同一口径。
-spec ensure_device_active(integer(), binary()) -> ok | {error, device_revoked}.
ensure_device_active(UserId, DeviceId) ->
    case user_device_ds:is_active(UserId, DeviceId) of
        true ->
            ok;
        false ->
            {error, device_revoked}
    end.

%% @doc 清理已消费 OTK 审计行（入参 seconds，薄透传至 repo）
-spec cleanup_consumed_one_time_keys(pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
cleanup_consumed_one_time_keys(RetentionSeconds) ->
    olm_identity_repo:cleanup_consumed_one_time_keys(RetentionSeconds).
