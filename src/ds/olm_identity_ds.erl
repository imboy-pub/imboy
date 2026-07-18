-module(olm_identity_ds).
%%%
%% olm_identity_ds — Olm 设备密钥数据服务层（thin pass-through to repo）。
%% 与 compliance_key_ds 同模式：仅做 G3 治理（handler 不直调 repo）。
%%%

-export([upsert_identity/6]).
-export([find_identity/2]).
-export([list_identity_by_uids/1]).
-export([upsert_one_time_keys/4]).
-export([count_one_time_keys/2]).
-export([claim_one_time_key/3]).
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

-spec upsert_one_time_keys(integer(), binary(), [{binary(), binary()}], pos_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
upsert_one_time_keys(UserId, DeviceId, Keys, MaxKeys) ->
    olm_identity_repo:upsert_one_time_keys(UserId, DeviceId, Keys, MaxKeys).

-spec count_one_time_keys(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_one_time_keys(UserId, DeviceId) ->
    olm_identity_repo:count_one_time_keys(UserId, DeviceId).

-spec claim_one_time_key(integer(), binary(), integer()) -> {ok, map()} | {error, exhausted}.
claim_one_time_key(UserId, DeviceId, ClaimedBy) ->
    olm_identity_repo:claim_one_time_key(UserId, DeviceId, ClaimedBy).

-spec upsert_fallback_key(integer(), binary(), binary(), binary()) ->
    {ok, term()} | {error, term()}.
upsert_fallback_key(UserId, DeviceId, KeyId, KeyB64) ->
    olm_identity_repo:upsert_fallback_key(UserId, DeviceId, KeyId, KeyB64).

-spec claim_fallback_key(integer(), binary()) -> {ok, map()} | {error, exhausted}.
claim_fallback_key(UserId, DeviceId) ->
    olm_identity_repo:claim_fallback_key(UserId, DeviceId).

%% @doc 清理已消费 OTK 审计行（入参 seconds，薄透传至 repo）
-spec cleanup_consumed_one_time_keys(pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
cleanup_consumed_one_time_keys(RetentionSeconds) ->
    olm_identity_repo:cleanup_consumed_one_time_keys(RetentionSeconds).
