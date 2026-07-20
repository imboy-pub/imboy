-module(trust_audit_repo).
%%%
%% trust_audit_repo — 设备信任决策事件流仓库层（ADR 06 §8.2.2 + ADR 16 §3.3.1）。
%%
%% append-only：仅 INSERT 与 SELECT，**不提供 update/delete**（不可变审计不变量，
%% 守护测试 T-06-11）。actor_signature 是身份认证级签名（≠ E2EE payload 解密）。
%% E2EE-014：insert 走 event_id 幂等（ON CONFLICT DO NOTHING）；freshness/版本快照入列。
%%%

-include("log.hrl").

-export([tablename/0]).
-export([insert_event/1]).
-export([max_target_identity_version/2]).
-export([actor_device_state/2]).
-export([list_by_target/2]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"trust_audit">>).

%% @doc 追加一条信任决策事件（append-only，event_id 幂等）。
%%  同 event_id 重放命中 partial UNIQUE → DO NOTHING → 返回 {ok, duplicate}（不写第二行）。
%%  首次写入 → RETURNING id → {ok, inserted}。
-spec insert_event(map()) -> {ok, inserted | duplicate} | {error, term()}.
insert_event(#{
    actor_uid := ActorUid,
    target_uid := TargetUid,
    target_device_id := TargetDeviceId,
    target_ed25519 := TargetEd25519,
    from_state := FromState,
    to_state := ToState,
    method := Method,
    actor_signature := ActorSignature,
    event_id := EventId,
    issued_at := IssuedAt,
    expires_at := ExpiresAt,
    actor_device_generation := ActorGen,
    target_identity_version := TargetVer
}) when is_integer(ActorUid), is_integer(TargetUid) ->
    Tb = tablename(),
    Sql = <<
        "INSERT INTO ",
        Tb/binary,
        " (actor_uid, target_uid, target_device_id, target_ed25519,",
        "  from_state, to_state, method, actor_signature,",
        "  event_id, issued_at, expires_at, actor_device_generation, target_identity_version)",
        " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)",
        " ON CONFLICT (event_id) WHERE event_id IS NOT NULL DO NOTHING",
        " RETURNING id"
    >>,
    case
        elib_pg:query(Sql, [
            ActorUid,
            TargetUid,
            TargetDeviceId,
            TargetEd25519,
            FromState,
            ToState,
            Method,
            ActorSignature,
            EventId,
            IssuedAt,
            ExpiresAt,
            ActorGen,
            TargetVer
        ])
    of
        {ok, [_Row | _]} -> {ok, inserted};
        {ok, []} -> {ok, duplicate};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 该 target device 历史已记录的最大 target_identity_version（无记录返回 0）。
%%  用于 E2EE-014 单调校验（防身份键回退）。
-spec max_target_identity_version(integer(), binary()) -> {ok, integer()} | {error, term()}.
max_target_identity_version(TargetUid, TargetDeviceId) when is_integer(TargetUid) ->
    Tb = tablename(),
    Sql = <<
        "SELECT COALESCE(MAX(target_identity_version), 0) AS v FROM ",
        Tb/binary,
        " WHERE target_uid = $1 AND target_device_id = $2",
        "   AND target_identity_version IS NOT NULL"
    >>,
    case elib_pg:query(Sql, [TargetUid, TargetDeviceId]) of
        {ok, [#{<<"v">> := V} | _]} -> {ok, V};
        {ok, []} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

%% @doc actor 设备的 status 与 device_generation（跨表读 user_device，供 trust 校验）。
%%  E2EE-014：撤销 actor 与旧设备重放拒绝依据。无记录返回 {ok, not_found}。
-spec actor_device_state(integer(), binary()) -> {ok, map() | not_found} | {error, term()}.
actor_device_state(ActorUid, ActorDeviceId) when is_integer(ActorUid) ->
    Ud = elib_pg_sql:public_tablename(<<"user_device">>),
    Sql = <<
        "SELECT status, device_generation FROM ",
        Ud/binary,
        " WHERE user_id = $1 AND device_id = $2 LIMIT 1"
    >>,
    case elib_pg:query(Sql, [ActorUid, ActorDeviceId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 查询某目标设备的信任事件历史（按时间升序，供审计回放）。
-spec list_by_target(integer(), binary()) -> {ok, [map()]} | {error, term()}.
list_by_target(TargetUid, TargetDeviceId) when is_integer(TargetUid) ->
    Tb = tablename(),
    Sql = <<
        "SELECT actor_uid, target_uid, target_device_id, from_state, to_state,",
        "       method, event_id, issued_at, created_at",
        " FROM ",
        Tb/binary,
        " WHERE target_uid = $1 AND target_device_id = $2",
        " ORDER BY id ASC"
    >>,
    elib_pg:query(Sql, [TargetUid, TargetDeviceId]).
