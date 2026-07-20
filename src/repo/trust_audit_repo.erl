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
-export([actor_device_state/2]).
-export([list_by_target/2]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"trust_audit">>).

%% @doc 追加一条信任决策事件（append-only，event_id 幂等 + 版本单调 + 冲突归属核对）。
%%  单事务原子完成三件事，闭合安全审查两条 Medium：
%%   1. per-target advisory 锁串行化并发写入，锁内读 MAX(target_identity_version) 再决定，
%%      闭合「读-改-写」TOCTOU：并发的旧版本重放不会与新版本同时通过 >= 校验。
%%   2. 版本回退（TargetVer < 历史 MAX）→ {error, identity_version_rollback}。
%%   3. event_id 冲突（DO NOTHING 命中）→ 回读既有行核对 (actor_uid, target_uid,
%%      target_device_id, to_state) 是否与本次一致：一致才是合法重放 {ok, duplicate}；
%%      不一致说明被他人抢占同一 event_id → {error, event_id_conflict}，不静默吞掉合法事件。
-spec insert_event(map()) ->
    {ok, inserted | duplicate} | {error, binary() | term()}.
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
    LockKey =
        <<(integer_to_binary(TargetUid))/binary, ":", TargetDeviceId/binary>>,
    InsertSql = <<
        "INSERT INTO ",
        Tb/binary,
        " (actor_uid, target_uid, target_device_id, target_ed25519,",
        "  from_state, to_state, method, actor_signature,",
        "  event_id, issued_at, expires_at, actor_device_generation, target_identity_version)",
        " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)",
        " ON CONFLICT (event_id) WHERE event_id IS NOT NULL DO NOTHING",
        " RETURNING id"
    >>,
    MaxSql = <<
        "SELECT COALESCE(MAX(target_identity_version), 0) AS v FROM ",
        Tb/binary,
        " WHERE target_uid = $1 AND target_device_id = $2",
        "   AND target_identity_version IS NOT NULL"
    >>,
    OwnerSql = <<
        "SELECT actor_uid, target_uid, target_device_id, to_state FROM ",
        Tb/binary,
        " WHERE event_id = $1 LIMIT 1"
    >>,
    Params = [
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
    ],
    elib_pg:with_tx(fun(Conn) ->
        %% 1. per-target 事务级 advisory 锁（同一 target device 的写入串行化）
        _ = elib_pg:query(
            Conn, <<"SELECT pg_advisory_xact_lock(hashtext($1))">>, [LockKey]
        ),
        %% 2. 锁内读历史最大版本，锁内决定是否回退
        case elib_pg:query(Conn, MaxSql, [TargetUid, TargetDeviceId]) of
            {ok, [#{<<"v">> := MaxVer} | _]} when TargetVer < MaxVer ->
                {error, <<"identity_version_rollback">>};
            {ok, _} ->
                insert_locked(
                    Conn,
                    InsertSql,
                    Params,
                    OwnerSql,
                    EventId,
                    ActorUid,
                    TargetUid,
                    TargetDeviceId,
                    ToState
                );
            {error, Reason} ->
                {error, Reason}
        end
    end).

%% 锁内幂等插入 + 冲突归属核对（见 insert_event 文档）。
insert_locked(
    Conn,
    InsertSql,
    Params,
    OwnerSql,
    EventId,
    ActorUid,
    TargetUid,
    TargetDeviceId,
    ToState
) ->
    case elib_pg:query(Conn, InsertSql, Params) of
        {ok, [_Row | _]} ->
            {ok, inserted};
        {ok, []} ->
            %% event_id 冲突：核对既有行归属，防他人抢占吞掉合法事件
            case elib_pg:query(Conn, OwnerSql, [EventId]) of
                {ok, [
                    #{
                        <<"actor_uid">> := ActorUid,
                        <<"target_uid">> := TargetUid,
                        <<"target_device_id">> := TargetDeviceId,
                        <<"to_state">> := ToState
                    }
                    | _
                ]} ->
                    {ok, duplicate};
                {ok, [_Other | _]} ->
                    {error, <<"event_id_conflict">>};
                {ok, []} ->
                    %% 冲突后行不见（并发删除不可能，append-only）→ 保守拒绝
                    {error, <<"event_id_conflict">>};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
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
