-module(trust_audit_repo).
%%%
%% trust_audit_repo — 设备信任决策事件流仓库层（ADR 06 §8.2.2）。
%%
%% append-only：仅 INSERT 与 SELECT，**不提供 update/delete**（不可变审计不变量，
%% 守护测试 T-06-11）。记录「谁信任谁、何时、何方法」，供 T2/T8 事后追查。
%% 注：actor_signature 是身份认证级签名（≠ E2EE payload 解密），不违反 ADR 02 §6。
%%%

-include("log.hrl").

-export([tablename/0]).
-export([insert_event/8]).
-export([list_by_target/2]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"trust_audit">>).

%% @doc 追加一条信任决策事件（append-only）。id 由 bigserial 自增，created_at 默认 now()。
-spec insert_event(
    integer(), integer(), binary(), binary(), binary(), binary(), binary(), binary()
) -> {ok, term()} | {error, term()}.
insert_event(
    ActorUid, TargetUid, TargetDeviceId, TargetEd25519, FromState, ToState, Method, ActorSignature
) when
    is_integer(ActorUid), is_integer(TargetUid)
->
    Tb = tablename(),
    Sql = <<
        "INSERT INTO ",
        Tb/binary,
        " (actor_uid, target_uid, target_device_id, target_ed25519,",
        "  from_state, to_state, method, actor_signature)",
        " VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
    >>,
    elib_pg:query(Sql, [
        ActorUid,
        TargetUid,
        TargetDeviceId,
        TargetEd25519,
        FromState,
        ToState,
        Method,
        ActorSignature
    ]).

%% @doc 查询某目标设备的信任事件历史（按时间升序，供审计回放）。
-spec list_by_target(integer(), binary()) -> {ok, [map()]} | {error, term()}.
list_by_target(TargetUid, TargetDeviceId) when is_integer(TargetUid) ->
    Tb = tablename(),
    Sql = <<
        "SELECT actor_uid, target_uid, target_device_id, from_state, to_state,",
        "       method, created_at",
        " FROM ",
        Tb/binary,
        " WHERE target_uid = $1 AND target_device_id = $2",
        " ORDER BY id ASC"
    >>,
    elib_pg:query(Sql, [TargetUid, TargetDeviceId]).
