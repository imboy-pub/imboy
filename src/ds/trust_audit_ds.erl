-module(trust_audit_ds).
%%%
%% trust_audit_ds — 设备信任事件数据服务层（thin pass-through to repo，G3 治理）。
%% 与 olm_identity_ds 同模式：handler/logic 不直调 repo。
%%%

-export([insert_event/8]).
-export([list_by_target/2]).

-spec insert_event(
    integer(), integer(), binary(), binary(), binary(), binary(), binary(), binary()
) -> {ok, term()} | {error, term()}.
insert_event(
    ActorUid, TargetUid, TargetDeviceId, TargetEd25519, FromState, ToState, Method, ActorSignature
) ->
    trust_audit_repo:insert_event(
        ActorUid,
        TargetUid,
        TargetDeviceId,
        TargetEd25519,
        FromState,
        ToState,
        Method,
        ActorSignature
    ).

-spec list_by_target(integer(), binary()) -> {ok, [map()]} | {error, term()}.
list_by_target(TargetUid, TargetDeviceId) ->
    trust_audit_repo:list_by_target(TargetUid, TargetDeviceId).
