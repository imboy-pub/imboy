-module(trust_audit_ds).
%%%
%% trust_audit_ds — 设备信任事件数据服务层（thin pass-through to repo，G3 治理）。
%% 与 olm_identity_ds 同模式：handler/logic 不直调 repo。
%%%

-export([insert_event/1]).
-export([actor_device_state/2]).
-export([list_by_target/2]).

-spec insert_event(map()) -> {ok, inserted | duplicate} | {error, term()}.
insert_event(Fields) when is_map(Fields) ->
    trust_audit_repo:insert_event(Fields).

-spec actor_device_state(integer(), binary()) -> {ok, map() | not_found} | {error, term()}.
actor_device_state(ActorUid, ActorDeviceId) ->
    trust_audit_repo:actor_device_state(ActorUid, ActorDeviceId).

-spec list_by_target(integer(), binary()) -> {ok, [map()]} | {error, term()}.
list_by_target(TargetUid, TargetDeviceId) ->
    trust_audit_repo:list_by_target(TargetUid, TargetDeviceId).
