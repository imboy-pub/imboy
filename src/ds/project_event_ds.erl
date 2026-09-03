-module(project_event_ds).
-compile([nowarn_deprecated_catch]).
%%%
% project_event_ds 是 project_event domain service 缩写
% 项目事件领域服务（双体验 v2.5.2 WP4/T6a）
%
% T6a 独占 project_event writer 事务接口；T6b 的 task 状态流转经
% record_task_status_tx/6 写事件（与状态 UPDATE 同一 Conn = 同一事务，
% 事件原子性：无孤儿事件——事务回滚时事件与状态变更一起消失）。
% 本模块仅负责 task_status 事件写入；Activity 读取由聚合仓库负责。
%%%

-export([record_task_status_tx/6]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 事务内记录 task 状态流转事件（event_type='task_status'，
%% target_id=TaskId，payload 含 from/to status + actor）
-spec record_task_status_tx(any(), integer(), integer(), integer(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
record_task_status_tx(Conn, ProjectId, TaskId, ActorId, FromStatus, ToStatus) ->
    Payload = #{
        <<"from">> => FromStatus,
        <<"to">> => ToStatus,
        <<"actor">> => ActorId
    },
    project_event_repo:insert_tx(Conn, #{
        <<"project_id">> => ProjectId,
        <<"actor_id">> => ActorId,
        <<"event_type">> => <<"task_status">>,
        <<"target_id">> => TaskId,
        <<"payload">> => jsone:encode(Payload, [native_utf8]),
        <<"created_at">> => elib_dt:now()
    }).
