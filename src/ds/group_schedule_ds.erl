-module(group_schedule_ds).

%%%
% group_schedule_ds — 群日程数据服务层（2026-04 G2-a + G3 治理）
%
% 封装 group_schedule_repo（及其参与者 / 提醒子表）对 Logic 层的访问。
% Pass-through wrapper over group_schedule_repo and its participant/remind
% sub-tables, enforcing Handler→Logic→DS→Repo boundary.
%%%

-export([
    find_by_id/1,
    find_by_id/2,
    find_by_schedule_id/1
]).

%% G3：日程主表 pass-through
-export([
    insert_schedule/1,
    update_schedule/2,
    update_status/2,
    list_by_group_id/5,
    count_by_group_id/3,
    list_by_user_id/5
]).

%% G3：参与者表 pass-through
-export([
    insert_participant/1,
    update_participant_status/3,
    list_participants/1,
    count_participants/1,
    delete_participant/2
]).

%% G3：提醒表 pass-through
-export([
    insert_remind/1,
    list_pending_reminds/0,
    update_remind_sent/1
]).

%% @doc 按内部 PK 查日程
find_by_id(Id) -> group_schedule_repo:find_by_id(Id).

%% @doc 按内部 PK + 指定列查日程
find_by_id(Id, Column) -> group_schedule_repo:find_by_id(Id, Column).

%% @doc 按对外 schedule_id 查日程
find_by_schedule_id(ScheduleId) -> group_schedule_repo:find_by_schedule_id(ScheduleId).

%% ===================================================================
%% schedule main table
%% ===================================================================

insert_schedule(Data) -> group_schedule_repo:insert(Data).

update_schedule(Id, Data) -> group_schedule_repo:update(Id, Data).

update_status(Id, Status) -> group_schedule_repo:update_status(Id, Status).

list_by_group_id(GroupId, StartAt, EndAt, Page, Size) ->
    group_schedule_repo:list_by_group_id(GroupId, StartAt, EndAt, Page, Size).

count_by_group_id(GroupId, StartAt, EndAt) ->
    group_schedule_repo:count_by_group_id(GroupId, StartAt, EndAt).

list_by_user_id(UserId, StartAt, EndAt, Page, Size) ->
    group_schedule_repo:list_by_user_id(UserId, StartAt, EndAt, Page, Size).

%% ===================================================================
%% participants
%% ===================================================================

insert_participant(Data) -> group_schedule_repo:insert_participant(Data).

update_participant_status(ScheduleId, UserId, Status) ->
    group_schedule_repo:update_participant_status(ScheduleId, UserId, Status).

list_participants(ScheduleId) -> group_schedule_repo:list_participants(ScheduleId).

count_participants(ScheduleId) -> group_schedule_repo:count_participants(ScheduleId).

delete_participant(ScheduleId, UserId) ->
    group_schedule_repo:delete_participant(ScheduleId, UserId).

%% ===================================================================
%% reminds
%% ===================================================================

insert_remind(Data) -> group_schedule_repo:insert_remind(Data).

list_pending_reminds() -> group_schedule_repo:list_pending_reminds().

update_remind_sent(RemindId) -> group_schedule_repo:update_remind_sent(RemindId).
