-module(group_schedule_ds).

%%%
% group_schedule_ds — 群日程数据服务层（2026-04 G2-a + G3 治理）
%
% 封装 group_schedule_repo（及其参与者 / 提醒子表）对 Logic 层的访问。
% Pass-through wrapper over group_schedule_repo and its participant/remind
% sub-tables, enforcing Handler→Logic→DS→Repo boundary.
%
% T7 归档写守卫（P0 后续批）：内容写路径（建/改/取消日程、参与者增改删、
% 建提醒）经 workspace_guard:write_tx 同事务守卫（{group,Gid} /
% {group_schedule,PK|ScheduleId} → workspace 行锁），归档后拒绝（980）；
% update_remind_sent（提醒生命周期簿记）用 write_tx_or_skip freeze 语义；
% 查询路径不加守卫。
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

insert_schedule(Data) ->
    %% T7 归档写守卫：{group, Gid} 行锁与写入同事务（Data 自带 group_id）；
    %% group_id 缺失/非法时走 repo 自动提交版保留校验错误契约。
    case maps:get(group_id, Data, undefined) of
        Gid when is_integer(Gid), Gid > 0 ->
            workspace_guard:write_tx({group, Gid}, fun(Conn) ->
                group_schedule_repo:insert_tx(Conn, Data)
            end);
        _ ->
            group_schedule_repo:insert(Data)
    end.

update_schedule(Id, Data) ->
    %% T7 归档写守卫：{group_schedule, PK}（PK → group → workspace）
    workspace_guard:write_tx({group_schedule, Id}, fun(Conn) ->
        group_schedule_repo:update_tx(Conn, Id, Data)
    end).

update_status(Id, Status) ->
    %% T7 归档写守卫：{group_schedule, PK}；用户 cancel 与 adm 治理共用本入口。
    workspace_guard:write_tx({group_schedule, Id}, fun(Conn) ->
        group_schedule_repo:update_status_tx(Conn, Id, Status)
    end).

list_by_group_id(GroupId, StartAt, EndAt, Page, Size) ->
    group_schedule_repo:list_by_group_id(GroupId, StartAt, EndAt, Page, Size).

count_by_group_id(GroupId, StartAt, EndAt) ->
    group_schedule_repo:count_by_group_id(GroupId, StartAt, EndAt).

list_by_user_id(UserId, StartAt, EndAt, Page, Size) ->
    group_schedule_repo:list_by_user_id(UserId, StartAt, EndAt, Page, Size).

%% ===================================================================
%% participants
%% ===================================================================

insert_participant(Data) ->
    %% T7 归档写守卫：{group_schedule, ScheduleId}（Data 自带对外 schedule_id）；
    %% schedule_id 缺失时走 repo 自动提交版保留插入行为契约。
    case maps:get(schedule_id, Data, undefined) of
        ScheduleId when is_binary(ScheduleId), ScheduleId =/= <<>> ->
            workspace_guard:write_tx({group_schedule, ScheduleId}, fun(Conn) ->
                group_schedule_repo:insert_participant_tx(Conn, Data)
            end);
        _ ->
            group_schedule_repo:insert_participant(Data)
    end.

update_participant_status(ScheduleId, UserId, Status) ->
    %% T7 归档写守卫：{group_schedule, ScheduleId}（参加/不参加为内容写，980）
    workspace_guard:write_tx({group_schedule, ScheduleId}, fun(Conn) ->
        group_schedule_repo:update_participant_status_tx(Conn, ScheduleId, UserId, Status)
    end).

list_participants(ScheduleId) -> group_schedule_repo:list_participants(ScheduleId).

count_participants(ScheduleId) -> group_schedule_repo:count_participants(ScheduleId).

delete_participant(ScheduleId, UserId) ->
    %% T7 归档写守卫：{group_schedule, ScheduleId}
    workspace_guard:write_tx({group_schedule, ScheduleId}, fun(Conn) ->
        group_schedule_repo:delete_participant_tx(Conn, ScheduleId, UserId)
    end).

%% ===================================================================
%% reminds
%% ===================================================================

insert_remind(Data) ->
    %% T7 归档写守卫：{group_schedule, ScheduleId}（提醒记录属日程内容派生，
    %% 归档后不再新建提醒）
    case maps:get(schedule_id, Data, undefined) of
        ScheduleId when is_binary(ScheduleId), ScheduleId =/= <<>> ->
            workspace_guard:write_tx({group_schedule, ScheduleId}, fun(Conn) ->
                group_schedule_repo:insert_remind_tx(Conn, Data)
            end);
        _ ->
            group_schedule_repo:insert_remind(Data)
    end.

list_pending_reminds() -> group_schedule_repo:list_pending_reminds().

update_remind_sent(RemindId) ->
    %% T7 派生读写（freeze 语义）：提醒已发送标记为生命周期簿记，
    %% 归档时跳过标记（提醒保持待发，恢复工作区后续跑）；读取/扫描不受影响。
    case
        workspace_guard:write_tx_or_skip({group_schedule_remind, RemindId}, fun(Conn) ->
            group_schedule_repo:update_remind_sent_tx(Conn, RemindId)
        end)
    of
        {written, Ret} -> Ret;
        skipped -> {ok, 0}
    end.
