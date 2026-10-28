-module(group_schedule_ds).

-moduledoc "群日程数据服务层（2026-04 G2-a + G3 治理）。".
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
    update_remind_sent/1,
    cancel_pending_reminds/1,
    claim_remind/1
]).

-export([dispatch_remind/3, update_with_reminders/4, cancel_with_reminders/2]).

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

update_status(Id, 4) ->
    workspace_guard:write_tx({group_schedule, Id}, fun(Conn) ->
        case tx_value(group_schedule_repo:lock_schedule_by_id_tx(Conn, Id)) of
            [#{<<"schedule_id">> := ScheduleId}] -> cancel_schedule_tx(Conn, Id, ScheduleId);
            [] -> {ok, 0}
        end
    end);
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

%% R18：作废待发提醒（取消日程/改期重排时调用）
cancel_pending_reminds(ScheduleId) ->
    group_schedule_repo:cancel_pending_reminds(ScheduleId).

%% R18：派发前原子认领（CAS），1=认领成功需发送；0=跳过
claim_remind(RemindId) ->
    group_schedule_repo:claim_remind(RemindId).

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

%% The schedule row serializes dispatch against cancellation and rescheduling.
dispatch_remind(RemindId, ScheduleId, UserId) ->
    workspace_guard:write_tx_or_skip({group_schedule_remind, RemindId}, fun(Conn) ->
        case locked_schedule(Conn, ScheduleId) of
            undefined ->
                skipped;
            #{<<"status">> := 4} ->
                skipped;
            #{<<"title">> := Title, <<"start_at">> := StartAt} ->
                case
                    tx_value(
                        group_schedule_repo:claim_remind_tx(Conn, RemindId, ScheduleId, UserId)
                    )
                of
                    0 ->
                        skipped;
                    1 ->
                        Payload = #{schedule_id => ScheduleId, title => Title, start_at => StartAt},
                        ok = tx_ok(msg_s2c_ds:stage_schedule_remind_tx(Conn, UserId, Payload)),
                        sent
                end
        end
    end).

update_with_reminders(ScheduleId, Id, Data, StartAt) ->
    workspace_guard:write_tx({group_schedule, Id}, fun(Conn) ->
        case locked_schedule(Conn, ScheduleId) of
            #{<<"id">> := Id, <<"status">> := Status} = Schedule when Status =/= 4 ->
                1 = tx_value(group_schedule_repo:update_tx(Conn, Id, Data)),
                _ = tx_value(group_schedule_repo:cancel_pending_reminds_tx(Conn, ScheduleId)),
                Rows = tx_value(group_schedule_repo:list_participants_tx(Conn, ScheduleId)),
                schedule_reminders_tx(
                    Conn,
                    ScheduleId,
                    StartAt,
                    maps:get(<<"remind_before">>, Schedule, undefined),
                    Rows
                ),
                {ok, 1};
            #{<<"status">> := 4} ->
                {error, already_cancelled};
            _ ->
                {error, not_found}
        end
    end).

cancel_with_reminders(ScheduleId, Id) ->
    workspace_guard:write_tx({group_schedule, Id}, fun(Conn) ->
        case locked_schedule(Conn, ScheduleId) of
            #{<<"id">> := Id, <<"status">> := Status} when Status =/= 4 ->
                cancel_schedule_tx(Conn, Id, ScheduleId);
            #{<<"status">> := 4} ->
                {error, already_cancelled};
            _ ->
                {error, not_found}
        end
    end).

cancel_schedule_tx(Conn, Id, ScheduleId) ->
    case tx_value(group_schedule_repo:update_status_tx(Conn, Id, 4)) of
        1 ->
            _ = tx_value(group_schedule_repo:cancel_pending_reminds_tx(Conn, ScheduleId)),
            {ok, 1};
        0 ->
            {ok, 0}
    end.

locked_schedule(Conn, ScheduleId) ->
    case tx_value(group_schedule_repo:lock_schedule_tx(Conn, ScheduleId)) of
        [] -> undefined;
        [Schedule] -> Schedule
    end.

schedule_reminders_tx(Conn, ScheduleId, StartAt, Before0, Rows) ->
    Before =
        case Before0 of
            N when is_integer(N) -> N;
            B when is_binary(B) ->
                try
                    binary_to_integer(B)
                catch
                    _:_ -> 0
                end;
            _ ->
                0
        end,
    Now = elib_dt:rfc3339_to(elib_dt:now()),
    Start = elib_dt:rfc3339_to(StartAt),
    case
        Before > 0 andalso is_integer(Now) andalso is_integer(Start) andalso
            Start - Before * 60000 > Now
    of
        true ->
            RemindAt = elib_dt:to_rfc3339(Start - Before * 60000),
            lists:foreach(
                fun(#{<<"user_id">> := UserId}) ->
                    _ = tx_value(
                        group_schedule_repo:insert_remind_tx(
                            Conn,
                            #{
                                schedule_id => ScheduleId,
                                user_id => UserId,
                                remind_at => RemindAt,
                                is_sent => false
                            }
                        )
                    ),
                    ok
                end,
                Rows
            );
        false ->
            ok
    end.

tx_value({ok, Value}) -> Value;
tx_value({ok, Value, _}) -> Value;
tx_value({error, Reason}) -> elib_pg:abort({abort_tx, Reason}).

tx_ok(ok) -> ok;
tx_ok({error, Reason}) -> elib_pg:abort({abort_tx, Reason}).
