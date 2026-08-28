-module(group_task_ds).

%%%
% group_task_ds — 群作业数据服务层（2026-04 G2-a + G3 治理）
%
% 职责：封装 group_task_repo / group_task_assignment_repo 两类仓储访问，
% 供 Logic 层调用，避免 Logic 直接依赖 Repo（4 层架构）。
% Responsibility: wrap group_task_repo and group_task_assignment_repo for
% Logic layer consumption, enforcing Handler→Logic→DS→Repo boundary.
%
% T7 归档写守卫（P0 后续批）：全部内容写路径（建/改作业、分配、提交、
% 批改、软删/恢复）经 workspace_guard:write_tx 同事务守卫
% （{group,Gid} / {group_task,PK|TaskId} / {group_task_assignment,Id}
% → workspace 行锁），归档后拒绝（980）；查询路径不加守卫。
%%%%

%% group_task_repo 只读
-export([
    find_by_id/1,
    find_by_task_id/1,
    find_any_by_id/1,
    find_any_by_task_id/1
]).

%% group_task_repo 写入 / 列表（G3 新增 pass-through）
-export([
    insert_task/1,
    update_task/2,
    soft_delete/1,
    restore/1,
    list_by_group_id/3,
    list_by_group_id/4,
    count_by_group_id/1,
    count_by_group_id/2,
    list_deleted_by_group_id/3,
    list_deleted_by_group_id/4,
    count_deleted_by_group_id/1,
    count_deleted_by_group_id/2,
    list_by_group_and_user/4,
    list_by_group_and_user/5
]).

%% group_task_assignment_repo（G3 新增 pass-through）
-export([
    assignment_insert/1,
    assignment_update/2,
    assignment_find_by_id/1,
    assignment_find_by_task_and_user/2,
    assignment_list_by_task_id/3,
    assignment_list_by_user_id/3,
    assignment_list_by_user_id/4,
    assignment_count_by_status/2
]).

%% @doc 按内部 PK 查未删除作业
find_by_id(Id) -> group_task_repo:find_by_id(Id).

%% @doc 按对外 task_id 查未删除作业
find_by_task_id(TaskId) -> group_task_repo:find_by_task_id(TaskId).

%% @doc 按内部 PK 查作业（含软删除）
find_any_by_id(Id) -> group_task_repo:find_any_by_id(Id).

%% @doc 按对外 task_id 查作业（含软删除）
find_any_by_task_id(TaskId) -> group_task_repo:find_any_by_task_id(TaskId).

%% @doc 新建作业
insert_task(Data) ->
    %% T7 归档写守卫：{group, Gid} 行锁与写入同事务（Data 自带 group_id）；
    %% group_id 缺失/非法时走 repo 自动提交版保留必填字段校验错误契约。
    case maps:get(group_id, Data, undefined) of
        Gid when is_integer(Gid), Gid > 0 ->
            workspace_guard:write_tx({group, Gid}, fun(Conn) ->
                group_task_repo:insert_tx(Conn, Data)
            end);
        _ ->
            group_task_repo:insert(Data)
    end.

%% @doc 更新作业
update_task(TaskId, Data) ->
    %% T7 归档写守卫：{group_task, PK}（PK → group → workspace）；
    %% 用户更新与 adm 治理（task_close 等）共用本入口。
    workspace_guard:write_tx({group_task, TaskId}, fun(Conn) ->
        group_task_repo:update_tx(Conn, TaskId, Data)
    end).

%% @doc 按群分页列表（不含状态）
list_by_group_id(GroupId, Page, Size) ->
    group_task_repo:list_by_group_id(GroupId, Page, Size).

%% @doc 按群 + 状态分页列表
list_by_group_id(GroupId, Status, Page, Size) ->
    group_task_repo:list_by_group_id(GroupId, Status, Page, Size).

%% @doc 按群 + 指派人分页列表
list_by_group_and_user(GroupId, AssigneeId, Page, Size) ->
    group_task_repo:list_by_group_and_user(GroupId, AssigneeId, Page, Size).

%% @doc 按群 + 指派人 + 状态分页列表
list_by_group_and_user(GroupId, AssigneeId, Status, Page, Size) ->
    group_task_repo:list_by_group_and_user(GroupId, AssigneeId, Status, Page, Size).

%% ===================================================================
%% group_task_assignment_repo pass-through
%% ===================================================================

%% @doc 新建作业分配
assignment_insert(Data) ->
    %% T7 归档写守卫：{group_task, TaskId}（Data 自带对外 task_id）；
    %% task_id 缺失/非法时走 repo 自动提交版保留校验错误契约。
    case maps:get(task_id, Data, undefined) of
        TaskId when is_binary(TaskId), TaskId =/= <<>> ->
            workspace_guard:write_tx({group_task, TaskId}, fun(Conn) ->
                group_task_assignment_repo:insert_tx(Conn, Data)
            end);
        _ ->
            group_task_assignment_repo:insert(Data)
    end.

%% @doc 更新作业分配
assignment_update(AssignmentId, Data) ->
    %% T7 归档写守卫：{group_task_assignment, Id}（分配 → 作业 → 群）
    workspace_guard:write_tx({group_task_assignment, AssignmentId}, fun(Conn) ->
        group_task_assignment_repo:update_tx(Conn, AssignmentId, Data)
    end).

%% @doc 按内部 PK 查分配
assignment_find_by_id(Id) -> group_task_assignment_repo:find_by_id(Id).

%% @doc 按 (task_id, user_id) 查分配
assignment_find_by_task_and_user(TaskId, UserId) ->
    group_task_assignment_repo:find_by_task_and_user(TaskId, UserId).

%% @doc 按 task_id 列分配
assignment_list_by_task_id(TaskId, Page, Size) ->
    group_task_assignment_repo:list_by_task_id(TaskId, Page, Size).

%% @doc 按 user_id 列分配（无状态过滤）
assignment_list_by_user_id(UserId, Page, Size) ->
    group_task_assignment_repo:list_by_user_id(UserId, Page, Size).

%% @doc 按 user_id + 状态列分配
assignment_list_by_user_id(UserId, Status, Page, Size) ->
    group_task_assignment_repo:list_by_user_id(UserId, Status, Page, Size).

%% G3 thin wrappers for adm_group_handler
soft_delete(TaskPk) ->
    %% T7 归档写守卫：{group_task, PK}（adm 治理软删）
    workspace_guard:write_tx({group_task, TaskPk}, fun(Conn) ->
        group_task_repo:soft_delete_tx(Conn, TaskPk)
    end).
restore(TaskPk) ->
    %% T7 归档写守卫：{group_task, PK}（adm 治理恢复）
    workspace_guard:write_tx({group_task, TaskPk}, fun(Conn) ->
        group_task_repo:restore_tx(Conn, TaskPk)
    end).
count_by_group_id(GroupId) -> group_task_repo:count_by_group_id(GroupId).
count_by_group_id(GroupId, Status) -> group_task_repo:count_by_group_id(GroupId, Status).
list_deleted_by_group_id(GroupId, Page, Size) ->
    group_task_repo:list_deleted_by_group_id(GroupId, Page, Size).
list_deleted_by_group_id(GroupId, Status, Page, Size) ->
    group_task_repo:list_deleted_by_group_id(GroupId, Status, Page, Size).
count_deleted_by_group_id(GroupId) -> group_task_repo:count_deleted_by_group_id(GroupId).
count_deleted_by_group_id(GroupId, Status) ->
    group_task_repo:count_deleted_by_group_id(GroupId, Status).
assignment_count_by_status(UserId, Status) ->
    group_task_assignment_repo:count_by_status(UserId, Status).
