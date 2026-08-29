-module(project_member_ds).
-compile([nowarn_deprecated_catch]).
%%%
% project_member_ds 是 project_member domain service 缩写
% 项目成员领域服务（Channel-first-class W2 ZC-02）
%
% 核心职责（全部单事务，with_tx；业务写与 project_event 写同一 Conn = 同一
% 事务，无孤儿事件——回滚时事件与成员变更一起消失）：
%   1. invite：project 归属解析 → 归档写守卫（先锁 workspace 行）→
%      actor 仍为 project owner 事务内复检（403，M-3 对齐 transfer 标准）→
%      目标 active workspace_member 同事务复检（400；DB 写入端触发器
%      trg_project_member_ws_active 为第二道兜底）→ upsert 幂等
%      （新增/恢复 changed 写 member_invited 事件；已 active unchanged
%      幂等无事件）。
%   2. remove：归属解析 + 守卫 → actor 治理权事务内复检（403，M-3：
%      project owner 或 active workspace owner）→ 软删 active→removed
%      （真实移除写 member_removed 事件；无 active 行幂等 already_removed
%      无事件）。不动 workspace_member——Workspace 治理由 workspace_logic
%      负责；移除端 DB 触发器 trg_workspace_member_remove_guard_pm 约束次序。
%   3. transfer_owner：归属解析 + 守卫 → actor 仍为 project owner 事务内
%      复检（403）→ 目标须 active 非 Guest workspace_member（409）→
%      新 Owner 未完成 assignee task 冲突校验（409 直接拒绝，执行台账
%      假设 A2）→ 新/旧 Owner 双 upsert 成员行 → project.owner_id 更新 →
%      member_owner_transferred 事件。
%   4. ensure_owner_member_tx：建 Project 事务内 Owner 自动入项目接线点
%      （ZC-05 由 project_ds:create 调用；无事件——project_created 事件已
%      覆盖创建语义，执行台账假设 A3）。
%
% 事件类型契约（chk_project_event_type 14 值）：本 DS 只写
%   member_invited / member_removed / member_owner_transferred，禁止自造值。
% 错误约定：{error, {Code, Msg}}（Code 为语义码）直接透传；
%   内部失败返回 {error, {tag, Reason}} 由 logic 层归一 500（同 project_logic）。
%%%

-export([invite/3]).
-export([remove/3]).
-export([transfer_owner/3]).
-export([list/3]).
-export([find/2]).
-export([ensure_owner_member_tx/4]).

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 邀请项目成员（幂等；单事务：守卫 + actor owner 事务内复检 +
%% active ws member 复检 + upsert + 事件）
%% 权限（仅 Project Owner）logic 层前置 + 本事务内复检（M-3 防检查-写窗口）。
-spec invite(integer(), integer(), integer()) ->
    {ok, map(), created | existing} | {error, term()}.
invite(ActorUid, ProjectId, TargetUid) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            Project = project_repo:find_tx(Conn, ProjectId, <<"id,workspace_id,owner_id">>),
            WsId = ensure_writable_project_tx(Conn, Project),
            %% 事务内 actor 复检（M-3，与 transfer_owner 同标准防检查-写窗口）：
            %% 仅 Project Owner
            ok = ensure_actor_owner_tx(maps:get(<<"owner_id">>, Project, 0), ActorUid),
            %% 应用层同事务前置：目标必须是 active workspace_member
            ensure_active_ws_member_tx(Conn, WsId, TargetUid),
            case project_member_repo:upsert_active_tx(Conn, WsId, ProjectId, TargetUid, ActorUid) of
                {ok, changed, Row} ->
                    {ok, _} = record_member_event_tx(
                        Conn, ProjectId, <<"member_invited">>, TargetUid, ActorUid, #{
                            <<"invited_by">> => ActorUid
                        }
                    ),
                    {ok, Row, created};
                {ok, unchanged, Row} ->
                    %% 幂等：已 active，无变更不写事件
                    {ok, Row, existing};
                {error, Reason} ->
                    throw({abort_tx, {member_invite_failed, Reason}})
            end
        end),
    case Result of
        {ok, Row, Status} ->
            {ok, Row, Status};
        {error, Reason} ->
            _ = ?ERROR_LOG([project_member_invite_failed, ProjectId, ActorUid, TargetUid, Reason]),
            {error, Reason}
    end.

%% @doc 移除项目成员（软删；幂等；单事务：守卫 + actor 治理权复检 + 移除 + 事件）
%% 权限（Project Owner 或 Workspace Owner 治理权）logic 层前置 + 本事务内
%% 复检（M-3：owner 判定读 project.owner_id，治理权判定读 ws role/status）。
-spec remove(integer(), integer(), integer()) ->
    {ok, map(), removed | already_removed} | {error, term()}.
remove(ActorUid, ProjectId, TargetUid) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            Project = project_repo:find_tx(Conn, ProjectId, <<"id,workspace_id,owner_id">>),
            WsId = ensure_writable_project_tx(Conn, Project),
            %% 事务内 actor 复检（M-3，与 transfer_owner 同标准防检查-写窗口）：
            %% Project Owner 或 active Workspace Owner（治理权）
            ok = ensure_governor_actor_tx(
                Conn, WsId, maps:get(<<"owner_id">>, Project, 0), ActorUid
            ),
            case project_member_repo:remove_tx(Conn, ProjectId, TargetUid) of
                {ok, 1} ->
                    {ok, _} = record_member_event_tx(
                        Conn, ProjectId, <<"member_removed">>, TargetUid, ActorUid, #{}
                    ),
                    {ok, removed};
                {ok, 0} ->
                    %% 幂等：无 active 行（重复移除/非成员），成功且无事件
                    {ok, already_removed};
                {error, Reason} ->
                    throw({abort_tx, {member_remove_failed, Reason}})
            end
        end),
    case Result of
        {ok, Status} ->
            {ok, #{project_id => ProjectId, user_id => TargetUid, status => <<"removed">>}, Status};
        {error, Reason} ->
            _ = ?ERROR_LOG([project_member_remove_failed, ProjectId, ActorUid, TargetUid, Reason]),
            {error, Reason}
    end.

%% @doc 项目 Owner 转移（单事务）
%% logic 层前置权限；事务内复检：actor 仍为 project owner（403）、目标为
%% active 非 Guest workspace_member（409）、目标无未完成 assignee task（409）。
-spec transfer_owner(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
transfer_owner(ActorUid, ProjectId, TargetUid) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            Project = project_repo:find_tx(Conn, ProjectId, <<"id,workspace_id,owner_id">>),
            WsId = ensure_writable_project_tx(Conn, Project),
            OwnerId = maps:get(<<"owner_id">>, Project, 0),
            ok = ensure_actor_owner_tx(OwnerId, ActorUid),
            ok = ensure_transfer_target_member_tx(Conn, WsId, OwnerId, TargetUid),
            %% 未完成 Task 冲突校验：新 Owner 在本项目存在未完成 assignee task
            %% → 直接拒绝（执行台账假设 A2；先改派或完成再转移）
            case project_member_repo:unfinished_assignee_tasks_tx(Conn, ProjectId, TargetUid) of
                {ok, []} ->
                    ok;
                {ok, _Tasks} ->
                    throw(
                        {abort_tx, {409, <<"转移目标在本项目有未完成任务，请先改派或完成后再转移"/utf8>>}}
                    );
                {error, Reason2} ->
                    throw({abort_tx, {member_transfer_failed, Reason2}})
            end,
            %% 新 Owner 自动入项目（invited_by=旧 Owner；已 active 则 unchanged）；
            %% 旧 Owner 保持成员身份（invited_by=NULL）
            {ok, _, _} =
                project_member_repo:upsert_active_tx(Conn, WsId, ProjectId, TargetUid, ActorUid),
            {ok, _, _} =
                project_member_repo:upsert_active_tx(Conn, WsId, ProjectId, OwnerId, null),
            Now = elib_dt:now(),
            {ok, _} = project_repo:update_fields_tx(
                Conn, ProjectId, #{<<"owner_id">> => TargetUid, <<"updated_at">> => Now}
            ),
            {ok, _} = record_member_event_tx(
                Conn, ProjectId, <<"member_owner_transferred">>, TargetUid, ActorUid, #{
                    <<"from">> => OwnerId,
                    <<"to">> => TargetUid
                }
            ),
            #{
                project_id => ProjectId,
                workspace_id => WsId,
                previous_owner_id => OwnerId,
                owner_id => TargetUid
            }
        end),
    case Result of
        Map when is_map(Map) ->
            _ = ?INFO_LOG([project_owner_transferred, ProjectId, ActorUid, TargetUid]),
            {ok, Map};
        {error, Reason} ->
            _ = ?ERROR_LOG([project_member_transfer_failed, ProjectId, ActorUid, TargetUid, Reason]),
            {error, Reason}
    end.

%% @doc 项目成员分页列表（只列 active；权限由 logic 层前置）
-spec list(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list(ProjectId, Page, Size) ->
    project_member_repo:page_by_project(
        ProjectId,
        Page,
        Size,
        <<"pm.workspace_id,pm.project_id,pm.user_id,pm.invited_by,pm.joined_at,",
            "pm.status,u.nickname,u.avatar,u.account">>
    ).

%% @doc 查询项目成员行（自动提交；空 map = 无记录；logic 层权限判定用）
-spec find(integer(), integer()) -> map().
find(ProjectId, Uid) ->
    project_member_repo:find(
        ProjectId, Uid, <<"workspace_id,project_id,user_id,invited_by,joined_at,status">>
    ).

%% @doc 事务内 Owner 自动入项目（建 Project 时同事务写 owner 的 active
%% project_member 行；幂等可重复；无事件——执行台账假设 A3）
%% ZC-05 接线点：project_ds:create 的 with_tx 内调用（见 patch 清单）。
-spec ensure_owner_member_tx(any(), integer(), integer(), integer()) -> ok | no_return().
ensure_owner_member_tx(Conn, WsId, ProjectId, OwnerUid) ->
    case project_member_repo:upsert_active_tx(Conn, WsId, ProjectId, OwnerUid, null) of
        {ok, _Changed, _Row} ->
            ok;
        {error, Reason} ->
            throw({abort_tx, {owner_member_backfill_failed, Reason}})
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 事务内：project 归属解析（writable 守卫；invite/remove 先 find_tx 取
%% owner_id 做 actor 复检，再走本守卫；M-3 起不再单独提供 by-id 入口）
-spec ensure_writable_project_tx(any(), map()) -> integer() | no_return().
ensure_writable_project_tx(Conn, Project) ->
    case maps:get(<<"workspace_id">>, Project, undefined) of
        WsId when is_integer(WsId) ->
            ok = workspace_guard:abort_on_error(
                workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
            ),
            WsId;
        _ ->
            throw({abort_tx, {404, <<"项目不存在"/utf8>>}})
    end.

%% 事务内：actor 仍为 project owner（403；权限在事务内复检防检查-写窗口；
%% M-3 起 invite / remove / transfer_owner 共用此复检标准）
-spec ensure_actor_owner_tx(integer(), integer()) -> ok | no_return().
ensure_actor_owner_tx(OwnerId, ActorUid) ->
    case OwnerId =:= ActorUid of
        true -> ok;
        false -> throw({abort_tx, {403, <<"仅项目 Owner 可执行该操作"/utf8>>}})
    end.

%% 事务内：actor 仍具治理权——project owner 或 active workspace owner
%% （403；M-3：remove 与 transfer_owner 同标准的事务内复检）
-spec ensure_governor_actor_tx(any(), integer(), integer(), integer()) -> ok | no_return().
ensure_governor_actor_tx(Conn, WsId, OwnerId, ActorUid) ->
    case OwnerId =:= ActorUid of
        true ->
            ok;
        false ->
            case workspace_member_repo:find_tx(Conn, WsId, ActorUid, <<"role,status">>) of
                #{<<"status">> := <<"active">>, <<"role">> := <<"owner">>} ->
                    ok;
                _ ->
                    throw(
                        {abort_tx, {403, <<"仅项目 Owner 或工作区 Owner 可执行该操作"/utf8>>}}
                    )
            end
    end.

%% 事务内：转移目标合法（400 自转；409 非 active/非 Guest workspace member）
-spec ensure_transfer_target_member_tx(any(), integer(), integer(), integer()) ->
    ok | no_return().
ensure_transfer_target_member_tx(Conn, WsId, OwnerId, TargetUid) ->
    case TargetUid =:= OwnerId of
        true ->
            throw({abort_tx, {400, <<"新 Owner 不能是当前 Owner"/utf8>>}});
        false ->
            case workspace_member_repo:find_tx(Conn, WsId, TargetUid, <<"role,status">>) of
                #{<<"status">> := <<"active">>, <<"role">> := <<"guest">>} ->
                    throw({abort_tx, {409, <<"Owner 转移目标不能是 Guest"/utf8>>}});
                #{<<"status">> := <<"active">>} ->
                    ok;
                _ ->
                    throw(
                        {abort_tx, {409, <<"转移目标不是该工作区的 active 成员"/utf8>>}}
                    )
            end
    end.

%% 事务内：目标必须是 active workspace_member（400；fail-fast 于 DB 触发器兜底）
-spec ensure_active_ws_member_tx(any(), integer(), integer()) -> ok | no_return().
ensure_active_ws_member_tx(Conn, WsId, TargetUid) ->
    case workspace_member_repo:find_tx(Conn, WsId, TargetUid, <<"status">>) of
        #{<<"status">> := <<"active">>} ->
            ok;
        _ ->
            throw(
                {abort_tx, {400, <<"受邀用户必须是该工作区的 active 成员"/utf8>>}}
            )
    end.

%% 事务内写成员事件（与业务写同一 Conn = 同一事务；payload 附 actor）
-spec record_member_event_tx(any(), integer(), binary(), integer(), integer(), map()) ->
    {ok, integer()} | no_return().
record_member_event_tx(Conn, ProjectId, EventType, TargetUid, ActorUid, Extra) ->
    Payload = maps:merge(Extra, #{<<"actor">> => ActorUid}),
    case
        project_event_repo:insert_tx(Conn, #{
            <<"project_id">> => ProjectId,
            <<"actor_id">> => ActorUid,
            <<"event_type">> => EventType,
            <<"target_id">> => TargetUid,
            <<"payload">> => jsone:encode(Payload, [native_utf8]),
            <<"created_at">> => elib_dt:now()
        })
    of
        {ok, Id} ->
            {ok, Id};
        {error, Reason} ->
            throw({abort_tx, {member_event_failed, {EventType, Reason}}})
    end.
