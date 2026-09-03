-module(project_member_logic).
-compile([nowarn_deprecated_catch]).
%%%
% project_member_logic 项目成员业务逻辑（Channel-first-class W2 ZC-02）
%
% W2 权限模型（计划 §ZC-02 纲要细化；执行台账"权限模型决策"节）：
%   Project Owner（project.owner_id）= 项目完全管理权：邀请 / 移除 / 转移；
%   Workspace Owner 保留治理权：可移除任何 project member（移除端 DB 触发器
%     约束其与 workspace_member 移除的次序）；治理含成员列表读；
%   非 Owner 访问 Project 成员资源必须叠加 active Project Member——
%     直接用 project id 打成员接口、无 active 成员关系 → 403（W2 项目隔离）；
%   Workspace role='guest' → Project 只读（一切写操作 403）；
%   非 active workspace_member 的一切成员操作 → 403（DB 移除端/写入端触发器
%     兜底前的前置校验，失败无部分写入）。
%
% 错误约定：{error, {Code, Msg}}（Code 为语义码）直接透传；DS 内部失败
%   {error, {tag, Reason}} 在此归一 500（同 project_logic:create_checked）。
%%%

-export([list/4]).
-export([invite/3]).
-export([remove/3]).
-export([transfer_owner/3]).
-export([admin_page/3]).
-export([ensure_can_read/2]).
-export([ensure_can_write/2]).

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Admin 治理只读：项目成员分页（adm ACL workspaces:read 门；
%% 不走 workspace 成员权限语义，JOIN user 供治理面展示）
-spec admin_page(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_page(ProjectId, Page0, Size0) ->
    Page = max(1, Page0),
    Size = max(1, min(100, Size0)),
    case
        project_member_repo:page_by_project(
            ProjectId,
            Page,
            Size,
            <<"pm.workspace_id,pm.project_id,pm.user_id,pm.invited_by,pm.joined_at,",
                "pm.status,u.nickname,u.avatar,u.account">>
        )
    of
        {ok, P} ->
            {ok, P};
        {error, Reason} ->
            _ = ?ERROR_LOG([admin_member_page_failed, ProjectId, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end.

%% @doc 项目成员列表（分页；Project Owner / Workspace Owner / active PM 可读）
-spec list(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
list(Uid, ProjectId, Page0, Size0) ->
    case ensure_can_read(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            Page = max(Page0, 1),
            Size = max(1, min(Size0, 100)),
            case project_member_ds:list(ProjectId, Page, Size) of
                {ok, Result} ->
                    {ok, Result};
                {error, {Code, Msg}} when is_integer(Code) ->
                    {error, {Code, Msg}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([project_member_page_failed, ProjectId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% @doc 邀请项目成员（仅 Project Owner；Guest 403；幂等）
-spec invite(integer(), integer(), integer()) ->
    {ok, map(), created | existing} | {error, {integer(), binary()}}.
invite(Uid, ProjectId, TargetUid) ->
    case ensure_can_manage(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            case is_valid_uid(TargetUid) of
                false ->
                    {error, {400, <<"用户ID格式有误"/utf8>>}};
                true ->
                    case project_member_ds:invite(Uid, ProjectId, TargetUid) of
                        {ok, Member, Status} ->
                            _ = ?INFO_LOG([project_member_invited, ProjectId, Uid, TargetUid]),
                            {ok, Member, Status};
                        {error, {Code, Msg}} when is_integer(Code) ->
                            {error, {Code, Msg}};
                        {error, Reason} ->
                            _ = ?ERROR_LOG([project_member_invite_failed, ProjectId, Reason]),
                            {error, {500, <<"邀请失败，请稍后重试"/utf8>>}}
                    end
            end
    end.

%% @doc 移除项目成员（Project Owner 或 Workspace Owner 治理权；幂等；
%% 移除 Project Owner 本身 409 须先转移）
-spec remove(integer(), integer(), integer()) ->
    {ok, map(), removed | already_removed} | {error, {integer(), binary()}}.
remove(Uid, ProjectId, TargetUid) ->
    case ensure_can_govern(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, Project} ->
            case TargetUid =:= maps:get(<<"owner_id">>, Project, 0) of
                true ->
                    {error, {409, <<"项目 Owner 不能被移除，请先转移项目 Owner"/utf8>>}};
                false ->
                    case project_member_ds:remove(Uid, ProjectId, TargetUid) of
                        {ok, Result, Status} ->
                            _ = ?INFO_LOG([project_member_removed, ProjectId, Uid, TargetUid]),
                            {ok, Result, Status};
                        {error, {Code, Msg}} when is_integer(Code) ->
                            {error, {Code, Msg}};
                        {error, Reason} ->
                            _ = ?ERROR_LOG([project_member_remove_failed, ProjectId, Reason]),
                            {error, {500, <<"移除失败，请稍后重试"/utf8>>}}
                    end
            end
    end.

%% @doc 项目 Owner 转移（仅 Project Owner；目标 active 非 Guest ws member；
%% 目标有未完成 assignee task 409——执行台账假设 A2）
-spec transfer_owner(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
transfer_owner(Uid, ProjectId, TargetUid) ->
    case ensure_can_manage(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            case is_valid_uid(TargetUid) of
                false ->
                    {error, {400, <<"用户ID格式有误"/utf8>>}};
                true ->
                    case TargetUid =:= Uid of
                        true ->
                            {error, {400, <<"新 Owner 不能是当前 Owner"/utf8>>}};
                        false ->
                            transfer_owner_checked(Uid, ProjectId, TargetUid)
                    end
            end
    end.

transfer_owner_checked(Uid, ProjectId, TargetUid) ->
    case project_member_ds:transfer_owner(Uid, ProjectId, TargetUid) of
        {ok, Result} ->
            _ = ?INFO_LOG([project_owner_transferred, ProjectId, Uid, TargetUid]),
            {ok, Result};
        {error, {Code, Msg}} when is_integer(Code) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([project_member_transfer_failed, ProjectId, Reason]),
            {error, {500, <<"转移失败，请稍后重试"/utf8>>}}
    end.

%% ===================================================================
%% 权限辅助
%% ===================================================================

%% @doc 读权限：Project Owner / Workspace Owner（治理）/ active Project Member；
%% 其余（含非 Project Member 的普通 Workspace Member——直接 ID 直访）403
-spec ensure_can_read(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_read(Uid, ProjectId) ->
    case load_project(ProjectId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, Project} ->
            case actor_context(Uid, Project) of
                {error, Forbidden} ->
                    {error, Forbidden};
                {ok, Ctx} ->
                    case
                        maps:get(is_project_owner, Ctx) orelse
                            maps:get(is_ws_owner, Ctx) orelse
                            maps:get(is_active_pm, Ctx)
                    of
                        true -> {ok, Project};
                        false -> {error, {403, <<"仅项目成员可访问该项目资源"/utf8>>}}
                    end
            end
    end.

%% @doc 内容写权限：Project Owner 或 active Project Member；Guest 只读。
-spec ensure_can_write(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_write(Uid, ProjectId) ->
    case ensure_can_read(Uid, ProjectId) of
        {error, _} = Err ->
            Err;
        {ok, Project} ->
            case actor_context(Uid, Project) of
                {ok, #{ws_role := <<"guest">>}} ->
                    {error, {403, <<"Guest 角色为只读，不能执行该操作"/utf8>>}};
                {ok, Ctx} ->
                    case maps:get(is_project_owner, Ctx) orelse maps:get(is_active_pm, Ctx) of
                        true -> {ok, Project};
                        false -> {error, {403, <<"仅项目成员可修改该项目资源"/utf8>>}}
                    end
            end
    end.

%% @doc 管理权（邀请 / Owner 转移）：仅 Project Owner；Guest 身份只读 403
-spec ensure_can_manage(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_manage(Uid, ProjectId) ->
    case load_project(ProjectId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, Project} ->
            case actor_context(Uid, Project) of
                {error, Forbidden} ->
                    {error, Forbidden};
                {ok, Ctx} ->
                    case maps:get(ws_role, Ctx) of
                        <<"guest">> ->
                            {error, {403, <<"Guest 角色为只读，不能执行该操作"/utf8>>}};
                        _ ->
                            case maps:get(is_project_owner, Ctx) of
                                true -> {ok, Project};
                                false -> {error, {403, <<"仅项目 Owner 可执行该操作"/utf8>>}}
                            end
                    end
            end
    end.

%% @doc 治理权（移除）：Project Owner 或 Workspace Owner；Guest 身份只读 403
-spec ensure_can_govern(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_govern(Uid, ProjectId) ->
    case load_project(ProjectId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, Project} ->
            case actor_context(Uid, Project) of
                {error, Forbidden} ->
                    {error, Forbidden};
                {ok, Ctx} ->
                    case maps:get(ws_role, Ctx) of
                        <<"guest">> ->
                            {error, {403, <<"Guest 角色为只读，不能执行该操作"/utf8>>}};
                        _ ->
                            case
                                maps:get(is_project_owner, Ctx) orelse
                                    maps:get(is_ws_owner, Ctx)
                            of
                                true -> {ok, Project};
                                false -> {error, {403, <<"仅项目 Owner 可执行该操作"/utf8>>}}
                            end
                    end
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% actor 权限上下文：ws 角色（非 active ws member → 403）+ 三项布尔
-spec actor_context(integer(), map()) ->
    {ok, map()} | {error, {403, binary()}}.
actor_context(Uid, Project) ->
    WsId = maps:get(<<"workspace_id">>, Project),
    ProjectId = maps:get(<<"id">>, Project),
    case workspace_logic:my_role(WsId, Uid) of
        {error, _Reason} ->
            {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}};
        {ok, Role} ->
            IsProjectOwner = maps:get(<<"owner_id">>, Project, 0) =:= Uid,
            IsWsOwner = Role =:= <<"owner">>,
            IsActivePm =
                case project_member_ds:find(ProjectId, Uid) of
                    #{<<"status">> := <<"active">>} -> true;
                    _ -> false
                end,
            {ok, #{
                ws_role => Role,
                is_project_owner => IsProjectOwner,
                is_ws_owner => IsWsOwner,
                is_active_pm => IsActivePm
            }}
    end.

-spec load_project(integer()) -> {ok, map()} | {error, {404, binary()}}.
load_project(ProjectId) ->
    case project_ds:find_by_id(ProjectId) of
        Project when is_map(Project), map_size(Project) > 0 ->
            {ok, Project};
        _ ->
            {error, {404, <<"项目不存在"/utf8>>}}
    end.

-spec is_valid_uid(term()) -> boolean().
is_valid_uid(Uid) when is_integer(Uid), Uid > 0 -> true;
is_valid_uid(_) -> false.
