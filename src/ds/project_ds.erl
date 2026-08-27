-module(project_ds).
-compile([nowarn_deprecated_catch]).
%%%
% project_ds 是 project domain service 缩写
% 项目领域服务（双体验 v2.5.2 WP4/T6a；分层镜像 WP3 workspace_ds）
%
% 核心职责：
%   1. 创建（单事务）：归档写守卫（workspace_guard:ensure_writable_tx，先锁
%      workspace 行）→ INSERT project（owner_id=creator，DB 复合 FK+可延迟
%      触发器兜底 owner active membership）。
%   2. 更新（单事务）：改名/描述/状态（active|done）——事务内先读
%      workspace_id → 写守卫锁行 → UPDATE；无物理删除。
%   3. 查询：find_by_id / 工作区分页列表（稳定排序 created_at DESC, id DESC，
%      limit 钳制 ≤100）。
%
% W0 硬约束（Gate W）：不建/不读/不写 project_member；无 Pinned/Resources/
% Activity 聚合查询（defer）；无 project_channel_rel（defer）；
% project_event 仅在 task 状态流转写入（T6b，经 project_event_ds）。
%%%

-export([create/4]).
-export([find_by_id/1]).
-export([page_by_workspace/3]).
-export([update_fields/2]).
-export([update_status/2]).

-include("log.hrl").

-define(MAX_PAGE_SIZE, 100).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 创建项目（单事务：写守卫 + INSERT；creator 即 Project Owner）
-spec create(integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
create(CreatorUid, WsId, Name, Description) ->
    Now = elib_dt:now(),
    Result =
        elib_pg:with_tx(fun(Conn) ->
            %% 归档写守卫：先锁 workspace 行（与归档/恢复事务线性化）
            ok = workspace_guard:abort_on_error(
                workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
            ),
            Data = #{
                <<"workspace_id">> => WsId,
                <<"name">> => Name,
                <<"description">> => Description,
                <<"owner_id">> => CreatorUid,
                <<"status">> => <<"active">>,
                <<"created_at">> => Now,
                <<"updated_at">> => Now
            },
            case project_repo:add_tx(Conn, Data) of
                {ok, ProjectId} ->
                    {ok, ProjectId};
                {error, Reason} ->
                    throw({abort_tx, {project_create_failed, Reason}})
            end
        end),
    case Result of
        {ok, ProjectId} -> {ok, find_by_id(ProjectId)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 项目详情
-spec find_by_id(integer()) -> map() | {error, term()}.
find_by_id(ProjectId) ->
    project_repo:find_by_id(
        ProjectId, <<"id,workspace_id,name,description,owner_id,status,created_at,updated_at">>
    ).

%% @doc 工作区项目列表（稳定排序 created_at DESC, id DESC；limit ≤ 100）
-spec page_by_workspace(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
page_by_workspace(WsId, Page0, Size0) ->
    Size = max(1, min(Size0, ?MAX_PAGE_SIZE)),
    Page = max(Page0, 1),
    project_repo:page_by_workspace(
        WsId,
        Page,
        Size,
        <<"id,workspace_id,name,description,owner_id,status,created_at,updated_at">>
    ).

%% @doc 更新项目字段（单事务：先读 workspace_id → 写守卫 → UPDATE）
%% Fields 仅含白名单键 name/description（logic 层已清洗）。
-spec update_fields(integer(), map()) -> {ok, map()} | {error, term()}.
update_fields(ProjectId, Fields) ->
    update_tx(ProjectId, fun(Conn, _Project) ->
        {ok, _} = project_repo:update_fields_tx(Conn, ProjectId, Fields),
        ok
    end).

%% @doc 更新项目状态（active|done；单事务同上）
-spec update_status(integer(), binary()) -> {ok, map()} | {error, term()}.
update_status(ProjectId, Status) ->
    update_tx(ProjectId, fun(Conn, _Project) ->
        Now = elib_dt:now(),
        {ok, _} = project_repo:update_fields_tx(
            Conn, ProjectId, #{<<"status">> => Status, <<"updated_at">> => Now}
        ),
        ok
    end).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 通用事务更新：事务内先读项目行（含 workspace_id）→ 写守卫锁行 → Fun
-spec update_tx(integer(), fun((any(), map()) -> ok | {error, term()})) ->
    {ok, map()} | {error, term()}.
update_tx(ProjectId, Fun) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            Project = project_repo:find_tx(Conn, ProjectId, <<"id,workspace_id">>),
            case maps:get(<<"workspace_id">>, Project, undefined) of
                WsId when is_integer(WsId) ->
                    ok = workspace_guard:abort_on_error(
                        workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
                    ),
                    case Fun(Conn, Project) of
                        ok -> ok;
                        {error, Reason} -> throw({abort_tx, Reason})
                    end;
                _ ->
                    throw({abort_tx, project_not_found})
            end
        end),
    case Result of
        ok -> {ok, find_by_id(ProjectId)};
        {error, Reason} -> {error, Reason}
    end.
