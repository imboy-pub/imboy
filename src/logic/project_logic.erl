-module(project_logic).
-compile([nowarn_deprecated_catch]).
%%%
% project_logic 项目业务逻辑（双体验 v2.5.2 WP4/T6a）
%
% W0 权限模型（计划 §1.4.2 三角色矩阵 + Gate W=W0）：
%   创建：Workspace Owner/Member ✅；Guest 403（只读）；非工作区成员 403
%   详情/列表：active Workspace Member 可见（W0：Project 对全部 active 成员开放，
%             无 project_member、无 /projects/:id/members 端点）
%   改名/描述/状态：Owner/Member 可写；Guest 403
%   删除：无物理删除（仅 status active|done 流转）
%
% 创建时 creator 同事务成为 Project Owner（owner_id 列 + DB 复合 FK +
% 可延迟触发器兜底 active membership；无 project_member 表行——W0 的
% "Owner"语义只体现在 project.owner_id 列 + workspace_member 角色上下文）。
%
% 归档写守卫：archived workspace 拒写（workspace_guard，稳定错误码 980）。
%
% 移除 Workspace Member 前的 Task 冲突检查（供 workspace_logic 移除流程）：
% 数据来源 workspace_member_repo:unfinished_tasks_of_user/3
% （project_task JOIN project WHERE assignee_id=? AND status<>'done'），
% WP3 已接入 remove_member_tx 的 409 membership_conflict 冲突清单。
%
% 错误约定：{error, {Code, Msg}}，Code 取 error_code.hrl 语义码。
%%%

-export([create/4]).
-export([detail/2]).
-export([list/4]).
-export([update/4]).
-export([update_status/3]).
-export([valid_status/1]).
%% Admin 运营管理入口（双体验 v2.5.2 WP7/T11b；只读，无写操作；
%% 鉴权在 adm_workspace_handler 层走 adm_acl）
-export([admin_page/4]).
-export([admin_detail/1]).

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 创建项目（Owner/Member 可建；Guest/非成员 403；archived 980）
-spec create(integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
create(Uid, WsId, Name, Description) ->
    case workspace_exists(WsId) of
        false ->
            {error, {404, <<"工作区不存在"/utf8>>}};
        true ->
            case workspace_logic:ensure_can_create_resource(WsId, Uid) of
                {error, Reason} ->
                    {error, Reason};
                ok ->
                    case valid_name(Name) of
                        false ->
                            {error, {400, <<"项目名称不能为空且不超过 200 字符"/utf8>>}};
                        true ->
                            create_checked(Uid, WsId, Name, normalize_description(Description))
                    end
            end
    end.

create_checked(Uid, WsId, Name, Description) ->
    case project_ds:create(Uid, WsId, Name, Description) of
        {ok, Project} ->
            _ = ?INFO_LOG([project_created, Uid, WsId, maps:get(<<"id">>, Project, 0)]),
            {ok, Project};
        {error, {Code, Msg}} when is_integer(Code) ->
            %% 归档守卫稳定错误码（980）等已成形错误直接透传
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([project_create_failed, Uid, WsId, Reason]),
            {error, {500, <<"项目创建失败，请稍后重试"/utf8>>}}
    end.

%% @doc 项目详情（Project Owner / Workspace Owner / active Project Member 可读）
-spec detail(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
detail(Uid, ProjectId) ->
    project_member_logic:ensure_can_read(Uid, ProjectId).

%% @doc 工作区项目列表（active 工作区成员可读；稳定排序+分页）
-spec list(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
list(Uid, WsId, Page, Size) ->
    case workspace_logic:ensure_member(WsId, Uid) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Role} ->
            case project_ds:page_by_workspace(WsId, Page, Size) of
                {ok, Result} ->
                    {ok, Result};
                {error, Reason2} ->
                    _ = ?ERROR_LOG([project_page_failed, WsId, Reason2]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% @doc 改名/描述（Owner/Member 可写；Guest 403；archived 980）
-spec update(integer(), integer(), binary() | undefined, binary() | undefined) ->
    {ok, map()} | {error, {integer(), binary()}}.
update(Uid, ProjectId, Name, Description) ->
    case ensure_can_write(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, Project} ->
            Name2 =
                case Name of
                    N when is_binary(N), N =/= <<>> -> N;
                    _ -> maps:get(<<"name">>, Project, <<>>)
                end,
            case valid_name(Name2) of
                false ->
                    {error, {400, <<"项目名称不能为空且不超过 200 字符"/utf8>>}};
                true ->
                    Description2 =
                        case Description of
                            D when is_binary(D) -> D;
                            _ -> maps:get(<<"description">>, Project, <<>>)
                        end,
                    case
                        project_ds:update_fields(ProjectId, #{
                            <<"name">> => Name2,
                            <<"description">> => Description2,
                            <<"updated_at">> => elib_dt:now()
                        })
                    of
                        {ok, Updated} ->
                            {ok, Updated};
                        {error, {Code, Msg}} when is_integer(Code) ->
                            {error, {Code, Msg}};
                        {error, Reason2} ->
                            _ = ?ERROR_LOG([project_update_failed, ProjectId, Uid, Reason2]),
                            {error, {500, <<"更新失败，请稍后重试"/utf8>>}}
                    end
            end
    end.

%% @doc 状态流转（active|done；Owner/Member 可写；Guest 403；archived 980）
-spec update_status(integer(), integer(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
update_status(Uid, ProjectId, Status) ->
    case valid_status(Status) of
        false ->
            {error, {400, <<"项目状态仅支持 active/done"/utf8>>}};
        true ->
            case ensure_can_write(Uid, ProjectId) of
                {error, Reason} ->
                    {error, Reason};
                {ok, _Project} ->
                    case project_ds:update_status(ProjectId, Status) of
                        {ok, Updated} ->
                            _ = ?INFO_LOG([project_status_changed, ProjectId, Uid, Status]),
                            {ok, Updated};
                        {error, {Code, Msg}} when is_integer(Code) ->
                            {error, {Code, Msg}};
                        {error, Reason2} ->
                            _ = ?ERROR_LOG([project_status_failed, ProjectId, Uid, Reason2]),
                            {error, {500, <<"更新失败，请稍后重试"/utf8>>}}
                    end
            end
    end.

%% ===================================================================
%% 权限/校验辅助
%% ===================================================================

%% @doc 项目写权限（编辑/状态流转）：Owner/Member ✅；Guest/非成员 403
%% （§1.4.2 矩阵"创建 Project；编辑有权 Project"行——W0 全部 active 成员有权）
-spec ensure_can_write(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_write(Uid, ProjectId) ->
    project_member_logic:ensure_can_write(Uid, ProjectId).

%% @doc 项目状态值域（W0：仅 active|done）
-spec valid_status(binary()) -> boolean().
valid_status(<<"active">>) -> true;
valid_status(<<"done">>) -> true;
valid_status(_) -> false.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec load_project(integer()) -> {ok, map()} | {error, {404, binary()}}.
load_project(ProjectId) ->
    case project_ds:find_by_id(ProjectId) of
        Project when is_map(Project), map_size(Project) > 0 ->
            {ok, Project};
        _ ->
            {error, {404, <<"项目不存在"/utf8>>}}
    end.

-spec workspace_exists(integer()) -> boolean().
workspace_exists(WsId) ->
    case workspace_ds:find_by_id(WsId, <<"id">>) of
        #{<<"id">> := _} -> true;
        _ -> false
    end.

-spec valid_name(term()) -> boolean().
valid_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    %% varchar(200) 列宽，按字符数校验防截断报错
    string:length(Name) =< 200;
valid_name(_) ->
    false.

-spec normalize_description(term()) -> binary().
normalize_description(D) when is_binary(D) -> D;
normalize_description(_) -> <<>>.

%% ===================================================================
%% Admin 运营管理（双体验 v2.5.2 WP7/T11b；只读展示 + assignee 概览）
%% ===================================================================

%% @doc Admin 项目分页列表（搜索/状态筛选；批量任务计数防 N+1）
-spec admin_page(integer(), integer(), binary() | all, binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_page(Page, Size, Status, Keyword) ->
    Status2 = normalize_admin_status(Status),
    case project_ds:admin_page(Page, Size, Status2, Keyword) of
        {ok, #{list := []} = Result} ->
            %% 空页跳过任务计数查询
            {ok, Result};
        {ok, #{list := Rows} = Result} ->
            Counts = project_ds:admin_batch_task_counts([maps:get(<<"id">>, Row, 0) || Row <- Rows]),
            List2 = [attach_task_counts(Row, Counts) || Row <- Rows],
            {ok, Result#{list => List2}};
        {error, Reason} ->
            _ = ?ERROR_LOG([project_admin_page_failed, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end.

%% @doc Admin 项目详情（只读：基本信息 + workspace 概要 + task 状态分布 + assignee 概览）
%% W0 无 project member——不展示成员表，仅 assignee 聚合（Admin 不提供成员增删）。
-spec admin_detail(integer()) -> {ok, map()} | {error, {404, binary()}}.
admin_detail(ProjectId) ->
    case load_project(ProjectId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, Project} ->
            WsId = maps:get(<<"workspace_id">>, Project),
            WS = workspace_ds:find_by_id(WsId, <<"id,name,status,owner_id">>),
            Owner = user_ds:find_by_id(
                maps:get(<<"owner_id">>, Project, 0), <<"id,nickname,avatar,account">>
            ),
            TaskStats = project_ds:admin_task_status_stats(ProjectId),
            {ok, Assignees} = project_ds:admin_assignee_overview(ProjectId, 20),
            {ok, Project#{
                workspace => WS,
                owner => Owner,
                task_stats => TaskStats,
                assignees => Assignees
            }}
    end.

%% Admin 状态筛选归一：仅认 active/done，其余 all
-spec normalize_admin_status(binary() | all) -> binary() | all.
normalize_admin_status(<<"active">>) -> <<"active">>;
normalize_admin_status(<<"done">>) -> <<"done">>;
normalize_admin_status(_) -> all.

%% 列表行附加任务计数（缺省 0）
-spec attach_task_counts(map(), map()) -> map().
attach_task_counts(Row, Counts) ->
    Pid = maps:get(<<"id">>, Row, 0),
    Entry = maps:get(Pid, Counts, #{}),
    Row#{
        <<"task_total">> => maps:get(total, Entry, 0),
        <<"task_done">> => maps:get(done, Entry, 0)
    }.
