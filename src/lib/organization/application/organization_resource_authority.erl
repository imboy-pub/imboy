%%% @doc 资源级 Organization 管理权判定（GZAPP-03/D04 权限矩阵后端复核）。
%%%
%%% 语义：企业群/企业频道（scope=workspace 的 group/channel）除「资源自身
%%% 授权源」（群主 / 频道创建者 / 工作区成员）之外，**本资源所属 Workspace
%%% 的 Organization owner/admin 也是合法授权源**——D04「Org Owner/Admin
%%% 管理全部 Workspace、企业群和企业频道」。
%%%
%%% 授权链：{group|channel, Id} → workspace_resolver 解析归属 Workspace →
%%% Workspace.organization_id → organization_workspace_access:ensure_org_manager/2。
%%%
%%% 边界铁律：
%%%   * personal 域资源（无 Workspace 归属）恒 403——其授权源仍只有资源
%%%     自身的 owner/creator，本模块不改变个人域语义；
%%%   * 资源不存在 / Workspace 无 Organization → 403（不泄露存在性，
%%%     与跨租户探测同一稳定错误）；
%%%   * resolver / DB 异常 → 503 fail-closed（不放行）。
-module(organization_resource_authority).

-export([ensure_manager/2]).

-include("log.hrl").

-define(RESOURCE_MANAGER_FORBIDDEN,
    {403, <<"仅资源所有者或组织 Owner/Admin 可执行该操作"/utf8>>}
).

%% @doc 判定 UserId 是否该资源所属 Organization 的 owner/admin。
%% ok 通过；personal 资源 / 非 org manager / 资源不存在 → 403；
%% DB 异常 fail-closed 503。
-spec ensure_manager({group, integer()} | {channel, integer()}, integer()) ->
    ok | {error, {403 | 503, binary()}}.
ensure_manager(Resource, UserId) ->
    try workspace_resolver:resolve_workspace(Resource) of
        {ok, WsId} ->
            organization_workspace_access:ensure_org_manager_for_ws(WsId, UserId);
        personal ->
            {error, ?RESOURCE_MANAGER_FORBIDDEN};
        {error, not_found} ->
            {error, ?RESOURCE_MANAGER_FORBIDDEN};
        {error, _Reason} ->
            {error, ?RESOURCE_MANAGER_FORBIDDEN}
    catch
        %% resolver 契约：DB 层异常以 error:{resolver_db_error, _} 抛出
        error:{resolver_db_error, Reason} ->
            _ = ?ERROR_LOG([resource_authority_resolver_failed, Resource, UserId, Reason]),
            {error, {503, <<"权限校验暂时不可用，请稍后重试"/utf8>>}};
        Class:Reason ->
            _ = ?ERROR_LOG([resource_authority_unexpected, Resource, UserId, Class, Reason]),
            {error, {503, <<"权限校验暂时不可用，请稍后重试"/utf8>>}}
    end.
