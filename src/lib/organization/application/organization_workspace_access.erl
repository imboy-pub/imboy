%%% @doc Organization → Workspace 管理权限映射（GZAPP-02/G7，D04 权限矩阵后端复核）。
%%%
%%% 语义：**org owner/admin 对本 org 的全部 Workspace 拥有管理权**——
%%% 与「本 Workspace owner」并列为 workspace 管理类操作（archive/restore/
%%% 成员管理/角色变更）的两个合法授权源。
%%%
%%% 边界铁律（不改既有语义，仅新增判定入口）：
%%%   * 只做**权限判定**：不写 workspace_member 行、不授予 ws 角色——
%%%     C05「Org Role ≠ Workspace Role，org owner 不自动成为 ws owner」
%%%     依然成立；本模块只是把「org 治理者可管理 org 资产」显式化为可复用谓词；
%%%   * org member（非 owner/admin）/ 非成员 → 无管理权（403）；
%%%   * 个人域 Workspace（organization_id 为空）与本模块无关——
%%%     其管理权仍仅由 workspace owner 裁决；
%%%   * 判定真源 = organization_member 的 active 行 role ∈ {owner, admin}
%%%     （与 workspace_ds:create_template 的 organization_create_forbidden
%%%     同一裁决口径）。
%%%
%%% 两个入口：
%%%   * `ensure_org_manager/2`：管理操作入口校验（ok | {error, {403|503, Msg}}，
%%%     DB 异常 fail-closed 503）；
%%%   * `is_org_manager_tx/3`：事务内谓词（供同事务二次校验复用，
%%%     如 change_role_tx 的 Actor 防伪；个人域 undefined 恒 false）。
-module(organization_workspace_access).

-export([ensure_org_manager/2]).
-export([is_org_manager_tx/3]).
%% GZAPP-03：Workspace 级入口（群/频道访问门复用）
-export([ensure_org_manager_for_ws/2]).

-include("log.hrl").

-define(ORG_MANAGER_FORBIDDEN,
    {403, <<"仅工作区 Owner 或组织 Owner/Admin 可执行该操作"/utf8>>}
).

%% ===================================================================
%% 入口校验（非事务）
%% ===================================================================

%% @doc 判定 UserId 是否 OrgId 的 owner/admin（管理权）。
%% ok 通过；非 owner/admin / 非成员 → 403；DB 异常 fail-closed 503。
-spec ensure_org_manager(integer(), integer()) -> ok | {error, {403 | 503, binary()}}.
ensure_org_manager(OrgId, UserId) ->
    case organization_member_repo:find_active(OrgId, UserId, <<"role">>) of
        {ok, #{<<"role">> := Role}} when Role =:= <<"owner">>; Role =:= <<"admin">> ->
            ok;
        {ok, _OtherRole} ->
            {error, ?ORG_MANAGER_FORBIDDEN};
        {error, not_found} ->
            {error, ?ORG_MANAGER_FORBIDDEN};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_manager_lookup_failed, OrgId, UserId, Reason]),
            {error, {503, <<"组织权限校验暂时不可用，请稍后重试"/utf8>>}}
    end.

%% @doc Workspace 级管理权判定：Workspace 归属 Organization 的 owner/admin。
%% 个人域 Workspace（organization_id 为空/NULL）→ 403（无 org 可授权，
%% 其治理权仍仅由 workspace owner 裁决）；Workspace 不存在 → 403（不泄露
%% 存在性）；DB 异常 fail-closed 503。
-spec ensure_org_manager_for_ws(integer(), integer()) ->
    ok | {error, {403 | 503, binary()}}.
ensure_org_manager_for_ws(WsId, UserId) ->
    case workspace_repo:find_by_id(WsId, <<"organization_id">>) of
        #{<<"organization_id">> := OrgId} when is_integer(OrgId), OrgId > 0 ->
            ensure_org_manager(OrgId, UserId);
        #{} ->
            {error, ?ORG_MANAGER_FORBIDDEN};
        {error, Reason} ->
            _ = ?ERROR_LOG([organization_ws_scope_lookup_failed, WsId, UserId, Reason]),
            {error, {503, <<"组织权限校验暂时不可用，请稍后重试"/utf8>>}}
    end.

%% ===================================================================
%% 事务内谓词
%% ===================================================================

%% @doc 事务内判定 UserId 是否 OrgId 的 owner/admin。
%% 个人域（OrgId=undefined）恒 false；DB 异常恒 false（fail-closed）。
-spec is_org_manager_tx(any(), integer() | undefined, integer()) -> boolean().
is_org_manager_tx(_Conn, undefined, _UserId) ->
    false;
is_org_manager_tx(Conn, OrgId, UserId) ->
    case organization_member_repo:find_active_tx(Conn, OrgId, UserId, <<"role">>) of
        {ok, #{<<"role">> := Role}} when Role =:= <<"owner">>; Role =:= <<"admin">> ->
            true;
        _ ->
            false
    end.
