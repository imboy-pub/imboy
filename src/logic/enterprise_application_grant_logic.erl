-module(enterprise_application_grant_logic).

-moduledoc "Application Grant 授权求值层（FULL-01）—— Organization/Workspace Grant 与 scope 判定。".
%%%
% enterprise_application_grant_logic 是 Application Grant 的授权求值层
% （FULL-01 / plan-full §3.1「Organization Grant + Workspace Grant；scope 与
% 资源授权取交集，逐请求读取当前状态，撤权立即生效」；V2.1 §5.2 移除
% 未受管兼容旁路）。
%
% 求值模型（无缓存、无跨请求结论）：
%   * **生效 scope** = Application.allowed_scopes（能力上限，固定枚举）
%     ∩ ⋃{生效 Grant 的 scope 集合}——**无条件求交**：零 Grant（从未授予、
%     全部撤销或全部过期）⇒ 空集 ⇒ scope gate 与资源边界一律拒绝
%     （403 insufficient_scope，§5.2 求值序第 9 步「at least one current
%     active Grant exists」）。V2.1 起不存在「零 Grant 回退 allowed_scopes」
%     的广州期兼容旁路；本地 fixture 必须显式创建 Grant（F-09/D-06）。
%   * `grant_governed` 仅作**诊断标记**（该 Application 是否存在任何 Grant
%     行，含 revoked/已过期；授权行被 00000139 触发器禁止物理删除 ⇒ 只增
%     不减），不参与任何授权判定分支。
%   * **资源边界**：workspace 级资源必须由**同一个**生效 Grant 同时覆盖 scope 与
%     workspace（kind=none 覆盖 org 全域；kind=explicit 需显式命中）——不允许
%     「scope 来自 A、workspace 来自 B」的拼接。org 级资源（manifest
%     grant=org scoped 的路径）要求**同一**生效 Grant 覆盖 scope 与 Org 全域
%     （kind='none'）：显式 Workspace Grant 不授权 org 级操作。
%     workspace 级入口 = require_workspace_tx/4；org 级入口 = require_org_tx/3。
%   * 每次求值都是一次真库读（经 00000139 的 effective 视图，读时求值）；撤销/
%     到期/降级在**下一次请求**即生效。本模块不持有任何进程字典/ETS/缓存。
%
% 失败闭合：授权读取失败（DB 错误/形态异常）一律 {error, security_gate_closed}，
% 绝不降级为放行；上下文缺字段/形态非法同样 fail-closed。
%
% 分层：本模块是 Logic 层，只依赖 Repo 与 scope 枚举模块。认证链（api 层，
% EPGZ-02 既有链）与 FULL-02 的 handler 通过本模块求值，不直接读授权表。
%%%

-export([
    context_tx/4,
    require_workspace_tx/4,
    require_org_tx/3
]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc auth context 的授权求值（认证链内、同一事务、逐请求调用）。
%% AppScopes 是 Application.allowed_scopes 解出的固定 scope 列表。
%% 返回 {ok, #{grant_governed := boolean(), effective_scopes := [binary()]}}；
%% 读取失败 fail-closed 成 {error, security_gate_closed}。
%% 语义（V2.1 §5.2）：生效 scope 恒为 AppScopes ∩ 生效 Grant scopes——
%% 零 Grant ⇒ 空集（scope gate 必然拒绝，「撤权/降级/从未授权下一请求即
%% 失败」统一成立）。grant_governed 只是诊断标记（是否存在任何 Grant 行），
%% 不再作为回退 allowed_scopes 的开关。
-spec context_tx(any(), integer(), integer(), [binary()]) ->
    {ok, #{grant_governed := boolean(), effective_scopes := [binary()]}}
    | {error, security_gate_closed}.
context_tx(Conn, OrgId, AppId, AppScopes) when
    is_integer(OrgId), is_integer(AppId), is_list(AppScopes)
->
    case enterprise_application_grant_repo:grant_governed_tx(Conn, OrgId, AppId) of
        {ok, Governed} ->
            case enterprise_application_grant_repo:effective_scopes_tx(Conn, OrgId, AppId) of
                {ok, GrantScopes} ->
                    {ok, #{
                        grant_governed => Governed,
                        effective_scopes => intersect(AppScopes, GrantScopes)
                    }};
                {error, _Reason} ->
                    {error, security_gate_closed}
            end;
        {error, _Reason} ->
            {error, security_gate_closed}
    end;
context_tx(_Conn, _OrgId, _AppId, _AppScopes) ->
    {error, security_gate_closed}.

%% @doc 资源边界求值（FULL-02 handler 在每个 workspace 级操作上逐请求调用）：
%% Ctx 是认证链产物（enterprise_internal_auth:authenticate_tx/3 的返回）；
%% RequiredScope 是该操作要求的**固定** scope。
%% V2.1 §5.2：无未受管旁路——RequiredScope 必须在**生效** scope 内（零 Grant
%% ⇒ 空集 ⇒ insufficient_scope），且存在同一生效 Grant 同时覆盖该 scope 与
%% WorkspaceId。
%% 拒绝码：insufficient_scope（scope 不在生效集）/ organization_boundary_violation
%% （scope 有但无 Grant 覆盖该 workspace）/ security_gate_closed（读取失败或
%% 上下文形态非法，fail-closed）。
-spec require_workspace_tx(any(), map(), integer(), binary()) ->
    ok
    | {error,
        insufficient_scope
        | organization_boundary_violation
        | security_gate_closed}.
require_workspace_tx(Conn, Ctx, WorkspaceId, RequiredScope) when
    is_map(Ctx), is_integer(WorkspaceId), is_binary(RequiredScope)
->
    case ctx_effective_scopes(Ctx) of
        {ok, Effective} ->
            case lists:member(RequiredScope, Effective) of
                false ->
                    {error, insufficient_scope};
                true ->
                    OrgId = maps:get(organization_id, Ctx),
                    AppId = maps:get(application_id, Ctx),
                    case
                        enterprise_application_grant_repo:workspace_covered_tx(
                            Conn, OrgId, AppId, WorkspaceId, RequiredScope
                        )
                    of
                        {ok, true} -> ok;
                        {ok, false} -> {error, organization_boundary_violation};
                        {error, _Reason} -> {error, security_gate_closed}
                    end
            end;
        {error, _Reason} ->
            {error, security_gate_closed}
    end;
require_workspace_tx(_Conn, _Ctx, _WorkspaceId, _RequiredScope) ->
    {error, security_gate_closed}.

%% @doc 资源边界求值（**org 级**资源，FULL-02；INT-02/03/07/08/09 等 manifest
%% grant=org scoped 的路径）：Ctx 是认证链产物，RequiredScope 是该操作要求的
%% 固定 scope。
%% V2.1 §5.2：无未受管旁路——RequiredScope 必须在**生效** scope 内（零 Grant
%% ⇒ 空集 ⇒ insufficient_scope），且存在**覆盖 Org 全域**的同一生效 Grant
%% （workspace_scope_kind='none'）同时覆盖该 scope。显式 Workspace Grant
%% 只覆盖列出的 workspace，不授权 org 级操作（否则窄授权会被隐式放大，
%% fail-open）。
%% 拒绝码同 require_workspace_tx/4：insufficient_scope（scope 不在生效集）/
%% organization_boundary_violation（scope 有但无 Org 全域 Grant 覆盖）/
%% security_gate_closed（读取失败或上下文形态非法，fail-closed）。
-spec require_org_tx(any(), map(), binary()) ->
    ok
    | {error,
        insufficient_scope
        | organization_boundary_violation
        | security_gate_closed}.
require_org_tx(Conn, Ctx, RequiredScope) when is_map(Ctx), is_binary(RequiredScope) ->
    case ctx_effective_scopes(Ctx) of
        {ok, Effective} ->
            case lists:member(RequiredScope, Effective) of
                false ->
                    {error, insufficient_scope};
                true ->
                    OrgId = maps:get(organization_id, Ctx),
                    AppId = maps:get(application_id, Ctx),
                    case
                        enterprise_application_grant_repo:org_covered_tx(
                            Conn, OrgId, AppId, RequiredScope
                        )
                    of
                        {ok, true} -> ok;
                        {ok, false} -> {error, organization_boundary_violation};
                        {error, _Reason} -> {error, security_gate_closed}
                    end
            end;
        {error, _Reason} ->
            {error, security_gate_closed}
    end;
require_org_tx(_Conn, _Ctx, _RequiredScope) ->
    {error, security_gate_closed}.

%% ===================================================================
%% Internal
%% ===================================================================

%% 从认证上下文取「生效 scope」（含 org/app 身份形态校验）；形态非法
%% （缺键/非整数/非列表）fail-closed。grant_governed 不再参与判定
%% （V2.1：仅诊断标记，ctx 内有无该键不影响结果）。
-spec ctx_effective_scopes(map()) -> {ok, [binary()]} | {error, malformed_ctx}.
ctx_effective_scopes(Ctx) ->
    case
        {
            maps:get(organization_id, Ctx, undefined),
            maps:get(application_id, Ctx, undefined),
            maps:get(granted_scopes, Ctx, undefined)
        }
    of
        {OrgId, AppId, Scopes} when
            is_integer(OrgId), is_integer(AppId), is_list(Scopes)
        ->
            {ok, [S || S <- Scopes, is_binary(S)]};
        _ ->
            {error, malformed_ctx}
    end.

%% 交集：去重、稳定排序（scope_gate 用 lists:member，顺序无语义）。
-spec intersect([binary()], [binary()]) -> [binary()].
intersect(AppScopes, GrantScopes) ->
    Set = sets:from_list([S || S <- GrantScopes, is_binary(S)]),
    lists:usort([S || S <- AppScopes, is_binary(S), sets:is_element(S, Set)]).
