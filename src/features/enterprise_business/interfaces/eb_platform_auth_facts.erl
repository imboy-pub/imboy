%%% @doc 平台面的只读授权事实**装配适配器**（`eb_auth_port` 的 platform_admin 类实现）。
%%%
%%% 依据：`eb_auth_port` 冻结的 payload 形状（平台类：`#{adm_user_id, permissions}`）、
%%% plan §5.3（平台权限 `enterprise_business:read|write`）、EB-09-A01/A04。
%%%
%%% 为什么在 `interfaces/`：本模块**不新建任何 RBAC**，也不持有任何数据——它只是把
%%% 既有 Admin ACL（`adm_acl:permissions/1`，imboyadmin 的 role→permission 权威来源）
%%% 投影成企业授权契约要求的只读事实形状。放在这里的原因是租约（EB-09 只写
%%% `interfaces/**`）：企业侧的 `infrastructure/` 由 EB-03R/EB-07 拥有，而平台事实
%%% 是**平台既有机制**的适配，不是企业基础设施。
%%%
%%% 纪律（与 `eb_pg_auth_facts` 同规）：
%%%   * **逐请求**加载、**零缓存**（每次调用现读一次 `adm_acl:permissions/1`）；
%%%   * **零写**：本模块一个写操作都没有，只有一次权限读取；
%%%   * **fail-closed**：无 `adm_user_id` / 非正整数一律 `{error, _}`，绝不返回
%%%     「默认放行」的空事实；未知 Admin 用户得到空权限集 ⇒ 后续判定必然 403。
%%%   * 本模块自身**不含** SQL、不引 `elib_pg`/`eb_pg_*`（A05 的静态判据同样适用）。
-module(eb_platform_auth_facts).

-behaviour(eb_auth_port).

-export([load_request_facts/1, permissions/1]).

%% @doc 逐请求装载平台类授权事实（成员关系不参与平台判定：平台管理员不是 Org 成员，
%% 租户作用域由路由/用例的显式 OrgId+WorkspaceId 强制，见 `eb_platform_handler`）。
-spec load_request_facts(map()) -> {ok, map()} | {error, term()}.
load_request_facts(Request) when is_map(Request) ->
    case maps:get(adm_user_id, Request, undefined) of
        AdmUserId when is_integer(AdmUserId), AdmUserId > 0 ->
            {ok, #{adm_user_id => AdmUserId, permissions => permissions(AdmUserId)}};
        _Missing ->
            {error, {missing_adm_user_id, Request}}
    end;
load_request_facts(_Request) ->
    {error, invalid_request}.

%% @doc Admin 的权限集（唯一来源：既有 Admin ACL；未知用户/异常一律空集 ⇒ fail-closed）。
-spec permissions(integer()) -> [binary()].
permissions(AdmUserId) ->
    try adm_acl:permissions(AdmUserId) of
        Permissions when is_list(Permissions) -> [P || P <- Permissions, is_binary(P)];
        _Other -> []
    catch
        _:_ -> []
    end.
