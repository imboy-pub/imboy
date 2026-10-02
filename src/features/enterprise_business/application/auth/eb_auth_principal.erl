%%% @doc 五类 Principal 与「route metadata 是唯一认证分流依据」的解析。
%%%
%%% 依据：plan v4.1 EB-D09、EB-D10、EB-04；§5.1/§5.2/§5.3 的租户 / 平台面划分。
%%%
%%% 五类 `auth_context`（EB-D10，顺序冻结）：
%%%
%%% | principal               | 凭证            | 必需条件 |
%%% |-------------------------|-----------------|----------|
%%% | `enterprise_owner_admin`| 普通 IMBoy JWT  | active org member + governance role owner/admin |
%%% | `enterprise_member`     | 普通 IMBoy JWT  | active org member + active identity assignment + function 类型 + 独立 permission |
%%% | `platform_admin`        | Admin session   | 对应 `enterprise_business:*` / `customer_service:*` 权限 |
%%% | `cs_visit`              | 短期 visit token| digest 有效、未过期/吊销、绑定同一 Org/contact |
%%% | `cs_shop_key`           | shop key        | digest 有效、未吊销、绑定同一 Org |
%%%
%%% 分流纪律（EB-D10）：
%%%
%%%   * **route metadata 是唯一依据**。`principal_for_route/1` 只读路由第三元素（既有
%%%     路由表 `{Path, Handler, Opts}` 的 `Opts`）里的 `auth_context` 等键；
%%%     `path` 只用于**校验元数据自洽**，绝不作为 principal 的来源——不得按 URL
%%%     字符串猜测。
%%%   * **面与 principal 必须相容**。声明 `tenant` 面却要 `platform_admin`（或反之）
%%%     一律 fail-closed（`surface_principal_mismatch`）：企业身份没有路径越权到
%%%     `/api/adm`，Admin session 也不能冒充 Organization member。
%%%   * **缺元数据 fail-closed**（`missing_auth_context`），不降级为「不要求企业授权」。
%%% 本模块是**纯函数**：无 I/O、无进程、无隐式时间/随机源。
-module(eb_auth_principal).

-export([
    principals/0,
    credential_class/1,
    tenant_surface_prefixes/0,
    platform_surface_prefixes/0,
    is_tenant_surface_path/1,
    is_platform_surface_path/1,
    classify_surface/1,
    principal_for_route/1
]).

-type principal() :: atom().
-type surface() :: tenant | platform | personal.
-type credential_class() :: imboy_jwt | adm_session | visit_token | shop_key.
-type requirement() :: #{
    surface := surface() | undefined,
    principal := principal(),
    required_function := binary() | undefined,
    required_permission := binary() | undefined,
    required_governance := [binary()],
    require_contact := boolean()
}.

-export_type([principal/0, surface/0, credential_class/0, requirement/0]).

%% 租户面前缀（无尾斜杠；`/api/v1/enterprise` 与 `/api/v1/enterprise/...` 都算）。
%% V1 仅 `function_key=sales|customer_service`，故租户面只有企业通用面与客服面。
-define(TENANT_PREFIXES, [<<"/api/v1/enterprise">>, <<"/api/v1/cs">>, <<"/api/v1/seat">>]).

%% 平台运营面前缀（EB-D09：imboyadmin 只走 `/api/adm`）。
-define(PLATFORM_PREFIXES, [<<"/adm">>, <<"/api/adm">>]).

%% ===================================================================
%% 五类 principal
%% ===================================================================

%% @doc 五类 principal（顺序冻结，便于逐字审计）。
-spec principals() -> [principal()].
principals() ->
    [
        enterprise_owner_admin,
        enterprise_member,
        platform_admin,
        cs_visit,
        cs_shop_key
    ].

%% @doc 该 principal 类别要求的凭证类别。凭证类别不符 → `principal_mismatch`。
-spec credential_class(principal()) -> credential_class().
credential_class(enterprise_owner_admin) -> imboy_jwt;
credential_class(enterprise_member) -> imboy_jwt;
credential_class(platform_admin) -> adm_session;
credential_class(cs_visit) -> visit_token;
credential_class(cs_shop_key) -> shop_key;
credential_class(Unknown) -> {error, {unknown_principal, Unknown}}.

%% ===================================================================
%% 面（surface）判定
%% ===================================================================

-spec tenant_surface_prefixes() -> [binary()].
tenant_surface_prefixes() ->
    ?TENANT_PREFIXES.

-spec platform_surface_prefixes() -> [binary()].
platform_surface_prefixes() ->
    ?PLATFORM_PREFIXES.

%% @doc 是否企业租户面路径。前缀匹配按**段边界**，`/api/v1/enterprisex` 不算。
-spec is_tenant_surface_path(binary()) -> boolean().
is_tenant_surface_path(Path) ->
    matches_any_prefix(Path, ?TENANT_PREFIXES).

%% @doc 是否平台运营面路径。
-spec is_platform_surface_path(binary()) -> boolean().
is_platform_surface_path(Path) ->
    matches_any_prefix(Path, ?PLATFORM_PREFIXES).

%% @doc 按路径归类面；两面同时命中视为元数据/路由表缺陷，fail-closed。
-spec classify_surface(binary()) -> {ok, surface()} | {error, term()}.
classify_surface(Path) when is_binary(Path) ->
    Tenant = is_tenant_surface_path(Path),
    Platform = is_platform_surface_path(Path),
    case {Tenant, Platform} of
        {true, true} -> {error, {ambiguous_surface, tenant, platform}};
        {true, false} -> {ok, tenant};
        {false, true} -> {ok, platform};
        {false, false} -> {ok, personal}
    end;
classify_surface(Path) ->
    {error, {invalid_path, Path}}.

matches_any_prefix(Path, Prefixes) when is_binary(Path), is_list(Prefixes) ->
    lists:any(fun(Prefix) -> matches_prefix(Path, Prefix) end, Prefixes);
matches_any_prefix(_Path, _Prefixes) ->
    false.

matches_prefix(Path, Prefix) ->
    Size = byte_size(Prefix),
    case Path of
        Prefix ->
            true;
        <<Prefix:Size/binary, Next, _Rest/binary>> when Next =:= $/ ->
            true;
        _ ->
            false
    end.

%% ===================================================================
%% route metadata → 授权需求
%% ===================================================================

%% @doc 从路由元数据解析授权需求（**唯一**的 principal 来源）。
%%
%% 判定顺序固定（使同一输入的失败原因唯一可复现）：
%%   1. 元数据必须是 map；
%%   2. 必须有 `auth_context` 且取值属于 `principals/0`；
%%   3. 声明面与 path 推断面必须一致（声明了 `surface` 且带 `path` 时）；
%%   4. 有效面（声明的优先，否则由 `path` 推断）必须与 principal 相容；
%%   5. 该 principal 的必需键必须齐备（缺则 fail-closed，不取默认放行）。
-spec principal_for_route(map()) -> {ok, requirement()} | {error, term()}.
principal_for_route(Metadata) when is_map(Metadata) ->
    case maps:get(auth_context, Metadata, undefined) of
        undefined ->
            {error, missing_auth_context};
        AuthContext ->
            case principal_class(AuthContext) of
                {error, _} = Err ->
                    Err;
                {ok, Principal} ->
                    route_surface(Principal, Metadata)
            end
    end;
principal_for_route(NotAMap) ->
    {error, {invalid_route_metadata, NotAMap}}.

principal_class(AuthContext) when is_atom(AuthContext) ->
    case lists:member(AuthContext, principals()) of
        true -> {ok, AuthContext};
        false -> {error, {unknown_auth_context, AuthContext}}
    end;
principal_class(AuthContext) ->
    {error, {unknown_auth_context, AuthContext}}.

route_surface(Principal, Metadata) ->
    case declared_surface(Metadata) of
        {error, _} = Err ->
            Err;
        {ok, Declared} ->
            case infer_surface(Metadata) of
                {error, _} = Err ->
                    Err;
                {ok, Inferred} ->
                    case surface_conflict(Declared, Inferred) of
                        {error, _} = Err ->
                            Err;
                        ok ->
                            Effective = effective_surface(Declared, Inferred),
                            case surface_compatible(Effective, Principal) of
                                false ->
                                    {error, {surface_principal_mismatch, Effective, Principal}};
                                true ->
                                    requirement(Principal, Effective, Metadata)
                            end
                    end
            end
    end.

declared_surface(Metadata) ->
    case maps:get(surface, Metadata, undefined) of
        undefined ->
            {ok, undefined};
        Surface when Surface =:= tenant; Surface =:= platform; Surface =:= personal ->
            {ok, Surface};
        Surface ->
            {error, {unknown_surface, Surface}}
    end.

infer_surface(Metadata) ->
    case maps:get(path, Metadata, undefined) of
        undefined -> {ok, undefined};
        Path -> classify_surface(Path)
    end.

surface_conflict(undefined, _Inferred) ->
    ok;
surface_conflict(_Declared, undefined) ->
    ok;
surface_conflict(Declared, Inferred) ->
    case Declared =:= Inferred of
        true -> ok;
        false -> {error, {surface_mismatch, Declared, Inferred}}
    end.

effective_surface(undefined, Inferred) -> Inferred;
effective_surface(Declared, _Inferred) -> Declared.

%% 面与 principal 的相容矩阵：企业/客服 principal 只在租户面；
%% platform_admin 只在平台面。面未知（无 surface 也无 path）时不做相容判定，
%% 但必需键仍然齐备才放行。
surface_compatible(undefined, _Principal) ->
    true;
surface_compatible(tenant, Principal) ->
    Principal =:= enterprise_owner_admin orelse Principal =:= enterprise_member orelse
        Principal =:= cs_visit orelse Principal =:= cs_shop_key;
surface_compatible(platform, Principal) ->
    Principal =:= platform_admin;
surface_compatible(personal, _Principal) ->
    false.

requirement(Principal, Surface, Metadata) ->
    case required_keys(Principal, Metadata) of
        {error, _} = Err ->
            Err;
        {ok, RequiredFunction, RequiredPermission} ->
            case required_governance(Principal, Metadata) of
                {error, _} = Err ->
                    Err;
                {ok, Governance} ->
                    Base = #{
                        surface => Surface,
                        principal => Principal,
                        required_function => RequiredFunction,
                        required_permission => RequiredPermission,
                        required_governance => Governance,
                        require_contact => Principal =:= cs_visit
                    },
                    case maps:get(jwt_purpose, Metadata, human) of
                        seat -> {ok, Base#{require_seat => true}};
                        _ -> {ok, Base}
                    end
            end
    end.

%% enterprise_member 必须同时给出职能类型与独立权限；
%% platform_admin 必须给出平台权限。缺任一必需键 → fail-closed（不默认放行）。
required_keys(enterprise_member, Metadata) ->
    case maps:get(required_function, Metadata, undefined) of
        undefined -> {error, {missing_required_function, enterprise_member}};
        FunctionKey -> required_permission_keys(enterprise_member, FunctionKey, Metadata)
    end;
required_keys(platform_admin, Metadata) ->
    required_permission_keys(platform_admin, undefined, Metadata);
required_keys(_Principal, Metadata) ->
    {ok, maps:get(required_function, Metadata, undefined),
        maps:get(required_permission, Metadata, undefined)}.

required_permission_keys(_Principal, FunctionKey, #{required_permission := Permission}) when
    is_binary(Permission)
->
    {ok, FunctionKey, Permission};
required_permission_keys(Principal, _FunctionKey, _Metadata) ->
    {error, {missing_required_permission, Principal}}.

%% 治理角色：owner/admin 面固定要求治理角色集合；元数据若要收窄，必须仍在值域内
%% （越域 → fail-closed）。其他 principal 不要求治理角色。
required_governance(enterprise_owner_admin, Metadata) ->
    case maps:get(required_governance, Metadata, undefined) of
        undefined ->
            {ok, eb_auth_permission:governance_roles()};
        Declared when is_list(Declared) ->
            case [R || R <- Declared, not lists:member(R, eb_auth_permission:governance_roles())] of
                [] -> {ok, Declared};
                [Unknown | _] -> {error, {unknown_governance_role, Unknown}}
            end;
        NotAList ->
            {error, {invalid_required_governance, NotAList}}
    end;
required_governance(_Principal, _Metadata) ->
    {ok, []}.
