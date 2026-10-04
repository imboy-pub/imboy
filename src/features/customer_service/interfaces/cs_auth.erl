%%% @doc 客服五类身份的 metadata 认证（CS-02-A01/A04，plan EB-D10/§5.2/§5.3）。
%%%
%%% 五类 `auth_context`（与 `cs_actions` 的授权声明一一对应，值域冻结）：
%%%
%%% | principal               | 凭证                          | 必需条件 |
%%% |-------------------------|-------------------------------|----------|
%%% | `enterprise_owner_admin`| IMBoy JWT（current_uid）      | active member + owner/admin 治理角色 |
%%% | `cs_seat`               | IMBoy JWT（current_uid）      | active member + customer_service 职能 active assignment + seat enabled（A04）|
%%% | `platform_admin`        | Admin session（adm_user_id）  | `customer_service:read|write` 权限 |
%%% | `cs_visit`              | 头 `x-cs-visit-token`         | digest 命中 + 未吊销 + 未过期，只返回 (Org, contact) 作用域 |
%%% | `cs_shop_key`           | 头 `x-cs-shop-key`            | digest 命中 + 未吊销，绑定本 Org |
%%%
%%% 纪律（与 EB-D10 同源，全部 fail-closed）：
%%%
%%%   * **route metadata 是唯一分流依据**：`auth_context` 决定只取哪一类凭证，
%%%     其余类别（哪怕同时在场）一律不采信——这是「无混淆」的机制保证；
%%%   * **凭证不落日志、不进错误项**：digest/secret/token 只做比对与校验；
%%%   * **事实逐请求加载**（`auth_facts` 装配模块由 router 注入，经动态调用——
%%%     铁律 5：跨 Feature 只准引用 facade，装配模块名是路由元数据而非静态引用），
%%%     suspended/removed 下一个请求即失效，不缓存；
%%%   * **坐席门（A04）**：`cs_seat` 在进任何用例**之前**查 seat 行，
%%%     `enabled=false` 即 `{error, seat_disabled}`（suspend 即时拒绝），seat 不存在
%%%     同样拒绝（`seat_not_found`）——suspended seat actor 拿不到任何 HTTP 语义外的
%%%     信息，也不触发任何业务写；
%%%   * **租户证明**：客户端申报的 `organization_id` 与事实/凭证作用域**逐字比对**
%%%     （member 事实的 Org、visit/shop 校验返回的 Org），不等即拒（cross_org /
%%%     not_found）——申报不等于信任。
%%%
%%% 访客/门店凭证校验经 `customer_service_facade:verify_visit_token/verify_shop_key`
%%% （本 feature application，digest 逻辑不出 application 层）。
%%% 本模块是纯决策函数：无 SQL、无进程、无隐式时间源（`now` 可注入，缺省取系统毫秒）。
-module(cs_auth).

-moduledoc "客服五类身份的 metadata 认证（CS-02-A01/A04，plan EB-D10/§5.2/§5.3）。".
-export([
    authorize/3,
    principals/0,
    credential_class/1,
    visit_header/0,
    shop_key_header/0
]).

-type principal() :: atom().
-type auth_context() :: map().
-type metadata() :: map().
-type state() :: map().

%% 访客 / 门店凭证的传输头（credential 面，auth_middleware 对这些路径免 JWT/签名直通，
%% handler 侧 fail-closed 校验；中间件侧经 cs_http:is_credential_surface_path/1 判定）。
-define(VISIT_HEADER, <<"x-cs-visit-token">>).
-define(SHOP_KEY_HEADER, <<"x-cs-shop-key">>).

%% @doc 五类 principal（顺序冻结，便于逐字审计）。
-spec principals() -> [principal()].
principals() ->
    [enterprise_owner_admin, platform_admin, cs_seat, cs_visit, cs_shop_key].

%% @doc principal 要求的凭证类别（互斥；类别不符即 credential_missing/不采信）。
-spec credential_class(principal()) -> imboy_jwt | adm_session | visit_token | shop_key.
credential_class(enterprise_owner_admin) -> imboy_jwt;
credential_class(cs_seat) -> imboy_jwt;
credential_class(platform_admin) -> adm_session;
credential_class(cs_visit) -> visit_token;
credential_class(cs_shop_key) -> shop_key.

-spec visit_header() -> binary().
visit_header() ->
    ?VISIT_HEADER.

-spec shop_key_header() -> binary().
shop_key_header() ->
    ?SHOP_KEY_HEADER.

%% ===================================================================
%% 入口
%% ===================================================================

%% @doc 按路由元数据判定一次客服请求。
%%
%% `State` 含：路由元数据白名单（auth_context/required_*）、装配键 `auth_facts`、
%% 中间件注入的会话键（current_uid/adm_user_id）、以及 handler 解析出的
%% `organization_id`（path 或申报参数）。成功返回授权上下文（只含判定结果与
%% 租户/身份标识，绝不含凭证、digest、secret），失败返回可区分原因。
-spec authorize(metadata(), credential_source(), state()) ->
    {ok, auth_context()} | {error, term()}.
authorize(Metadata, Req, State) ->
    case route_metadata(Metadata) of
        {error, _} = Err ->
            Err;
        {ok, Route} ->
            Principal = maps:get(auth_context, Route),
            OrgId = maps:get(organization_id, State, undefined),
            case credential(Principal, Req, State) of
                {error, _} = Err ->
                    Err;
                {ok, Credential} ->
                    decide(Principal, Route, Credential, OrgId, State)
            end
    end.

-type credential_source() :: term().

%% 元数据白名单（避免把客户端可控值混进判定输入）；缺 auth_context 即 fail-closed。
route_metadata(Metadata) ->
    Keys = [auth_context, surface, required_function, required_permission, required_governance],
    Present = [{K, maps:get(K, Metadata, undefined)} || K <- Keys, maps:is_key(K, Metadata)],
    case proplists:get_value(auth_context, Present) of
        undefined ->
            {error, route_metadata_missing_auth_context};
        AuthContext ->
            case lists:member(AuthContext, principals()) of
                true -> {ok, maps:from_list(Present)};
                false -> {error, {unknown_auth_context, AuthContext}}
            end
    end.

%% ===================================================================
%% 凭证解析（按 principal 只取本类凭证——其余类别不采信）。
%% 会话键（current_uid/adm_user_id）来自中间件注入的 handler State；
%% 访客/门店凭证来自专用传输头（cowboy 请求头）。
%% ===================================================================

credential(Principal, _Req, State) when
    Principal =:= enterprise_owner_admin; Principal =:= cs_seat
->
    case current_uid(State) of
        Uid when is_integer(Uid), Uid > 0 ->
            {ok, #{class => imboy_jwt, user_id => Uid}};
        _ ->
            {error, credential_missing}
    end;
credential(platform_admin, _Req, State) ->
    case maps:get(adm_user_id, State, undefined) of
        Adm when is_integer(Adm), Adm > 0 ->
            {ok, #{class => adm_session, adm_user_id => Adm}};
        _ ->
            {error, credential_missing}
    end;
credential(cs_visit, Req, _State) ->
    header_credential(Req, ?VISIT_HEADER, visit_token);
credential(cs_shop_key, Req, _State) ->
    header_credential(Req, ?SHOP_KEY_HEADER, shop_key).

header_credential(Req, Header, Class) ->
    case header(Req, Header) of
        Raw when is_binary(Raw), Raw =/= <<>> ->
            {ok, #{class => Class, raw => Raw}};
        _ ->
            {error, credential_missing}
    end.

%% ===================================================================
%% 分类判定
%% ===================================================================

decide(Principal, Route, Credential, OrgId, State) ->
    Expected = credential_class(Principal),
    case maps:get(class, Credential, undefined) of
        Expected ->
            decide_in(Principal, Route, Credential, OrgId, State);
        Actual ->
            %% 路由声明的 principal 与凭证类别不符：不混淆、不降级。
            {error, {principal_mismatch, Principal, Actual}}
    end.

%% —— 治理类：active member + governance role ——
decide_in(enterprise_owner_admin, Route, Credential, OrgId, State) ->
    with_member_facts(OrgId, Credential, State, fun(Member, _Facts) ->
        Required = maps:get(required_governance, Route, [<<"owner">>, <<"admin">>]),
        Roles = governance_roles_of(Member),
        case [R || R <- Required, lists:member(R, Roles)] of
            [] ->
                {error, {governance_insufficient, Required}};
            _Matched ->
                {ok, #{
                    auth_context => enterprise_owner_admin,
                    organization_id => OrgId,
                    user_id => maps:get(user_id, Member, undefined),
                    governance_roles => Roles
                }}
        end
    end);
%% —— 坐席类：active member + customer_service assignment + seat enabled（A04）——
decide_in(cs_seat, Route, Credential, OrgId, State) ->
    with_member_facts(OrgId, Credential, State, fun(Member, Facts) ->
        case seat_permission(Route, Facts) of
            {error, _} = Err ->
                Err;
            ok ->
                seat_identity(Route, OrgId, Credential, State, Member, Facts)
        end
    end);
%% —— 平台类：Admin session + customer_service:read|write ——
decide_in(platform_admin, Route, Credential, _OrgId, State) ->
    case facts_module(State) of
        {error, _} = Err ->
            Err;
        {ok, FactsMod} ->
            case
                FactsMod:load_request_facts(#{
                    adm_user_id => maps:get(adm_user_id, Credential)
                })
            of
                {error, _} = Err ->
                    Err;
                {ok, Facts} ->
                    platform_decide(Route, Credential, undefined, Facts)
            end
    end;
%% —— 访客 / 门店类：digest 校验经本 feature facade（org 申报被同语句证明）——
decide_in(cs_visit, _Route, Credential, OrgId, State) ->
    %% digest 未命中（含跨 Org 的 token）是凭证错误（401 语义），不是资源 404：
    %% 在认证层翻译，业务层的 not_found 保持 404（避免枚举仍由同语句保证）。
    case
        customer_service_facade:verify_visit_token(OrgId, #{
            secret => maps:get(raw, Credential), at => now_sec(State)
        })
    of
        {ok, Scope} ->
            case maps:get(organization_id, Scope, undefined) of
                OrgId when is_integer(OrgId) ->
                    {ok, #{
                        auth_context => cs_visit,
                        organization_id => OrgId,
                        contact_id => maps:get(contact_id, Scope, undefined)
                    }};
                _ ->
                    {error, cross_org}
            end;
        {error, not_found} ->
            %% digest 未命中（含跨 Org token）＝凭证错误（401 语义），不是资源 404。
            {error, credential_invalid};
        {error, _} = Err ->
            Err
    end;
decide_in(cs_shop_key, _Route, Credential, OrgId, _State) ->
    case
        customer_service_facade:verify_shop_key(OrgId, #{
            secret => maps:get(raw, Credential)
        })
    of
        {ok, Key} ->
            case maps:get(organization_id, Key, undefined) of
                OrgId when is_integer(OrgId) ->
                    {ok, #{auth_context => cs_shop_key, organization_id => OrgId}};
                _ ->
                    {error, cross_org}
            end;
        {error, not_found} ->
            %% digest 未命中（含跨 Org key）＝凭证错误（401 语义）。
            {error, credential_invalid};
        {error, _} = Err ->
            Err
    end.

%% 平台判定：事实的 adm_user_id 必须与凭证一致（防串身），再查权限。
%% F-SEC-05（closure 安全面）：`required_permission` 缺省一律 fail-closed——
%% 与 EB 侧 `eb_auth_app` 的无默认 `maps:get/2` 同口径；未来新增路由漏登记
%% 权限时是 403 拒绝而非静默放行。
platform_decide(Route, Credential, _OrgId, Facts) ->
    CredentialAdm = maps:get(adm_user_id, Credential, undefined),
    case maps:get(adm_user_id, Facts, undefined) of
        Adm when is_integer(Adm), Adm =:= CredentialAdm ->
            case maps:get(required_permission, Route, undefined) of
                undefined ->
                    {error, {missing_required_permission, platform_admin}};
                Required ->
                    Granted = [P || P <- maps:get(permissions, Facts, []), is_binary(P)],
                    case lists:member(Required, Granted) of
                        true ->
                            {ok, #{auth_context => platform_admin, adm_user_id => Adm}};
                        false ->
                            {error, {permission_missing, Required}}
                    end
            end;
        _ ->
            {error, platform_identity_mismatch}
    end.

%% 坐席独立权限（职能不替代权限；facts 的 permissions 已由 active-member 门守卫）。
%% F-SEC-05：缺省 fail-closed，同 platform_decide。
seat_permission(Route, Facts) ->
    case maps:get(required_permission, Route, undefined) of
        undefined ->
            {error, {missing_required_permission, seat}};
        Required ->
            Granted = [P || P <- maps:get(permissions, Facts, []), is_binary(P)],
            case lists:member(Required, Granted) of
                true -> ok;
                false -> {error, {permission_missing, Required}}
            end
    end.

%% 从 active assignments 挑出本 Org 的 required_function 坐席身份（恰一条），
%% 再过 **seat enabled 门**（A04：suspended seat actor 即时拒绝，且发生在任何
%% 业务用例之前）。
seat_identity(Route, OrgId, Credential, State, _Member, Facts) ->
    RequiredFunction = maps:get(required_function, Route, <<"customer_service">>),
    UserId = maps:get(user_id, Credential),
    Assignments = [
        A
     || A <- maps:get(assignments, Facts, []),
        maps:get(user_id, A, undefined) =:= UserId,
        maps:get(organization_id, A, undefined) =:= OrgId,
        maps:get(status, A, undefined) =:= active,
        maps:get(function_key, A, undefined) =:= RequiredFunction
    ],
    case Assignments of
        [Assignment] ->
            seat_enabled_gate(OrgId, maps:get(business_identity_id, Assignment), State);
        [] ->
            {error, identity_assignment_missing};
        _Multiple ->
            {error, {multiple_active_assignment, RequiredFunction}}
    end.

seat_enabled_gate(OrgId, BusinessIdentityId, _State) when is_integer(BusinessIdentityId) ->
    case customer_service_facade:fetch_seat(OrgId, #{business_identity_id => BusinessIdentityId}) of
        {error, not_found} ->
            {error, {seat_not_found, BusinessIdentityId}};
        {error, _} = Err ->
            Err;
        {ok, Seat} ->
            case maps:get(enabled, Seat, false) of
                false ->
                    {error, seat_disabled};
                true ->
                    {ok, #{
                        auth_context => cs_seat,
                        organization_id => OrgId,
                        business_identity_id => BusinessIdentityId
                    }}
            end
    end;
seat_enabled_gate(_OrgId, _BadIdentity, _State) ->
    {error, identity_assignment_missing}.

%% ===================================================================
%% 事实加载（逐请求、零缓存、fail-closed）
%% ===================================================================

with_member_facts(OrgId, Credential, State, Next) when is_integer(OrgId) ->
    case facts_module(State) of
        {error, _} = Err ->
            Err;
        {ok, FactsMod} ->
            case
                FactsMod:load_request_facts(#{
                    organization_id => OrgId,
                    user_id => maps:get(user_id, Credential)
                })
            of
                {error, _} = Err ->
                    Err;
                {ok, Facts} ->
                    case maps:get(member, Facts, undefined) of
                        #{status := active} = Member ->
                            Next(Member, Facts);
                        #{status := Status} ->
                            {error, {member_not_active, Status}};
                        _Missing ->
                            {error, member_not_found}
                    end
            end
    end;
with_member_facts(_OrgId, _Credential, _State, _Next) ->
    {error, missing_org_id}.

%% 装配模块名来自 route metadata（router 注入），不是静态跨单元引用（铁律 5）。
facts_module(State) ->
    case maps:get(auth_facts, State, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> {error, auth_assembly_missing}
    end.

governance_roles_of(Member) ->
    [R || R <- maps:get(governance_roles, Member, []), is_binary(R)].

%% 时钟：可注入（测试），缺省系统秒。只用于 visit token 过期判定。
%% 量纲必须与 visit token 行的 expires_at/revoked_at（epoch 秒，widget 面签发
%% 同源）一致——毫秒基准会让 `Now >= ExpiresAt` 恒真，param org 面（
%% /cs/sessions/:id/{messages,rating} 等 cs_visit 路由）一律 401 token_expired
%% （DF-4 同族终局，2026-09-30 真机走查实证；此前 6c405e69 条目层补丁不
%% 达意——缺陷在本鉴权层的比较基准，不在动作表 opts）。
now_sec(State) ->
    case maps:get(now, State, undefined) of
        N when is_integer(N) -> N;
        _ -> os:system_time(second)
    end.

%% —— cowboy 请求读取的薄封装（便于纯测试注入 proplist/map 形态的伪请求）——

current_uid(State) when is_map(State) ->
    maps:get(current_uid, State, 0).

header(Req, Name) when is_map(Req) ->
    case maps:get(headers, Req, undefined) of
        Headers when is_map(Headers) -> maps:get(Name, Headers, undefined);
        _ -> undefined
    end;
header(_Req, _Name) ->
    undefined.
