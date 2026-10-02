%%% @doc 企业授权用例：**每次请求**同时校验 active member + active identity
%%% assignment + 独立 permission，并按 route metadata 决定的 principal 类别分流。
%%%
%%% 依据：plan v4.1 EB-04、EB-D02/EB-D03/EB-D09/EB-D10/EB-D11、§2.1 #4、§5.4。
%%%
%%% 判定纪律（全部 fail-closed）：
%%%
%%%   1. **route metadata 决定 principal**（`eb_auth_principal:principal_for_route/1`）。
%%%      元数据缺失 / 自相矛盾 / 面与 principal 不相容 → 立即拒绝，**不加载任何事实**
%%%      （零副作用）。
%%%   2. **凭证类别必须匹配 principal**。普通 IMBoy JWT 到不了 `platform_admin`，
%%%      Admin session 也冒充不了 `enterprise_member`（`principal_mismatch`，
%%%      同样不加载事实）。
%%%   3. **逐请求加载事实**（`eb_auth_port` 只读扩展点或注入的加载器）。JWT 自报的
%%%      `member_status` / `role` / `permissions` **一律不采信**，因此 suspended /
%%%      removed 在**下一个请求**即失效，不等 token 过期，也不跨请求缓存。
%%%   4. **租户作用域显式**：事实里的 `organization_id` 必须与路由解析出的目标 Org
%%%      一致（`cross_org`）；assignment 必须属于同一 Org 且 assignee 就是请求者
%%%      （否则 `cross_identity` / `identity_assignment_missing`）。
%%%   5. **三概念分离**（`eb_auth_permission`）：职能类型、动作权限、治理角色分别
%%%      判定，`function_key` 不替代 permission，也不自动获得 governance role。
%%%   6. **坐席门（CS-BE-01C / GAP-1）**：`enterprise_member` 选中的 assignment
%%%      `function_key = customer_service` 时，**逐请求**经 `customer_service_facade`
%%%      （铁律 5：跨单元只经 facade）读该坐席 seat 行——`enabled=false` 即
%%%      `{error, seat_disabled}`（suspend 即时拒绝，错误码与 CS 面
%%%      `cs_auth:seat_enabled_gate/3` 逐字一致）；seat 行不存在（从未开通坐席）
%%%      维持既有授权行为（放行至后续 asset ACL / 用例门）；其他取数错误
%%%      fail-closed 原样拒绝。`sales` 职能不属坐席域，门不触发、零 seat 查询。
%%%
%%% 本模块是**纯决策函数**：无 I/O、无进程、无隐式时间源（`now` 由请求注入）；
%%% 事实读取经 `eb_auth_port`（只读契约，无任何写 callback），坐席门经
%%% `customer_service_facade:fetch_seat/2`（跨单元只读）。拒绝路径不写
%%% 任何东西、不改动注入事实、重复调用结果逐字相同。
%%% 本卡不新建第二套 RBAC：治理角色与平台权限的权威来源仍是既有机制，本模块只消费
%%% 经扩展点加载的事实。
-module(eb_auth_app).

-export([
    authorize/2,
    authorize_via_port/3,
    principal_for_route/1
]).

-type request() :: map().
-type auth_context() :: map().
-type denial() :: {error, term()}.

-export_type([request/0, auth_context/0, denial/0]).

%% ===================================================================
%% 入口
%% ===================================================================

%% @doc 判定一次企业请求。成功返回授权上下文（只含判定结果与租户/身份标识，
%% 绝不含凭证、digest 或任何密文），失败返回**可区分**的拒绝原因。
-spec authorize(map(), request()) -> {ok, auth_context()} | denial().
authorize(RouteMetadata, Request) when is_map(Request) ->
    case principal_for_route(RouteMetadata) of
        {error, _} = Err ->
            Err;
        {ok, Requirement} ->
            enforce_credential(Requirement, Request)
    end;
authorize(_RouteMetadata, Request) ->
    {error, {invalid_request, Request}}.

%% @doc route metadata → 授权需求（对外收敛，供 handler / router 复用）。
-spec principal_for_route(map()) -> {ok, eb_auth_principal:requirement()} | {error, term()}.
principal_for_route(RouteMetadata) ->
    eb_auth_principal:principal_for_route(RouteMetadata).

%% @doc 用装配好的**只读**事实扩展点执行一次判定。
%%
%% 每次调用都现读一次事实（`fun/0` 在 `authorize/2` 内被调用一次），不做任何跨请求
%% 缓存——这是 suspended 立即生效的机制保证。
-spec authorize_via_port(eb_auth_port:port_module(), map(), request()) ->
    {ok, auth_context()} | denial().
authorize_via_port(PortModule, RouteMetadata, Request) when is_atom(PortModule), is_map(Request) ->
    authorize(RouteMetadata, Request#{
        facts => {load, fun() -> PortModule:load_request_facts(Request) end}
    });
authorize_via_port(PortModule, _RouteMetadata, Request) ->
    {error, {invalid_port, PortModule, Request}}.

%% ===================================================================
%% 凭证类别
%% ===================================================================

enforce_credential(Requirement, Request) ->
    Principal = maps:get(principal, Requirement),
    Expected = eb_auth_principal:credential_class(Principal),
    Credential = maps:get(credential, Request, undefined),
    case credential_class(Credential) of
        undefined ->
            {error, credential_missing};
        Expected ->
            dispatch(Principal, Requirement, Request, Credential);
        Actual ->
            {error, {principal_mismatch, Principal, Actual}}
    end.

credential_class(Credential) when is_map(Credential) ->
    maps:get(class, Credential, undefined);
credential_class(_Credential) ->
    undefined.

dispatch(enterprise_member, Requirement, Request, Credential) ->
    enterprise_member(Requirement, Request, Credential);
dispatch(enterprise_owner_admin, Requirement, Request, Credential) ->
    enterprise_owner_admin(Requirement, Request, Credential);
dispatch(platform_admin, Requirement, Request, Credential) ->
    platform_admin(Requirement, Request, Credential);
dispatch(cs_visit, Requirement, Request, Credential) ->
    cs_visit(Requirement, Request, Credential);
dispatch(cs_shop_key, Requirement, Request, Credential) ->
    cs_shop_key(Requirement, Request, Credential).

%% ===================================================================
%% 成员类：active member + active assignment + function/permission
%% ===================================================================

enterprise_member(Requirement, Request, Credential) ->
    with_member_facts(Request, fun(Facts, Member) ->
        UserId = credential_user_id(Credential),
        OrgId = maps:get(organization_id, Request, undefined),
        RequiredFunction = maps:get(required_function, Requirement),
        case active_assignments(Facts, UserId, OrgId) of
            {error, _} = Err ->
                Err;
            {ok, Active} ->
                FunctionKeys = [maps:get(function_key, A, undefined) || A <- Active],
                %% 三概念分离：职能类型由 eb_auth_permission 判定（不是权限、不是治理角色）
                case eb_auth_permission:function_satisfied(RequiredFunction, FunctionKeys) of
                    {error, _} = Err ->
                        Err;
                    ok ->
                        %% 身份归属 hint 由 facts 实现按请求资源（会话）投影
                        %% 进授权事实（见 eb_pg_auth_facts:maybe_add_identity_hint）；
                        %% 纯逻辑注入同样经 facts 键传入。
                        Hint = maps:get(resource_identity_hint, Facts, undefined),
                        case select_assignment(Active, RequiredFunction, Hint) of
                            {error, _} = Err ->
                                Err;
                            {ok, Assignment} ->
                                member_permission(Requirement, Request, Facts, Member, Assignment)
                        end
                end
        end
    end).

member_permission(Requirement, Request, Facts, Member, Assignment) ->
    RequiredPermission = maps:get(required_permission, Requirement),
    case eb_auth_permission:permission_satisfied(RequiredPermission, permissions_of(Facts)) of
        {error, _} = Err ->
            Err;
        ok ->
            %% CS-BE-01C（GAP-1）：权限判定通过后、授权成功返回前过坐席门——
            %% 只有「职能、身份、权限全部就绪」的 customer_service 请求才读一次
            %% seat 事实（permission 不足的普通拒绝不触发 seat 查询，零副作用）。
            OrgId = maps:get(organization_id, Request, undefined),
            case
                seat_enabled_gate(
                    OrgId,
                    maps:get(function_key, Assignment, undefined),
                    maps:get(business_identity_id, Assignment, undefined),
                    maps:get(require_seat, Requirement, false)
                )
            of
                {error, _} = Err2 ->
                    Err2;
                ok ->
                    {ok, #{
                        auth_context => enterprise_member,
                        organization_id => maps:get(organization_id, Request, undefined),
                        user_id => maps:get(user_id, Assignment, undefined),
                        business_identity_id => maps:get(
                            business_identity_id, Assignment, undefined
                        ),
                        function_key => maps:get(function_key, Assignment, undefined),
                        permissions => permissions_of(Facts),
                        governance_roles => governance_roles_of(Member)
                    }}
            end
    end.

%% ===================================================================
%% 坐席门（CS-BE-01C / GAP-1，2026-09-25）
%% ===================================================================

%% @doc customer_service 职能的 enterprise_member 坐席门。
%%
%% CS-INT-01 GAP-1：CS 面（`cs_seat` principal）suspend 坐席后 fail-closed，
%% 但 eb 面（enterprise assets/messages）此前只查 active assignment、不查 seat，
%% suspended 坐席仍可读写——本门闭合该缺口。语义与错误码对齐 CS 面
%% `cs_auth:seat_enabled_gate/3`：
%%
%%   * seat 行存在且 `enabled = false` → `{error, seat_disabled}`（逐请求现读，
%%     suspend 在下一个请求即失效，不缓存、不等 token 过期）；
%%   * seat 行不存在（`{error, not_found}`，从未开通坐席）→ 维持 eb 面既有
%%     授权行为（放行至后续 asset ACL / 用例门；`eb_tenant_handler_tests` 的
%%     cs_seat_not_assignee 场景钉住该行为，本门不得提前拦截改标签）；
%%   * 其他取数错误 → fail-closed 原样拒绝（不静默放行）；
%%   * CS 职能但选中 assignment 的 identity 标识畸形/缺失 → 无法证明 seat
%%     状态，fail-closed（`identity_assignment_missing`，与 CS 面兜底同标签）；
%%   * `sales` 职能不属坐席域（seat 是客服坐席的域实体）→ 门不触发。
%%
%% 读取经 `customer_service_facade:fetch_seat/2`（铁律 5：单元间只经 facade；
%% 只读）。查询带目标 `OrgId`（租户作用域显式，铁律 6）。
seat_enabled_gate(OrgId, <<"customer_service">>, IdentityId, RequireSeat) when
    is_integer(OrgId), is_integer(IdentityId)
->
    case customer_service_facade:fetch_seat(OrgId, #{business_identity_id => IdentityId}) of
        {ok, Seat} ->
            case maps:get(enabled, Seat, false) of
                true -> ok;
                false -> {error, seat_disabled}
            end;
        {error, not_found} when RequireSeat ->
            {error, {seat_not_found, IdentityId}};
        {error, not_found} ->
            ok;
        {error, _Reason} = Err ->
            Err
    end;
seat_enabled_gate(_OrgId, <<"customer_service">>, _MalformedIdentity, _RequireSeat) ->
    {error, identity_assignment_missing};
seat_enabled_gate(_OrgId, _NonSeatDomainFunction, _IdentityId, _RequireSeat) ->
    ok.

%% ===================================================================
%% 治理类：active member + governance role（不因业务身份自动获得）
%% ===================================================================

enterprise_owner_admin(Requirement, Request, _Credential) ->
    with_member_facts(Request, fun(Facts, Member) ->
        Required = maps:get(required_governance, Requirement),
        case eb_auth_permission:governance_satisfied(Required, governance_roles_of(Member)) of
            {error, _} = Err ->
                Err;
            ok ->
                %% FND-2（RULING-2026-09-15 §五）：治理上下文的 actor user_id 取自
                %% **已验证的 member facts**（与 JWT 同源加载），不信任请求正文；
                %% 下游 handler 的审计 actor 链（eb_tenant_handler:actor_user_id/2）
                %% 从本上下文取值，请求正文的 actor_user_id 一律不采信。
                {ok, #{
                    auth_context => enterprise_owner_admin,
                    organization_id => maps:get(organization_id, Request, undefined),
                    user_id => maps:get(user_id, Member, undefined),
                    role => maps:get(role, Member, undefined),
                    governance_roles => governance_roles_of(Member),
                    permissions => permissions_of(Facts)
                }}
        end
    end).

%% 成员事实的公共前段：逐请求加载 → 租户一致 → member active。
with_member_facts(Request, Next) ->
    case load_facts(Request) of
        {error, _} = Err ->
            Err;
        {ok, Facts} ->
            case check_org_scope(Facts, Request) of
                {error, _} = Err ->
                    Err;
                ok ->
                    case active_member(Facts) of
                        {error, _} = Err -> Err;
                        {ok, Member} -> Next(Facts, Member)
                    end
            end
    end.

%% ===================================================================
%% 平台类：Admin session + `*:read|write` 权限（不依赖 Org 成员关系）
%% ===================================================================

platform_admin(Requirement, Request, Credential) ->
    case load_facts(Request) of
        {error, _} = Err ->
            Err;
        {ok, Facts} ->
            case adm_identity_matches(Facts, Credential) of
                {error, _} = Err ->
                    Err;
                ok ->
                    RequiredPermission = maps:get(required_permission, Requirement),
                    case
                        eb_auth_permission:permission_satisfied(
                            RequiredPermission, permissions_of(Facts)
                        )
                    of
                        {error, _} = Err ->
                            Err;
                        ok ->
                            {ok, #{
                                auth_context => platform_admin,
                                adm_user_id => maps:get(adm_user_id, Facts, undefined),
                                permissions => permissions_of(Facts)
                            }}
                    end
            end
    end.

adm_identity_matches(Facts, Credential) ->
    case {maps:get(adm_user_id, Facts, undefined), maps:get(adm_user_id, Credential, undefined)} of
        {Id, Id} when is_integer(Id) -> ok;
        _Mismatch -> {error, platform_identity_mismatch}
    end.

%% ===================================================================
%% 访客 / 店铺类：digest + 未过期 + 未吊销 + 绑定同一 Org(/contact)
%% ===================================================================

cs_visit(Requirement, Request, Credential) ->
    with_credential_facts(Request, fun(Facts) ->
        case check_org_scope(Facts, Request) of
            {error, _} = Err ->
                Err;
            ok ->
                case
                    digest_ok(
                        maps:get(digest, Facts, undefined),
                        maps:get(digest, Credential, undefined),
                        visit_token_digest_mismatch
                    )
                of
                    {error, _} = Err ->
                        Err;
                    ok ->
                        case maps:get(revoked, Facts, true) of
                            true ->
                                {error, visit_token_revoked};
                            false ->
                                visit_expiry(Requirement, Request, Facts)
                        end
                end
        end
    end).

visit_expiry(Requirement, Request, Facts) ->
    Now = maps:get(now, Request, undefined),
    ExpiresAt = maps:get(expires_at, Facts, undefined),
    case {is_integer(Now), ExpiresAt} of
        {true, ExpiresAt} when is_integer(ExpiresAt) ->
            case ExpiresAt > Now of
                true -> visit_contact(Requirement, Request, Facts);
                false -> {error, visit_token_expired}
            end;
        _MissingClock ->
            %% 注入时钟缺失 → 无法证明未过期 → fail-closed
            {error, visit_token_expired}
    end.

visit_contact(Requirement, Request, Facts) ->
    case maps:get(require_contact, Requirement, false) of
        false ->
            {ok, visitor_context(cs_visit, Request, Facts)};
        true ->
            ContactId = maps:get(contact_id, Request, undefined),
            case {is_integer(ContactId), maps:get(contact_id, Facts, undefined)} of
                {true, ContactId} -> {ok, visitor_context(cs_visit, Request, Facts)};
                _Mismatch -> {error, cross_contact}
            end
    end.

cs_shop_key(_Requirement, Request, Credential) ->
    with_credential_facts(Request, fun(Facts) ->
        case check_org_scope(Facts, Request) of
            {error, _} = Err ->
                Err;
            ok ->
                case
                    digest_ok(
                        maps:get(digest, Facts, undefined),
                        maps:get(digest, Credential, undefined),
                        shop_key_digest_mismatch
                    )
                of
                    {error, _} = Err ->
                        Err;
                    ok ->
                        case maps:get(revoked, Facts, true) of
                            true -> {error, shop_key_revoked};
                            false -> {ok, visitor_context(cs_shop_key, Request, Facts)}
                        end
                end
        end
    end).

visitor_context(AuthContext, Request, Facts) ->
    #{
        auth_context => AuthContext,
        organization_id => maps:get(organization_id, Request, undefined),
        contact_id => maps:get(contact_id, Facts, undefined)
    }.

with_credential_facts(Request, Next) ->
    case load_facts(Request) of
        {error, _} = Err -> Err;
        {ok, Facts} -> Next(Facts)
    end.

%% ===================================================================
%% 事实加载与不变量
%% ===================================================================

%% @doc 逐请求加载事实。`{load, Fun}`（或直接传 `Fun/0`）每次调用现读一次；
%% 注入的 map 亦按请求级使用（不做任何模块级缓存）。
load_facts(#{facts := {load, Fun}}) when is_function(Fun, 0) ->
    Fun();
load_facts(#{facts := Fun}) when is_function(Fun, 0) ->
    Fun();
load_facts(#{facts := Facts}) when is_map(Facts) ->
    {ok, Facts};
load_facts(_Request) ->
    {error, facts_unavailable}.

%% @doc 租户作用域守卫：事实归属 Org 必须与路由解析出的目标 Org 一致。
%% 缺失任一侧同样 fail-closed（不得退化成「无租户条件」）。
check_org_scope(Facts, Request) ->
    Target = maps:get(organization_id, Request, undefined),
    case maps:get(organization_id, Facts, undefined) of
        Target when is_integer(Target) -> ok;
        _Mismatch -> {error, cross_org}
    end.

active_member(Facts) ->
    case maps:get(member, Facts, undefined) of
        #{status := active} = Member ->
            {ok, Member};
        #{status := Status} ->
            {error, {member_not_active, Status}};
        _Missing ->
            {error, member_not_found}
    end.

%% @doc active assignment 解析：先按 assignee 过滤，再查跨 Org identity（换租户的
%% assignment 一律 `cross_identity`，fail-closed），最后只保留 status=active。
%%
%% 同一 (Org, user) 允许同时持有 `sales` 与 `customer_service` 两条 active
%% assignment（§EB-D02 基数冻结）；由 `select_assignment/2` 按路由要求的职能挑选。
active_assignments(Facts, UserId, OrgId) ->
    Assignments = list_of(maps:get(assignments, Facts, [])),
    Owned = [A || A <- Assignments, maps:get(user_id, A, undefined) =:= UserId],
    case [A || A <- Owned, maps:get(organization_id, A, undefined) =/= OrgId] of
        [_ | _] ->
            {error, cross_identity};
        [] ->
            case [A || A <- Owned, maps:get(status, A, undefined) =:= active] of
                [] -> {error, identity_assignment_missing};
                Active -> {ok, Active}
            end
    end.

%% @doc 在 active assignment 中挑出路由要求的那一条职能身份。
%% `RequiredFunction` 为单个 `function_key()` 或职能白名单（命中其一）；
%% 白名单内出现多条 active（同用户同时持有两类职能且都被路由接纳）→
%% fail-closed（`multiple_active_assignment`），不静默取第一条。
%%
%% 身份归属确定性规则（EB-01 / A01.36 方案 a，2026-09-20）：仅当歧义发生
%% 且请求携带 `resource_identity_hint`（请求资源——如会话——的经办
%% `business_identity_id`，由接口层在构造 Request 时预取）时，才在歧义集合中
%% 过滤 `business_identity_id =:= Hint`：<b>恰一条命中 → 以该身份执行（翻转）；
%% 零条或多条命中 → 维持歧义拒绝</b>。fail-closed 强度不变：无 hint、hint 落空、
%% hint 仍歧义一律拒绝；白名单、身份基数不变量、拒绝原因族均不改动。
select_assignment(Active, RequiredFunction, Hint) ->
    ReqList = required_function_list(RequiredFunction),
    Matching = [
        A
     || A <- Active,
        lists:member(maps:get(function_key, A, undefined), ReqList)
    ],
    case Matching of
        [Assignment] ->
            {ok, Assignment};
        [] ->
            {error, identity_assignment_missing};
        _Multiple when Hint =:= undefined ->
            {error, {multiple_active_assignment, RequiredFunction}};
        _Multiple ->
            case
                [
                    A
                 || A <- Matching,
                    maps:get(business_identity_id, A, undefined) =:= Hint
                ]
            of
                [Assignment] -> {ok, Assignment};
                _NotExactlyOne -> {error, {multiple_active_assignment, RequiredFunction}}
            end
    end.

%% 路由声明的职能归一为白名单列表：单个 binary 包装为单元素列表；
%% 列表原样（空列表/非列表一律空集 → fail-closed 挑不出 identity）。
%% 联合分析把输入域收窄为 `binary() | undefined` 后判 is_list 子句恒假
%% （select_assignment/3 引入 /3 arity 后触发）；运行时白名单列表是真实
%% 输入，属 dialyzer 误报——局部压制，不动 baseline 棘轮。
-dialyzer({nowarn_function, required_function_list/1}).
required_function_list(Required) when is_binary(Required) -> [Required];
required_function_list(Required) when is_list(Required) -> Required;
required_function_list(_Other) -> [].

%% digest 比对使用 OTP 内置 `crypto:hash_equals/2`（等长常量时间比较），
%% 不自造比较算法；长度不等直接判不等（OTP 该函数对长度不等会抛 badarg，
%% 长度本身非密，可先判）。任一侧缺失/非二进制 → fail-closed。
%% 错误项只含原因原子，绝不回显 digest。
digest_ok(Expected, Presented, MismatchReason) when is_binary(Expected), is_binary(Presented) ->
    case byte_size(Expected) =:= byte_size(Presented) of
        false ->
            {error, MismatchReason};
        true ->
            case crypto:hash_equals(Expected, Presented) of
                true -> ok;
                false -> {error, MismatchReason}
            end
    end;
digest_ok(_Expected, _Presented, MismatchReason) ->
    {error, MismatchReason}.

credential_user_id(#{user_id := UserId}) -> UserId;
credential_user_id(_Credential) -> undefined.

permissions_of(Facts) ->
    PermissionList = list_of(maps:get(permissions, Facts, [])),
    [P || P <- PermissionList, is_binary(P)].

governance_roles_of(Member) ->
    RoleList = list_of(maps:get(governance_roles, Member, [])),
    [R || R <- RoleList, is_binary(R)].

list_of(List) when is_list(List) -> List;
list_of(_NotAList) -> [].
