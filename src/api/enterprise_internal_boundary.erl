-module(enterprise_internal_boundary).

%%%
% enterprise_internal_boundary 是 internal 面的**资源边界接线表**
% （FULL-02）。FULL-01 交付了授权求值面
% （enterprise_application_grant_logic:require_workspace_tx/4 / require_org_tx/3）
% 但**未接线到 API 路径**；本模块是那一步的唯一接线点：把每条路由的
% manifest `grant` 语义（org scoped / workspace scoped / application self）
% 编译成「用哪个边界判定」，handler 在**同一事务内**、执行业务前调用
% enforce/4（或 enforce_dynamic/5）。
%
%% 与冻结真源的关系：
%   * `spec/1` 的 route id 集合与 src/api/enterprise_internal_routes.erl 的
%     冻结表逐条对齐（真库套件断言「冻结表每条路由都有边界 spec」）；
%   * grant 语义按 control/internal-api-manifest.yaml 的 routes[].grant 映射：
%       none-self / application self            -> none
%       org scoped                              -> org
%       workspace scoped (+ group in workspace) -> workspace
%     self 类（INT-01/12/13/14）没有 org/workspace 资源边界——它们的边界是
%     「自身 context」或「code 绑定」，由 repo 的 org/app 复合条件承担。
%
%% 未受管应用（零 Grant 行）不再有兼容旁路（V2.1 §5.2 / F-09 / D-06）：
%% 生效 scope 恒为 allowed_scopes ∩ 生效 Grant scopes——零 Grant 即空集，
%% scope gate 与本模块的边界判定都拒绝（insufficient_scope），不存在
%% 「回退 allowed_scopes 且 Boundary no-op」的路径。受管应用逐请求真库读、
%% 撤权/降权下一请求即失败（fail-closed；读取失败/上下文形态非法 →
%% security_gate_closed）。
%%
%% kind 语义（V2.1 扩到 4 值）：
%%   none      —— application self / code 绑定（INT-01/12/13/14/23），无
%%                org/workspace 资源边界，仅 scope 门；
%%   org       —— org 级资源：需同一生效 Grant 覆盖 scope + Org 全域；
%%   workspace —— workspace 级资源（含 W 过滤必填的列表，如 INT-28/30）：
%%                需同一生效 Grant 覆盖 scope + 目标 W（W 来自 path 绑定、
%%                资源行所属 W 或必填 query 过滤参数）；
%%   list      —— 覆盖 W 集合列表（INT-24/26）：enforce/4 只复验 scope 在
%%                生效集（ctx 内判定，无额外 DB 读）；**行级收窄是 handler
%%                义务**——列表 SQL 必须把行集限制为「覆盖该 scope 的生效
%%                Grant 所覆盖的 W 集合」（org 全域 Grant = Org 内全部 W）。
%%                A2 的列表 repo 查询必须显式实现该谓词，不得整表回读后
%%                内存过滤放行。
%
%% 未登记 route id **fail-closed**（security_gate_closed）：新增路由若忘了登记
%% 边界，宁可拒绝也不放行（本模块是「登记即受边界治理」的单一入口）。
%%%

-export([
    spec/1,
    ids/0,
    frozen_ids/0,
    enforce/4,
    enforce_dynamic/5,
    required_scope_for_sender_mode/1,
    error_code/1
]).

%% FULL-02 阶段新增（已由 A0 接线进冻结表/Router/manifest/契约）
-define(NEW_IDS, [
    <<"INT-15">>,
    <<"INT-16">>,
    <<"INT-17">>,
    <<"INT-18">>,
    <<"INT-19">>,
    <<"INT-20">>,
    <<"INT-21">>,
    <<"INT-22">>
]).

%% FULL-03 阶段新增（A0 接线；与 NEW_IDS 分开只为保留阶段来源）
-define(FULL03_IDS, [
    <<"INT-23">>
]).

%% V2.1 阶段新增（plan §6.1/§6.2 冻结 INT-24..31 资源只读面；handler 由
%% A2 实现、A0 接线 Router——本表只登记边界规格）
-define(V21_IDS, [
    <<"INT-24">>,
    <<"INT-25">>,
    <<"INT-26">>,
    <<"INT-27">>,
    <<"INT-28">>,
    <<"INT-29">>,
    <<"INT-30">>,
    <<"INT-31">>
]).

%% v1.1.1 追加（INT-32 测试投递；webhook application 自属面）
-define(V111_IDS, [
    <<"INT-32">>
]).

%%%===================================================================
%%% 边界规格
%%%===================================================================

%% @doc 冻结路由（manifest INT-01..INT-14，GZ 期生效）的边界规格。
-spec spec(binary()) -> {ok, map()} | error.
spec(<<"INT-01">>) ->
    {ok, #{kind => none, scope => <<"application:read">>}};
spec(<<"INT-02">>) ->
    {ok, #{kind => org, scope => <<"identities:write">>}};
spec(<<"INT-03">>) ->
    {ok, #{kind => org, scope => <<"identities:read">>}};
spec(<<"INT-04">>) ->
    {ok, #{kind => workspace, scope => <<"groups:write">>}};
spec(<<"INT-05">>) ->
    {ok, #{kind => workspace, scope => <<"groups:write">>}};
spec(<<"INT-06">>) ->
    {ok, #{kind => workspace, scope => <<"groups:write">>}};
spec(<<"INT-07">>) ->
    {ok, #{kind => org, scope => <<"files:write">>}};
spec(<<"INT-08">>) ->
    {ok, #{kind => org, scope => <<"files:write">>}};
spec(<<"INT-09">>) ->
    {ok, #{kind => org, scope => dynamic}};
spec(<<"INT-10">>) ->
    {ok, #{kind => workspace, scope => dynamic}};
spec(<<"INT-11">>) ->
    {ok, #{kind => org, scope => <<"friend_requests:create">>}};
spec(<<"INT-12">>) ->
    {ok, #{kind => none, scope => <<"webhooks:manage">>}};
spec(<<"INT-13">>) ->
    {ok, #{kind => none, scope => <<"webhooks:manage">>}};
%% INT-23 投递列表 + 健康度摘要（只读；与 INT-12/13 同为 application 自属面，
%% 无 org/workspace 资源边界，故 kind=none，仅 scope 门）
spec(<<"INT-23">>) ->
    {ok, #{kind => none, scope => <<"webhooks:manage">>}};
spec(<<"INT-14">>) ->
    {ok, #{kind => none, scope => <<"sso:exchange">>}};
%% ---- FULL-02 新增路由（待 A0 追加 manifest + Router 注册；见 checkpoint
%%      「A0 需要接线的路由清单」）----
%% INT-15 撤销 external identity 映射（org scoped）
spec(<<"INT-15">>) ->
    {ok, #{kind => org, scope => <<"identities:write">>}};
%% INT-16 映射 cursor directory（org scoped，受限分页）
spec(<<"INT-16">>) ->
    {ok, #{kind => org, scope => <<"identities:read">>}};
%% INT-17 成员 cursor directory（org scoped；给 workspace_id 时另按 workspace 边界）
spec(<<"INT-17">>) ->
    {ok, #{kind => org, scope => <<"identities:read">>}};
%% INT-18 群详情（workspace scoped + group in workspace；V2.1 scope 修正：
%% 读操作降为 groups:read，与路由表/plan §6.2 逐字一致）
spec(<<"INT-18">>) ->
    {ok, #{kind => workspace, scope => <<"groups:read">>}};
%% INT-19 群更新（同上）
spec(<<"INT-19">>) ->
    {ok, #{kind => workspace, scope => <<"groups:write">>}};
%% INT-20 成员角色（同上）
spec(<<"INT-20">>) ->
    {ok, #{kind => workspace, scope => <<"groups:write">>}};
%% INT-21 群归档（同上 + 归属 application 必须为本 app）
spec(<<"INT-21">>) ->
    {ok, #{kind => workspace, scope => <<"groups:write">>}};
%% INT-22 附件留存/hold/purge 治理（org scoped）
spec(<<"INT-22">>) ->
    {ok, #{kind => org, scope => <<"files:write">>}};
%% ---- V2.1 新增（plan §6.2：全部只读，A-R audit；kind 语义见 moduledoc）----
%% INT-24 Workspace 列表：行集收窄到覆盖 W 集合（kind=list，handler 义务）
spec(<<"INT-24">>) ->
    {ok, #{kind => list, scope => <<"workspaces:read">>}};
%% INT-25 Workspace 详情（path W 边界）
spec(<<"INT-25">>) ->
    {ok, #{kind => workspace, scope => <<"workspaces:read">>}};
%% INT-26 企业群列表：origin app + 覆盖 W 过滤（kind=list，handler 义务）
spec(<<"INT-26">>) ->
    {ok, #{kind => list, scope => <<"groups:read">>}};
%% INT-27 群成员列表（group 所属 W 边界）
spec(<<"INT-27">>) ->
    {ok, #{kind => workspace, scope => <<"groups:read">>}};
%% INT-28 项目列表（W 过滤必填——W 来自 query，enforce 用该 W）
spec(<<"INT-28">>) ->
    {ok, #{kind => workspace, scope => <<"projects:read">>}};
%% INT-29 项目详情（project 所属 W 边界）
spec(<<"INT-29">>) ->
    {ok, #{kind => workspace, scope => <<"projects:read">>}};
%% INT-30 频道列表（scope=workspace AND status=1；W 过滤必填）
spec(<<"INT-30">>) ->
    {ok, #{kind => workspace, scope => <<"channels:read">>}};
%% INT-31 频道详情（channel 所属 W 边界；status=1 only）
spec(<<"INT-31">>) ->
    {ok, #{kind => workspace, scope => <<"channels:read">>}};
%% INT-32 测试投递（v1.1.1 追加；与 INT-12/13/23 同为 application 自属面，
%% 无 org/workspace 资源边界，仅 scope 门）
spec(<<"INT-32">>) ->
    {ok, #{kind => none, scope => <<"webhooks:manage">>}};
spec(<<"INT-33">>) ->
    {ok, #{kind => org, scope => <<"customer_service:read">>}};
spec(<<"INT-34">>) ->
    {ok, #{kind => org, scope => <<"customer_service:read">>}};
spec(<<"INT-35">>) ->
    {ok, #{kind => org, scope => <<"customer_service:write">>}};
spec(<<"INT-36">>) ->
    {ok, #{kind => org, scope => <<"customer_service:write">>}};
spec(<<"INT-37">>) ->
    {ok, #{kind => org, scope => <<"workspaces:write">>}};
spec(<<"INT-38">>) ->
    {ok, #{kind => workspace, scope => <<"workspaces:write">>}};
spec(<<"INT-39">>) ->
    {ok, #{kind => workspace, scope => <<"workspaces:write">>}};
spec(_RouteId) ->
    error.

%% @doc 已登记边界的 route id 全集（冻结 + 新增）。
-spec ids() -> [binary()].
ids() ->
    [maps:get(id, R) || R <- route_specs()].

%% @doc 冻结表内的 route id 全集。
%% FULL-02 集成时 A0 已把 INT-15..22 登记进 enterprise_internal_routes:routes/0
%% 与 imboy_router（并逐条进 manifest 与契约），故「新增 id」不再是「未生效」——
%% 本函数与 ids/0 等价，保留函数名以避免调用侧改动；?NEW_IDS 仍保留用于标注
%% 来源阶段（FULL-02）。
-spec frozen_ids() -> [binary()].
frozen_ids() ->
    ids().

%%%===================================================================
%%% 边界判定
%%%===================================================================

%% @doc 静态 scope 路由的边界判定（handler 在业务事务内、执行业务前调用）。
%% WorkspaceId 仅 workspace 类路由需要（org/list 类忽略；workspace 类路由
%% 缺 WorkspaceId 上下文 → fail-closed）。
-spec enforce(any(), map(), binary(), undefined | integer()) ->
    ok | {error, atom()}.
enforce(Conn, Ctx, RouteId, WorkspaceId) ->
    case spec(RouteId) of
        {ok, #{kind := none}} ->
            ok;
        {ok, #{kind := list, scope := Scope}} when is_binary(Scope) ->
            %% 覆盖 W 集合列表（INT-24/26）：scope 必须在生效集（ctx 内判定，
            %% 无额外 DB 读——中间件 scope gate 已判过，此处是边界层的
            %% 纵深复验）；行级「覆盖 W 集合」收窄是 handler 的列表 SQL 义务。
            case lists:member(Scope, effective_scopes_of(Ctx)) of
                true -> ok;
                false -> {error, insufficient_scope}
            end;
        {ok, #{kind := Kind, scope := Scope}} when is_binary(Scope) ->
            enforce_kind(Conn, Ctx, Kind, WorkspaceId, Scope);
        {ok, #{kind := _Kind, scope := dynamic}} ->
            %% 动态 scope 路由必须走 enforce_dynamic/5（scope 由 sender_mode 决定）；
            %% 走静态入口属接线错误 → fail-closed。
            {error, security_gate_closed};
        error ->
            {error, security_gate_closed}
    end.

%% @doc 动态 scope 路由（INT-09/10）的边界判定：RequiredScope 是本次请求按
%% sender_mode 实际要求的 scope（见 required_scope_for_sender_mode/1）。
-spec enforce_dynamic(any(), map(), binary(), undefined | integer(), binary()) ->
    ok | {error, atom()}.
enforce_dynamic(Conn, Ctx, RouteId, WorkspaceId, RequiredScope) ->
    case spec(RouteId) of
        {ok, #{kind := Kind, scope := dynamic}} when is_binary(RequiredScope) ->
            enforce_kind(Conn, Ctx, Kind, WorkspaceId, RequiredScope);
        _ ->
            {error, security_gate_closed}
    end.

%% @doc sender_mode → 本次请求要求的固定 scope（manifest scope_rules 的唯一实现；
%% handler 的边界判定与 enterprise_message_logic 的 scope gate 都读这里，
%% 避免两处各写一份而漂移）。
-spec required_scope_for_sender_mode(term()) -> {ok, binary()} | error.
required_scope_for_sender_mode(<<"human">>) ->
    {ok, <<"messages:send_as_human">>};
required_scope_for_sender_mode(<<"application">>) ->
    {ok, <<"messages:send">>};
required_scope_for_sender_mode(_) ->
    error.

%% @doc 边界错误码 → stable 13 码（本模块内部用 atom，与认证链同款约定；
%% HTTP 适配器 enterprise_internal_middleware:normalize_code/1 归一）。
-spec error_code(atom()) -> binary().
error_code(Code) when is_atom(Code) ->
    atom_to_binary(Code, utf8).

%%%===================================================================
%%% Internal
%%%===================================================================

-spec enforce_kind(any(), map(), org | workspace, undefined | integer(), binary()) ->
    ok | {error, atom()}.
enforce_kind(Conn, Ctx, org, _WorkspaceId, Scope) ->
    enterprise_application_grant_logic:require_org_tx(Conn, Ctx, Scope);
enforce_kind(Conn, Ctx, workspace, WorkspaceId, Scope) when is_integer(WorkspaceId) ->
    enterprise_application_grant_logic:require_workspace_tx(Conn, Ctx, WorkspaceId, Scope);
enforce_kind(_Conn, _Ctx, workspace, _WorkspaceId, _Scope) ->
    %% workspace 类路由缺 workspace 上下文（接线错误/绑定缺失）→ fail-closed。
    {error, security_gate_closed}.

%% @doc 登记表的 id → spec 展开（ids/0 与真库对齐断言的真源）。
-spec route_specs() -> [map()].
route_specs() ->
    [
        #{id => Id, spec => Spec}
     || Id <- candidate_ids(),
        {ok, Spec} <- [spec(Id)]
    ].

-spec candidate_ids() -> [binary()].
candidate_ids() ->
    [
        <<"INT-01">>,
        <<"INT-02">>,
        <<"INT-03">>,
        <<"INT-04">>,
        <<"INT-05">>,
        <<"INT-06">>,
        <<"INT-07">>,
        <<"INT-08">>,
        <<"INT-09">>,
        <<"INT-10">>,
        <<"INT-11">>,
        <<"INT-12">>,
        <<"INT-13">>,
        <<"INT-14">>
        | ?NEW_IDS ++ ?FULL03_IDS ++ ?V21_IDS ++ ?V111_IDS ++
            [
                <<"INT-33">>,
                <<"INT-34">>,
                <<"INT-35">>,
                <<"INT-36">>,
                <<"INT-37">>,
                <<"INT-38">>,
                <<"INT-39">>
            ]
    ].

%% ctx 的生效 scope（认证链产物；零 Grant 应用恒为空集 → list 类路由拒绝）。
-spec effective_scopes_of(map()) -> [binary()].
effective_scopes_of(Ctx) when is_map(Ctx) ->
    case maps:get(granted_scopes, Ctx, undefined) of
        Scopes when is_list(Scopes) -> [S || S <- Scopes, is_binary(S)];
        _ -> []
    end.
