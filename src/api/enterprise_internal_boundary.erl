-module(enterprise_internal_boundary).

-moduledoc "HMAC authenticated OA integration with live business attribution checks.".

-export([
    spec/1,
    ids/0,
    frozen_ids/0,
    enforce/4,
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
    {ok, #{kind => none}};
spec(<<"INT-02">>) ->
    {ok, #{kind => org}};
spec(<<"INT-03">>) ->
    {ok, #{kind => org}};
spec(<<"INT-04">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-05">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-06">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-07">>) ->
    {ok, #{kind => org}};
spec(<<"INT-08">>) ->
    {ok, #{kind => org}};
spec(<<"INT-09">>) ->
    {ok, #{kind => org}};
spec(<<"INT-10">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-11">>) ->
    {ok, #{kind => org}};
spec(<<"INT-12">>) ->
    {ok, #{kind => none}};
spec(<<"INT-13">>) ->
    {ok, #{kind => none}};
spec(<<"INT-23">>) ->
    {ok, #{kind => none}};
spec(<<"INT-44">>) ->
    {ok, #{kind => none}};
spec(<<"INT-14">>) ->
    {ok, #{kind => none}};
%% ---- FULL-02 新增路由（待 A0 追加 manifest + Router 注册；见 checkpoint
%%      「A0 需要接线的路由清单」）----
%% INT-15 撤销 external identity 映射（org scoped）
spec(<<"INT-15">>) ->
    {ok, #{kind => org}};
%% INT-16 映射 cursor directory（org scoped，受限分页）
spec(<<"INT-16">>) ->
    {ok, #{kind => org}};
%% INT-17 成员 cursor directory（org scoped；给 workspace_id 时另按 workspace 边界）
spec(<<"INT-17">>) ->
    {ok, #{kind => org}};
spec(<<"INT-18">>) ->
    {ok, #{kind => workspace}};
%% INT-19 群更新（同上）
spec(<<"INT-19">>) ->
    {ok, #{kind => workspace}};
%% INT-20 成员角色（同上）
spec(<<"INT-20">>) ->
    {ok, #{kind => workspace}};
%% INT-21 群归档（同上 + 归属 application 必须为本 app）
spec(<<"INT-21">>) ->
    {ok, #{kind => workspace}};
%% INT-22 附件留存/hold/purge 治理（org scoped）
spec(<<"INT-22">>) ->
    {ok, #{kind => org}};
%% ---- V2.1 新增（plan §6.2：全部只读，A-R audit；kind 语义见 moduledoc）----
%% INT-24 Workspace 列表：行集收窄到覆盖 W 集合（kind=list，handler 义务）
spec(<<"INT-24">>) ->
    {ok, #{kind => list}};
%% INT-25 Workspace 详情（path W 边界）
spec(<<"INT-25">>) ->
    {ok, #{kind => workspace}};
%% INT-26 企业群列表：origin app + 覆盖 W 过滤（kind=list，handler 义务）
spec(<<"INT-26">>) ->
    {ok, #{kind => list}};
%% INT-27 群成员列表（group 所属 W 边界）
spec(<<"INT-27">>) ->
    {ok, #{kind => workspace}};
%% INT-28 项目列表（W 过滤必填——W 来自 query，enforce 用该 W）
spec(<<"INT-28">>) ->
    {ok, #{kind => workspace}};
%% INT-29 项目详情（project 所属 W 边界）
spec(<<"INT-29">>) ->
    {ok, #{kind => workspace}};
%% INT-30 频道列表（scope=workspace AND status=1；W 过滤必填）
spec(<<"INT-30">>) ->
    {ok, #{kind => workspace}};
%% INT-31 频道详情（channel 所属 W 边界；status=1 only）
spec(<<"INT-31">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-32">>) ->
    {ok, #{kind => none}};
spec(<<"INT-33">>) ->
    {ok, #{kind => org}};
spec(<<"INT-34">>) ->
    {ok, #{kind => org}};
spec(<<"INT-35">>) ->
    {ok, #{kind => org}};
spec(<<"INT-36">>) ->
    {ok, #{kind => org}};
spec(<<"INT-37">>) ->
    {ok, #{kind => org}};
spec(<<"INT-38">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-39">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-40">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-41">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-42">>) ->
    {ok, #{kind => workspace}};
spec(<<"INT-43">>) ->
    {ok, #{kind => org}};
%% Source reconciliation filters live source/target grants inside its SQL reads.
spec(<<"INT-45">>) ->
    {ok, #{kind => list}};
spec(<<"INT-46">>) ->
    {ok, #{kind => list}};
spec(<<"INT-47">>) ->
    {ok, #{kind => list}};
spec(<<"INT-48">>) ->
    {ok, #{kind => list}};
spec(<<"INT-49">>) ->
    {ok, #{kind => list}};
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

%% Resource attribution is checked in the business transaction.
-spec enforce(any(), map(), binary(), undefined | integer()) -> ok | {error, atom()}.
enforce(Conn, Ctx, RouteId, WorkspaceId) ->
    case spec(RouteId) of
        {ok, #{kind := none}} ->
            ok;
        {ok, #{kind := list}} ->
            enterprise_internal_access_repo:organization_tx(Conn, Ctx);
        {ok, #{kind := org}} ->
            enterprise_internal_access_repo:organization_tx(Conn, Ctx);
        {ok, #{kind := workspace}} when is_integer(WorkspaceId) ->
            enterprise_internal_access_repo:workspace_tx(
                Conn, Ctx, WorkspaceId, workspace_lock(RouteId)
            );
        _ ->
            {error, security_gate_closed}
    end.

%% @doc 边界错误码 → stable 13 码（本模块内部用 atom，与认证链同款约定；
%% HTTP 适配器 enterprise_internal_middleware:normalize_code/1 归一）。
-spec error_code(atom()) -> binary().
error_code(Code) when is_atom(Code) ->
    atom_to_binary(Code, utf8).

%%%===================================================================
%%% Internal
%%%===================================================================

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
                <<"INT-39">>,
                <<"INT-40">>,
                <<"INT-41">>,
                <<"INT-42">>,
                <<"INT-43">>,
                <<"INT-44">>,
                <<"INT-45">>,
                <<"INT-46">>,
                <<"INT-47">>,
                <<"INT-48">>,
                <<"INT-49">>
            ]
    ].

%% Only channel mutations upgrade the workspace lock after the boundary check.
workspace_lock(Id) when Id =:= <<"INT-40">>; Id =:= <<"INT-41">>; Id =:= <<"INT-42">> -> unlocked;
workspace_lock(_) -> read.
