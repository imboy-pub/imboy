-module(adm_enterprise_filter).
-moduledoc "Admin 企业菜单入口的服务端强制资源过滤 / server-side resource filter。".
%%%
% adm_enterprise_filter —— Admin 企业菜单入口的服务端强制资源过滤
%
% 合同（docs/plans/2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.1.md
% §13.1 / §13.3 Reuse Matrix）：
%   * query 参数（preset / organization_id / workspace_id）只表达 UI 状态；
%     服务端必须在 adm 读路径重验——不接受仅靠 query string 或前端过滤伪装。
%   * 企业入口（preset=enterprise）恒定强制 scope='workspace'（groups/channels），
%     personal 资源在企业入口零可见（负例测试断言）；channel 另强制 status=1。
%   * 服务端从不读取客户端提供的 scope 参数——scope 谓词只作为服务端常量
%     注入，不存在"信任客户端 scope"的路径。
%   * Organization 过滤由服务端把 organization_id 解析为 workspace id 集合
%     （workspace.organization_id 为真源），解析失败 fail-closed（零可见）；
%     同时携带 organization_id + workspace_id 时服务端复核归属，跨 O 不可见。
%
% 本模块是纯谓词构造器（除 org_workspace_ids/1 的真源解析），不复制
% Human/Internal 域的 handler 逻辑，只在 adm 读路径叠加谓词。
%%%

-export([
    scope_params_from_req/1,
    organization_id_from_req/1,
    org_workspace_ids/1,
    group_where/3,
    channel_where/3
]).

-include_lib("eunit/include/eunit.hrl").

-define(ENTERPRISE_PRESET, <<"enterprise">>).
-define(SCOPE_WORKSPACE, <<"workspace">>).
-define(ENTERPRISE_CHANNEL_STATUS, 1).
%% TSID 恒为正整数，id = 0 是保证零命中的参数化假谓词
%% （不使用 workspace_id = 0：personal 行 workspace_id 为 NULL，
%%   NULL = 0 不命中，但 id = 0 对两列语义都无歧义且必空）。
-define(NONE_PREDICATE_ID, 0).

-type scope_params() :: #{
    preset := binary(),
    organization_id := non_neg_integer(),
    workspace_id := non_neg_integer()
}.
-type org_workspace_ids() :: not_requested | {ok, [integer()]}.

%% ===================================================================
%% 参数解析（只表达 UI 状态；谓词由本模块强制重验）
%% ===================================================================

%% @doc 从 query 解析企业入口三参数；非法/负值一律归零（永不放宽过滤）。
-spec scope_params_from_req(cowboy_req:req()) -> scope_params().
scope_params_from_req(Req) ->
    {ok, Preset} = elib_param:binary(preset, Req, <<>>),
    {ok, OrgId} = elib_param:int(organization_id, Req, 0),
    {ok, WsId} = elib_param:int(workspace_id, Req, 0),
    #{
        preset => Preset,
        organization_id => normalize_non_negative(OrgId),
        workspace_id => normalize_non_negative(WsId)
    }.

%% @doc 工作区/项目列表的 Organization 服务端过滤参数（无 preset 语义）。
-spec organization_id_from_req(cowboy_req:req()) -> non_neg_integer().
organization_id_from_req(Req) ->
    {ok, OrgId} = elib_param:int(organization_id, Req, 0),
    normalize_non_negative(OrgId).

%% ===================================================================
%% Organization → workspace id 真源解析
%% ===================================================================

%% @doc 把 organization_id 解析为 workspace id 集合（workspace.organization_id 真源）。
%% 未携带 organization_id → not_requested；携带则恒返回 {ok, Ids}——
%% DB 查询失败也归为 {ok, []}（fail-closed：企业维度零可见，而非全量兜底）。
-spec org_workspace_ids(scope_params()) -> org_workspace_ids().
org_workspace_ids(#{organization_id := OrgId}) when OrgId > 0 ->
    case workspace_ds:admin_workspace_ids_by_organization(OrgId) of
        {ok, Ids} when is_list(Ids) ->
            {ok, Ids};
        _ ->
            {ok, []}
    end;
org_workspace_ids(_) ->
    not_requested.

%% ===================================================================
%% 资源谓词（groups / channels）
%% ===================================================================

%% @doc 企业群列表谓词：enterprise preset 强制 scope='workspace'
%% （personal 零可见），再叠加 O/W 过滤。
-spec group_where(map(), scope_params(), org_workspace_ids()) -> map().
group_where(Base, #{preset := Preset} = ScopeParams, OrgWs) ->
    Where = maybe_force_scope(Base, Preset),
    apply_workspace_predicates(Where, ScopeParams, OrgWs).

%% @doc 企业频道列表谓词：enterprise preset 强制 scope='workspace' + status=1
%% （均覆盖任何 UI 状态参数），再叠加 O/W 过滤。
-spec channel_where(map(), scope_params(), org_workspace_ids()) -> map().
channel_where(Base, #{preset := Preset} = ScopeParams, OrgWs) ->
    Where0 = maybe_force_scope(Base, Preset),
    Where1 =
        case is_enterprise(Preset) of
            true -> Where0#{status => ?ENTERPRISE_CHANNEL_STATUS};
            false -> Where0
        end,
    apply_workspace_predicates(Where1, ScopeParams, OrgWs).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec is_enterprise(binary()) -> boolean().
is_enterprise(?ENTERPRISE_PRESET) -> true;
is_enterprise(_) -> false.

%% scope 谓词只在此处作为服务端常量注入；客户端无论传什么 scope 都不影响。
-spec maybe_force_scope(map(), binary()) -> map().
maybe_force_scope(Base, Preset) ->
    case is_enterprise(Preset) of
        true -> Base#{scope => ?SCOPE_WORKSPACE};
        false -> Base
    end.

%% O/W 谓词：organization_id 由真源解析为 id 集合后以 IN 下推；
%% 与 workspace_id 同时出现时服务端复核归属（跨 O 不可见）。
%% 解析为空集 / 归属不符 → id = 0 假谓词（参数化、必零命中）。
-spec apply_workspace_predicates(map(), scope_params(), org_workspace_ids()) -> map().
apply_workspace_predicates(Where, #{organization_id := OrgId, workspace_id := WsId}, OrgWs) when
    OrgId > 0, WsId > 0
->
    case OrgWs of
        {ok, Ids} ->
            case lists:member(WsId, Ids) of
                true -> Where#{workspace_id => WsId};
                false -> none_predicate(Where)
            end;
        not_requested ->
            %% 防御分支：org>0 却未解析（理论不可达），fail-closed
            none_predicate(Where)
    end;
apply_workspace_predicates(Where, #{organization_id := OrgId}, OrgWs) when OrgId > 0 ->
    case OrgWs of
        {ok, []} -> none_predicate(Where);
        {ok, Ids} -> Where#{workspace_id => {in, Ids}};
        not_requested -> none_predicate(Where)
    end;
apply_workspace_predicates(Where, #{workspace_id := WsId}, _OrgWs) when WsId > 0 ->
    Where#{workspace_id => WsId};
apply_workspace_predicates(Where, _ScopeParams, _OrgWs) ->
    Where.

-spec none_predicate(map()) -> map().
none_predicate(Where) ->
    Where#{id => {op, <<"=">>, ?NONE_PREDICATE_ID}}.

-spec normalize_non_negative(integer()) -> non_neg_integer().
normalize_non_negative(N) when is_integer(N), N > 0 -> N;
normalize_non_negative(_) -> 0.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

enterprise_forces_workspace_scope_on_groups_test() ->
    Where = group_where(
        #{}, #{preset => <<"enterprise">>, organization_id => 0, workspace_id => 0}, not_requested
    ),
    ?assertEqual(#{scope => <<"workspace">>}, Where).

non_enterprise_keeps_global_governance_semantics_test() ->
    Where = group_where(
        #{status => 1}, #{preset => <<>>, organization_id => 0, workspace_id => 0}, not_requested
    ),
    ?assertEqual(#{status => 1}, Where),
    %% 运营中心入口即使客户端伪造 scope 参数也不会被读取（本模块无该入口）
    ?assertNot(maps:is_key(scope, Where)).

enterprise_group_with_org_resolves_workspace_in_test() ->
    Params = #{preset => <<"enterprise">>, organization_id => 8001, workspace_id => 0},
    Where = group_where(#{}, Params, {ok, [7001, 7002]}),
    ?assertEqual(#{scope => <<"workspace">>, workspace_id => {in, [7001, 7002]}}, Where).

enterprise_group_org_resolves_empty_leaks_nothing_test() ->
    Params = #{preset => <<"enterprise">>, organization_id => 8001, workspace_id => 0},
    Where = group_where(#{}, Params, {ok, []}),
    %% 空组织 → id = 0 假谓词：personal 与 workspace 资源均零命中
    ?assertEqual(#{scope => <<"workspace">>, id => {op, <<"=">>, 0}}, Where).

workspace_not_in_org_is_invisible_test() ->
    Params = #{preset => <<"enterprise">>, organization_id => 8001, workspace_id => 7003},
    Where = group_where(#{}, Params, {ok, [7001]}),
    ?assertEqual(#{scope => <<"workspace">>, id => {op, <<"=">>, 0}}, Where).

workspace_in_org_visible_test() ->
    Params = #{preset => <<"enterprise">>, organization_id => 8001, workspace_id => 7001},
    Where = group_where(#{}, Params, {ok, [7001, 7002]}),
    ?assertEqual(#{scope => <<"workspace">>, workspace_id => 7001}, Where).

workspace_only_filter_without_org_test() ->
    Params = #{preset => <<>>, organization_id => 0, workspace_id => 7001},
    Where = group_where(#{}, Params, not_requested),
    ?assertEqual(#{workspace_id => 7001}, Where).

enterprise_channel_forces_scope_and_active_status_test() ->
    Params = #{preset => <<"enterprise">>, organization_id => 0, workspace_id => 0},
    %% 即使基础谓词带 status=0（UI 状态），企业入口强制 status=1
    Where = channel_where(#{status => 0}, Params, not_requested),
    ?assertEqual(#{scope => <<"workspace">>, status => 1}, Where).

non_enterprise_channel_keeps_status_filter_test() ->
    Params = #{preset => <<>>, organization_id => 0, workspace_id => 0},
    Where = channel_where(#{status => 0}, Params, not_requested),
    ?assertEqual(#{status => 0}, Where).

keyword_or_clause_composes_with_forced_predicates_test() ->
    Base = #{<<"__or">> => [#{title => {op, <<"LIKE">>, <<"%ops%">>}}], status => 1},
    Params = #{preset => <<"enterprise">>, organization_id => 8001, workspace_id => 0},
    Where = group_where(Base, Params, {ok, [7001]}),
    ?assertEqual(
        #{
            <<"__or">> => [#{title => {op, <<"LIKE">>, <<"%ops%">>}}],
            status => 1,
            scope => <<"workspace">>,
            workspace_id => {in, [7001]}
        },
        Where
    ).

normalize_non_negative_test() ->
    ?assertEqual(0, normalize_non_negative(-1)),
    ?assertEqual(0, normalize_non_negative(0)),
    ?assertEqual(42, normalize_non_negative(42)).

-endif.
