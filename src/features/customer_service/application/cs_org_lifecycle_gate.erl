%%% @doc Organization archived → CS 新写稳定拒绝门（ORG-08 / C16 / C08）。
%%%
%%% 依据 `2026-09-16-enterprise-organization-cs-compatibility.md` §4 Adapter-Only
%%% Changes 第 1 条：「Organization archived 时拒绝新 Seat claim 和新 Session」，
%%% 第 2 条「Organization Invitation/Restore 完成后再进入 CS onboarding」——本模块
%%% 是唯一落点，不改 Seat/Session/Identity 的表主键、owner 或生命周期。
%%%
%%% 冻结语义（零重解释）：
%%%   * `organization.status` 是生命周期唯一真源（C16）；本模块消费
%%%     `cs_org_lifecycle_port` 的只读事实（装配实现读 Organization 域既有读
%%%     API，CS 侧无任何 org 表 SQL），逐请求加载、零缓存；
%%%   * archived → `{error, organization_archived}`：**稳定** denial（幂等——
%%%     重复 archive/denial 不修改任何 CS state，会话历史与 Seat 保留可读）；
%%%   * 授权只读放行：fetch/list 类用例不经过本门（C16「允许授权只读」）；
%%%   * restore 后组织回到 active，新写恢复（不在此模块实现，事实翻转即放行）。
%%%
%%% 幂等（ORG-08 IDEMPOTENCY）：本模块零写、零审计副作用，重复裁决同一输入
%%% 得同一结果；门失败不影响 CS 存量数据。
-module(cs_org_lifecycle_gate).

-moduledoc "Organization archived → CS 新写稳定拒绝门（ORG-08 / C16 / C08）。".
-include("generated/imboy_product_features.hrl").

-export([assert_session_writable/2]).

%% @doc 新写（open_session / claim）前的组织生命周期门。
%%
%% `Params` 与 cs_session_app 用例参数同面：`org_lifecycle` 键可注入端口
%% 覆盖（测试用，经 `cs_app_support:port/2`），缺省走 `cs_infra_ports` 装配。
%%
%% 返回：
%%   * `ok` —— 组织 active（或事实不可得时**不**拦截：见下）；
%%   * `{error, organization_archived}` —— 组织已归档，稳定拒绝；
%%   * `{error, not_found}` —— 组织不存在（与 store 同语句租户裁决同向 fail-closed）；
%%   * `{error, term()}` —— 端口未装配 / 事实读取失败，原样上抛（调用方按
%%     既有 5xx 语义处理；不猜「可用即放行」以外的任何解释）。
-spec assert_session_writable(integer(), map()) -> ok | {error, term()}.
assert_session_writable(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    case lifecycle_port(Params) of
        {error, _} = Err ->
            Err;
        {ok, FactsMod} ->
            case FactsMod:status(OrgId) of
                {ok, active} ->
                    ok;
                {ok, archived} ->
                    {error, organization_archived};
                {error, not_found} ->
                    {error, not_found};
                {error, _} = Err ->
                    Err
            end
    end;
assert_session_writable(OrgId, _Params) ->
    {error, {invalid_organization_id, OrgId}}.

%% 端口解析（与 cs_app_support:port/2 同语义；该函数未导出故本地复制）：
%% Params 同键注入（模块原子；undefined 也是 atom，须显式排除）优先，
%% 否则取 cs_infra_ports 装配默认。
lifecycle_port(Params) ->
    case maps:get(org_lifecycle, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined ->
            {ok, Mod};
        _ ->
            default_lifecycle_port()
    end.

%% F-EB10-1 特性裁剪调用门：`cs_infra_ports` 属 customer_service 裁剪集，
%% 未选档整条省略——缺省装配路径必须落在 `-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE)`
%% 活动分支内（同 cs_session_app:dispatch_message/2 的仓内范式）；
%% 未选档显式 fail-closed（与 cs_infra_ports:resolve 的 unimplemented_port
%% 同形），不做静默兜底。
-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).
default_lifecycle_port() ->
    cs_infra_ports:resolve(org_lifecycle).
-else.
default_lifecycle_port() ->
    {error, {unimplemented_port, org_lifecycle}}.
-endif.
