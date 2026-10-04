%%% @doc Agent Membership Read Port（AG31-01；架构合同 §6.2 冻结签名，Interface 1）。
%%%
%%% Agent 域对 Organization 成员关系事实的**只读消费层**边界（架构 §6.2/§6.3，
%%% 冻结契约逐字实现，不得改签名）：
%%%
%%%   * Agent 共享 `organization_member`，Agent 域**不建第二套成员表**；
%%%     Organization/Workspace 表只读，本 port 无任何写能力（零副作用回调）。
%%%   * Organization role 只能是 `member`（Agent 最高 member，owner/admin 必须
%%%     Human——ORG-01 invariant 由 DB 兜底）；`resolve_organization_membership/2`
%%%     的 ok 形状把 `role` 锁死为 `member`。
%%%   * Workspace membership 的 `Role` 为 ws member 行真实 role 透传（契约不约束
%%%     取值域，形状由 ORG facts 决定，本 port 不收窄——避免实现层 dialyzer 级联）。
%%%
%%% 不变量（§6.3，全部 MUST）：
%%%
%%%   * **零缓存**：每次调用都实时打 facts 源；unavailable 时 fail closed，
%%%     **禁止用缓存旧值放行**（suspend/remove 必须即时失效）。
%%%   * facts 带 `fact_version` 返回；重复读零副作用（facts 纯 SELECT）。
%%%   * `{ok, ...}` 形状锁死 `status := active`：任何非 active 事实（suspended/
%%%     removed/archived）一律映射为对应错误原子，绝不放行。
%%%
%%% 错误语义（port 原子）：
%%%
%%%   * `not_found`：org / member 行 / ws / ws-member 行不存在（fail closed，
%%%     非法入参同映射，不区分"不存在"与"参数坏"）。
%%%   * `inactive`：member 行或 org/workspace 事实非 active（suspended/removed/
%%%     archived 等）；org role 非 `member`（理论上 DB invariant 挡死）同映射。
%%%     注意 `archived` 原子只出现在 `resolve_organization_state/1` 上下文。
%%%   * `invalid_agent_role`：主体不是 Agent 身份（ORG facts {403} 预检）。
%%%   * `cross_organization`：workspace 不属于该 org。
%%%   * `archived`：org 事实 status = archived（仅 `resolve_organization_state/1`）。
%%%   * `unavailable`：facts 底层不可用（{500} / crash / 未知返回形状）。
-module(agent_membership_port).

-moduledoc "Agent Membership Read Port（AG31-01，Interface 1 冻结签名）—— 成员关系读取扩展点。".
-export_type([
    organization_id/0,
    agent_id/0,
    workspace_id/0,
    version/0,
    ws_role/0
]).

-type organization_id() :: pos_integer().
-type agent_id() :: pos_integer().
-type workspace_id() :: pos_integer().
%% fact_version 透传整数（ORG facts 快照：non_neg_integer()；port 类型不收窄，
%% 数值合法性由实现层入口校验）。
-type version() :: integer().
%% workspace member 真实 role 透传（§6.2 契约 `role := Role` 不约束取值域）。
-type ws_role() :: term().

%% @doc 解析 Agent 在该 Organization 的成员关系（org 门 + member 行双查）。
%%
%% `status := active, role := member` 锁死：org 非 active、member 行
%% suspended/removed、role 非 `member` 一律 fail closed（`inactive`）。
%% 主体非 Agent 身份（facts {403}）→ `invalid_agent_role`。
-callback resolve_organization_membership(
    OrganizationId :: organization_id(), AgentId :: agent_id()
) ->
    {ok, #{status := active, role := member, version := version()}}
    | {error, not_found | inactive | invalid_agent_role | unavailable}.

%% @doc 解析 Agent 在该 Workspace 的成员关系（先验 workspace 归属该 org）。
%%
%% workspace 不属该 org → `cross_organization`（且不得继续查 ws-member 行）；
%% workspace 非 active 或 ws-member 行非 active → `inactive`；
%% ws-member 行缺失 → `not_found`。ok 时 `Role` 为 ws member 行真实 role 透传。
-callback resolve_workspace_membership(
    OrganizationId :: organization_id(), WorkspaceId :: workspace_id(), AgentId :: agent_id()
) ->
    {ok, #{status := active, role := ws_role(), version := version()}}
    | {error, not_found | inactive | cross_organization | unavailable}.

%% @doc 解析 Organization 当前状态（无主体，纯 org 行事实）。
%%
%% `status := active` → ok；`archived` → `{error, archived}`；
%% 未知 status 值或 facts 底层不可用 → `unavailable`（fail closed）。
-callback resolve_organization_state(OrganizationId :: organization_id()) ->
    {ok, #{status := active, version := version()}}
    | {error, archived | not_found | unavailable}.
