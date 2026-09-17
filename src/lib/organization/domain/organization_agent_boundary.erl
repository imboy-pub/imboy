-module(organization_agent_boundary).

%% Agent Organization Boundary 纯决策（domain，ORG-06）。
%%
%% 冻结真源：Agent Organization Contract（FROZEN_ORGANIZATION_SIDE_CONTRACT）
%% + Core Contract C12/C13/C14 + 计划 ORG-06。
%% 本模块是边界裁决的单一决策真源：application 层只做编排与锁序，
%% 不内联任何 allow/deny 语义；infrastructure 只做 SQL。
%%
%% 冻结语义：
%%   * Agent 身份 = user(account_type=1)；Human = user(account_type=0)；
%%   * Agent 最高 role=member（owner/admin 必须 Human——ORG-01 的 126/127
%%     DB invariant 兜底 owner；admin 无 DB guard，由本边界 fail closed）；
%%   * archived / suspended / removed 一律 fail closed（allowed=false）；
%%   * facts 带 fact_version（锚定行 updated_at 的单调投影），
%%     lifecycle command 用 ExpectedVersion 做乐观并发（stale 拒）。
%%
%% 本模块保持纯净：无 elib_pg / 无时间 / 无随机（observed_at 由调用方供给）。

-export([
    fact_version/0,
    ensure_agent_identity/1,
    ensure_human_identity/1,
    organization_allowed/1,
    membership_allowed/3,
    workspace_membership_allowed/2,
    workspace_ownership_allowed/2,
    stale_version/2
]).

%% facts 合同版本（C18 versioned API；形状变更时 bump）。
-define(FACT_VERSION, 1).

%% @doc facts 合同版本。
-spec fact_version() -> pos_integer().
fact_version() ->
    ?FACT_VERSION.

%% @doc Agent 身份校验：仅 user(account_type=1)。
-spec ensure_agent_identity(integer()) -> ok | {error, not_agent}.
ensure_agent_identity(1) ->
    ok;
ensure_agent_identity(AccountType) when is_integer(AccountType) ->
    {error, not_agent};
ensure_agent_identity(_) ->
    {error, not_agent}.

%% @doc Human 身份校验：仅 user(account_type=0)。
%% lifecycle command 的操作人必须是 Human owner/admin（合同 §3）。
-spec ensure_human_identity(integer()) -> ok | {error, not_human}.
ensure_human_identity(0) ->
    ok;
ensure_human_identity(AccountType) when is_integer(AccountType) ->
    {error, not_human};
ensure_human_identity(_) ->
    {error, not_human}.

%% @doc Organization 状态是否放行（C16：archived 禁新 Run / 新授权写）。
-spec organization_allowed(binary()) -> boolean().
organization_allowed(<<"active">>) ->
    true;
organization_allowed(_) ->
    false.

%% @doc Organization membership fact 的放行裁决。
%% 三条必要条件缺一不可：Org active + 成员行 active + role=member
%% （admin/owner 行即使存在也 fail closed——member-only 冻结语义）。
-spec membership_allowed(binary(), binary(), binary()) -> boolean().
membership_allowed(OrgStatus, MemberStatus, Role) ->
    organization_allowed(OrgStatus) andalso
        MemberStatus =:= <<"active">> andalso
        Role =:= <<"member">>.

%% @doc Workspace membership fact 的放行裁决。
%% Workspace archived 或成员行 removed/suspended 均拒绝
%% （workspace_member.status 枚举 active|removed，冻结于 00000076）。
-spec workspace_membership_allowed(binary(), binary()) -> boolean().
workspace_membership_allowed(WorkspaceStatus, WsMemberStatus) ->
    WorkspaceStatus =:= <<"active">> andalso WsMemberStatus =:= <<"active">>.

%% @doc validate workspace ownership 的放行裁决：
%% 同 Org 且目标 Workspace 自身 active。
-spec workspace_ownership_allowed(boolean(), binary()) -> boolean().
workspace_ownership_allowed(SameOrg, WorkspaceStatus) ->
    SameOrg andalso WorkspaceStatus =:= <<"active">>.

%% @doc 乐观并发裁决。
%% ExpectedVersion=undefined → none（不校验）；等于当前 → fresh；否则 stale
%% （行不存在时调用方以 0 为当前版本：带版本的创建预期即 stale）。
-spec stale_version(undefined | integer(), integer()) -> fresh | stale | none.
stale_version(undefined, _CurrentVersion) ->
    none;
stale_version(ExpectedVersion, CurrentVersion) when is_integer(ExpectedVersion) ->
    case ExpectedVersion =:= CurrentVersion of
        true ->
            fresh;
        false ->
            stale
    end.
