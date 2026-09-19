%%% @doc Agent Membership Read Adapter（AG31-01）：ORG facts → `agent_membership_port`
%%% 的 infrastructure 实现（Interface 1 消费侧翻译层）。
%%%
%%% 上游是 TRACK-ORG 交付的 facts 适配器 `organization_agent_facts_app`（ORG-06，
%%% commit 69f9a039 形状快照为本模块唯一事实源——本分支尚无该文件，故**必须动态
%%% 模块调用**，静态调用在本分支编译即失败）。默认绑定 `organization_agent_facts_app`
%%% 即冻结绑定（集成树汇合后解析）；`imboy` app env `agent_org_facts_module` 仅
%%% 测试/运维覆盖用，不写 sys.config.example（保持 diff 最小）。
%%%
%%% ## 消费映射表（ORG HTTP 风格错误 → port 原子，AG31-01 派工 §M）
%%%
%%%   * `{404,_}` → `not_found`；`{403,_}`（主体非 Agent 身份）→ `invalid_agent_role`；
%%%     `{400,_}`（id 非正整数）→ `not_found`（fail closed；本模块入口自行做正整数
%%%     校验，非法入参同映射）；`{500,_}` 与任何 crash/throw/exit/未知返回形状
%%%     → `unavailable`（try/catch 兜底）。
%%%
%%% ## 裁决规则（§6.2 ok 形状锁死 status := active，不消费 ORG 的 allowed 当判定）
%%%
%%%   * `resolve_organization_membership/2`：先 `resolve_organization/1` 过 org 门
%%%     （org 非 active 在 membership 上下文 → `inactive`；`archived` 原子只归
%%%     `resolve_organization_state/1`），再 `resolve_membership/2`：member 行
%%%     status 非 active 或 role 非 `member`（理论上 DB invariant 挡死）→
%%%     `inactive`；ok 时 `version` 透传 membership 行 `fact_version`。
%%%   * `resolve_organization_state/1`：status=active → ok；status=archived →
%%%     `{error, archived}`；未知 status → `unavailable`（fail closed）。
%%%   * `resolve_workspace_membership/3`：先 `validate_workspace_in_org/2`——
%%%     same_org=false → `cross_organization`（且不再查 ws-member 行）；
%%%     same_org=true ∧ workspace_status=active 才继续 `resolve_workspace_membership/2`
%%%     （注意 ORG 侧是二参 `(WsId, AgentId)`）；ws member 行 status 非 active 或
%%%     workspace_status 回读非 active（并发竞态兜底）→ `inactive`；ok 时 `Role`
%%%     为 ws member 行真实 role 透传。
%%%
%%% ## 不缓存（§6.3）
%%%
%%% 每次调用都实时打 facts 模块，unavailable 不用旧值放行——suspend/remove 必须
%%% 即时失效。本模块零进程字典 / 零 ETS / 零持久化状态。
-module(agent_org_membership_adapter).

-behaviour(agent_membership_port).

-export([
    resolve_organization_membership/2,
    resolve_workspace_membership/3,
    resolve_organization_state/1
]).

%% ===================================================================
%% facts 绑定（冻结默认 + env 覆盖，仅测试/运维用）
%% ===================================================================

facts_module() ->
    application:get_env(imboy, agent_org_facts_module, organization_agent_facts_app).

%% ===================================================================
%% agent_membership_port callbacks
%% ===================================================================

resolve_organization_membership(OrgId, AgentId) ->
    case validate_ids([{organization_id, OrgId}, {agent_id, AgentId}]) of
        ok ->
            facts_call(fun(Mod) ->
                case Mod:resolve_organization(OrgId) of
                    {ok, OrgFact} when is_map(OrgFact) ->
                        %% org 门：membership 上下文里 org 非 active 一律 inactive
                        case norm_fact_val(maps:get(status, OrgFact, undefined)) of
                            active -> resolve_member_fact(OrgId, AgentId);
                            _ -> {error, inactive}
                        end;
                    {error, {Code, _Msg}} when is_integer(Code) ->
                        {error, http_error(Code)};
                    _Unknown ->
                        {error, unavailable}
                end
            end);
        {error, _Bad} ->
            {error, not_found}
    end.

resolve_workspace_membership(OrgId, WsId, AgentId) ->
    case validate_ids([{organization_id, OrgId}, {workspace_id, WsId}, {agent_id, AgentId}]) of
        ok ->
            facts_call(fun(Mod) ->
                case Mod:validate_workspace_in_org(OrgId, WsId) of
                    {ok, OwnershipFact} when is_map(OwnershipFact) ->
                        ws_gate(WsId, AgentId, OwnershipFact);
                    {error, {Code, _Msg}} when is_integer(Code) ->
                        {error, http_error(Code)};
                    _Unknown ->
                        {error, unavailable}
                end
            end);
        {error, _Bad} ->
            {error, not_found}
    end.

resolve_organization_state(OrgId) ->
    case validate_ids([{organization_id, OrgId}]) of
        ok ->
            facts_call(fun(Mod) ->
                case Mod:resolve_organization(OrgId) of
                    {ok, OrgFact} when is_map(OrgFact) ->
                        state_verdict(OrgFact);
                    {error, {Code, _Msg}} when is_integer(Code) ->
                        %% state 上下文无主体：{403} 不可能合法出现 → unavailable
                        case Code of
                            404 -> {error, not_found};
                            400 -> {error, not_found};
                            _ -> {error, unavailable}
                        end;
                    _Unknown ->
                        {error, unavailable}
                end
            end);
        {error, _Bad} ->
            {error, not_found}
    end.

%% ===================================================================
%% 内部：member 行 / ws-member 行裁决
%% ===================================================================

resolve_member_fact(OrgId, AgentId) ->
    facts_call(fun(Mod) ->
        case Mod:resolve_membership(OrgId, AgentId) of
            {ok, MemberFact} when is_map(MemberFact) ->
                member_verdict(MemberFact);
            {error, {Code, _Msg}} when is_integer(Code) ->
                {error, http_error(Code)};
            _Unknown ->
                {error, unavailable}
        end
    end).

%% membership 上下文：任何非 active（member 行或 org_status 回读）→ inactive；
%% role 非 member（DB invariant 挡死的理论分支）同 fail closed。
member_verdict(MemberFact) ->
    Status = norm_fact_val(maps:get(status, MemberFact, undefined)),
    OrgStatus = norm_fact_val(maps:get(org_status, MemberFact, undefined)),
    Role = norm_fact_val(maps:get(role, MemberFact, undefined)),
    case {Status, OrgStatus, Role} of
        {active, active, member} ->
            ok_version(MemberFact, fun(Version) ->
                {ok, #{status => active, role => member, version => Version}}
            end);
        _NonActive ->
            {error, inactive}
    end.

ws_gate(WsId, AgentId, OwnershipFact) ->
    case
        {
            maps:get(same_org, OwnershipFact, undefined),
            norm_fact_val(maps:get(workspace_status, OwnershipFact, undefined))
        }
    of
        {false, _} ->
            {error, cross_organization};
        {true, active} ->
            resolve_ws_member_fact(WsId, AgentId);
        {true, _NonActiveWs} ->
            {error, inactive}
    end.

resolve_ws_member_fact(WsId, AgentId) ->
    facts_call(fun(Mod) ->
        %% ORG facts 是二参 (WsId, AgentId)：内部按 WsId 反查 workspace 行
        case Mod:resolve_workspace_membership(WsId, AgentId) of
            {ok, WsMemberFact} when is_map(WsMemberFact) ->
                ws_member_verdict(WsMemberFact);
            {error, {Code, _Msg}} when is_integer(Code) ->
                {error, http_error(Code)};
            _Unknown ->
                {error, unavailable}
        end
    end).

ws_member_verdict(WsMemberFact) ->
    Status = norm_fact_val(maps:get(status, WsMemberFact, undefined)),
    WsStatus = norm_fact_val(maps:get(workspace_status, WsMemberFact, undefined)),
    Role = norm_fact_val(maps:get(role, WsMemberFact, undefined)),
    case {Status, WsStatus} of
        {active, active} when Role =/= undefined ->
            ok_version(WsMemberFact, fun(Version) ->
                {ok, #{status => active, role => Role, version => Version}}
            end);
        _NonActive ->
            {error, inactive}
    end.

%% state 上下文：只有 active 与 archived 是已知值，其余 fail closed 为 unavailable。
state_verdict(OrgFact) ->
    case norm_fact_val(maps:get(status, OrgFact, undefined)) of
        active ->
            ok_version(OrgFact, fun(Version) ->
                {ok, #{status => active, version => Version}}
            end);
        archived ->
            {error, archived};
        _UnknownStatus ->
            {error, unavailable}
    end.

%% fact_version 缺失/非整数 = 未知返回形状 → unavailable（fail closed）。
ok_version(Fact, Build) ->
    case maps:get(fact_version, Fact, undefined) of
        Version when is_integer(Version), Version >= 0 ->
            Build(Version);
        _Bad ->
            {error, unavailable}
    end.

%% facts 状态/角色归一化：ORG facts（organization_agent_facts_app，C18 v1）
%% 经真实 PG 返回 text 列的 binary 形态（<<"active">> 等），其内部 boundary
%% 合同（organization_allowed/1 等）也以 binary 为准；而本模块的裁决与输出
%% 合同是 atom（grant/run command 消费 {ok, #{status => active, ...}}，eunit
%% mock 均为 atom）。IT-10 集成实测（run 20260918T124045Z-6af21b0d）发现两侧
%% 形状漂移：真库上 binary 与 atom 恒不匹配 → membership 恒 inactive、state
%% 恒 unavailable，grant/run 链路在真实部署中全部失效。修法=本模块读取边界
%% 统一归一化：已知枚举 binary → atom；atom 原样透传（兼容既有 mock 合同）；
%% 未知值原样返回（与 atom 比较必落入各 verdict 的 _NonActive/_Unknown 分支，
%% fail closed 语义不变）。
norm_fact_val(<<"active">>) -> active;
norm_fact_val(<<"removed">>) -> removed;
norm_fact_val(<<"archived">>) -> archived;
norm_fact_val(<<"owner">>) -> owner;
norm_fact_val(<<"admin">>) -> admin;
norm_fact_val(<<"member">>) -> member;
norm_fact_val(Value) -> Value.

%% ===================================================================
%% 内部：入口校验 / 错误翻译 / crash 兜底
%% ===================================================================

validate_ids(Pairs) ->
    Bad = [Key || {Key, Value} <- Pairs, not is_pos_int(Value)],
    case Bad of
        [] -> ok;
        _ -> {error, {bad_id, Bad}}
    end.

is_pos_int(Value) when is_integer(Value), Value > 0 -> true;
is_pos_int(_) -> false.

%% ORG HTTP 风格错误码 → port 原子（membership 上下文；{400} 同 {404} fail closed）。
http_error(404) -> not_found;
http_error(403) -> invalid_agent_role;
http_error(400) -> not_found;
http_error(500) -> unavailable;
http_error(_Other) -> unavailable.

%% facts 调用兜底：crash/throw/exit 一律 unavailable（§6.3 fail closed，不缓存）。
facts_call(F) ->
    try
        F(facts_module())
    catch
        _Class:_Reason -> {error, unavailable}
    end.
