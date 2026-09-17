%%% @doc MCP Tool adapter（AG31-06；架构 §12 dispatch 链首切片）。
%%%
%%% 职责：readonly MCP Tool 经**双门交集**后执行——
%%%   1. Agent 门：`agent_tool_authorizer:authorize/3` 全链（十步 fail-closed
%%%      + 决策持久化 + 审计）；deny/approval_required 原样透传，MCP 面零接触
%%%      （A16：Agent grant 缺 → Agent 门先拒，MCP dispatch=0）；
%%%   2. MCP 门：消费现有 MCP 治理权威面 `mcp_governance_logic:authorize/2`
%%%      （OwnerUid=agent_id、ToolName=tool_id；enforce 关闭时该面恒 allow
%%%      ——交集语义下 MCP allow 不可能放大 Agent deny，不影响 Agent 判定）；
%%%      MCP deny → handler_count=0（A17：MCP grant 仍必需）；
%%%   3. 执行：经 env seam `agent_mcp_tool_executor_module` ——**默认未配置
%%%      = fail-closed**（{deny, mcp_executor_not_configured}）；真实异步
%%%      executor（barrel_mcp_registry:run_tool/3 spawn 模型）由集成期注入。
%%%      注入模块回调 execute/3：
%%%      `(ToolName, Args, Ctx) -> {ok, RawResult} | {error, term()}`。
%%%
%%% 组合语义：effect dedup 由 authorizer 内建（同 external_idempotency_key
%%% 重试 → {deny, duplicate_effect} → executor 不再被调，恰一执行）；
%%% sanitized 返回（零原始载荷/PII/凭据）；executor 崩溃 fail closed。
-module(agent_mcp_tool_adapter).

-export([invoke/3]).

%% @doc 执行一个 MCP Tool 调用：
%% ```
%% invoke(AgentRunContext, ToolDescriptor, ResourceContext)
%%   -> {ok, Sanitized}
%%    | {approval_required, ApprovalContext}
%%    | {deny, ReasonCode}
%%    | {error, term()}
%% '''
invoke(AgentRunContext, ToolDescriptor, ResourceContext) ->
    case agent_tool_authorizer:authorize(AgentRunContext, ToolDescriptor, ResourceContext) of
        {deny, Reason} ->
            %% A16：Agent 门先行——deny 时 MCP 治理面/executor 零调用。
            {deny, Reason};
        {approval_required, _AC} = Approval ->
            Approval;
        {allow, DecisionContext} ->
            mcp_gate_then_execute(AgentRunContext, DecisionContext, ToolDescriptor, ResourceContext)
    end.

mcp_gate_then_execute(AgentRunContext, DecisionContext, ToolDescriptor, ResourceContext) ->
    OwnerUid = maps:get(agent_id, AgentRunContext),
    ToolName = maps:get(tool_id, ToolDescriptor),
    try mcp_governance_logic:authorize(OwnerUid, ToolName) of
        allow ->
            execute(DecisionContext, ToolDescriptor, ResourceContext);
        {deny, _McpReason} ->
            %% A17：MCP client grant 仍必需——稳定 reason 归并（原始 binary
            %% 治理文案不进 API 面）。
            {deny, mcp_gate_denied};
        _BadShape ->
            {deny, mcp_gate_unavailable}
    catch
        _Class:_Reason ->
            {deny, mcp_gate_unavailable}
    end.

executor_module() ->
    application:get_env(imboy, agent_mcp_tool_executor_module, none).

execute(DecisionContext, ToolDescriptor, ResourceContext) ->
    ToolName = maps:get(tool_id, ToolDescriptor),
    Args = #{resource => ResourceContext},
    Ctx = #{effect_id => maps:get(effect_id, DecisionContext, undefined)},
    case executor_module() of
        none ->
            {deny, mcp_executor_not_configured};
        Module ->
            try Module:execute(ToolName, Args, Ctx) of
                {ok, RawResult} ->
                    {ok, sanitize(DecisionContext, ToolDescriptor, RawResult)};
                {error, Reason} ->
                    {error, {tool_failed, Reason}};
                _BadShape ->
                    {error, tool_bad_shape}
            catch
                _Class:_Reason ->
                    {error, tool_crashed}
            end
    end.

sanitize(DecisionContext, ToolDescriptor, RawResult) ->
    #{
        tool_id => maps:get(tool_id, ToolDescriptor),
        effect_id => maps:get(effect_id, DecisionContext, undefined),
        status => ok,
        result_digest => result_digest(RawResult)
    }.

result_digest(Raw) ->
    Data = io_lib:format("~p", [Raw]),
    binary:encode_hex(erlang:md5(unicode:characters_to_binary(Data))).
