%%% @doc Native Tool adapter（AG31-06；架构 §12 dispatch 链首切片）。
%%%
%%% 职责：readonly Native Tool 经中央授权咽喉 `agent_tool_authorizer:authorize/3`
%%% 全链（十步 fail-closed + 决策持久化 + 审计）后，查 Native 工具注册表并
%%% 调用 handler，返回 sanitized 结果（只摘要不透传原始载荷）。
%%%
%%% 组合语义（派发包 §T）：
%%%   * 授权在先：authorize/3 deny/approval_required 原样透传，注册表与
%%%     handler 零接触（deny 路径 handler 调用数恒 0）；
%%%   * 注册表经 env seam：`imboy` app env `agent_native_tool_registry_module`
%%%     ——**默认未配置 = fail-closed**（查无此 tool → {deny, unknown_tool}），
%%%     生产注册表由后续集成期注入；注入模块回调 lookup/1：
%%%     `{ok, Handler} | {error, not_found}`，Handler::
%%%     `fun((ResourceContext) -> {ok, RawResult} | {error, term()})`；
%%%   * duplicate effect（同 external_idempotency_key 重试）→ authorize 返回
%%%     {deny, duplicate_effect} → handler 不再被调（at-most-once，恰一执行）；
%%%   * sanitized result：返回值只含 tool_id/effect_id/状态/结果摘要 digest，
%%%     零原始载荷（零 PII/凭据外泄面）。
%%%
%%% 纯编排：无 I/O 之外的进程/表/时钟持有；handler 崩溃 fail closed 向上
%%% 暴露 {error, _}，不改写授权判定。
-module(agent_native_tool_adapter).

-export([invoke/3]).

%% @doc 执行一个 Native Tool 调用：
%% ```
%% invoke(AgentRunContext, ToolDescriptor, ResourceContext)
%%   -> {ok, Sanitized}
%%    | {approval_required, ApprovalContext}
%%    | {deny, ReasonCode}
%%    | {error, term()}   %% handler/注册表运行时故障（授权判定不被改写）
%% '''
invoke(AgentRunContext, ToolDescriptor, ResourceContext) ->
    case agent_tool_authorizer:authorize(AgentRunContext, ToolDescriptor, ResourceContext) of
        {deny, Reason} ->
            {deny, Reason};
        {approval_required, _AC} = Approval ->
            Approval;
        {allow, DecisionContext} ->
            allow_and_invoke(DecisionContext, ToolDescriptor, ResourceContext)
    end.

allow_and_invoke(DecisionContext, ToolDescriptor, ResourceContext) ->
    case lookup_handler(maps:get(tool_id, ToolDescriptor)) of
        {ok, Handler} ->
            invoke_handler(DecisionContext, ToolDescriptor, ResourceContext, Handler);
        {error, not_found} ->
            {deny, unknown_tool};
        {error, Reason} ->
            {deny, {tool_registry_failed, Reason}}
    end.

registry_module() ->
    application:get_env(imboy, agent_native_tool_registry_module, none).

lookup_handler(ToolId) ->
    case registry_module() of
        none ->
            {error, not_found};
        Module ->
            try Module:lookup(ToolId) of
                {ok, Handler} when is_function(Handler, 1) -> {ok, Handler};
                {error, not_found} = E -> E;
                _BadShape -> {error, {registry_bad_shape}}
            catch
                _Class:_Reason -> {error, registry_crashed}
            end
    end.

invoke_handler(DecisionContext, ToolDescriptor, ResourceContext, Handler) ->
    try Handler(ResourceContext) of
        {ok, RawResult} ->
            {ok, sanitize(DecisionContext, ToolDescriptor, RawResult)};
        {error, Reason} ->
            {error, {tool_failed, Reason}};
        _BadShape ->
            {error, {tool_bad_shape}}
    catch
        _Class:_Reason ->
            {error, tool_crashed}
    end.

%% sanitized：结果只留摘要——原始载荷不进返回值/审计面。
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
