%%% @doc Agent Resource Policy 默认模块（AG31-05；A0 裁决 R2）。
%%%
%%% R2 冻结：**未配置策略 = 恒 deny**。V3.1 首切片没有任何 domain Resource
%%% Policy 语义落地（能力面 staged off，D7 空集目录同哲学），因此本默认模块
%%% 对一切输入返回 `{deny, resource_policy_denied}`——零产品语义、纯保守。
%%%
%%% 覆盖方式：`imboy` app env `agent_resource_policy_module`（默认本模块）。
%%% 测试/未来 domain 策略模块注入实现同形回调：
%%%
%%%   evaluate(AgentRunContext, ToolDescriptor, ResourceContext)
%%%     -> allow | {deny, ReasonCode}
%%%
%%% 纯决策：无 I/O、无时钟、无进程状态；调用方负责 try/catch（本模块不可
%%% 能抛出；注入模块的异常由调用方 fail closed）。
-module(agent_resource_policy).

-moduledoc "Agent Resource Policy 默认模块（AG31-05，A0 裁决 R2）。".
-export([evaluate/3]).

evaluate(_AgentRunContext, _ToolDescriptor, _ResourceContext) ->
    {deny, resource_policy_denied}.
