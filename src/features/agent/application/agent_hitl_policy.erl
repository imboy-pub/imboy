%%% @doc Agent HITL Policy 默认模块（AG31-05；A0 裁决 R3）。
%%%
%%% R3 冻结：side_effect_class=readonly → 不需审批；其他**已知** class →
%%% approval_required；未知 class 或未知 risk_level → deny。默认政策零产品
%%% 语义、纯保守。
%%%
%%% 纯分类真源在 domain `agent_tool_decision:hitl_verdict/2`（铁律 4：判定
%%% 语义归 domain；本模块只是 env seam 的默认绑定壳）。
%%%
%%% 覆盖方式：`imboy` app env `agent_hitl_policy_module`（默认本模块）。
%%% 注入模块实现同形回调：
%%%
%%%   evaluate(RiskLevel, SideEffectClass)
%%%     -> no_approval | approval_required | {deny, ReasonCode}
%%%
%%% 无 I/O、无时钟、无进程状态；注入模块的异常由调用方 fail closed。
-module(agent_hitl_policy).

-moduledoc "Agent HITL Policy 默认模块（AG31-05，A0 裁决 R3）—— 人审策略。".
-export([evaluate/2]).

evaluate(RiskLevel, SideEffectClass) ->
    agent_tool_decision:hitl_verdict(RiskLevel, SideEffectClass).
