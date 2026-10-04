%% 兼容旧调用点；策略实现集中在 ai_agent_policy。
-module(ai_agent_capability).

-moduledoc "AI agent 能力查询兼容层 —— 旧调用点入口，策略实现集中在 ai_agent_policy。".
-export([allows/2]).

allows(Agent, Capability) ->
    ai_agent_policy:allows(Agent, Capability).
