%% 兼容旧调用点；策略实现集中在 ai_agent_policy。
-module(ai_agent_capability).

-export([allows/2]).

allows(Agent, Capability) ->
    ai_agent_policy:allows(Agent, Capability).
