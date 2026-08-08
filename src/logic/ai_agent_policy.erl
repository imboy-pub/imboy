-module(ai_agent_policy).

%% 角色有效策略：发布角色优先，未绑定或角色不可用时保留 legacy fallback。

-export([effective/1, allows/2]).

-include("log.hrl").

-spec effective(map()) -> map().
effective(Agent) when is_map(Agent) ->
    case has_role_policy(Agent) of
        true ->
            #{
                <<"role_code">> => maps:get(<<"role_code">>, Agent),
                <<"system_prompt">> => maps:get(<<"system_prompt">>, Agent, <<>>),
                <<"capabilities">> => maps:get(<<"capabilities">>, Agent, #{}),
                <<"knowledge_policy">> => maps:get(<<"knowledge_policy">>, Agent, #{}),
                <<"policy_source">> => <<"role">>
            };
        false ->
            #{
                <<"system_prompt">> => maps:get(<<"system_prompt">>, Agent, <<>>),
                <<"capabilities">> => maps:get(<<"capabilities">>, Agent, #{}),
                <<"knowledge_policy">> => maps:get(
                    <<"knowledge_policy">>,
                    Agent,
                    #{<<"knowledge">> => #{<<"mode">> => <<"required">>}}
                ),
                <<"policy_source">> => <<"legacy_fallback">>
            }
    end;
effective(_) ->
    #{
        <<"system_prompt">> => <<>>,
        <<"capabilities">> => #{},
        <<"knowledge_policy">> => #{},
        <<"policy_source">> => <<"legacy_fallback">>
    }.

-spec allows(map(), binary()) -> boolean().
allows(Agent, Capability) when is_map(Agent), is_binary(Capability) ->
    Allowed =
        case has_role_policy(Agent) of
            true -> role_allows(Agent, Capability);
            false -> legacy_allows(Agent, Capability)
        end,
    case Allowed of
        true ->
            true;
        false ->
            try
                ?INFO_LOG([
                    ai_agent_capability_denied,
                    #{
                        role_code => maps:get(<<"role_code">>, Agent, <<"legacy_fallback">>),
                        capability => Capability,
                        role_status => maps:get(<<"role_status">>, Agent, 1)
                    }
                ])
            catch
                _:_ -> ok
            end,
            false
    end;
allows(_, _) ->
    false.

has_role_policy(Agent) ->
    maps:is_key(<<"role_code">>, Agent) andalso maps:is_key(<<"knowledge_policy">>, Agent).

role_allows(Agent, <<"knowledge">>) ->
    role_enabled(Agent) andalso
        begin
            Mode = policy_mode(Agent, <<"knowledge">>, <<"off">>),
            Mode =:= <<"on_demand">> orelse Mode =:= <<"required">>
        end;
role_allows(Agent, <<"group_reply">>) ->
    role_enabled(Agent) andalso
        policy_mode(Agent, <<"group_reply">>, <<"off">>) =:= <<"mention_only">>;
role_allows(Agent, <<"proactive">>) ->
    role_enabled(Agent) andalso
        policy_mode(Agent, <<"proactive">>, <<"off">>) =:= <<"welcome_only">>;
role_allows(_Agent, _Capability) ->
    false.

legacy_allows(Agent, Capability) ->
    Capabilities = maps:get(<<"capabilities">>, Agent, #{}),
    case maps:get(Capability, Capabilities, undefined) of
        Value when is_boolean(Value) ->
            Value;
        undefined when Capability =:= <<"group_reply">>; Capability =:= <<"proactive">> ->
            true;
        undefined when Capability =:= <<"knowledge">> ->
            true;
        _ ->
            false
    end.

policy_mode(Agent, Key, Default) ->
    Policy = maps:get(<<"knowledge_policy">>, Agent, #{}),
    Section = maps:get(Key, Policy, #{}),
    maps:get(<<"mode">>, Section, Default).

role_enabled(Agent) ->
    maps:get(<<"role_status">>, Agent, 1) =:= 1.
