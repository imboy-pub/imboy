-module(ai_agent_tool_loop_tests).
-include_lib("eunit/include/eunit.hrl").

applicable_is_fail_closed_test() ->
    Agent = #{<<"tools">> => [<<"get_contacts">>]},
    ?assertEqual(false, ai_agent_tool_loop:applicable(Agent, imboy_llm_openai)).

run_is_disabled_test() ->
    ?assertEqual(
        {error, disabled_v1},
        ai_agent_tool_loop:run(42, [], #{tools => [<<"get_contacts">>]}, fake_provider, #{})
    ).
