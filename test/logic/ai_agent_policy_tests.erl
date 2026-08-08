-module(ai_agent_policy_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

role_policy_overrides_legacy_capabilities_test_() ->
    ?TEST_SIMPLE(fun() ->
        Effective = ai_agent_policy:effective(#{
            <<"role_code">> => <<"support">>,
            <<"system_prompt">> => <<"role prompt">>,
            <<"knowledge_policy">> => #{
                <<"group_reply">> => #{<<"mode">> => <<"off">>}
            },
            <<"capabilities">> => #{<<"group_reply">> => true}
        }),
        ?assertEqual(<<"role">>, maps:get(<<"policy_source">>, Effective)),
        ?assertNot(
            ai_agent_policy:allows(
                Effective#{<<"role_status">> => 1},
                <<"group_reply">>
            )
        )
    end).

legacy_agent_gets_explicit_fallback_source_test_() ->
    ?TEST_SIMPLE(fun() ->
        Effective = ai_agent_policy:effective(#{
            <<"system_prompt">> => <<"legacy prompt">>,
            <<"capabilities">> => #{<<"group_reply">> => true}
        }),
        ?assertEqual(<<"legacy_fallback">>, maps:get(<<"policy_source">>, Effective)),
        ?assert(
            ai_agent_policy:allows(
                #{<<"capabilities">> => #{<<"group_reply">> => true}},
                <<"group_reply">>
            )
        )
    end).
