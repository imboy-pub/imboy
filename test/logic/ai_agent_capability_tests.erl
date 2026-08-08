-module(ai_agent_capability_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

role_policy_is_source_of_group_capability_test_() ->
    ?TEST_SIMPLE(fun() ->
        Agent = #{
            <<"role_code">> => <<"support">>,
            <<"knowledge_policy">> => #{
                <<"group_reply">> => #{<<"mode">> => <<"mention_only">>},
                <<"proactive">> => #{<<"mode">> => <<"off">>}
            },
            <<"capabilities">> => #{<<"group_reply">> => false}
        },
        ?assert(ai_agent_capability:allows(Agent, <<"group_reply">>)),
        ?assertNot(ai_agent_capability:allows(Agent, <<"proactive">>))
    end).

role_policy_off_cannot_be_overridden_by_agent_capabilities_test_() ->
    ?TEST_SIMPLE(fun() ->
        Agent = #{
            <<"role_code">> => <<"support">>,
            <<"knowledge_policy">> => #{
                <<"group_reply">> => #{<<"mode">> => <<"off">>}
            },
            <<"capabilities">> => #{<<"group_reply">> => true}
        },
        ?assertNot(ai_agent_capability:allows(Agent, <<"group_reply">>))
    end).

disabled_role_blocks_all_capabilities_test_() ->
    ?TEST_SIMPLE(fun() ->
        Agent = #{
            <<"role_code">> => <<"support">>,
            <<"role_status">> => 0,
            <<"knowledge_policy">> => #{
                <<"group_reply">> => #{<<"mode">> => <<"mention_only">>}
            }
        },
        ?assertNot(ai_agent_capability:allows(Agent, <<"group_reply">>))
    end).

legacy_agent_keeps_existing_capability_behavior_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(
            ai_agent_capability:allows(
                #{<<"capabilities">> => #{<<"group_reply">> => true}},
                <<"group_reply">>
            )
        ),
        ?assertNot(
            ai_agent_capability:allows(
                #{<<"capabilities">> => #{<<"group_reply">> => false}},
                <<"group_reply">>
            )
        )
    end).
