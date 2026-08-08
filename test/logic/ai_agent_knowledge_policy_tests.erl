-module(ai_agent_knowledge_policy_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

off_policy_skips_knowledge_read_test_() ->
    ?WITH_MECKS(
        [{config_ds, [{'get', 2, fun(_, _) -> error(knowledge_must_not_be_read) end}]}],
        fun() ->
            ?assertEqual(
                <<>>,
                ai_agent_kb_logic:context(
                    #{
                        <<"role_code">> => <<"quiet">>,
                        <<"knowledge_policy">> => #{
                            <<"knowledge">> => #{<<"mode">> => <<"off">>}
                        }
                    },
                    <<"refund">>
                )
            )
        end
    ).

on_demand_policy_returns_only_matching_bounded_context_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun
                    (<<"ai_agent.kb.enabled">>, _) -> true;
                    (<<"ai_agent.kb.group_rule">>, _) -> <<"refund: 7 days\ninvoice: contact">>;
                    (<<"ai_agent.kb.faq">>, _) -> <<"refund: faq answer">>
                end}
            ]}
        ],
        fun() ->
            Context = ai_agent_kb_logic:context(
                #{
                    <<"role_code">> => <<"support">>,
                    <<"knowledge_policy">> => #{
                        <<"knowledge">> => #{
                            <<"mode">> => <<"on_demand">>,
                            <<"source">> => <<"all">>,
                            <<"max_context_bytes">> => 12
                        }
                    }
                },
                <<"refund">>
            ),
            ?assert(byte_size(Context) =< 12),
            ?assertNotEqual(nomatch, binary:match(Context, <<"refund">>)),
            ?assertEqual(nomatch, binary:match(Context, <<"invoice">>))
        end
    ).

required_policy_is_still_bounded_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun
                    (<<"ai_agent.kb.enabled">>, _) -> true;
                    (<<"ai_agent.kb.group_rule">>, _) -> <<"1234567890">>;
                    (<<"ai_agent.kb.faq">>, _) -> <<>>
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                5,
                byte_size(
                    ai_agent_kb_logic:context(
                        #{
                            <<"knowledge_policy">> => #{
                                <<"knowledge">> => #{
                                    <<"mode">> => <<"required">>,
                                    <<"max_context_bytes">> => 5
                                }
                            }
                        },
                        <<>>
                    )
                )
            )
        end
    ).
