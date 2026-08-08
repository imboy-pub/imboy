-module(ai_agent_role_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

normalize_policy_applies_safe_defaults_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, Policy} = ai_agent_role_ds:normalize_policy(#{}),
        ?assertEqual(
            <<"on_demand">>,
            maps:get(<<"mode">>, maps:get(<<"knowledge">>, Policy))
        ),
        ?assertEqual(
            <<"off">>,
            maps:get(<<"mode">>, maps:get(<<"group_reply">>, Policy))
        ),
        ?assertEqual(
            <<"off">>,
            maps:get(<<"mode">>, maps:get(<<"proactive">>, Policy))
        )
    end).

normalize_policy_rejects_unknown_capability_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, {unknown_capability, <<"web_search">>}},
            ai_agent_role_ds:normalize_policy(#{<<"web_search">> => #{}})
        )
    end).

normalize_policy_rejects_invalid_knowledge_mode_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, {invalid_mode, <<"knowledge">>, <<"always">>}},
            ai_agent_role_ds:normalize_policy(#{
                <<"knowledge">> => #{<<"mode">> => <<"always">>}
            })
        )
    end).

validate_config_rejects_empty_prompt_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, system_prompt_required},
            ai_agent_role_ds:validate_config(#{
                <<"code">> => <<"doctor">>,
                <<"name">> => <<"assistant">>,
                <<"system_prompt">> => <<>>
            })
        )
    end).

effective_config_prefers_role_behavior_and_keeps_agent_identity_test_() ->
    ?TEST_SIMPLE(fun() ->
        Agent = #{
            <<"user_id">> => 42,
            <<"provider">> => <<"qianfan">>,
            <<"model">> => <<"qwen-flash">>,
            <<"system_prompt">> => <<"old prompt">>,
            <<"capabilities">> => #{<<"knowledge">> => false}
        },
        Role = #{
            <<"code">> => <<"doctor">>,
            <<"version">> => 3,
            <<"system_prompt">> => <<"doctor prompt">>,
            <<"capabilities">> => #{
                <<"knowledge">> => true,
                <<"group_reply">> => false
            },
            <<"knowledge_policy">> => #{}
        },
        {ok, Effective} = ai_agent_role_ds:effective_config(Agent, Role),
        ?assertEqual(42, maps:get(<<"user_id">>, Effective)),
        ?assertEqual(<<"qwen-flash">>, maps:get(<<"model">>, Effective)),
        ?assertEqual(<<"doctor prompt">>, maps:get(<<"system_prompt">>, Effective)),
        ?assertEqual(<<"doctor">>, maps:get(<<"role_code">>, Effective)),
        ?assertEqual(3, maps:get(<<"role_version">>, Effective))
    end).

page_forwards_filters_to_repo_test_() ->
    ?WITH_MECK(
        ai_agent_role_repo,
        [
            {'page', 3, fun(2, 10, #{keyword := <<"doctor">>, status := 1} = Filters) ->
                ?assertEqual(#{keyword => <<"doctor">>, status => 1}, Filters),
                {ok, #{total => 0, page => 2, size => 10, list => []}}
            end}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{total := 0}},
                ai_agent_role_ds:page(2, 10, #{keyword => <<"doctor">>, status => 1})
            )
        end
    ).
