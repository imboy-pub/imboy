-module(imboy_profile_preset_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

community_profile_defaults_test_() ->
    ?TEST_SIMPLE(fun() ->
        Defaults = imboy_profile_preset:defaults(community),
        Capabilities = maps:get(capabilities, Defaults),
        Features = maps:get(features, Defaults),

        ?assertEqual(archived, maps:get(storage_mode, Capabilities)),
        ?assertEqual(optional, maps:get(e2ee_mode, Capabilities)),
        ?assertEqual(false, maps:get(message_search, Capabilities)),
        ?assertEqual(true, maps:get(channel, Features)),
        ?assertEqual(true, maps:get(moment, Features))
    end).

enterprise_profile_defaults_test_() ->
    ?TEST_SIMPLE(fun() ->
        Defaults = imboy_profile_preset:defaults(enterprise),
        Capabilities = maps:get(capabilities, Defaults),
        Features = maps:get(features, Defaults),

        ?assertEqual(archived, maps:get(storage_mode, Capabilities)),
        ?assertEqual(disabled, maps:get(e2ee_mode, Capabilities)),
        ?assertEqual(true, maps:get(message_search, Capabilities)),
        ?assertEqual(true, maps:get(channel, Features)),
        ?assertEqual(true, maps:get(channel_invitation, Features))
    end).

unknown_profile_falls_back_to_community_test_() ->
    ?TEST_SIMPLE(fun() ->
        Community = imboy_profile_preset:defaults(community),
        Unknown = imboy_profile_preset:defaults(<<"unknown">>),

        ?assertEqual(Community, Unknown)
    end).

overseas_baseline_sensitive_features_disabled_test_() ->
    ?TEST_SIMPLE(fun() ->
        Defaults = imboy_profile_preset:defaults(overseas_baseline),
        Features = maps:get(features, Defaults),

        %% L-01 默认 OFF 清单
        ?assertEqual(false, maps:get(location, Features)),
        ?assertEqual(false, maps:get(channel_discover, Features)),
        ?assertEqual(false, maps:get(channel_order, Features)),
        ?assertEqual(false, maps:get(live_room, Features)),
        ?assertEqual(false, maps:get(ai_marketplace, Features)),
        ?assertEqual(false, maps:get(bot_webhook, Features)),

        %% 核心基线保持可用
        ?assertEqual(true, maps:get(core, Features)),
        ?assertEqual(true, maps:get(e2ee, Features)),
        ?assertEqual(true, maps:get(channel, Features)),
        ?assertEqual(true, maps:get(moment, Features))
    end).

overseas_baseline_capabilities_match_community_test_() ->
    ?TEST_SIMPLE(fun() ->
        Overseas = imboy_profile_preset:defaults(overseas_baseline),
        Community = imboy_profile_preset:defaults(community),

        ?assertEqual(
            maps:get(capabilities, Community),
            maps:get(capabilities, Overseas)
        )
    end).

overseas_baseline_profile_normalize_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            [community, enterprise, overseas_baseline],
            imboy_profile_preset:supported_profiles()
        ),
        ?assertEqual(overseas_baseline, imboy_profile_preset:normalize_profile(overseas_baseline)),
        ?assertEqual(
            overseas_baseline,
            imboy_profile_preset:normalize_profile(<<"overseas_baseline">>)
        ),
        ?assertEqual(
            overseas_baseline,
            imboy_profile_preset:normalize_profile("overseas_baseline")
        )
    end).
