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
        ?assertEqual(false, maps:get(channel, Features)),
        ?assertEqual(false, maps:get(moment, Features))
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
