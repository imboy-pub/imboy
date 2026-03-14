-module(imboy_plugin_registry_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

channel_manifest_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifest = imboy_plugin_registry:get(channel),

        ?assertEqual(plugin, maps:get(kind, Manifest)),
        ?assertEqual(
            [channel, channel_discover, channel_invitation, channel_order],
            maps:get(feature_keys, Manifest)
        )
    end).

group_collab_manifest_is_aggregate_plugin_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifest = imboy_plugin_registry:get(group_collab),

        ?assertEqual(aggregate_plugin, maps:get(kind, Manifest)),
        ?assertEqual([vote, schedule, task], maps:get(children, Manifest)),
        ?assertEqual(
            [group_vote, group_schedule, group_task],
            maps:get(feature_keys, Manifest)
        )
    end).

plugin_names_returns_expected_catalog_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            [channel, moment, location, group_collab],
            imboy_plugin_registry:plugin_names()
        )
    end).
