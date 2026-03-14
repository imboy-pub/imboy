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

channel_api_required_feature_defaults_to_channel_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(channel, imboy_plugin_registry:required_feature(api, channel_handler, show))
    end).

channel_api_required_feature_uses_subfeature_override_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            channel_discover,
            imboy_plugin_registry:required_feature(api, channel_handler, discover)
        ),
        ?assertEqual(
            channel_order,
            imboy_plugin_registry:required_feature(api, channel_handler, create_order)
        )
    end).

channel_admin_required_feature_defaults_to_channel_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            channel,
            imboy_plugin_registry:required_feature(admin, adm_channel_handler, list)
        ),
        ?assertEqual(
            channel_invitation,
            imboy_plugin_registry:required_feature(admin, adm_channel_handler, invitations)
        )
    end).

moment_and_location_api_required_features_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(moment, imboy_plugin_registry:required_feature(api, moment_handler, feed)),
        ?assertEqual(
            location,
            imboy_plugin_registry:required_feature(api, location_handler, people_nearby)
        )
    end).

group_collab_required_features_for_api_and_admin_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            group_vote,
            imboy_plugin_registry:required_feature(api, group_vote_handler, list)
        ),
        ?assertEqual(
            group_schedule,
            imboy_plugin_registry:required_feature(api, group_schedule_handler, list)
        ),
        ?assertEqual(
            group_task,
            imboy_plugin_registry:required_feature(api, group_task_handler, list)
        ),
        ?assertEqual(
            group_vote,
            imboy_plugin_registry:required_feature(admin, adm_group_handler, vote_list)
        ),
        ?assertEqual(
            group_schedule,
            imboy_plugin_registry:required_feature(admin, adm_group_handler, schedule_list)
        ),
        ?assertEqual(
            group_task,
            imboy_plugin_registry:required_feature(admin, adm_group_handler, task_list)
        )
    end).

shared_report_handlers_resolve_target_features_across_manifests_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            moment,
            imboy_plugin_registry:required_feature_for_target(api, report_handler, <<"moment">>)
        ),
        ?assertEqual(
            channel,
            imboy_plugin_registry:required_feature_for_target(api, report_handler, <<"channel">>)
        ),
        ?assertEqual(
            moment,
            imboy_plugin_registry:required_feature_for_target(admin, adm_report_handler, <<"moment">>)
        ),
        ?assertEqual(
            channel,
            imboy_plugin_registry:required_feature_for_target(admin, adm_report_handler, <<"channel">>)
        ),
        ?assertEqual(
            undefined,
            imboy_plugin_registry:required_feature_for_target(api, report_handler, <<"group">>)
        )
    end).
