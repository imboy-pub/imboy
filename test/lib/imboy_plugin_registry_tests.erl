-module(imboy_plugin_registry_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

all_manifests_expose_stable_contract_test_() ->
    ?TEST_SIMPLE(fun() ->
        ManifestMap = imboy_plugin_registry:manifests(),

        ?assert(is_map(ManifestMap)),
        ?assert(map_size(ManifestMap) > 0),
        lists:foreach(
            fun({PluginName, Manifest}) ->
                ?assertEqual(Manifest, imboy_plugin_registry:manifest(PluginName)),
                ?assert(maps:is_key(kind, Manifest)),
                ?assert(maps:is_key(feature_keys, Manifest)),
                ?assert(maps:is_key(api_feature_rules, Manifest)),
                ?assert(maps:is_key(admin_feature_rules, Manifest)),
                case maps:get(kind, Manifest) of
                    aggregate_plugin ->
                        ?assert(maps:is_key(children, Manifest));
                    _ ->
                        ok
                end
            end,
            maps:to_list(ManifestMap)
        )
    end).

channel_manifest_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifest = imboy_plugin_registry:manifest(channel),

        ?assertEqual(plugin, maps:get(kind, Manifest)),
        ?assertEqual(
            [channel, channel_discover, channel_invitation, channel_order],
            maps:get(feature_keys, Manifest)
        )
    end).

group_collab_manifest_is_aggregate_plugin_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifest = imboy_plugin_registry:manifest(group_collab),

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
            imboy_plugin_registry:required_feature_for_target(
                admin, adm_report_handler, <<"moment">>
            )
        ),
        ?assertEqual(
            channel,
            imboy_plugin_registry:required_feature_for_target(
                admin, adm_report_handler, <<"channel">>
            )
        ),
        ?assertEqual(
            undefined,
            imboy_plugin_registry:required_feature_for_target(api, report_handler, <<"group">>)
        )
    end).

%% --- A1: feature_enabled/2 atom/binary key bug fix ---

feature_enabled_atom_key_converts_to_binary_for_lookup_test_() ->
    ?TEST_SIMPLE(fun() ->
        EnabledFeatures = #{<<"channel">> => true},
        ?assertEqual(true, imboy_plugin_registry:feature_enabled(channel, EnabledFeatures))
    end).

feature_enabled_binary_key_matches_directly_test_() ->
    ?TEST_SIMPLE(fun() ->
        EnabledFeatures = #{<<"moment">> => true},
        ?assertEqual(true, imboy_plugin_registry:feature_enabled(<<"moment">>, EnabledFeatures))
    end).

feature_enabled_returns_false_for_disabled_feature_test_() ->
    ?TEST_SIMPLE(fun() ->
        EnabledFeatures = #{<<"channel">> => false},
        ?assertEqual(false, imboy_plugin_registry:feature_enabled(channel, EnabledFeatures))
    end).

feature_enabled_returns_false_for_missing_feature_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, imboy_plugin_registry:feature_enabled(channel, #{}))
    end).

feature_enabled_accepts_nested_atom_enabled_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        EnabledFeatures = #{<<"location">> => #{enabled => true}},
        ?assertEqual(true, imboy_plugin_registry:feature_enabled(location, EnabledFeatures))
    end).

feature_enabled_accepts_nested_binary_enabled_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        EnabledFeatures = #{<<"e2ee">> => #{<<"enabled">> => true}},
        ?assertEqual(true, imboy_plugin_registry:feature_enabled(e2ee, EnabledFeatures))
    end).

feature_enabled_returns_false_for_nested_disabled_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        EnabledFeatures = #{<<"moment">> => #{enabled => false}},
        ?assertEqual(false, imboy_plugin_registry:feature_enabled(moment, EnabledFeatures))
    end).

%% --- A1/B1: all_feature_keys/0 ---

all_feature_keys_returns_list_of_atoms_test_() ->
    ?TEST_SIMPLE(fun() ->
        Keys = imboy_plugin_registry:all_feature_keys(),
        ?assert(is_list(Keys)),
        lists:foreach(fun(K) -> ?assert(is_atom(K)) end, Keys)
    end).

all_feature_keys_contains_no_duplicates_test_() ->
    ?TEST_SIMPLE(fun() ->
        Keys = imboy_plugin_registry:all_feature_keys(),
        ?assertEqual(length(Keys), length(lists:usort(Keys)))
    end).

all_feature_keys_contains_expected_channel_features_test_() ->
    ?TEST_SIMPLE(fun() ->
        Keys = imboy_plugin_registry:all_feature_keys(),
        lists:foreach(
            fun(K) -> ?assert(lists:member(K, Keys)) end,
            [channel, channel_discover, channel_invitation, channel_order]
        )
    end).
