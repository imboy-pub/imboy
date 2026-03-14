-module(imboy_policy_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

current_profile_defaults_to_community_when_missing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun(product_profile, community) -> community end}
        ]}
    ], fun() ->
        ?assertEqual(community, imboy_policy:current_profile())
    end).

current_profile_reads_explicit_enterprise_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun(product_profile, community) -> enterprise end}
        ]}
    ], fun() ->
        ?assertEqual(enterprise, imboy_policy:current_profile())
    end).

effective_capabilities_merge_profile_defaults_and_overrides_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun
                (product_profile, community) -> enterprise;
                (capabilities, #{}) ->
                    #{
                        message_export => false,
                        retention_policy => #{
                            mode => rolling_days,
                            days => 90
                        }
                    }
            end}
        ]}
    ], fun() ->
        Capabilities = imboy_policy:effective_capabilities(),

        ?assertEqual(archived, maps:get(storage_mode, Capabilities)),
        ?assertEqual(true, maps:get(message_search, Capabilities)),
        ?assertEqual(false, maps:get(message_export, Capabilities)),
        ?assertEqual(
            #{mode => rolling_days, days => 90},
            maps:get(retention_policy, Capabilities)
        )
    end).

effective_features_preserve_missing_features_block_compatibility_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun
                (product_profile, community) -> community;
                (capabilities, #{}) -> #{};
                (features, undefined) -> undefined
            end}
        ]}
    ], fun() ->
        Features = imboy_policy:effective_features(),

        ?assertEqual(true, maps:get(core, Features)),
        ?assertEqual(true, maps:get(moment, Features)),
        ?assertEqual(true, maps:get(channel_discover, Features))
    end).

effective_policy_returns_profile_capabilities_features_and_plugins_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun
                (product_profile, community) -> enterprise;
                (capabilities, #{}) -> #{audit_mode => full};
                (features, undefined) ->
                    #{
                        channel => #{enabled => true},
                        channel_invitation => #{enabled => true},
                        moment => #{enabled => false}
                    }
            end}
        ]}
    ], fun() ->
        Policy = imboy_policy:effective(),
        Plugins = maps:get(plugins, Policy),

        ?assertEqual(enterprise, maps:get(profile, Policy)),
        ?assertMatch(#{audit_mode := full}, maps:get(capabilities, Policy)),
        ?assertMatch(#{channel := true}, maps:get(features, Policy)),
        ?assertEqual(true, maps:get(enabled, maps:get(channel, Plugins))),
        ?assertEqual(false, maps:get(enabled, maps:get(moment, Plugins)))
    end).

secure_e2ee_forces_search_export_off_and_downgrades_audit_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun
                (product_profile, community) -> enterprise;
                (capabilities, #{}) ->
                    #{
                        storage_mode => secure_e2ee,
                        message_search => true,
                        message_export => true,
                        audit_mode => full
                    }
            end}
        ]}
    ], fun() ->
        Capabilities = imboy_policy:effective_capabilities(),

        ?assertEqual(secure_e2ee, maps:get(storage_mode, Capabilities)),
        ?assertEqual(false, maps:get(message_search, Capabilities)),
        ?assertEqual(false, maps:get(message_export, Capabilities)),
        ?assertEqual(metadata, maps:get(audit_mode, Capabilities)),
        ?assertEqual(false, imboy_policy:message_search_enabled()),
        ?assertEqual(false, imboy_policy:message_export_enabled()),
        ?assertEqual(metadata, imboy_policy:message_audit_mode()),
        ?assertEqual(true, imboy_policy:message_audit_enabled()),
        ?assertEqual(false, imboy_policy:message_body_visible())
    end).

required_e2ee_disables_body_visibility_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 2, fun
                (product_profile, community) -> community;
                (capabilities, #{}) ->
                    #{
                        e2ee_mode => required,
                        message_search => true,
                        audit_mode => full
                    }
            end}
        ]}
    ], fun() ->
        Capabilities = imboy_policy:effective_capabilities(),

        ?assertEqual(required, maps:get(e2ee_mode, Capabilities)),
        ?assertEqual(false, maps:get(message_search, Capabilities)),
        ?assertEqual(metadata, maps:get(audit_mode, Capabilities)),
        ?assertEqual(false, imboy_policy:message_search_enabled()),
        ?assertEqual(false, imboy_policy:message_body_visible())
    end).
