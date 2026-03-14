-module(imboy_policy_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

default_config_get(_Key, Default) ->
    Default.

current_profile_defaults_to_community_when_missing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun default_config_get/2},
            {'env', 2, fun(product_profile, community) -> community end}
        ]}
    ], fun() ->
        ?assertEqual(community, imboy_policy:current_profile())
    end).

current_profile_reads_explicit_enterprise_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun default_config_get/2},
            {'env', 2, fun(product_profile, community) -> enterprise end}
        ]}
    ], fun() ->
        ?assertEqual(enterprise, imboy_policy:current_profile())
    end).

current_profile_prefers_runtime_config_over_sys_config_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun(<<"product_profile">>, _Default) -> <<"enterprise">> end},
            {'env', 2, fun(product_profile, community) -> community end}
        ]}
    ], fun() ->
        ?assertEqual(enterprise, imboy_policy:current_profile())
    end).

effective_capabilities_merge_profile_defaults_and_overrides_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun default_config_get/2},
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
            {'get', 2, fun default_config_get/2},
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
            {'get', 2, fun default_config_get/2},
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
            {'get', 2, fun default_config_get/2},
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
            {'get', 2, fun default_config_get/2},
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

effective_view_returns_json_friendly_policy_payload_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun default_config_get/2},
            {'env', 2, fun
                (product_profile, community) -> enterprise;
                (capabilities, #{}) ->
                    #{
                        storage_mode => archived,
                        e2ee_mode => disabled,
                        message_search => true,
                        message_export => true,
                        audit_mode => full,
                        retention_policy => #{
                            mode => rolling_days,
                            days => 365
                        }
                    };
                (features, undefined) ->
                    #{
                        channel => #{enabled => true},
                        channel_invitation => #{enabled => true},
                        moment => #{enabled => false}
                    }
            end}
        ]}
    ], fun() ->
        PolicyView = imboy_policy:effective_view(),
        Capabilities = maps:get(<<"capabilities">>, PolicyView),
        Features = maps:get(<<"features">>, PolicyView),
        Plugins = maps:get(<<"plugins">>, PolicyView),
        ChannelPlugin = maps:get(<<"channel">>, Plugins),

        ?assertEqual(<<"enterprise">>, maps:get(<<"profile">>, PolicyView)),
        ?assertEqual(<<"archived">>, maps:get(<<"storage_mode">>, Capabilities)),
        ?assertEqual(<<"full">>, maps:get(<<"audit_mode">>, Capabilities)),
        ?assertEqual(365, maps:get(<<"days">>, maps:get(<<"retention_policy">>, Capabilities))),
        ?assertEqual(true, maps:get(<<"channel">>, Features)),
        ?assertEqual(false, maps:get(<<"moment">>, Features)),
        ?assertEqual(<<"plugin">>, maps:get(<<"kind">>, ChannelPlugin)),
        ?assertEqual(true, maps:get(<<"enabled">>, ChannelPlugin)),
        ?assertEqual(
            [<<"channel">>, <<"channel_discover">>, <<"channel_invitation">>, <<"channel_order">>],
            maps:get(<<"feature_keys">>, ChannelPlugin)
        ),
        ?assertEqual(
            [<<"channel_tab">>, <<"channel_discover_page">>],
            maps:get(<<"app_entries">>, ChannelPlugin)
        ),
        ?assertEqual(false, maps:is_key(<<"api_feature_rules">>, ChannelPlugin)),
        ?assertEqual(false, maps:is_key(<<"admin_feature_rules">>, ChannelPlugin)),
        ?assertEqual(false, maps:is_key(<<"api_target_feature_rules">>, ChannelPlugin)),
        ?assertEqual(false, maps:is_key(<<"admin_target_feature_rules">>, ChannelPlugin))
    end).

meta_view_returns_profiles_defaults_and_edit_options_test_() ->
    ?TEST_SIMPLE(fun() ->
        Meta = imboy_policy:meta_view(),
        CapabilityOptions = maps:get(<<"capability_options">>, Meta),
        ProfileDefaults = maps:get(<<"profile_defaults">>, Meta),
        CommunityDefaults = maps:get(<<"community">>, ProfileDefaults),
        EnterpriseDefaults = maps:get(<<"enterprise">>, ProfileDefaults),
        Plugins = maps:get(<<"plugins">>, Meta),
        ChannelPlugin = maps:get(<<"channel">>, Plugins),

        ?assertEqual([<<"community">>, <<"enterprise">>], maps:get(<<"profiles">>, Meta)),
        ?assertEqual(
            [<<"archived">>, <<"secure_e2ee">>],
            maps:get(<<"storage_mode">>, CapabilityOptions)
        ),
        ?assertEqual(
            [<<"disabled">>, <<"optional">>, <<"required">>],
            maps:get(<<"e2ee_mode">>, CapabilityOptions)
        ),
        ?assertEqual(
            [<<"none">>, <<"metadata">>, <<"full">>],
            maps:get(<<"audit_mode">>, CapabilityOptions)
        ),
        ?assertEqual(
            [<<"forever">>, <<"rolling_days">>],
            maps:get(<<"retention_policy_mode">>, CapabilityOptions)
        ),
        ?assertEqual(
            <<"metadata">>,
            maps:get(<<"audit_mode">>, maps:get(<<"capabilities">>, CommunityDefaults))
        ),
        ?assertEqual(
            true,
            maps:get(<<"channel">>, maps:get(<<"features">>, EnterpriseDefaults))
        ),
        ?assertEqual(
            [<<"channel">>, <<"channel_discover">>, <<"channel_invitation">>, <<"channel_order">>],
            maps:get(<<"feature_keys">>, ChannelPlugin)
        ),
        ?assertEqual(false, maps:is_key(<<"enabled">>, ChannelPlugin))
    end).

saved_view_returns_saved_overrides_and_compacts_complete_plugin_blocks_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun
                (<<"product_profile">>, _Default) -> <<"enterprise">>;
                (<<"capabilities">>, _Default) ->
                    #{
                        <<"message_export">> => false,
                        <<"audit_mode">> => <<"metadata">>
                    };
                (<<"features">>, _Default) ->
                    #{
                        <<"channel">> => #{<<"enabled">> => true},
                        <<"channel_discover">> => #{<<"enabled">> => true},
                        <<"channel_invitation">> => #{<<"enabled">> => true},
                        <<"channel_order">> => #{<<"enabled">> => true},
                        <<"moment">> => #{<<"enabled">> => false},
                        <<"group_vote">> => #{<<"enabled">> => false},
                        <<"group_schedule">> => #{<<"enabled">> => false},
                        <<"group_task">> => #{<<"enabled">> => false}
                    };
                (_Key, Default) -> Default
            end}
        ]}
    ], fun() ->
        SavedView = imboy_policy:saved_view(),
        ?assertEqual(<<"enterprise">>, maps:get(<<"profile">>, SavedView)),
        ?assertEqual(
            #{<<"message_export">> => false, <<"audit_mode">> => <<"metadata">>},
            maps:get(<<"capabilities">>, SavedView)
        ),
        ?assertEqual(
            #{
                <<"channel">> => true,
                <<"moment">> => false,
                <<"group_collab">> => false
            },
            maps:get(<<"plugins">>, SavedView)
        ),
        ?assertEqual(false, maps:is_key(<<"features">>, SavedView))
    end).

saved_view_keeps_mixed_plugin_feature_overrides_under_features_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun
                (<<"product_profile">>, Default) -> Default;
                (<<"capabilities">>, Default) -> Default;
                (<<"features">>, _Default) ->
                    #{
                        <<"channel">> => #{<<"enabled">> => true},
                        <<"channel_discover">> => #{<<"enabled">> => true},
                        <<"channel_invitation">> => #{<<"enabled">> => true},
                        <<"channel_order">> => #{<<"enabled">> => false},
                        <<"location">> => #{<<"enabled">> => true}
                    };
                (_Key, Default) -> Default
            end}
        ]}
    ], fun() ->
        SavedView = imboy_policy:saved_view(),
        Plugins = maps:get(<<"plugins">>, SavedView),
        Features = maps:get(<<"features">>, SavedView),

        ?assertEqual(#{<<"location">> => true}, Plugins),
        ?assertEqual(false, maps:is_key(<<"channel">>, Plugins)),
        ?assertEqual(true, maps:get(<<"channel">>, Features)),
        ?assertEqual(true, maps:get(<<"channel_discover">>, Features)),
        ?assertEqual(true, maps:get(<<"channel_invitation">>, Features)),
        ?assertEqual(false, maps:get(<<"channel_order">>, Features))
    end).

effective_policy_prefers_runtime_config_over_sys_config_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun
                (<<"product_profile">>, _Default) -> <<"enterprise">>;
                (<<"capabilities">>, _Default) ->
                    #{
                        <<"message_export">> => false,
                        <<"audit_mode">> => <<"full">>
                    };
                (<<"features">>, _Default) ->
                    #{
                        <<"channel">> => #{<<"enabled">> => false},
                        <<"moment">> => #{<<"enabled">> => true}
                    }
            end},
            {'env', 2, fun
                (product_profile, community) -> community;
                (capabilities, #{}) -> #{message_export => true, audit_mode => metadata};
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
        ?assertEqual(enterprise, maps:get(profile, Policy)),
        ?assertEqual(false, maps:get(message_export, maps:get(capabilities, Policy))),
        ?assertEqual(full, maps:get(audit_mode, maps:get(capabilities, Policy))),
        ?assertEqual(false, maps:get(channel, maps:get(features, Policy))),
        ?assertEqual(false, maps:get(channel_invitation, maps:get(features, Policy))),
        ?assertEqual(true, maps:get(moment, maps:get(features, Policy))),
        ?assertEqual(false, maps:get(enabled, maps:get(channel, maps:get(plugins, Policy)))),
        ?assertEqual(true, maps:get(enabled, maps:get(moment, maps:get(plugins, Policy))))
    end).

save_config_persists_profile_capabilities_and_plugin_translated_features_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun(Key, Default) ->
                case persistent_term:get({policy_config, Key}, undefined) of
                    undefined ->
                        Default;
                    Value ->
                        Value
                end
            end},
            {'set', 2, fun(Key, Value) ->
                persistent_term:put({policy_config, Key}, Value),
                ok
            end},
            {'env', 2, fun
                (product_profile, community) -> community;
                (capabilities, #{}) -> #{};
                (features, undefined) -> undefined
            end}
        ]}
    ], fun() ->
        persistent_term:erase({policy_config, <<"product_profile">>}),
        persistent_term:erase({policy_config, <<"capabilities">>}),
        persistent_term:erase({policy_config, <<"features">>}),
        {ok, PolicyView} = save_policy_config(#{
            <<"profile">> => <<"enterprise">>,
            <<"capabilities">> => #{
                <<"message_export">> => false,
                <<"audit_mode">> => <<"metadata">>
            },
            <<"features">> => #{
                <<"moment">> => true
            },
            <<"plugins">> => #{
                <<"channel">> => #{<<"enabled">> => true},
                <<"group_collab">> => false
            }
        }),
        ?assertEqual(3, meck:num_calls(config_ds, set, 2)),
        ?assertEqual(<<"enterprise">>, maps:get(<<"profile">>, PolicyView)),
        ?assertEqual(<<"metadata">>, maps:get(<<"audit_mode">>, maps:get(<<"capabilities">>, PolicyView))),
        ?assertEqual(true, maps:get(<<"channel">>, maps:get(<<"features">>, PolicyView))),
        ?assertEqual(false, maps:get(<<"group_vote">>, maps:get(<<"features">>, PolicyView))),
        ?assertEqual(true, maps:get(<<"enabled">>, maps:get(<<"channel">>, maps:get(<<"plugins">>, PolicyView)))),
        ?assertEqual(
            false,
            maps:get(<<"enabled">>, maps:get(<<"group_collab">>, maps:get(<<"plugins">>, PolicyView)))
        )
    end).

save_config_rejects_invalid_profile_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'set', 2, fun(_Key, _Value) -> erlang:error(should_not_be_called) end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"invalid profile value">>},
            save_policy_config(#{<<"profile">> => <<"invalid">>})
        ),
        ?assertEqual(0, meck:num_calls(config_ds, set, 2))
    end).

save_config_explicit_feature_override_beats_plugin_translation_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun(Key, Default) ->
                case persistent_term:get({policy_config, Key}, undefined) of
                    undefined ->
                        Default;
                    Value ->
                        Value
                end
            end},
            {'set', 2, fun(Key, Value) ->
                persistent_term:put({policy_config, Key}, Value),
                ok
            end},
            {'env', 2, fun
                (product_profile, community) -> community;
                (capabilities, #{}) -> #{};
                (features, undefined) -> undefined
            end}
        ]}
    ], fun() ->
        persistent_term:erase({policy_config, <<"features">>}),
        {ok, _PolicyView} = save_policy_config(#{
            <<"plugins">> => #{
                <<"channel">> => true
            },
            <<"features">> => #{
                <<"channel_order">> => false
            }
        }),
        AfterSaveView = imboy_policy:effective_view(),
        ?assertEqual(1, meck:num_calls(config_ds, set, 2)),
        ?assertEqual(
            false,
            maps:get(<<"channel_order">>, maps:get(<<"features">>, AfterSaveView))
        ),
        ?assertEqual(
            true,
            maps:get(<<"channel">>, maps:get(<<"features">>, AfterSaveView))
        )
    end).

save_config_merges_partial_updates_with_existing_runtime_overrides_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'get', 2, fun(Key, Default) ->
                case persistent_term:get({policy_config, Key}, undefined) of
                    undefined ->
                        Default;
                    Value ->
                        Value
                end
            end},
            {'set', 2, fun(Key, Value) ->
                persistent_term:put({policy_config, Key}, Value),
                ok
            end},
            {'env', 2, fun
                (product_profile, community) -> community;
                (capabilities, #{}) -> #{};
                (features, undefined) -> undefined
            end}
        ]}
    ], fun() ->
        persistent_term:put(
            {policy_config, <<"capabilities">>},
            #{
                <<"message_search">> => true,
                <<"audit_mode">> => <<"metadata">>
            }
        ),
        persistent_term:put(
            {policy_config, <<"features">>},
            #{
                <<"moment">> => #{<<"enabled">> => true},
                <<"location">> => #{<<"enabled">> => false}
            }
        ),
        {ok, PolicyView} = save_policy_config(#{
            <<"capabilities">> => #{
                <<"message_export">> => false
            },
            <<"plugins">> => #{
                <<"channel">> => true
            },
            <<"features">> => #{
                <<"channel_order">> => false
            }
        }),
        ?assertEqual(2, meck:num_calls(config_ds, set, 2)),
        ?assertEqual(
            #{
                <<"message_search">> => true,
                <<"audit_mode">> => <<"metadata">>,
                <<"message_export">> => false
            },
            persistent_term:get({policy_config, <<"capabilities">>})
        ),
        ?assertEqual(
            #{
                <<"moment">> => #{<<"enabled">> => true},
                <<"location">> => #{<<"enabled">> => false},
                <<"channel">> => #{<<"enabled">> => true},
                <<"channel_discover">> => #{<<"enabled">> => true},
                <<"channel_invitation">> => #{<<"enabled">> => true},
                <<"channel_order">> => #{<<"enabled">> => false}
            },
            persistent_term:get({policy_config, <<"features">>})
        ),
        ?assertEqual(
            false,
            maps:get(<<"message_export">>, maps:get(<<"capabilities">>, PolicyView))
        ),
        ?assertEqual(
            true,
            maps:get(<<"message_search">>, maps:get(<<"capabilities">>, PolicyView))
        ),
        ?assertEqual(
            true,
            maps:get(<<"moment">>, maps:get(<<"features">>, PolicyView))
        ),
        ?assertEqual(
            false,
            maps:get(<<"location">>, maps:get(<<"features">>, PolicyView))
        ),
        ?assertEqual(
            false,
            maps:get(<<"channel_order">>, maps:get(<<"features">>, PolicyView))
        )
    end).

save_config_rejects_invalid_capability_enum_input_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'set', 2, fun(_Key, _Value) -> erlang:error(should_not_be_called) end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"invalid storage_mode value">>},
            save_policy_config(#{
                <<"capabilities">> => #{
                    <<"storage_mode">> => <<"invalid_mode">>
                }
            })
        ),
        ?assertEqual(0, meck:num_calls(config_ds, set, 2))
    end).

save_config_rejects_invalid_feature_boolean_input_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'set', 2, fun(_Key, _Value) -> erlang:error(should_not_be_called) end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"invalid features payload">>},
            save_policy_config(#{
                <<"features">> => #{
                    <<"channel">> => #{<<"enabled">> => <<"maybe">>}
                }
            })
        ),
        ?assertEqual(0, meck:num_calls(config_ds, set, 2))
    end).

save_config_rejects_invalid_plugin_boolean_input_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'set', 2, fun(_Key, _Value) -> erlang:error(should_not_be_called) end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"invalid plugins payload">>},
            save_policy_config(#{
                <<"plugins">> => #{
                    <<"channel">> => #{<<"enabled">> => <<"maybe">>}
                }
            })
        ),
        ?assertEqual(0, meck:num_calls(config_ds, set, 2))
    end).

save_policy_config(Payload) ->
    ok = ensure_policy_module_loaded(),
    ExportCandidates = [save_config, save_admin_config, save_policy_config, save_admin_policy_config],
    case [Name || Name <- ExportCandidates, erlang:function_exported(imboy_policy, Name, 1)] of
        [Name | _] ->
            apply(imboy_policy, Name, [Payload]);
        [] ->
            erlang:error({missing_policy_save_export, ExportCandidates})
    end.

ensure_policy_module_loaded() ->
    case code:ensure_loaded(imboy_policy) of
        {module, imboy_policy} ->
            ok;
        Error ->
            erlang:error({failed_to_load_policy_module, Error})
    end.
