-module(imboy_policy_catalog).

%% @doc Static catalog/metadata extracted from imboy_policy.erl §5.
%% Pure data functions; no side effects.

-export([
    profile_defaults_catalog/0,
    origin_meta_catalog/0,
    capability_meta_catalog/0,
    capability_constraint_catalog/0,
    feature_meta_catalog/0,
    plugin_managed_feature_names/0,
    feature_dependency_catalog/0,
    feature_field_catalog/0,
    feature_field_meta/1,
    feature_plugin_owner/1,
    plugin_meta_catalog/0,
    editor_order_catalog/0,
    capability_names/0,
    request_shape_meta_catalog/0,
    dependencies/1
]).

-spec profile_defaults_catalog() -> map().
profile_defaults_catalog() ->
    maps:from_list([
        {Profile, imboy_profile_preset:defaults(Profile)}
     || Profile <- imboy_profile_preset:supported_profiles()
    ]).

-spec origin_meta_catalog() -> map().
origin_meta_catalog() ->
    #{
        semantics => canonical_saved_snapshot,
        description => <<"origins describe the canonical saved snapshot after plugin compaction">>,
        sections => #{
            profile => [default, override],
            capabilities => [default, override],
            features => [default, feature_override, plugin_override],
            plugins => [default, override, feature_overrides]
        }
    }.

-spec capability_meta_catalog() -> map().
capability_meta_catalog() ->
    #{
        storage_mode => #{
            type => enum,
            options => [archived, compliance_e2ee, secure_e2ee]
        },
        e2ee_mode => #{
            type => enum,
            options => [disabled, optional, compliance, required]
        },
        message_search => #{
            type => boolean
        },
        message_export => #{
            type => boolean
        },
        audit_mode => #{
            type => enum,
            options => [none, metadata, full]
        },
        retention_policy => #{
            type => object,
            fields => #{
                mode => #{type => string},
                days => #{type => integer}
            }
        },
        constraints => capability_constraint_catalog()
    }.

-spec capability_constraint_catalog() -> map().
capability_constraint_catalog() ->
    #{
        storage_mode => #{
            compliance_e2ee => #{
                message_search => compliance_key,
                audit_mode => full
            },
            secure_e2ee => #{
                message_search => false,
                message_export => false,
                audit_mode => metadata
            }
        },
        e2ee_mode => #{
            compliance => #{
                message_search => compliance_key,
                audit_mode => full
            },
            required => #{
                message_search => false,
                audit_mode => metadata
            }
        }
    }.

-spec feature_meta_catalog() -> map().
feature_meta_catalog() ->
    PluginManaged = plugin_managed_feature_names(),
    #{
        all => imboy_feature:feature_names(),
        plugin_managed => PluginManaged,
        standalone => imboy_feature:feature_names() -- PluginManaged,
        dependencies => feature_dependency_catalog(),
        catalog => feature_field_catalog()
    }.

-spec plugin_managed_feature_names() -> [atom()].
plugin_managed_feature_names() ->
    lists:usort(
        lists:append([
            maps:get(feature_keys, Manifest, [])
         || Manifest <- maps:values(imboy_plugin_registry:manifests())
        ])
    ).

-spec feature_dependency_catalog() -> map().
feature_dependency_catalog() ->
    lists:foldl(
        fun(FeatureName, Acc) ->
            case dependencies(FeatureName) of
                [] -> Acc;
                Deps -> Acc#{FeatureName => Deps}
            end
        end,
        #{},
        imboy_feature:feature_names()
    ).

-spec feature_field_catalog() -> map().
feature_field_catalog() ->
    maps:from_list([
        {FeatureName, feature_field_meta(FeatureName)}
     || FeatureName <- imboy_feature:feature_names()
    ]).

-spec feature_field_meta(atom()) -> map().
feature_field_meta(FeatureName) ->
    ManagedBy = feature_plugin_owner(FeatureName),
    Dependencies = dependencies(FeatureName),
    Meta0 = #{type => boolean},
    Meta1 =
        case ManagedBy of
            undefined ->
                Meta0;
            _ ->
                Meta0#{managed_by => ManagedBy}
        end,
    case Dependencies of
        [] ->
            Meta1;
        _ ->
            Meta1#{dependencies => Dependencies}
    end.

-spec feature_plugin_owner(atom()) -> atom() | undefined.
feature_plugin_owner(FeatureName) ->
    feature_plugin_owner(FeatureName, imboy_plugin_registry:plugin_names()).

-spec feature_plugin_owner(atom(), [atom()]) -> atom() | undefined.
feature_plugin_owner(_FeatureName, []) ->
    undefined;
feature_plugin_owner(FeatureName, [PluginName | Rest]) ->
    FeatureKeys = maps:get(feature_keys, imboy_plugin_registry:manifest(PluginName), []),
    case lists:member(FeatureName, FeatureKeys) of
        true ->
            PluginName;
        false ->
            feature_plugin_owner(FeatureName, Rest)
    end.

-spec plugin_meta_catalog() -> map().
plugin_meta_catalog() ->
    maps:map(
        fun(_Name, Manifest) ->
            imboy_policy_codec:public_plugin_manifest(Manifest)
        end,
        imboy_plugin_registry:manifests()
    ).

-spec editor_order_catalog() -> map().
editor_order_catalog() ->
    #{
        sections => [profile, capabilities, plugins, features],
        profiles => imboy_profile_preset:supported_profiles(),
        capabilities => capability_names(),
        features => imboy_feature:feature_names(),
        plugins => imboy_plugin_registry:plugin_names()
    }.

-spec capability_names() -> [atom()].
capability_names() ->
    [
        storage_mode,
        e2ee_mode,
        message_search,
        message_export,
        audit_mode,
        retention_policy
    ].

-spec request_shape_meta_catalog() -> map().
request_shape_meta_catalog() ->
    #{
        top_level_fields => [profile, capabilities, plugins, features],
        profile => #{
            canonical_key => profile,
            accepted_keys => [profile, product_profile],
            type => enum,
            options => imboy_profile_preset:supported_profiles(),
            nullable => true
        },
        capabilities => #{
            canonical_key => capabilities,
            type => object,
            fields => capability_names(),
            null_clears_fields => true
        },
        features => #{
            canonical_key => features,
            type => object,
            fields => imboy_feature:feature_names(),
            value_forms => [boolean, enabled_object],
            null_clears_fields => true
        },
        plugins => #{
            canonical_key => plugins,
            type => object,
            fields => imboy_plugin_registry:plugin_names(),
            value_forms => [boolean, enabled_object],
            null_clears_fields => true
        }
    }.

-spec dependencies(atom()) -> [atom()].
dependencies(channel_discover) -> [channel];
dependencies(channel_invitation) -> [channel];
dependencies(channel_order) -> [channel];
dependencies(_) -> [].
