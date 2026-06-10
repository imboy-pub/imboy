-module(imboy_policy_view).
%% @doc 策略预览视图层（从 imboy_policy.erl §preview 提取）

-export([preview_view/1, preview_saved_view/1, preview_effective_view/1]).

%% Preview functions delegate to persistence layer for data access
-define(PRODUCT_PROFILE_CONFIG_KEY, <<"product_profile">>).
-define(CAPABILITIES_CONFIG_KEY, <<"capabilities">>).
-define(FEATURES_CONFIG_KEY, <<"features">>).
-define(DELETE_VALUE, '$delete').

-spec preview_view(term()) -> map().
preview_view(SaveSections) ->
    Saved = preview_saved_view(SaveSections),
    Effective = preview_effective_view(SaveSections),
    #{
        <<"saved">> => Saved,
        <<"effective">> => Effective,
        <<"adjustments">> => preview_adjustments_view(Saved, Effective),
        <<"origins">> => origins_view(Saved)
    }.

-spec preview_saved_view(map()) -> map().
preview_saved_view(SaveSections) ->
    Profile =
        case maps:find(?PRODUCT_PROFILE_CONFIG_KEY, SaveSections) of
            {ok, ProfileValue} -> ProfileValue;
            error -> saved_profile_override()
        end,
    Capabilities =
        case maps:find(?CAPABILITIES_CONFIG_KEY, SaveSections) of
            {ok, CapabilityValue} ->
                normalize_preview_capability_overrides(
                    merge_persisted_section(?CAPABILITIES_CONFIG_KEY, CapabilityValue)
                );
            error ->
                saved_capability_overrides()
        end,
    Features =
        case maps:find(?FEATURES_CONFIG_KEY, SaveSections) of
            {ok, FeatureValue} ->
                normalize_preview_feature_overrides(
                    merge_persisted_section(?FEATURES_CONFIG_KEY, FeatureValue)
                );
            error ->
                saved_feature_overrides()
        end,
    saved_view_from_values(Profile, Capabilities, Features).

-spec preview_effective_view(map()) -> map().
preview_effective_view(SaveSections) ->
    ProfileConfig =
        case maps:find(?PRODUCT_PROFILE_CONFIG_KEY, SaveSections) of
            {ok, ?DELETE_VALUE} -> config_ds:env(product_profile, community);
            {ok, ProfileValue} -> ProfileValue;
            error -> load_profile_config()
        end,
    CapabilityConfig =
        case maps:find(?CAPABILITIES_CONFIG_KEY, SaveSections) of
            {ok, CapabilityValue} ->
                merge_persisted_section(?CAPABILITIES_CONFIG_KEY, CapabilityValue);
            error ->
                load_capability_config()
        end,
    FeatureConfig =
        case maps:find(?FEATURES_CONFIG_KEY, SaveSections) of
            {ok, FeatureValue} -> merge_persisted_section(?FEATURES_CONFIG_KEY, FeatureValue);
            error -> load_feature_config()
        end,
    effective_view_from_configs(ProfileConfig, CapabilityConfig, FeatureConfig).

-spec preview_adjustments_view(map(), map()) -> map().
preview_adjustments_view(Saved, Effective) ->
    Sections0 = maybe_put_saved_section(
        #{},
        capabilities,
        capability_adjustments(
            maps:get(<<"capabilities">>, Saved, #{}),
            maps:get(<<"capabilities">>, Effective, #{})
        )
    ),
    Sections1 = maybe_put_saved_section(
        Sections0,
        plugins,
        plugin_adjustments(
            maps:get(<<"plugins">>, Saved, #{}),
            maps:get(<<"plugins">>, Effective, #{}),
            maps:get(<<"capabilities">>, Effective, #{})
        )
    ),
    imboy_policy_codec:public_term(
        maybe_put_saved_section(
            Sections1,
            features,
            feature_adjustments(
                maps:get(<<"features">>, Saved, #{}),
                maps:get(<<"features">>, Effective, #{}),
                maps:get(<<"plugins">>, Effective, #{}),
                maps:get(<<"capabilities">>, Effective, #{})
            )
        )
    ).

-spec origins_view(map()) -> map().
origins_view(Saved) ->
    % Origins describe the canonical saved snapshot after plugin compaction.
    SavedCapabilities = maps:get(<<"capabilities">>, Saved, #{}),
    SavedFeatures = maps:get(<<"features">>, Saved, #{}),
    SavedPlugins = maps:get(<<"plugins">>, Saved, #{}),
    #{
        <<"profile">> => origin_from_presence(maps:is_key(<<"profile">>, Saved), override, default),
        <<"capabilities">> => capability_origins(SavedCapabilities),
        <<"features">> => feature_origins(SavedFeatures, SavedPlugins),
        <<"plugins">> => plugin_origins(SavedFeatures, SavedPlugins)
    }.

-spec origin_from_presence(boolean(), atom(), atom()) -> binary().
origin_from_presence(true, PresentOrigin, _MissingOrigin) ->
    atom_to_binary(PresentOrigin, utf8);
origin_from_presence(false, _PresentOrigin, MissingOrigin) ->
    atom_to_binary(MissingOrigin, utf8).

-spec capability_origins(map()) -> map().
capability_origins(SavedCapabilities) ->
    maps:from_list([
        {
            imboy_policy_codec:public_key(Key),
            origin_from_presence(
                maps:is_key(imboy_policy_codec:public_key(Key), SavedCapabilities),
                override,
                default
            )
        }
     || Key <- imboy_policy_catalog:capability_names()
    ]).

-spec feature_origins(map(), map()) -> map().
feature_origins(SavedFeatures, SavedPlugins) ->
    maps:from_list([
        {imboy_policy_codec:public_key(Key), feature_origin(Key, SavedFeatures, SavedPlugins)}
     || Key <- feature_names()
    ]).

-spec feature_origin(atom(), map(), map()) -> binary().
feature_origin(FeatureName, SavedFeatures, SavedPlugins) ->
    FeatureKey = imboy_policy_codec:public_key(FeatureName),
    case maps:is_key(FeatureKey, SavedFeatures) of
        true ->
            <<"feature_override">>;
        false ->
            case imboy_policy_catalog:feature_plugin_owner(FeatureName) of
                undefined ->
                    <<"default">>;
                PluginName ->
                    case maps:is_key(imboy_policy_codec:public_key(PluginName), SavedPlugins) of
                        true ->
                            <<"plugin_override">>;
                        false ->
                            <<"default">>
                    end
            end
    end.

-spec plugin_origins(map(), map()) -> map().
plugin_origins(SavedFeatures, SavedPlugins) ->
    maps:from_list([
        {
            imboy_policy_codec:public_key(PluginName),
            plugin_origin(PluginName, SavedFeatures, SavedPlugins)
        }
     || PluginName <- imboy_plugin_registry:plugin_names()
    ]).

-spec plugin_origin(atom(), map(), map()) -> binary().
plugin_origin(PluginName, SavedFeatures, SavedPlugins) ->
    PluginKey = imboy_policy_codec:public_key(PluginName),
    case maps:is_key(PluginKey, SavedPlugins) of
        true ->
            <<"override">>;
        false ->
            FeatureKeys = maps:get(feature_keys, imboy_plugin_registry:manifest(PluginName), []),
            case
                lists:any(
                    fun(FeatureKey) ->
                        maps:is_key(imboy_policy_codec:public_key(FeatureKey), SavedFeatures)
                    end,
                    FeatureKeys
                )
            of
                true ->
                    <<"feature_overrides">>;
                false ->
                    <<"default">>
            end
    end.

%% 委托至数据来源模块
saved_profile_override() -> imboy_policy_persistence:saved_profile_override().
saved_capability_overrides() -> imboy_policy_persistence:saved_capability_overrides().
saved_feature_overrides() -> imboy_policy_persistence:saved_feature_overrides().
load_profile_config() -> imboy_policy_persistence:load_profile_config().
load_capability_config() -> imboy_policy_persistence:load_capability_config().
load_feature_config() -> imboy_policy_persistence:load_feature_config().
merge_persisted_section(K, V) -> imboy_policy_persistence:merge_persisted_section(K, V).
maybe_put_saved_section(M, K, V) -> imboy_policy_persistence:maybe_put_saved_section(M, K, V).
saved_view_from_values(P, C, F) -> imboy_policy:saved_view_from_values(P, C, F).
effective_view_from_configs(P, C, F) -> imboy_policy:effective_view_from_configs(P, C, F).
normalize_preview_capability_overrides(V) -> imboy_policy:normalize_preview_capability_overrides(V).
normalize_preview_feature_overrides(V) -> imboy_policy:normalize_preview_feature_overrides(V).
capability_adjustments(S, E) -> imboy_policy:capability_adjustments(S, E).
plugin_adjustments(S, E, C) -> imboy_policy:plugin_adjustments(S, E, C).
feature_adjustments(S, E, P, C) -> imboy_policy:feature_adjustments(S, E, P, C).
feature_names() -> imboy_feature:feature_names().
