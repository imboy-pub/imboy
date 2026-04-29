-module(imboy_policy).

-export([
    current_profile/0,
    effective/0,
    effective_view/0,
    admin_config_view/0,
    meta_view/0,
    saved_view/0,
    effective_capabilities/0,
    effective_features/0,
    effective_plugins/0,
    preview_admin_config/1,
    save_admin_config/1,
    save_config/1,
    message_search_enabled/0,
    message_export_enabled/0,
    message_audit_mode/0,
    message_audit_enabled/0,
    message_body_visible/0,
    message_encryption_required/0,
    e2ee_enabled/0,
    validate_message_write/5
]).

-define(PRODUCT_PROFILE_CONFIG_KEY, <<"product_profile">>).
-define(CAPABILITIES_CONFIG_KEY, <<"capabilities">>).
-define(FEATURES_CONFIG_KEY, <<"features">>).
-define(DELETE_VALUE, '$delete').

-spec current_profile() -> community | enterprise.
current_profile() ->
    case normalize_profile_input(load_profile_config()) of
        {ok, Profile} ->
            Profile;
        error ->
            imboy_profile_preset:current()
    end.

-spec effective() -> map().
effective() ->
    effective_from_configs(load_profile_config(), load_capability_config(), load_feature_config()).

-spec effective_view() -> map().
effective_view() ->
    effective_view_from_configs(load_profile_config(), load_capability_config(), load_feature_config()).

-spec saved_view() -> map().
saved_view() ->
    SavedFeatures0 = saved_feature_overrides(),
    {SavedPlugins, SavedFeatures} = compact_saved_plugin_overrides(SavedFeatures0),
    Sections0 = maybe_put_saved_section(#{}, profile, saved_profile_override()),
    Sections1 = maybe_put_saved_section(Sections0, capabilities, saved_capability_overrides()),
    Sections2 = maybe_put_saved_section(Sections1, plugins, SavedPlugins),
    public_term(maybe_put_saved_section(Sections2, features, SavedFeatures)).

-spec admin_config_view() -> map().
admin_config_view() ->
    Saved = saved_view(),
    Effective = effective_view(),
    #{
        <<"meta">> => meta_view(),
        <<"saved">> => Saved,
        <<"effective">> => Effective,
        <<"adjustments">> => preview_adjustments_view(Saved, Effective),
        <<"origins">> => origins_view(Saved)
    }.

-spec meta_view() -> map().
meta_view() ->
    public_term(#{
        profiles => #{
            supported => imboy_profile_preset:supported_profiles(),
            defaults => profile_defaults_catalog()
        },
        origins => origin_meta_catalog(),
        capabilities => capability_meta_catalog(),
        features => feature_meta_catalog(),
        plugins => plugin_meta_catalog(),
        editor_order => editor_order_catalog(),
        write_contract => #{
            plugins_translate_to_features => true,
            feature_overrides_take_precedence => true,
            null_clears_overrides => true,
            request_shape => request_shape_meta_catalog(),
            preview_available => true,
            preview_returns => [saved, effective, adjustments, origins],
            bootstrap_available => true,
            bootstrap_returns => [meta, saved, effective, adjustments, origins],
            save_returns => [effective, saved, adjustments, origins],
            validation_error_details => true,
            validation_error_fields => [section, field, reason],
            editable_sections => [profile, capabilities, plugins, features]
        }
    }).

-spec effective_capabilities() -> map().
effective_capabilities() ->
    effective_capabilities_for_profile(current_profile(), load_capability_config()).

-spec message_search_enabled() -> boolean().
message_search_enabled() ->
    maps:get(message_search, effective_capabilities(), false).

-spec message_export_enabled() -> boolean().
message_export_enabled() ->
    maps:get(message_export, effective_capabilities(), false).

-spec message_audit_mode() -> none | metadata | full.
message_audit_mode() ->
    maps:get(audit_mode, effective_capabilities(), none).

-spec message_audit_enabled() -> boolean().
message_audit_enabled() ->
    message_audit_mode() =/= none.

-spec message_body_visible() -> boolean().
message_body_visible() ->
    message_audit_mode() =:= full.

-spec message_encryption_required() -> boolean().
message_encryption_required() ->
    Capabilities = effective_capabilities(),
    StorageMode = maps:get(storage_mode, Capabilities, archived),
    E2eeMode = maps:get(e2ee_mode, Capabilities, disabled),
    StorageMode =:= secure_e2ee orelse
    StorageMode =:= compliance_e2ee orelse
    E2eeMode =:= required orelse
    E2eeMode =:= compliance.

-spec e2ee_enabled() -> boolean().
e2ee_enabled() ->
    maps:get(e2ee_mode, effective_capabilities(), disabled) =/= disabled.

-spec validate_message_write(binary(), binary(), binary(), term(), term()) -> ok | {error, binary()}.
validate_message_write(Type, MsgType, Action, E2EE, Payload) ->
    case policy_managed_content_write(Type, Action) andalso message_encryption_required() of
        false ->
            ok;
        true ->
            case encrypted_message_body(MsgType, E2EE, Payload) of
                true ->
                    ok;
                false ->
                    {error, <<"encrypted_message_required">>}
            end
    end.

-spec policy_managed_content_write(binary(), binary()) -> boolean().
policy_managed_content_write(<<"C2C">>, Action) ->
    content_bearing_action(Action);
policy_managed_content_write(<<"C2G">>, Action) ->
    content_bearing_action(Action);
policy_managed_content_write(_, _) ->
    false.

-spec content_bearing_action(binary()) -> boolean().
content_bearing_action(<<>>) ->
    true;
content_bearing_action(<<"message_edit">>) ->
    true;
content_bearing_action(_) ->
    false.

-spec encrypted_message_body(binary(), term(), term()) -> boolean().
encrypted_message_body(<<"e2ee">>, E2EE, Payload) when is_map(E2EE), is_binary(Payload) ->
    map_size(E2EE) > 0 andalso Payload =/= <<>>;
encrypted_message_body(_, _, _) ->
    false.

-spec save_admin_config(map()) -> {ok, map()} | {error, binary()} | {error, binary(), map()}.
save_admin_config(Payload) ->
    save_config(Payload).

-spec preview_admin_config(map()) -> {ok, map()} | {error, binary()} | {error, binary(), map()}.
preview_admin_config(Payload) ->
    preview_config(Payload).

-spec save_config(map()) -> {ok, map()} | {error, binary()} | {error, binary(), map()}.
save_config(Payload) when is_map(Payload) ->
    Sections = normalize_config_sections(Payload),
    case validate_save_sections(Sections) of
        {ok, SaveSections} when map_size(SaveSections) > 0 ->
            persist_config_sections(SaveSections),
            {ok, save_result_view()};
        {ok, _SaveSections} ->
            policy_error_result(
                undefined, undefined, missing_editable_fields, <<"policy payload missing editable fields">>);
        {error, Reason, Details} ->
            {error, Reason, Details}
    end;
save_config(_) ->
    policy_error_result(undefined, undefined, invalid_payload_type, <<"policy payload must be an object">>).

-spec save_result_view() -> map().
save_result_view() ->
    Effective = effective_view(),
    Saved = saved_view(),
    Effective#{
        <<"saved">> => Saved,
        <<"adjustments">> => preview_adjustments_view(Saved, Effective),
        <<"origins">> => origins_view(Saved)
    }.

-spec preview_config(map()) -> {ok, map()} | {error, binary()} | {error, binary(), map()}.
preview_config(Payload) when is_map(Payload) ->
    Sections = normalize_config_sections(Payload),
    case validate_save_sections(Sections) of
        {ok, SaveSections} when map_size(SaveSections) > 0 ->
            {ok, preview_view(SaveSections)};
        {ok, _SaveSections} ->
            policy_error_result(
                undefined, undefined, missing_editable_fields, <<"policy payload missing editable fields">>);
        {error, Reason, Details} ->
            {error, Reason, Details}
    end;
preview_config(_) ->
    policy_error_result(undefined, undefined, invalid_payload_type, <<"policy payload must be an object">>).

-spec effective_features() -> map().
effective_features() ->
    Profile = current_profile(),
    {_, Features, _} = effective_policy_components(
        Profile,
        load_capability_config(),
        load_feature_config()
    ),
    Features.

-spec effective_plugins() -> map().
effective_plugins() ->
    Profile = current_profile(),
    {_, _, Plugins} = effective_policy_components(
        Profile,
        load_capability_config(),
        load_feature_config()
    ),
    Plugins.

-spec effective_plugins(map()) -> map().
effective_plugins(Features) ->
    maps:map(
        fun(_Name, Manifest) ->
            Enabled = lists:any(
                fun(FeatureKey) ->
                    maps:get(FeatureKey, Features, false)
                end,
                maps:get(feature_keys, Manifest, [])
            ),
            Manifest#{enabled => Enabled}
        end,
        imboy_plugin_registry:manifests()
    ).

-spec effective_from_configs(term(), term(), term()) -> map().
effective_from_configs(ProfileConfig, CapabilityConfig, FeatureConfig) ->
    Profile = resolve_profile(ProfileConfig),
    {Capabilities, Features, Plugins} = effective_policy_components(
        Profile,
        CapabilityConfig,
        FeatureConfig
    ),
    #{
        profile => Profile,
        capabilities => Capabilities,
        features => Features,
        plugins => Plugins
    }.

-spec effective_view_from_configs(term(), term(), term()) -> map().
effective_view_from_configs(ProfileConfig, CapabilityConfig, FeatureConfig) ->
    public_effective_policy(
        effective_from_configs(ProfileConfig, CapabilityConfig, FeatureConfig)
    ).

-spec public_effective_policy(map()) -> map().
public_effective_policy(Policy) ->
    Plugins0 = maps:get(plugins, Policy, #{}),
    Plugins = maps:map(
        fun(_Name, Manifest) ->
            public_plugin_manifest(Manifest)
        end,
        Plugins0
    ),
    public_term(Policy#{plugins => Plugins}).

-spec preview_view(map()) -> map().
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
    Profile = case maps:find(?PRODUCT_PROFILE_CONFIG_KEY, SaveSections) of
        {ok, ProfileValue} -> ProfileValue;
        error -> saved_profile_override()
    end,
    Capabilities = case maps:find(?CAPABILITIES_CONFIG_KEY, SaveSections) of
        {ok, CapabilityValue} ->
            normalize_preview_capability_overrides(
                merge_persisted_section(?CAPABILITIES_CONFIG_KEY, CapabilityValue)
            );
        error -> saved_capability_overrides()
    end,
    Features = case maps:find(?FEATURES_CONFIG_KEY, SaveSections) of
        {ok, FeatureValue} ->
            normalize_preview_feature_overrides(
                merge_persisted_section(?FEATURES_CONFIG_KEY, FeatureValue)
            );
        error -> saved_feature_overrides()
    end,
    saved_view_from_values(Profile, Capabilities, Features).

-spec preview_effective_view(map()) -> map().
preview_effective_view(SaveSections) ->
    ProfileConfig = case maps:find(?PRODUCT_PROFILE_CONFIG_KEY, SaveSections) of
        {ok, ?DELETE_VALUE} -> config_ds:env(product_profile, community);
        {ok, ProfileValue} -> ProfileValue;
        error -> load_profile_config()
    end,
    CapabilityConfig = case maps:find(?CAPABILITIES_CONFIG_KEY, SaveSections) of
        {ok, CapabilityValue} -> merge_persisted_section(?CAPABILITIES_CONFIG_KEY, CapabilityValue);
        error -> load_capability_config()
    end,
    FeatureConfig = case maps:find(?FEATURES_CONFIG_KEY, SaveSections) of
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
            maps:get(<<"capabilities">>, Effective, #{}))
    ),
    Sections1 = maybe_put_saved_section(
        Sections0,
        plugins,
        plugin_adjustments(
            maps:get(<<"plugins">>, Saved, #{}),
            maps:get(<<"plugins">>, Effective, #{}),
            maps:get(<<"capabilities">>, Effective, #{}))
    ),
    public_term(
        maybe_put_saved_section(
            Sections1,
            features,
            feature_adjustments(
                maps:get(<<"features">>, Saved, #{}),
                maps:get(<<"features">>, Effective, #{}),
                maps:get(<<"plugins">>, Effective, #{}),
                maps:get(<<"capabilities">>, Effective, #{}))
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
        {public_key(Key), origin_from_presence(maps:is_key(public_key(Key), SavedCapabilities), override, default)}
        || Key <- capability_names()
    ]).

-spec feature_origins(map(), map()) -> map().
feature_origins(SavedFeatures, SavedPlugins) ->
    maps:from_list([
        {public_key(Key), feature_origin(Key, SavedFeatures, SavedPlugins)}
        || Key <- feature_names()
    ]).

-spec feature_origin(atom(), map(), map()) -> binary().
feature_origin(FeatureName, SavedFeatures, SavedPlugins) ->
    FeatureKey = public_key(FeatureName),
    case maps:is_key(FeatureKey, SavedFeatures) of
        true ->
            <<"feature_override">>;
        false ->
            case feature_plugin_owner(FeatureName) of
                undefined ->
                    <<"default">>;
                PluginName ->
                    case maps:is_key(public_key(PluginName), SavedPlugins) of
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
        {public_key(PluginName), plugin_origin(PluginName, SavedFeatures, SavedPlugins)}
        || PluginName <- imboy_plugin_registry:plugin_names()
    ]).

-spec plugin_origin(atom(), map(), map()) -> binary().
plugin_origin(PluginName, SavedFeatures, SavedPlugins) ->
    PluginKey = public_key(PluginName),
    case maps:is_key(PluginKey, SavedPlugins) of
        true ->
            <<"override">>;
        false ->
            FeatureKeys = maps:get(feature_keys, imboy_plugin_registry:manifest(PluginName), []),
            case lists:any(fun(FeatureKey) -> maps:is_key(public_key(FeatureKey), SavedFeatures) end, FeatureKeys) of
                true ->
                    <<"feature_overrides">>;
                false ->
                    <<"default">>
            end
    end.

-spec capability_adjustments(map(), map()) -> map().
capability_adjustments(SavedCapabilities, EffectiveCapabilities) ->
    lists:foldl(
        fun(Key, Acc) ->
            case capability_adjustment(Key, SavedCapabilities, EffectiveCapabilities) of
                {ok, Adjustment} ->
                    Acc#{Key => Adjustment};
                error ->
                    Acc
            end
        end,
        #{},
        [<<"message_search">>, <<"message_export">>, <<"audit_mode">>]
    ).

-spec capability_adjustment(binary(), map(), map()) -> {ok, map()} | error.
capability_adjustment(Key, SavedCapabilities, EffectiveCapabilities) ->
    case maps:find(Key, SavedCapabilities) of
        {ok, SavedValue} ->
            EffectiveValue = maps:get(Key, EffectiveCapabilities, SavedValue),
            case SavedValue =:= EffectiveValue of
                true ->
                    error;
                false ->
                    {ok,
                        #{
                            saved => SavedValue,
                            effective => EffectiveValue,
                            reason => constraint,
                            caused_by => capability_adjustment_caused_by(Key, EffectiveCapabilities)
                        }}
            end;
        error ->
            error
    end.

-spec capability_adjustment_caused_by(binary(), map()) -> map().
capability_adjustment_caused_by(<<"message_search">>, EffectiveCapabilities) ->
    constraint_cause_map(
        [{<<"storage_mode">>, <<"secure_e2ee">>}, {<<"e2ee_mode">>, <<"required">>}],
        EffectiveCapabilities
    );
capability_adjustment_caused_by(<<"message_export">>, EffectiveCapabilities) ->
    constraint_cause_map([{<<"storage_mode">>, <<"secure_e2ee">>}], EffectiveCapabilities);
capability_adjustment_caused_by(<<"audit_mode">>, EffectiveCapabilities) ->
    constraint_cause_map(
        [{<<"storage_mode">>, <<"secure_e2ee">>}, {<<"e2ee_mode">>, <<"required">>}],
        EffectiveCapabilities
    );
capability_adjustment_caused_by(_, _EffectiveCapabilities) ->
    #{}.

-spec constraint_cause_map([{binary(), term()}], map()) -> map().
constraint_cause_map(Candidates, EffectiveCapabilities) ->
    maps:from_list([
        {Key, ExpectedValue}
        || {Key, ExpectedValue} <- Candidates,
           maps:get(Key, EffectiveCapabilities, undefined) =:= ExpectedValue
    ]).

-spec plugin_adjustments(map(), map(), map()) -> map().
plugin_adjustments(SavedPlugins, EffectivePlugins, EffectiveCapabilities) ->
    maps:fold(
        fun(Key, SavedValue, Acc) ->
            case plugin_adjustment(Key, SavedValue, EffectivePlugins, EffectiveCapabilities) of
                {ok, Adjustment} ->
                    Acc#{Key => Adjustment};
                error ->
                    Acc
            end
        end,
        #{},
        SavedPlugins
    ).

-spec plugin_adjustment(binary(), term(), map(), map()) -> {ok, map()} | error.
plugin_adjustment(Key, SavedValue, EffectivePlugins, EffectiveCapabilities) ->
    EffectiveEnabled = plugin_enabled_in_public_map(Key, EffectivePlugins),
    case SavedValue =:= true andalso EffectiveEnabled =:= false of
        true ->
            case plugin_constraint_adjustment(Key, EffectivePlugins, EffectiveCapabilities) of
                {ok, Constraint} ->
                    {ok, Constraint#{
                        saved => SavedValue,
                        effective => EffectiveEnabled
                    }};
                error ->
                    error
            end;
        false ->
            error
    end.

-spec feature_adjustments(map(), map(), map(), map()) -> map().
feature_adjustments(SavedFeatures, EffectiveFeatures, EffectivePlugins, EffectiveCapabilities) ->
    maps:fold(
        fun(Key, SavedValue, Acc) ->
            case feature_adjustment(
                Key,
                SavedValue,
                EffectiveFeatures,
                EffectivePlugins,
                EffectiveCapabilities
            ) of
                {ok, Adjustment} ->
                    Acc#{Key => Adjustment};
                error ->
                    Acc
            end
        end,
        #{},
        SavedFeatures
    ).

-spec feature_adjustment(binary(), term(), map(), map(), map()) -> {ok, map()} | error.
feature_adjustment(Key, SavedValue, EffectiveFeatures, EffectivePlugins, EffectiveCapabilities) ->
    Dependencies = feature_dependencies_for_key(Key),
    EffectiveValue = maps:get(Key, EffectiveFeatures, SavedValue),
    case SavedValue =/= EffectiveValue andalso SavedValue =:= true andalso EffectiveValue =:= false of
        true when Dependencies =/= [] ->
            {ok,
                #{
                    saved => SavedValue,
                    effective => EffectiveValue,
                    reason => dependency,
                    depends_on => Dependencies
                }};
        true ->
            case feature_plugin_constraint_adjustment(Key, EffectivePlugins, EffectiveCapabilities) of
                {ok, Constraint} ->
                    {ok, Constraint#{
                        saved => SavedValue,
                        effective => EffectiveValue
                    }};
                error ->
                    error
            end;
        false ->
            error
    end.

-spec feature_plugin_constraint_adjustment(binary(), map(), map()) -> {ok, map()} | error.
feature_plugin_constraint_adjustment(Key, EffectivePlugins, EffectiveCapabilities) ->
    case feature_name_from_public_key(Key) of
        undefined ->
            error;
        FeatureName ->
            case feature_plugin_owner(FeatureName) of
                undefined ->
                    error;
                PluginName ->
                    plugin_constraint_adjustment(PluginName, EffectivePlugins, EffectiveCapabilities)
            end
    end.

-spec feature_dependencies_for_key(binary()) -> [binary()].
feature_dependencies_for_key(Key) ->
    try
        public_term(dependencies(binary_to_existing_atom(Key, utf8)))
    catch
        error:badarg ->
            []
    end.

-spec saved_view_from_values(term(), map(), map()) -> map().
saved_view_from_values(Profile0, CapabilityOverrides, FeatureOverrides0) ->
    {SavedPlugins, SavedFeatures} = compact_saved_plugin_overrides(FeatureOverrides0),
    Profile = normalize_saved_profile_value(Profile0),
    Sections0 = maybe_put_saved_section(#{}, profile, Profile),
    Sections1 = maybe_put_saved_section(Sections0, capabilities, CapabilityOverrides),
    Sections2 = maybe_put_saved_section(Sections1, plugins, SavedPlugins),
    public_term(maybe_put_saved_section(Sections2, features, SavedFeatures)).

-spec normalize_saved_profile_value(term()) -> community | enterprise | undefined.
normalize_saved_profile_value(undefined) ->
    undefined;
normalize_saved_profile_value(Value) ->
    case normalize_profile_input(Value) of
        {ok, Profile} ->
            Profile;
        error ->
            undefined
    end.

-spec resolve_profile(term()) -> community | enterprise.
resolve_profile(ProfileConfig) ->
    case normalize_profile_input(ProfileConfig) of
        {ok, Profile} ->
            Profile;
        error ->
            imboy_profile_preset:current()
    end.

-spec effective_capabilities_for_profile(community | enterprise, term()) -> map().
effective_capabilities_for_profile(Profile, CapabilityConfig) ->
    Defaults = normalize_capability_map(
        maps:get(capabilities, imboy_profile_preset:defaults(Profile), #{})
    ),
    Overrides = normalize_capability_map(CapabilityConfig),
    normalize_capabilities(maps:merge(Defaults, Overrides), Defaults).

-spec effective_policy_components(community | enterprise, term(), term()) -> {map(), map(), map()}.
effective_policy_components(Profile, CapabilityConfig, FeatureConfig) ->
    Capabilities = effective_capabilities_for_profile(Profile, CapabilityConfig),
    BaseFeatures = effective_features_for_profile(Profile, FeatureConfig),
    {Features, Plugins} = resolve_plugin_constraints(BaseFeatures, Capabilities),
    {Capabilities, Features, Plugins}.

-spec effective_features_from_config(term()) -> map().
effective_features_from_config(FeatureConfig) ->
    effective_features_from_switches(normalize_feature_switches(FeatureConfig)).

-spec effective_features_for_profile(community | enterprise, term()) -> map().
effective_features_for_profile(Profile, FeatureConfig) ->
    Defaults = normalize_feature_switches(
        maps:get(features, imboy_profile_preset:defaults(Profile), #{})
    ),
    Overrides = normalize_feature_switches(FeatureConfig),
    effective_features_from_config(maps:merge(Defaults, Overrides)).

-spec effective_features_from_switches(map()) -> map().
effective_features_from_switches(Features) ->
    maps:from_list([
        {Name, feature_enabled(Name, Features)}
        || Name <- feature_names()
    ]).

-spec normalize_feature_switches(term()) -> map().
normalize_feature_switches(Features) ->
    lists:foldl(
        fun(FeatureName, Acc) ->
            case lookup_feature_switch(Features, FeatureName) of
                undefined ->
                    Acc;
                Value ->
                    maps:put(FeatureName, Value, Acc)
            end
        end,
        #{},
        feature_names()
    ).

-spec resolve_plugin_constraints(map(), map()) -> {map(), map()}.
resolve_plugin_constraints(Features, Capabilities) ->
    Plugins = effective_plugins(Features),
    DisabledFeatureKeys = plugin_constrained_feature_keys(Plugins, Capabilities),
    ForcedFeatures = disable_feature_keys(Features, DisabledFeatureKeys),
    case ForcedFeatures =:= Features of
        true ->
            {Features, Plugins};
        false ->
            resolve_plugin_constraints(ForcedFeatures, Capabilities)
    end.

-spec plugin_constrained_feature_keys(map(), map()) -> [atom()].
plugin_constrained_feature_keys(Plugins, Capabilities) ->
    lists:usort(lists:append([
        maps:get(feature_keys, Manifest, [])
        || {_PluginName, Manifest} <- maps:to_list(Plugins),
           maps:get(enabled, Manifest, false),
           plugin_constraint_violation_native(Manifest, Plugins, Capabilities) =/= none
    ])).

-spec disable_feature_keys(map(), [atom()]) -> map().
disable_feature_keys(Features, FeatureKeys) ->
    lists:foldl(
        fun(FeatureKey, Acc) ->
            maps:put(FeatureKey, false, Acc)
        end,
        Features,
        FeatureKeys
    ).

-spec plugin_constraint_violation_native(map(), map(), map()) ->
    none | {dependency, [term()]} | {capability_constraint, map()}.
plugin_constraint_violation_native(Manifest, Plugins, Capabilities) ->
    case unsatisfied_plugin_dependencies_native(
        maps:get(depends_on_plugins, Manifest, []),
        Plugins
    ) of
        [] ->
            case unmet_capability_requirements_native(
                maps:get(requires_capabilities, Manifest, []),
                Capabilities
            ) of
                Requirements when map_size(Requirements) =:= 0 ->
                    none;
                Requirements ->
                    {capability_constraint, Requirements}
            end;
        Dependencies ->
            {dependency, Dependencies}
    end.

-spec unsatisfied_plugin_dependencies_native(term(), map()) -> [term()].
unsatisfied_plugin_dependencies_native(Dependencies, Plugins) ->
    [
        Dependency
        || Dependency <- normalize_dependency_list(Dependencies),
           not plugin_enabled_in_native_map(Dependency, Plugins)
    ].

-spec unmet_capability_requirements_native(term(), map()) -> map().
unmet_capability_requirements_native(Requirements, Capabilities) ->
    maps:from_list([
        {Key, Expected}
        || {Key, Expected} <- normalize_required_capabilities(Requirements),
           not capability_requirement_met(Expected, native_capability_value(Key, Capabilities))
    ]).

-spec normalize_dependency_list(term()) -> [term()].
normalize_dependency_list(Dependencies) when is_list(Dependencies) ->
    Dependencies;
normalize_dependency_list(_) ->
    [].

-spec normalize_required_capabilities(term()) -> [{term(), term()}].
normalize_required_capabilities(Requirements) when is_map(Requirements) ->
    maps:to_list(Requirements);
normalize_required_capabilities(Requirements) when is_list(Requirements) ->
    lists:foldl(
        fun(Item, Acc) ->
            case Item of
                {Key, Expected} ->
                    [{Key, Expected} | Acc];
                Key ->
                    [{Key, true} | Acc]
            end
        end,
        [],
        Requirements
    );
normalize_required_capabilities(_) ->
    [].

-spec plugin_enabled_in_native_map(term(), map()) -> boolean().
plugin_enabled_in_native_map(PluginRef, Plugins) ->
    case maps:find(normalize_plugin_ref(PluginRef), Plugins) of
        {ok, Manifest} ->
            maps:get(enabled, Manifest, false);
        error ->
            false
    end.

-spec native_capability_value(term(), map()) -> term().
native_capability_value(Key, Capabilities) ->
    maps:get(normalize_capability_ref(Key), Capabilities, undefined).

-spec capability_requirement_met(term(), term()) -> boolean().
capability_requirement_met(Expected, Actual) when is_list(Expected) ->
    case is_charlist(Expected) of
        true ->
            capability_requirement_met(unicode:characters_to_binary(Expected), Actual);
        false ->
            lists:any(fun(Option) -> capability_requirement_met(Option, Actual) end, Expected)
    end;
capability_requirement_met(true, Actual) ->
    capability_truthy(Actual);
capability_requirement_met(false, Actual) ->
    Actual =:= false;
capability_requirement_met(Expected, Actual) ->
    normalize_requirement_value(Expected) =:= normalize_requirement_value(Actual).

-spec capability_truthy(term()) -> boolean().
capability_truthy(undefined) ->
    false;
capability_truthy(null) ->
    false;
capability_truthy(false) ->
    false;
capability_truthy(0) ->
    false;
capability_truthy(<<"false">>) ->
    false;
capability_truthy("false") ->
    false;
capability_truthy(_) ->
    true.

-spec normalize_requirement_value(term()) -> term().
normalize_requirement_value(Value) when is_list(Value) ->
    case is_charlist(Value) of
        true ->
            unicode:characters_to_binary(Value);
        false ->
            [normalize_requirement_value(Item) || Item <- Value]
    end;
normalize_requirement_value(Value) ->
    Value.

-spec normalize_plugin_ref(term()) -> atom() | undefined.
normalize_plugin_ref(Key) when is_atom(Key) ->
    Key;
normalize_plugin_ref(Key) when is_binary(Key) ->
    try
        binary_to_existing_atom(Key, utf8)
    catch
        error:badarg ->
            undefined
    end;
normalize_plugin_ref(Key) when is_list(Key) ->
    normalize_plugin_ref(unicode:characters_to_binary(Key));
normalize_plugin_ref(_) ->
    undefined.

-spec normalize_capability_ref(term()) -> atom() | undefined.
normalize_capability_ref(Key) when is_atom(Key) ->
    Key;
normalize_capability_ref(Key) when is_binary(Key) ->
    try
        binary_to_existing_atom(Key, utf8)
    catch
        error:badarg ->
            undefined
    end;
normalize_capability_ref(Key) when is_list(Key) ->
    normalize_capability_ref(unicode:characters_to_binary(Key));
normalize_capability_ref(_) ->
    undefined.

-spec plugin_constraint_adjustment(term(), map(), map()) -> {ok, map()} | error.
plugin_constraint_adjustment(PluginRef, EffectivePlugins, EffectiveCapabilities) ->
    case plugin_manifest_by_public_ref(PluginRef) of
        undefined ->
            error;
        Manifest ->
            case unsatisfied_plugin_dependencies_public(
                maps:get(depends_on_plugins, Manifest, []),
                EffectivePlugins
            ) of
                [] ->
                    case unmet_capability_requirements_public(
                        maps:get(requires_capabilities, Manifest, []),
                        EffectiveCapabilities
                    ) of
                        Requirements when map_size(Requirements) =:= 0 ->
                            error;
                        Requirements ->
                            {ok, #{
                                reason => capability_constraint,
                                requires_capabilities => Requirements
                            }}
                    end;
                Dependencies ->
                    {ok, #{
                        reason => dependency,
                        depends_on_plugins => Dependencies
                    }}
            end
    end.

-spec unsatisfied_plugin_dependencies_public(term(), map()) -> [term()].
unsatisfied_plugin_dependencies_public(Dependencies, EffectivePlugins) ->
    [
        Dependency
        || Dependency <- normalize_dependency_list(Dependencies),
           not plugin_enabled_in_public_map(Dependency, EffectivePlugins)
    ].

-spec unmet_capability_requirements_public(term(), map()) -> map().
unmet_capability_requirements_public(Requirements, EffectiveCapabilities) ->
    maps:from_list([
        {Key, Expected}
        || {Key, Expected} <- normalize_required_capabilities(Requirements),
           not capability_requirement_met(Expected, maps:get(public_key(Key), EffectiveCapabilities, undefined))
    ]).

-spec plugin_enabled_in_public_map(term(), map()) -> boolean().
plugin_enabled_in_public_map(PluginRef, EffectivePlugins) ->
    case maps:find(public_key(PluginRef), EffectivePlugins) of
        {ok, PluginState} when is_map(PluginState) ->
            to_boolean(
                maps:get(<<"enabled">>, PluginState, maps:get(enabled, PluginState, false)),
                false
            );
        _ ->
            false
    end.

-spec plugin_manifest_by_public_ref(term()) -> map() | undefined.
plugin_manifest_by_public_ref(PluginRef) ->
    case normalize_plugin_ref(PluginRef) of
        undefined ->
            undefined;
        PluginName ->
            imboy_plugin_registry:manifest(PluginName)
    end.

-spec feature_name_from_public_key(binary()) -> atom() | undefined.
feature_name_from_public_key(Key) ->
    case binary_to_atom_or_undefined(Key) of
        undefined ->
            undefined;
        FeatureName ->
            case lists:member(FeatureName, feature_names()) of
                true ->
                    FeatureName;
                false ->
                    undefined
            end
    end.

-spec binary_to_atom_or_undefined(binary()) -> atom() | undefined.
binary_to_atom_or_undefined(Key) ->
    try
        binary_to_existing_atom(Key, utf8)
    catch
        error:badarg ->
            undefined
    end.

-spec normalize_preview_capability_overrides(term()) -> map().
normalize_preview_capability_overrides(Value) ->
    case normalize_capability_payload(Value) of
        {ok, Capabilities} ->
            Capabilities;
        {error, _} ->
            #{}
    end.

-spec normalize_preview_feature_overrides(term()) -> map().
normalize_preview_feature_overrides(Value) ->
    case normalize_feature_payload(Value) of
        {ok, FeatureConfig} ->
            flatten_saved_feature_config(FeatureConfig);
        {error, _} ->
            #{}
    end.

-spec feature_names() -> [atom()].
feature_names() ->
    [
        core,
        e2ee,
        channel,
        location,
        moment,
        channel_discover,
        channel_invitation,
        channel_order,
        group_vote,
        group_schedule,
        group_task
    ].

-spec feature_enabled(atom(), term()) -> boolean().
feature_enabled(FeatureName, Features) ->
    CurrentEnabled = switch_enabled(lookup_feature_switch(Features, FeatureName)),
    DependencyEnabled = lists:all(
        fun(Dependency) ->
            switch_enabled(lookup_feature_switch(Features, Dependency))
        end,
        dependencies(FeatureName)
    ),
    CurrentEnabled andalso DependencyEnabled.

-spec dependencies(atom()) -> [atom()].
dependencies(channel_discover) ->
    [channel];
dependencies(channel_invitation) ->
    [channel];
dependencies(channel_order) ->
    [channel];
dependencies(_) ->
    [].

-spec lookup_feature_switch(term(), atom()) -> term().
lookup_feature_switch(Features, FeatureName) when is_map(Features) ->
    find_in_map(Features, candidate_keys(FeatureName));
lookup_feature_switch(Features, FeatureName) when is_list(Features) ->
    find_in_proplist(Features, candidate_keys(FeatureName));
lookup_feature_switch(_, _) ->
    undefined.

-spec find_in_map(map(), [term()]) -> term().
find_in_map(_Features, []) ->
    undefined;
find_in_map(Features, [Key | Rest]) ->
    case maps:find(Key, Features) of
        {ok, Value} ->
            Value;
        error ->
            find_in_map(Features, Rest)
    end.

-spec find_in_proplist(list(), [term()]) -> term().
find_in_proplist(_Features, []) ->
    undefined;
find_in_proplist(Features, [Key | Rest]) ->
    case proplists:get_value(Key, Features, undefined) of
        undefined ->
            find_in_proplist(Features, Rest);
        Value ->
            Value
    end.

-spec switch_enabled(term()) -> boolean().
switch_enabled(undefined) ->
    true;
switch_enabled(true) ->
    true;
switch_enabled(false) ->
    false;
switch_enabled(#{enabled := Enabled}) ->
    to_boolean(Enabled, true);
switch_enabled(#{<<"enabled">> := Enabled}) ->
    to_boolean(Enabled, true);
switch_enabled(Options) when is_list(Options) ->
    case proplists:get_value(enabled, Options, undefined) of
        undefined ->
            case proplists:get_value(<<"enabled">>, Options, undefined) of
                undefined ->
                    true;
                Enabled ->
                    to_boolean(Enabled, true)
            end;
        Enabled ->
            to_boolean(Enabled, true)
    end;
switch_enabled(Value) ->
    to_boolean(Value, true).

-spec candidate_keys(atom()) -> [term()].
candidate_keys(FeatureName) ->
    Binary = atom_to_binary(FeatureName, utf8),
    [FeatureName, Binary, binary_to_list(Binary)].

-spec to_boolean(term(), boolean()) -> boolean().
to_boolean(true, _Default) ->
    true;
to_boolean(false, _Default) ->
    false;
to_boolean(1, _Default) ->
    true;
to_boolean(0, _Default) ->
    false;
to_boolean(<<"true">>, _Default) ->
    true;
to_boolean(<<"false">>, _Default) ->
    false;
to_boolean("true", _Default) ->
    true;
to_boolean("false", _Default) ->
    false;
to_boolean(undefined, Default) ->
    Default;
to_boolean(_, Default) ->
    Default.

-spec normalize_map(term()) -> map().
normalize_map(undefined) ->
    #{};
normalize_map(Value) when is_map(Value) ->
    Value;
normalize_map(Value) when is_list(Value) ->
    maps:from_list(Value);
normalize_map(_) ->
    #{}.

-spec load_profile_config() -> term().
load_profile_config() ->
    load_config_value(?PRODUCT_PROFILE_CONFIG_KEY, config_ds:env(product_profile, community)).

-spec load_capability_config() -> term().
load_capability_config() ->
    load_config_value(?CAPABILITIES_CONFIG_KEY, config_ds:env(capabilities, #{})).

-spec load_feature_config() -> term().
load_feature_config() ->
    load_config_value(?FEATURES_CONFIG_KEY, config_ds:env(features, undefined)).

-spec load_config_value(binary(), term()) -> term().
load_config_value(Key, Default) ->
    case catch config_ds:get(Key, Default) of
        {'EXIT', _} ->
            Default;
        null ->
            Default;
        undefined ->
            Default;
        Value ->
            Value
    end.

-spec load_saved_config_value(binary()) -> term().
load_saved_config_value(Key) ->
    load_config_value(Key, #{}).

-spec saved_profile_override() -> community | enterprise | undefined.
saved_profile_override() ->
    case normalize_profile_input(load_config_value(?PRODUCT_PROFILE_CONFIG_KEY, undefined)) of
        {ok, Profile} ->
            Profile;
        error ->
            undefined
    end.

-spec saved_capability_overrides() -> map().
saved_capability_overrides() ->
    case normalize_capability_payload(load_saved_config_value(?CAPABILITIES_CONFIG_KEY)) of
        {ok, Capabilities} ->
            Capabilities;
        {error, _} ->
            #{}
    end.

-spec saved_feature_overrides() -> map().
saved_feature_overrides() ->
    case normalize_feature_payload(load_saved_config_value(?FEATURES_CONFIG_KEY)) of
        {ok, FeatureConfig} ->
            flatten_saved_feature_config(FeatureConfig);
        {error, _} ->
            #{}
    end.

-spec flatten_saved_feature_config(map()) -> map().
flatten_saved_feature_config(FeatureConfig) ->
    maps:from_list([
        {Key, maps:get(enabled, Toggle, false)}
        || {Key, Toggle} <- maps:to_list(FeatureConfig)
    ]).

-spec compact_saved_plugin_overrides(map()) -> {map(), map()}.
compact_saved_plugin_overrides(FeatureOverrides0) ->
    lists:foldl(
        fun(PluginName, {PluginsAcc, FeatureAcc}) ->
            case plugin_override_candidate(PluginName, FeatureAcc) of
                {ok, Enabled, FeatureKeys} ->
                    {
                        maps:put(PluginName, Enabled, PluginsAcc),
                        maps:without(FeatureKeys, FeatureAcc)
                    };
                error ->
                    {PluginsAcc, FeatureAcc}
            end
        end,
        {#{}, FeatureOverrides0},
        imboy_plugin_registry:plugin_names()
    ).

-spec plugin_override_candidate(atom(), map()) -> {ok, boolean(), [atom()]} | error.
plugin_override_candidate(PluginName, FeatureOverrides) ->
    FeatureKeys = maps:get(feature_keys, imboy_plugin_registry:manifest(PluginName), []),
    Values = [maps:get(Key, FeatureOverrides, '$missing') || Key <- FeatureKeys],
    case Values of
        [] ->
            error;
        _ ->
            case lists:any(fun(Value) -> Value =:= '$missing' end, Values) of
                true ->
                    error;
                false ->
                    case lists:usort(Values) of
                        [Enabled] when is_boolean(Enabled) ->
                            {ok, Enabled, FeatureKeys};
                        _ ->
                            error
                    end
            end
    end.

-spec maybe_put_saved_section(map(), atom(), term()) -> map().
maybe_put_saved_section(Sections, _Key, undefined) ->
    Sections;
maybe_put_saved_section(Sections, _Key, Value) when is_map(Value), map_size(Value) =:= 0 ->
    Sections;
maybe_put_saved_section(Sections, Key, Value) ->
    Sections#{Key => Value}.

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
        all => feature_names(),
        plugin_managed => PluginManaged,
        standalone => feature_names() -- PluginManaged,
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
    maps:from_list([
        {FeatureName, dependencies(FeatureName)}
        || FeatureName <- feature_names(),
           length(dependencies(FeatureName)) > 0
    ]).

-spec feature_field_catalog() -> map().
feature_field_catalog() ->
    maps:from_list([
        {FeatureName, feature_field_meta(FeatureName)}
        || FeatureName <- feature_names()
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
            public_plugin_manifest(Manifest)
        end,
        imboy_plugin_registry:manifests()
    ).

-spec editor_order_catalog() -> map().
editor_order_catalog() ->
    #{
        sections => [profile, capabilities, plugins, features],
        profiles => imboy_profile_preset:supported_profiles(),
        capabilities => capability_names(),
        features => feature_names(),
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
            fields => feature_names(),
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

-spec normalize_capability_map(term()) -> map().
normalize_capability_map(Value) ->
    Map0 = normalize_map(Value),
    lists:foldl(
        fun(Key, Acc) ->
            case find_in_map(Map0, candidate_keys(Key)) of
                undefined ->
                    Acc;
                KeyValue ->
                    maps:put(Key, KeyValue, Acc)
            end
        end,
        #{},
        capability_names()
    ).

-spec normalize_capabilities(map(), map()) -> map().
normalize_capabilities(Capabilities0, Defaults) ->
    StorageMode = normalize_storage_mode(
        capability_value(Capabilities0, storage_mode, maps:get(storage_mode, Defaults, archived)),
        maps:get(storage_mode, Defaults, archived)
    ),
    E2eeMode = normalize_e2ee_mode(
        capability_value(Capabilities0, e2ee_mode, maps:get(e2ee_mode, Defaults, disabled)),
        maps:get(e2ee_mode, Defaults, disabled)
    ),
    MessageSearch = to_boolean(
        capability_value(Capabilities0, message_search, maps:get(message_search, Defaults, false)),
        maps:get(message_search, Defaults, false)
    ),
    MessageExport = to_boolean(
        capability_value(Capabilities0, message_export, maps:get(message_export, Defaults, false)),
        maps:get(message_export, Defaults, false)
    ),
    AuditMode = normalize_audit_mode(
        capability_value(Capabilities0, audit_mode, maps:get(audit_mode, Defaults, none)),
        maps:get(audit_mode, Defaults, none)
    ),
    RetentionPolicy = normalize_retention_policy(
        capability_value(Capabilities0, retention_policy, maps:get(retention_policy, Defaults, #{})),
        maps:get(retention_policy, Defaults, #{})
    ),
    Normalized0 = Capabilities0#{
        storage_mode => StorageMode,
        e2ee_mode => E2eeMode,
        message_search => MessageSearch,
        message_export => MessageExport,
        audit_mode => AuditMode,
        retention_policy => RetentionPolicy
    },
    enforce_capability_constraints(Normalized0).

-spec capability_value(map(), atom(), term()) -> term().
capability_value(Map, Key, Default) ->
    case find_in_map(Map, candidate_keys(Key)) of
        undefined ->
            Default;
        Value ->
            Value
    end.

-spec enforce_capability_constraints(map()) -> map().
enforce_capability_constraints(Capabilities0) ->
    StorageMode = maps:get(storage_mode, Capabilities0, archived),
    E2eeMode = maps:get(e2ee_mode, Capabilities0, disabled),
    MessageSearch =
        case StorageMode =:= secure_e2ee orelse E2eeMode =:= required of
            true ->
                false;
            false ->
                maps:get(message_search, Capabilities0, false)
        end,
    MessageExport =
        case StorageMode of
            secure_e2ee ->
                false;
            _ ->
                maps:get(message_export, Capabilities0, false)
        end,
    AuditMode0 = maps:get(audit_mode, Capabilities0, none),
    AuditMode =
        case body_visibility_allowed(StorageMode, E2eeMode) of
            true ->
                AuditMode0;
            false when AuditMode0 =:= full ->
                metadata;
            false ->
                AuditMode0
        end,
    Capabilities0#{
        message_search => MessageSearch,
        message_export => MessageExport,
        audit_mode => AuditMode
    }.

-spec body_visibility_allowed(atom(), atom()) -> boolean().
body_visibility_allowed(secure_e2ee, _E2eeMode) ->
    false;
body_visibility_allowed(_StorageMode, required) ->
    false;
body_visibility_allowed(compliance_e2ee, _E2eeMode) ->
    true;  % compliance 模式下合规密钥持有者可查看
body_visibility_allowed(_StorageMode, compliance) ->
    true;  % compliance 模式下合规密钥持有者可查看
body_visibility_allowed(_, _) ->
    true.

-spec normalize_storage_mode(term(), atom()) -> archived | compliance_e2ee | secure_e2ee.
normalize_storage_mode(archived, _Default) ->
    archived;
normalize_storage_mode(compliance_e2ee, _Default) ->
    compliance_e2ee;
normalize_storage_mode(secure_e2ee, _Default) ->
    secure_e2ee;
normalize_storage_mode(<<"archived">>, _Default) ->
    archived;
normalize_storage_mode(<<"compliance_e2ee">>, _Default) ->
    compliance_e2ee;
normalize_storage_mode(<<"secure_e2ee">>, _Default) ->
    secure_e2ee;
normalize_storage_mode("archived", _Default) ->
    archived;
normalize_storage_mode("compliance_e2ee", _Default) ->
    compliance_e2ee;
normalize_storage_mode("secure_e2ee", _Default) ->
    secure_e2ee;
normalize_storage_mode(_, Default) ->
    Default.

-spec normalize_e2ee_mode(term(), atom()) -> disabled | optional | compliance | required.
normalize_e2ee_mode(disabled, _Default) ->
    disabled;
normalize_e2ee_mode(optional, _Default) ->
    optional;
normalize_e2ee_mode(compliance, _Default) ->
    compliance;
normalize_e2ee_mode(required, _Default) ->
    required;
normalize_e2ee_mode(<<"disabled">>, _Default) ->
    disabled;
normalize_e2ee_mode(<<"optional">>, _Default) ->
    optional;
normalize_e2ee_mode(<<"compliance">>, _Default) ->
    compliance;
normalize_e2ee_mode(<<"required">>, _Default) ->
    required;
normalize_e2ee_mode("disabled", _Default) ->
    disabled;
normalize_e2ee_mode("optional", _Default) ->
    optional;
normalize_e2ee_mode("compliance", _Default) ->
    compliance;
normalize_e2ee_mode("required", _Default) ->
    required;
normalize_e2ee_mode(_, Default) ->
    Default.

-spec normalize_audit_mode(term(), atom()) -> none | metadata | full.
normalize_audit_mode(none, _Default) ->
    none;
normalize_audit_mode(metadata, _Default) ->
    metadata;
normalize_audit_mode(full, _Default) ->
    full;
normalize_audit_mode(<<"none">>, _Default) ->
    none;
normalize_audit_mode(<<"metadata">>, _Default) ->
    metadata;
normalize_audit_mode(<<"full">>, _Default) ->
    full;
normalize_audit_mode("none", _Default) ->
    none;
normalize_audit_mode("metadata", _Default) ->
    metadata;
normalize_audit_mode("full", _Default) ->
    full;
normalize_audit_mode(_, Default) ->
    Default.

-spec normalize_retention_policy(term(), map()) -> map().
normalize_retention_policy(Value, Default) ->
    Policy = normalize_map(Value),
    case maps:size(Policy) of
        0 ->
            Default;
        _ ->
            Policy
    end.

-spec normalize_config_sections(map()) -> map().
normalize_config_sections(Payload) ->
    Sections0 = #{},
    Sections1 = maybe_put_profile_section(Sections0, Payload),
    Sections2 = maybe_put_capabilities_section(Sections1, Payload),
    maybe_put_features_section(Sections2, Payload).

-spec maybe_put_profile_section(map(), map()) -> map().
maybe_put_profile_section(Sections, Payload) ->
    case payload_value(Payload, [profile, <<"profile">>, product_profile, <<"product_profile">>]) of
        {ok, null} ->
            Sections#{?PRODUCT_PROFILE_CONFIG_KEY => ?DELETE_VALUE};
        {ok, Value} ->
            case normalize_profile_input(Value) of
                {ok, Profile} ->
                    Sections#{?PRODUCT_PROFILE_CONFIG_KEY => atom_to_binary(Profile, utf8)};
                error ->
                    Sections#{
                        profile_error => policy_error_detail(
                            profile, profile, invalid_profile, <<"invalid profile value">>)
                    }
            end;
        error ->
            Sections
    end.

-spec maybe_put_capabilities_section(map(), map()) -> map().
maybe_put_capabilities_section(Sections, Payload) ->
    case payload_value(Payload, [capabilities, <<"capabilities">>]) of
        {ok, Value} ->
            case normalize_capability_payload(Value) of
                {ok, CapabilityConfig} ->
                    Sections#{?CAPABILITIES_CONFIG_KEY => public_term(CapabilityConfig)};
                {error, Detail} ->
                    Sections#{capabilities_error => Detail}
            end;
        error ->
            Sections
    end.

-spec maybe_put_features_section(map(), map()) -> map().
maybe_put_features_section(Sections, Payload) ->
    ExistingFeatureOverrides = saved_feature_overrides(),
    FeaturesResult =
        case payload_value(Payload, [features, <<"features">>]) of
            {ok, FeatureValue} ->
                normalize_feature_payload(FeatureValue);
            error ->
                {ok, #{}}
        end,
    PluginsResult =
        case payload_value(Payload, [plugins, <<"plugins">>]) of
            {ok, PluginValue} ->
                normalize_plugin_payload(PluginValue, ExistingFeatureOverrides);
            error ->
                {ok, #{}}
        end,
    case {FeaturesResult, PluginsResult} of
        {{error, Detail}, _} ->
            Sections#{features_error => Detail};
        {_, {error, Detail}} ->
            Sections#{features_error => Detail};
        {{ok, FeatureConfig}, {ok, PluginFeatureConfig}} ->
            MergedFeatureConfig = maps:merge(PluginFeatureConfig, FeatureConfig),
            case map_size(MergedFeatureConfig) of
                0 ->
                    Sections;
                _ ->
                    Sections#{?FEATURES_CONFIG_KEY => public_term(MergedFeatureConfig)}
            end
    end.

 -spec validate_save_sections(map()) -> {ok, map()} | {error, binary(), map()}.
validate_save_sections(Sections) ->
    ErrorKeys = [profile_error, capabilities_error, features_error],
    case [maps:get(Key, Sections) || Key <- ErrorKeys, maps:is_key(Key, Sections)] of
        [Detail | _] ->
            {error, policy_error_message(Detail), public_policy_error_detail(Detail)};
        [] ->
            {ok, maps:without(ErrorKeys, Sections)}
    end.

-spec payload_value(map(), [term()]) -> {ok, term()} | error.
payload_value(_Payload, []) ->
    error;
payload_value(Payload, [Key | Rest]) ->
    case maps:find(Key, Payload) of
        {ok, Value} ->
            {ok, Value};
        error ->
            payload_value(Payload, Rest)
    end.

-spec normalize_profile_input(term()) -> {ok, community | enterprise} | error.
normalize_profile_input(community) ->
    {ok, community};
normalize_profile_input(enterprise) ->
    {ok, enterprise};
normalize_profile_input(<<"community">>) ->
    {ok, community};
normalize_profile_input(<<"enterprise">>) ->
    {ok, enterprise};
normalize_profile_input("community") ->
    {ok, community};
normalize_profile_input("enterprise") ->
    {ok, enterprise};
normalize_profile_input(_) ->
    error.

-spec normalize_capability_payload(term()) -> {ok, map()} | {error, map()}.
normalize_capability_payload(Value) ->
    Map0 = normalize_map(Value),
    Result = lists:foldl(
        fun(Key, {ok, Acc}) ->
            case find_in_map(Map0, candidate_keys(Key)) of
                undefined ->
                    {ok, Acc};
                null ->
                    {ok, maps:put(Key, ?DELETE_VALUE, Acc)};
                Item ->
                    case normalize_capability_payload_value(Key, Item) of
                        {ok, NormalizedValue} ->
                            {ok, maps:put(Key, NormalizedValue, Acc)};
                        {error, _} = Error ->
                            Error
                    end
            end;
           (_Key, {error, _} = Error) ->
                Error
        end,
        {ok, #{}},
        capability_names()
    ),
    case Result of
        {error, _} = Error ->
            Error;
        {ok, Capabilities} ->
            case {map_size(Map0), map_size(Capabilities)} of
                {0, 0} ->
                    {ok, #{}};
                {_, 0} ->
                    {error,
                        policy_error_detail(
                            capabilities, undefined, invalid_payload, <<"invalid capabilities payload">>)};
                _ ->
                    {ok, Capabilities}
            end
    end.

-spec normalize_capability_payload_value(atom(), term()) -> {ok, term()} | {error, map()}.
normalize_capability_payload_value(storage_mode, Value) ->
    case parse_storage_mode(Value) of
        {ok, StorageMode} ->
            {ok, StorageMode};
        error ->
            {error,
                policy_error_detail(
                    capabilities, storage_mode, invalid_enum, <<"invalid storage_mode value">>)}
    end;
normalize_capability_payload_value(e2ee_mode, Value) ->
    case parse_e2ee_mode(Value) of
        {ok, E2eeMode} ->
            {ok, E2eeMode};
        error ->
            {error,
                policy_error_detail(capabilities, e2ee_mode, invalid_enum, <<"invalid e2ee_mode value">>)}
    end;
normalize_capability_payload_value(message_search, Value) ->
    case parse_toggle_payload(Value) of
        {ok, Enabled} ->
            {ok, Enabled};
        error ->
            {error,
                policy_error_detail(
                    capabilities, message_search, invalid_boolean, <<"invalid message_search value">>)}
    end;
normalize_capability_payload_value(message_export, Value) ->
    case parse_toggle_payload(Value) of
        {ok, Enabled} ->
            {ok, Enabled};
        error ->
            {error,
                policy_error_detail(
                    capabilities, message_export, invalid_boolean, <<"invalid message_export value">>)}
    end;
normalize_capability_payload_value(audit_mode, Value) ->
    case parse_audit_mode(Value) of
        {ok, AuditMode} ->
            {ok, AuditMode};
        error ->
            {error,
                policy_error_detail(capabilities, audit_mode, invalid_enum, <<"invalid audit_mode value">>)}
    end;
normalize_capability_payload_value(retention_policy, Value) ->
    case normalize_retention_policy_payload(Value) of
        {ok, Policy} ->
            {ok, Policy};
        {error, _} = Error ->
            Error
    end;
normalize_capability_payload_value(_Key, Value) ->
    {ok, Value}.

-spec normalize_feature_payload(term()) -> {ok, map()} | {error, map()}.
normalize_feature_payload(Value) ->
    Map0 = normalize_map(Value),
    Result = lists:foldl(
        fun(Key, {ok, Acc}) ->
            case find_in_map(Map0, candidate_keys(Key)) of
                undefined ->
                    {ok, Acc};
                null ->
                    {ok, maps:put(Key, ?DELETE_VALUE, Acc)};
                Item ->
                    case parse_toggle_payload(Item) of
                        {ok, Enabled} ->
                            {ok, maps:put(Key, #{enabled => Enabled}, Acc)};
                        error ->
                            {error,
                                policy_error_detail(
                                    features, Key, invalid_boolean, <<"invalid features payload">>)}
                    end
            end;
           (_Key, {error, _} = Error) ->
                Error
        end,
        {ok, #{}},
        feature_names()
    ),
    case Result of
        {error, _} = Error ->
            Error;
        {ok, Features} ->
            case {map_size(Map0), map_size(Features)} of
                {0, 0} ->
                    {ok, #{}};
                {_, 0} ->
                    {error,
                        policy_error_detail(
                            features, undefined, invalid_payload, <<"invalid features payload">>)};
                _ ->
                    {ok, Features}
            end
    end.

-spec normalize_plugin_payload(term(), map()) -> {ok, map()} | {error, map()}.
normalize_plugin_payload(Value, ExistingFeatureOverrides) ->
    Map0 = normalize_map(Value),
    Result = lists:foldl(
        fun(PluginName, {ok, Acc}) ->
            case find_in_map(Map0, candidate_keys(PluginName)) of
                undefined ->
                    {ok, Acc};
                null ->
                    {ok,
                        maps:merge(
                            Acc,
                            plugin_clear_payload(PluginName, ExistingFeatureOverrides)
                        )};
                Item ->
                    case parse_toggle_payload(Item) of
                        {ok, Enabled} ->
                            Manifest = imboy_plugin_registry:manifest(PluginName),
                            FeatureKeys = maps:get(feature_keys, Manifest, []),
                        {ok,
                                lists:foldl(
                                    fun(FeatureKey, FeatureAcc) ->
                                        maps:put(FeatureKey, #{enabled => Enabled}, FeatureAcc)
                                    end,
                                    Acc,
                                    FeatureKeys
                                )};
                        error ->
                            {error,
                                policy_error_detail(
                                    plugins, PluginName, invalid_boolean, <<"invalid plugins payload">>)}
                    end
            end;
           (_PluginName, {error, _} = Error) ->
                Error
        end,
        {ok, #{}},
        imboy_plugin_registry:plugin_names()
    ),
    case Result of
        {error, _} = Error ->
            Error;
        {ok, FeatureConfig} ->
            case {map_size(Map0), map_size(FeatureConfig)} of
                {0, 0} ->
                    {ok, #{}};
                {_, 0} ->
                    {error,
                        policy_error_detail(
                            plugins, undefined, invalid_payload, <<"invalid plugins payload">>)};
                _ ->
                    {ok, FeatureConfig}
            end
    end.

-spec plugin_clear_payload(atom(), map()) -> map().
plugin_clear_payload(PluginName, ExistingFeatureOverrides) ->
    case plugin_override_candidate(PluginName, ExistingFeatureOverrides) of
        {ok, _Enabled, FeatureKeys} ->
            lists:foldl(
                fun(FeatureKey, Acc) ->
                    maps:put(FeatureKey, ?DELETE_VALUE, Acc)
                end,
                #{},
                FeatureKeys
            );
        error ->
            preserve_plugin_feature_overrides(PluginName, ExistingFeatureOverrides)
    end.

-spec preserve_plugin_feature_overrides(atom(), map()) -> map().
preserve_plugin_feature_overrides(PluginName, ExistingFeatureOverrides) ->
    Manifest = imboy_plugin_registry:manifest(PluginName),
    FeatureKeys = maps:get(feature_keys, Manifest, []),
    lists:foldl(
        fun(FeatureKey, Acc) ->
            case maps:find(FeatureKey, ExistingFeatureOverrides) of
                {ok, Enabled} ->
                    maps:put(FeatureKey, #{enabled => Enabled}, Acc);
                error ->
                    Acc
            end
        end,
        #{},
        FeatureKeys
    ).

-spec persist_config_sections(map()) -> ok.
persist_config_sections(Sections) ->
    _ = [
        config_ds:set(Key, merge_persisted_section(Key, Value))
        || {Key, Value} <- maps:to_list(Sections)
    ],
    ok.

-spec merge_persisted_section(binary(), term()) -> term().
merge_persisted_section(?PRODUCT_PROFILE_CONFIG_KEY, ?DELETE_VALUE) ->
    null;
merge_persisted_section(?PRODUCT_PROFILE_CONFIG_KEY, Value) ->
    Value;
merge_persisted_section(Key, Value) ->
    merge_saved_map_updates(normalize_map(load_saved_config_value(Key)), normalize_map(Value)).

-spec merge_saved_map_updates(map(), map()) -> map().
merge_saved_map_updates(Existing, Updates) ->
    DeleteKeys = [
        Key
        || {Key, DeleteValue} <- maps:to_list(Updates),
           is_delete_marker(DeleteValue)
    ],
    Existing1 = maps:without(DeleteKeys, Existing),
    PutMap = maps:from_list([
        {Key, UpdateValue}
        || {Key, UpdateValue} <- maps:to_list(Updates),
           not is_delete_marker(UpdateValue)
    ]),
    maps:merge(Existing1, PutMap).

-spec is_delete_marker(term()) -> boolean().
is_delete_marker(?DELETE_VALUE) ->
    true;
is_delete_marker(<<"$delete">>) ->
    true;
is_delete_marker(_) ->
    false.

-spec parse_toggle_payload(term()) -> {ok, boolean()} | error.
parse_toggle_payload(#{enabled := Enabled}) ->
    parse_boolean_value(Enabled);
parse_toggle_payload(#{<<"enabled">> := Enabled}) ->
    parse_boolean_value(Enabled);
parse_toggle_payload(Options) when is_list(Options) ->
    case is_charlist(Options) of
        true ->
            parse_boolean_value(Options);
        false ->
            case proplists:get_value(enabled, Options, undefined) of
                undefined ->
                    case proplists:get_value(<<"enabled">>, Options, undefined) of
                        undefined ->
                            error;
                        Enabled ->
                            parse_boolean_value(Enabled)
                    end;
                Enabled ->
                    parse_boolean_value(Enabled)
            end
    end;
parse_toggle_payload(Value) ->
    parse_boolean_value(Value).

-spec parse_boolean_value(term()) -> {ok, boolean()} | error.
parse_boolean_value(true) ->
    {ok, true};
parse_boolean_value(false) ->
    {ok, false};
parse_boolean_value(1) ->
    {ok, true};
parse_boolean_value(0) ->
    {ok, false};
parse_boolean_value(<<"true">>) ->
    {ok, true};
parse_boolean_value(<<"false">>) ->
    {ok, false};
parse_boolean_value("true") ->
    {ok, true};
parse_boolean_value("false") ->
    {ok, false};
parse_boolean_value(_) ->
    error.

-spec is_charlist(list()) -> boolean().
is_charlist([]) ->
    true;
is_charlist([H | T]) when is_integer(H), H >= 0, H =< 16#10FFFF ->
    is_charlist(T);
is_charlist(_) ->
    false.

-spec parse_storage_mode(term()) -> {ok, archived | compliance_e2ee | secure_e2ee} | error.
parse_storage_mode(archived) ->
    {ok, archived};
parse_storage_mode(compliance_e2ee) ->
    {ok, compliance_e2ee};
parse_storage_mode(secure_e2ee) ->
    {ok, secure_e2ee};
parse_storage_mode(<<"archived">>) ->
    {ok, archived};
parse_storage_mode(<<"compliance_e2ee">>) ->
    {ok, compliance_e2ee};
parse_storage_mode(<<"secure_e2ee">>) ->
    {ok, secure_e2ee};
parse_storage_mode("archived") ->
    {ok, archived};
parse_storage_mode("compliance_e2ee") ->
    {ok, compliance_e2ee};
parse_storage_mode("secure_e2ee") ->
    {ok, secure_e2ee};
parse_storage_mode(_) ->
    error.

-spec parse_e2ee_mode(term()) -> {ok, disabled | optional | compliance | required} | error.
parse_e2ee_mode(disabled) ->
    {ok, disabled};
parse_e2ee_mode(optional) ->
    {ok, optional};
parse_e2ee_mode(compliance) ->
    {ok, compliance};
parse_e2ee_mode(required) ->
    {ok, required};
parse_e2ee_mode(<<"disabled">>) ->
    {ok, disabled};
parse_e2ee_mode(<<"optional">>) ->
    {ok, optional};
parse_e2ee_mode(<<"compliance">>) ->
    {ok, compliance};
parse_e2ee_mode(<<"required">>) ->
    {ok, required};
parse_e2ee_mode("disabled") ->
    {ok, disabled};
parse_e2ee_mode("optional") ->
    {ok, optional};
parse_e2ee_mode("compliance") ->
    {ok, compliance};
parse_e2ee_mode("required") ->
    {ok, required};
parse_e2ee_mode(_) ->
    error.

-spec parse_audit_mode(term()) -> {ok, none | metadata | full} | error.
parse_audit_mode(none) ->
    {ok, none};
parse_audit_mode(metadata) ->
    {ok, metadata};
parse_audit_mode(full) ->
    {ok, full};
parse_audit_mode(<<"none">>) ->
    {ok, none};
parse_audit_mode(<<"metadata">>) ->
    {ok, metadata};
parse_audit_mode(<<"full">>) ->
    {ok, full};
parse_audit_mode("none") ->
    {ok, none};
parse_audit_mode("metadata") ->
    {ok, metadata};
parse_audit_mode("full") ->
    {ok, full};
parse_audit_mode(_) ->
    error.

-spec normalize_retention_policy_payload(term()) -> {ok, map()} | {error, map()}.
normalize_retention_policy_payload(Value) ->
    Policy = normalize_map(Value),
    case maps:size(Policy) of
        0 ->
            {error,
                policy_error_detail(
                    capabilities, retention_policy, invalid_object, <<"invalid retention_policy value">>)};
        _ ->
            {ok, Policy}
    end.

-spec policy_error_result(atom() | undefined, atom() | undefined, atom(), binary()) ->
    {error, binary(), map()}.
policy_error_result(Section, Field, Reason, Message) ->
    Detail = policy_error_detail(Section, Field, Reason, Message),
    {error, Message, public_policy_error_detail(Detail)}.

-spec policy_error_detail(atom() | undefined, atom() | undefined, atom(), binary()) -> map().
policy_error_detail(Section, Field, Reason, Message) ->
    Detail0 = maybe_put_saved_section(#{}, section, Section),
    Detail1 = maybe_put_saved_section(Detail0, field, Field),
    Detail1#{
        reason => Reason,
        message => Message
    }.

-spec policy_error_message(map()) -> binary().
policy_error_message(Detail) ->
    maps:get(message, Detail).

-spec public_policy_error_detail(map()) -> map().
public_policy_error_detail(Detail) ->
    public_term(maps:remove(message, Detail)).

-spec public_term(term()) -> term().
public_term(Map) when is_map(Map) ->
    maps:from_list([
        {public_key(Key), public_term(Value)}
        || {Key, Value} <- maps:to_list(Map)
    ]);
public_term(List) when is_list(List) ->
    [public_term(Value) || Value <- List];
public_term(true) ->
    true;
public_term(false) ->
    false;
public_term(null) ->
    null;
public_term(undefined) ->
    null;
public_term(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
public_term(Value) ->
    Value.

-spec public_key(term()) -> binary().
public_key(Key) when is_binary(Key) ->
    Key;
public_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
public_key(Key) when is_list(Key) ->
    unicode:characters_to_binary(Key);
public_key(Key) ->
    ec_cnv:to_binary(Key).

-spec public_plugin_manifest(map()) -> map().
public_plugin_manifest(Manifest) ->
    AllowedKeys = [
        kind,
        feature_keys,
        requires_capabilities,
        depends_on_plugins,
        app_entries,
        admin_entries,
        api_handlers,
        children,
        enabled
    ],
    maps:with(AllowedKeys, Manifest).
