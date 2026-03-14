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
    message_body_visible/0
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
    #{
        <<"meta">> => meta_view(),
        <<"saved">> => saved_view(),
        <<"effective">> => effective_view()
    }.

-spec meta_view() -> map().
meta_view() ->
    public_term(#{
        profiles => #{
            supported => imboy_profile_preset:supported_profiles(),
            defaults => profile_defaults_catalog()
        },
        capabilities => capability_meta_catalog(),
        features => feature_meta_catalog(),
        plugins => plugin_meta_catalog(),
        write_contract => #{
            plugins_translate_to_features => true,
            feature_overrides_take_precedence => true,
            null_clears_overrides => true,
            preview_available => true,
            preview_returns => [saved, effective, adjustments],
            bootstrap_available => true,
            bootstrap_returns => [meta, saved, effective],
            save_returns => [effective],
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

-spec save_admin_config(map()) -> {ok, map()} | {error, binary()}.
save_admin_config(Payload) ->
    save_config(Payload).

-spec preview_admin_config(map()) -> {ok, map()} | {error, binary()}.
preview_admin_config(Payload) ->
    preview_config(Payload).

-spec save_config(map()) -> {ok, map()} | {error, binary()}.
save_config(Payload) when is_map(Payload) ->
    Sections = normalize_config_sections(Payload),
    case validate_save_sections(Sections) of
        {ok, SaveSections} when map_size(SaveSections) > 0 ->
            persist_config_sections(SaveSections),
            {ok, effective_view()};
        {ok, _SaveSections} ->
            {error, <<"policy payload missing editable fields">>};
        {error, Reason} ->
            {error, Reason}
    end;
save_config(_) ->
    {error, <<"policy payload must be an object">>}.

-spec preview_config(map()) -> {ok, map()} | {error, binary()}.
preview_config(Payload) when is_map(Payload) ->
    Sections = normalize_config_sections(Payload),
    case validate_save_sections(Sections) of
        {ok, SaveSections} when map_size(SaveSections) > 0 ->
            {ok, preview_view(SaveSections)};
        {ok, _SaveSections} ->
            {error, <<"policy payload missing editable fields">>};
        {error, Reason} ->
            {error, Reason}
    end;
preview_config(_) ->
    {error, <<"policy payload must be an object">>}.

-spec effective_features() -> map().
effective_features() ->
    effective_features_from_config(load_feature_config()).

-spec effective_plugins() -> map().
effective_plugins() ->
    effective_plugins(effective_features()).

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
        imboy_plugin_registry:all()
    ).

-spec effective_from_configs(term(), term(), term()) -> map().
effective_from_configs(ProfileConfig, CapabilityConfig, FeatureConfig) ->
    Profile = resolve_profile(ProfileConfig),
    Features = effective_features_from_config(FeatureConfig),
    #{
        profile => Profile,
        capabilities => effective_capabilities_for_profile(Profile, CapabilityConfig),
        features => Features,
        plugins => effective_plugins(Features)
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
        <<"adjustments">> => preview_adjustments_view(Saved, Effective)
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
    public_term(
        maybe_put_saved_section(
            Sections0,
            features,
            feature_adjustments(
                maps:get(<<"features">>, Saved, #{}),
                maps:get(<<"features">>, Effective, #{}))
        )
    ).

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

-spec feature_adjustments(map(), map()) -> map().
feature_adjustments(SavedFeatures, EffectiveFeatures) ->
    maps:fold(
        fun(Key, SavedValue, Acc) ->
            case feature_adjustment(Key, SavedValue, EffectiveFeatures) of
                {ok, Adjustment} ->
                    Acc#{Key => Adjustment};
                error ->
                    Acc
            end
        end,
        #{},
        SavedFeatures
    ).

-spec feature_adjustment(binary(), term(), map()) -> {ok, map()} | error.
feature_adjustment(Key, SavedValue, EffectiveFeatures) ->
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
        _ ->
            error
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

-spec effective_features_from_config(term()) -> map().
effective_features_from_config(FeatureConfig) ->
    maps:from_list([
        {Name, feature_enabled(Name, FeatureConfig)}
        || Name <- feature_names()
    ]).

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
    FeatureKeys = maps:get(feature_keys, imboy_plugin_registry:get(PluginName), []),
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

-spec capability_meta_catalog() -> map().
capability_meta_catalog() ->
    #{
        storage_mode => #{
            type => enum,
            options => [archived, secure_e2ee]
        },
        e2ee_mode => #{
            type => enum,
            options => [disabled, optional, required]
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
            secure_e2ee => #{
                message_search => false,
                message_export => false,
                audit_mode => metadata
            }
        },
        e2ee_mode => #{
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
        dependencies => feature_dependency_catalog()
    }.

-spec plugin_managed_feature_names() -> [atom()].
plugin_managed_feature_names() ->
    lists:usort(
        lists:append([
            maps:get(feature_keys, Manifest, [])
            || Manifest <- maps:values(imboy_plugin_registry:all())
        ])
    ).

-spec feature_dependency_catalog() -> map().
feature_dependency_catalog() ->
    maps:from_list([
        {FeatureName, dependencies(FeatureName)}
        || FeatureName <- feature_names(),
           length(dependencies(FeatureName)) > 0
    ]).

-spec plugin_meta_catalog() -> map().
plugin_meta_catalog() ->
    maps:map(
        fun(_Name, Manifest) ->
            public_plugin_manifest(Manifest)
        end,
        imboy_plugin_registry:all()
    ).

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
body_visibility_allowed(_, _) ->
    true.

-spec normalize_storage_mode(term(), atom()) -> archived | secure_e2ee.
normalize_storage_mode(archived, _Default) ->
    archived;
normalize_storage_mode(secure_e2ee, _Default) ->
    secure_e2ee;
normalize_storage_mode(<<"archived">>, _Default) ->
    archived;
normalize_storage_mode(<<"secure_e2ee">>, _Default) ->
    secure_e2ee;
normalize_storage_mode("archived", _Default) ->
    archived;
normalize_storage_mode("secure_e2ee", _Default) ->
    secure_e2ee;
normalize_storage_mode(_, Default) ->
    Default.

-spec normalize_e2ee_mode(term(), atom()) -> disabled | optional | required.
normalize_e2ee_mode(disabled, _Default) ->
    disabled;
normalize_e2ee_mode(optional, _Default) ->
    optional;
normalize_e2ee_mode(required, _Default) ->
    required;
normalize_e2ee_mode(<<"disabled">>, _Default) ->
    disabled;
normalize_e2ee_mode(<<"optional">>, _Default) ->
    optional;
normalize_e2ee_mode(<<"required">>, _Default) ->
    required;
normalize_e2ee_mode("disabled", _Default) ->
    disabled;
normalize_e2ee_mode("optional", _Default) ->
    optional;
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
                    Sections#{profile_error => <<"invalid profile value">>}
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
                {error, Reason} ->
                    Sections#{capabilities_error => Reason}
            end;
        error ->
            Sections
    end.

-spec maybe_put_features_section(map(), map()) -> map().
maybe_put_features_section(Sections, Payload) ->
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
                normalize_plugin_payload(PluginValue);
            error ->
                {ok, #{}}
        end,
    case {FeaturesResult, PluginsResult} of
        {{error, Reason}, _} ->
            Sections#{features_error => Reason};
        {_, {error, Reason}} ->
            Sections#{features_error => Reason};
        {{ok, FeatureConfig}, {ok, PluginFeatureConfig}} ->
            MergedFeatureConfig = maps:merge(PluginFeatureConfig, FeatureConfig),
            case map_size(MergedFeatureConfig) of
                0 ->
                    Sections;
                _ ->
                    Sections#{?FEATURES_CONFIG_KEY => public_term(MergedFeatureConfig)}
            end
    end.

-spec validate_save_sections(map()) -> {ok, map()} | {error, binary()}.
validate_save_sections(Sections) ->
    ErrorKeys = [profile_error, capabilities_error, features_error],
    case [maps:get(Key, Sections) || Key <- ErrorKeys, maps:is_key(Key, Sections)] of
        [Reason | _] ->
            {error, Reason};
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

-spec normalize_capability_payload(term()) -> {ok, map()} | {error, binary()}.
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
                    {error, <<"invalid capabilities payload">>};
                _ ->
                    {ok, Capabilities}
            end
    end.

-spec normalize_capability_payload_value(atom(), term()) -> {ok, term()} | {error, binary()}.
normalize_capability_payload_value(storage_mode, Value) ->
    case parse_storage_mode(Value) of
        {ok, StorageMode} ->
            {ok, StorageMode};
        error ->
            {error, <<"invalid storage_mode value">>}
    end;
normalize_capability_payload_value(e2ee_mode, Value) ->
    case parse_e2ee_mode(Value) of
        {ok, E2eeMode} ->
            {ok, E2eeMode};
        error ->
            {error, <<"invalid e2ee_mode value">>}
    end;
normalize_capability_payload_value(message_search, Value) ->
    case parse_toggle_payload(Value) of
        {ok, Enabled} ->
            {ok, Enabled};
        error ->
            {error, <<"invalid message_search value">>}
    end;
normalize_capability_payload_value(message_export, Value) ->
    case parse_toggle_payload(Value) of
        {ok, Enabled} ->
            {ok, Enabled};
        error ->
            {error, <<"invalid message_export value">>}
    end;
normalize_capability_payload_value(audit_mode, Value) ->
    case parse_audit_mode(Value) of
        {ok, AuditMode} ->
            {ok, AuditMode};
        error ->
            {error, <<"invalid audit_mode value">>}
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

-spec normalize_feature_payload(term()) -> {ok, map()} | {error, binary()}.
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
                            {error, <<"invalid features payload">>}
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
                    {error, <<"invalid features payload">>};
                _ ->
                    {ok, Features}
            end
    end.

-spec normalize_plugin_payload(term()) -> {ok, map()} | {error, binary()}.
normalize_plugin_payload(Value) ->
    Map0 = normalize_map(Value),
    Result = lists:foldl(
        fun(PluginName, {ok, Acc}) ->
            case find_in_map(Map0, candidate_keys(PluginName)) of
                undefined ->
                    {ok, Acc};
                null ->
                    Manifest = imboy_plugin_registry:get(PluginName),
                    FeatureKeys = maps:get(feature_keys, Manifest, []),
                    {ok,
                        lists:foldl(
                            fun(FeatureKey, FeatureAcc) ->
                                maps:put(FeatureKey, ?DELETE_VALUE, FeatureAcc)
                            end,
                            Acc,
                            FeatureKeys
                        )};
                Item ->
                    case parse_toggle_payload(Item) of
                        {ok, Enabled} ->
                            Manifest = imboy_plugin_registry:get(PluginName),
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
                            {error, <<"invalid plugins payload">>}
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
                    {error, <<"invalid plugins payload">>};
                _ ->
                    {ok, FeatureConfig}
            end
    end.

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

-spec parse_storage_mode(term()) -> {ok, archived | secure_e2ee} | error.
parse_storage_mode(archived) ->
    {ok, archived};
parse_storage_mode(secure_e2ee) ->
    {ok, secure_e2ee};
parse_storage_mode(<<"archived">>) ->
    {ok, archived};
parse_storage_mode(<<"secure_e2ee">>) ->
    {ok, secure_e2ee};
parse_storage_mode("archived") ->
    {ok, archived};
parse_storage_mode("secure_e2ee") ->
    {ok, secure_e2ee};
parse_storage_mode(_) ->
    error.

-spec parse_e2ee_mode(term()) -> {ok, disabled | optional | required} | error.
parse_e2ee_mode(disabled) ->
    {ok, disabled};
parse_e2ee_mode(optional) ->
    {ok, optional};
parse_e2ee_mode(required) ->
    {ok, required};
parse_e2ee_mode(<<"disabled">>) ->
    {ok, disabled};
parse_e2ee_mode(<<"optional">>) ->
    {ok, optional};
parse_e2ee_mode(<<"required">>) ->
    {ok, required};
parse_e2ee_mode("disabled") ->
    {ok, disabled};
parse_e2ee_mode("optional") ->
    {ok, optional};
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

-spec normalize_retention_policy_payload(term()) -> {ok, map()} | {error, binary()}.
normalize_retention_policy_payload(Value) ->
    Policy = normalize_map(Value),
    case maps:size(Policy) of
        0 ->
            {error, <<"invalid retention_policy value">>};
        _ ->
            {ok, Policy}
    end.

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
