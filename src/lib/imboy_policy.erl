%% @doc 产品策略管理模块（核心层）
%%
%% 已完成四模块拆分（2026-06）：
%%   - imboy_policy_codec.erl    : 纯编解码（normalize_map, public_term 等）
%%   - imboy_policy_catalog.erl  : 静态 catalog/metadata + dependencies/1
%%   - imboy_policy_normalize.erl: 纯规范化/验证函数（normalize_*, payload_*, merge_* 等）
%%   - imboy_policy.erl          : 公开 API + 业务逻辑（当前 ~1270 行）
%%
%% 依赖方向（单向）：imboy_policy → imboy_policy_normalize → imboy_policy_codec, imboy_policy_catalog
%%
%% 章节：
%%   §1 Public API                  line ~31
%%   §2 Effective policy (read)     line ~270
%%   §3 Feature / capability names  line ~960
%%   §4 Save / persist              line ~1060

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
    validate_message_write/5,
    maybe_put_saved_section/3
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
    effective_view_from_configs(
        load_profile_config(), load_capability_config(), load_feature_config()
    ).

-spec saved_view() -> map().
saved_view() ->
    SavedFeatures0 = saved_feature_overrides(),
    {SavedPlugins, SavedFeatures} = compact_saved_plugin_overrides(SavedFeatures0),
    Sections0 = maybe_put_saved_section(#{}, profile, saved_profile_override()),
    Sections1 = maybe_put_saved_section(Sections0, capabilities, saved_capability_overrides()),
    Sections2 = maybe_put_saved_section(Sections1, plugins, SavedPlugins),
    imboy_policy_codec:public_term(maybe_put_saved_section(Sections2, features, SavedFeatures)).

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
    imboy_policy_codec:public_term(#{
        profiles => #{
            supported => imboy_profile_preset:supported_profiles(),
            defaults => imboy_policy_catalog:profile_defaults_catalog()
        },
        origins => imboy_policy_catalog:origin_meta_catalog(),
        capabilities => imboy_policy_catalog:capability_meta_catalog(),
        features => imboy_policy_catalog:feature_meta_catalog(),
        plugins => imboy_policy_catalog:plugin_meta_catalog(),
        editor_order => imboy_policy_catalog:editor_order_catalog(),
        write_contract => #{
            plugins_translate_to_features => true,
            feature_overrides_take_precedence => true,
            null_clears_overrides => true,
            request_shape => imboy_policy_catalog:request_shape_meta_catalog(),
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

-spec validate_message_write(binary(), binary(), binary(), term(), term()) ->
    ok | {error, binary()}.
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
            imboy_policy_codec:policy_error_result(
                undefined,
                undefined,
                missing_editable_fields,
                <<"policy payload missing editable fields">>
            );
        {error, Reason, Details} ->
            {error, Reason, Details}
    end;
save_config(_) ->
    imboy_policy_codec:policy_error_result(
        undefined, undefined, invalid_payload_type, <<"policy payload must be an object">>
    ).

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
            imboy_policy_codec:policy_error_result(
                undefined,
                undefined,
                missing_editable_fields,
                <<"policy payload missing editable fields">>
            );
        {error, Reason, Details} ->
            {error, Reason, Details}
    end;
preview_config(_) ->
    imboy_policy_codec:policy_error_result(
        undefined, undefined, invalid_payload_type, <<"policy payload must be an object">>
    ).

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
            imboy_policy_codec:public_plugin_manifest(Manifest)
        end,
        Plugins0
    ),
    imboy_policy_codec:public_term(Policy#{plugins => Plugins}).

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
                    {ok, #{
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
            case
                feature_adjustment(
                    Key,
                    SavedValue,
                    EffectiveFeatures,
                    EffectivePlugins,
                    EffectiveCapabilities
                )
            of
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
    case
        SavedValue =/= EffectiveValue andalso SavedValue =:= true andalso EffectiveValue =:= false
    of
        true when Dependencies =/= [] ->
            {ok, #{
                saved => SavedValue,
                effective => EffectiveValue,
                reason => dependency,
                depends_on => Dependencies
            }};
        true ->
            case
                feature_plugin_constraint_adjustment(Key, EffectivePlugins, EffectiveCapabilities)
            of
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
            case imboy_policy_catalog:feature_plugin_owner(FeatureName) of
                undefined ->
                    error;
                PluginName ->
                    plugin_constraint_adjustment(
                        PluginName, EffectivePlugins, EffectiveCapabilities
                    )
            end
    end.

-spec feature_dependencies_for_key(binary()) -> [binary()].
feature_dependencies_for_key(Key) ->
    try
        imboy_policy_codec:public_term(
            imboy_policy_catalog:dependencies(binary_to_existing_atom(Key, utf8))
        )
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
    imboy_policy_codec:public_term(maybe_put_saved_section(Sections2, features, SavedFeatures)).

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
    lists:usort(
        lists:append([
            maps:get(feature_keys, Manifest, [])
         || {_PluginName, Manifest} <- maps:to_list(Plugins),
            maps:get(enabled, Manifest, false),
            plugin_constraint_violation_native(Manifest, Plugins, Capabilities) =/= none
        ])
    ).

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
    case
        unsatisfied_plugin_dependencies_native(
            maps:get(depends_on_plugins, Manifest, []),
            Plugins
        )
    of
        [] ->
            case
                unmet_capability_requirements_native(
                    maps:get(requires_capabilities, Manifest, []),
                    Capabilities
                )
            of
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
    case imboy_policy_codec:is_charlist(Expected) of
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
    case imboy_policy_codec:is_charlist(Value) of
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
            case
                unsatisfied_plugin_dependencies_public(
                    maps:get(depends_on_plugins, Manifest, []),
                    EffectivePlugins
                )
            of
                [] ->
                    case
                        unmet_capability_requirements_public(
                            maps:get(requires_capabilities, Manifest, []),
                            EffectiveCapabilities
                        )
                    of
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
        not capability_requirement_met(
            Expected, maps:get(imboy_policy_codec:public_key(Key), EffectiveCapabilities, undefined)
        )
    ]).

-spec plugin_enabled_in_public_map(term(), map()) -> boolean().
plugin_enabled_in_public_map(PluginRef, EffectivePlugins) ->
    case maps:find(imboy_policy_codec:public_key(PluginRef), EffectivePlugins) of
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

%% @doc 委托至 imboy_feature 单一数据源，避免重复维护。
-spec feature_names() -> [atom()].
feature_names() ->
    imboy_feature:feature_names().

-spec feature_enabled(atom(), term()) -> boolean().
feature_enabled(FeatureName, Features) ->
    CurrentEnabled = switch_enabled(lookup_feature_switch(Features, FeatureName)),
    DependencyEnabled = lists:all(
        fun(Dependency) ->
            switch_enabled(lookup_feature_switch(Features, Dependency))
        end,
        imboy_policy_catalog:dependencies(FeatureName)
    ),
    CurrentEnabled andalso DependencyEnabled.

-spec lookup_feature_switch(term(), atom()) -> term().
lookup_feature_switch(Features, FeatureName) when is_map(Features) ->
    find_in_map(Features, candidate_keys(FeatureName));
lookup_feature_switch(Features, FeatureName) when is_list(Features) ->
    find_in_proplist(Features, candidate_keys(FeatureName));
lookup_feature_switch(_, _) ->
    undefined.

%% 委托至 imboy_policy_normalize（提取于 2026-06）
find_in_map(Features, Keys) -> imboy_policy_normalize:find_in_map(Features, Keys).
find_in_proplist(Features, Keys) -> imboy_policy_normalize:find_in_proplist(Features, Keys).
switch_enabled(Value) -> imboy_policy_normalize:switch_enabled(Value).
candidate_keys(Name) -> imboy_policy_normalize:candidate_keys(Name).
to_boolean(Value, Default) -> imboy_policy_normalize:to_boolean(Value, Default).

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

plugin_override_candidate(Name, Overrides) ->
    imboy_policy_normalize:plugin_override_candidate(Name, Overrides).

-spec maybe_put_saved_section(map(), atom(), term()) -> map().
maybe_put_saved_section(Sections, _Key, undefined) ->
    Sections;
maybe_put_saved_section(Sections, _Key, Value) when is_map(Value), map_size(Value) =:= 0 ->
    Sections;
maybe_put_saved_section(Sections, Key, Value) ->
    Sections#{Key => Value}.

%% 委托至 imboy_policy_normalize
normalize_capability_map(V) -> imboy_policy_normalize:normalize_capability_map(V).
normalize_capabilities(C, D) -> imboy_policy_normalize:normalize_capabilities(C, D).

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
                        profile_error => imboy_policy_codec:policy_error_detail(
                            profile, profile, invalid_profile, <<"invalid profile value">>
                        )
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
                    Sections#{
                        ?CAPABILITIES_CONFIG_KEY => imboy_policy_codec:public_term(CapabilityConfig)
                    };
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
                    Sections#{
                        ?FEATURES_CONFIG_KEY => imboy_policy_codec:public_term(MergedFeatureConfig)
                    }
            end
    end.

%% 委托至 imboy_policy_normalize
validate_save_sections(S) -> imboy_policy_normalize:validate_save_sections(S).
payload_value(P, Keys) -> imboy_policy_normalize:payload_value(P, Keys).
normalize_profile_input(V) -> imboy_policy_normalize:normalize_profile_input(V).
normalize_capability_payload(V) -> imboy_policy_normalize:normalize_capability_payload(V).
normalize_feature_payload(V) -> imboy_policy_normalize:normalize_feature_payload(V).
normalize_plugin_payload(V, E) -> imboy_policy_normalize:normalize_plugin_payload(V, E).

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
    merge_saved_map_updates(
        imboy_policy_codec:normalize_map(load_saved_config_value(Key)),
        imboy_policy_codec:normalize_map(Value)
    ).

%% 委托至 imboy_policy_normalize
merge_saved_map_updates(E, U) -> imboy_policy_normalize:merge_saved_map_updates(E, U).
