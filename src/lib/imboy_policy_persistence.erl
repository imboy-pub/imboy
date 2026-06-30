-module(imboy_policy_persistence).
-compile([nowarn_deprecated_catch]).
%% @doc 产品策略持久化层（从 imboy_policy.erl §4 Save/persist 提取）
%%
%% 负责：策略配置的读取(load_*)、保存(save_*/persist_*)及规范化辅助。
%% 依赖：config_ds（数据访问）, imboy_policy_codec, imboy_policy_normalize。

-export([
    save_admin_config/1,
    preview_admin_config/1,
    save_config/1,
    save_result_view/0,
    preview_config/1,
    load_profile_config/0,
    load_capability_config/0,
    load_feature_config/0,
    load_saved_config_value/1,
    saved_profile_override/0,
    saved_capability_overrides/0,
    saved_feature_overrides/0,
    flatten_saved_feature_config/1,
    compact_saved_plugin_overrides/1,
    maybe_put_saved_section/3,
    persist_config_sections/1,
    load_config_value/2,
    normalize_config_sections/1,
    maybe_put_profile_section/2,
    maybe_put_capabilities_section/2,
    maybe_put_features_section/2,
    merge_persisted_section/2
]).

-define(PRODUCT_PROFILE_CONFIG_KEY, <<"product_profile">>).
-define(CAPABILITIES_CONFIG_KEY, <<"capabilities">>).
-define(FEATURES_CONFIG_KEY, <<"features">>).
-define(DELETE_VALUE, '$delete$').

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

effective_view() -> imboy_policy:effective_view().
saved_view() -> imboy_policy:saved_view().
preview_adjustments_view(S, E) -> imboy_policy_view:preview_adjustments_view(S, E).
origins_view(S) -> imboy_policy_view:origins_view(S).
preview_view(S) -> imboy_policy_view:preview_view(S).
