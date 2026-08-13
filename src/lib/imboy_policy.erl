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
    preview_admin_config/1,
    save_admin_config/1,
    message_search_enabled/0,
    message_export_enabled/0,
    message_audit_mode/0,
    message_audit_enabled/0,
    message_body_visible/0,
    message_encryption_required/0,
    e2ee_enabled/0,
    validate_message_write/5,
    content_bearing_action/1,
    encrypted_message_body/3,
    maybe_put_saved_section/3,
    capability_adjustments/2,
    effective_view_from_configs/3,
    feature_adjustments/4,
    normalize_preview_capability_overrides/1,
    normalize_preview_feature_overrides/1,
    plugin_adjustments/3,
    saved_view_from_values/3
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
    Base = effective_capabilities_for_profile(current_profile(), load_capability_config()),
    apply_capability_env_override(Base).

%% @doc 叠加 IMBOY_* 运行时环境变量覆盖（imboy_env:override_e2ee_mode 在启动时
%% set_env 的 application env capabilities）。优先级：env override > DB 持久化 > preset。
%% 不设 IMBOY_E2EE_MODE 时 application env 为 undefined，不覆盖，保留 DB/preset。
%% 修复点：此前 override_e2ee_mode 设的 application env 不被 effective_capabilities
%% 读取，导致 IMBOY_E2EE_MODE 无法覆盖 DB 里的 e2ee_mode（生产 DB 存 required 但
%% 现网明文为主，强制 required 会拒收明文 C2C）。
-spec apply_capability_env_override(map()) -> map().
apply_capability_env_override(Base) ->
    case application:get_env(imboy, capabilities, undefined) of
        Override when is_map(Override), map_size(Override) > 0 ->
            maps:merge(Base, Override);
        _ ->
            Base
    end.

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
%% v2.0 契约：是否加密以顶层 e2ee 字段为准，msg_type 保留原始类型（text/image…）
%% 供接收方渲染（见 imboyapp chat_network_service.dart:360）。只要 e2ee 是非空 map
%% 且 payload 非空即视为已加密，不再要求 msg_type=<<"e2ee">>——旧契约与客户端
%% 实际发送行为冲突，会导致 required 模式下加密消息被误判为明文而拒收。
encrypted_message_body(_MsgType, E2EE, Payload) when is_map(E2EE), is_binary(Payload) ->
    %% PFv3 Olm per-device fan-out 密文全在 e2ee.devices 信封、payload 恒为空串
    %% （客户端 _encryptC2COlmFanOut 返回 'payload': ''）。2026-08-11 生产实证：
    %% 旧判定要求 payload 非空，strict 模式把每条合法 Olm 密文误判成明文拒收
    %% （encrypted_message_required）。空 devices 空信封+空 payload 仍拒收（防空壳）。
    map_size(E2EE) > 0 andalso (Payload =/= <<>> orelse has_device_envelopes(E2EE));
encrypted_message_body(_, _, _) ->
    false.

%% @private PFv3 fan-out 信封判定：e2ee.devices 为非空 map 即存在逐设备密文。
-spec has_device_envelopes(map()) -> boolean().
has_device_envelopes(E2EE) when is_map(E2EE) ->
    case maps:get(<<"devices">>, E2EE, undefined) of
        Devices when is_map(Devices) -> map_size(Devices) > 0;
        _ -> false
    end.

%% §4 委托至 imboy_policy_persistence（持久化层）
-spec save_admin_config(map()) -> {ok, map()} | {error, binary()} | {error, binary(), map()}.
save_admin_config(P) -> imboy_policy_persistence:save_admin_config(P).

-spec preview_admin_config(map()) -> {ok, map()} | {error, binary()} | {error, binary(), map()}.
preview_admin_config(P) -> imboy_policy_persistence:preview_admin_config(P).

load_profile_config() -> imboy_policy_persistence:load_profile_config().
load_capability_config() -> imboy_policy_persistence:load_capability_config().
load_feature_config() -> imboy_policy_persistence:load_feature_config().
saved_profile_override() -> imboy_policy_persistence:saved_profile_override().
saved_capability_overrides() -> imboy_policy_persistence:saved_capability_overrides().
saved_feature_overrides() -> imboy_policy_persistence:saved_feature_overrides().
flatten_saved_feature_config(C) -> imboy_policy_persistence:flatten_saved_feature_config(C).
compact_saved_plugin_overrides(F) -> imboy_policy_persistence:compact_saved_plugin_overrides(F).
maybe_put_saved_section(S, K, V) -> imboy_policy_persistence:maybe_put_saved_section(S, K, V).
-spec effective_features() -> map().
effective_features() ->
    Profile = current_profile(),
    {_, Features, _} = effective_policy_components(
        Profile,
        load_capability_config(),
        load_feature_config()
    ),
    Features.

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
    Policy0 = effective_from_configs(ProfileConfig, CapabilityConfig, FeatureConfig),
    %% env override（IMBOY_E2EE_MODE 等）同样要作用到 view：/api/v1/app/policy
    %% 读这里，客户端据此决定是否加密（PolicyGate.requireReadyForSend）。
    %% 漏掉会让 disabled 部署下客户端仍拿到 DB 的 required，对端无设备密钥时
    %% E2EE 加密失败 fail-closed 拒发，C2C 文本消息发不出。与
    %% effective_capabilities/0 的 a7e78ace 修复保持同一 override 来源。
    Cap0 = maps:get(capabilities, Policy0, #{}),
    Policy = Policy0#{capabilities => apply_capability_env_override(Cap0)},
    public_effective_policy(Policy).

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

normalize_capability_map(V) -> imboy_policy_normalize:normalize_capability_map(V).
normalize_capabilities(C, D) -> imboy_policy_normalize:normalize_capabilities(C, D).
normalize_capability_payload(V) -> imboy_policy_normalize:normalize_capability_payload(V).
normalize_feature_payload(V) -> imboy_policy_normalize:normalize_feature_payload(V).

normalize_profile_input(V) -> imboy_policy_normalize:normalize_profile_input(V).

preview_adjustments_view(S, E) -> imboy_policy_view:preview_adjustments_view(S, E).
origins_view(S) -> imboy_policy_view:origins_view(S).
