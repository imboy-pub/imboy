%% @doc 策略规范化与验证辅助模块
%%
%% 从 imboy_policy.erl §6 提取的纯函数集合：
%%   - 基础工具  : find_in_map, candidate_keys, to_boolean, payload_value …
%%   - 枚举解析  : normalize_storage_mode, normalize_e2ee_mode, normalize_audit_mode …
%%   - 能力规范化: normalize_capabilities, normalize_capability_map …
%%   - Payload 验证: normalize_capability_payload, normalize_feature_payload, normalize_plugin_payload
%%   - 删除标记  : is_delete_marker, merge_saved_map_updates
%%
%% 依赖方向（单向）：imboy_policy_normalize → imboy_policy_codec, imboy_policy_catalog,
%%                                           imboy_feature, imboy_plugin_registry
%%
%% imboy_policy → imboy_policy_normalize（新增） → imboy_policy_codec, imboy_policy_catalog

-module(imboy_policy_normalize).

-export([
    %% 基础工具
    find_in_map/2,
    find_in_proplist/2,
    switch_enabled/1,
    candidate_keys/1,
    to_boolean/2,
    payload_value/2,
    %% 枚举/类型解析
    normalize_profile_input/1,
    normalize_storage_mode/2,
    normalize_e2ee_mode/2,
    normalize_audit_mode/2,
    normalize_retention_policy/2,
    body_visibility_allowed/2,
    %% 能力规范化
    capability_value/3,
    enforce_capability_constraints/1,
    normalize_capability_map/1,
    normalize_capabilities/2,
    %% Payload 验证
    validate_save_sections/1,
    normalize_capability_payload/1,
    normalize_capability_payload_value/2,
    normalize_feature_payload/1,
    normalize_plugin_payload/2,
    plugin_override_candidate/2,
    plugin_clear_payload/2,
    preserve_plugin_feature_overrides/2,
    %% 删除标记
    is_delete_marker/1,
    merge_saved_map_updates/2
]).

-define(DELETE_VALUE, '$delete').

%% ===================================================================
%% 基础工具
%% ===================================================================

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

%% ===================================================================
%% 枚举/类型解析
%% ===================================================================

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
    Policy = imboy_policy_codec:normalize_map(Value),
    case maps:size(Policy) of
        0 -> Default;
        _ -> Policy
    end.

-spec body_visibility_allowed(atom(), atom()) -> boolean().
body_visibility_allowed(secure_e2ee, _E2eeMode) ->
    false;
body_visibility_allowed(_StorageMode, required) ->
    false;
body_visibility_allowed(compliance_e2ee, _E2eeMode) ->
    true;
body_visibility_allowed(_StorageMode, compliance) ->
    true;
body_visibility_allowed(_, _) ->
    true.

%% ===================================================================
%% 能力规范化
%% ===================================================================

-spec capability_value(map(), atom(), term()) -> term().
capability_value(Map, Key, Default) ->
    case find_in_map(Map, candidate_keys(Key)) of
        undefined -> Default;
        Value -> Value
    end.

-spec enforce_capability_constraints(map()) -> map().
enforce_capability_constraints(Capabilities0) ->
    StorageMode = maps:get(storage_mode, Capabilities0, archived),
    E2eeMode = maps:get(e2ee_mode, Capabilities0, disabled),
    MessageSearch =
        case StorageMode =:= secure_e2ee orelse E2eeMode =:= required of
            true -> false;
            false -> maps:get(message_search, Capabilities0, false)
        end,
    MessageExport =
        case StorageMode of
            secure_e2ee -> false;
            _ -> maps:get(message_export, Capabilities0, false)
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

-spec normalize_capability_map(term()) -> map().
normalize_capability_map(Value) ->
    Map0 = imboy_policy_codec:normalize_map(Value),
    lists:foldl(
        fun(Key, Acc) ->
            case find_in_map(Map0, candidate_keys(Key)) of
                undefined -> Acc;
                KeyValue -> maps:put(Key, KeyValue, Acc)
            end
        end,
        #{},
        imboy_policy_catalog:capability_names()
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
        capability_value(
            Capabilities0, retention_policy, maps:get(retention_policy, Defaults, #{})
        ),
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

%% ===================================================================
%% Payload 验证
%% ===================================================================

-spec validate_save_sections(map()) -> {ok, map()} | {error, binary(), map()}.
validate_save_sections(Sections) ->
    ErrorKeys = [profile_error, capabilities_error, features_error],
    case [maps:get(Key, Sections) || Key <- ErrorKeys, maps:is_key(Key, Sections)] of
        [Detail | _] ->
            {error, imboy_policy_codec:policy_error_message(Detail),
                imboy_policy_codec:public_policy_error_detail(Detail)};
        [] ->
            {ok, maps:without(ErrorKeys, Sections)}
    end.

-spec normalize_capability_payload(term()) -> {ok, map()} | {error, map()}.
normalize_capability_payload(Value) ->
    Map0 = imboy_policy_codec:normalize_map(Value),
    Result = lists:foldl(
        fun
            (Key, {ok, Acc}) ->
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
        imboy_policy_catalog:capability_names()
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
                        imboy_policy_codec:policy_error_detail(
                            capabilities,
                            undefined,
                            invalid_payload,
                            <<"invalid capabilities payload">>
                        )};
                _ ->
                    {ok, Capabilities}
            end
    end.

-spec normalize_capability_payload_value(atom(), term()) -> {ok, term()} | {error, map()}.
normalize_capability_payload_value(storage_mode, Value) ->
    case imboy_policy_codec:parse_storage_mode(Value) of
        {ok, StorageMode} ->
            {ok, StorageMode};
        error ->
            {error,
                imboy_policy_codec:policy_error_detail(
                    capabilities, storage_mode, invalid_enum, <<"invalid storage_mode value">>
                )}
    end;
normalize_capability_payload_value(e2ee_mode, Value) ->
    case imboy_policy_codec:parse_e2ee_mode(Value) of
        {ok, E2eeMode} ->
            {ok, E2eeMode};
        error ->
            {error,
                imboy_policy_codec:policy_error_detail(
                    capabilities, e2ee_mode, invalid_enum, <<"invalid e2ee_mode value">>
                )}
    end;
normalize_capability_payload_value(message_search, Value) ->
    case imboy_policy_codec:parse_toggle_payload(Value) of
        {ok, Enabled} ->
            {ok, Enabled};
        error ->
            {error,
                imboy_policy_codec:policy_error_detail(
                    capabilities,
                    message_search,
                    invalid_boolean,
                    <<"invalid message_search value">>
                )}
    end;
normalize_capability_payload_value(message_export, Value) ->
    case imboy_policy_codec:parse_toggle_payload(Value) of
        {ok, Enabled} ->
            {ok, Enabled};
        error ->
            {error,
                imboy_policy_codec:policy_error_detail(
                    capabilities,
                    message_export,
                    invalid_boolean,
                    <<"invalid message_export value">>
                )}
    end;
normalize_capability_payload_value(audit_mode, Value) ->
    case imboy_policy_codec:parse_audit_mode(Value) of
        {ok, AuditMode} ->
            {ok, AuditMode};
        error ->
            {error,
                imboy_policy_codec:policy_error_detail(
                    capabilities, audit_mode, invalid_enum, <<"invalid audit_mode value">>
                )}
    end;
normalize_capability_payload_value(retention_policy, Value) ->
    case imboy_policy_codec:normalize_retention_policy_payload(Value) of
        {ok, Policy} ->
            {ok, Policy};
        {error, _} = Error ->
            Error
    end;
normalize_capability_payload_value(_Key, Value) ->
    {ok, Value}.

-spec normalize_feature_payload(term()) -> {ok, map()} | {error, map()}.
normalize_feature_payload(Value) ->
    Map0 = imboy_policy_codec:normalize_map(Value),
    Result = lists:foldl(
        fun
            (Key, {ok, Acc}) ->
                case find_in_map(Map0, candidate_keys(Key)) of
                    undefined ->
                        {ok, Acc};
                    null ->
                        {ok, maps:put(Key, ?DELETE_VALUE, Acc)};
                    Item ->
                        case imboy_policy_codec:parse_toggle_payload(Item) of
                            {ok, Enabled} ->
                                {ok, maps:put(Key, #{enabled => Enabled}, Acc)};
                            error ->
                                {error,
                                    imboy_policy_codec:policy_error_detail(
                                        features,
                                        Key,
                                        invalid_boolean,
                                        <<"invalid features payload">>
                                    )}
                        end
                end;
            (_Key, {error, _} = Error) ->
                Error
        end,
        {ok, #{}},
        imboy_feature:feature_names()
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
                        imboy_policy_codec:policy_error_detail(
                            features, undefined, invalid_payload, <<"invalid features payload">>
                        )};
                _ ->
                    {ok, Features}
            end
    end.

-spec normalize_plugin_payload(term(), map()) -> {ok, map()} | {error, map()}.
normalize_plugin_payload(Value, ExistingFeatureOverrides) ->
    Map0 = imboy_policy_codec:normalize_map(Value),
    Result = lists:foldl(
        fun
            (PluginName, {ok, Acc}) ->
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
                        case imboy_policy_codec:parse_toggle_payload(Item) of
                            {ok, Enabled} ->
                                Manifest = imboy_plugin_registry:manifest(PluginName),
                                FeatureKeys = maps:get(feature_keys, Manifest, []),
                                {ok,
                                    lists:foldl(
                                        fun(FeatureKey, FeatureAcc) ->
                                            maps:put(
                                                FeatureKey, #{enabled => Enabled}, FeatureAcc
                                            )
                                        end,
                                        Acc,
                                        FeatureKeys
                                    )};
                            error ->
                                {error,
                                    imboy_policy_codec:policy_error_detail(
                                        plugins,
                                        PluginName,
                                        invalid_boolean,
                                        <<"invalid plugins payload">>
                                    )}
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
                        imboy_policy_codec:policy_error_detail(
                            plugins, undefined, invalid_payload, <<"invalid plugins payload">>
                        )};
                _ ->
                    {ok, FeatureConfig}
            end
    end.

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

%% ===================================================================
%% 删除标记
%% ===================================================================

-spec is_delete_marker(term()) -> boolean().
is_delete_marker(?DELETE_VALUE) ->
    true;
is_delete_marker(<<"$delete">>) ->
    true;
is_delete_marker(_) ->
    false.

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
