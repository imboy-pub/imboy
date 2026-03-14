-module(imboy_policy).

-export([
    current_profile/0,
    effective/0,
    effective_capabilities/0,
    effective_features/0,
    effective_plugins/0,
    message_search_enabled/0,
    message_export_enabled/0,
    message_audit_mode/0,
    message_audit_enabled/0,
    message_body_visible/0
]).

-spec current_profile() -> community | enterprise.
current_profile() ->
    imboy_profile_preset:current().

-spec effective() -> map().
effective() ->
    Features = effective_features(),
    #{
        profile => current_profile(),
        capabilities => effective_capabilities(),
        features => Features,
        plugins => effective_plugins(Features)
    }.

-spec effective_capabilities() -> map().
effective_capabilities() ->
    Defaults = normalize_capability_map(
        maps:get(capabilities, imboy_profile_preset:defaults(current_profile()), #{})
    ),
    Overrides = normalize_capability_map(config_ds:env(capabilities, #{})),
    normalize_capabilities(maps:merge(Defaults, Overrides), Defaults).

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

-spec effective_features() -> map().
effective_features() ->
    Features = config_ds:env(features, undefined),
    maps:from_list([
        {Name, feature_enabled(Name, Features)}
        || Name <- feature_names()
    ]).

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
    KnownKeys = lists:foldl(
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
    ),
    maps:merge(Map0, KnownKeys).

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
