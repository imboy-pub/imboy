-module(imboy_feature).

-export([enabled/1, ensure_enabled/2, all/0]).

-include("error_code.hrl").

-type feature() :: atom() | binary() | string().

-spec enabled(feature()) -> boolean().
enabled(Feature) ->
    FeatureName = normalize_feature_name(Feature),
    Features = config_ds:env(features, undefined),
    feature_enabled(FeatureName, Features).

-spec all() -> map().
all() ->
    maps:from_list([
        {atom_to_binary(Name, utf8), enabled(Name)}
        || Name <- feature_names()
    ]).

-spec ensure_enabled(cowboy_req:req(), feature()) -> ok | {error, cowboy_req:req()}.
ensure_enabled(Req, Feature) ->
    case enabled(Feature) of
        true ->
            ok;
        false ->
            {error,
             elib_response:error(
                 Req,
                 imboy_error:error_msg(?ERR_FEATURE_DISABLED),
                 ?ERR_FEATURE_DISABLED)}
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

-spec feature_enabled(binary(), term()) -> boolean().
feature_enabled(FeatureName, Features) ->
    CurrentEnabled = switch_enabled(lookup_feature_switch(Features, FeatureName)),
    DependencyEnabled = lists:all(
        fun(Dependency) ->
            switch_enabled(lookup_feature_switch(Features, Dependency))
        end,
        dependencies(FeatureName)
    ),
    CurrentEnabled andalso DependencyEnabled.

-spec dependencies(binary()) -> [binary()].
dependencies(<<"channel_discover">>) ->
    [<<"channel">>];
dependencies(<<"channel_invitation">>) ->
    [<<"channel">>];
dependencies(<<"channel_order">>) ->
    [<<"channel">>];
dependencies(_) ->
    [].

-spec lookup_feature_switch(term(), binary()) -> term().
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

-spec candidate_keys(binary()) -> [term()].
candidate_keys(FeatureName) ->
    case maybe_existing_atom(FeatureName) of
        undefined ->
            [FeatureName, binary_to_list(FeatureName)];
        AtomKey ->
            [AtomKey, FeatureName, binary_to_list(FeatureName)]
    end.

-spec maybe_existing_atom(binary()) -> atom() | undefined.
maybe_existing_atom(FeatureName) ->
    try
        binary_to_existing_atom(FeatureName, utf8)
    catch
        error:badarg ->
            undefined
    end.

-spec normalize_feature_name(feature()) -> binary().
normalize_feature_name(Feature) when is_atom(Feature) ->
    atom_to_binary(Feature, utf8);
normalize_feature_name(Feature) when is_binary(Feature) ->
    Feature;
normalize_feature_name(Feature) when is_list(Feature) ->
    unicode:characters_to_binary(Feature).

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
