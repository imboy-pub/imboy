-module(imboy_feature).

-export([enabled/1, ensure_enabled/2, all/0, feature_names/0]).

-include("error_code.hrl").

-type feature() :: atom() | binary() | string().

-spec enabled(feature()) -> boolean().
enabled(Feature) ->
    case normalize_feature_key(Feature) of
        undefined ->
            true;
        FeatureKey ->
            maps:get(FeatureKey, imboy_policy:effective_features(), true)
    end.

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

-spec normalize_feature_key(feature()) -> atom() | undefined.
normalize_feature_key(Feature) when is_atom(Feature) ->
    case lists:member(Feature, feature_names()) of
        true ->
            Feature;
        false ->
            undefined
    end;
normalize_feature_key(Feature) when is_binary(Feature) ->
    maybe_existing_feature_atom(Feature);
normalize_feature_key(Feature) when is_list(Feature) ->
    maybe_existing_feature_atom(unicode:characters_to_binary(Feature)).

-spec maybe_existing_feature_atom(binary()) -> atom() | undefined.
maybe_existing_feature_atom(Feature) ->
    try
        FeatureKey = binary_to_existing_atom(Feature, utf8),
        normalize_feature_key(FeatureKey)
    catch
        error:badarg ->
            undefined
    end.
