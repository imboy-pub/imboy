-module(product_terminology).

-export([
    init/0,
    public/1,
    validate_all/0,
    validate_document/2
]).

-define(DEFAULT_PROFILE, <<"generic">>).
-define(PROFILES_KEY, {?MODULE, profiles}).

%% @doc Load and validate immutable terminology profiles for request-time reads.
-spec init() -> ok.
init() ->
    Profiles = load_all(),
    persistent_term:put(?PROFILES_KEY, Profiles),
    ok.

-spec public(term()) -> map().
public(Profile0) ->
    Profiles = profiles(),
    Profile = select_profile(Profile0, Profiles),
    #{hash := Hash, terms := Terms} = maps:get(Profile, Profiles),
    #{<<"profile">> => Profile, <<"hash">> => Hash, <<"terms">> => Terms}.

%% @doc Validate every checked-in terminology profile without activating one.
-spec validate_all() -> ok.
validate_all() ->
    _ = load_all(),
    ok.

-spec load_all() -> map().
load_all() ->
    Pattern = filename:join(terminology_dir(), "*.json"),
    case filelib:wildcard(Pattern) of
        [] ->
            erlang:error({terminology_files_missing, Pattern});
        Paths ->
            Profiles = maps:from_list([load_entry(Path) || Path <- Paths]),
            case maps:is_key(?DEFAULT_PROFILE, Profiles) of
                true ->
                    Profiles;
                false ->
                    erlang:error({terminology_default_missing, ?DEFAULT_PROFILE})
            end
    end.

-spec profiles() -> map().
profiles() ->
    case persistent_term:get(?PROFILES_KEY, undefined) of
        undefined ->
            %% Application startup normally primes this cache. Lazy loading keeps
            %% direct callers and release tooling deterministic outside the app.
            ok = init(),
            persistent_term:get(?PROFILES_KEY);
        Profiles ->
            Profiles
    end.

-spec select_profile(term(), map()) -> binary().
select_profile(Profile, Profiles) when is_binary(Profile), byte_size(Profile) =< 32 ->
    case is_valid_profile_name(Profile) andalso maps:is_key(Profile, Profiles) of
        true -> Profile;
        false -> ?DEFAULT_PROFILE
    end;
select_profile(_Profile, _Profiles) ->
    ?DEFAULT_PROFILE.

-spec load_entry(file:filename()) -> {binary(), map()}.
load_entry(Path) ->
    Profile = unicode:characters_to_binary(filename:basename(Path, ".json")),
    ok = validate_profile_name(Profile),
    {Profile, load_file(Path, Profile)}.

-spec load_file(file:filename(), binary()) -> map().
load_file(Path, ExpectedProfile) ->
    Json =
        case file:read_file(Path) of
            {ok, Bin} ->
                Bin;
            {error, FileReason} ->
                erlang:error({terminology_file_error, Path, FileReason})
        end,
    Document = decode_json(Json, Path),
    ok = validate_document(ExpectedProfile, Document),
    Hex = binary:encode_hex(crypto:hash(sha256, Json), lowercase),
    #{
        profile => ExpectedProfile,
        hash => <<"sha256:", Hex/binary>>,
        terms => maps:get(<<"terms">>, Document)
    }.

-spec decode_json(binary(), file:filename()) -> term().
decode_json(Json, Path) ->
    ObjectFinish = fun(Pairs, Acc) ->
        case duplicate_key(Pairs, #{}) of
            none ->
                {maps:from_list(Pairs), Acc};
            Key ->
                erlang:error({duplicate_json_object_key, Key})
        end
    end,
    try json:decode(Json, ok, #{object_finish => ObjectFinish}) of
        {Document, ok, <<>>} ->
            Document;
        {_Document, ok, Rest} ->
            erlang:error({trailing_json_data, Rest})
    catch
        error:DecodeReason ->
            erlang:error({invalid_terminology_json, Path, DecodeReason})
    end.

-spec duplicate_key([{binary(), term()}], map()) -> none | binary().
duplicate_key([], _Seen) ->
    none;
duplicate_key([{Key, _Value} | Rest], Seen) ->
    case maps:is_key(Key, Seen) of
        true ->
            Key;
        false ->
            duplicate_key(Rest, Seen#{Key => true})
    end.

-spec validate_document(binary(), term()) -> ok.
validate_document(ExpectedProfile, Json) when is_binary(Json) ->
    validate_document(ExpectedProfile, decode_json(Json, memory));
validate_document(ExpectedProfile, Document) when is_map(Document) ->
    AllowedFields = [<<"schema_version">>, <<"profile">>, <<"terms">>],
    ok = reject_unknown_fields(Document, AllowedFields, document),
    SchemaVersion = required_field(<<"schema_version">>, Document),
    Profile = required_field(<<"profile">>, Document),
    Terms = required_field(<<"terms">>, Document),
    case SchemaVersion of
        1 ->
            ok;
        _ ->
            erlang:error({invalid_terminology, {unsupported_schema_version, SchemaVersion}})
    end,
    case Profile of
        ExpectedProfile ->
            ok;
        _ ->
            erlang:error({invalid_terminology, {profile_mismatch, ExpectedProfile, Profile}})
    end,
    case is_map(Terms) of
        true ->
            validate_terms(Terms);
        false ->
            erlang:error({invalid_terminology, {invalid_terms, Terms}})
    end;
validate_document(_ExpectedProfile, Document) ->
    erlang:error({invalid_terminology, {document_must_be_object, Document}}).

-spec validate_terms(map()) -> ok.
validate_terms(Terms) ->
    Required = required_term_keys(),
    Keys = maps:keys(Terms),
    Missing = Required -- Keys,
    Unknown = Keys -- Required,
    case {Missing, Unknown} of
        {[], []} ->
            ok;
        _ ->
            erlang:error({invalid_terminology, {term_keys, Missing, Unknown}})
    end,
    InvalidLabels = [
        {Key, Label}
     || {Key, Label} <- maps:to_list(Terms),
        not (is_binary(Label) andalso byte_size(Label) > 0 andalso byte_size(Label) =< 128)
    ],
    case InvalidLabels of
        [] ->
            ok;
        _ ->
            erlang:error({invalid_terminology, {invalid_labels, InvalidLabels}})
    end.

-spec required_term_keys() -> [binary()].
required_term_keys() ->
    [
        <<"participant">>,
        <<"delegate">>,
        <<"operator">>,
        <<"group">>,
        <<"task_assignment">>,
        <<"task_submission">>,
        <<"submission_review">>,
        <<"review_assist">>
    ].

-spec reject_unknown_fields(map(), [binary()], atom()) -> ok.
reject_unknown_fields(Map, Allowed, Context) ->
    case maps:keys(Map) -- Allowed of
        [] ->
            ok;
        Unknown ->
            erlang:error({invalid_terminology, {unknown_fields, Context, Unknown}})
    end.

-spec required_field(binary(), map()) -> term().
required_field(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        error ->
            erlang:error({invalid_terminology, {missing_field, Key}})
    end.

-spec validate_profile_name(binary()) -> ok.
validate_profile_name(Profile) ->
    case is_valid_profile_name(Profile) of
        true ->
            ok;
        false ->
            erlang:error({invalid_terminology_profile, Profile})
    end.

-spec is_valid_profile_name(binary()) -> boolean().
is_valid_profile_name(Profile) ->
    re:run(Profile, <<"^[a-z][a-z0-9_-]{0,31}$">>, [{capture, none}]) =:= match.

-spec terminology_dir() -> file:filename().
terminology_dir() ->
    case code:priv_dir(imboy) of
        Dir when is_list(Dir) ->
            filename:join(Dir, "terminology");
        {error, Reason} ->
            erlang:error({terminology_priv_dir_error, Reason})
    end.
