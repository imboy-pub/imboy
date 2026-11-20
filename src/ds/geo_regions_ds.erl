-module(geo_regions_ds).
-moduledoc "Validate an account region selection against a server-pinned GeoRegions release.".
-export([validate/2, source_names/2]).

-define(PROTOCOL, <<"1.0">>).

-type validation_error() ::
    invalid_selection | unsupported_version | unavailable | invalid_artifact.

-spec validate(map(), map()) -> ok | {error, validation_error()}.
validate(Selection, Pin) ->
    case validated_path(Selection, Pin) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

-spec source_names(map(), map()) -> {ok, [binary()]} | {error, validation_error()}.
source_names(Selection, Pin) ->
    case validated_path(Selection, Pin) of
        {ok, Names} ->
            case lists:all(fun nonempty_string/1, Names) of
                true -> {ok, Names};
                false -> {error, invalid_artifact}
            end;
        {error, _} = Error ->
            Error
    end.

validated_path(#{<<"protocolVersion">> := Protocol}, _) when Protocol =/= ?PROTOCOL ->
    {error, unsupported_version};
validated_path(
    #{
        <<"protocolVersion">> := ?PROTOCOL,
        <<"datasetId">> := Dataset,
        <<"countryCode">> := <<A, B>> = Country,
        <<"dataRevision">> := Revision,
        <<"pathIds">> := [Root | _] = Path
    } = Selection,
    Pin
) when
    is_binary(Dataset),
    byte_size(Dataset) > 0,
    is_binary(Revision),
    is_list(Path),
    A >= $A,
    A =< $Z,
    B >= $A,
    B =< $Z
->
    case valid_selection(Selection, Root, Country, Revision, Path) of
        true -> validate_pin(Pin, Dataset, Country, Revision, Path);
        false -> {error, invalid_selection}
    end;
validated_path(_, _) ->
    {error, invalid_selection}.

valid_selection(Selection, Root, Country, Revision, Path) ->
    Root =:= <<"cc:", Country/binary>> andalso
        re:run(Revision, <<"^s-[a-f0-9]{16}$">>, [{capture, none}, dollar_endonly]) =:= match andalso
        proper_all(fun nonempty_string/1, Path) andalso
        length(lists:usort(Path)) =:= length(Path) andalso
        maps:without(
            [
                <<"protocolVersion">>,
                <<"datasetId">>,
                <<"countryCode">>,
                <<"dataRevision">>,
                <<"pathIds">>,
                <<"displaySnapshot">>,
                <<"displayLocale">>
            ],
            Selection
        ) =:= #{} andalso
        valid_display(Selection).

proper_all(_Predicate, []) ->
    true;
proper_all(Predicate, [Head | Tail]) ->
    Predicate(Head) andalso proper_all(Predicate, Tail);
proper_all(_Predicate, _) ->
    false.

nonempty_string(Bin) ->
    is_binary(Bin) andalso byte_size(Bin) > 0.

valid_display(Selection) ->
    Snapshot = maps:get(<<"displaySnapshot">>, Selection, []),
    Locale = maps:get(<<"displayLocale">>, Selection, <<"en">>),
    proper_all(fun erlang:is_binary/1, Snapshot) andalso
        valid_locale(Locale).

valid_locale(Locale) when is_binary(Locale) ->
    case unicode:characters_to_list(Locale) of
        Characters when is_list(Characters) ->
            Count = length(Characters),
            Count >= 2 andalso Count =< 255;
        _ ->
            false
    end;
valid_locale(_) ->
    false.

validate_pin(#{data_root := Root, manifest_sha256 := Digest}, Dataset, Country, Revision, Path) when
    is_binary(Root), is_binary(Digest), byte_size(Digest) =:= 64
->
    case filename:pathtype(Root) of
        absolute ->
            case read_pinned(filename:join(Root, <<"manifest.json">>), Digest, any) of
                {ok, Manifest} ->
                    validate_manifest(Manifest, Root, Dataset, Country, Revision, Path);
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, unavailable}
    end;
validate_pin(_, _, _, _, _) ->
    {error, unavailable}.

validate_manifest(
    #{<<"protocolVersion">> := ?PROTOCOL, <<"datasetId">> := Dataset, <<"resources">> := Entries},
    Root,
    Dataset,
    Country,
    Revision,
    Path
) when is_list(Entries) ->
    ExpectedPath = <<"regions/", Country/binary, ".json">>,
    case
        [
            Entry
         || #{<<"kind">> := <<"structure">>, <<"countryCode">> := CC} = Entry <- Entries,
            CC =:= Country
        ]
    of
        [
            #{
                <<"revision">> := Revision,
                <<"path">> := ExpectedPath,
                <<"bytes">> := Bytes,
                <<"sha256">> := Digest
            }
        ] when
            is_integer(Bytes), Bytes > 0, is_binary(Digest), byte_size(Digest) =:= 64
        ->
            case read_pinned(filename:join(Root, ExpectedPath), Digest, Bytes) of
                {ok, Structure} -> validate_structure(Structure, Dataset, Country, Revision, Path);
                {error, _} = Error -> Error
            end;
        [] ->
            {error, unavailable};
        [#{<<"revision">> := Other}] when Other =/= Revision ->
            {error, unsupported_version};
        _ ->
            {error, invalid_artifact}
    end;
validate_manifest(
    #{<<"protocolVersion">> := ?PROTOCOL, <<"datasetId">> := Other}, _, Dataset, _, _, _
) when
    is_binary(Other), Other =/= Dataset
->
    {error, unsupported_version};
validate_manifest(_, _, _, _, _, _) ->
    {error, invalid_artifact}.

validate_structure(
    #{
        <<"protocolVersion">> := ?PROTOCOL,
        <<"datasetId">> := Dataset,
        <<"countryCode">> := Country,
        <<"revision">> := Revision,
        <<"nodes">> := Nodes
    },
    Dataset,
    Country,
    Revision,
    Path
) when is_list(Nodes) ->
    case index_nodes(Nodes, #{}) of
        {ok, Index} -> validate_edges(Path, null, Index, []);
        {error, _} = Error -> Error
    end;
validate_structure(_, _, _, _, _) ->
    {error, invalid_artifact}.

index_nodes([], Index) ->
    {ok, Index};
index_nodes([#{<<"id">> := Id, <<"parentId">> := Parent} = Node | Tail], Index) when
    is_binary(Id), byte_size(Id) > 0, is_binary(Parent) orelse Parent =:= null
->
    case maps:is_key(Id, Index) of
        true -> {error, invalid_artifact};
        false -> index_nodes(Tail, Index#{Id => Node})
    end;
index_nodes(_, _) ->
    {error, invalid_artifact}.

validate_edges([], _, _, Names) ->
    {ok, lists:reverse(Names)};
validate_edges([Id | Tail], Parent, Index, Names) ->
    case maps:find(Id, Index) of
        {ok, #{<<"parentId">> := Parent} = Node} ->
            validate_edges(Tail, Id, Index, [maps:get(<<"name">>, Node, undefined) | Names]);
        _ ->
            {error, invalid_selection}
    end.

read_pinned(Path, Digest, Bytes) ->
    case file:read_file(Path) of
        {ok, Data} when Bytes =:= any; byte_size(Data) =:= Bytes ->
            case binary:encode_hex(crypto:hash(sha256, Data), lowercase) of
                Digest -> decode(Data);
                _ -> {error, invalid_artifact}
            end;
        {ok, _} ->
            {error, invalid_artifact};
        {error, _} ->
            {error, unavailable}
    end.

decode(Data) ->
    try
        {ok, to_maps(jsone:decode(Data, [{object_format, tuple}, reject_invalid_utf8]))}
    catch
        error:_ -> {error, invalid_artifact}
    end.

to_maps({Pairs}) ->
    Map = maps:from_list([{K, to_maps(V)} || {K, V} <- Pairs]),
    case map_size(Map) =:= length(Pairs) of
        true -> Map;
        false -> error(duplicate_artifact_key)
    end;
to_maps(List) when is_list(List) ->
    lists:map(fun to_maps/1, List);
to_maps(Value) ->
    Value.
