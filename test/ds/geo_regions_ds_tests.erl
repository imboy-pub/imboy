-module(geo_regions_ds_tests).
-include_lib("eunit/include/eunit.hrl").

validate_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun complete_and_optional_path/1,
        fun wrong_parent_and_unknown_node/1,
        fun root_and_shape/1,
        fun namespace_version_and_display/1,
        fun unavailable_resources/1,
        fun incorrect_bytes_or_digest/1,
        fun unsafe_manifest_entries/1,
        fun mismatched_structure_header_and_duplicate_nodes/1,
        fun duplicate_json_keys/1
    ]}.

setup() ->
    Scratch = os:getenv("I03_TEST_ROOT"),
    true = is_list(Scratch) andalso filename:pathtype(Scratch) =:= absolute,
    Dir = filename:join(Scratch, binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12)))),
    ok = file:make_dir(Dir),
    ok = file:make_dir(filename:join(Dir, "regions")),
    File = os:getenv("I03_TEST_FIXTURE", "test/fixtures/geo_regions/consumer_path.json"),
    {ok, Structure} = file:read_file(File),
    write_country(Dir, Structure).

cleanup(#{data_root := Dir}) ->
    ok = file:del_dir_r(Dir).

write_country(Dir, Structure) ->
    File = <<"regions/JP.json">>,
    ok = file:write_file(filename:join(Dir, File), Structure),
    write_manifest(Dir, [resource(File, Structure)]).

write_manifest(Dir, Entries) ->
    Data = jsone:encode(#{
        <<"protocolVersion">> => <<"1.0">>,
        <<"datasetId">> => <<"geo-regions-fixture">>,
        <<"revision">> => <<"m-0123456789abcdef">>,
        <<"resources">> => Entries
    }),
    write_manifest_data(Dir, Data).

write_manifest_data(Dir, Data) ->
    ok = file:write_file(filename:join(Dir, "manifest.json"), Data),
    #{data_root => list_or_binary(Dir), manifest_sha256 => digest(Data)}.

resource(File, Structure) ->
    #{
        <<"kind">> => <<"structure">>,
        <<"countryCode">> => <<"JP">>,
        <<"path">> => File,
        <<"sha256">> => digest(Structure),
        <<"bytes">> => byte_size(Structure),
        <<"revision">> => <<"s-0123456789abcdef">>
    }.

digest(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).

list_or_binary(Value) ->
    unicode:characters_to_binary(Value).

read_country(#{data_root := Dir}) ->
    {ok, Data} = file:read_file(filename:join(Dir, "regions/JP.json")),
    Data.

selection(Path) ->
    #{
        <<"protocolVersion">> => <<"1.0">>,
        <<"datasetId">> => <<"geo-regions-fixture">>,
        <<"countryCode">> => <<"JP">>,
        <<"dataRevision">> => <<"s-0123456789abcdef">>,
        <<"pathIds">> => Path
    }.

country_selection() ->
    selection([<<"cc:JP">>]).

complete_and_optional_path(Pin) ->
    [
        ?_assertEqual(ok, geo_regions_ds:validate(country_selection(), Pin)),
        ?_assertEqual(ok, geo_regions_ds:validate(selection([<<"cc:JP">>, <<"jp:pref-a">>]), Pin)),
        ?_assertEqual(
            ok,
            geo_regions_ds:validate(selection([<<"cc:JP">>, <<"jp:pref-a">>, <<"jp:city-a">>]), Pin)
        )
    ].

wrong_parent_and_unknown_node(Pin) ->
    [
        ?_assertEqual(
            {error, invalid_selection},
            geo_regions_ds:validate(selection([<<"cc:JP">>, <<"jp:pref-b">>, <<"jp:city-a">>]), Pin)
        ),
        ?_assertEqual(
            {error, invalid_selection},
            geo_regions_ds:validate(selection([<<"cc:JP">>, <<"jp:retired">>]), Pin)
        ),
        ?_assertEqual(
            {error, invalid_selection},
            geo_regions_ds:validate(selection([<<"cc:JP">>, <<"Duplicate display">>]), Pin)
        )
    ].

root_and_shape(Pin) ->
    [
        ?_assertEqual(
            {error, invalid_selection}, geo_regions_ds:validate(selection([<<"JP">>]), Pin)
        ),
        ?_assertEqual(
            {error, invalid_selection},
            geo_regions_ds:validate(selection([<<"cc:JP">>, <<"cc:JP">>]), Pin)
        ),
        ?_assertEqual(
            {error, invalid_selection}, geo_regions_ds:validate(selection([<<"cc:JP">> | bad]), Pin)
        ),
        ?_assertEqual(
            {error, invalid_selection},
            geo_regions_ds:validate((country_selection())#{<<"path">> => <<"/untrusted">>}, Pin)
        )
    ].

namespace_version_and_display(Pin) ->
    Base = country_selection(),
    [
        ?_assertEqual(
            {error, unsupported_version},
            geo_regions_ds:validate(Base#{<<"datasetId">> := <<"unrelated">>}, Pin)
        ),
        ?_assertEqual(
            {error, unsupported_version},
            geo_regions_ds:validate(Base#{<<"protocolVersion">> := <<"2.0">>}, Pin)
        ),
        ?_assertEqual(
            {error, unsupported_version},
            geo_regions_ds:validate(Base#{<<"dataRevision">> := <<"s-0000000000000000">>}, Pin)
        ),
        ?_assertEqual(
            ok, geo_regions_ds:validate(Base#{<<"displaySnapshot">> => [<<"Alternate">>]}, Pin)
        )
    ].

unavailable_resources(Pin = #{data_root := Dir}) ->
    ?_test(begin
        ?assertEqual({error, unavailable}, geo_regions_ds:validate(country_selection(), #{})),
        ?assertEqual(
            {error, unavailable},
            geo_regions_ds:validate(country_selection(), Pin#{data_root := <<"relative">>})
        ),
        ok = file:delete(filename:join(Dir, "regions/JP.json")),
        ?assertEqual({error, unavailable}, geo_regions_ds:validate(country_selection(), Pin))
    end).

incorrect_bytes_or_digest(Pin = #{data_root := Dir}) ->
    ?_test(begin
        ?assertEqual(
            {error, invalid_artifact},
            geo_regions_ds:validate(country_selection(), Pin#{
                manifest_sha256 := digest(<<"untrusted">>)
            })
        ),
        Data = read_country(Pin),
        File = filename:join(Dir, "regions/JP.json"),
        ok = file:write_file(File, <<Data/binary, " ">>),
        ?assertEqual({error, invalid_artifact}, geo_regions_ds:validate(country_selection(), Pin)),
        <<_, Tail/binary>> = Data,
        ok = file:write_file(File, <<" ", Tail/binary>>),
        ?assertEqual({error, invalid_artifact}, geo_regions_ds:validate(country_selection(), Pin))
    end).

unsafe_manifest_entries(Pin = #{data_root := Dir}) ->
    ?_test(begin
        Structure = read_country(Pin),
        Entry = resource(<<"regions/JP.json">>, Structure),
        ?assertEqual(
            {error, invalid_artifact},
            geo_regions_ds:validate(
                country_selection(), write_manifest(Dir, [Entry#{<<"path">> := <<"../JP.json">>}])
            )
        ),
        ?assertEqual(
            {error, invalid_artifact},
            geo_regions_ds:validate(country_selection(), write_manifest(Dir, [Entry, Entry]))
        )
    end).

mismatched_structure_header_and_duplicate_nodes(Pin = #{data_root := Dir}) ->
    ?_test(begin
        Structure = jsone:decode(read_country(Pin)),
        ?assertEqual(
            {error, invalid_artifact},
            geo_regions_ds:validate(
                country_selection(),
                write_country(Dir, jsone:encode(Structure#{<<"countryCode">> := <<"FR">>}))
            )
        ),
        #{<<"nodes">> := [Root | _] = Nodes} = Structure,
        ?assertEqual(
            {error, invalid_artifact},
            geo_regions_ds:validate(
                country_selection(),
                write_country(Dir, jsone:encode(Structure#{<<"nodes">> := [Root | Nodes]}))
            )
        )
    end).

duplicate_json_keys(#{data_root := Dir}) ->
    ?_test(begin
        Pin = write_manifest_data(
            Dir,
            <<"{\"protocolVersion\":\"1.0\",\"datasetId\":\"geo-regions-fixture\",\"datasetId\":\"unrelated\",\"resources\":[]}">>
        ),
        ?assertEqual({error, invalid_artifact}, geo_regions_ds:validate(country_selection(), Pin))
    end).
