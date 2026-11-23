-module(user_region_contract_tests).

-include_lib("eunit/include/eunit.hrl").

legacy_region_preserves_exact_string_test() ->
    Region = <<"  同名地区 / 旧行政码  "/utf8>>,
    ?assertEqual(
        {ok, {set_field, <<"region">>, Region}}, user_agg:validate_update(<<"region">>, Region)
    ),
    ?assertEqual(<<"unresolved">>, user_agg:region_selection_status(Region, absent)),
    ?assertEqual(<<"empty">>, user_agg:region_selection_status(<<>>, absent)).

update_envelope_keeps_legacy_and_identity_separate_test() ->
    Region = <<"  Beijing  ">>,
    Selection = #{<<"countryCode">> => <<"CN">>, <<"pathIds">> => [<<"cc:CN">>]},
    Body = #{<<"region">> => Region, <<"selection">> => Selection},
    ?assertEqual(
        {ok, {set_region_selection, Region, Selection}},
        user_agg:validate_update(<<"regionSelection">>, Body)
    ),
    ?assertEqual(
        {error, bad_region_selection},
        user_agg:validate_update(<<"regionSelection">>, Body#{<<"pin">> => <<"client supplied">>})
    ),
    ?assertEqual(
        {error, bad_region_selection},
        user_agg:validate_update(<<"regionSelection">>, Body#{<<"region">> => <<"a", 0, "b">>})
    ),
    TooLong = binary:copy(<<"地"/utf8>>, 81),
    ?assertEqual(
        {error, bad_region_selection},
        user_agg:validate_update(<<"regionSelection">>, Body#{<<"region">> => TooLong})
    ).

jsonb_nul_in_optional_display_metadata_is_rejected_test() ->
    Selection = #{<<"countryCode">> => <<"CN">>, <<"pathIds">> => [<<"cc:CN">>]},
    lists:foreach(
        fun(Metadata) ->
            Body = #{
                <<"region">> => <<"China">>, <<"selection">> => maps:merge(Selection, Metadata)
            },
            ?assertEqual(
                {error, bad_region_selection}, user_agg:validate_update(<<"regionSelection">>, Body)
            )
        end,
        [#{<<"displayLocale">> => <<"en", 0>>}, #{<<"displaySnapshot">> => [<<"China", 0>>]}]
    ).
