%%%-------------------------------------------------------------------
%%% @doc Tests for `barrel_mcp_schema'.
%%% @end
%%%-------------------------------------------------------------------
-module(barrel_mcp_schema_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Type
%%====================================================================

type_string_ok_test() ->
    ?assertEqual(
        ok,
        barrel_mcp_schema:validate(<<"hi">>, #{<<"type">> => <<"string">>})
    ).

type_string_mismatch_test() ->
    ?assertMatch(
        {error, [{[], {type_mismatch, <<"string">>}}]},
        barrel_mcp_schema:validate(42, #{<<"type">> => <<"string">>})
    ).

type_union_test() ->
    S = #{<<"type">> => [<<"string">>, <<"null">>]},
    ?assertEqual(ok, barrel_mcp_schema:validate(<<"x">>, S)),
    ?assertEqual(ok, barrel_mcp_schema:validate(null, S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate(1, S)).

%%====================================================================
%% Object
%%====================================================================

required_property_missing_test() ->
    S = #{
        <<"type">> => <<"object">>,
        <<"required">> => [<<"q">>],
        <<"properties">> => #{<<"q">> => #{<<"type">> => <<"string">>}}
    },
    ?assertMatch(
        {error, [{[], {missing_required, <<"q">>}}]},
        barrel_mcp_schema:validate(#{}, S)
    ).

required_property_present_test() ->
    S = #{
        <<"type">> => <<"object">>,
        <<"required">> => [<<"q">>],
        <<"properties">> => #{<<"q">> => #{<<"type">> => <<"string">>}}
    },
    ?assertEqual(
        ok,
        barrel_mcp_schema:validate(#{<<"q">> => <<"hi">>}, S)
    ).

nested_property_path_test() ->
    S = #{
        <<"type">> => <<"object">>,
        <<"properties">> =>
            #{
                <<"a">> => #{
                    <<"type">> => <<"object">>,
                    <<"properties">> =>
                        #{<<"b">> => #{<<"type">> => <<"integer">>}}
                }
            }
    },
    Bad = #{<<"a">> => #{<<"b">> => <<"not int">>}},
    {error, [{Path, _}]} = barrel_mcp_schema:validate(Bad, S),
    ?assertEqual([<<"a">>, <<"b">>], Path).

additional_properties_rejected_test() ->
    S = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"q">> => #{<<"type">> => <<"string">>}},
        <<"additionalProperties">> => false
    },
    ?assertMatch(
        {error, [{_, {unexpected_property, <<"extra">>}}]},
        barrel_mcp_schema:validate(#{<<"q">> => <<"x">>, <<"extra">> => 1}, S)
    ).

%%====================================================================
%% Array / items / uniqueItems
%%====================================================================

array_items_validate_test() ->
    S = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{<<"type">> => <<"integer">>}
    },
    ?assertEqual(ok, barrel_mcp_schema:validate([1, 2, 3], S)),
    {error, [{Path, _}]} =
        barrel_mcp_schema:validate([1, <<"oops">>, 3], S),
    ?assertEqual([1], Path).

unique_items_test() ->
    S = #{<<"type">> => <<"array">>, <<"uniqueItems">> => true},
    ?assertEqual(ok, barrel_mcp_schema:validate([1, 2, 3], S)),
    ?assertMatch(
        {error, [{[], items_not_unique}]},
        barrel_mcp_schema:validate([1, 1, 2], S)
    ).

min_max_items_test() ->
    S = #{
        <<"type">> => <<"array">>,
        <<"minItems">> => 1,
        <<"maxItems">> => 2
    },
    ?assertEqual(ok, barrel_mcp_schema:validate([1], S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate([], S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate([1, 2, 3], S)).

%%====================================================================
%% String / enum / pattern
%%====================================================================

enum_test() ->
    S = #{
        <<"type">> => <<"string">>,
        <<"enum">> => [<<"a">>, <<"b">>]
    },
    ?assertEqual(ok, barrel_mcp_schema:validate(<<"a">>, S)),
    ?assertMatch(
        {error, [{[], {not_in_enum, _}}]},
        barrel_mcp_schema:validate(<<"c">>, S)
    ).

string_length_test() ->
    S = #{
        <<"type">> => <<"string">>,
        <<"minLength">> => 2,
        <<"maxLength">> => 4
    },
    ?assertEqual(ok, barrel_mcp_schema:validate(<<"abc">>, S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate(<<"a">>, S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate(<<"abcde">>, S)).

pattern_match_test() ->
    S = #{<<"type">> => <<"string">>, <<"pattern">> => <<"^[a-z]+$">>},
    ?assertEqual(ok, barrel_mcp_schema:validate(<<"hello">>, S)),
    ?assertMatch(
        {error, [{[], {pattern_mismatch, _}}]},
        barrel_mcp_schema:validate(<<"Hello">>, S)
    ).

%%====================================================================
%% Number bounds
%%====================================================================

minimum_test() ->
    S = #{<<"type">> => <<"integer">>, <<"minimum">> => 0},
    ?assertEqual(ok, barrel_mcp_schema:validate(0, S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate(-1, S)).

exclusive_maximum_test() ->
    S = #{<<"type">> => <<"number">>, <<"exclusiveMaximum">> => 1},
    ?assertEqual(ok, barrel_mcp_schema:validate(0.5, S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate(1, S)).

%%====================================================================
%% allOf / anyOf / oneOf
%%====================================================================

any_of_test() ->
    S = #{
        <<"anyOf">> => [
            #{<<"type">> => <<"string">>},
            #{<<"type">> => <<"integer">>}
        ]
    },
    ?assertEqual(ok, barrel_mcp_schema:validate(<<"x">>, S)),
    ?assertEqual(ok, barrel_mcp_schema:validate(7, S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate(true, S)).

one_of_unique_test() ->
    S = #{
        <<"oneOf">> => [
            #{<<"type">> => <<"string">>},
            #{<<"type">> => <<"integer">>}
        ]
    },
    ?assertEqual(ok, barrel_mcp_schema:validate(<<"x">>, S)).

one_of_no_match_test() ->
    S = #{
        <<"oneOf">> => [
            #{<<"type">> => <<"string">>},
            #{<<"type">> => <<"integer">>}
        ]
    },
    ?assertMatch(
        {error, [{[], no_oneof_match}]},
        barrel_mcp_schema:validate(true, S)
    ).

all_of_test() ->
    S = #{
        <<"allOf">> => [
            #{<<"type">> => <<"integer">>},
            #{<<"minimum">> => 0}
        ]
    },
    ?assertEqual(ok, barrel_mcp_schema:validate(5, S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate(-1, S)),
    ?assertMatch({error, _}, barrel_mcp_schema:validate(<<"x">>, S)).
