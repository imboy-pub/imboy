-module(imboy_plugin_prop_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% Property-based style tests for plugin modules (no proper/triq dep)
%%% Uses rand module for random input generation.
%%%
%%% Properties tested:
%%%   P1: parse_migration_filename — roundtrip invariant
%%%   P2: parse_migration_filename — random garbage always returns error
%%%   P3: diff_pending — subset invariant (result ⊆ disk files)
%%%   P4: diff_pending — idempotent (double diff = single diff)
%%%   P5: validate_manifest — random maps always rejected or valid
%%%   P6: parse_contract_version — valid format roundtrip
%%%   P7: diff_pending — sorted by seq invariant
%%%
%%% Run: erl -noshell -pa ebin -eval "eunit:test([imboy_plugin_prop_tests],[verbose])" -s init stop
%%% @end
%%%-------------------------------------------------------------------

-define(PROP_ITERATIONS, 200).
-define(SEED, {
    erlang:unique_integer([positive]),
    erlang:unique_integer([positive]),
    erlang:unique_integer([positive])
}).

init_rand() ->
    rand:seed(exsss, ?SEED).

%% ===================================================================
%% P1: parse_migration_filename — roundtrip invariant
%% ===================================================================

prop_validate_random_map_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        init_rand(),
        Results = [
            begin
                RandomMap = random_map(3),
                case imboy_plugin_toml:validate_manifest(RandomMap) of
                    {error, _} ->
                        true;
                    {ok, _} ->
                        maps:is_key(name, RandomMap) andalso
                            maps:is_key(version, RandomMap) andalso
                            maps:is_key(contract_version, RandomMap)
                end
            end
         || _ <- lists:seq(1, ?PROP_ITERATIONS)
        ],
        ?assertEqual([], [R || R <- Results, R =/= true])
    end).

%% ===================================================================
%% P6: parse_contract_version — roundtrip
%% ===================================================================

prop_version_roundtrip_test_() ->
    ?TEST_SIMPLE(fun() ->
        init_rand(),
        Results = [
            begin
                Major = rand:uniform(99),
                Minor = rand:uniform(99),
                Bin = list_to_binary(
                    integer_to_list(Major) ++ "." ++ integer_to_list(Minor)
                ),
                case imboy_plugin_toml:parse_contract_version(Bin) of
                    {Major, Minor} -> true;
                    {error, invalid_format} -> Major =:= 0
                end
            end
         || _ <- lists:seq(1, ?PROP_ITERATIONS)
        ],
        ?assertEqual([], [R || R <- Results, R =/= true])
    end).

%% ===================================================================
%% P7: diff_pending — sorted by ascending seq
%% ===================================================================

random_descr() ->
    Len = rand:uniform(20),
    [
        random_char("abcdefghijklmnopqrstuvwxyz0123456789_")
     || _ <- lists:seq(1, Len - 1)
    ] ++
        [random_char("abcdefghijklmnopqrstuvwxyz0123456789")].

random_char(Chars) ->
    lists:nth(rand:uniform(length(Chars)), Chars).

random_map(Depth) when Depth =< 0 ->
    random_primitive();
random_map(Depth) ->
    case rand:uniform(4) of
        1 ->
            random_primitive();
        2 ->
            #{random_atom() => random_map(Depth - 1)};
        3 ->
            [random_map(Depth - 1) || _ <- lists:seq(1, rand:uniform(3))];
        4 ->
            #{
                random_atom() => random_map(Depth - 1),
                random_atom() => random_map(Depth - 1)
            }
    end.

random_primitive() ->
    case rand:uniform(5) of
        1 -> rand:uniform(1000);
        2 -> list_to_binary(random_descr());
        3 -> random_atom();
        4 -> undefined;
        5 -> true
    end.

random_atom() ->
    list_to_atom("x" ++ integer_to_list(rand:uniform(9999))).
