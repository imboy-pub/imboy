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
-define(SEED, {erlang:unique_integer([positive]), erlang:unique_integer([positive]), erlang:unique_integer([positive])}).

init_rand() ->
    rand:seed(exsss, ?SEED).

%% ===================================================================
%% P1: parse_migration_filename — roundtrip invariant
%% ===================================================================

prop_parse_roundtrip_test_() ->
    ?TEST_SIMPLE(fun() ->
        init_rand(),
        Results = [begin
            Plugin = random_plugin_name(),
            Seq = rand:uniform(9999),
            Descr = random_descr(),
            Filename = Plugin ++ "_" ++ pad_seq(Seq) ++ "_" ++ Descr ++ ".sql",
            {ok, P, S, D} = imboy_plugin_migrate:parse_migration_filename(Filename),
            P =:= Plugin andalso S =:= Seq andalso D =:= Descr
        end || _ <- lists:seq(1, ?PROP_ITERATIONS)],
        Failed = [R || R <- Results, R =/= true],
        ?assertEqual([], Failed)
    end).

%% ===================================================================
%% P2: parse_migration_filename — random garbage always error
%% ===================================================================

prop_parse_garbage_always_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        init_rand(),
        Results = [begin
            Garbage = random_garbage(),
            imboy_plugin_migrate:parse_migration_filename(Garbage)
                =:= {error, invalid_filename}
        end || _ <- lists:seq(1, ?PROP_ITERATIONS)],
        Failed = [I || {I, false} <- lists:zip(lists:seq(1, ?PROP_ITERATIONS), Results)],
        ?assertEqual([], Failed)
    end).

%% ===================================================================
%% P3: diff_pending — subset invariant
%% ===================================================================

prop_diff_subset_test_() ->
    ?TEST_SIMPLE(fun() ->
        init_rand(),
        Results = [begin
            DiskFiles = random_disk_files(),
            AppliedSeqs = random_applied_seqs(),
            Pending = imboy_plugin_migrate:diff_pending(DiskFiles, AppliedSeqs),
            lists:all(fun(F) -> lists:member(F, DiskFiles) end, Pending)
        end || _ <- lists:seq(1, ?PROP_ITERATIONS)],
        ?assertEqual([], [R || R <- Results, R =/= true])
    end).

%% ===================================================================
%% P4: diff_pending — idempotent
%% ===================================================================

prop_diff_idempotent_test_() ->
    ?TEST_SIMPLE(fun() ->
        init_rand(),
        Results = [begin
            DiskFiles = random_disk_files(),
            AppliedSeqs = random_applied_seqs(),
            P1 = imboy_plugin_migrate:diff_pending(DiskFiles, AppliedSeqs),
            P2 = imboy_plugin_migrate:diff_pending(P1, AppliedSeqs),
            P1 =:= P2
        end || _ <- lists:seq(1, ?PROP_ITERATIONS)],
        ?assertEqual([], [R || R <- Results, R =/= true])
    end).

%% ===================================================================
%% P5: validate_manifest — random maps rejected or genuinely valid
%% ===================================================================

prop_validate_random_map_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        init_rand(),
        Results = [begin
            RandomMap = random_map(3),
            case imboy_plugin_toml:validate_manifest(RandomMap) of
                {error, _} -> true;
                {ok, _} ->
                    maps:is_key(name, RandomMap)
                    andalso maps:is_key(version, RandomMap)
                    andalso maps:is_key(contract_version, RandomMap)
            end
        end || _ <- lists:seq(1, ?PROP_ITERATIONS)],
        ?assertEqual([], [R || R <- Results, R =/= true])
    end).

%% ===================================================================
%% P6: parse_contract_version — roundtrip
%% ===================================================================

prop_version_roundtrip_test_() ->
    ?TEST_SIMPLE(fun() ->
        init_rand(),
        Results = [begin
            Major = rand:uniform(99),
            Minor = rand:uniform(99),
            Bin = list_to_binary(
                integer_to_list(Major) ++ "." ++ integer_to_list(Minor)),
            case imboy_plugin_toml:parse_contract_version(Bin) of
                {Major, Minor} -> true;
                {error, invalid_format} -> Major =:= 0
            end
        end || _ <- lists:seq(1, ?PROP_ITERATIONS)],
        ?assertEqual([], [R || R <- Results, R =/= true])
    end).

%% ===================================================================
%% P7: diff_pending — sorted by ascending seq
%% ===================================================================

prop_diff_sorted_test_() ->
    ?TEST_SIMPLE(fun() ->
        init_rand(),
        Results = [begin
            DiskFiles = random_disk_files(),
            AppliedSeqs = random_applied_seqs(),
            Pending = imboy_plugin_migrate:diff_pending(DiskFiles, AppliedSeqs),
            Seqs = [begin
                {ok, _, S, _} = imboy_plugin_migrate:parse_migration_filename(F),
                S
            end || F <- Pending],
            Seqs =:= lists:sort(Seqs)
        end || _ <- lists:seq(1, ?PROP_ITERATIONS)],
        ?assertEqual([], [R || R <- Results, R =/= true])
    end).

%% ===================================================================
%% Generators
%% ===================================================================

random_plugin_name() ->
    First = random_char("abcdefghijklmnopqrstuvwxyz"),
    Mid = [random_char("abcdefghijklmnopqrstuvwxyz0123456789_")
           || _ <- lists:seq(1, rand:uniform(10))],
    Last = random_char("abcdefghijklmnopqrstuvwxyz0123456789"),
    [First | Mid ++ [Last]].

random_descr() ->
    Len = rand:uniform(20),
    [random_char("abcdefghijklmnopqrstuvwxyz0123456789_")
     || _ <- lists:seq(1, Len - 1)]
    ++ [random_char("abcdefghijklmnopqrstuvwxyz0123456789")].

pad_seq(N) ->
    lists:flatten(io_lib:format("~4..0w", [N])).

random_char(Chars) ->
    lists:nth(rand:uniform(length(Chars)), Chars).

random_garbage() ->
    Len = rand:uniform(30),
    [rand:uniform(127) || _ <- lists:seq(1, Len)].

random_disk_files() ->
    Count = rand:uniform(10),
    Plugin = random_plugin_name(),
    [Plugin ++ "_" ++ pad_seq(rand:uniform(999)) ++ "_"
     ++ random_descr() ++ ".sql"
     || _ <- lists:seq(1, Count)].

random_applied_seqs() ->
    Count = rand:uniform(5),
    [rand:uniform(999) || _ <- lists:seq(1, Count)].

random_map(Depth) when Depth =< 0 ->
    random_primitive();
random_map(Depth) ->
    case rand:uniform(4) of
        1 -> random_primitive();
        2 ->
            #{random_atom() => random_map(Depth - 1)};
        3 ->
            [random_map(Depth - 1) || _ <- lists:seq(1, rand:uniform(3))];
        4 ->
            #{random_atom() => random_map(Depth - 1),
              random_atom() => random_map(Depth - 1)}
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
