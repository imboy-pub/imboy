-module(imboy_quality_budget).
-export([reserve/3]).

reserve(State, Category0, RunId) ->
    try
        Category = category(Category0),
        true = is_binary(RunId) andalso byte_size(RunId) > 0,
        Recovery = maps:get(<<"approved_recovery_20261104">>, State),
        validate(Recovery),
        false = maps:get(<<"unknown_additional_launch">>, Recovery),
        Runs = maps:get(<<"new_full_runs">>, Recovery),
        false = lists:any(fun(R) -> maps:get(<<"run_id">>, R) =:= RunId end, Runs),
        Counts = maps:get(<<"new_full_counts">>, Recovery),
        Allowances = maps:get(<<"new_full_allowances">>, Recovery),
        true = maps:get(Category, Counts) < maps:get(Category, Allowances),
        true = maps:get(<<"remaining_new_full">>, Recovery) > 0,
        true = certification_ready(Category, Runs),
        Run = #{<<"run_id">> => RunId, <<"category">> => Category,
                <<"status">> => <<"started">>},
        NewRecovery = Recovery#{<<"new_full_runs">> := Runs ++ [Run],
            <<"new_full_counts">> := Counts#{Category := maps:get(Category, Counts) + 1},
            <<"new_full_started">> := maps:get(<<"new_full_started">>, Recovery) + 1,
            <<"remaining_new_full">> := maps:get(<<"remaining_new_full">>, Recovery) - 1},
        {ok, State#{<<"approved_recovery_20261104">> := NewRecovery}}
    catch
        _:_ -> {error, invalid_or_exhausted_budget}
    end.

category(p1_revalidation) -> <<"p1_revalidation">>;
category(certification) -> <<"certification">>;
category(incremental) -> <<"incremental">>;
category(cold) -> <<"cold">>;
category(B) when is_binary(B) ->
    true = lists:member(B, categories()), B.

categories() -> [<<"p1_revalidation">>, <<"certification">>, <<"incremental">>, <<"cold">>].

validate(R) ->
    61 = maps:get(<<"total_limit">>, R),
    35 = maps:get(<<"historical_reserved">>, R),
    1 = maps:get(<<"new_final_batches">>, R),
    true = maps:get(<<"stop_on_certification_failure">>, R),
    Unknown = maps:get(<<"unknown_additional_launch">>, R),
    true = is_boolean(Unknown),
    Allowances = maps:get(<<"new_full_allowances">>, R),
    Expected = #{<<"p1_revalidation">> => 2, <<"certification">> => 20,
                 <<"incremental">> => 3, <<"cold">> => 1},
    Expected = Allowances,
    Counts = maps:get(<<"new_full_counts">>, R),
    true = is_map(Counts) andalso map_size(Counts) =:= 4,
    Runs = maps:get(<<"new_full_runs">>, R),
    true = is_list(Runs),
    lists:foreach(fun validate_run/1, Runs),
    Ids = [maps:get(<<"run_id">>, Run) || Run <- Runs],
    true = length(Ids) =:= length(lists:usort(Ids)),
    lists:foreach(fun(C) ->
        N = maps:get(C, Counts),
        true = is_integer(N) andalso N >= 0 andalso N =< maps:get(C, Allowances),
        N = length([ok || Run <- Runs, maps:get(<<"category">>, Run) =:= C])
    end, categories()),
    Started = maps:get(<<"new_full_started">>, R),
    Remaining = maps:get(<<"remaining_new_full">>, R),
    true = is_integer(Started) andalso Started =:= length(Runs),
    true = is_integer(Remaining) andalso Remaining >= 0,
    26 = Started + Remaining,
    ok.

validate_run(Run) ->
    Id = maps:get(<<"run_id">>, Run),
    true = is_binary(Id) andalso byte_size(Id) > 0,
    _ = category(maps:get(<<"category">>, Run)),
    true = lists:member(maps:get(<<"status">>, Run),
                        [<<"started">>, <<"passed">>, <<"failed">>, <<"interrupted">>]),
    ok.

certification_ready(<<"certification">>, Runs) ->
    lists:all(fun(R) -> maps:get(<<"category">>, R) =/= <<"certification">> orelse
                         maps:get(<<"status">>, R) =:= <<"passed">> end, Runs);
certification_ready(_, _) -> true.
