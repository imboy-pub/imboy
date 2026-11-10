-module(imboy_quality_budget_tests).
-include_lib("eunit/include/eunit.hrl").

fixture() ->
    #{
        <<"history">> => preserved,
        <<"approved_recovery_20261104">> =>
            #{
                <<"total_limit">> => 61,
                <<"historical_reserved">> => 35,
                <<"new_final_batches">> => 1,
                <<"stop_on_certification_failure">> => true,
                <<"unknown_additional_launch">> => false,
                <<"new_full_allowances">> => #{
                    <<"p1_revalidation">> => 4,
                    <<"certification">> => 20,
                    <<"incremental">> => 3,
                    <<"cold">> => 1
                },
                <<"new_full_counts">> => #{
                    <<"p1_revalidation">> => 0,
                    <<"certification">> => 0,
                    <<"incremental">> => 0,
                    <<"cold">> => 0
                },
                <<"new_full_started">> => 0,
                <<"remaining_new_full">> => 26,
                <<"new_full_runs">> => []
            }
    }.

reservation_test() ->
    {ok, S} = imboy_quality_budget:reserve(fixture(), cold, <<"one">>),
    ?assertEqual(preserved, maps:get(<<"history">>, S)),
    R = maps:get(<<"approved_recovery_20261104">>, S),
    ?assertEqual(1, maps:get(<<"new_full_started">>, R)),
    ?assertEqual(25, maps:get(<<"remaining_new_full">>, R)),
    ?assertEqual(1, maps:get(<<"cold">>, maps:get(<<"new_full_counts">>, R))),
    ?assertMatch({error, _}, imboy_quality_budget:reserve(S, cold, <<"one">>)),
    ?assertMatch({error, _}, imboy_quality_budget:reserve(S, cold, <<"two">>)),
    ?assertMatch({error, _}, imboy_quality_budget:reserve(S, incremental, <<"one">>)).

missing_context_test() ->
    ?assertMatch({error, _}, imboy_quality_budget:reserve(#{}, cold, <<"one">>)),
    S = fixture(),
    R = maps:get(<<"approved_recovery_20261104">>, S),
    S1 = S#{<<"approved_recovery_20261104">> := maps:remove(<<"unknown_additional_launch">>, R)},
    ?assertMatch({error, _}, imboy_quality_budget:reserve(S1, cold, <<"one">>)),
    S2 = S#{<<"approved_recovery_20261104">> := R#{<<"unknown_additional_launch">> := true}},
    ?assertMatch({error, _}, imboy_quality_budget:reserve(S2, cold, <<"one">>)),
    ?assertMatch({error, _}, imboy_quality_budget:reserve(S, bogus, <<"one">>)).

certification_stop_test() ->
    {ok, S} = imboy_quality_budget:reserve(fixture(), certification, <<"cert1">>),
    ?assertMatch({error, _}, imboy_quality_budget:reserve(S, certification, <<"cert2">>)),
    R = maps:get(<<"approved_recovery_20261104">>, S),
    [Run] = maps:get(<<"new_full_runs">>, R),
    Failed = S#{
        <<"approved_recovery_20261104">> := R#{
            <<"new_full_runs">> := [Run#{<<"status">> := <<"failed">>}]
        }
    },
    ?assertMatch({error, _}, imboy_quality_budget:reserve(Failed, certification, <<"cert2">>)),
    Passed = S#{
        <<"approved_recovery_20261104">> := R#{
            <<"new_full_runs">> := [Run#{<<"status">> := <<"passed">>}]
        }
    },
    ?assertMatch({ok, _}, imboy_quality_budget:reserve(Passed, certification, <<"cert2">>)).

invalid_total_test() ->
    S = fixture(),
    R = maps:get(<<"approved_recovery_20261104">>, S),
    Bad = S#{<<"approved_recovery_20261104">> := R#{<<"remaining_new_full">> := 0}},
    ?assertMatch({error, _}, imboy_quality_budget:reserve(Bad, cold, <<"one">>)).

total_exhaustion_test() ->
    %% 合计仍为 26（全局池硬上限）：p1 上调到 4 后，certification 由 19 降为 18，
    %% 否则总和变 27 会先撞全局池、测不到「分类额度耗尽」。
    Categories =
        lists:duplicate(4, p1_revalidation) ++
            lists:duplicate(18, certification) ++ lists:duplicate(3, incremental) ++ [cold],
    {Final, _} = lists:foldl(
        fun(C, {S, N}) ->
            Id = integer_to_binary(N),
            {ok, Reserved} = imboy_quality_budget:reserve(S, C, Id),
            R = maps:get(<<"approved_recovery_20261104">>, Reserved),
            Runs = maps:get(<<"new_full_runs">>, R),
            Completed = [
                case maps:get(<<"run_id">>, Run) of
                    Id -> Run#{<<"status">> := <<"passed">>};
                    _ -> Run
                end
             || Run <- Runs
            ],
            {
                Reserved#{<<"approved_recovery_20261104">> := R#{<<"new_full_runs">> := Completed}},
                N + 1
            }
        end,
        {fixture(), 1},
        Categories
    ),
    R = maps:get(<<"approved_recovery_20261104">>, Final),
    ?assertEqual(26, maps:get(<<"new_full_started">>, R)),
    ?assertEqual(0, maps:get(<<"remaining_new_full">>, R)),
    ?assertMatch({error, _}, imboy_quality_budget:reserve(Final, certification, <<"extra">>)).
