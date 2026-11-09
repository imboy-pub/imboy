-module(imboy_quality_full_budget_tests).
-include_lib("eunit/include/eunit.hrl").

reservation_is_recorded_once_test() ->
    State = fixture(),
    RunId = <<"test-run-1">>,
    Identity = #{<<"candidate_sha">> => <<"synthetic">>},
    Next = imboy_quality_full_budget:full_transition(
        State, reserve, <<"p1_revalidation">>, RunId, Identity, <<"owner-hash">>, #{}
    ),
    Recovery = maps:get(<<"approved_recovery_20261104">>, Next),
    ?assertEqual(1, maps:get(<<"new_full_started">>, Recovery)),
    ?assertEqual(25, maps:get(<<"remaining_new_full">>, Recovery)),
    [Run] = maps:get(<<"new_full_runs">>, Recovery),
    ?assertEqual(false, maps:get(<<"claimed">>, Run)),
    ?assertEqual(false, maps:get(<<"build_complete">>, Run)),
    ?assertEqual(Identity, maps:get(<<"identity">>, Run)),
    ?assertException(
        error,
        _,
        imboy_quality_full_budget:full_transition(
            Next, reserve, <<"p1_revalidation">>, RunId, Identity, <<"owner-hash">>, #{}
        )
    ).

missing_or_unknown_budget_rejected_test() ->
    S = fixture(),
    R = maps:get(<<"approved_recovery_20261104">>, S),
    Unknown = S#{<<"approved_recovery_20261104">> := R#{<<"unknown_additional_launch">> := true}},
    ?assertException(
        error,
        _,
        imboy_quality_full_budget:full_transition(
            Unknown, reserve, <<"p1_revalidation">>, <<"x">>, #{}, <<"h">>, #{}
        )
    ),
    ?assertException(
        error,
        _,
        imboy_quality_full_budget:full_transition(
            #{}, reserve, <<"p1_revalidation">>, <<"x">>, #{}, <<"h">>, #{}
        )
    ).

fixture() ->
    Cats = [<<"p1_revalidation">>, <<"certification">>, <<"incremental">>, <<"cold">>],
    #{
        <<"checkpoint_sequence">> => 0,
        <<"approved_recovery_20261104">> => #{
            <<"total_limit">> => 61,
            <<"historical_reserved">> => 35,
            <<"new_final_batches">> => 1,
            <<"stop_on_certification_failure">> => true,
            <<"unknown_additional_launch">> => false,
            <<"new_full_allowances">> => #{
                <<"p1_revalidation">> => 3,
                <<"certification">> => 20,
                <<"incremental">> => 3,
                <<"cold">> => 1
            },
            <<"new_full_counts">> => maps:from_list([{K, 0} || K <- Cats]),
            <<"new_full_started">> => 0,
            <<"remaining_new_full">> => 26,
            <<"new_full_runs">> => []
        }
    }.
