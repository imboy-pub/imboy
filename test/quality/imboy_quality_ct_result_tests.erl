-module(imboy_quality_ct_result_tests).

-include_lib("eunit/include/eunit.hrl").

real_return_counts_test() ->
    ?assertMatch(
        #{
            verdict := pass,
            passed_cases := 3,
            failed_cases := 0,
            user_skip := 0,
            auto_skip := 0,
            skip := 0
        },
        imboy_quality_ct:classify_detailed({3, 0, {0, 0}})
    ).

failure_and_skips_are_not_pass_test() ->
    Cases = [
        {{0, 0, {0, 0}}, zero_testcases},
        {{3, 1, {0, 0}}, {failed, 1}},
        {{3, 0, {1, 0}}, {auto_or_user_skip, 1}},
        {{3, 0, {0, 2}}, {auto_or_user_skip, 2}},
        {{error, timeout}, {ct_error, timeout}},
        {{3, 0, 0}, {unexpected_ct_result, {3, 0, 0}}},
        {{-1, 0, {0, 0}}, {unexpected_ct_result, {-1, 0, {0, 0}}}}
    ],
    lists:foreach(
        fun({Input, Reason}) ->
            ?assertEqual({fail, Reason}, imboy_quality_ct:classify(Input)),
            ?assertMatch(#{verdict := fail}, imboy_quality_ct:classify_detailed(Input))
        end,
        Cases
    ).

critical_report_remains_fail_closed_test() ->
    Good = imboy_quality_ct:classify_detailed({1, 0, {0, 0}}),
    Results = [
        {S, Good}
     || S <- [
            msg_delivery_SUITE,
            msg_ack_logic_SUITE,
            imboy_quality_ct02_org_boundary_SUITE,
            imboy_quality_ct03_migration_SUITE
        ]
    ],
    ?assertEqual(
        {error, candidate_and_cleanup_binding_not_implemented},
        imboy_quality_ct:critical_report(Results, #{})
    ),
    ?assertEqual(
        {error, incomplete_or_failed_critical},
        imboy_quality_ct:critical_report(tl(Results), #{})
    ).

report_names_do_not_overlap_test() ->
    ?assertNotEqual(
        imboy_quality_ct:report_filename(critical, <<"run1">>),
        imboy_quality_ct:report_filename(ct04, <<"run1">>)
    ),
    ?assertMatch({error, _}, imboy_quality_ct:report_filename(critical, <<"../bad">>)).
