-module(imboy_quality_exit_result_tests).
-include_lib("eunit/include/eunit.hrl").

group_rejects_duplicate_and_empty_selectors_test() ->
    ?assertEqual([alpha, beta], imboy_quality_cli:parse_group("alpha, beta")),
    ?assertException(error, {badmatch, false}, imboy_quality_cli:parse_group("alpha,alpha")),
    ?assertException(error, {badmatch, false}, imboy_quality_cli:parse_group("alpha,,beta")),
    ?assertException(error, badarg, imboy_quality_cli:parse_group("unregistered_root_072f1")).

failure_skip_cancel_never_succeed_test() ->
    Base = #{<<"fail">> => 0, <<"skip">> => 0, <<"cancel">> => 0},
    ?assertEqual(0, imboy_quality_cli:result_exit_code(Base)),
    ?assertEqual(1, imboy_quality_cli:result_exit_code(Base#{<<"fail">> := 1})),
    ?assertEqual(3, imboy_quality_cli:result_exit_code(Base#{<<"cancel">> := 1})),
    ?assertEqual(3, imboy_quality_cli:result_exit_code(Base#{<<"skip">> := 1})),
    ?assertEqual(2, imboy_quality_cli:result_exit_code(maps:remove(<<"skip">>, Base))).

group_is_partial_and_requires_complete_counts_and_cleanup_test() ->
    G = #{
        <<"op">> => <<"eunit_group">>,
        <<"scope">> => <<"PARTIAL">>,
        <<"run_id">> => <<"run">>,
        <<"cleanup">> => #{<<"ok">> => true},
        <<"roots_total">> => 2,
        <<"roots_ok">> => 2,
        <<"roots_missing">> => 0,
        <<"roots_failed">> => 0,
        <<"pass">> => 3,
        <<"fail">> => 0,
        <<"skip">> => 0,
        <<"cancel">> => 0
    },
    ?assertEqual(0, imboy_quality_cli:group_result_exit_code(G)),
    lists:foreach(
        fun(Bad) -> ?assertEqual(2, imboy_quality_cli:group_result_exit_code(Bad)) end,
        [
            G#{<<"scope">> := <<"FULL">>},
            G#{<<"skip">> := 1},
            G#{<<"cancel">> := 1},
            G#{<<"roots_missing">> := 1},
            G#{<<"cleanup">> := #{<<"ok">> => false}},
            G#{<<"roots_total">> := 0, <<"roots_ok">> := 0},
            maps:remove(<<"fail">>, G)
        ]
    ).
