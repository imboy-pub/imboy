-module(eunit_runner_retry_tests).
-include_lib("eunit/include/eunit.hrl").

persistent_dirty_exhausts_budget_test() ->
    ?assertEqual(25, attempts(4, 0, {imboy, {migration_dirty, 198}})).

ordinary_start_failure_exhausts_short_budget_test() ->
    ?assertEqual(4, attempts(4, 0, {imboy, eaddrinuse})).

late_dirty_does_not_reset_total_attempts_test() ->
    ?assertEqual(22, eunit_runner:boot_retry_remaining(2, 2, {migration_dirty, 198})),
    ?assertEqual(25, attempts(2, 2, {migration_dirty, 198})).

final_dirty_failure_has_no_remaining_attempt_test() ->
    ?assertEqual(0, eunit_runner:boot_retry_remaining(1, 24, {migration_dirty, 198})).

attempts(0, Done, _Reason) ->
    Done;
attempts(Left, Done, Reason) when Done < 30 ->
    Next = eunit_runner:boot_retry_remaining(Left, Done, Reason),
    attempts(Next, Done + 1, Reason).
