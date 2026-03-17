-module(msg_store_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_store_ds public contract tests
%%%
%%% Keep this suite on the stable public surface. Earlier versions reached
%%% into callbacks like init/1 and handle_info/2, which introduced timers and
%%% logger side effects that made the full EUnit run hang during teardown.
%%%===================================================================

start_link_function_exists_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(is_function(fun msg_store_ds:start_link/0, 0))
    end).

stage_success_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> {ok, 1} end}
    ], fun() ->
        Result = msg_store_ds:stage(
            <<"c2c">>,
            <<"msg_123">>,
            <<"text">>,
            <<>>,
            #{},
            <<"{\"content\":\"hello\"}">>,
            1,
            2,
            <<"2023-01-01T00:00:00Z">>,
            <<"2023-01-01T00:00:00Z">>
        ),
        ?assertEqual(ok, Result)
    end).

stage_duplicate_is_idempotent_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) ->
            {error, {unique_violation, <<"msg_123">>}}
        end}
    ], fun() ->
        Result = msg_store_ds:stage(
            <<"c2c">>,
            <<"msg_123">>,
            <<"text">>,
            <<>>,
            #{},
            <<"{\"content\":\"hello\"}">>,
            1,
            2,
            <<"2023-01-01T00:00:00Z">>,
            <<"2023-01-01T00:00:00Z">>
        ),
        ?assertEqual(ok, Result)
    end).

stage_error_returns_error_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) ->
            {error, database_connection_lost}
        end}
    ], fun() ->
        Result = msg_store_ds:stage(
            <<"c2c">>,
            <<"msg_123">>,
            <<"text">>,
            <<>>,
            #{},
            <<"{\"content\":\"hello\"}">>,
            1,
            2,
            <<"2023-01-01T00:00:00Z">>,
            <<"2023-01-01T00:00:00Z">>
        ),
        ?assertEqual(error, Result)
    end).

stage_preserves_group_recipient_list_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_, _, _, _, _, _, _, ToIdList, _, _) ->
            ?assertEqual([2, 3, 4], ToIdList),
            {ok, 1}
        end}
    ], fun() ->
        Result = msg_store_ds:stage(
            <<"c2g">>,
            <<"msg_456">>,
            <<"text">>,
            <<>>,
            #{},
            <<"{\"content\":\"hello group\"}">>,
            1,
            [2, 3, 4],
            <<"2023-01-01T00:00:00Z">>,
            <<"2023-01-01T00:00:00Z">>
        ),
        ?assertEqual(ok, Result)
    end).

stage_preserves_s2c_action_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'stage', 10, fun(_, _, _, Action, _, _, _, _, _, _) ->
            ?assertEqual(<<"pull_offline_msg">>, Action),
            {ok, 1}
        end}
    ], fun() ->
        Result = msg_store_ds:stage(
            <<"s2c">>,
            <<"msg_789">>,
            <<"system">>,
            <<"pull_offline_msg">>,
            #{},
            <<"{\"msg_ids\":[\"msg1\",\"msg2\"]}">>,
            0,
            1,
            <<"2023-01-01T00:00:00Z">>,
            <<"2023-01-01T00:00:00Z">>
        ),
        ?assertEqual(ok, Result)
    end).

enqueue_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = msg_store_ds:enqueue(
            <<"c2c">>,
            <<"msg_123">>,
            #{payload => <<"{\"content\":\"hello\"}">>}
        ),
        ?assertEqual(ok, Result)
    end).

unstage_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = msg_store_ds:unstage(<<"msg_123">>),
        ?assertEqual(ok, Result)
    end).

len_reads_pending_binary_key_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {ok, #{<<"pending">> => 100, <<"processed">> => 5000}}
        end}
    ], fun() ->
        ?assertEqual(100, msg_store_ds:len())
    end).

len_reads_pending_atom_key_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {ok, #{pending => 50}}
        end}
    ], fun() ->
        ?assertEqual(50, msg_store_ds:len())
    end).

len_returns_zero_on_error_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {error, database_error}
        end}
    ], fun() ->
        ?assertEqual(0, msg_store_ds:len())
    end).

status_returns_queue_stats_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {ok, #{<<"pending">> => 100, <<"processed">> => 5000, <<"total">> => 5100}}
        end}
    ], fun() ->
        Result = msg_store_ds:status(),
        ?assertMatch(#{
            queue_len := 100,
            staging_stats := {ok, #{<<"pending">> := 100, <<"processed">> := 5000, <<"total">> := 5100}}
        }, Result)
    end).

status_returns_zero_queue_on_error_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'get_staging_stats', 0, fun() ->
            {error, database_error}
        end}
    ], fun() ->
        Result = msg_store_ds:status(),
        ?assertMatch(#{queue_len := 0, staging_stats := {error, database_error}}, Result)
    end).
