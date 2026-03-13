-module(channel_logic_subscription_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

get_unread_summary_success_filters_invalid_rows_test_() ->
    ?WITH_MECKS([
        {channel_subscription_repo, [
            {'count_unread', 1, fun(1001) -> 9 end},
            {'count_unread_channels', 1, fun(1001) -> 2 end},
            {'list_unread_by_uid', 1, fun(1001) ->
                {ok, [
                    #{<<"channel_id">> => 11, <<"unread_count">> => 8},
                    #{<<"channel_id">> => 12, <<"unread_count">> => 0},
                    #{<<"channel_id">> => -1, <<"unread_count">> => 3},
                    #{<<"channel_id">> => 13, <<"unread_count">> => <<"bad">>}
                ]}
            end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_11">> end}
        ]}
    ], fun() ->
        Result = channel_logic_subscription:get_unread_summary(1001),
        ?assertMatch(
            {ok, #{
                <<"total_unread">> := 9,
                <<"unread_channels">> := 2,
                <<"channels">> := [#{<<"channel_id">> := <<"ch_11">>, <<"unread_count">> := 8}]
            }},
            Result
        ),
        ?assertEqual(1, meck:num_calls(elib_hashids, encode, 1))
    end).

get_unread_summary_returns_error_when_repo_returns_error_test_() ->
    ?WITH_MECKS([
        {channel_subscription_repo, [
            {'count_unread', 1, fun(1001) -> 0 end},
            {'count_unread_channels', 1, fun(1001) -> 0 end},
            {'list_unread_by_uid', 1, fun(1001) ->
                {error, db_down}
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"db_down">>},
            channel_logic_subscription:get_unread_summary(1001)
        )
    end).

get_unread_summary_returns_error_when_repo_returns_unexpected_payload_test_() ->
    ?WITH_MECKS([
        {channel_subscription_repo, [
            {'count_unread', 1, fun(1001) -> 0 end},
            {'count_unread_channels', 1, fun(1001) -> 0 end},
            {'list_unread_by_uid', 1, fun(1001) ->
                unexpected_payload
            end}
        ]}
    ], fun() ->
        Result = channel_logic_subscription:get_unread_summary(1001),
        ?assertMatch({error, _}, Result),
        {error, ErrorBin} = Result,
        ?assert(is_binary(ErrorBin))
    end).
