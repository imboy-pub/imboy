-module(channel_logic_subscription_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

get_unread_summary_success_filters_invalid_rows_test_() ->
    ?WITH_MECKS(
        [
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
            ]}
        ],
        fun() ->
            Result = channel_logic_subscription:get_unread_summary(1001),
            ?assertMatch(
                {ok, #{
                    <<"total_unread">> := 9,
                    <<"unread_channels">> := 2,
                    <<"channels">> := [#{<<"channel_id">> := 11, <<"unread_count">> := 8}]
                }},
                Result
            )
        end
    ).

get_unread_summary_returns_error_when_repo_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'count_unread', 1, fun(1001) -> 0 end},
                {'count_unread_channels', 1, fun(1001) -> 0 end},
                {'list_unread_by_uid', 1, fun(1001) ->
                    {error, db_down}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"db_down">>},
                channel_logic_subscription:get_unread_summary(1001)
            )
        end
    ).

get_unread_summary_returns_error_when_repo_returns_unexpected_payload_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'count_unread', 1, fun(1001) -> 0 end},
                {'count_unread_channels', 1, fun(1001) -> 0 end},
                {'list_unread_by_uid', 1, fun(1001) ->
                    unexpected_payload
                end}
            ]}
        ],
        fun() ->
            Result = channel_logic_subscription:get_unread_summary(1001),
            ?assertMatch({error, _}, Result),
            {error, ErrorBin} = Result,
            ?assert(is_binary(ErrorBin))
        end
    ).

subscribe_preserves_workspace_archived_code_test_() ->
    %% 归档竞态兜底：入口前置检查通过后、DS 同事务守卫拒绝 {error,{980,Msg}}，
    %% logic 层必须保留 tuple 透传给 handler envelope，不得 safe_to_binary 压平。
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_id', 2, fun(11, _Cols) ->
                    #{<<"id">> => 11, <<"access_type">> => 0, <<"join_policy">> => 0}
                end},
                {'subscribe', 2, fun(11, 1001) ->
                    {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}}
                end}
            ]},
            {channel_logic_common, [
                {'guard_channel_writable', 1, fun(11) -> ok end}
            ]},
            {channel_logic_notify, [
                {'notify_channel_subscribed', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}},
                channel_logic_subscription:subscribe(1001, <<"11">>)
            )
        end
    ).

unsubscribe_preserves_workspace_archived_code_test_() ->
    %% 入口前置检查通过后、DS 事务内归档（竞态窗口）：980 仍须 tuple 透传。
    ?WITH_MECKS(
        [
            {channel_logic_common, [
                {'guard_channel_writable', 1, fun(11) -> ok end}
            ]},
            {channel_ds, [
                {'unsubscribe', 2, fun(11, 1001) ->
                    {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}}
                end}
            ]},
            {channel_logic_notify, [
                {'notify_channel_unsubscribed', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}},
                channel_logic_subscription:unsubscribe(1001, <<"11">>)
            )
        end
    ).
