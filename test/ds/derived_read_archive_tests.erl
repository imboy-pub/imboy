-module(derived_read_archive_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

channel_clear_unread_tx_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'write_tx_or_skip', 2, fun({channel, 11}, WriteFun) ->
                    {written, WriteFun(fake_conn)}
                end}
            ]},
            {channel_subscription_repo, [
                {'clear_unread_tx', 3, fun(fake_conn, 11, 22) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 1}, channel_subscription_ds:clear_unread(11, 22))
        end
    ).

channel_clear_unread_archived_skips_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'write_tx_or_skip', 2, fun({channel, 11}, _WriteFun) -> skipped end}
            ]},
            {channel_subscription_repo, [
                {'clear_unread_tx', 3, fun(_, _, _) -> erlang:error(unexpected_write) end}
            ]}
        ],
        fun() ->
            ?assertEqual(skipped, channel_subscription_ds:clear_unread(11, 22))
        end
    ).

group_notice_read_tx_test_() ->
    Notice = #{<<"id">> => 33},
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'write_tx_or_skip', 2, fun({group_notice, 33}, WriteFun) ->
                    {written, WriteFun(fake_conn)}
                end}
            ]},
            {group_notice_repo, [
                {'increment_read_count_tx', 2, fun(fake_conn, 33) -> {ok, Notice} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, Notice}, group_notice_ds:mark_as_read(33))
        end
    ).

group_notice_read_archived_skips_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'write_tx_or_skip', 2, fun({group_notice, 33}, _WriteFun) -> skipped end}
            ]},
            {group_notice_repo, [
                {'increment_read_count_tx', 2, fun(_, _) -> erlang:error(unexpected_write) end}
            ]}
        ],
        fun() ->
            ?assertEqual(skipped, group_notice_ds:mark_as_read(33))
        end
    ).
