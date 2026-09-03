-module(channel_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% channel_ds 模块 EUnit（P0-2 幂等与计数一致性）
%%%===================================================================

subscribe_increments_counter_when_state_changes_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'upsert_active', 3, fun(fake_conn, 1, 100) -> {ok, true} end}
            ]},
            {channel_repo, [
                {'increment_subscribers', 3, fun(fake_conn, 1, 1) -> {ok, 1} end}
            ]},
            writable_guard_mock(),
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(Key) ->
                    case Key of
                        {channel_subs, 1} -> ok;
                        {channel, 1} -> ok;
                        _ -> erlang:error({unexpected_cache_key, Key})
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, channel_ds:subscribe(1, 100)),
            ?assertEqual(1, meck:num_calls(channel_repo, increment_subscribers, 3)),
            ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1))
        end
    ).

subscribe_is_idempotent_when_already_active_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'upsert_active', 3, fun(fake_conn, 1, 100) -> {ok, false} end}
            ]},
            {channel_repo, [
                {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(should_not_increment) end}
            ]},
            writable_guard_mock(),
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(Key) ->
                    case Key of
                        {channel_subs, 1} -> ok;
                        {channel, 1} -> ok;
                        _ -> erlang:error({unexpected_cache_key, Key})
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, channel_ds:subscribe(1, 100)),
            ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3)),
            ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1))
        end
    ).

unsubscribe_decrements_counter_when_state_changes_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'delete', 3, fun(fake_conn, 1, 100) -> {ok, 1} end}
            ]},
            {channel_repo, [
                {'increment_subscribers', 3, fun(fake_conn, 1, -1) -> {ok, 1} end}
            ]},
            writable_guard_mock(),
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(Key) ->
                    case Key of
                        {channel_subs, 1} -> ok;
                        {channel, 1} -> ok;
                        _ -> erlang:error({unexpected_cache_key, Key})
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, channel_ds:unsubscribe(1, 100)),
            ?assertEqual(1, meck:num_calls(channel_repo, increment_subscribers, 3)),
            ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1))
        end
    ).

unsubscribe_is_idempotent_when_already_inactive_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'delete', 3, fun(fake_conn, 1, 100) -> {ok, 0} end}
            ]},
            {channel_repo, [
                {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(should_not_increment) end}
            ]},
            writable_guard_mock(),
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(Key) ->
                    case Key of
                        {channel_subs, 1} -> ok;
                        {channel, 1} -> ok;
                        _ -> erlang:error({unexpected_cache_key, Key})
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, channel_ds:unsubscribe(1, 100)),
            ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3)),
            ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1))
        end
    ).

subscribe_returns_error_when_tx_aborts_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'upsert_active', 3, fun(fake_conn, 1, 100) -> {error, db_error} end}
            ]},
            {channel_repo, [
                {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(should_not_increment) end}
            ]},
            writable_guard_mock(),
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(_Key) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, <<"db_error">>}, channel_ds:subscribe(1, 100)),
            ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3)),
            ?assertEqual(0, meck:num_calls(imboy_cache, flush, 1))
        end
    ).

unsubscribe_returns_error_when_tx_aborts_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'delete', 3, fun(fake_conn, 1, 100) -> {error, db_error} end}
            ]},
            {channel_repo, [
                {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(should_not_increment) end}
            ]},
            writable_guard_mock(),
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(_Key) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, <<"db_error">>}, channel_ds:unsubscribe(1, 100)),
            ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3)),
            ?assertEqual(0, meck:num_calls(imboy_cache, flush, 1))
        end
    ).

subscribe_rejects_archived_workspace_in_same_transaction_test_() ->
    ?WITH_MECKS(
        [
            archived_guard_mock(),
            {channel_subscription_repo, [
                {'upsert_active', 3, fun(_, _, _) -> erlang:error(must_not_write) end}
            ]},
            {channel_repo, [
                {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(must_not_write) end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(_) -> erlang:error(must_not_flush) end}
            ]}
        ],
        fun() ->
            %% 980 稳定错误码必须以 tuple 原样透传（handler envelope 映射），不得压平
            ?assertEqual(
                {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}},
                channel_ds:subscribe(1, 100)
            )
        end
    ).

unsubscribe_rejects_archived_workspace_in_same_transaction_test_() ->
    ?WITH_MECKS(
        [
            archived_guard_mock(),
            {channel_subscription_repo, [
                {'delete', 3, fun(_, _, _) -> erlang:error(must_not_write) end}
            ]},
            {channel_repo, [
                {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(must_not_write) end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(_) -> erlang:error(must_not_flush) end}
            ]}
        ],
        fun() ->
            %% 980 稳定错误码必须以 tuple 原样透传（handler envelope 映射），不得压平
            ?assertEqual(
                {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}},
                channel_ds:unsubscribe(1, 100)
            )
        end
    ).

writable_guard_mock() ->
    {workspace_guard, [
        {'ensure_writable_tx', 2, fun(fake_conn, {channel, 1}) -> ok end},
        {'abort_on_error', 1, fun(ok) -> ok end}
    ]}.

archived_guard_mock() ->
    {workspace_guard, [
        {'ensure_writable_tx', 2, fun(fake_conn, {channel, 1}) ->
            {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}}
        end},
        {'abort_on_error', 1, fun
            (ok) -> ok;
            ({error, Reason}) -> throw({abort_tx, Reason})
        end}
    ]}.

is_subscribed_returns_true_when_subscription_exists_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'is_subscribed', 2, fun(_ChannelId, _Uid) -> true end}
            ]}
        ],
        fun() ->
            ?assertEqual(true, channel_ds:is_subscribed(1, 100))
        end
    ).

is_subscribed_returns_false_when_subscription_missing_test_() ->
    ?WITH_MECKS(
        [
            {channel_subscription_repo, [
                {'is_subscribed', 2, fun(_ChannelId, _Uid) -> false end}
            ]}
        ],
        fun() ->
            ?assertEqual(false, channel_ds:is_subscribed(1, 100))
        end
    ).

%% 回归：update 路径 tags 必须 jsonb 编码（对齐 create 路径 add_optional_fields），
%% 否则 epgsql 把 Erlang list 拼进 jsonb 参数导致 PG 22P02（频道更新必失败）。
%% P0 收口后 update 走 write_tx（归档守卫同事务）：mock 守卫 + with_tx + update_tx。
update_encodes_tags_as_jsonb_before_update_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'ensure_writable_tx', 2, fun(fake_conn, {channel, 11}) -> ok end},
                {'abort_on_error', 1, fun(ok) -> ok end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {channel_repo, [
                {'update_tx', 3, fun(fake_conn, 11, Data) ->
                    ?assertEqual(
                        [<<"a">>, <<"b">>],
                        jsone:decode(maps:get(<<"tags">>, Data))
                    ),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Result = channel_ds:update(11, #{
                <<"tags">> => [<<"a">>, <<"b">>],
                <<"name">> => <<"Channel X">>
            }),
            ?assertEqual({ok, 1}, Result)
        end
    ).

update_encodes_empty_tags_list_as_empty_json_array_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'ensure_writable_tx', 2, fun(fake_conn, {channel, 11}) -> ok end},
                {'abort_on_error', 1, fun(ok) -> ok end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {channel_repo, [
                {'update_tx', 3, fun(fake_conn, 11, Data) ->
                    ?assertEqual([], jsone:decode(maps:get(<<"tags">>, Data))),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Result = channel_ds:update(11, #{
                <<"tags">> => [],
                <<"avatar">> => <<"u1">>
            }),
            ?assertEqual({ok, 1}, Result)
        end
    ).

update_passes_through_text_fields_when_no_tags_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'ensure_writable_tx', 2, fun(fake_conn, {channel, 11}) -> ok end},
                {'abort_on_error', 1, fun(ok) -> ok end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {channel_repo, [
                {'update_tx', 3, fun(fake_conn, 11, Data) ->
                    ?assertEqual(#{<<"name">> => <<"Channel X">>}, Data),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Result = channel_ds:update(11, #{<<"name">> => <<"Channel X">>}),
            ?assertEqual({ok, 1}, Result)
        end
    ).

insert_reaction_retries_once_after_dead_connection_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'write_tx', 2, fun(_Target, Write) ->
                    case get(reaction_write_attempt) of
                        undefined ->
                            put(reaction_write_attempt, 1),
                            {error, dead_connection};
                        1 ->
                            put(reaction_write_attempt, 2),
                            Write(fake_conn)
                    end
                end}
            ]},
            {channel_repo, [
                {'insert_reaction_tx', 6, fun(fake_conn, 1, 2, 3, <<"like">>, 4) ->
                    {ok, 5}
                end}
            ]}
        ],
        fun() ->
            erase(reaction_write_attempt),
            ?assertEqual({ok, 5}, channel_ds:insert_reaction(1, 2, 3, <<"like">>, 4)),
            ?assertEqual(2, get(reaction_write_attempt))
        end
    ).

delete_reaction_retries_once_when_pool_is_temporarily_empty_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'write_tx', 2, fun(_Target, Write) ->
                    case get(reaction_delete_attempt) of
                        undefined ->
                            put(reaction_delete_attempt, 1),
                            {error, no_connection};
                        1 ->
                            put(reaction_delete_attempt, 2),
                            Write(fake_conn)
                    end
                end}
            ]},
            {channel_repo, [
                {'delete_reaction_tx', 5, fun(fake_conn, 1, 2, 3, <<"like">>) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            erase(reaction_delete_attempt),
            ?assertEqual({ok, 1}, channel_ds:delete_reaction(1, 2, 3, <<"like">>)),
            ?assertEqual(2, get(reaction_delete_attempt))
        end
    ).
