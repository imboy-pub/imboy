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
update_encodes_tags_as_jsonb_before_update_test_() ->
    ?WITH_MECKS(
        [
            {channel_repo, [
                {'update', 2, fun(11, Data) ->
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
            {channel_repo, [
                {'update', 2, fun(11, Data) ->
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
            {channel_repo, [
                {'update', 2, fun(11, Data) ->
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
