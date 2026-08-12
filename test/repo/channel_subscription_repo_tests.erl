-module(channel_subscription_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(PG_SCHEMA_MOCK,
    {config_ds, [
        {'env', 1, fun
            (sql_driver) -> pgsql;
            (_) -> undefined
        end}
    ]}
).

%%%===================================================================
%%% @doc channel_subscription_repo 的 repo 层单元测试（基于 mock，无数据库依赖）
%%%===================================================================

tablename_returns_public_channel_subscription_table_test_() ->
    ?WITH_MECKS([?PG_SCHEMA_MOCK], fun() ->
        ?assertEqual(<<"public.channel_subscription">>, channel_subscription_repo:tablename())
    end).

find_returns_row_when_subscription_exists_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, [11, 1001]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"status = 1 LIMIT 1">>) =/= nomatch),
                    {ok, #{<<"channel_id">> => 11, <<"user_id">> => 1001, <<"status">> => 1}}
                end}
            ]}
        ],
        fun() ->
            Result = channel_subscription_repo:find(11, 1001),
            ?assertEqual(1001, maps:get(<<"user_id">>, Result))
        end
    ).

find_returns_empty_map_when_subscription_missing_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, [11, 2002]) ->
                    {error, not_found}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(#{}, channel_subscription_repo:find(11, 2002))
        end
    ).

list_by_channel_first_page_uses_limit_clause_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [11, 50]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(
                        re:run(
                            SqlBin,
                            <<"ORDER BY cs.is_pinned DESC, cs.subscribed_at DESC LIMIT \\$2">>
                        ) =/= nomatch
                    ),
                    {ok, [#{<<"user_id">> => 1001}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, [#{<<"user_id">> := 1001}]},
                channel_subscription_repo:list_by_channel(11, 0, 50)
            )
        end
    ).

list_by_channel_next_page_uses_cursor_filter_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [11, 900, 20]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"id < \\$2">>) =/= nomatch),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, []}, channel_subscription_repo:list_by_channel(11, 900, 20))
        end
    ).

delete_soft_sets_status_zero_test_() ->
    ?WITH_MECKS(
        [
            ?PG_SCHEMA_MOCK,
            {elib_pg, [
                {'update', 4, fun(
                    <<"public.channel_subscription">>,
                    #{status := 0},
                    <<"channel_id = $1 AND user_id = $2 AND status = 1">>,
                    [11, 1001]
                ) ->
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 1}, channel_subscription_repo:delete(11, 1001))
        end
    ).

delete_with_conn_soft_sets_status_zero_test_() ->
    ?WITH_MECKS(
        [
            ?PG_SCHEMA_MOCK,
            {elib_pg, [
                {'update', 5, fun(
                    fake_conn,
                    <<"public.channel_subscription">>,
                    #{status := 0},
                    <<"channel_id = $1 AND user_id = $2 AND status = 1">>,
                    [11, 1001]
                ) ->
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 1}, channel_subscription_repo:delete(fake_conn, 11, 1001))
        end
    ).

upsert_active_returns_true_when_state_changes_test_() ->
    ?_test(begin
        [meck_helper:cleanup_mock(M) || M <- [elib_dt, elib_tsid, elib_pg]],
        ok = meck:new(elib_dt, [no_link]),
        ok = meck:new(elib_tsid, [no_link]),
        ok = meck:new(elib_pg, [no_link]),
        try
            meck:expect(elib_dt, now, 0, fun() -> <<"2026-02-22T00:00:00Z">> end),
            meck:expect(elib_tsid, generate, 1, fun(channel_subscription) -> 9001 end),
            meck:expect(elib_pg, query, 3, fun(
                fake_conn, Sql, [9001, 11, 1001, <<"2026-02-22T00:00:00Z">>]
            ) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(
                    re:run(SqlBin, <<"ON CONFLICT \\(channel_id, user_id\\) DO UPDATE">>) =/=
                        nomatch
                ),
                ?assert(re:run(SqlBin, <<"status <> 1">>) =/= nomatch),
                {ok, [#{<<"changed">> => 1}]}
            end),
            ?assertEqual({ok, true}, channel_subscription_repo:upsert_active(fake_conn, 11, 1001))
        after
            meck:unload([elib_pg, elib_tsid, elib_dt])
        end
    end).

upsert_active_returns_true_when_driver_returns_counted_rows_test_() ->
    ?_test(begin
        [meck_helper:cleanup_mock(M) || M <- [elib_dt, elib_tsid, elib_pg]],
        ok = meck:new(elib_dt, [no_link]),
        ok = meck:new(elib_tsid, [no_link]),
        ok = meck:new(elib_pg, [no_link]),
        try
            meck:expect(elib_dt, now, 0, fun() -> <<"2026-02-22T00:00:00Z">> end),
            meck:expect(elib_tsid, generate, 1, fun(channel_subscription) -> 9002 end),
            meck:expect(elib_pg, query, 3, fun(
                fake_conn, _Sql, [9002, 11, 1001, <<"2026-02-22T00:00:00Z">>]
            ) ->
                {ok, [#{<<"changed">> => 1}]}
            end),
            ?assertEqual({ok, true}, channel_subscription_repo:upsert_active(fake_conn, 11, 1001))
        after
            meck:unload([elib_pg, elib_tsid, elib_dt])
        end
    end).

upsert_active_returns_false_when_already_active_test_() ->
    ?_test(begin
        [meck_helper:cleanup_mock(M) || M <- [elib_dt, elib_tsid, elib_pg]],
        ok = meck:new(elib_dt, [no_link]),
        ok = meck:new(elib_tsid, [no_link]),
        ok = meck:new(elib_pg, [no_link]),
        try
            meck:expect(elib_dt, now, 0, fun() -> <<"2026-02-22T00:00:00Z">> end),
            meck:expect(elib_tsid, generate, 1, fun(channel_subscription) -> 9003 end),
            meck:expect(elib_pg, query, 3, fun(
                fake_conn, _Sql, [9003, 11, 1001, <<"2026-02-22T00:00:00Z">>]
            ) ->
                {ok, []}
            end),
            ?assertEqual({ok, false}, channel_subscription_repo:upsert_active(fake_conn, 11, 1001))
        after
            meck:unload([elib_pg, elib_tsid, elib_dt])
        end
    end).

increment_unread_executes_and_returns_ok_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'execute', 2, fun(Sql, [11]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"unread_count = unread_count \\+ 1">>) =/= nomatch),
                    {ok, 3}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, channel_subscription_repo:increment_unread(11))
        end
    ).

clear_unread_sets_zero_and_updates_last_read_at_test_() ->
    ?WITH_MECKS(
        [
            ?PG_SCHEMA_MOCK,
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {elib_pg, [
                {'update', 4, fun(
                    <<"public.channel_subscription">>,
                    #{unread_count := 0, last_read_at := 1700000000000},
                    <<"channel_id = $1 AND status = 1">>,
                    [11]
                ) ->
                    {ok, 5}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 5}, channel_subscription_repo:clear_unread(11))
        end
    ).

count_unread_returns_zero_when_query_fails_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, [1001]) ->
                    {error, db_down}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(0, channel_subscription_repo:count_unread(1001))
        end
    ).

get_unread_count_returns_value_when_row_exists_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, [11, 1001]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"SELECT unread_count">>) =/= nomatch),
                    {ok, #{<<"unread_count">> => 6}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(6, channel_subscription_repo:get_unread_count(11, 1001))
        end
    ).

list_unread_by_uid_queries_only_positive_unread_rows_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [1001]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"unread_count > 0">>) =/= nomatch),
                    {ok, [#{<<"channel_id">> => 11, <<"unread_count">> => 3}]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, [#{<<"channel_id">> => 11, <<"unread_count">> => 3}]},
                channel_subscription_repo:list_unread_by_uid(1001)
            )
        end
    ).

list_unread_counts_by_channel_includes_zero_rows_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [11]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"ORDER BY user_id ASC">>) =/= nomatch),
                    {ok, [
                        #{<<"user_id">> => 1001, <<"unread_count">> => 0},
                        #{<<"user_id">> => 1002, <<"unread_count">> => 5}
                    ]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, [
                    #{<<"user_id">> => 1001, <<"unread_count">> => 0},
                    #{<<"user_id">> => 1002, <<"unread_count">> => 5}
                ]},
                channel_subscription_repo:list_unread_counts_by_channel(11)
            )
        end
    ).

is_subscribed_checks_existence_semantics_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, [11, 1001]) ->
                    {ok, #{<<"exists">> => 1}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(true, channel_subscription_repo:is_subscribed(11, 1001))
        end
    ).

is_subscribed_returns_false_when_subscription_missing_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, [11, 1001]) ->
                    {ok, #{}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(false, channel_subscription_repo:is_subscribed(11, 1001))
        end
    ).

%% P0-1: 验证 clear_unread/2 包含 user_id 维度，仅清除指定用户的未读
clear_unread_with_uid_includes_user_in_where_clause_test_() ->
    ?WITH_MECKS(
        [
            ?PG_SCHEMA_MOCK,
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {elib_pg, [
                {'update', 4, fun(
                    <<"public.channel_subscription">>,
                    #{unread_count := 0, last_read_at := 1700000000000},
                    WhereClause,
                    [11, 1001]
                ) ->
                    %% 验证 WHERE 子句包含 channel_id 和 user_id
                    ?assert(re:run(WhereClause, <<"user_id">>) =/= nomatch),
                    ?assert(re:run(WhereClause, <<"channel_id">>) =/= nomatch),
                    %% 验证参数顺序正确：[ChannelId, Uid]
                    ?assertEqual(11, hd([11, 1001])),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            %% 调用新的 clear_unread/2，仅清除指定用户的未读
            ?assertEqual({ok, 1}, channel_subscription_repo:clear_unread(11, 1001))
        end
    ).

%% P0-1: 验证 clear_unread/2 不影响其他用户
clear_unread_with_uid_does_not_affect_other_users_test_() ->
    ?WITH_MECKS(
        [
            ?PG_SCHEMA_MOCK,
            {elib_dt, [
                {'now', 0, fun() -> 1700000000000 end}
            ]},
            {elib_pg, [
                {'update', 4, fun(<<"public.channel_subscription">>, _, WhereClause, Params) ->
                    %% WHERE 子句必须包含 user_id，不能只按 channel_id 清除
                    case re:run(WhereClause, <<"user_id">>) of
                        nomatch -> erlang:error(where_clause_missing_user_id);
                        _ -> ok
                    end,
                    %% 参数必须包含 user_id
                    ?assertEqual(2, length(Params)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            %% 调用 clear_unread/2，确认不会影响其他用户
            ?assertMatch({ok, _}, channel_subscription_repo:clear_unread(11, 1001))
        end
    ).
