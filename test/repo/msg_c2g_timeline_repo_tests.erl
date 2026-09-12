-module(msg_c2g_timeline_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2g_timeline_repo 模块的 EUnit 测试
%%%
%%% 目标：验证群组消息时间线数据访问层功能
%%% 覆盖：时间线查询
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]}
        ],
        fun() ->
            Result = msg_c2g_timeline_repo:tablename(),
            ?assertEqual(<<"public.msg_c2g_timeline">>, Result)
        end
    ).

list_by_uid_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, [1, 10000000]) -> {ok, []} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, []}, msg_c2g_timeline_repo:list_by_uid(1, <<"tl.msg_id">>))
        end
    ).

client_ack_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(_Sql, [1, <<"msg_123">>]) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 1}, msg_c2g_timeline_repo:client_ack(1, <<"msg_123">>))
        end
    ).

authorized_pending_query_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    assert_generation_acl(Sql),
                    ?assertEqual([42, 25], Params),
                    {ok, [#{<<"msg_id">> => <<"new-generation">>}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, [#{<<"msg_id">> := <<"new-generation">>}]},
                msg_c2g_timeline_repo:list_by_uid(42, <<"tl.msg_id">>, 25)
            )
        end
    ).

authorized_pending_since_query_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    assert_generation_acl(Sql),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"tl.created_at >= $2">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"LIMIT $3">>)),
                    ?assertEqual([42, <<"2026-09-11T00:00:00Z">>, 25], Params),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, []},
                msg_c2g_timeline_repo:list_by_uid_since(
                    42, <<"tl.msg_id, tl.created_at">>, 25, <<"2026-09-11T00:00:00Z">>
                )
            )
        end
    ).

authorized_pending_count_uses_same_acl_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, Params) ->
                    assert_generation_acl(Sql),
                    ?assertEqual(nomatch, binary:match(Sql, <<"LIMIT">>)),
                    ?assertEqual([42], Params),
                    {ok, [#{<<"count">> => 3}]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(3, msg_c2g_timeline_repo:count_pending_by_uid(42, undefined))
        end
    ).

assert_generation_acl(Sql) ->
    [
        ?assertNotEqual(nomatch, binary:match(Sql, Fragment))
     || Fragment <- [
            <<"grp.status = 1">>,
            <<"gm.status = 1">>,
            <<"gmg.end_seq IS NULL">>,
            <<"tl.to_uid = $1">>,
            <<"tl.client_ack = false">>,
            <<"tl.conv_seq IS NOT NULL">>,
            <<"tl.conv_seq >= gmg.start_seq">>
        ]
    ].
