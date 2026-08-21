-module(channel_admin_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc channel_admin_repo 的 repo 层单元测试（基于 mock，无数据库依赖）
%%%===================================================================

tablename_returns_public_channel_admin_table_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% eunit_runner 未加载时 sql_driver 未设，public_tablename 不加前缀
        ?assertEqual(<<"channel_admin">>, channel_admin_repo:tablename())
    end).

find_returns_row_when_exists_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(Sql, [11, 1001]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"FROM channel_admin">>) =/= nomatch),
                    {ok, #{<<"channel_id">> => 11, <<"user_id">> => 1001, <<"role">> => 2}}
                end}
            ]}
        ],
        fun() ->
            Result = channel_admin_repo:find(11, 1001),
            ?assertEqual(2, maps:get(<<"role">>, Result))
        end
    ).

find_returns_empty_map_when_missing_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, [11, 1001]) ->
                    {error, not_found}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(#{}, channel_admin_repo:find(11, 1001))
        end
    ).

is_admin_returns_true_when_role_exists_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, [11, 1001]) ->
                    {ok, #{<<"role">> => 1}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(true, channel_admin_repo:is_admin(11, 1001))
        end
    ).

get_role_returns_zero_when_not_admin_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'one', 2, fun(_Sql, [11, 2002]) ->
                    {error, not_found}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(0, channel_admin_repo:get_role(11, 2002))
        end
    ).

update_role_calls_update_with_expected_filter_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'update', 4, fun(
                    _Table, #{role := 3}, <<"channel_id = $1 AND user_id = $2">>, [11, 2002]
                ) ->
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, 1}, channel_admin_repo:update_role(11, 2002, 3))
        end
    ).

add_with_conn_uses_sql_insert_builder_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(channel_admin) -> 501 end}
            ]},
            {elib_pg, [
                {'query', 3, fun(conn_pid, _Sql, _Params) ->
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Data = #{channel_id => 11, user_id => 1001, role => 2},
            ?assertEqual({ok, 501}, channel_admin_repo:add(conn_pid, Data))
        end
    ).

list_by_channel_orders_by_role_desc_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [11]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"ORDER BY ca.role DESC">>) =/= nomatch),
                    {ok, [#{<<"user_id">> => 1001, <<"role">> => 3}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, [#{<<"role">> := 3}]}, channel_admin_repo:list_by_channel(11))
        end
    ).
