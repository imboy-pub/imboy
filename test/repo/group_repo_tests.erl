-module(group_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 imboy_db → elib_pg 迁移的语义正确性
%%% 使用 meck mock，不依赖真实数据库
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_public_prefix_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]}
        ],
        fun() ->
            Result = group_repo:tablename(),
            ?assertEqual(<<"public.group">>, Result)
        end
    ).

%% ===================================================================
%% add/2 测试
%% ===================================================================

add_valid_group_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(_Table) -> 600001 end}
            ]},
            {elib_pg, [
                {'query', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Data = #{
                owner_uid => 1,
                creator_uid => 1,
                type => 1,
                join_limit => 1,
                user_id_sum => 1,
                created_at => 1700000000
            },
            Result = group_repo:add(undefined, Data),
            ?assertEqual({ok, 600001}, Result)
        end
    ).

add_error_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(_Table) -> 600002 end}
            ]},
            {elib_pg, [
                {'query', 3, fun(_Conn, _Sql, _Params) -> {error, unique_violation} end}
            ]}
        ],
        fun() ->
            Data = #{
                owner_uid => 1,
                created_at => 1700000000
            },
            Result = group_repo:add(undefined, Data),
            ?assertMatch({error, _}, Result)
        end
    ).

%% 回归：真库集成测试实测复现 PG 42701 "column id specified more than
%% once"。normalize_legacy_create_data/1 用 atom `id` key 承载调用方传入
%% 的 id/gid，add/2 曾用 binary <<"id">> key 无条件覆盖——两个 key 类型
%% 不同、Erlang map 不会互相覆盖，elib_pg_sql:insert/2 拼出的 INSERT
%% 语句里 "id" 列因此重复两次（同款 bug 见 user_repo_tests.erl）。
add_with_atom_id_key_does_not_duplicate_id_column_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(_Table) -> 600003 end}
            ]},
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, _Params) ->
                    put(captured_sql, iolist_to_binary(Sql)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                id => 999,
                owner_uid => 1,
                created_at => 1700000000
            },
            Result = group_repo:add(undefined, Data),
            ?assertEqual({ok, 600003}, Result),
            Sql = get(captured_sql),
            [_, Rest] = binary:split(Sql, <<"(">>),
            [ColsPart, _] = binary:split(Rest, <<")">>),
            Cols = binary:split(ColsPart, <<",">>, [global]),
            ?assertEqual(1, length([C || C <- Cols, C =:= <<"id">>]))
        end
    ).

%% ===================================================================
%% find_by_id/2 测试
%% ===================================================================

find_by_id_existing_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"id">> => 1, <<"owner_uid">> => 100}}
                end}
            ]}
        ],
        fun() ->
            Gid = 1,
            Column = <<"id, owner_uid">>,
            Result = group_repo:find_by_id(Gid, Column),
            ?assertMatch(#{<<"id">> := _, <<"owner_uid">> := _}, Result)
        end
    ).

find_by_id_not_existing_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            Gid = 999999,
            Column = <<"id">>,
            Result = group_repo:find_by_id(Gid, Column),
            ?assertMatch({error, _}, Result)
        end
    ).

find_by_id_all_columns_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"id">> => 1, <<"title">> => <<"test">>, <<"status">> => 1}}
                end}
            ]}
        ],
        fun() ->
            Gid = 1,
            Column = <<"*">>,
            Result = group_repo:find_by_id(Gid, Column),
            ?assertMatch(#{<<"id">> := _}, Result)
        end
    ).

find_by_id_binary_id_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'one', 2, fun(_Sql, _Params) ->
                    {ok, #{<<"id">> => 1}}
                end}
            ]}
        ],
        fun() ->
            Gid = <<"1">>,
            Column = <<"id">>,
            Result = group_repo:find_by_id(Gid, Column),
            ?assertMatch(#{<<"id">> := _}, Result)
        end
    ).

%% ===================================================================
%% list_by_ids/2 测试
%% ===================================================================

list_by_ids_non_empty_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [#{<<"id">> => 1}, #{<<"id">> => 2}, #{<<"id">> => 3}]}
                end}
            ]}
        ],
        fun() ->
            Ids = [1, 2, 3],
            Column = <<"id">>,
            Result = group_repo:list_by_ids(Ids, Column),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

list_by_ids_empty_list_test_() ->
    fun() ->
        Ids = [],
        Column = <<"id">>,
        Result = group_repo:list_by_ids(Ids, Column),
        ?assertEqual({ok, []}, Result)
    end.

list_by_ids_all_columns_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [#{<<"id">> => 1, <<"title">> => <<"test">>}]}
                end}
            ]}
        ],
        fun() ->
            Ids = [1],
            Column = <<"*">>,
            Result = group_repo:list_by_ids(Ids, Column),
            case Result of
                {ok, List} when is_list(List) -> ?assert(true);
                {ok, _} -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, List}")
            end
        end
    ).

list_by_ids_with_duplicates_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [#{<<"id">> => 1}, #{<<"id">> => 2}]}
                end}
            ]}
        ],
        fun() ->
            Ids = [1, 1, 2],
            Column = <<"id">>,
            Result = group_repo:list_by_ids(Ids, Column),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

%% ===================================================================
%% list_by_uid/2 测试
%% ===================================================================

list_by_uid_default_limit_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            Uid = 1,
            Column = <<"id">>,
            Result = group_repo:list_by_uid(Uid, Column),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

list_by_uid_custom_limit_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {ok, [#{<<"id">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            Uid = 1,
            Column = <<"id">>,
            Limit = 5,
            Result = group_repo:list_by_uid(Uid, Column, Limit),
            ?assertMatch({ok, [_ | _]}, Result)
        end
    ).

list_by_uid_zero_limit_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Uid = 1,
            Column = <<"id">>,
            Limit = 0,
            Result = group_repo:list_by_uid(Uid, Column, Limit),
            ?assertMatch({ok, []}, Result)
        end
    ).

list_by_uid_non_existing_user_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Uid = 999999,
            Column = <<"id">>,
            Limit = 10,
            Result = group_repo:list_by_uid(Uid, Column, Limit),
            ?assertMatch({ok, []}, Result)
        end
    ).
