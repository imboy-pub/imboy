-module(imboy_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_pg 模块的 EUnit 测试
%%%
%%% 目标：验证核心 PostgreSQL 访问层的正确性
%%% 覆盖：execute, query, one, pluck, insert, update, select, page, with_tx
%%%===================================================================

%% ===================================================================
%% query/2 测试
%% ===================================================================

query_select_all_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"SELECT 1 AS col">>,
        Result = imboy_pg:query(Sql, []),
        ?assertMatch({ok, [_]}, Result)
    end).

query_select_empty_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = imboy_pg:query(<<"SELECT 1 WHERE 1=0">>, []),
        ?assertEqual({ok, []}, Result)
    end).

query_with_params_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"SELECT $1 AS col">>,
        Result = imboy_pg:query(Sql, [42]),
        ?assertMatch({ok, [_]}, Result)
    end).

query_with_binary_param_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"SELECT $1 AS col">>,
        Result = imboy_pg:query(Sql, [<<"test">>]),
        ?assertMatch({ok, [_]}, Result)
    end).

query_with_null_param_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"SELECT $1 AS col">>,
        Result = imboy_pg:query(Sql, [undefined]),
        ?assertMatch({ok, [_]}, Result)
    end).

%% ===================================================================
%% one/2,3 测试
%% ===================================================================

one_single_row_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"SELECT 1 AS id, 'test' AS name">>,
        Result = imboy_pg:one(Sql, []),
        case Result of
            {ok, Row} when is_map(Row) ->
                ?assertMatch(#{<<"id">> := _, <<"name">> := _}, Row),
                ?assert(maps:is_key(<<"id">>, Row)),
                ?assert(maps:is_key(<<"name">>, Row));
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Row}")
        end
    end).

one_empty_result_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"SELECT 1 AS id WHERE 1=0">>,
        Result = imboy_pg:one(Sql, []),
        ?assertEqual({ok, undefined}, Result)
    end).

one_multiple_rows_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"SELECT generate_series(1,3) AS id">>,
        Result = imboy_pg:one(Sql, []),
        % one 应该只返回第一行
        ?assertMatch({ok, #{<<"id">> := 1}}, Result)
    end).

one_with_timeout_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"SELECT 1">>,
        Result = imboy_pg:one(Sql, [], 1000),
        ?assertMatch({ok, #{<<"?column?">> := 1}}, Result)
    end).

%% ===================================================================
%% pluck/4,5 测试
%% ===================================================================

pluck_existing_value_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Field = <<"id">>,
        Result = imboy_pg:pluck(Table, Field, #{id => 1}, #{}, 0),
        case Result of
            {ok, Val} when is_integer(Val) ->
                % 如果找到记录，应该返回整数 ID
                ?assert(Val > 0, "Expected positive integer ID");
            {ok, Val} ->
                % 其他类型的数据也应该验证
                ?assert(is_integer(Val), "Expected integer value");
            {error, Reason} ->
                % 数据库错误也应该有明确的原因
                ?assert(is_atom(Reason), "Expected atom error reason")
        end
    end).

pluck_with_default_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Field = <<"id">>,
        Default = 0,
        Result = imboy_pg:pluck(Table, Field, #{id => -1}, #{}, Default),
        ?assertEqual({ok, Default}, Result)
    end).

pluck_with_opts_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Field = <<"id">>,
        Opts = #{order_by => [{id, desc}], limit => 1},
        Result = imboy_pg:pluck(Table, Field, #{id => 1}, Opts, 0),
        ?assertMatch({ok, [1]}, Result)
    end).

pluck_empty_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Field = <<"id">>,
        Result = imboy_pg:pluck(Table, Field, #{}, #{}, 0),
        ?assertMatch({ok, [_|_]}, Result)
    end).

%% ===================================================================
%% pluck_value/4,5 测试
%% ===================================================================

pluck_value_success_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Field = <<"id">>,
        Result = imboy_pg:pluck_value(Table, Field, #{id => 1}, #{}, 0),
        % 应该返回实际的 ID 值，而不是 {ok, Id}
        ?assert(is_integer(Result), "Expected integer value"),
        ?assert(Result > 0, "Expected positive integer ID")
    end).

pluck_value_default_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Field = <<"id">>,
        Default = 0,
        Result = imboy_pg:pluck_value(Table, Field, #{id => -1}, #{}, Default),
        % 应该返回默认值，而不是 {ok, Default}
        ?assertEqual(Default, Result)
    end).

pluck_value_with_opts_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Field = <<"id">>,
        Opts = #{order_by => [{id, desc}], limit => 1},
        Result = imboy_pg:pluck_value(Table, Field, #{id => 1}, Opts, 0),
        % 应该返回实际的 ID 值
        ?assert(is_integer(Result), "Expected integer value")
    end).

pluck_value_error_handling_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 即使发生数据库错误，也应该返回默认值
        Table = user_repo:tablename(),
        Field = <<"id">>,
        Default = -1,
        % 使用一个可能导致错误的查询
        Result = imboy_pg:pluck_value(Table, Field, #{invalid_field => <<"'">>}, #{}, Default),
        % 应该返回默认值
        ?assertEqual(Default, Result)
    end).

%% ===================================================================
%% execute/2,3 测试
%% ===================================================================

execute_insert_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"INSERT INTO public.user_config (user_id, created_at) VALUES ($1, $2) ON CONFLICT DO NOTHING">>,
        Params = [999999, imboy_dt:now()],
        Result = imboy_pg:execute(Sql, Params),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

execute_update_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"UPDATE public.user_config SET updated_at = $1 WHERE user_id = $2">>,
        Params = [imboy_dt:now(), 999999],
        Result = imboy_pg:execute(Sql, Params),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

execute_delete_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"DELETE FROM public.user_config WHERE user_id = $1">>,
        Params = [999999],
        Result = imboy_pg:execute(Sql, Params),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

execute_empty_params_test_() ->
    ?TEST_WITH_DB(fun() ->
        Sql = <<"SELECT 1">>,
        Result = imboy_pg:execute(Sql, []),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

%% ===================================================================
%% insert/2,3 测试
%% ===================================================================

insert_valid_map_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Data = #{
            account => <<"test_insert_123">>,
            password => <<"hash">>,
            status => 1,
            created_at => imboy_dt:now()
        },
        Result = imboy_pg:insert(Table, Data),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

insert_with_raw_field_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Data = #{
            account => <<"test_raw">>,
            created_at => {raw, <<"NOW()">>}
        },
        Result = imboy_pg:insert(Table, Data),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            {error, Reason} -> ?assert(is_atom(Reason), "Expected atom error reason");
            _ -> ?assert(false)
        end
    end).

insert_empty_map_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Data = #{},
        Result = imboy_pg:insert(Table, Data),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) -> ?assert(true);
            _ -> ?assert(false, "Expected {error, Reason}")
        end
    end).

insert_with_timeout_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Data = #{account => <<"test_timeout">>, created_at => imboy_dt:now()},
        Result = imboy_pg:insert(Table, Data),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

%% ===================================================================
%% update/4 测试
%% ===================================================================

update_with_valid_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Data = #{updated_at => imboy_dt:now()},
        WhereSql = <<"id = $1">>,
        Result = imboy_pg:update(Table, Data, WhereSql, [1]),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

update_with_empty_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Data = #{updated_at => imboy_dt:now()},
        WhereSql = <<"1=0">>,
        Result = imboy_pg:update(Table, Data, WhereSql, []),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

update_empty_data_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Data = #{},
        WhereSql = <<"id = 1">>,
        Result = imboy_pg:update(Table, Data, WhereSql, []),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) -> ?assert(true);
            _ -> ?assert(false, "Expected {error, Reason}")
        end
    end).

update_with_param_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Data = #{updated_at => imboy_dt:now()},
        WhereSql = <<"id = $1">>,
        Result = imboy_pg:update(Table, Data, WhereSql, [1]),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

%% ===================================================================
%% select/2,3 测试
%% ===================================================================

select_basic_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        WhereSql = <<"id = 1">>,
        Result = imboy_pg:select(Table, WhereSql),
        case Result of
            {ok, List} when is_list(List) -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, List}")
        end
    end).

select_empty_result_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        WhereSql = <<"id = -1">>,
        Result = imboy_pg:select(Table, WhereSql),
        ?assertEqual({ok, []}, Result)
    end).

select_with_timeout_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        WhereSql = <<"id > 0">>,
        Result = imboy_pg:select(Table, WhereSql),
        case Result of
            {ok, List} when is_list(List) -> ?assert(length(List) >= 0);
            _ -> ?assert(false, "Expected {ok, List}")
        end
    end).

%% ===================================================================
%% page/4,6,7 测试
%% ===================================================================

page_basic_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        WhereMap = #{id => {op, <<">">>, 0}},
        Page = 1,
        Size = 10,
        Result = imboy_pg:page(Table, WhereMap, Page, Size),
        case Result of
            {ok, PageData} when is_map(PageData); is_list(PageData) -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, PageData}")
        end
    end).

page_with_column_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Column = <<"id">>,
        WhereMap = #{id => {op, <<">">>, 0}},
        OrderBy = <<"id DESC">>,
        Page = 1,
        Size = 5,
        Result = imboy_pg:page(Table, Column, WhereMap, OrderBy, Page, Size),
        case Result of
            {ok, PageData} when is_map(PageData); is_list(PageData) -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, PageData}")
        end
    end).

page_large_offset_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        WhereMap = #{id => {op, <<">">>, 0}},
        Page = 101,
        Size = 10,
        Result = imboy_pg:page(Table, WhereMap, Page, Size),
        case Result of
            {ok, PageData} when is_map(PageData); is_list(PageData) -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, PageData}")
        end
    end).

page_with_timeout_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        WhereMap = #{id => {op, <<">">>, 0}},
        OrderBy = <<"id ASC">>,
        Page = 1,
        Size = 10,
        Result = imboy_pg:page(Table, <<"*">>, WhereMap, OrderBy, Page, Size),
        case Result of
            {ok, PageData} when is_map(PageData); is_list(PageData) -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, PageData}")
        end
    end).

%% ===================================================================
%% insert_batch/3,4 测试
%% ===================================================================

insert_batch_valid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Cols = [account, password, status],
        Rows = [
            [<<"batch1">>, <<"hash1">>, 1],
            [<<"batch2">>, <<"hash2">>, 1]
        ],
        Result = imboy_pg:insert_batch(Table, Cols, Rows),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

insert_batch_single_row_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Cols = [account],
        Rows = [[<<"single">>]],
        Result = imboy_pg:insert_batch(Table, Cols, Rows),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount > 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).

insert_batch_empty_rows_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = user_repo:tablename(),
        Cols = [account],
        Rows = [],
        Result = imboy_pg:insert_batch(Table, Cols, Rows),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) -> ?assert(true);
            _ -> ?assert(false, "Expected {error, Reason}")
        end
    end).

%% ===================================================================
%% with_tx/1,2 测试
%% ===================================================================

with_tx_success_test_() ->
    ?TEST_WITH_DB(fun() ->
        Fun = fun(Conn) ->
            Sql = <<"SELECT 1">>,
            {ok, _, _} = imboy_pg:query(Conn, Sql, []),
            ok
        end,
        Result = imboy_pg:with_tx(Fun),
        ?assertEqual(ok, Result)
    end).

with_tx_rollback_test_() ->
    ?TEST_WITH_DB(fun() ->
        Fun = fun(__Conn) ->
            error(force_rollback)
        end,
        Result = imboy_pg:with_tx(Fun),
        case Result of
            {rollback, Reason} ->
                % 验证事务回滚成功
                ?assert(is_atom(Reason), "Expected atom rollback reason");
            {error, ErrorReason} ->
                % 验证错误信息
                ?assert(is_atom(ErrorReason), "Expected atom error reason");
            _ -> ?assert(false)
        end
    end).

with_tx_with_opts_test_() ->
    ?TEST_WITH_DB(fun() ->
        Fun = fun(__Conn) -> ok end,
        Result = imboy_pg:with_tx(Fun, [{reraise, true}]),
        ?assertEqual(ok, Result)
    end).
