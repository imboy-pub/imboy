-module(elib_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_pg 模块的 EUnit 测试
%%%
%%% 目标：验证核心 PostgreSQL 访问层的正确性
%%% 覆盖：execute, query, one, pluck, insert, update, select, page, with_tx
%%%
%%% 策略：使用 meck mock pooler / epgsql / config_ds，
%%%        不 mock elib_pg_sql（纯 SQL 构建保持真实）。
%%%===================================================================

%% Spawn a simple process that acts as a fake connection.
%% Returns its pid so that is_pid/1 check in with_conn passes.
spawn_fake_conn() ->
    spawn(fun() ->
        receive
            stop -> ok
        after 300000 -> ok
        end
    end).

%% ===================================================================
%% Common mock setup / teardown helpers
%% ===================================================================

setup_pooler_mock() ->
    FakeConn = spawn_fake_conn(),
    meck:new(pooler, [no_link, passthrough]),
    meck:expect(pooler, take_member, 1, fun(pgsql) -> FakeConn; (_) -> error_no_members end),
    meck:expect(pooler, return_member, 2, fun(_Driver, _Conn) -> ok end),
    FakeConn.

setup_pooler_mock_with_error() ->
    meck:new(pooler, [no_link, passthrough]),
    meck:expect(pooler, take_member, 1, fun(_Driver) -> error_no_members end),
    meck:expect(pooler, return_member, 2, fun(_Driver, _Conn) -> ok end),
    ok.

teardown_pooler_mock(_FakeConn) ->
    catch meck:unload(pooler).

stop_fake_conn(FakeConn) when is_pid(FakeConn) ->
    FakeConn ! stop;
stop_fake_conn(_) ->
    ok.

%% ===================================================================
%% query/2 测试
%% ===================================================================

query_select_all_test_() ->
    Col = #column{name = <<"col">>, type = int4, oid = 23, size = 4,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{1}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Result = elib_pg:query(<<"SELECT 1 AS col">>, []),
             ?assertMatch({ok, [_|_]}, Result),
             {ok, [Row]} = Result,
             ?assertEqual(1, maps:get(<<"col">>, Row))
         end)
     end}.

query_select_empty_test_() ->
    Col = #column{name = <<"col">>, type = int4, oid = 23, size = 4,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], []}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Result = elib_pg:query(<<"SELECT 1 WHERE 1=0">>, []),
             ?assertEqual({ok, []}, Result)
         end)
     end}.

query_with_params_test_() ->
    Col = #column{name = <<"col">>, type = int4, oid = 23, size = 4,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, [42]) ->
                 {ok, [Col], [{42}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"SELECT $1 AS col">>,
             Result = elib_pg:query(Sql, [42]),
             ?assertMatch({ok, [_|_]}, Result)
         end)
     end}.

query_with_binary_param_test_() ->
    Col = #column{name = <<"col">>, type = text, oid = 25, size = -1,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, [<<"test">>]) ->
                 {ok, [Col], [{<<"test">>}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"SELECT $1 AS col">>,
             Result = elib_pg:query(Sql, [<<"test">>]),
             ?assertMatch({ok, [_|_]}, Result)
         end)
     end}.

query_with_null_param_test_() ->
    Col = #column{name = <<"col">>, type = int4, oid = 23, size = 4,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, [undefined]) ->
                 {ok, [Col], [{null}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"SELECT $1 AS col">>,
             Result = elib_pg:query(Sql, [undefined]),
             ?assertMatch({ok, [_|_]}, Result)
         end)
     end}.

query_error_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {error, #{message => <<"test error">>}}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Result = elib_pg:query(<<"SELECT bad">>, []),
             ?assertMatch({error, _}, Result)
         end)
     end}.

%% ===================================================================
%% one/2,3 测试
%% ===================================================================

one_single_row_test_() ->
    Col1 = #column{name = <<"id">>, type = int4, oid = 23, size = 4,
                   modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    Col2 = #column{name = <<"name">>, type = text, oid = 25, size = -1,
                   modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col1, Col2], [{1, <<"test">>}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"SELECT 1 AS id, 'test' AS name">>,
             Result = elib_pg:one(Sql, []),
             ?assertMatch({ok, #{<<"id">> := 1, <<"name">> := <<"test">>}}, Result)
         end)
     end}.

one_empty_result_test_() ->
    Col = #column{name = <<"id">>, type = int4, oid = 23, size = 4,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], []}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"SELECT 1 AS id WHERE 1=0">>,
             Result = elib_pg:one(Sql, [], undefined),
             ?assertEqual({ok, undefined}, Result)
         end)
     end}.

one_multiple_rows_test_() ->
    Col = #column{name = <<"id">>, type = int4, oid = 23, size = 4,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{1}, {2}, {3}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"SELECT generate_series(1,3) AS id">>,
             Result = elib_pg:one(Sql, []),
             ?assertMatch({ok, #{<<"id">> := 1}}, Result)
         end)
     end}.

one_with_custom_default_test_() ->
    Col = #column{name = <<"id">>, type = int4, oid = 23, size = 4,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], []}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"SELECT 1 AS id WHERE 1=0">>,
             Result = elib_pg:one(Sql, [], #{error => not_found}),
             ?assertEqual({ok, #{error => not_found}}, Result)
         end)
     end}.

%% ===================================================================
%% pluck/4,5 测试
%% ===================================================================

pluck_existing_value_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{42}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Field = <<"id">>,
             Result = elib_pg:pluck(Table, Field, #{id => 42}, #{}, 0),
             ?assertEqual({ok, 42}, Result)
         end)
     end}.

pluck_with_default_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], []}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Field = <<"id">>,
             Default = 0,
             Result = elib_pg:pluck(Table, Field, #{id => -1}, #{}, Default),
             ?assertEqual({ok, Default}, Result)
         end)
     end}.

pluck_with_opts_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{100}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Field = <<"id">>,
             Opts = #{order_by => [{id, desc}], limit => 1},
             Result = elib_pg:pluck(Table, Field, #{id => 100}, Opts, 0),
             ?assertEqual({ok, 100}, Result)
         end)
     end}.

pluck_empty_where_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{7}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Field = <<"id">>,
             Result = elib_pg:pluck(Table, Field, #{}, #{}, 0),
             %% pluck returns {ok, Value} where Value is the single field value
             ?assertMatch({ok, Val} when is_integer(Val), Result)
         end)
     end}.

%% ===================================================================
%% pluck_value/4,5 测试
%% ===================================================================

pluck_value_success_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{42}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Field = <<"id">>,
             Result = elib_pg:pluck_value(Table, Field, #{id => 42}, #{}, 0),
             %% pluck_value returns the bare value (not wrapped in {ok, _})
             ?assert(is_integer(Result), "Expected integer value"),
             ?assert(Result > 0, "Expected positive integer ID")
         end)
     end}.

pluck_value_default_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], []}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Field = <<"id">>,
             Default = 0,
             Result = elib_pg:pluck_value(Table, Field, #{id => -1}, #{}, Default),
             ?assertEqual(Default, Result)
         end)
     end}.

pluck_value_with_opts_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{55}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Field = <<"id">>,
             Opts = #{order_by => [{id, desc}], limit => 1},
             Result = elib_pg:pluck_value(Table, Field, #{id => 55}, Opts, 0),
             ?assert(is_integer(Result), "Expected integer value"),
             ?assert(Result > 0, "Expected positive integer ID")
         end)
     end}.

pluck_value_error_handling_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {error, #{message => <<"column does not exist">>}}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Field = <<"id">>,
             Default = -1,
             Result = elib_pg:pluck_value(Table, Field, #{invalid_field => <<"'">>}, #{}, Default),
             %% pluck_value returns Default on error
             ?assertEqual(Default, Result)
         end)
     end}.

%% ===================================================================
%% execute/2,3 测试
%% ===================================================================

execute_insert_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 1}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"INSERT INTO user_config (user_id, created_at) VALUES ($1, $2)">>,
             Params = [999999, 1000],
             Result = elib_pg:execute(Sql, Params),
             ?assertEqual({ok, 1}, Result)
         end)
     end}.

execute_update_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 0}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"UPDATE user_config SET updated_at = $1 WHERE user_id = $2">>,
             Params = [1000, 999999],
             Result = elib_pg:execute(Sql, Params),
             ?assertMatch({ok, N} when is_integer(N), Result)
         end)
     end}.

execute_delete_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 2}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"DELETE FROM user_config WHERE user_id = $1">>,
             Params = [999999],
             Result = elib_pg:execute(Sql, Params),
             ?assertEqual({ok, 2}, Result)
         end)
     end}.

execute_empty_params_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, []}]) -> [{ok, 1}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"SELECT 1">>,
             Result = elib_pg:execute(Sql, []),
             ?assertMatch({ok, N} when is_integer(N), Result)
         end)
     end}.

execute_with_returning_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 1, [{99}]}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"INSERT INTO foo (x) VALUES ($1) RETURNING id">>,
             Result = elib_pg:execute(Sql, [42]),
             %% execute returns {ok, Count, Returning} when RETURNING is present
             ?assertEqual({ok, 1, [{99}]}, Result)
         end)
     end}.

execute_parse_error_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {error, #{message => <<"syntax error">>}} end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Sql = <<"INVALID SQL">>,
             Result = elib_pg:execute(Sql, []),
             ?assertMatch({error, _}, Result)
         end)
     end}.

%% ===================================================================
%% insert/2,3 测试
%% ===================================================================

insert_valid_map_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 1}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Data = #{
                 account => <<"test_insert_123">>,
                 password => <<"hash">>,
                 status => 1,
                 created_at => 1000
             },
             Result = elib_pg:insert(Table, Data),
             ?assertMatch({ok, N} when is_integer(N), Result)
         end)
     end}.

insert_with_returning_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 1, [{42}]}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Data = #{
                 account => <<"test_returning">>,
                 created_at => 1000
             },
             Result = elib_pg:insert(Table, Data, <<" RETURNING id">>),
             ?assertEqual({ok, 1, [{42}]}, Result)
         end)
     end}.

insert_with_raw_field_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 1}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Data = #{
                 account => <<"test_raw">>,
                 created_at => {raw, <<"NOW()">>}
             },
             Result = elib_pg:insert(Table, Data),
             ?assertMatch({ok, N} when is_integer(N), Result)
         end)
     end}.

insert_error_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {error, #{message => <<"violates not-null">>}} end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Data = #{account => <<"test_err">>},
             Result = elib_pg:insert(Table, Data),
             ?assertMatch({error, _}, Result)
         end)
     end}.

%% ===================================================================
%% update/4 测试
%% ===================================================================

update_with_valid_where_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 1}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Data = #{updated_at => 1000},
             WhereSql = <<"id = $1">>,
             Result = elib_pg:update(Table, Data, WhereSql, [1]),
             ?assertEqual({ok, 1}, Result)
         end)
     end}.

update_with_empty_where_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 0}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Data = #{updated_at => 1000},
             WhereSql = <<"1=0">>,
             Result = elib_pg:update(Table, Data, WhereSql, []),
             ?assertEqual({ok, 0}, Result)
         end)
     end}.

update_with_param_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 1}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Data = #{updated_at => 1000},
             WhereSql = <<"id = $1">>,
             Result = elib_pg:update(Table, Data, WhereSql, [1]),
             ?assertMatch({ok, N} when is_integer(N), Result)
         end)
     end}.

%% ===================================================================
%% select/2,3 测试
%% ===================================================================

select_basic_test_() ->
    Col1 = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                   modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    Col2 = #column{name = <<"account">>, type = text, oid = 25, size = -1,
                   modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col1, Col2], [{1, <<"alice">>}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             WhereSql = <<"id = 1">>,
             Result = elib_pg:select(Table, WhereSql),
             ?assertMatch({ok, [_|_]}, Result)
         end)
     end}.

select_empty_result_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], []}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             WhereSql = <<"id = -1">>,
             Result = elib_pg:select(Table, WhereSql),
             ?assertEqual({ok, []}, Result)
         end)
     end}.

select_with_params_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{10}, {20}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             WhereSql = <<"id > $1">>,
             Result = elib_pg:select(Table, WhereSql, [0]),
             ?assertMatch({ok, [_, _]}, Result)
         end)
     end}.

%% ===================================================================
%% page/4,6 测试
%% ===================================================================

page_basic_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{1}, {2}, {3}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             WhereMap = #{id => {op, <<">">>, 0}},
             Result = elib_pg:page(Table, WhereMap, 1, 10),
             ?assertMatch({ok, [_|_]}, Result)
         end)
     end}.

page_with_column_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{10}, {9}, {8}, {7}, {6}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Column = <<"id">>,
             WhereMap = #{id => {op, <<">">>, 0}},
             OrderBy = <<"id DESC">>,
             Result = elib_pg:page(Table, Column, WhereMap, OrderBy, 1, 5),
             ?assertMatch({ok, [_|_]}, Result)
         end)
     end}.

page_empty_result_test_() ->
    Col = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], []}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             WhereMap = #{id => {op, <<">">>, 0}},
             Result = elib_pg:page(Table, WhereMap, 101, 10),
             ?assertEqual({ok, []}, Result)
         end)
     end}.

%% ===================================================================
%% page_with_total/4,6 测试
%% ===================================================================

page_with_total_basic_test_() ->
    Col1 = #column{name = <<"count">>, type = int8, oid = 20, size = 8,
                   modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    Col2 = #column{name = <<"id">>, type = int8, oid = 20, size = 8,
                   modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         %% First call: count(*) -> returns 3
         %% Second call: page query -> returns rows
         meck:expect(epgsql, equery, 3, fun
             (_Conn, Sql, _Params) ->
                 SqlBin = iolist_to_binary(Sql),
                 case binary:match(SqlBin, <<"count(">>) of
                     nomatch ->
                         {ok, [Col2], [{1}, {2}, {3}]};
                     _ ->
                         {ok, [Col1], [{3}]}
                 end
         end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             WhereMap = #{id => {op, <<">">>, 0}},
             Result = elib_pg:page_with_total(Table, WhereMap, 1, 10),
             ?assertMatch({ok, #{total := 3, page := 1, size := 10, list := [_, _, _]}}, Result)
         end)
     end}.

page_with_total_zero_test_() ->
    Col = #column{name = <<"count">>, type = int8, oid = 20, size = 8,
                  modifier = -1, format = 1, table_oid = 0, table_attr_number = 0},
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, equery, 3,
             fun(_Conn, _Sql, _Params) ->
                 {ok, [Col], [{0}]}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             WhereMap = #{id => {op, <<">">>, 0}},
             Result = elib_pg:page_with_total(Table, WhereMap, 1, 10),
             ?assertEqual({ok, #{total => 0, page => 1, size => 10, list => []}}, Result)
         end)
     end}.

%% ===================================================================
%% insert_batch/3 测试
%% ===================================================================

insert_batch_valid_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 2}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Cols = [account, password, status],
             Rows = [
                 [<<"batch1">>, <<"hash1">>, 1],
                 [<<"batch2">>, <<"hash2">>, 1]
             ],
             Result = elib_pg:insert_batch(Table, Cols, Rows),
             ?assertEqual({ok, 2}, Result)
         end)
     end}.

insert_batch_single_row_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, parse, 2,
             fun(_Conn, _Sql) -> {ok, stmt} end),
         meck:expect(epgsql, execute_batch, 2,
             fun(_Conn, [{stmt, _Params}]) -> [{ok, 1}] end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Table = <<"public.user">>,
             Cols = [account],
             Rows = [[<<"single">>]],
             Result = elib_pg:insert_batch(Table, Cols, Rows),
             ?assertEqual({ok, 1}, Result)
         end)
     end}.

%% ===================================================================
%% with_conn / pooler 测试
%% ===================================================================

with_conn_no_members_test_() ->
    {setup,
     fun() ->
         setup_pooler_mock_with_error(),
         code:unstick_mod(timer),
         meck:new(timer, [no_link, passthrough]),
         meck:expect(timer, sleep, 1, fun(_Ms) -> ok end)
     end,
     fun(_) ->
         catch meck:unload(timer),
         code:stick_mod(timer),
         catch meck:unload(pooler)
     end,
     fun(_) ->
         ?_test(begin
             Result = elib_pg:with_conn(fun(_Conn) -> ok end),
             ?assertEqual({error, no_connection}, Result)
         end)
     end}.

%% ===================================================================
%% with_tx/1,2 测试
%% ===================================================================

with_tx_success_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, with_transaction, 3,
             fun(_Conn, Fun, _Opts) -> Fun(FakeConn) end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Fun = fun(_Conn) -> ok end,
             Result = elib_pg:with_tx(Fun),
             ?assertEqual(ok, Result)
         end)
     end}.

with_tx_rollback_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, with_transaction, 3,
             fun(_Conn, _Fun, _Opts) ->
                 {rollback, force_rollback}
             end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Fun = fun(_Conn) -> error(force_rollback) end,
             Result = elib_pg:with_tx(Fun),
             ?assertMatch({rollback, force_rollback}, Result)
         end)
     end}.

with_tx_with_opts_test_() ->
    {setup,
     fun() ->
         FakeConn = setup_pooler_mock(),
         meck:new(config_ds, [no_link, passthrough]),
         meck:expect(config_ds, env, 1, fun(sql_driver) -> pgsql; (_) -> undefined end),
         meck:new(epgsql, [no_link, passthrough]),
         meck:expect(epgsql, with_transaction, 3,
             fun(_Conn, Fun, _Opts) -> Fun(FakeConn) end),
         FakeConn
     end,
     fun(FakeConn) ->
         catch meck:unload(epgsql),
         catch meck:unload(config_ds),
         teardown_pooler_mock(FakeConn),
         stop_fake_conn(FakeConn)
     end,
     fun(_FakeConn) ->
         ?_test(begin
             Fun = fun(_Conn) -> ok end,
             Result = elib_pg:with_tx(Fun, [{reraise, true}]),
             ?assertEqual(ok, Result)
         end)
     end}.

%% ===================================================================
%% escape_like/1 测试
%% ===================================================================

escape_like_percent_test_() ->
    ?_test(begin
        ?assertEqual(<<"\\%">>, elib_pg:escape_like(<<"%">>)),
        ?assertEqual(<<"a\\%b">>, elib_pg:escape_like(<<"a%b">>))
    end).

escape_like_underscore_test_() ->
    ?_test(begin
        ?assertEqual(<<"\\_">>, elib_pg:escape_like(<<"_">>)),
        ?assertEqual(<<"a\\_b">>, elib_pg:escape_like(<<"a_b">>))
    end).

escape_like_backslash_test_() ->
    ?_test(begin
        ?assertEqual(<<"\\\\">>, elib_pg:escape_like(<<"\\">>)),
        ?assertEqual(<<"a\\\\b">>, elib_pg:escape_like(<<"a\\b">>))
    end).

escape_like_no_special_chars_test_() ->
    ?_test(begin
        ?assertEqual(<<"hello">>, elib_pg:escape_like(<<"hello">>))
    end).

escape_like_combined_test_() ->
    ?_test(begin
        ?assertEqual(<<"a\\%b\\_c\\\\d">>, elib_pg:escape_like(<<"a%b_c\\d">>))
    end).

escape_like_empty_test_() ->
    ?_test(begin
        ?assertEqual(<<"">>, elib_pg:escape_like(<<"">>))
    end).
