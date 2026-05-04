-module(imboy_plugin_migrate_exec_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% P3-T2: 插件 DB 迁移执行层（切片 2：副作用）
%%%
%%% 覆盖 / Coverage:
%%%   1. ensure_table_sql/1 SQL 生成正确
%%%   2. record_applied_sql/3 SQL 生成正确
%%%   3. applied_seqs_sql/1 SQL 生成正确
%%%   4. migration_table_name/1 生成正确
%%%   5. run_pending diff 逻辑集成
%%% @end
%%%-------------------------------------------------------------------

-define(TEST_PLUGIN, test_plugin).

setup() ->
    application:set_env(imboy, env, test),
    ok.

cleanup(_State) ->
    ok.

%% ===================================================================
%% 1. ensure_schema_migrations_table SQL 生成正确
%% ===================================================================

ensure_table_sql_is_valid_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             SQL = imboy_plugin_migrate:ensure_table_sql(?TEST_PLUGIN),
             ?assert(is_binary(SQL)),
             ?assertMatch({_, _}, binary:match(SQL, <<"test_plugin">>))
         end)
     end}.

%% ===================================================================
%% 2. record_applied SQL 生成正确
%% ===================================================================

record_applied_sql_is_valid_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             SQL = imboy_plugin_migrate:record_applied_sql(?TEST_PLUGIN, 1, <<"init">>),
             ?assert(is_binary(SQL)),
             ?assertMatch({_, _}, binary:match(SQL, <<"test_plugin">>)),
             ?assertMatch({_, _}, binary:match(SQL, <<"1">>))
         end)
     end}.

%% ===================================================================
%% 3. applied_seqs_query SQL 生成正确
%% ===================================================================

applied_seqs_sql_is_valid_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             SQL = imboy_plugin_migrate:applied_seqs_sql(?TEST_PLUGIN),
             ?assert(is_binary(SQL)),
             ?assertMatch({_, _}, binary:match(SQL, <<"test_plugin">>))
         end)
     end}.

%% ===================================================================
%% 4. migration_table_name 生成正确
%% ===================================================================

migration_table_name_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         [?_assertEqual(<<"schema_migrations_channel">>,
                        imboy_plugin_migrate:migration_table_name(channel)),
          ?_assertEqual(<<"schema_migrations_test_plugin">>,
                        imboy_plugin_migrate:migration_table_name(?TEST_PLUGIN))]
     end}.

%% ===================================================================
%% 5. run_pending diff 逻辑集成
%% ===================================================================

run_pending_diff_logic_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             DiskFiles = ["test_plugin_0001_init.sql", "test_plugin_0002_add_col.sql"],
             Applied = [1],
             Pending = imboy_plugin_migrate:diff_pending(DiskFiles, Applied),
             ?assertEqual(["test_plugin_0002_add_col.sql"], Pending)
         end)
     end}.
