-module(imboy_plugin_migrate_uninstall_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% P3-T3: 插件 uninstall.sql 可选支持
%%%
%%% 覆盖 / Coverage:
%%%   1. drop_schema_migrations_table_sql/1 SQL 生成正确
%%%   2. list_uninstall_file/1 文件不存在返回 not_found
%%%   3. uninstall_sql_file_path/1 路径生成正确
%%% @end
%%%-------------------------------------------------------------------

-define(TEST_PLUGIN, test_plugin).

setup() ->
    application:set_env(imboy, env, test),
    ok.

cleanup(_) -> ok.

%% ===================================================================
%% 1. drop_schema_migrations_table_sql SQL 生成正确
%% ===================================================================

drop_table_sql_is_valid_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             SQL = imboy_plugin_migrate:drop_schema_migrations_table_sql(channel),
             ?assert(is_binary(SQL)),
             ?assertMatch({_, _}, binary:match(SQL, <<"schema_migrations_channel">>)),
             ?assertMatch({_, _}, binary:match(SQL, <<"DROP TABLE">>))
         end)
     end}.

%% ===================================================================
%% 2. list_uninstall_file 返回 not_found（目录不存在）
%% ===================================================================

list_uninstall_file_missing_dir_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             Manifest = #{name => ?TEST_PLUGIN, migrations => #{dir => <<"migrations">>}},
             ?assertEqual({error, not_found},
                          imboy_plugin_migrate:list_uninstall_file(Manifest))
         end)
     end}.

%% ===================================================================
%% 3. uninstall_sql_file_path 路径生成正确
%% ===================================================================

uninstall_sql_file_path_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             Manifest = #{name => channel, migrations => #{dir => <<"migrations">>}},
             Path = imboy_plugin_migrate:uninstall_sql_file_path(Manifest),
             ?assertEqual("priv/plugins/channel/migrations/uninstall.sql", Path)
         end)
     end}.
