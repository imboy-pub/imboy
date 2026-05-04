-module(imboy_plugin_migrate_rollback_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% P3-V2: 迁移失败自动回滚验证
%%%
%%% 验证 diff_pending 排序 + 表名一致性 + 错误输入鲁棒性。
%%% 实际 elib_pg:with_tx 事务回滚需要 DB 集成测试。
%%% @end
%%%-------------------------------------------------------------------

setup() ->
    application:set_env(imboy, env, test),
    ok.

cleanup(_) -> ok.

%% ===================================================================
%% 1. 空列表 → diff 无执行
%% ===================================================================

empty_pending_means_no_execution_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             ?assertEqual([], imboy_plugin_migrate:diff_pending([], []))
         end)
     end}.

%% ===================================================================
%% 2. 单个 pending → 正确识别
%% ===================================================================

diff_single_pending_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             DiskFiles = ["channel_0001_init.sql", "channel_0002_add_col.sql"],
             Applied = [1],
             Pending = imboy_plugin_migrate:diff_pending(DiskFiles, Applied),
             ?assertEqual(1, length(Pending)),
             ?assertEqual("channel_0002_add_col.sql", hd(Pending))
         end)
     end}.

%% ===================================================================
%% 3. 乱序 disk 文件 → diff_pending 按 seq 排序
%% ===================================================================

diff_sorts_by_seq_ascending_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             DiskFiles = [
                 "channel_0003_idx.sql",
                 "channel_0001_init.sql",
                 "channel_0002_add_col.sql"
             ],
             Pending = imboy_plugin_migrate:diff_pending(DiskFiles, []),
             Seqs = [begin
                 {ok, _, S, _} = imboy_plugin_migrate:parse_migration_filename(F),
                 S
             end || F <- Pending],
             ?assertEqual([1, 2, 3], Seqs)
         end)
     end}.

%% ===================================================================
%% 4. 表名一致性：ensure + record + applied + drop 引用同一表
%% ===================================================================

table_name_consistency_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         ?_test(begin
             Tab = imboy_plugin_migrate:migration_table_name(channel),
             EnsureSQL = imboy_plugin_migrate:ensure_table_sql(channel),
             RecordSQL = imboy_plugin_migrate:record_applied_sql(channel, 1, <<"init">>),
             AppliedSQL = imboy_plugin_migrate:applied_seqs_sql(channel),
             DropSQL = imboy_plugin_migrate:drop_schema_migrations_table_sql(channel),

             ?assertMatch({_, _}, binary:match(EnsureSQL, Tab)),
             ?assertMatch({_, _}, binary:match(RecordSQL, Tab)),
             ?assertMatch({_, _}, binary:match(AppliedSQL, Tab)),
             ?assertMatch({_, _}, binary:match(DropSQL, Tab))
         end)
     end}.

%% ===================================================================
%% 5. parse_migration_filename 错误输入不崩溃
%% ===================================================================

parse_bad_filenames_no_crash_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         BadNames = ["", "no_sql", "channel.sql", "Channel_0001_init.sql",
                     <<>>, 123, "channel_0001_init"],
         [?_assertEqual({error, invalid_filename},
                        imboy_plugin_migrate:parse_migration_filename(N))
          || N <- BadNames]
     end}.
