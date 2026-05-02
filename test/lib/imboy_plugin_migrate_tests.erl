-module(imboy_plugin_migrate_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_migrate 的 EUnit 测试（P3 切片 1：纯函数）
%%%
%%% 覆盖 / Coverage:
%%%   - parse_migration_filename/1: 9 项（合法/非法格式 + 边界）
%%%   - diff_pending/2: 5 项（全 pending / 部分 / 全 applied / 非法跳过 / 排序）
%%%   - list_migration_files/1: 3 项（fixture 目录扫描 / 空 / 缺失）
%%%   - plugin_dir/1: 1 项（路径计算）
%%% @end
%%%-------------------------------------------------------------------

%% ===================================================================
%% Helpers
%% ===================================================================

unique_dir() ->
    filename:join(
        "/tmp",
        "imboy_plugin_migrate_test_" ++
            integer_to_list(erlang:unique_integer([positive]))
    ).

mkdir_p(Path) ->
    %% OTP 25+ ensure_path 递归创建包含末段的全部目录，比 ensure_dir 更可靠
    ok = filelib:ensure_path(Path).

rm_rf(Path) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, Entries} = file:list_dir(Path),
            lists:foreach(fun(E) -> rm_rf(filename:join(Path, E)) end, Entries),
            file:del_dir(Path);
        false ->
            file:delete(Path)
    end.

touch(Path) ->
    ok = file:write_file(Path, <<"-- placeholder">>).

%% ===================================================================
%% parse_migration_filename/1
%% ===================================================================

parse_valid_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {ok, "channel", 1, "init"},
            imboy_plugin_migrate:parse_migration_filename("channel_0001_init.sql")
        )
    end).

parse_valid_long_descr_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {ok, "moment", 42, "add_index_on_uid"},
            imboy_plugin_migrate:parse_migration_filename("moment_0042_add_index_on_uid.sql")
        )
    end).

parse_valid_compound_plugin_test_() ->
    %% group_collab 是合法 plugin 名（含下划线）
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {ok, "group_collab", 7, "init"},
            imboy_plugin_migrate:parse_migration_filename("group_collab_0007_init.sql")
        )
    end).

parse_no_seq_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_filename},
            imboy_plugin_migrate:parse_migration_filename("channel_init.sql")
        )
    end).

parse_no_descr_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_filename},
            imboy_plugin_migrate:parse_migration_filename("channel_0001.sql")
        )
    end).

parse_no_extension_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_filename},
            imboy_plugin_migrate:parse_migration_filename("channel_0001_init")
        )
    end).

parse_uppercase_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_filename},
            imboy_plugin_migrate:parse_migration_filename("Channel_0001_init.sql")
        )
    end).

parse_non_list_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_filename},
            imboy_plugin_migrate:parse_migration_filename(<<"channel_0001_init.sql">>)
        )
    end).

parse_empty_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_filename},
            imboy_plugin_migrate:parse_migration_filename("")
        )
    end).

%% ===================================================================
%% diff_pending/2
%% ===================================================================

diff_all_pending_test_() ->
    ?TEST_SIMPLE(fun() ->
        Disk = ["channel_0001_a.sql", "channel_0002_b.sql", "channel_0003_c.sql"],
        Applied = [],
        ?assertEqual(Disk, imboy_plugin_migrate:diff_pending(Disk, Applied))
    end).

diff_partial_applied_test_() ->
    ?TEST_SIMPLE(fun() ->
        Disk = ["channel_0001_a.sql", "channel_0002_b.sql", "channel_0003_c.sql"],
        Applied = [1, 2],
        ?assertEqual(["channel_0003_c.sql"],
                     imboy_plugin_migrate:diff_pending(Disk, Applied))
    end).

diff_all_applied_returns_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Disk = ["channel_0001_a.sql", "channel_0002_b.sql"],
        Applied = [1, 2],
        ?assertEqual([], imboy_plugin_migrate:diff_pending(Disk, Applied))
    end).

diff_skips_invalid_filenames_test_() ->
    ?TEST_SIMPLE(fun() ->
        Disk = ["channel_0001_a.sql", "README.md", "ad-hoc-script.sql"],
        Applied = [],
        ?assertEqual(["channel_0001_a.sql"],
                     imboy_plugin_migrate:diff_pending(Disk, Applied))
    end).

diff_sorts_by_seq_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 输入乱序
        Disk = ["channel_0010_z.sql", "channel_0001_a.sql", "channel_0005_m.sql"],
        Applied = [],
        ?assertEqual(
            ["channel_0001_a.sql", "channel_0005_m.sql", "channel_0010_z.sql"],
            imboy_plugin_migrate:diff_pending(Disk, Applied)
        )
    end).

%% ===================================================================
%% plugin_dir/1
%% ===================================================================

plugin_dir_path_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifest = #{
            name => channel,
            migrations => #{dir => <<"priv/migrations">>}
        },
        ?assertEqual(
            "priv/plugins/channel/priv/migrations",
            imboy_plugin_migrate:plugin_dir(Manifest)
        )
    end).

%% ===================================================================
%% list_migration_files/1
%% ===================================================================

list_migration_files_filters_and_sorts_test_() ->
    {setup,
     fun() ->
         Base = unique_dir(),
         %% 模拟 priv/plugins/channel/priv/migrations
         PluginDir = filename:join([Base, "priv", "plugins", "channel"]),
         MigDir = filename:join(PluginDir, "priv/migrations"),
         mkdir_p(MigDir),
         touch(filename:join(MigDir, "channel_0003_c.sql")),
         touch(filename:join(MigDir, "channel_0001_a.sql")),
         touch(filename:join(MigDir, "channel_0002_b.sql")),
         touch(filename:join(MigDir, "README.md")),       %% 应过滤
         touch(filename:join(MigDir, "ad_hoc.sql")),       %% 应过滤
         {Base, MigDir}
     end,
     fun({Base, _}) -> rm_rf(Base) end,
     fun({Base, _MigDir}) ->
         %% 切到临时根目录跑 list_migration_files
         {ok, OldCwd} = file:get_cwd(),
         try
             ok = file:set_cwd(Base),
             Manifest = #{
                 name => channel,
                 migrations => #{dir => <<"priv/migrations">>}
             },
             {ok, Files} = imboy_plugin_migrate:list_migration_files(Manifest),
             [
                 ?_assertEqual(
                     ["channel_0001_a.sql",
                      "channel_0002_b.sql",
                      "channel_0003_c.sql"],
                     Files
                 )
             ]
         after
             file:set_cwd(OldCwd)
         end
     end}.

list_migration_files_empty_dir_test_() ->
    {setup,
     fun() ->
         Base = unique_dir(),
         MigDir = filename:join([Base, "priv", "plugins", "moment", "priv/migrations"]),
         mkdir_p(MigDir),
         Base
     end,
     fun(Base) -> rm_rf(Base) end,
     fun(Base) ->
         {ok, OldCwd} = file:get_cwd(),
         try
             ok = file:set_cwd(Base),
             Manifest = #{
                 name => moment,
                 migrations => #{dir => <<"priv/migrations">>}
             },
             %% 先同步调用拿值（cwd 切换有效期内），再用 lazy assertion 比较
             Result = imboy_plugin_migrate:list_migration_files(Manifest),
             ?_assertEqual({ok, []}, Result)
         after
             file:set_cwd(OldCwd)
         end
     end}.

list_migration_files_missing_dir_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifest = #{
            name => nonexistent_plugin,
            migrations => #{dir => <<"priv/migrations">>}
        },
        ?assertMatch({error, enoent},
                     imboy_plugin_migrate:list_migration_files(Manifest))
    end).
