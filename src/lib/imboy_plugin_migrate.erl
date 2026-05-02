-module(imboy_plugin_migrate).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_migrate - 插件 DB 迁移（纯函数层，切片 1）
%%% Plugin DB migration (pure-function layer, slice 1)
%%%
%%% 切片 1（本模块）：纯函数 / Pure functions:
%%%   - list_migration_files/1: 扫描插件 migrations 目录，返回排序文件名列表
%%%   - parse_migration_filename/1: 解析 <plugin>_NNNN_<descr>.sql
%%%   - diff_pending/2: disk vs applied，计算 pending list
%%%   - plugin_dir/1: 计算插件 migrations 目录路径
%%%
%%% 切片 2（待实施）：副作用层 / Side-effects layer:
%%%   - run/2: 执行 SQL + 更新 schema_migrations_<plugin> 表
%%%   - 事务 + savepoint 自动回滚（依赖 elib_pg）
%%%
%%% 命名约定 / Naming convention (contract.md §3.3):
%%%   - 文件名：<plugin>_NNNN_<descr>.sql（如 channel_0001_init.sql）
%%%   - 表名前缀：<plugin>_<entity>（强约束，loader 启动期校验）
%%%   - 追踪表：schema_migrations_<plugin>（每插件独立）
%%%
%%% Source of truth: doc/plugin/contract.md §3 / roadmap P3
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-export([
    list_migration_files/1,
    parse_migration_filename/1,
    diff_pending/2,
    plugin_dir/1
]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc 计算插件 migrations 目录的相对路径（基于 priv/plugins/<name>/<dir>）。
%% Compute migrations dir path under priv/plugins/<name>/<dir>.
-spec plugin_dir(Manifest :: map()) -> file:filename().
plugin_dir(#{name := Name, migrations := #{dir := DirBin}})
        when is_atom(Name), is_binary(DirBin) ->
    filename:join([
        "priv", "plugins",
        atom_to_list(Name),
        binary_to_list(DirBin)
    ]).

%% @doc 列出插件 migrations 目录下所有合法 .sql 文件名（已按字典序排序）。
%% List all valid .sql migration filenames under plugin's migrations dir (sorted).
%% 仅返回符合命名约定的文件：<plugin>_NNNN_<descr>.sql
-spec list_migration_files(Manifest :: map()) ->
    {ok, [string()]} | {error, term()}.
list_migration_files(Manifest) when is_map(Manifest) ->
    Name = maps:get(name, Manifest),
    Dir = plugin_dir(Manifest),
    case file:list_dir(Dir) of
        {ok, Entries} ->
            PluginPrefix = atom_to_list(Name),
            Pattern = "^" ++ PluginPrefix ++ "_[0-9]+_.+\\.sql$",
            SqlFiles = [F || F <- Entries,
                        re:run(F, Pattern) =/= nomatch],
            {ok, lists:sort(SqlFiles)};
        {error, _} = E ->
            E
    end.

%% @doc 解析迁移文件名 <plugin>_NNNN_<descr>.sql。
%% Parse migration filename <plugin>_NNNN_<descr>.sql.
%% 返回 {ok, PluginStr, Seq, Descr} 或 {error, invalid_filename}。
%% Returns {ok, PluginStr, Seq, Descr} or {error, invalid_filename}.
%% 注意：plugin 字段返回 string 而非 atom，避免 atom 表无界增长（V2 评审 HIGH-1）。
%% Note: plugin field returned as string, not atom (V2 review HIGH-1: atom table protection).
-spec parse_migration_filename(string()) ->
    {ok, string(), pos_integer(), string()} | {error, invalid_filename}.
parse_migration_filename(Filename) when is_list(Filename) ->
    %% 非贪婪匹配 plugin 部分，确保 NNNN 是第一段数字
    case re:run(
        Filename,
        "^([a-z][a-z0-9_]*?)_([0-9]+)_(.+)\\.sql$",
        [{capture, all_but_first, list}]
    ) of
        {match, [PluginStr, SeqStr, Descr]} ->
            try
                Seq = list_to_integer(SeqStr),
                {ok, PluginStr, Seq, Descr}
            catch
                error:badarg -> {error, invalid_filename}
            end;
        nomatch ->
            {error, invalid_filename}
    end;
parse_migration_filename(_) ->
    {error, invalid_filename}.

%% @doc 对比 disk 文件 vs 已应用 seq 列表，返回待应用的文件列表（按 seq 升序）。
%% Diff disk files vs applied seq list, return pending files (sorted by seq).
%% 跳过非法文件名（即不会因 disk 中混入非约定文件而失败）。
%% Skips invalid filenames (so disk pollution doesn't fail the diff).
-spec diff_pending([string()], [pos_integer()]) -> [string()].
diff_pending(DiskFiles, AppliedSeqs)
        when is_list(DiskFiles), is_list(AppliedSeqs) ->
    AppliedSet = sets:from_list(AppliedSeqs),
    Pending = lists:filter(
        fun(F) ->
            case parse_migration_filename(F) of
                {ok, _, Seq, _} -> not sets:is_element(Seq, AppliedSet);
                {error, _} -> false
            end
        end,
        DiskFiles
    ),
    lists:sort(
        fun(A, B) ->
            {ok, _, SA, _} = parse_migration_filename(A),
            {ok, _, SB, _} = parse_migration_filename(B),
            SA =< SB
        end,
        Pending
    ).
