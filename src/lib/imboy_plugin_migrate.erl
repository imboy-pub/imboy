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
%%% Source of truth: docs/plugin/contract.md §3 / roadmap P3
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-export([
    list_migration_files/1,
    parse_migration_filename/1,
    diff_pending/2,
    plugin_dir/1,
    %% 切片 2：副作用层 / Side-effects layer
    migration_table_name/1,
    ensure_table_sql/1,
    record_applied_sql/3,
    applied_seqs_sql/1,
    run_pending/2,
    %% 切片 2 卸载支持 / Uninstall support
    drop_schema_migrations_table_sql/1,
    list_uninstall_file/1,
    uninstall_sql_file_path/1,
    run_uninstall/2
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

%% ===================================================================
%% 切片 2：副作用层 / Side-effects layer
%% ===================================================================

%% @doc 生成 schema_migrations_<plugin> 追踪表名。
-spec migration_table_name(atom()) -> binary().
migration_table_name(PluginName) when is_atom(PluginName) ->
    NameBin = atom_to_binary(PluginName, utf8),
    <<"schema_migrations_", NameBin/binary>>.

%% @doc 生成 CREATE TABLE IF NOT EXISTS schema_migrations_<plugin> SQL。
-spec ensure_table_sql(atom()) -> binary().
ensure_table_sql(PluginName) when is_atom(PluginName) ->
    Tab = migration_table_name(PluginName),
    <<"CREATE TABLE IF NOT EXISTS ", Tab/binary,
      " (version INTEGER PRIMARY KEY, "
      "description TEXT, "
      "applied_at TIMESTAMPTZ DEFAULT now())">>.

%% @doc 生成 INSERT 已执行 migration 记录的 SQL。
-spec record_applied_sql(atom(), pos_integer(), binary()) -> binary().
record_applied_sql(PluginName, Seq, Descr) when is_atom(PluginName), is_integer(Seq), is_binary(Descr) ->
    Tab = migration_table_name(PluginName),
    SeqBin = integer_to_binary(Seq),
    <<"INSERT INTO ", Tab/binary, " (version, description) VALUES (",
      SeqBin/binary, ", '", Descr/binary, "')">>.

%% @doc 生成查询已执行 seq 列表的 SQL。
-spec applied_seqs_sql(atom()) -> binary().
applied_seqs_sql(PluginName) when is_atom(PluginName) ->
    Tab = migration_table_name(PluginName),
    <<"SELECT version FROM ", Tab/binary, " ORDER BY version">>.

%% @doc 执行插件的 pending migration 文件。
%% 1. 确保 schema_migrations 表存在
%% 2. 查询已执行 seq
%% 3. diff_pending 计算待执行列表
%% 4. 逐文件执行 SQL，每文件独立事务
%% 5. 成功后记录到 schema_migrations
-spec run_pending(atom(), map()) -> {ok, [string()]} | {error, term()}.
run_pending(PluginName, Manifest) when is_atom(PluginName), is_map(Manifest) ->
    try
        %% 1. ensure table
        CreateSQL = ensure_table_sql(PluginName),
        {ok, _} = elib_pg:query(CreateSQL, []),

        %% 2. get applied seqs
        SelectSQL = applied_seqs_sql(PluginName),
        Applied = case elib_pg:query(SelectSQL, []) of
            {ok, Rows} -> [V || #{version := V} <- Rows];
            {error, {undefined_table, _}} -> []
        end,

        %% 3. list disk files & diff
        {ok, DiskFiles} = list_migration_files(Manifest),
        Pending = diff_pending(DiskFiles, Applied),

        %% 4. execute each pending file
        Dir = plugin_dir(Manifest),
        Executed = execute_pending_files(PluginName, Dir, Pending),
        {ok, Executed}
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason};
        Class:Reason2 ->
            {error, {Class, Reason2}}
    end.

%% @doc 逐文件执行 pending migration（每文件独立事务）。
%% 执行成功则记录 seq，失败则停止并返回已执行列表。
-spec execute_pending_files(atom(), string(), [string()]) -> [string()].
execute_pending_files(_PluginName, _Dir, []) ->
    [];
execute_pending_files(PluginName, Dir, [File | Rest]) ->
    FilePath = filename:join(Dir, File),
    case file:read_file(FilePath) of
        {ok, SQLBin} ->
            case elib_pg:with_tx(fun(_Conn) ->
                case elib_pg:query(SQLBin, []) of
                    {ok, _} -> ok;
                    {error, Reason} -> throw({migration_failed, File, Reason})
                end
            end) of
                {atomic, ok} ->
                    {ok, _, Seq, Descr} = parse_migration_filename(File),
                    DescrBin = list_to_binary(Descr),
                    RecSQL = record_applied_sql(PluginName, Seq, DescrBin),
                    {ok, _} = elib_pg:query(RecSQL, []),
                    [File | execute_pending_files(PluginName, Dir, Rest)];
                {aborted, Reason} ->
                    [{File, {error, Reason}} | [{R, skipped} || R <- Rest]]
            end;
        {error, Reason} ->
            [{File, {error, {file_read, Reason}}}]
    end.

%% ===================================================================
%% Uninstall 支持 / Uninstall support
%% ===================================================================

%% @doc 生成 DROP TABLE schema_migrations_<plugin> SQL。
-spec drop_schema_migrations_table_sql(atom()) -> binary().
drop_schema_migrations_table_sql(PluginName) when is_atom(PluginName) ->
    Tab = migration_table_name(PluginName),
    <<"DROP TABLE IF EXISTS ", Tab/binary>>.

%% @doc 计算 uninstall.sql 文件路径。
-spec uninstall_sql_file_path(map()) -> string().
uninstall_sql_file_path(#{name := Name, migrations := #{dir := DirBin}})
        when is_binary(DirBin) ->
    filename:join([
        "priv", "plugins",
        atom_to_list(Name),
        binary_to_list(DirBin),
        "uninstall.sql"
    ]).

%% @doc 查找 uninstall.sql 文件是否存在。
-spec list_uninstall_file(map()) -> {ok, string()} | {error, not_found}.
list_uninstall_file(Manifest) when is_map(Manifest) ->
    Path = uninstall_sql_file_path(Manifest),
    case filelib:is_file(Path) of
        true -> {ok, Path};
        false -> {error, not_found}
    end.

%% @doc 执行插件卸载：
%% 1. 可选执行 uninstall.sql（事务包裹）
%% 2. 删除 schema_migrations_<plugin> 追踪表
-spec run_uninstall(atom(), map()) -> ok | {error, term()}.
run_uninstall(PluginName, Manifest) when is_atom(PluginName), is_map(Manifest) ->
    try
        %% 1. optional uninstall.sql
        case list_uninstall_file(Manifest) of
            {ok, Path} ->
                {ok, SQLBin} = file:read_file(Path),
                case elib_pg:with_tx(fun(_Conn) ->
                    case elib_pg:query(SQLBin, []) of
                        {ok, _} -> ok;
                        {error, Reason} -> throw({uninstall_sql_failed, Reason})
                    end
                end) of
                    {atomic, ok} -> ok;
                    {aborted, Reason} -> throw({uninstall_sql_failed, Reason})
                end;
            {error, not_found} ->
                ok
        end,

        %% 2. drop tracking table
        DropSQL = drop_schema_migrations_table_sql(PluginName),
        {ok, _} = elib_pg:query(DropSQL, []),

        ok
    catch
        error:{badmatch, {error, R}} ->
            {error, R};
        throw:R ->
            {error, R};
        Class:R ->
            {error, {Class, R}}
    end.
