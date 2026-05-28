-module(imboy_migrate).

%%% @doc 数据库迁移模块
%%% 负责执行数据库结构升级和序列重置

-export([migrate/0]).
-export([migrate_to_msg_v2/0]).
-export([set_max_id_seq/0]).
-export([migrate_msg_payload_plaintext/0]).

-export([get_scripts_path/0]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

% imboy_migrate:migrate_to_msg_v2().
migrate_to_msg_v2() ->
    ?LOG_INFO("开始迁移到消息表 v2.0 格式..."),
    ?LOG_INFO("警告：此操作将删除所有消息数据，不可恢复！"),
    elib_pg:with_tx(fun(Conn) ->
        %% 删除旧表
        % elib_pg:query("DROP TABLE IF EXISTS msg_c2c CASCADE"),
        % elib_pg:query("DROP TABLE IF EXISTS msg_c2g CASCADE"),
        % elib_pg:query("DROP TABLE IF EXISTS msg_c2s CASCADE"),
        % elib_pg:query("DROP TABLE IF EXISTS msg_s2c CASCADE"),
        %% 步骤 1: 删除旧表（分别执行，因为 prepared statement 不支持多条语句）
        Tables = [
            <<"public.\"msg_c2c\"">>,
            <<"public.\"msg_c2g\"">>,
            <<"public.\"msg_c2s\"">>,
            <<"public.\"msg_s2c\"">>
        ],
        ?LOG_INFO("正在删除旧表..."),
        lists:foreach(
            fun(Table) ->
                Sql = <<"DROP TABLE IF EXISTS ", Table/binary>>,
                case epgsql:squery(Conn, Sql) of
                    {ok, _, _} ->
                        ?LOG_INFO("  表已删除: ~s", [Table]);
                    {ok, _} ->
                        ?LOG_INFO("  表已删除: ~s", [Table]);
                    {error, Reason} ->
                        ?LOG_ERROR("删除表失败 ~s: ~p", [Table, Reason]),
                        throw({drop_table_failed, Table, Reason})
                end
            end,
            Tables
        ),

        %% 步骤 2: 读取并执行 4 个 SQL 文件
        MigrationsPath = get_scripts_path(),
        SqlFiles = [
            filename:join(MigrationsPath, "000042_msg_c2c.up.sql"),
            filename:join(MigrationsPath, "000043_msg_c2g.up.sql"),
            filename:join(MigrationsPath, "000045_msg_c2s.up.sql"),
            filename:join(MigrationsPath, "000046_msg_s2c.up.sql")
        ],

        ?LOG_INFO("正在执行 SQL 迁移文件..."),
        ?LOG_INFO("迁移文件路径: ~s", [MigrationsPath]),
        execute_sql_files(Conn, SqlFiles)
    end).

%% @doc 执行数据库迁移
%% 读取 SQL 脚本并按顺序升级数据库结构
%% @returns ok | {error, Reason}
%% @example
%% imboy_migrate:migrate().
%% @note 升级相关的 SQL 文件必须是按顺序命名的
-spec migrate() -> ok | {error, term()}.
migrate() ->
    Conf = config_ds:env(super_account),
    Path = get_scripts_path(),
    ?LOG_INFO("[imboy_migrate] running migrations from ~s", [Path]),
    {ok, Conn} = epgsql:connect(Conf),
    Config = #{conn => Conn, dir => Path},
    try
        case erlang_migrate:up(Config) of
            ok ->
                ?LOG_INFO("[imboy_migrate] all migrations applied"),
                ok;
            {error, {dirty_state, Hint}} ->
                ?LOG_ERROR("[imboy_migrate] dirty state — run force/2 to recover: ~s", [Hint]),
                erlang:error({migration_dirty, Hint});
            {error, Reason} ->
                ?LOG_ERROR("[imboy_migrate] migration failed: ~p", [Reason]),
                erlang:error({migration_failed, Reason})
        end
    after
        epgsql:close(Conn)
    end.

migrate_msg_payload_plaintext() ->
    Key = config_ds:env(postgre_aes_key),
    Tables = [
        msg_c2c_repo:tablename(),
        msg_c2g_repo:tablename(),
        msg_c2s_repo:tablename(),
        msg_s2c_repo:tablename(),
        msg_store_repo:tablename()
    ],
    lists:foldl(
        fun(Tb, Acc) ->
            maps:put(Tb, migrate_msg_payload_plaintext(Tb, Key), Acc)
        end,
        #{},
        Tables
    ).

migrate_msg_payload_plaintext(Tb, Key) ->
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET payload = convert_from(decode(encode(decrypt(decode(replace(payload,'aes_cbc_',''),'base64'), $1::text, 'aes-cbc/pad:pkcs') , 'escape'), 'base64'), 'UTF8')",
            " WHERE payload IS NOT NULL AND payload <> '' AND payload ~ '^(aes_cbc_)?[A-Za-z0-9+/=]+$'">>,
    elib_pg:execute(Sql, [Key]).

%% @doc 设置序列的最大值
%% 执行 set_max_id_seq.sql 脚本重置所有表的自增序列
%% @returns #{success => SuccessCount, fail => FailCount}
%% @example
%% imboy_migrate:set_max_id_seq().
-spec set_max_id_seq() -> #{success := non_neg_integer(), fail := non_neg_integer()}.
set_max_id_seq() ->
    Conf = config_ds:env(super_account),
    Path = get_scripts_path(),
    {ok, Conn} = epgsql:connect(Conf),

    % Get the parent directory of Path
    ParentDir = filename:dirname(Path),
    File = filename:join(ParentDir, "set_max_id_seq.sql"),

    % Read the file content
    {ok, FileContent} = file:read_file(File),

    % Split into individual SQL statements and filter out comments/empty lines
    SqlStatements = lists:filtermap(
        fun(Stmt) ->
            case binary_to_list(Stmt) of
                "" ->
                    false;
                [First | _] = Line ->
                    case First of
                        % Skip shell-style comments
                        $# -> false;
                        % Skip SQL comments
                        $- when length(Line) > 1, hd(tl(Line)) =:= $- -> false;
                        _ -> {true, Line}
                    end
            end
        end,
        binary:split(FileContent, <<"\n">>, [global, trim])
    ),

    % Execute each statement and collect results
    {SuccessCount, FailCount} =
        lists:foldl(
            fun(Sql, {S, F}) ->
                logger:info("Executing: ~s~n", [Sql]),
                case epgsql:squery(Conn, Sql) of
                    {ok, _, _} = Success ->
                        logger:info("Success: ~p~n", [Success]),
                        {S + 1, F};
                    {error, Error} ->
                        logger:error("Error: ~p~n", [Error]),
                        {S, F + 1};
                    Other ->
                        logger:warning("Unexpected result: ~p~n", [Other]),
                        {S, F + 1}
                end
            end,
            {0, 0},
            SqlStatements
        ),

    ok = epgsql:close(Conn),
    #{success => SuccessCount, fail => FailCount}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

% 获取处理后的脚本绝对路径
-spec get_scripts_path() -> string().
get_scripts_path() ->
    % 从配置获取路径，支持 $PROJECT_DIR 占位符
    RawPath = config_ds:env(scripts_path),
    % 展开 $PROJECT_DIR 为实际路径
    case is_list(RawPath) of
        true ->
            expand_project_dir(RawPath);
        false when is_binary(RawPath) ->
            expand_project_dir(binary_to_list(RawPath));
        _ ->
            % 如果配置无效，使用默认路径
            filename:join(code:priv_dir(imboy), "migrations")
    end.

%% @doc 展开 $PROJECT_DIR 占位符为实际项目路径
%% @param Path 包含 $PROJECT_DIR 的路径字符串
%% @returns 展开后的绝对路径
-spec expand_project_dir(string()) -> string().
expand_project_dir(Path) ->
    case string:split(Path, "$PROJECT_DIR") of
        [_, ""] ->
            % 只有 $PROJECT_DIR，替换为 priv_dir
            code:priv_dir(imboy);
        [_, Relative] ->
            % $PROJECT_DIR/相对路径
            % 去掉前导斜杠，避免 filename:join 将其视为绝对路径
            Trimmed =
                case Relative of
                    % Unix 风格
                    [$/ | Rest] -> Rest;
                    % Windows 风格
                    [$\\ | Rest] -> Rest;
                    _ -> Relative
                end,
            filename:join(code:priv_dir(imboy), Trimmed);
        _ ->
            % 没有 $PROJECT_DIR，直接返回
            Path
    end.

-spec priv_is_valid(list()) -> boolean().
priv_is_valid(List) ->
    lists:all(
        fun(E) ->
            case E of
                {ok, _} -> true;
                % Handle results with 2 elements
                {ok, _, _} -> true;
                % Handle results with 3 elements
                {ok, _, _, _} -> true;
                _ -> false
            end
        end,
        List
    ).

%% @private 执行多个 SQL 文件
%% 读取并逐个执行 SQL 文件，如果任何一个失败则回滚事务
%% @param Conn 数据库连接
%% @param SqlFiles SQL 文件路径列表
%% @returns ok | {error, Reason}
-spec execute_sql_files(pid(), list(string())) -> ok | {error, term()}.
execute_sql_files(_Conn, []) ->
    ok;
execute_sql_files(Conn, [FilePath | Rest]) ->
    ?LOG_INFO("  执行 SQL 文件: ~s", [FilePath]),
    case read_sql_file(FilePath) of
        {ok, SqlContent} ->
            %% 将 SQL 文件内容按分号分割成多条语句
            Statements = split_sql_statements(SqlContent),
            ?LOG_INFO("  SQL 文件包含 ~p 条语句", [length(Statements)]),
            case execute_sql_statements(Conn, Statements) of
                ok ->
                    ?LOG_INFO("  SQL 文件执行成功: ~s", [FilePath]),
                    execute_sql_files(Conn, Rest);
                {error, Reason} ->
                    ?LOG_ERROR("  SQL 文件执行失败: ~s, 错误: ~p", [FilePath, Reason]),
                    {error, {sql_file_failed, FilePath, Reason}}
            end;
        {error, Reason} ->
            ?LOG_ERROR("  读取 SQL 文件失败: ~s, 错误: ~p", [FilePath, Reason]),
            {error, {read_file_failed, FilePath, Reason}}
    end.

%% @private 读取 SQL 文件内容
%% @param FilePath 文件路径
%% @returns {ok, Content} | {error, Reason}
-spec read_sql_file(string()) -> {ok, binary()} | {error, term()}.
read_sql_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            {ok, Content};
        {error, Reason} ->
            {error, {file_read_error, FilePath, Reason}}
    end.

%% @private 分割 SQL 语句
%% 将 SQL 文件内容按分号分割成多条语句，过滤掉注释和空行
%% @param SqlContent SQL 文件内容
%% @returns SQL 语句列表
-spec split_sql_statements(binary()) -> [binary()].
split_sql_statements(SqlContent) ->
    %% 先移除单行注释（以 -- 开头到行尾的内容）
    DoubleDash = <<"-">>,
    Pattern = <<DoubleDash/binary, DoubleDash/binary, "[^\\n]*">>,
    NoSingleLineComments = re:replace(SqlContent, Pattern, <<>>, [global, {return, binary}]),
    %% 按分号分割
    RawStatements = binary:split(NoSingleLineComments, <<";">>, [global]),
    %% 过滤掉空语句
    lists:filtermap(
        fun(Stmt) ->
            %% 去除两端空白
            Trimmed = trim_sql_whitespace(Stmt),
            case Trimmed of
                <<>> -> false;
                _ when byte_size(Trimmed) > 0 -> {true, Trimmed}
            end
        end,
        RawStatements
    ).

%% @private 去除 SQL 语句两端的空白字符
%% @param Sql SQL 语句
%% @returns 去除空白后的 SQL 语句
-spec trim_sql_whitespace(binary()) -> binary().
trim_sql_whitespace(Sql) ->
    %% 去除前导空白（空格、制表符、换行）
    Trimmed = re:replace(Sql, <<"^[\\s\\n\\r]+">>, <<>>, [{return, binary}]),
    %% 去除尾部空白
    re:replace(Trimmed, <<"[\\s\\n\\r]+$">>, <<>>, [{return, binary}]).

%% @private 执行 SQL 语句列表
%% 逐个执行 SQL 语句，如果任何一个失败则返回错误
%% @param Conn 数据库连接
%% @param Statements SQL 语句列表
%% @returns ok | {error, Reason}
-spec execute_sql_statements(pid(), [binary()]) -> ok | {error, term()}.
execute_sql_statements(_Conn, []) ->
    ok;
execute_sql_statements(Conn, [Stmt | Rest]) ->
    ?LOG_INFO("    执行: ~s", [Stmt]),
    case epgsql:squery(Conn, Stmt) of
        %% 成功情况：单个结果
        {ok, [], []} ->
            ?LOG_INFO("      成功"),
            execute_sql_statements(Conn, Rest);
        {ok, _Count} ->
            ?LOG_INFO("      成功"),
            execute_sql_statements(Conn, Rest);
        {ok, _Cols, _Rows} ->
            ?LOG_INFO("      成功"),
            execute_sql_statements(Conn, Rest);
        {ok, _Count, _Cols, _Rows} ->
            ?LOG_INFO("      成功"),
            execute_sql_statements(Conn, Rest);
        %% 错误情况：单个错误元组
        {error, _Reason} = Error ->
            ?LOG_ERROR("      失败: ~p~n", [Error]),
            ?LOG_ERROR("      SQL: ~s", [Stmt]),
            {error, {statement_failed, Stmt, Error}};
        %% 成功情况：结果列表
        [{ok, _, _} | _] ->
            ?LOG_INFO("      成功"),
            execute_sql_statements(Conn, Rest);
        [{ok, _, _, _} | _] ->
            ?LOG_INFO("      成功"),
            execute_sql_statements(Conn, Rest);
        [{ok, _} | _] ->
            ?LOG_INFO("      成功"),
            execute_sql_statements(Conn, Rest);
        %% 错误情况：列表中包含错误
        [{error, _} | _] = Error ->
            ?LOG_ERROR("      失败: ~p~n", [Error]),
            ?LOG_ERROR("      SQL: ~s", [Stmt]),
            {error, {statement_failed, Stmt, Error}};
        %% 其他情况
        Other ->
            ?LOG_ERROR("      未知返回: ~p~n", [Other]),
            {error, {unexpected_result, Stmt, Other}}
    end.
