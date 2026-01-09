-module(msg_store_repo).
%%%
% msg_store_repo 是消息写入队列备份表的仓库层
% 提供备份表的 CRUD 操作，保证消息零丢失
%%%

-include("log.hrl").

%% ==================== API ====================

-export([tablename/0]).

%% 表管理
-export([ensure_table_exists/0]).

%% 写入操作
-export([stage/7]).

%% 删除操作
-export([unstage/2]).
-export([claim_pending/2]).
-export([mark_processed/2]).
-export([mark_failed/4]).
-export([delete_processed/1]).
-export([truncate_processed/0]).
-export([vacuum_table/0]).

%% 查询操作
-export([get_unstaged/1]).
-export([get_staging_stats/0]).

%% ==================== API Functions ====================

%% @doc 获取备份表名
-spec tablename() -> binary().
tablename() ->
    imboy_pg_sql:public_tablename(<<"msg_store_staging">>).

%% @doc 写入备份表
-spec stage(binary(), binary(), binary(), integer(), integer() | [integer()], binary(), binary()) ->
    {ok, term()} | {ok, term(), term()} | {error, term()}.
stage(MsgType, MsgId, Payload, FromId, ToId, CreatedAt, ServerTs) when is_integer(ToId) ->
    Tb = tablename(),
    Data = #{
        msg_type => MsgType,
        msg_id => MsgId,
        payload => {raw, imboy_hasher:encoded_val(Payload)},
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTs,
        retry_count => 0
    },
    imboy_pg:insert(Tb, Data);

stage(MsgType, MsgId, Payload, FromId, ToIdList, CreatedAt, ServerTs) when is_list(ToIdList) ->
    Tb = tablename(),
    Data = #{
        msg_type => MsgType,
        msg_id => MsgId,
        payload => {raw, imboy_hasher:encoded_val(Payload)},
        from_id => FromId,
        to_id_list => ToIdList,
        created_at => CreatedAt,
        server_ts => ServerTs,
        retry_count => 0
    },
    imboy_pg:insert(Tb, Data).

%% @doc 删除备份表记录（消息成功写入正式表后调用）
-spec unstage(binary(), binary()) -> {ok, integer()} | {error, any()}.
unstage(MsgType, MsgId) ->
    Tb = tablename(),
    % 使用参数化查询避免 SQL 注入
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE msg_type = $1 AND msg_id = $2">>,
    imboy_pg:execute(Sql, [MsgType, MsgId]).

%% @doc 抢占未处理消息（FOR UPDATE SKIP LOCKED），并设置 lease（available_at）
-spec claim_pending(pos_integer(), pos_integer()) -> {ok, list(map())} | {error, term()}.
claim_pending(Limit, LeaseSeconds) ->
    Tb = tablename(),
    imboy_pg:with_tx(fun(Conn) ->
        Sql = <<"SELECT id, msg_type, msg_id, payload, from_id, to_id, to_id_list, created_at, server_ts, retry_count ",
                "FROM ", Tb/binary,
                " WHERE processed_at IS NULL ",
                " AND available_at <= NOW() ",
                " ORDER BY created_at ASC ",
                " LIMIT $1 ",
                " FOR UPDATE SKIP LOCKED">>,
        case imboy_pg:query(Conn, Sql, [Limit]) of
            {ok, Rows} ->
                case Rows of
                    [] ->
                        {ok, []};
                    _ ->
                        Ids = [maps:get(<<"id">>, Row) || Row <- Rows],
                        LeaseSql = <<"UPDATE ", Tb/binary,
                                     " SET available_at = NOW() + INTERVAL '1 second' * $1 ",
                                     " WHERE id = ANY($2)">>,
                        _ = imboy_pg:execute(Conn, LeaseSql, [LeaseSeconds, Ids]),
                        {ok, Rows}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    end).

%% @doc 标记消息已处理（不会删除记录，留给定时清理）
-spec mark_processed(binary(), binary()) -> {ok, integer()} | {error, any()}.
mark_processed(MsgType, MsgId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET processed_at = NOW(), error_msg = NULL ",
            " WHERE msg_type = $1 AND msg_id = $2">>,
    imboy_pg:execute(Sql, [MsgType, MsgId]).

%% @doc 标记失败并设置下次重试时间
-spec mark_failed(binary(), binary(), binary(), pos_integer()) -> {ok, integer()} | {error, any()}.
mark_failed(MsgType, MsgId, ErrorMsg, DelaySeconds) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET retry_count = retry_count + 1, ",
            " error_msg = $3, ",
            " available_at = NOW() + INTERVAL '1 second' * $4 ",
            " WHERE msg_type = $1 AND msg_id = $2 AND processed_at IS NULL">>,
    imboy_pg:execute(Sql, [MsgType, MsgId, ErrorMsg, DelaySeconds]).

%% @doc 获取未处理的备份消息（用于启动时恢复）
-spec get_unstaged(integer()) -> {ok, list(map())} | {error, any()}.
get_unstaged(Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT msg_type, msg_id, payload, from_id, to_id, to_id_list, created_at, server_ts ",
            "FROM ", Tb/binary,
            " WHERE processed_at IS NULL ",
            "ORDER BY created_at ASC ",
            "LIMIT $1">>,
    imboy_pg:query(Sql, [Limit]).

%% @doc 清理已处理的备份消息（定时任务调用）
-spec delete_processed(integer()) -> {ok, integer()} | {error, any()}.
delete_processed(Seconds) ->
    Tb = tablename(),
    % 清理超过指定秒数的已处理消息
    Sql = <<"DELETE FROM ", Tb/binary,
            " WHERE processed_at IS NOT NULL ",
            " AND processed_at < NOW() - INTERVAL '1 second' * $1">>,
    imboy_pg:execute(Sql, [Seconds]).

%% @doc 使用 TRUNCATE 快速清空所有已处理的备份消息
% TRUNCATE 比 DELETE 更快，且立即释放磁盘空间
% 注意：此函数会先保存未处理消息，然后 TRUNCATE，再恢复未处理消息
-spec truncate_processed() -> {ok, integer()} | {error, any()}.
truncate_processed() ->
    Tb = tablename(),
    TempTb = <<Tb/binary, "_temp">>,
    % 1. 创建临时表保存未处理消息
    CreateTempSql = <<
        "CREATE TEMP TABLE ", TempTb/binary, " AS ",
        "SELECT * FROM ", Tb/binary, " WHERE processed_at IS NULL"
    >>,
    case imboy_pg:execute(CreateTempSql, []) of
        {ok, _} ->
            % 2. TRUNCATE 原表（立即释放空间）
            TruncateSql = <<"TRUNCATE TABLE ", Tb/binary>>,
            case imboy_pg:execute(TruncateSql, []) of
                {ok, _} ->
                    % 3. 从临时表恢复未处理消息
                    RestoreSql = <<
                        "INSERT INTO ", Tb/binary, " ",
                        "SELECT * FROM ", TempTb/binary
                    >>,
                    case imboy_pg:execute(RestoreSql, []) of
                        {ok, _} ->
                            % 4. 删除临时表
                            DropTempSql = <<"DROP TABLE ", TempTb/binary>>,
                            imboy_pg:execute(DropTempSql, []);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 手动执行 VACUUM 清理死元组
% 建议在低峰期定期执行（如每天凌晨）
-spec vacuum_table() -> ok | {error, any()}.
vacuum_table() ->
    Tb = tablename(),
    % VACUUM FULL 会重建表，释放空间但锁表
    % VACUUM (ANALYZE) 只清理死元组并更新统计信息，不锁表
    Sql = <<"VACUUM (ANALYZE, VERBOSE) ", Tb/binary>>,
    case imboy_pg:execute(Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取备份表统计信息
-spec get_staging_stats() -> map().
get_staging_stats() ->
    Tb = tablename(),
    Sql = <<"SELECT ",
            "COUNT(*) FILTER (WHERE processed_at IS NULL) as pending, ",
            "COUNT(*) FILTER (WHERE processed_at IS NOT NULL) as processed, ",
            "COUNT(*) as total "
            "FROM ", Tb/binary>>,
    case imboy_pg:query(Sql, []) of
        {ok, [Stats]} -> Stats;
        {ok, []} -> #{pending => 0, processed => 0, total => 0};
        _ -> #{pending => 0, processed => 0, total => 0}
    end.

%% @doc 确保表存在（自动创建）
-spec ensure_table_exists() -> ok | {error, any()}.
ensure_table_exists() ->
    % 检查表是否存在
    CheckSql = <<"SELECT COUNT(*) as count FROM information_schema.tables ",
                 "WHERE table_schema = 'public' AND table_name = 'msg_store_staging'">>,
    case imboy_pg:query(CheckSql, []) of
        {ok, []} ->
            ok = ?INFO_LOG("msg_store_staging table not found, creating..."),
            create_table();
        {ok, [#{<<"count">> := 0}]} ->
            ok = ?INFO_LOG("msg_store_staging table not found, creating..."),
            create_table();
        {error, Reason} ->
            ok = ?ERROR_LOG("Failed to check table existence: ~p", [Reason]),
            {error, Reason};
        Unexpected ->
            ok = ?ERROR_LOG("Unexpected result when checking table existence: ~p", [Unexpected]),
            {error, Unexpected}
    end.

%% @doc 创建备份表
-spec create_table() -> ok | {error, any()}.
create_table() ->
    Tb = tablename(),
    % 创建表（带存储参数优化 autovacuum）
    CreateTableSql = <<
        "CREATE TABLE IF NOT EXISTS ", Tb/binary, " ("
        "id BIGSERIAL PRIMARY KEY,"
        "msg_type VARCHAR(10) NOT NULL CHECK (msg_type IN ('c2c', 'c2g', 's2c', 'c2s')),"
        "msg_id VARCHAR(64) NOT NULL,"
        "payload TEXT NOT NULL,"
        "from_id BIGINT NOT NULL,"
        "to_id BIGINT,"
        "to_id_list BIGINT[],"
        "created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),"
        "server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),"
        "retry_count INTEGER DEFAULT 0,"
        "processed_at TIMESTAMPTZ,"
        "error_msg TEXT,"
        "available_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),"
        "CONSTRAINT uk_staging_msg_id UNIQUE (msg_type, msg_id)"
        ") WITH ("
        "  autovacuum_vacuum_scale_factor = 0.1,"  % 10% 的数据变化时触发 VACUUM（默认 20%）
        "  autovacuum_vacuum_threshold = 50,"        % 至少 50 条记录变化时触发 VACUUM
        "  autovacuum_analyze_scale_factor = 0.05," % 5% 的数据变化时触发 ANALYZE
        "  autovacuum_analyze_threshold = 25,"       % 至少 25 条记录变化时触发 ANALYZE
        "  fillfactor = 70"                          % 页面填充率 70%，为 HOT UPDATE 留出空间
        ")"
    >>,

    case imboy_pg:execute(CreateTableSql, []) of
        {ok, _} ->
            % 创建索引
            Indexes = [
                <<"CREATE INDEX IF NOT EXISTS idx_staging_created ON ", Tb/binary, "(created_at)">>,
                <<"CREATE INDEX IF NOT EXISTS idx_staging_msg_type ON ", Tb/binary, "(msg_type)">>,
                <<"CREATE INDEX IF NOT EXISTS idx_staging_to_id ON ", Tb/binary, "(to_id) WHERE to_id IS NOT NULL">>,
                <<"CREATE INDEX IF NOT EXISTS idx_staging_processed ON ", Tb/binary, "(processed_at) WHERE processed_at IS NULL">>,
                <<"CREATE INDEX IF NOT EXISTS idx_staging_available ON ", Tb/binary, "(available_at) WHERE processed_at IS NULL">>
            ],
            lists:foreach(fun(IndexSql) ->
                imboy_pg:execute(IndexSql, [])
            end, Indexes),
            ok = ?INFO_LOG("msg_store_staging table created successfully"),
            ok;
        {error, Reason} ->
            ok = ?ERROR_LOG("Failed to create msg_store_staging table: ~p", [Reason]),
            {error, Reason}
    end.
