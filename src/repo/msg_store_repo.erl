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
-export([create_indexes/1]).

%% 写入操作
-export([stage/10]).

%% 删除操作
-export([unstage/2]).
-export([claim_pending/2]).
-export([mark_processed/1]).
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
    elib_pg_sql:public_tablename(<<"msg_store_staging">>).


%% @doc 写入备份表（v2.0）
%% @param Type 消息类别 (c2c/c2g/s2c/c2s)
%% @param MsgId 消息唯一ID
%% @param MsgType 消息子类型 (text/image/video/etc)
%% @param Action S2C 操作类型
%% @param E2EE 端到端加密元数据 (JSONB map 或 null)
%% @param Payload 消息内容 (JSON binary)
%% @param FromId 发送者用户ID
%% @param ToId 接收者用户ID (单聊) 或 ToIdList (群聊)
%% @param CreatedAt 消息创建时间 (RFC3339 binary)
%% @param ServerTs 服务器时间戳 (RFC3339 binary)
-spec stage(binary(), binary(), binary(), binary(), map(), binary(), integer(), integer() | [integer()], binary(), binary()) ->
          {ok, term()} | {ok, term(), term()} | {error, term()}.
stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs) when is_integer(ToId) ->
    Tb = tablename(),
    Data = #{
        type => Type,
        msg_id => MsgId,
        msg_type => MsgType,
        action => Action,
        e2ee => case E2EE of
            null -> null;
            <<>> -> null;
            Map when is_map(Map) -> jsone:encode(Map, [native_utf8]);  % map 需要 encode
            Bin when is_binary(Bin) -> Bin;  % 已经是 JSON binary（可能是双重编码的源头）
            _ -> null
        end,
        payload => Payload,
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTs,
        retry_count => 0},
    %% 【幂等性修复】捕获唯一约束错误
    case elib_pg:insert(Tb, Data) of
        {ok, _} = OkResult -> OkResult;
        {error, {error, {error, <<"23505">>, unique_violation, _, _}}} ->
            %% PostgreSQL 唯一约束错误：消息已存在（幂等性）
            {error, {unique_violation, MsgId}};
        {error, {error, Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end;

stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToIdList, CreatedAt, ServerTs) when is_list(ToIdList) ->
    Tb = tablename(),
    Data = #{type => Type,
             msg_id => MsgId,
             msg_type => MsgType,
             action => Action,
             e2ee => case E2EE of
                 null -> null;
                 <<>> -> null;
                 Map when is_map(Map) -> jsone:encode(Map, [native_utf8]);  % map 需要 encode
                 Bin when is_binary(Bin) -> Bin;  % 已经是 JSON binary（可能是双重编码的源头）
                 _ -> null
             end,
             payload => Payload,
             from_id => FromId,
             to_id_list => ToIdList,
             created_at => CreatedAt,
             server_ts => ServerTs,
             retry_count => 0},
    %% 【幂等性修复】捕获唯一约束错误
    case elib_pg:insert(Tb, Data) of
        {ok, _} = OkResult -> OkResult;
        {error, {error, {error, <<"23505">>, unique_violation, _, _}}} ->
            %% PostgreSQL 唯一约束错误：消息已存在（幂等性）
            {error, {unique_violation, MsgId}};
        {error, {error, Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc 删除备份表记录（消息成功写入正式表后调用）
-spec unstage(binary(), binary()) -> {ok, integer()} | {error, any()}.
unstage(Type, MsgId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE type = $1 AND msg_id = $2">>,
    elib_pg:execute(Sql, [Type, MsgId]).


%% @doc 抢占未处理消息（FOR UPDATE SKIP LOCKED），并设置 lease（available_at）
-spec claim_pending(pos_integer(), pos_integer()) -> {ok, list(map())} | {error, term()}.
claim_pending(Limit, LeaseSeconds) ->
    Tb = tablename(),
    elib_pg:with_tx(fun(Conn) ->
        Sql = <<"SELECT id, type, msg_id, payload, from_id, to_id, to_id_list, created_at, server_ts, retry_count, "
                "msg_type, action, e2ee "
                "FROM ", Tb/binary,
                " WHERE processed_at IS NULL ",
                " AND available_at <= NOW() ",
                " ORDER BY created_at ASC ",
                " LIMIT $1 ",
                " FOR UPDATE SKIP LOCKED">>,
        case elib_pg:query(Conn, Sql, [Limit]) of
            {ok, Rows} ->
                case Rows of
                    [] ->
                        {ok, []};
                    _ ->
                        Ids = [maps:get(<<"id">>, Row) || Row <- Rows],
                        LeaseSql = <<"UPDATE ", Tb/binary,
                                     " SET available_at = NOW() + INTERVAL '1 second' * $1 ",
                                     " WHERE id = ANY($2)">>,
                        _ = elib_pg:execute(Conn, LeaseSql, [LeaseSeconds, Ids]),
                        {ok, Rows}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    end).


%% @doc 标记消息已处理（不区分类型，一条 SQL 更新所有类型）
%% 优化版本：移除 type 条件，避免对每种类型都执行一次 UPDATE
%% @param MsgId 消息唯一ID
%% @return {ok, Count} | {error, any()}
-spec mark_processed(binary()) -> {ok, integer()} | {error, any()}.
mark_processed(MsgId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET processed_at = NOW(), error_msg = NULL ",
            " WHERE msg_id = $1">>,
    elib_pg:execute(Sql, [MsgId]).


%% @doc 标记消息已处理（不会删除记录，留给定时清理）
%% @deprecated 使用 mark_processed/1 代替，避免重复执行
-spec mark_processed(binary(), binary()) -> {ok, integer()} | {error, any()}.
mark_processed(Type, MsgId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET processed_at = NOW(), error_msg = NULL ",
            " WHERE type = $1 AND msg_id = $2">>,
    elib_pg:execute(Sql, [Type, MsgId]).


%% @doc 标记失败并设置下次重试时间
-spec mark_failed(binary(), binary(), binary(), pos_integer()) -> {ok, integer()} | {error, any()}.
mark_failed(Type, MsgId, ErrorMsg, DelaySeconds) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET retry_count = retry_count + 1, ", " error_msg = $3, ",
            " available_at = NOW() + INTERVAL '1 second' * $4 ",
            " WHERE type = $1 AND msg_id = $2 AND processed_at IS NULL">>,
    elib_pg:execute(Sql, [Type, MsgId, ErrorMsg, DelaySeconds]).


%% @doc 获取未处理的备份消息（用于启动时恢复）
-spec get_unstaged(integer()) -> {ok, list(map())} | {error, any()}.
get_unstaged(Limit) ->
    Tb = tablename(),
    Sql = <<"SELECT msg_type, msg_id, payload, from_id, to_id, to_id_list, created_at, server_ts "
            "FROM ", Tb/binary,
            " WHERE processed_at IS NULL ",
            "ORDER BY created_at ASC ",
            "LIMIT $1">>,
    elib_pg:query(Sql, [Limit]).


%% @doc 清理已处理的备份消息（定时任务调用）
-spec delete_processed(integer()) -> {ok, integer()} | {error, any()}.
delete_processed(Seconds) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary,
            " WHERE processed_at IS NOT NULL ",
            " AND processed_at < NOW() - INTERVAL '1 second' * $1">>,
    elib_pg:execute(Sql, [Seconds]).


%% @doc 获取备份表的统计信息
-spec get_staging_stats() -> {ok, map()} | {error, any()}.
get_staging_stats() ->
    Tb = tablename(),
    Sql = <<"SELECT ",
            "COUNT(*) FILTER (processed_at IS NULL) as pending, ",
            "COUNT(*) FILTER (processed_at IS NOT NULL) as processed, ",
            "COUNT(*) FILTER (error_msg IS NOT NULL) as failed, ",
            "COUNT(*) as total ",
            "FROM ", Tb/binary>>,
    elib_pg:query(Sql, []).


%% @doc 清空备份表（慎用！）
-spec truncate_processed() -> {ok, integer()} | {error, any()}.
truncate_processed() ->
    Tb = tablename(),
    elib_pg:query(<<"TRUNCATE TABLE ", Tb/binary>>, []).


%% @doc 清理备份表空间
-spec vacuum_table() -> {ok, term()} | {error, any()}.
vacuum_table() ->
    Tb = tablename(),
    elib_pg:query(<<"VACUUM ANALYZE ", Tb/binary>>, []).


%% @doc 确保备份表存在
%% 动态创建 msg_store_staging 表及其索引
-spec ensure_table_exists() -> ok | {error, any()}.
ensure_table_exists() ->
    Tb = tablename(),
    case elib_pg:execute(
        <<"CREATE TABLE IF NOT EXISTS ", Tb/binary, " (
            id BIGSERIAL PRIMARY KEY,
            type VARCHAR(10) NOT NULL,
            msg_id VARCHAR(50) NOT NULL,
            msg_type VARCHAR(50),
            action VARCHAR(50),
            e2ee JSONB,
            payload JSONB NOT NULL,
            from_id BIGINT NOT NULL,
            to_id BIGINT,
            to_id_list BIGINT[],
            created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
            server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
            retry_count INTEGER NOT NULL DEFAULT 0,
            processed_at TIMESTAMPTZ,
            available_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
            error_msg TEXT,
            CONSTRAINT msg_store_staging_type_msg_id_key UNIQUE (type, msg_id)
        )">>,
        []
    ) of
        {ok, _} ->
            %% 创建索引
            create_indexes(Tb);
        {error, {error, Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建备份表的索引
-spec create_indexes(binary()) -> ok | {error, any()}.
create_indexes(Tb) ->
    %% 创建 processed_at 索引（用于清理已处理记录）
    _ = elib_pg:execute(
        <<"CREATE INDEX IF NOT EXISTS ", Tb/binary, "_processed_at_idx
            ON ", Tb/binary, " (processed_at) WHERE processed_at IS NOT NULL">>,
        []
    ),
    %% 创建 available_at 索引（用于抢占待处理记录）
    _ = elib_pg:execute(
        <<"CREATE INDEX IF NOT EXISTS ", Tb/binary, "_available_at_idx
            ON ", Tb/binary, " (available_at) WHERE processed_at IS NULL">>,
        []
    ),
    %% 创建 created_at 索引（用于按时间排序）
    _ = elib_pg:execute(
        <<"CREATE INDEX IF NOT EXISTS ", Tb/binary, "_created_at_idx
            ON ", Tb/binary, " (created_at)">>,
        []
    ),
    ok.
