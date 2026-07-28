-module(msg_store_repo).
%%%
% msg_store_repo 是消息写入队列备份表的仓库层
% 提供备份表的 CRUD 操作，保证消息零丢失
%%%

-include("log.hrl").

%% ==================== API ====================

-export([tablename/0]).

-ifdef(TEST).
%% 仅测试导出：payload/e2ee 的 JSONB 规范化（数字开头密文误判回归）
-export([msg_store_payload_to_jsonb/1]).
-export([msg_store_e2ee_to_jsonb/1]).
-endif.

%% 表管理
-export([ensure_table_exists/0]).
-export([create_indexes/1]).

%% 写入操作
-export([stage/10]).
-export([stage/11]).

%% 删除操作
-export([unstage/2]).
-export([claim_pending/2]).
-export([mark_processed/1]).
-export([mark_failed/4]).
-export([delete_processed/1]).
-export([truncate_processed/0]).
-export([vacuum_table/0]).

%% 查询操作
-export([get_unstaged/1]).
-export([get_staging_stats/0]).
-export([find_by_msg_id/1]).

%% ==================== API Functions ====================

%% @doc 按消息 ID 查 staging 行（秒撤兜底：消息可能仍在异步管道未落正式表）
-spec find_by_msg_id(binary()) -> {ok, map()} | {error, not_found} | {error, term()}.
find_by_msg_id(MsgId) ->
    Tb = tablename(),
    Sql =
        <<"SELECT msg_id, from_id, to_id, created_at FROM ", Tb/binary,
            " WHERE msg_id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [MsgId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

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
-spec stage(
    binary(),
    binary(),
    binary(),
    binary(),
    map(),
    binary(),
    integer(),
    integer() | [integer()],
    binary(),
    binary()
) ->
    {ok, term()} | {ok, term(), term()} | {error, term()}.
stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs) ->
    stage(Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs, <<>>).

%% @doc 写入备份表（A2-a：带服务端验证过的发送者设备标识）
%%
%% 相比 stage/10 多一个 SenderDid：PFv3 接收侧 context binding 第 6 项拿
%% 信封顶层的 `sender_did` 与受认证的 `protected_header.sender_did` 硬比对
%% （ADR 15 §3.3）。实时投递靠 `message_ds:with_sender_device/2` 现场盖章，
%% 离线（decrypt-on-read）路径没有「现场」——必须在 staging 落库时就存下来，
%% 否则重连拉取的 v3 消息永久判 `context_mismatch_sender_did` 不可读。
%%
%% SenderDid 为 `<<>>` 时**不写该列**（保持 NULL）：空串不是设备标识，
%% 写空串会让接收侧把「服务端没提供」误判成「设备 ID 是空串」。
%%
%% @param SenderDid 发送者设备 ID（取自 WebSocket 连接认证态，客户端不可伪造）
-spec stage(
    binary(),
    binary(),
    binary(),
    binary(),
    map(),
    binary(),
    integer(),
    integer() | [integer()],
    binary(),
    binary(),
    binary()
) ->
    {ok, term()} | {ok, term(), term()} | {error, term()}.
stage(
    Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToId, CreatedAt, ServerTs, SenderDid
) when
    is_integer(ToId)
->
    Tb = tablename(),
    Data0 = #{
        type => Type,
        msg_id => MsgId,
        msg_type => MsgType,
        action => Action,
        e2ee => msg_store_e2ee_to_jsonb(E2EE),
        %% payload 列是 JSONB：
        %%  - 普通消息：Payload 已经是合法 JSON binary（如 {"text":"..."}）
        %%  - E2EE 消息：Payload 是裸 base64 密文 binary，必须包装为 JSON 字符串
        %%  - Map：编码为 JSON object
        payload => msg_store_payload_to_jsonb(Payload),
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTs,
        retry_count => 0
    },
    Data = put_sender_did(Data0, SenderDid),
    %% 预生成 TSID
    GenId = elib_tsid:generate(msg_store),
    Data2 = Data#{id => GenId},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    %% 【幂等性修复】捕获唯一约束错误
    case elib_pg:query(Sql, Params) of
        {ok, _} ->
            {ok, GenId};
        {error, {error, {error, <<"23505">>, unique_violation, _, _}}} ->
            %% PostgreSQL 唯一约束错误：消息已存在（幂等性）
            {error, {unique_violation, MsgId}};
        {error, {error, Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end;
stage(
    Type, MsgId, MsgType, Action, E2EE, Payload, FromId, ToIdList, CreatedAt, ServerTs, SenderDid
) when
    is_list(ToIdList)
->
    Tb = tablename(),
    Data0 = #{
        type => Type,
        msg_id => MsgId,
        msg_type => MsgType,
        action => Action,
        e2ee => msg_store_e2ee_to_jsonb(E2EE),
        %% payload 列是 JSONB：
        %%  - 普通消息：Payload 已经是合法 JSON binary（如 {"text":"..."}）
        %%  - E2EE 消息：Payload 是裸 base64 密文 binary，必须包装为 JSON 字符串
        %%  - Map：编码为 JSON object
        payload => msg_store_payload_to_jsonb(Payload),
        from_id => FromId,
        to_id_list => ToIdList,
        created_at => CreatedAt,
        server_ts => ServerTs,
        retry_count => 0
    },
    Data = put_sender_did(Data0, SenderDid),
    %% 预生成 TSID
    GenId2 = elib_tsid:generate(msg_store),
    Data3 = Data#{id => GenId2},
    {Sql2, Params2} = elib_pg_sql:insert(Tb, Data3),
    %% 【幂等性修复】捕获唯一约束错误
    case elib_pg:query(Sql2, Params2) of
        {ok, _} ->
            {ok, GenId2};
        {error, {error, {error, <<"23505">>, unique_violation, _, _}}} ->
            %% PostgreSQL 唯一约束错误：消息已存在（幂等性）
            {error, {unique_violation, MsgId}};
        {error, {error, Reason}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc 仅在设备标识非空时写该列；空值一律保持 NULL。
%% 与 message_ds:with_sender_device/2 的「缺字段时不补空值」同一语义。
-spec put_sender_did(map(), term()) -> map().
put_sender_did(Data, Did) when is_binary(Did), Did =/= <<>> ->
    Data#{sender_did => Did};
put_sender_did(Data, _Did) ->
    Data.

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
        Sql = <<
            "SELECT id, type, msg_id, payload, from_id, to_id, to_id_list, created_at, server_ts, retry_count, "
            "msg_type, action, e2ee, sender_did "
            "FROM ",
            Tb/binary,
            " WHERE processed_at IS NULL ",
            " AND available_at <= NOW() ",
            " ORDER BY created_at ASC ",
            " LIMIT $1 ",
            " FOR UPDATE SKIP LOCKED"
        >>,
        case elib_pg:query(Conn, Sql, [Limit]) of
            {ok, Rows} ->
                case Rows of
                    [] ->
                        {ok, []};
                    _ ->
                        Ids = [maps:get(<<"id">>, Row) || Row <- Rows],
                        LeaseSql =
                            <<"UPDATE ", Tb/binary,
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
    Sql =
        <<"UPDATE ", Tb/binary, " SET processed_at = NOW(), error_msg = NULL ",
            " WHERE msg_id = $1">>,
    elib_pg:execute(Sql, [MsgId]).

%% @doc 标记失败并设置下次重试时间
-spec mark_failed(binary(), binary(), binary(), pos_integer()) -> {ok, integer()} | {error, any()}.
mark_failed(Type, MsgId, ErrorMsg, DelaySeconds) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET retry_count = retry_count + 1, ", " error_msg = $3, ",
            " available_at = NOW() + INTERVAL '1 second' * $4 ",
            " WHERE type = $1 AND msg_id = $2 AND processed_at IS NULL">>,
    elib_pg:execute(Sql, [Type, MsgId, ErrorMsg, DelaySeconds]).

%% @doc 获取未处理的备份消息（用于启动时恢复）
-spec get_unstaged(integer()) -> {ok, list(map())} | {error, any()}.
get_unstaged(Limit) ->
    Tb = tablename(),
    Sql = <<
        "SELECT msg_type, msg_id, payload, from_id, to_id, to_id_list, created_at, server_ts "
        "FROM ",
        Tb/binary,
        " WHERE processed_at IS NULL ",
        "ORDER BY created_at ASC ",
        "LIMIT $1"
    >>,
    elib_pg:query(Sql, [Limit]).

%% @doc 清理已处理的备份消息（定时任务调用）
-spec delete_processed(integer()) -> {ok, integer()} | {error, any()}.
delete_processed(Seconds) ->
    Tb = tablename(),
    Sql =
        <<"DELETE FROM ", Tb/binary, " WHERE processed_at IS NOT NULL ",
            " AND processed_at < NOW() - INTERVAL '1 second' * $1">>,
    elib_pg:execute(Sql, [Seconds]).

%% @doc 获取备份表的统计信息
-spec get_staging_stats() -> {ok, map()} | {error, any()}.
get_staging_stats() ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", "COUNT(*) FILTER (WHERE processed_at IS NULL) as pending, ",
            "COUNT(*) FILTER (WHERE processed_at IS NOT NULL) as processed, ",
            "COUNT(*) FILTER (WHERE error_msg IS NOT NULL) as failed, ", "COUNT(*) as total ",
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
    case
        elib_pg:execute(
            <<"CREATE TABLE IF NOT EXISTS ", Tb/binary,
                " (\n"
                "            id BIGINT PRIMARY KEY,\n"
                "            type VARCHAR(10) NOT NULL,\n"
                "            msg_id VARCHAR(50) NOT NULL,\n"
                "            msg_type VARCHAR(50),\n"
                "            action VARCHAR(50),\n"
                "            e2ee JSONB,\n"
                %% A2-a：发送者设备标识（PFv3 context binding #6）。本 DDL 只覆盖
                %% 全新安装；存量部署由 priv/migrations/00000048 的 ALTER 补列——
                %% 两处必须同步，漏一处即新老部署 schema 分叉。
                "            sender_did VARCHAR(128),\n"
                "            payload JSONB NOT NULL,\n"
                "            from_id BIGINT NOT NULL,\n"
                "            to_id BIGINT,\n"
                "            to_id_list BIGINT[],\n"
                "            created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\n"
                "            server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),\n"
                "            retry_count INTEGER NOT NULL DEFAULT 0,\n"
                "            processed_at TIMESTAMPTZ,\n"
                "            available_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\n"
                "            error_msg TEXT,\n"
                "            CONSTRAINT msg_store_staging_type_msg_id_key UNIQUE (type, msg_id)\n"
                "        )">>,
            []
        )
    of
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
        <<"CREATE INDEX IF NOT EXISTS ", Tb/binary,
            "_processed_at_idx\n"
            "            ON ", Tb/binary, " (processed_at) WHERE processed_at IS NOT NULL">>,
        []
    ),
    %% 创建 available_at 索引（用于抢占待处理记录）
    _ = elib_pg:execute(
        <<"CREATE INDEX IF NOT EXISTS ", Tb/binary,
            "_available_at_idx\n"
            "            ON ", Tb/binary, " (available_at) WHERE processed_at IS NULL">>,
        []
    ),
    %% 创建 created_at 索引（用于按时间排序）
    _ = elib_pg:execute(
        <<"CREATE INDEX IF NOT EXISTS ", Tb/binary,
            "_created_at_idx\n"
            "            ON ", Tb/binary, " (created_at)">>,
        []
    ),
    ok.

%% @private
%% @doc 把传入的 payload 规范化为合法的 JSONB binary
%% 上游可能传：map（普通消息内容）、JSON 编码后的 binary（普通消息）、
%% 或裸 base64 密文 binary（E2EE 消息）。后者直接写入 JSONB 会触发
%% "invalid input syntax for type json"，需要包装为 JSON 字符串。
-spec msg_store_payload_to_jsonb(term()) -> binary().
msg_store_payload_to_jsonb(null) ->
    jsone:encode(null);
msg_store_payload_to_jsonb(Map) when is_map(Map) ->
    jsone:encode(Map, [native_utf8]);
msg_store_payload_to_jsonb(Bin) when is_binary(Bin) ->
    %% is_likely_json_binary 只看首字符，会把 "14bVk..." 这类以数字开头的
    %% 裸 E2EE 密文误判为 JSON 数字 → PG 22P02（真机实测，e2ee 消息 staging
    %% 全崩）。与 e2ee 字段同法：try-decode 真验证，不能解码则包装 JSON string。
    try jsone:decode(Bin, [{object_format, map}]) of
        _ -> Bin
    catch
        _:_ -> jsone:encode(Bin, [native_utf8])
    end;
msg_store_payload_to_jsonb(Other) ->
    jsone:encode(Other).

%% @private
%% @doc 把传入的 E2EE 元数据规范化为合法的 JSONB binary 或 null
%% 上游可能传：map（标准 E2EE 元数据）、JSON binary、空 binary、null、
%% 或者裸字符串（如某些上游路径只取了密文片段）。裸字符串必须包装为
%% JSON 字符串，否则触发 "invalid input syntax for type json"。
%% 与 payload 相同：try-decode 真验证，行为保持一致。
-spec msg_store_e2ee_to_jsonb(term()) -> binary() | null.
msg_store_e2ee_to_jsonb(null) ->
    null;
msg_store_e2ee_to_jsonb(<<>>) ->
    null;
msg_store_e2ee_to_jsonb(Map) when is_map(Map) ->
    jsone:encode(Map, [native_utf8]);
msg_store_e2ee_to_jsonb(Bin) when is_binary(Bin) ->
    %% is_likely_json_binary 只看首字符，会把 "4QuejM" 这类裸 base62 误判为 JSON 数字，
    %% 因此 e2ee 这里改用 try-decode 真验证：能解码才原样传，否则按 JSON string 包装。
    try jsone:decode(Bin, [{object_format, map}]) of
        _ -> Bin
    catch
        _:_ -> jsone:encode(Bin, [native_utf8])
    end;
msg_store_e2ee_to_jsonb(_) ->
    null.
