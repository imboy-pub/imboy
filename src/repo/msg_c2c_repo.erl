-module(msg_c2c_repo).
%%%
% msg_c2c_repo 是 msg_c2c repository 缩写
% 用户到用户离线消息数据仓库层，提供C2C消息的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([read_msg/4]).
-export([find_msg_by_id/1]).
-export([write_msg/8]).
-export([write_msg/9]).
-export([write_msg_with_reply/11]).
-export([delete_msg/1]).
-export([delete_msg/2]).
-export([count_by_to_id/1]).
-export([delete_by_to_id/1]).
-export([delete_by_msg_id_and_to_id/2]).
-export([delete_by_msg_ids_and_to_id/2]).
-export([delete_overflow_msg/2]).
-export([update_pinned/3]).
-export([update_payload_by_msg_id/2]).
-export([find_by_reply_to_msg_id/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取C2C消息表的表名
%% @return 返回C2C消息表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_c2c">>).

%% @doc 根据消息ID查找单条消息
%% @param MsgId 消息唯一ID
%% @return {ok, MsgMap} | {error, Reason}
-spec find_msg_by_id(binary()) -> {ok, map()} | {error, any()}.
find_msg_by_id(MsgId) ->
    Tb = tablename(),
    Sql = <<
        "SELECT from_id, to_id, created_at, payload "
        "FROM ",
        Tb/binary,
        " WHERE msg_id = $1 LIMIT 1"
    >>,
    case elib_pg:query(Sql, [MsgId]) of
        {ok, [Msg]} -> {ok, Msg};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% read_msg/3 已从公共 API 中移除（安全原因：原始 Where 字符串存在 SQL 注入风险）。
%% 所有调用方应使用 read_msg/4 并通过 Params 传递参数化条件。

%% @doc 读取C2C离线消息（带参数）
%% @param Where SQL WHERE子句条件
%% @param Column 要查询的列名
%% @param Limit 查询结果数量限制
%% @param Params 查询参数列表
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @details 使用i_ToId索引，按ID升序排序，支持参数化查询
-spec read_msg(binary(), binary(), integer(), list()) -> {ok, list(map())} | {error, any()}.
read_msg(Where, Column, Limit, Params) ->
    Tb = tablename(),
    % 计算 LIMIT 参数的编号（基于 WHERE 中的参数数量 + 1）
    LimitParamIndex = length(Params) + 1,
    LimitPlaceholder = iolist_to_binary(["$", integer_to_binary(LimitParamIndex)]),

    % 使用安全的参数化查询，避免SQL注入
    % WHERE to_id = $1 ORDER BY id ASC LIMIT $2
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary,
            " ORDER BY id ASC LIMIT ", LimitPlaceholder/binary>>,
    % 将参数列表合并：先传递WHERE条件的参数，最后是LIMIT参数
    elib_pg:query(Sql, Params ++ [Limit]).

%% @doc 写入C2C离线消息
%% @param CreatedAt 消息创建时间（RFC3339 binary）
%% @param Id 消息唯一ID
%% @param Payload 消息载荷（JSON binary）
%% @param FromId 发送者用户ID（integer，对应 bigint 列）
%% @param ToId 接收者用户ID（integer，对应 bigint 列）
%% @param ServerTS 服务器时间戳（RFC3339 binary）
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON map，可选）
%% @return {ok, Result} | {error, Reason}
%% @example msg_c2c_repo:write_msg(elib_dt:now(microsecond), <<"ciik13p2888j8hhi437g">>, <<"{\"text\":\"hello\"}">>, 1, 2, elib_dt:now(microsecond), <<"text">>, <<"{\"key\":\"...\"}">>).
-spec write_msg(
    binary(), binary(), binary(), integer(), integer(), binary(), binary(), map() | null
) -> ok | {error, term()}.
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS, MsgType, E2EE) ->
    %% from_id 和 to_id 是 bigint 类型，必须传入 integer
    Tb = tablename(),
    E2EEValue =
        case E2EE of
            null -> null;
            <<>> -> null;
            % map 需要 encode
            Map when is_map(Map) -> jsone:encode(Map, [native_utf8]);
            % 已经是 JSON binary（避免双重编码）
            Bin when is_binary(Bin) -> Bin;
            _ -> null
        end,
    %% 使用 ON CONFLICT DO NOTHING 避免重试时的唯一约束冲突
    %% 唯一约束: (msg_id, created_at)
    GenId = elib_tsid:generate(msg_c2c),
    Sql = [
        <<"INSERT INTO ">>,
        Tb,
        <<" (id, payload, from_id, to_id, created_at, server_ts, msg_id, msg_type, e2ee)">>,
        <<" VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)">>,
        <<" ON CONFLICT (msg_id, created_at) DO NOTHING">>
    ],
    case
        elib_pg:query(Sql, [
            GenId, Payload, FromId, ToId, CreatedAt, ServerTS, Id, MsgType, E2EEValue
        ])
    of
        {ok, _Rows} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 写入C2C消息（支持 expire_at 自毁时间）
-spec write_msg(
    binary(),
    binary(),
    binary(),
    integer(),
    integer(),
    binary(),
    binary(),
    map() | null,
    binary() | null
) ->
    ok | {error, term()}.
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS, MsgType, E2EE, null) ->
    write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS, MsgType, E2EE);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS, MsgType, E2EE, ExpireAt) ->
    Tb = tablename(),
    E2EEValue =
        case E2EE of
            null -> null;
            <<>> -> null;
            Map when is_map(Map) -> jsone:encode(Map, [native_utf8]);
            Bin when is_binary(Bin) -> Bin;
            _ -> null
        end,
    GenId = elib_tsid:generate(msg_c2c),
    Sql = [
        <<"INSERT INTO ">>,
        Tb,
        <<" (id, payload, from_id, to_id, created_at, server_ts, msg_id, msg_type, e2ee, expire_at)">>,
        <<" VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)">>,
        <<" ON CONFLICT (msg_id, created_at) DO NOTHING">>
    ],
    case
        elib_pg:query(Sql, [
            GenId, Payload, FromId, ToId, CreatedAt, ServerTS, Id, MsgType, E2EEValue, ExpireAt
        ])
    of
        {ok, _Rows} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 删除C2C离线消息（根据主键ID或消息ID）
%% @param Id 消息主键ID或消息唯一ID
%% @return {ok, Count} | {error, Reason}
-spec delete_msg(integer() | binary()) -> {ok, non_neg_integer()} | {error, any()}.
delete_msg(Id) when is_integer(Id) ->
    Where = <<"WHERE id = $1">>,
    delete_msg(Where, [Id]);
delete_msg(Id) ->
    % use index uk_c2c_MsgId
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, [Id]).

%% @doc 根据WHERE条件删除C2C离线消息
%% @param Where SQL WHERE子句
%% @param Params 参数列表
%% @return {ok, Count} | {error, Reason}
-spec delete_msg(binary(), list()) -> {ok, non_neg_integer()} | {error, any()}.
delete_msg(Where, Params) when is_list(Params) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    % ?DEBUG_LOG(['delete_msg', Params, Sql]),
    case elib_pg:execute(Sql, Params) of
        {ok, Count} -> {ok, Count};
        {ok, Count, _} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 统计指定用户的C2C离线消息数量
%% @param ToUid 接收者用户ID
%% @return 返回消息数量，错误时返回0
%% @example msg_c2c_repo:count_by_to_id(1).
%% @details 使用i_c2c_ToId索引
-spec count_by_to_id(integer()) -> integer().
count_by_to_id(ToUid) ->
    % use index i_c2c_ToId
    % 使用安全的参数化查询，避免SQL注入
    elib_pg:pluck_value(tablename(), <<"count(*) as count">>, #{to_id => ToUid}, #{}, 0).

%% @doc 删除超出限制数量的C2C离线消息
%% @param ToUid 接收者用户ID
%% @param Limit 保留的消息数量限制
%% @return ok | {error, Reason}
%% @details 使用单条 SQL 批量删除最旧消息，避免 N+1 问题
-spec delete_overflow_msg(integer(), integer()) -> ok | {error, any()}.
delete_overflow_msg(ToUid, Limit) ->
    Tb = tablename(),
    Sql = <<
        "DELETE FROM ",
        Tb/binary,
        " WHERE ctid IN ("
        "   SELECT ctid FROM ",
        Tb/binary,
        "   WHERE to_id = $1 ORDER BY id ASC LIMIT $2"
        " )"
    >>,
    case elib_pg:execute(Sql, [ToUid, Limit]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 删除用户的所有C2C离线消息
%% @param ToUid 接收者用户ID
%% @return {ok, Count} | {error, Reason}
-spec delete_by_to_id(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_to_id(ToUid) ->
    Where = <<"WHERE to_id = $1">>,
    delete_msg(Where, [ToUid]).

%% @doc 根据消息ID和接收者ID删除特定消息
%% @param MsgId 消息唯一ID
%% @param ToUid 接收者用户ID
%% @return {ok, Count} | {error, Reason}
-spec delete_by_msg_id_and_to_id(binary(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_msg_id_and_to_id(MsgId, ToUid) ->
    Where = <<"WHERE msg_id = $1 AND to_id = $2">>,
    delete_msg(Where, [MsgId, ToUid]).

%% @doc 批量删除多个消息ID（使用安全的参数化查询）
%% @param MsgIds 消息ID列表
%% @param ToUid 接收者用户ID
%% @return {ok, Count} | {error, Reason}
-spec delete_by_msg_ids_and_to_id(list(binary()), integer()) -> {ok, integer()} | {error, any()}.
delete_by_msg_ids_and_to_id(MsgIds, ToUid) when is_list(MsgIds), length(MsgIds) > 0 ->
    Tb = tablename(),
    % 手动构建参数化占位符和参数
    Placeholders = iolist_to_binary(
        lists:join(
            <<",">>,
            [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(MsgIds))]
        )
    ),
    Sql =
        <<"DELETE FROM ", Tb/binary, " WHERE msg_id IN (", Placeholders/binary, ") AND to_id = $",
            (integer_to_binary(length(MsgIds) + 1))/binary>>,
    elib_pg:execute(Sql, MsgIds ++ [ToUid]);
delete_by_msg_ids_and_to_id([], _ToUid) ->
    {ok, 0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 更新消息置顶状态
%% @param MsgId 消息ID
%% @param ToUid 接收者用户ID
%% @param Pinned 置顶状态 (true/false)
%% @return {ok, AffectedRows} | {error, Reason}
-spec update_pinned(binary(), integer(), boolean()) -> {ok, non_neg_integer()} | {error, term()}.
update_pinned(MsgId, ToUid, Pinned) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET pinned = $1, updated_at = CURRENT_TIMESTAMP "
            " WHERE msg_id = $2 AND to_id = $3">>,
    case elib_pg:execute(Sql, [Pinned, MsgId, ToUid]) of
        {ok, AffectedRows} ->
            {ok, AffectedRows};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 根据消息ID更新消息 payload（用于编辑/撤回 ack 持久化）
%% @param MsgId 原消息ID
%% @param PayloadJson JSON binary payload
%% @return {ok, AffectedRows} | {error, Reason}
-spec update_payload_by_msg_id(binary(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
update_payload_by_msg_id(MsgId, PayloadJson) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET payload = $1, server_ts = CURRENT_TIMESTAMP "
            " WHERE msg_id = $2">>,
    case elib_pg:execute(Sql, [PayloadJson, MsgId]) of
        {ok, AffectedRows} ->
            {ok, AffectedRows};
        {ok, AffectedRows, _} ->
            {ok, AffectedRows};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 写入带引用回复信息的C2C离线消息
%% 数据库已通过 00000054_msg_reply.sql 迁移添加 reply_to_msg_id(varchar(40)),
%% reply_to_from_id(bigint), reply_snippet(text) 三个字段
%% @param CreatedAt 消息创建时间（RFC3339 binary）
%% @param Id 消息唯一ID
%% @param Payload 消息载荷（JSON binary）
%% @param FromId 发送者用户ID（integer，对应 bigint 列）
%% @param ToId 接收者用户ID（integer，对应 bigint 列）
%% @param ServerTS 服务器时间戳（RFC3339 binary）
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON map，可选）
%% @param ReplyToMsgId 被引用回复的消息ID
%% @param ReplyToFromId 被引用消息的发送者ID
%% @param ReplySnippet 被引用消息的摘要（前50字符）
%% @return ok | {error, Reason}
-spec write_msg_with_reply(
    binary(),
    binary(),
    binary(),
    integer(),
    integer(),
    binary(),
    binary(),
    map() | null,
    binary() | null,
    integer() | null,
    binary() | null
) -> ok | {error, term()}.
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    FromId,
    ToId,
    ServerTS,
    MsgType,
    E2EE,
    null,
    _ReplyToFromId,
    _ReplySnippet
) ->
    %% 无引用回复信息，退化为普通写入
    write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS, MsgType, E2EE);
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    FromId,
    ToId,
    ServerTS,
    MsgType,
    E2EE,
    <<>>,
    _ReplyToFromId,
    _ReplySnippet
) ->
    %% 空引用回复ID，退化为普通写入
    write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS, MsgType, E2EE);
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    FromId,
    ToId,
    ServerTS,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet
) ->
    Tb = tablename(),
    E2EEValue =
        case E2EE of
            null -> null;
            <<>> -> null;
            Map when is_map(Map) -> jsone:encode(Map, [native_utf8]);
            Bin when is_binary(Bin) -> Bin;
            _ -> null
        end,
    ReplyToFromIdVal =
        case ReplyToFromId of
            null -> null;
            0 -> null;
            V when is_integer(V) -> V;
            _ -> null
        end,
    ReplySnippetVal =
        case ReplySnippet of
            null -> null;
            <<>> -> null;
            S when is_binary(S) -> S;
            _ -> null
        end,
    GenId = elib_tsid:generate(msg_c2c),
    Sql = [
        <<"INSERT INTO ">>,
        Tb,
        <<
            " (id, payload, from_id, to_id, created_at, server_ts, msg_id, msg_type, e2ee,"
            " reply_to_msg_id, reply_to_from_id, reply_snippet)"
        >>,
        <<" VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)">>,
        <<" ON CONFLICT (msg_id, created_at) DO NOTHING">>
    ],
    case
        elib_pg:query(Sql, [
            GenId,
            Payload,
            FromId,
            ToId,
            CreatedAt,
            ServerTS,
            Id,
            MsgType,
            E2EEValue,
            ReplyToMsgId,
            ReplyToFromIdVal,
            ReplySnippetVal
        ])
    of
        {ok, _Rows} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据被引用消息ID查找所有回复消息
%% 使用 idx_msg_c2c_reply 索引（00000054_msg_reply.sql 中创建）
%% @param ReplyToMsgId 被引用的消息ID
%% @return {ok, list(map())} | {error, Reason}
-spec find_by_reply_to_msg_id(binary()) -> {ok, list(map())} | {error, term()}.
find_by_reply_to_msg_id(ReplyToMsgId) when ReplyToMsgId =:= <<>>; ReplyToMsgId =:= null ->
    {ok, []};
find_by_reply_to_msg_id(ReplyToMsgId) ->
    Tb = tablename(),
    Sql = <<
        "SELECT msg_id, from_id, to_id, payload, created_at, reply_to_msg_id, reply_to_from_id, reply_snippet"
        " FROM ",
        Tb/binary,
        " WHERE reply_to_msg_id = $1 ORDER BY id ASC"
    >>,
    elib_pg:query(Sql, [ReplyToMsgId]).
