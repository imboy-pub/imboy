-module(msg_c2g_repo).
%%%
% msg_c2g_repo 是 msg_c2g repository 缩写
% 用户到群组离线消息数据仓库层，提供C2G消息的基础数据库操作
%%%

-include("chat.hrl").
-include("log.hrl").

-export([tablename/0]).
-export([write_msg/8]).
-export([write_msg/9]).
-export([write_msg_with_reply/11]).
-export([list_by_ids/2]).
-export([find_msg_by_id/1]).
-export([find_by_reply_to_msg_id/1]).
-export([delete_msg/1]).
-export([delete_msg/2]).
-export([count_read/1]).
-export([update_pinned/3]).
-export([update_payload_by_msg_id/2]).
-export([write_msg_with_mentions/9]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取C2G消息表的表名
%% @return 返回C2G消息表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_c2g">>).

% msg_c2g_repo:write_msg(1707686743435, <<"msg_id_1">>,  <<"{\"a\":1}">>,  1, [2,3,107], 7, <<"text">>, <<>>).
% msg_c2g_repo:write_msg(<<"2026-01-01 05:50:28.465444+00:00">>, <<"msg_id_1">>, <<"{\"a\":1}">>, 1, [2,3,107], 7, <<"image">>, <<"{\"key\":\"...\"}">>).
%%====================================================================
%% @doc 写入群离线消息及时间线表
%% 支持 CreatedAt: binary() | integer() (毫秒时间戳)
%%====================================================================
-spec write_msg(
        binary() | integer(), %% CreatedAt
        binary(),             %% MsgId
        binary(),             %% Payload
        integer(),            %% FromId
        [integer()],          %% ToUids
        integer(),            %% Gid
        binary(),             %% MsgType
        map() | null       %% E2EE (可选)
      ) -> ok.
write_msg(CreatedAtRaw, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE) ->
    write_msg(CreatedAtRaw, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE, null).

%% @doc 写入群离线消息（支持 expire_at 自毁时间）
-spec write_msg(
        binary() | integer(), binary(), binary(), integer(),
        [integer()], integer(), binary(), map() | null, binary() | null
      ) -> ok.
write_msg(CreatedAtRaw, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE, ExpireAt) ->
    %% ---------- 统一转换 CreatedAt ----------
    CreatedAt = elib_dt:to_rfc3339(CreatedAtRaw),

    TbMsg = tablename(),           %% 群离线消息表
    TbTimeline = msg_c2g_timeline_repo:tablename(), %% 群消息时间线表

    elib_pg:with_tx(fun(Conn) ->
        %% ---------- 插入群离线消息 ----------
        MsgData = #{
            payload => Payload,
            to_id => Gid,
            from_id => FromId,
            created_at => CreatedAt,
            server_ts => CreatedAt,
            topic_id => 0,
            msg_id => MsgId,
            msg_type => MsgType,
            e2ee => case E2EE of
                <<>> -> null;
                null -> null;
                Map when is_map(Map) -> jsone:encode(Map, [native_utf8]);
                Bin when is_binary(Bin) -> Bin;
                _ -> null
            end
        },
        %% 仅当 ExpireAt 非 null 时添加字段
        MsgData2 = case ExpireAt of
            null -> MsgData;
            _ -> MsgData#{expire_at => ExpireAt}
        end,
        case elib_pg:insert(Conn, TbMsg, MsgData2, <<>>) of
            {ok, _} -> ok;
            {error, InsertReason} ->
                ?ERROR_LOG({msg_c2g_insert_failed, MsgId, InsertReason}),
                error({msg_c2g_insert_failed, InsertReason})
        end,

        %% ---------- 批量插入时间线表 ----------
        Vals = [ [MsgId, ToId, Gid, CreatedAt] || ToId <- ToUids ],
        {SqlTimeline0, ParamsTimeline} =
            elib_pg_sql:insert_batch(TbTimeline, [msg_id, to_uid, to_gid, created_at], Vals),
        SqlTimeline = iolist_to_binary([
            SqlTimeline0,
            <<" ON CONFLICT (to_uid, msg_id) DO NOTHING">>
        ]),
        {ok, _} = elib_pg:execute(Conn, SqlTimeline, ParamsTimeline),
        ok
    end).


%% @doc 根据消息ID列表查询群离线消息
%% @param Ids 消息ID列表
%% @param Column 要查询的列名
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @example msg_c2g_repo:list_by_ids([<<"msg1">>, <<"msg2">>], <<"payload">>).
-spec list_by_ids(list(binary()), binary()) -> {ok, list(map())} | {error, any()}.
list_by_ids([], _Column) ->
    {ok, []};
list_by_ids(Ids, Column) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    Placeholders = iolist_to_binary(lists:join(<<",">>,
        [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(Ids))])),
    Where = <<" WHERE msg_id IN (", Placeholders/binary, ")">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary, " ORDER BY created_at ASC">>,
    % 将Ids转换为参数列表
    Params = [Id || Id <- Ids],
    % ?DEBUG_LOG(Sql),
    elib_pg:query(Sql, Params).


%% @doc 根据消息ID查找群聊消息
%% @param Id 消息ID
%% @return {ok, Row} 查询成功返回map | {error, Reason} 查询失败
%% @example msg_c2g_repo:find_msg_by_id(<<"msg_id">>).
-spec find_msg_by_id(binary()) -> {ok, map()} | {error, any()}.
find_msg_by_id(Id) ->
    Tb = tablename(),
    Column = <<"id, payload, created_at, from_id, to_id, msg_id, msg_type, e2ee">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE msg_id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [Row]} when is_map(Row) -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.


%% @doc 删除群离线消息（根据消息ID）
%% @param Id 消息ID
%% @return {ok, Count} 删除成功 | {error, Reason} 删除失败
%% @example msg_c2g_repo:delete_msg(<<"msg_id">>).
-spec delete_msg(binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete_msg(Id) ->
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, [Id]).


%% @doc 根据WHERE条件删除群离线消息
%% @param Where SQL WHERE子句
%% @param Params 参数列表
%% @return {ok, Count} 删除成功 | {error, Reason} 删除失败
-spec delete_msg(binary(), list()) -> {ok, non_neg_integer()} | {error, term()}.
delete_msg(Where, Params) when is_list(Params) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    elib_pg:execute(Sql, Params).


%% @doc 统计已读消息人数
%% 统计指定消息的已读用户数量（去重）
%% @param MsgId 消息ID
%% @return 已读人数
%% @end
-spec count_read(binary()) -> integer().
count_read(MsgId) ->
    Sql = <<"SELECT COUNT(DISTINCT to_uid) FROM msg_read WHERE msg_id = $1">>,
    case elib_pg:one(Sql, [MsgId]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 更新群消息置顶状态
%% @param MsgId 消息ID
%% @param ToUid 接收者用户ID
%% @param Pinned 置顶状态 (true/false)
%% @return {ok, AffectedRows} | {error, Reason}
-spec update_pinned(binary(), integer(), boolean()) -> {ok, non_neg_integer()} | {error, term()}.
update_pinned(MsgId, ToUid, Pinned) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
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
    Sql = <<"UPDATE ", Tb/binary,
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

%% @doc 写入带引用回复信息的C2G离线消息
%% 注意：当前数据库表不支持 reply_to_msg_id 等字段，此函数忽略回复信息
%% @param CreatedAt 消息创建时间（RFC3339 binary）
%% @param Id 消息唯一ID
%% @param Payload 消息载荷（JSON binary）
%% @param FromId 发送者用户ID（integer，对应 bigint 列）
%% @param ToUids 接收者用户ID列表
%% @param Gid 群组ID
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON map，可选）
%% @param _ReplyToMsgId 被引用回复的消息ID（暂不支持）
%% @param _ReplyToFromId 被引用消息的发送者ID（暂不支持）
%% @param _ReplySnippet 被引用消息的摘要（暂不支持）
%% @return ok | {error, Reason}
-spec write_msg_with_reply(binary(), binary(), binary(), integer(), [integer()], integer(),
                           binary(), map() | null, binary(), integer(), binary()) -> ok | {error, term()}.
write_msg_with_reply(CreatedAt, Id, Payload, FromId, ToUids, Gid, MsgType, E2EE,
                     _ReplyToMsgId, _ReplyToFromId, _ReplySnippet) ->
    % 当前数据库表不支持 reply_to_msg_id 等字段
    % 直接调用 write_msg/9 忽略回复信息
    write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid, MsgType, E2EE).

%% @doc 根据被引用消息ID查找所有回复消息
%% 注意：当前数据库表不支持 reply_to_msg_id 字段，此函数返回空列表
%% @param _ReplyToMsgId 被引用的消息ID
%% @return {ok, []}
-spec find_by_reply_to_msg_id(binary()) -> {ok, list(map())} | {error, term()}.
find_by_reply_to_msg_id(_ReplyToMsgId) ->
    % 当前数据库表不支持 reply_to_msg_id 字段
    {ok, []}.

%% @doc 写入带@提及信息的群离线消息
%% @param CreatedAtRaw 消息创建时间（binary RFC3339 或 integer 毫秒时间戳）
%% @param MsgId 消息ID
%% @param Payload 消息内容
%% @param FromId 发送者用户ID
%% @param ToUids 接收者用户ID列表
%% @param Gid 群组ID
%% @param MsgType 消息类型
%% @param E2EE 端到端加密信息（可选）
%% @param Mentions 被@的用户ID列表（integer）
%% @return ok
-spec write_msg_with_mentions(
        binary() | integer(), binary(), binary(), integer(), [integer()],
        integer(), binary(), map() | null, [integer()]
      ) -> ok.
write_msg_with_mentions(CreatedAtRaw, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE, Mentions) ->
    %% ---------- 统一转换 CreatedAt ----------
    CreatedAt = elib_dt:to_rfc3339(CreatedAtRaw),

    TbMsg = tablename(),           %% 群离线消息表
    TbTimeline = msg_c2g_timeline_repo:tablename(), %% 群消息时间线表

    elib_pg:with_tx(fun(Conn) ->
        %% ---------- 插入群离线消息（带@信息）----------
        case elib_pg:insert(Conn, TbMsg, #{
            payload => Payload,
            to_id => Gid,
            from_id => FromId,
            created_at => CreatedAt,
            server_ts => CreatedAt,
            topic_id => 0,
            msg_id => MsgId,
            msg_type => MsgType,
            e2ee => case E2EE of
                <<>> -> null;
                null -> null;
                Map when is_map(Map) -> jsone:encode(Map, [native_utf8]);
                Bin when is_binary(Bin) -> Bin;
                _ -> null
            end,
            mentions => case Mentions of
                [] -> null;
                _ -> jsone:encode(Mentions, [native_utf8])
            end
        }, <<>>) of
            {ok, _} -> ok;
            {error, InsertReason} ->
                ?ERROR_LOG({msg_c2g_mention_insert_failed, MsgId, InsertReason}),
                error({msg_c2g_insert_failed, InsertReason})
        end,

        %% ---------- 批量插入时间线表 ----------
        Vals = [ [MsgId, ToId, Gid, CreatedAt] || ToId <- ToUids ],
        {SqlTimeline0, ParamsTimeline} =
            elib_pg_sql:insert_batch(TbTimeline, [msg_id, to_uid, to_gid, created_at], Vals),
        SqlTimeline = iolist_to_binary([
            SqlTimeline0,
            <<" ON CONFLICT (to_uid, msg_id) DO NOTHING">>
        ]),
        {ok, _} = elib_pg:execute(Conn, SqlTimeline, ParamsTimeline),
        ok
    end).
