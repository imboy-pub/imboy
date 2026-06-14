-module(msg_c2g_ds).
%%%
% msg_c2g_ds 是 msg_c2g domain service 缩写
%%%
-export([write_msg/6]).
-export([write_msg/8]).
-export([write_msg_with_reply/11]).
-export([revoke_offline_msg/9]).
-export([edit_offline_msg/6]).
-export([read_msg/1]).
-export([read_msg/3]).
-export([find_msg_by_id/1]).
-export([delete_msg/1]).
-export([timeline_find_by_msg_id/1]).
-export([update_pinned/3]).
-export([count_read/1]).
-export([update_payload_by_msg_id/2]).
-export([timeline_delete_by_msg_ids_and_to_id/2]).
-export([count_unread_timeline_since/2]).
-export([set_expire_at/2]).
-export([delete_expired/2]).
-export([page/3]).

-include("log.hrl").

%% @doc 存储群组消息
%%
%% 将消息存储到群组消息表中，先检查消息是否已存在，避免重复存储
%%
%% @param CreatedAt 消息创建时间戳（integer 毫秒或 binary RFC3339）
%% @param Id 消息ID
%% @param Payload 消息内容（二进制格式）
%% @param FromId 发送方用户ID
%% @param ToUids 接收消息的用户ID列表
%% @param Gid 群组ID
%% @returns any() 数据库操作结果
-spec write_msg
    (integer() | binary(), binary(), binary(), integer(), list(), integer()) -> ok;
    (integer() | binary(), binary(), integer(), integer(), map(), binary()) -> ok.
write_msg(CreatedAtRaw, Id, FromId, Gid, PayloadMap, _PayloadMd5) when
    is_integer(FromId), is_integer(Gid), is_map(PayloadMap)
->
    PayloadBin = jsone:encode(PayloadMap, [native_utf8]),
    write_msg(CreatedAtRaw, Id, PayloadBin, FromId, [FromId], Gid);
% msg_c2g_ds:write_msg(1707686743435, <<"msg_id_1">>,  <<"{}">>,  1, [2,3,4], 1).
% msg_c2g_ds:write_msg(1707686743435, <<"msg_id_1">>,  <<"{\"a\":1}">>,  1, [2,3,107], 7).
write_msg(CreatedAtRaw, Id, Payload, FromId, ToUids, Gid) ->
    CreatedAt = elib_dt:to_rfc3339(CreatedAtRaw),

    %% 从 Payload 中提取 msg_type 和 e2ee 字段
    PayloadMap =
        try jsone:decode(Payload) of
            Map when is_map(Map) -> Map;
            _ -> #{}
        catch
            _:_ -> #{}
        end,

    MsgType = maps:get(<<"msg_type">>, PayloadMap, <<>>),
    % map() | null
    E2EE = maps:get(<<"e2ee">>, PayloadMap, null),

    % 使用安全的参数化查询，避免SQL注入
    case
        elib_pg:pluck_value(
            msg_c2g_repo:tablename(),
            <<"count(*)">>,
            #{<<"msg_id">> => Id},
            #{},
            0
        )
    of
        0 ->
            msg_c2g_repo:write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid, MsgType, E2EE);
        _ ->
            ok
    end.

%% @doc 存储群组消息（v2.0 格式，支持 msg_type 和 e2ee）
%%
%% 将消息存储到群组消息表中，包含消息类型和端到端加密信息
%%
%% @param CreatedAt 消息创建时间戳（integer 毫秒或 binary RFC3339）
%% @param Id 消息ID
%% @param Payload 消息内容（二进制格式，不包含 msg_type/e2ee）
%% @param FromId 发送方用户ID
%% @param ToUids 接收消息的用户ID列表
%% @param Gid 群组ID
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON binary，可选）
%% @returns any() 数据库操作结果
-spec write_msg(
    binary() | integer(),
    binary(),
    binary(),
    integer(),
    list(),
    integer(),
    binary(),
    binary() | null
) -> any().
write_msg(CreatedAtRaw, Id, Payload, FromId, ToUids, Gid, MsgType, E2EE) ->
    CreatedAt = elib_dt:to_rfc3339(CreatedAtRaw),

    % 使用安全的参数化查询，避免SQL注入
    case
        elib_pg:pluck_value(
            msg_c2g_repo:tablename(),
            <<"count(*)">>,
            #{<<"msg_id">> => Id},
            #{},
            0
        )
    of
        0 ->
            msg_c2g_repo:write_msg(CreatedAt, Id, Payload, FromId, ToUids, Gid, MsgType, E2EE);
        _ ->
            ok
    end.

%% @doc 撤回离线群组消息（v2.0 格式，支持 msg_type 和 action）
%%
%% 处理群组消息的撤回操作，支持显式传递 msg_type 和 action 参数
%%
%% @param Payload 撤回消息的新内容（不包含 msg_type/action）
%% @param NowTs 当前时间戳
%% @param MsgId 原消息ID
%% @param FromId 发送方用户ID
%% @param MemberUids 群组成员用户ID列表
%% @param Gid 群组ID
%% @param MsgType 消息类型（custom, text 等）
%% @param Action 操作类型（message_revoke_ack 等）
%% @param E2EE 端到端加密信息（可选）
%% @returns ok 表示操作成功
-spec revoke_offline_msg(
    binary(),
    binary() | integer(),
    binary(),
    integer(),
    list(),
    integer(),
    binary(),
    binary(),
    binary()
) -> ok.
revoke_offline_msg(Payload, NowTs, MsgId, FromId, MemberUids, Gid, MsgType, _Action, E2EE) ->
    % 存储消息（v2.0: 使用 write_msg/8 显式传递参数）
    write_msg(NowTs, MsgId, Payload, FromId, MemberUids, Gid, MsgType, E2EE),
    % 使用 elib_pg:update/4 + {raw, ...} 安全地更新 payload
    case
        elib_pg:update(
            msg_c2g_repo:tablename(),
            #{payload => Payload},
            <<"msg_id = $1">>,
            [MsgId]
        )
    of
        {ok, _} -> ok;
        {error, Reason1} -> ?ERROR_LOG([msg_c2g_payload_update_failed, MsgId, Reason1])
    end,
    % 已确认的消息需要重新确认
    % 使用安全的参数化查询，避免SQL注入
    case
        elib_pg:update(
            msg_c2g_timeline_repo:tablename(),
            #{client_ack => false},
            <<"msg_id = $1">>,
            [MsgId]
        )
    of
        {ok, _} -> ok;
        {error, Reason2} -> ?ERROR_LOG([msg_c2g_ack_update_failed, MsgId, Reason2])
    end,
    ok.

%% @doc 编辑离线消息
%% @param Payload 消息内容
%% @param NowTs 时间戳
%% @param MsgId 消息ID
%% @param FromId 发送者ID
%% @param MemberUids 成员ID列表
%% @param Gid 群组ID
%% @returns ok 表示操作成功
-spec edit_offline_msg(binary(), binary() | integer(), binary(), integer(), list(), integer()) ->
    ok.
edit_offline_msg(Payload, _NowTs, MsgId, FromId, _MemberUids, _Gid) ->
    % 使用 elib_pg:update/4 + {raw, ...} 安全地更新 payload
    case
        elib_pg:update(
            msg_c2g_repo:tablename(),
            #{payload => Payload},
            <<"msg_id = $1 AND from_id = $2">>,
            [MsgId, FromId]
        )
    of
        {ok, _} -> ok;
        {error, Reason1} -> ?ERROR_LOG([msg_c2g_edit_payload_update_failed, MsgId, FromId, Reason1])
    end,
    % 已确认的消息需要重新确认
    % 使用安全的参数化查询，避免SQL注入
    case
        elib_pg:update(
            msg_c2g_timeline_repo:tablename(),
            #{client_ack => false},
            <<"msg_id = $1">>,
            [MsgId]
        )
    of
        {ok, _} -> ok;
        {error, Reason2} -> ?ERROR_LOG([msg_c2g_edit_ack_update_failed, MsgId, Reason2])
    end,
    ok.

%% @doc 读取群消息
%%
%% 兼容两种历史入口：
%% 1. `read_msg(MsgId)` 读取单条群消息；
%% 2. `read_msg(ToUid)` 读取指定用户的离线群消息。
-spec read_msg
    (integer()) -> [map()];
    (binary()) -> {ok, map()} | {error, any()}.
read_msg(MsgId) when is_binary(MsgId) ->
    case find_msg_by_id(MsgId) of
        {error, not_found} ->
            {ok, #{}};
        Result ->
            Result
    end;
% msg_c2g_ds:read_msg(3).
read_msg(ToUid) when is_integer(ToUid) ->
    read_msg(ToUid, 1000, undefined).

%% @doc 读取群消息
%%
%% 兼容两种历史入口：
%% 1. `read_msg(ToUid, Limit, LastMsgAt)` 读取离线群消息；
%% 2. `read_msg(GroupId, Columns, Limit)` 按群读取历史消息列。
-spec read_msg
    (integer(), integer(), undefined | integer() | binary()) -> [map()];
    (integer(), [binary()], integer()) -> {ok, list(map())} | {error, any()}.
read_msg(GroupId, Columns, Limit) when
    is_integer(GroupId), is_list(Columns), is_integer(Limit)
->
    Tb = msg_c2g_repo:tablename(),
    ColumnBin = legacy_group_columns(Columns),
    Sql =
        <<"SELECT ", ColumnBin/binary, " FROM ", Tb/binary,
            " WHERE to_id = $1 ORDER BY created_at ASC LIMIT $2">>,
    elib_pg:query(Sql, [GroupId, Limit]);
% msg_c2g_ds:read_msg(3, 1000, 1707686743435).
read_msg(ToUid, Limit, undefined) ->
    % 获取用户未确认的消息
    Column = <<"msg_id, created_at">>,
    {ok, Rows} = msg_c2g_timeline_repo:list_by_uid(ToUid, Column, Limit),
    MsgIds = [MsgId || #{<<"msg_id">> := MsgId} <- Rows],
    % 按创建时间排序获取消息内容（包含 from_id 和 to_id）
    Column2 = <<"id, payload, from_id, to_id, created_at, server_ts, msg_id, msg_type, e2ee">>,
    case msg_c2g_repo:list_by_ids(MsgIds, Column2) of
        {ok, []} ->
            [];
        {ok, Rows2} ->
            % 与 msg_c2c_ds:read_msg 保持一致，返回包含 from_id 和 to_id 的数据
            % 同时反序列化 payload 与 e2ee 列（二者写入时均为 JSON 字符串）
            [
                elib_response:json_decode_field(
                    elib_response:json_decode_field(Row, <<"payload">>), <<"e2ee">>
                )
             || Row <- Rows2
            ]
    end;
read_msg(ToUid, Limit, LastMsgAt) ->
    % 使用 elib_dt:to_rfc3339/1 统一转换时间戳为 RFC3339 格式
    FixedLastMsgAt = elib_dt:to_rfc3339(LastMsgAt),
    % 获取指定时间之后的用户未确认消息
    Tb = msg_c2g_timeline_repo:tablename(),
    Column = <<"msg_id, created_at">>,
    Where =
        <<" WHERE to_uid = $1 AND client_ack = false AND created_at >= $2 ORDER BY created_at ASC LIMIT $3">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    case elib_pg:query(Sql, [ToUid, FixedLastMsgAt, Limit]) of
        {ok, []} ->
            [];
        {ok, Rows} ->
            MsgIds = [MsgId || #{<<"msg_id">> := MsgId} <- Rows],
            % 按创建时间排序获取消息内容（包含 from_id 和 to_id）
            Column2 =
                <<"id, payload, from_id, to_id, created_at, server_ts, msg_id, msg_type, e2ee">>,
            case msg_c2g_repo:list_by_ids(MsgIds, Column2) of
                {ok, []} ->
                    [];
                {ok, Rows2} ->
                    % 与 msg_c2c_ds:read_msg 保持一致，返回包含 from_id 和 to_id 的数据
                    [elib_response:json_decode_field(Row, <<"payload">>) || Row <- Rows2]
            end
    end.

%% @doc 根据消息ID查找群聊消息（用于撤回权限验证和时间限制检查）
%% @param MsgId 消息唯一ID
%% @return {ok, MsgMap} | {error, Reason}
-spec find_msg_by_id(binary()) -> {ok, map()} | {error, any()}.
find_msg_by_id(MsgId) ->
    msg_c2g_repo:find_msg_by_id(MsgId).

-spec legacy_group_columns([binary()]) -> binary().
legacy_group_columns([]) ->
    <<"id, from_id AS from_uid, to_id AS group_id, payload">>;
legacy_group_columns(Columns) ->
    iolist_to_binary(
        lists:join(
            <<", ">>,
            [legacy_group_column(Column) || Column <- Columns]
        )
    ).

-spec legacy_group_column(binary()) -> binary().
legacy_group_column(<<"from_uid">>) ->
    <<"from_id AS from_uid">>;
legacy_group_column(<<"group_id">>) ->
    <<"to_id AS group_id">>;
legacy_group_column(Column) ->
    Column.

%% @doc 删除群组消息
%%
%% 根据消息ID从群组消息表中删除消息
%%
%% @param Id 消息ID
%% @returns any() 数据库删除操作结果
-spec delete_msg(any()) -> ok | {error, any()}.
delete_msg(Id) ->
    case msg_c2g_repo:delete_msg(Id) of
        {ok, _Count} ->
            ok;
        {error, _Reason} = Error ->
            Error
    end.

%% ===================================================================
%% 引用回复功能
%% ===================================================================

%% @doc 存储带引用回复信息的群组消息
%%
%% 将群组消息及其引用信息存储到数据库中
%%
%% @param CreatedAt 消息创建时间戳（integer 毫秒或 binary RFC3339）
%% @param Id 消息ID
%% @param Payload 消息内容（JSON binary）
%% @param FromId 发送方用户ID
%% @param ToUids 接收消息的用户ID列表
%% @param Gid 群组ID
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON map，可选）
%% @param ReplyToMsgId 被引用回复的消息ID
%% @param ReplyToFromId 被引用消息的发送者ID
%% @param ReplySnippet 被引用消息的摘要
%% @returns ok | {error, Reason} 数据库操作结果
-spec write_msg_with_reply(
    binary() | integer(),
    binary(),
    binary(),
    integer(),
    [integer()],
    integer(),
    binary(),
    map() | null,
    binary(),
    integer(),
    binary()
) ->
    ok | {error, term()}.
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    FromId,
    ToUids,
    Gid,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet
) ->
    msg_c2g_repo:write_msg_with_reply(
        CreatedAt,
        Id,
        Payload,
        FromId,
        ToUids,
        Gid,
        MsgType,
        E2EE,
        ReplyToMsgId,
        ReplyToFromId,
        ReplySnippet
    ).

%% G3: msg_forward_logic / msg_c2g_logic 查找原始群消息记录，不应直调 timeline repo
-spec timeline_find_by_msg_id(binary()) -> {ok, map()} | {error, term()}.
timeline_find_by_msg_id(MsgId) ->
    msg_c2g_timeline_repo:find_by_msg_id(MsgId).

%% G3: msg_reaction_logic / msg_pinned_logic 不应直调 msg_c2g_repo
-spec update_pinned(binary(), integer(), boolean()) -> {ok, non_neg_integer()} | {error, term()}.
update_pinned(MsgId, ToUid, Pinned) ->
    msg_c2g_repo:update_pinned(MsgId, ToUid, Pinned).

%% G3: msg_c2g_logic 不应直调 msg_c2g_repo / thin DS wrappers
-spec count_read(binary()) -> integer().
count_read(MsgId) -> msg_c2g_repo:count_read(MsgId).

-spec update_payload_by_msg_id(binary(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
update_payload_by_msg_id(MsgId, PayloadJson) ->
    msg_c2g_repo:update_payload_by_msg_id(MsgId, PayloadJson).

%% G3: messaging_logic wrapper — timeline delete
-spec timeline_delete_by_msg_ids_and_to_id(list(binary()), integer()) ->
    {ok, integer()} | {error, any()}.
timeline_delete_by_msg_ids_and_to_id(MsgIds, Uid) ->
    msg_c2g_timeline_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid).

%% G3: messaging_logic 不应直调 msg_c2g_timeline_repo:tablename()
-spec count_unread_timeline_since(integer(), binary() | undefined) -> non_neg_integer().
count_unread_timeline_since(ToId, undefined) ->
    Tb = msg_c2g_timeline_repo:tablename(),
    Sql =
        <<"SELECT count(*) as count FROM ", Tb/binary, " WHERE to_id = $1 AND client_ack = false">>,
    case elib_pg:query(Sql, [ToId]) of
        {ok, [#{<<"count">> := Count}]} -> Count;
        _ -> 0
    end;
count_unread_timeline_since(ToId, Since) ->
    Tb = msg_c2g_timeline_repo:tablename(),
    Sql =
        <<"SELECT count(*) as count FROM ", Tb/binary,
            " WHERE to_id = $1 AND client_ack = false AND created_at >= $2">>,
    case elib_pg:query(Sql, [ToId, Since]) of
        {ok, [#{<<"count">> := Count}]} -> Count;
        _ -> 0
    end.

%% G3: msg_c2g_logic 不应直调 msg_c2g_repo:tablename()
-spec set_expire_at(binary(), binary()) -> ok.
set_expire_at(MsgId, ExpireAt) ->
    Tb = msg_c2g_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET expire_at = $1 WHERE msg_id = $2">>,
    case elib_pg:execute(Sql, [ExpireAt, MsgId]) of
        {ok, _} -> ok;
        {error, _Reason} -> ok
    end.

%% G3: msg_burn_logic 不应直调 msg_c2g_repo:tablename()
%% 使用 NOW() 替代参数化时间戳，避免 epgsql 无法编码 RFC3339 binary 为 timestamptz
-spec delete_expired(binary(), pos_integer()) -> non_neg_integer().
delete_expired(_Now, BatchSize) ->
    Tb = msg_c2g_repo:tablename(),
    Sql =
        <<"DELETE FROM ", Tb/binary,
            " WHERE id IN ("
            "  SELECT id FROM ", Tb/binary,
            "  WHERE expire_at IS NOT NULL AND expire_at <= NOW()"
            "  ORDER BY expire_at ASC LIMIT $1)">>,
    case elib_pg:execute(Sql, [BatchSize]) of
        {ok, Count} when is_integer(Count) -> Count;
        _ -> 0
    end.

%% @doc 分页查询群组消息
%% @param Where 过滤条件
%% @param Page 页码
%% @param Size 每页大小
%% @return {ok, map()} | {error, term()}
-spec page(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page(Where, Page, Size) ->
    Tb = msg_c2g_repo:tablename(),
    elib_pg:page_with_total(Tb, Where, Page, Size).
