-module(msg_c2g_ds).
%%%
% msg_c2g_ds 是 msg_c2g domain service 缩写
%%%
-export([write_msg/6]).
-export([write_msg/8]).
-export([write_msg_with_reply/11]).
-export([write_msg_with_reply/12]).
-export([write_msg_with_reply/13]).
-export([read_msg/1]).
-export([read_msg/3, read_msg_after/3, count_pending_after/2]).
-export([read_after_seq/3, count_after_seq/3]).
-export([find_msg_by_id/1]).
-export([delete_msg/1]).
-export([timeline_find_by_msg_id/1]).
-export([update_pinned/3]).
-export([count_read/1]).
-export([update_payload_by_msg_id/2]).
-export([timeline_delete_by_msg_ids_and_to_id/2]).
-export([count_unread_timeline_since/2]).
-export([count_unread_by_uid/2]).
-export([check_and_delete_overflow/1]).
-export([set_expire_at/2]).
-export([delete_expired/2]).
-export([page/3]).

-include("log.hrl").
-include("chat.hrl").

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
%% spec 由按参分信的多子句改为单一联合（同 read_msg，配方 c）：多子句
%% spec 的子句交集分析触发 Gradualizer pick_value(none()) 崩溃。
%% 第一子句三守卫保留——它们是两种调用约定的真实分派（非冗余细化），
%% 删除会让 map 载荷调用错误落入文本载荷路径。
-spec write_msg(
    integer() | binary(),
    binary(),
    binary() | integer(),
    integer(),
    list() | map(),
    integer() | binary()
) -> ok.
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
            Class:Reason ->
                ok = ?ERROR_LOG([msg_c2g_write_payload_decode_failed, Id, Class, Reason]),
                #{}
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
%% @param MsgId 撤回通知消息ID（新插入的通知行）
%% @doc 读取群消息
%%
%% 兼容两种历史入口：
%% 1. `read_msg(MsgId)` 读取单条群消息；
%% 2. `read_msg(ToUid)` 读取指定用户的离线群消息。
%% spec 由按参分信的多子句改为单一联合：多子句 spec 的子句交集分析触发
%% Gradualizer pick_value(none()) 崩溃（配方 c，2026-10-04）；两守卫保留
%% （分派语义不变），返回联合放宽由调用方按入参类型自证。
-spec read_msg(integer() | binary()) ->
    [map()] | {ok, map()} | {error, any()}.
read_msg(MsgId) when is_binary(MsgId) ->
    case find_msg_by_id(MsgId) of
        {error, not_found} ->
            {ok, #{}};
        Result ->
            Result
    end;
% msg_c2g_ds:read_msg(3).
%% 不加 is_integer(ToUid) 守卫：多子句 spec 的类型分派靠第一子句的
%% is_binary 守卫保序，第二子句冗余守卫细化触发 pick_value(none()) 崩溃
%% （配方 b'，2026-10-04）；非整数入参由此进入离线读路径按 SQL 语义报错。
read_msg(ToUid) ->
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
    Column = <<"tl.msg_id, tl.created_at, tl.conv_seq">>,
    {ok, Rows} = msg_c2g_timeline_repo:list_by_uid(ToUid, Column, Limit),
    load_timeline_messages(Rows);
read_msg(ToUid, Limit, LastMsgAt) ->
    Column = <<"tl.msg_id, tl.created_at, tl.conv_seq">>,
    Since = elib_dt:to_rfc3339(LastMsgAt),
    {ok, Rows} = msg_c2g_timeline_repo:list_by_uid_since(ToUid, Column, Limit, Since),
    load_timeline_messages(Rows).

read_msg_after(ToUid, Limit, Cursor) ->
    Column = <<"tl.msg_id, tl.created_at, tl.conv_seq">>,
    {ok, Rows} = msg_c2g_timeline_repo:list_by_uid_after(ToUid, Column, Limit, Cursor),
    load_timeline_messages(Rows).

count_pending_after(ToUid, Cursor) ->
    msg_c2g_timeline_repo:count_pending_after(ToUid, Cursor).

%% R01：单群 conv_seq 游标读取——服务端投递进度基于权威接受顺序（D05/R02
%% 的 B 边界钳制），不依赖客户端 created_at。Mark = {Gid, Seq}。
read_after_seq(ToUid, Limit, {Gid, Seq}) when is_integer(Limit), Limit >= 1 ->
    Column = <<"tl.msg_id, tl.created_at, tl.conv_seq, tl.to_gid">>,
    case msg_c2g_timeline_repo:list_by_uid_after_seq(ToUid, Column, Limit, {Gid, Seq}) of
        {ok, Rows} ->
            load_timeline_messages(Rows);
        {error, Reason} ->
            ?ERROR_LOG([c2g_read_after_seq_failed, ToUid, Gid, Reason]),
            []
    end.

count_after_seq(ToUid, Gid, Seq) ->
    msg_c2g_timeline_repo:count_after_seq(ToUid, Gid, Seq).

%% The content query can return timestamp ties in a different order. Keep
%% the timeline's total ordering, including its authoritative timestamp.
load_timeline_messages([]) ->
    [];
load_timeline_messages(Rows) ->
    MsgIds = [maps:get(<<"msg_id">>, Row) || Row <- Rows],
    Columns =
        <<"id, payload, from_id, to_id, created_at, server_ts, msg_id, msg_type, e2ee, sender_did">>,
    {ok, Contents} = msg_c2g_repo:list_by_ids(MsgIds, Columns),
    ById = maps:from_list([{maps:get(<<"msg_id">>, Row), Row} || Row <- Contents]),
    SeqById = timeline_seq_by_msg_id(Rows),
    [
        attach_timeline_conv_seq(
            decode_offline_message((maps:get(Id, ById))#{
                <<"created_at">> => maps:get(
                    <<"created_at">>, Row, maps:get(<<"created_at">>, maps:get(Id, ById), 0)
                )
            }),
            SeqById
        )
     || Row <- Rows, Id <- [maps:get(<<"msg_id">>, Row)], maps:is_key(Id, ById)
    ].

%% Legacy rows stored the complete realtime envelope in the payload column.
%% Unwrap only when its identity agrees with the authoritative database row.
decode_offline_message(Row) ->
    Decoded = elib_response:json_decode_field(
        elib_response:json_decode_field(Row, <<"payload">>), <<"e2ee">>
    ),
    restore_legacy_payload(Decoded).

restore_legacy_payload(
    #{
        <<"msg_id">> := Id,
        <<"from_id">> := FromId,
        <<"to_id">> := ToId,
        <<"payload">> := #{
            <<"id">> := Id,
            <<"type">> := <<"C2G">>,
            <<"from">> := From,
            <<"to">> := To,
            <<"payload">> := Body
        }
    } = Row
) when is_integer(FromId), is_integer(ToId) ->
    case {integer_to_binary(FromId), integer_to_binary(ToId)} of
        {From, To} -> Row#{<<"payload">> := Body};
        _ -> Row
    end;
restore_legacy_payload(Row) ->
    Row.

-spec timeline_seq_by_msg_id([map()]) -> map().
timeline_seq_by_msg_id(Rows) ->
    maps:from_list([
        {MsgId, Seq}
     || #{<<"msg_id">> := MsgId, <<"conv_seq">> := Seq} <- Rows,
        is_integer(Seq),
        Seq > 0
    ]).

%% conv_seq 的权威来源是按收件人冻结的 timeline 行，不接受消息正文自报值。
-spec attach_timeline_conv_seq(map(), map()) -> map().
attach_timeline_conv_seq(Msg, SeqByMsgId) ->
    MsgId = maps:get(<<"msg_id">>, Msg, undefined),
    case maps:find(MsgId, SeqByMsgId) of
        {ok, ConvSeq} -> Msg#{<<"conv_seq">> => ConvSeq};
        error -> Msg
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
    %% R03/D04：兼容旧入口，不带接受时间（accepted_at 列保持 NULL）。
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
        ReplySnippet,
        null,
        null
    ).

%% @doc R03/D04（IMBOY-TIMEZONE-REPAIR-V1）：带接受时间的引用回复直写。
%% AcceptedAt 与 staging 行同一值（首次持久接受事务内固定的服务端接受时间）。
%% @doc R03/D04 + R05-A 兼容旧入口（/12）：无自毁期限。
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
    ReplySnippet,
    AcceptedAt
) ->
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
        ReplySnippet,
        AcceptedAt,
        null
    ).

%% @doc R03/D04 + R05-A（IMBOY-TIMEZONE-REPAIR-V1）：带接受时间与自毁期限的
%% 引用回复直写。AcceptedAt/ExpireAt 与 staging 行同一值（接受事务内固定）。
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
    binary(),
    binary() | null,
    binary() | null
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
    ReplySnippet,
    AcceptedAt,
    ExpireAt
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
        ReplySnippet,
        null,
        AcceptedAt,
        ExpireAt
    ).

%% G3: msg_forward_logic / msg_c2g_logic 查找原始群消息记录，不应直调 timeline repo
-spec timeline_find_by_msg_id(binary()) -> {ok, [map()]} | {error, term()}.
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
    msg_c2g_timeline_repo:count_pending_by_uid(ToId, undefined);
count_unread_timeline_since(ToId, Since) ->
    msg_c2g_timeline_repo:count_pending_by_uid(ToId, Since).

%% Q04/F05（I1/I4）：会话级未读数——由 per-user per-group 已读游标驱动，
%% 与送达标记 client_ack 完全解耦（送达 ACK 永不减未读）。
%% G3: logic 层消费入口，不应直调 msg_c2g_timeline_repo。
-spec count_unread_by_uid(integer(), integer()) -> non_neg_integer().
count_unread_by_uid(Uid, Gid) ->
    msg_c2g_timeline_repo:count_unread_by_uid(Uid, Gid).

%% Q04/F05（I3）：timeline 行不再被 offline_ack 删除后的容量治理兜底
%% （retention 30 天之外的近实时清理），模式与 msg_c2c_ds 一致：
%% 1/20 采样 + ?SAVE_MSG_LIMIT 上限 + 异步删最老。
-spec check_and_delete_overflow(integer()) -> ok.
check_and_delete_overflow(ToUid) ->
    _ =
        case rand:uniform(20) =:= 1 of
            true ->
                Count = msg_c2g_timeline_repo:count_by_to_id(ToUid),
                case Count >= ?SAVE_MSG_LIMIT of
                    true ->
                        Limit = Count - ?SAVE_MSG_LIMIT + 1,
                        _ = elib_async:async_retry(
                            fun() ->
                                msg_c2g_timeline_repo:delete_overflow_timeline(ToUid, Limit)
                            end
                        );
                    false ->
                        ok
                end;
            false ->
                ok
        end,
    ok.

%% G3: msg_c2g_logic 不应直调 msg_c2g_repo:tablename()
%% R05-A（IMBOY-TIMEZONE-REPAIR-V1）：
%%   1. 仅当 expire_at 当前为 NULL 时写入——重复请求/重试不延长期限；
%%   2. 0 行 UPDATE 返回 {ok, 0}（明确结果），错误返回 {error, Reason}
%%      不再吞成 ok；期限持久化主路径是 staging 行携带 → worker 转正搬运。
-spec set_expire_at(binary(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
set_expire_at(MsgId, ExpireAt) ->
    Tb = msg_c2g_repo:tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET expire_at = $1 WHERE msg_id = $2", " AND expire_at IS NULL">>,
    case elib_pg:execute(Sql, [ExpireAt, MsgId]) of
        {ok, Count} when is_integer(Count) ->
            {ok, Count};
        {error, Reason} ->
            ?ERROR_LOG([msg_c2g_set_expire_at_failed, MsgId, Reason]),
            {error, Reason}
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
