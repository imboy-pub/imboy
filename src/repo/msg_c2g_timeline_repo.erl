-module(msg_c2g_timeline_repo).
%%%
% msg_c2g_timeline_repo 是 msg_c2g_timeline repository 缩写
%%%

-include("log.hrl").

-export([tablename/0]).
-export([client_ack/2]).
-export([client_ack_batch/2]).
-export([delete_timeline/2]).
-export([list_by_uid/2, list_by_uid/3]).
-export([list_by_uid_since/4, list_by_uid_after/4, count_pending_after/2]).
-export([count_pending_by_uid/2]).
-export([count_unread_by_uid/2]).
-export([check_msg/1]).
-export([count_by_to_id/1]).
-export([delete_by_msg_id/1]).
-export([delete_overflow_timeline/2]).
-export([delete_by_msg_ids_and_to_id/2]).
-export([find_by_msg_id/1]).

%% ===================================================================
%% API
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_c2g_timeline">>).

% msg_c2g_timeline_repo:list_by_uid(2, <<"msg_id">>, 10).
-spec list_by_uid(integer(), binary()) -> {ok, list(map())} | {error, term()}.
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000000).

-spec list_by_uid(integer(), binary(), integer()) -> {ok, list(map())} | {error, term()}.
list_by_uid(Uid, Column, Limit) ->
    list_authorized(Uid, Column, Limit, undefined).

-spec list_by_uid_since(integer(), binary(), integer(), binary()) ->
    {ok, list(map())} | {error, term()}.
list_by_uid_since(Uid, Column, Limit, Since) ->
    list_authorized(Uid, Column, Limit, Since).

-spec list_authorized(integer(), binary(), integer(), undefined | binary()) ->
    {ok, list(map())} | {error, term()}.
list_authorized(Uid, Column, Limit, Since) ->
    Tb = tablename(),
    {SinceSql, Params} =
        case Since of
            undefined -> {<<>>, [Uid, Limit]};
            _ -> {<<" AND tl.created_at >= $2">>, [Uid, Since, Limit]}
        end,
    LimitParam = integer_to_binary(length(Params)),
    Sql = authorized_pending_sql(Tb, Column, SinceSql, LimitParam),
    elib_pg:query(Sql, Params).

%% Composite cursor uses exactly the same ordering for the page and count.
list_by_uid_after(Uid, Column, Limit, {At, Id}) ->
    Where = <<" AND (tl.created_at, tl.msg_id) > ($2::timestamptz, $3::varchar)">>,
    Sql = authorized_pending_sql(tablename(), Column, Where, <<"4">>),
    elib_pg:query(Sql, [Uid, At, Id, Limit]).

count_pending_after(Uid, {At, Id}) ->
    Where = <<" AND (tl.created_at, tl.msg_id) > ($2::timestamptz, $3::varchar)">>,
    Sql = authorized_pending_sql(tablename(), <<"count(*) AS count">>, Where, undefined),
    case elib_pg:query(Sql, [Uid, At, Id]) of
        {ok, [#{<<"count">> := Count}]} -> Count;
        {error, Reason} -> error({offline_count_failed, Reason})
    end.

-spec count_pending_by_uid(integer(), undefined | binary()) -> non_neg_integer().
count_pending_by_uid(Uid, Since) ->
    Tb = tablename(),
    {SinceSql, Params} =
        case Since of
            undefined -> {<<>>, [Uid]};
            _ -> {<<" AND tl.created_at >= $2">>, [Uid, Since]}
        end,
    Sql = authorized_pending_sql(Tb, <<"count(*) AS count">>, SinceSql, undefined),
    case elib_pg:query(Sql, Params) of
        {ok, [#{<<"count">> := Count}]} -> Count;
        _ -> 0
    end.

%% Q04/F05（I3）：pending 过滤只保留成员代际 ACL（generation boundary），
%% 不再排除 client_ack = true 的行——timeline 行是 per (to_uid, msg_id) 的
%% 投递记录，client_ack 是 per-uid 送达标记：同 uid 的设备 A ACK 后，设备 B
%% 离线取回仍必须拿到该行（客户端按各自时间游标 next_last_msg_at 增量拉取，
%% 行保留不会造成重复下发）。行清理交给 retention（30 天）与容量治理。
authorized_pending_sql(Tb, Column, SinceSql, LimitParam) ->
    Entitlement = micro_community_access_repo:predicate_sql(<<"grp.id">>, <<"tl.to_uid">>),
    LimitSql =
        case LimitParam of
            undefined -> <<>>;
            _ -> <<" ORDER BY tl.created_at ASC, tl.msg_id ASC LIMIT $", LimitParam/binary>>
        end,
    <<"SELECT ", Column/binary, " FROM ", Tb/binary, " tl ",
        "JOIN public.\"group\" grp ON grp.id = tl.to_gid AND grp.status = 1 ",
        "JOIN public.group_member gm ON gm.group_id = tl.to_gid ",
        "AND gm.user_id = tl.to_uid AND gm.status = 1 ",
        "JOIN public.group_member_generation gmg ON gmg.group_id = tl.to_gid ",
        "AND gmg.user_id = tl.to_uid AND gmg.end_seq IS NULL ", "WHERE tl.to_uid = $1 ",
        "AND tl.conv_seq IS NOT NULL AND tl.conv_seq >= gmg.start_seq AND ", Entitlement/binary,
        SinceSql/binary, LimitSql/binary>>.

%% Q04/F05（I1/I4）：未读计数由 per-user per-group 的已读游标
%% （workspace_group_read_cursor.read_seq，按成员代际隔离）驱动——
%% 未读 = 已投递（timeline 行存在）且 conv_seq > 已读游标、非本人发送、
%% 未过期、非撤回确认。与 client_ack（送达）完全解耦：送达 ACK 永不减未读。
-spec count_unread_by_uid(integer(), integer()) -> non_neg_integer().
count_unread_by_uid(Uid, Gid) ->
    Entitlement = micro_community_access_repo:predicate_sql(<<"grp.id">>, <<"tl.to_uid">>),
    Tb = tablename(),
    Sql =
        <<"SELECT count(DISTINCT tl.msg_id)::integer AS count FROM ", Tb/binary, " tl ",
            "JOIN public.\"group\" grp ON grp.id = tl.to_gid AND grp.status = 1 ",
            "JOIN public.group_member gm ON gm.group_id = tl.to_gid ",
            "AND gm.user_id = tl.to_uid AND gm.status = 1 ",
            "JOIN public.msg_c2g m ON m.msg_id = tl.msg_id ",
            "AND m.created_at = tl.created_at AND m.to_id = tl.to_gid ",
            "JOIN public.group_member_generation gmg ON gmg.group_id = tl.to_gid ",
            "AND gmg.user_id = tl.to_uid AND gmg.end_seq IS NULL ",
            "LEFT JOIN public.workspace_group_read_cursor r ON r.generation_id = gmg.id ",
            "WHERE tl.to_uid = $1 AND tl.to_gid = $2 ",
            "AND tl.conv_seq IS NOT NULL AND tl.conv_seq >= gmg.start_seq ",
            "AND tl.conv_seq > COALESCE(r.read_seq, 0) AND m.from_id <> $1 ",
            "AND (m.expire_at IS NULL OR m.expire_at > NOW()) ",
            "AND m.payload->>'action' IS DISTINCT FROM 'message_revoke_ack' AND ",
            Entitlement/binary>>,
    case elib_pg:query(Sql, [Uid, Gid]) of
        {ok, [#{<<"count">> := Count}]} when is_integer(Count) -> Count;
        _ -> 0
    end.

% msg_c2g_timeline_repo:client_ack(109, <<"cor1aup1a20rgjtl5t8g">>).
-spec client_ack(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
client_ack(ToUid, MsgId) ->
    Tb = tablename(),
    % use index uk_c2g_timeline_ToUid_MsgId
    % 使用安全的参数化查询，避免SQL注入
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET client_ack = true WHERE to_uid = $1 AND msg_id = $2 AND client_ack = false">>,
    elib_pg:execute(Sql, [ToUid, MsgId]).

%% Q04/F05（I3）：offline_ack 的 c2g 批量送达确认——置 per-uid 送达标记而非删行。
%% 此前离线拉取确认直接删除 per-uid 行，任一设备确认后其他设备再也无法通过
%% 离线链路取回（设备 B 永丢）。行清理交给 retention（30 天）与容量治理。
-spec client_ack_batch(list(binary()), integer()) -> {ok, non_neg_integer()} | {error, term()}.
client_ack_batch([], _ToUid) ->
    {ok, 0};
client_ack_batch(MsgIds, ToUid) when is_list(MsgIds) ->
    Tb = tablename(),
    Placeholders = iolist_to_binary(
        lists:join(
            <<",">>,
            [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(MsgIds))]
        )
    ),
    Last = integer_to_binary(length(MsgIds) + 1),
    Sql =
        <<"UPDATE ", Tb/binary, " SET client_ack = true ", "WHERE msg_id IN (", Placeholders/binary,
            ") AND to_uid = $", Last/binary, " AND client_ack = false">>,
    elib_pg:execute(Sql, MsgIds ++ [ToUid]).

% msg_c2g_timeline_repo:delete_timeline(6).
-spec delete_timeline(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete_timeline(ToUid, MsgId) ->
    Tb = tablename(),
    % use index uk_c2g_timeline_ToUid_MsgId
    Where = <<" WHERE to_uid = $1 AND  msg_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    elib_pg:execute(Sql, [ToUid, MsgId]).

% msg_c2g_timeline_repo:check_msg(1).
-spec check_msg(binary()) -> non_neg_integer().
check_msg(MsgId) ->
    % use index uk_c2g_timeline_MsgId
    % 使用安全的参数化查询，避免SQL注入
    Tb = tablename(),
    Sql = <<"SELECT count(*) as count FROM ", Tb/binary, " WHERE msg_id = $1">>,
    case elib_pg:query(Sql, [MsgId]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

% msg_c2g_timeline_repo:count_by_to_id(1).
-spec count_by_to_id(integer()) -> non_neg_integer().
count_by_to_id(ToUid) ->
    % use index uk_c2g_timeline_ToUid_MsgId
    % 使用安全的参数化查询，避免SQL注入
    elib_pg:pluck_value(tablename(), <<"count(*)">>, #{to_uid => ToUid}, #{}, 0).

% msg_c2g_timeline_repo:delete_overflow_timeline(1, 100).
-spec delete_overflow_timeline(integer(), integer()) -> ok | {atom(), list(binary())}.
delete_overflow_timeline(ToUid, Limit) ->
    Tb = tablename(),
    % use index uk_c2g_timeline_ToUid_MsgId
    Where = <<" WHERE to_uid = $1 ORDER BY created_at ASC LIMIT $2">>,
    Sql = <<"SELECT msg_id FROM ", Tb/binary, Where/binary>>,
    % ?DEBUG_LOG(Sql),
    case elib_pg:query(Sql, [ToUid, Limit]) of
        {ok, []} ->
            ok;
        {ok, Rows} ->
            _ = [delete_timeline(ToUid, MsgId) || #{<<"msg_id">> := MsgId} <- Rows],
            {msg_ids, [MsgId || #{<<"msg_id">> := MsgId} <- Rows]}
    end.

% 根据消息ID删除群消息时间线记录
-spec delete_by_msg_id(binary()) -> {ok, 1} | {error, term()}.
delete_by_msg_id(MsgId) ->
    Tb = tablename(),
    Where = <<"WHERE msg_id = $1">>,
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary, " RETURNING to_uid">>,
    case elib_pg:execute(Sql, [MsgId]) of
        {ok, _Count, _Rows} ->
            {ok, 1};
        {ok, _Count} ->
            {ok, 1};
        {error, Reason} ->
            {error, Reason}
    end.

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
        <<"DELETE FROM ", Tb/binary, " WHERE msg_id IN (", Placeholders/binary, ") AND to_uid = $",
            (integer_to_binary(length(MsgIds) + 1))/binary>>,
    elib_pg:execute(Sql, MsgIds ++ [ToUid]);
delete_by_msg_ids_and_to_id([], _ToUid) ->
    {ok, 0}.

%% @doc 根据消息ID查询群消息时间线
%% 返回指定消息的群组ID
%% @param MsgId 消息ID
%% @return {ok, list(map())} | {error, Reason}
%% @end
-spec find_by_msg_id(binary()) -> {ok, list(map())} | {error, any()}.
find_by_msg_id(MsgId) ->
    Tb = tablename(),
    Sql = <<"SELECT to_gid FROM ", Tb/binary, " WHERE msg_id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [MsgId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
