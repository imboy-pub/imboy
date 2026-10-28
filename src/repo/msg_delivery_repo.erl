-module(msg_delivery_repo).
%%%
% msg_delivery_repo 是"按设备送达确认标记"的数据访问层（T03/P0-1，根因 R2）
%
% 语义：msg_delivery 一行 = (Kind, MsgId, ToUid) 消息已被设备 ToDid 确认；
% 无行 = 该设备尚未确认（离线/新设备天然无行，仍可拉到未清理的主行）。
% 主行（msg_c2c/msg_s2c）由 delete_delivered/4 在"全部活跃设备均已确认"后删除。
%%%

-export([tablename/0]).
-export([mark_acked/4]).
-export([mark_acked_batch/4]).
-export([delete_delivered/4]).
-export([delete_delivered_batch/4, delete_delivered_batch/5]).
-export([pending_filter/3, register_read_notice/4]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_delivery">>).

%% @doc 记录"设备 ToDid 已确认消息"（幂等，重复 ACK 无副作用）
-spec mark_acked(binary(), binary(), integer(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
mark_acked(Kind, MsgId, ToUid, ToDid) ->
    mark_acked_batch(Kind, [MsgId], ToUid, ToDid).

%% @doc 批量记录送达确认（单条 SQL，幂等）
-spec mark_acked_batch(binary(), [binary()], integer(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
mark_acked_batch(_Kind, [], _ToUid, _ToDid) ->
    {ok, 0};
mark_acked_batch(Kind, MsgIds, ToUid, ToDid) ->
    delivery_tx(fun(Conn) ->
        lock_messages(Conn, Kind, MsgIds, ToUid),
        Tb = tablename(),
        Main = msg_tablename(Kind),
        Staging = msg_store_repo:tablename(),
        %% shortcut: GC 后无终态证据的 ACK 仍保留；需要无限期去重时再引入持久终态账本。
        Sql =
            <<"INSERT INTO ", Tb/binary,
                " (msg_kind,msg_id,to_uid,to_did) SELECT $1::varchar, ids.msg_id, $2::bigint, $3::varchar ",
                "FROM unnest($4::varchar[]) ids(msg_id) WHERE NOT EXISTS (SELECT 1 FROM ",
                Staging/binary, " s WHERE (s.type=$1 OR ($1='c2c' AND s.type='c2c_notice' AND ",
                "s.action='message_read')) AND s.msg_id=ids.msg_id AND s.to_id=$2 ",
                "AND s.processed_at IS NOT NULL AND s.error_msg IS NULL ",
                "AND NOT EXISTS (SELECT 1 FROM ", Staging/binary,
                " p WHERE p.type=$1 AND p.msg_id=ids.msg_id AND p.to_id=$2 ",
                "AND (p.processed_at IS NULL OR p.error_msg IS NOT NULL)) ",
                "AND NOT EXISTS (SELECT 1 FROM ", Main/binary,
                " m WHERE m.msg_id=ids.msg_id AND m.to_id=$2)) ON CONFLICT DO NOTHING">>,
        checked(elib_pg:execute_count(Conn, Sql, [Kind, ToUid, ToDid, MsgIds]))
    end).

%% 在线已读通知不进入时间线；processed_at 仅表示登记已提交，沿用 staging 的 1h GC。
%% 重放固定 msg_ids；read_at 可因客户端省略后按 now 补齐而变化，不覆盖首次登记。
-spec register_read_notice(binary(), integer(), integer(), map()) -> ok | {error, term()}.
register_read_notice(MsgId, FromUid, ToUid, Payload) ->
    delivery_tx(fun(Conn) ->
        lock_messages(Conn, <<"c2c">>, [MsgId], ToUid),
        Tb = msg_store_repo:tablename(),
        Sql =
            <<"INSERT INTO ", Tb/binary,
                " (id,type,msg_id,action,payload,from_id,to_id,processed_at) ",
                "VALUES($1,'c2c_notice',$2,'message_read',$3,$4,$5,now()) ON CONFLICT DO NOTHING">>,
        _ = checked(
            elib_pg:execute(
                Conn,
                Sql,
                [elib_tsid:generate(msg_store), MsgId, jsone:encode(Payload), FromUid, ToUid]
            )
        ),
        case
            checked(
                elib_pg:query(
                    Conn,
                    <<"SELECT 1 FROM ", Tb/binary,
                        " WHERE type='c2c_notice' AND msg_id=$1 AND from_id=$2 AND to_id=$3 ",
                        "AND action='message_read' AND processed_at IS NOT NULL AND error_msg IS NULL ",
                        "AND payload->'msg_ids'=$4::jsonb">>,
                    [MsgId, FromUid, ToUid, jsone:encode(maps:get(<<"msg_ids">>, Payload, []))]
                )
            )
        of
            {ok, [_]} ->
                delete_delivered_batch(
                    Conn,
                    <<"c2c">>,
                    [MsgId],
                    ToUid,
                    application:get_env(imboy, msg_delivery_active_days, 30)
                );
            {ok, []} ->
                elib_pg:abort({rollback, notification_id_conflict})
        end
    end).

%% @doc 若消息已被该用户全部活跃设备确认，则删除主行并清除标记
%% 活跃设备 = user_device 中 status=1 且 last_active_at 在 ActiveDays 天内的设备。
%% 超过活跃窗口仍未确认的设备不再阻塞清理（其离线消息保留期即 ActiveDays）。
-spec delete_delivered(binary(), binary(), integer(), non_neg_integer()) -> ok | {error, term()}.
delete_delivered(Kind, MsgId, ToUid, ActiveDays) ->
    delete_delivered_batch(Kind, [MsgId], ToUid, ActiveDays).

%% @doc 同一消息的 ACK 与完成清理串行；无主行不是终态证据。
-spec delete_delivered_batch(binary(), [binary()], integer(), non_neg_integer()) ->
    ok | {error, term()}.
delete_delivered_batch(_Kind, [], _ToUid, _ActiveDays) ->
    ok;
delete_delivered_batch(Kind, MsgIds, ToUid, ActiveDays) ->
    delivery_tx(fun(Conn) ->
        delete_delivered_batch(Conn, Kind, MsgIds, ToUid, ActiveDays)
    end).

%% @doc worker 完成事务使用同一连接，清理失败必须回滚 processed_at。
delete_delivered_batch(Conn, Kind, MsgIds, ToUid, ActiveDays) ->
    lock_messages(Conn, Kind, MsgIds, ToUid),
    Tb = tablename(),
    Main = msg_tablename(Kind),
    Staging = msg_store_repo:tablename(),
    Devices = user_device_repo:tablename(),
    Pending =
        <<"SELECT 1 FROM ", Staging/binary,
            " s WHERE s.type=$1 AND s.msg_id=m.msg_id AND s.to_id=$2 AND (s.processed_at IS NULL ",
            "OR s.error_msg IS NOT NULL)">>,
    Sql =
        <<"DELETE FROM ", Main/binary, " m WHERE m.to_id=$2 AND m.msg_id=ANY($4::varchar[]) ",
            "AND NOT EXISTS (", Pending/binary, ") ", "AND EXISTS (SELECT 1 FROM ", Tb/binary,
            " a WHERE a.msg_kind=$1 AND a.msg_id=m.msg_id AND a.to_uid=$2) ",
            "AND NOT EXISTS (SELECT 1 FROM ", Devices/binary,
            " d WHERE d.user_id=$2 AND d.status=1 ",
            "AND d.last_active_at >= now()-make_interval(days => $3) ",
            "AND NOT EXISTS (SELECT 1 FROM ", Tb/binary,
            " a WHERE a.msg_kind=$1 AND a.msg_id=m.msg_id AND a.to_uid=$2 ",
            "AND a.to_did=d.device_id)) RETURNING m.msg_id">>,
    {ok, Deleted} = checked(elib_pg:query(Conn, Sql, [Kind, ToUid, ActiveDays, MsgIds])),
    DeletedIds = [maps:get(<<"msg_id">>, Row) || Row <- Deleted],
    Cleanup =
        <<"DELETE FROM ", Tb/binary,
            " a WHERE a.msg_kind=$1 AND a.to_uid=$2 AND a.msg_id=ANY($3::varchar[]) ",
            "AND NOT EXISTS (SELECT 1 FROM ", Main/binary,
            " m WHERE m.msg_id=a.msg_id AND m.to_id=a.to_uid) ", "AND NOT EXISTS (SELECT 1 FROM ",
            Staging/binary,
            " s WHERE s.type=$1 AND s.msg_id=a.msg_id AND s.to_id=$2 AND (s.processed_at IS NULL ",
            "OR s.error_msg IS NOT NULL)) ",
            "AND (a.msg_id=ANY($4::varchar[]) OR EXISTS (SELECT 1 FROM ", Staging/binary,
            " s WHERE (s.type=$1 OR ($1='c2c' AND s.type='c2c_notice' AND ",
            "s.action='message_read')) AND s.msg_id=a.msg_id AND s.to_id=$2 ",
            "AND s.processed_at IS NOT NULL AND s.error_msg IS NULL))">>,
    _ = checked(elib_pg:execute(Conn, Cleanup, [Kind, ToUid, MsgIds, DeletedIds])),
    ok.

lock_messages(Conn, Kind, MsgIds, ToUid) ->
    lists:foreach(
        fun(MsgId) ->
            Key = jsone:encode([Kind, ToUid, MsgId]),
            _ = checked(
                elib_pg:query(
                    Conn,
                    <<"SELECT pg_advisory_xact_lock(hashtextextended($1,0))">>,
                    [Key]
                )
            )
        end,
        lists:usort(MsgIds)
    ).

checked({error, Reason}) -> elib_pg:abort({rollback, Reason});
checked(Result) -> Result.

delivery_tx(Fun) ->
    case elib_pg:with_tx(Fun) of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

%% @doc 生成离线读取用的反连接过滤片段（追加到主表 WHERE 之后）
%% UidIdx/DidIdx 为调用方 SQL 中 to_uid / to_did 参数的占位序号。
%% 依赖主表以 public.msg_c2c / public.msg_s2c 形式出现在 FROM 中（可用表名限定外层列）。
-spec pending_filter(binary(), pos_integer(), pos_integer()) -> binary().
pending_filter(Kind, UidIdx, DidIdx) ->
    Tb = tablename(),
    MsgTbShort = <<"msg_", Kind/binary>>,
    <<" AND NOT EXISTS (SELECT 1 FROM ", Tb/binary, " a WHERE a.msg_kind = '", Kind/binary,
        "' AND a.msg_id = ", MsgTbShort/binary, ".msg_id AND a.to_uid = $",
        (integer_to_binary(UidIdx))/binary, " AND a.to_did = $", (integer_to_binary(DidIdx))/binary,
        ")">>.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

msg_tablename(<<"c2c">>) ->
    msg_c2c_repo:tablename();
msg_tablename(<<"s2c">>) ->
    msg_s2c_repo:tablename().
