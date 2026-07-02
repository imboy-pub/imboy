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
-export([delete_delivered_batch/4]).
-export([pending_filter/3]).

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
    Tb = tablename(),
    %% 每条消息一组 VALUES ($k, $m, $uid, $did)，参数顺序：Kind, Uid, Did, 然后逐个 MsgId
    Base = 3,
    Rows = iolist_to_binary(
        lists:join(
            <<",">>,
            [
                <<"($1, $", (integer_to_binary(Base + I))/binary, ", $2, $3)">>
             || I <- lists:seq(1, length(MsgIds))
            ]
        )
    ),
    Sql =
        <<"INSERT INTO ", Tb/binary, " (msg_kind, msg_id, to_uid, to_did) VALUES ", Rows/binary,
            " ON CONFLICT DO NOTHING">>,
    elib_pg:execute(Sql, [Kind, ToUid, ToDid | MsgIds]).

%% @doc 若消息已被该用户全部活跃设备确认，则删除主行并清除标记
%% 活跃设备 = user_device 中 status=1 且 last_active_at 在 ActiveDays 天内的设备。
%% 超过活跃窗口仍未确认的设备不再阻塞清理（其离线消息保留期即 ActiveDays）。
-spec delete_delivered(binary(), binary(), integer(), non_neg_integer()) -> ok.
delete_delivered(Kind, MsgId, ToUid, ActiveDays) ->
    delete_delivered_batch(Kind, [MsgId], ToUid, ActiveDays).

%% @doc 批量版本：两条 SQL——删已全端确认的主行，再清除主行已消失的标记
-spec delete_delivered_batch(binary(), [binary()], integer(), non_neg_integer()) -> ok.
delete_delivered_batch(_Kind, [], _ToUid, _ActiveDays) ->
    ok;
delete_delivered_batch(Kind, MsgIds, ToUid, ActiveDays) ->
    Tb = tablename(),
    MsgTb = msg_tablename(Kind),
    DeviceTb = user_device_repo:tablename(),
    %% 主表接收人列均为 to_id（见 00000005/00000008 DDL）
    %% 参数：$1=Kind, $2=ToUid, $3=ActiveDays, $4..=MsgIds
    Base = 3,
    In = placeholders(Base, length(MsgIds)),
    DelMainSql = <<
        "DELETE FROM ",
        MsgTb/binary,
        " m WHERE m.to_id = $2 AND m.msg_id IN (",
        In/binary,
        ")",
        " AND EXISTS (SELECT 1 FROM ",
        Tb/binary,
        " a2 WHERE a2.msg_kind = $1 AND a2.msg_id = m.msg_id AND a2.to_uid = $2)",
        " AND NOT EXISTS (SELECT 1 FROM ",
        DeviceTb/binary,
        " d WHERE d.user_id = $2 AND d.status = 1",
        " AND d.last_active_at >= now() - make_interval(days => $3)",
        " AND NOT EXISTS (SELECT 1 FROM ",
        Tb/binary,
        " a WHERE a.msg_kind = $1 AND a.msg_id = m.msg_id",
        " AND a.to_uid = $2 AND a.to_did = d.device_id))"
    >>,
    Params = [Kind, ToUid, ActiveDays | MsgIds],
    case elib_pg:execute(DelMainSql, Params) of
        {ok, N} when N > 0 ->
            %% 清除主行已消失的标记（含历史遗留孤儿标记）
            DelMarkSql = <<
                "DELETE FROM ",
                Tb/binary,
                " a WHERE a.msg_kind = $1 AND a.to_uid = $2 AND a.msg_id IN (",
                In/binary,
                ") AND NOT EXISTS (SELECT 1 FROM ",
                MsgTb/binary,
                " m WHERE m.msg_id = a.msg_id AND m.to_id = a.to_uid)"
            >>,
            _ = elib_pg:execute(DelMarkSql, [Kind, ToUid | MsgIds]),
            ok;
        _ ->
            ok
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

placeholders(Base, Count) ->
    iolist_to_binary(
        lists:join(
            <<",">>,
            [<<"$", (integer_to_binary(Base + I))/binary>> || I <- lists:seq(1, Count)]
        )
    ).
