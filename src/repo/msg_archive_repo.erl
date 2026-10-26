-module(msg_archive_repo).
%%%-------------------------------------------------------------------
%%% @doc  永久消息存储仓库层
%%%
%%% 操作 msg_store（永久存储）和 msg_store_seq（序列号计数器）两张表。
%%% 由 msg_store_worker 在消息成功写入投递表后异步调用。
%%%
%%% == 设计原则 ==
%%% - 普通消息由 Worker 归档；OA direct 必须在原受理事务同步归档
%%% - conv_seq 是 per-conversation 单调递增序列号，用于客户端增量同步
%%% - 跨时间分区查重并验证完整消息身份，保持重试幂等
%%% - 不存储投递状态，只存消息内容
%%%
%%% == conv_key 格式 ==
%%% - C2C: "c2c:{min_uid}:{max_uid}"  （两端 uid 取小大排序，保证唯一性）
%%% - C2G: "c2g:{group_id}"
%%%
%%% == 调用示例 ==
%%% ```erlang
%%% % 归档一条消息
%%% ok = msg_archive_repo:archive(Row).
%%%
%%% % 游标拉取历史
%%% {ok, Rows} = msg_archive_repo:get_history(<<"c2c:100:200">>, 0, 50).
%%% ```
%%% @end
%%%-------------------------------------------------------------------

-include("log.hrl").

-define(TABLE, <<"public.msg_store">>).
-define(SEQ_TABLE, <<"public.msg_store_seq">>).

%% ==================== API ====================

-export([conv_key/3]).
-export([next_conv_seq/1]).
-export([archive/1, archive_tx/2]).
-export([get_history/3, get_history/4]).
-export([get_history_batch/2]).
-export([message_result/4]).
%% R02/D05：归档增量的安全推进边界与受控补扫
-export([safe_head/4, safe_history/6, backfill/6, backfill_range_limit/0]).

message_result(ConvKey, MsgId, Sender, MinSeq) ->
    elib_pg:query(
        <<
            "SELECT conv_seq FROM public.msg_store WHERE conv_key=$1 "
            "AND msg_id=$2 AND from_id=$3 AND conv_seq >= $4 LIMIT 1"
        >>,
        [ConvKey, MsgId, Sender, MinSeq]
    ).

%% ==================== API Functions ====================

%%-------------------------------------------------------------------
%% @doc  计算会话唯一键
%%
%% C2C: "c2c:{min_uid}:{max_uid}" — 两端 uid 排序确保同一会话唯一键一致
%% C2G: "c2g:{group_id}"
%%
%% @param ChatType <<"c2c">> | <<"c2g">>
%% @param FromId   发送者 user_id (integer)
%% @param ToIdOrGid C2C 时为接收者 user_id；C2G 时为 group_id
%% @end
%%-------------------------------------------------------------------
-spec conv_key(binary(), integer(), integer()) -> binary().
conv_key(<<"c2c">>, FromId, ToId) ->
    A = erlang:min(FromId, ToId),
    B = erlang:max(FromId, ToId),
    <<"c2c:", (integer_to_binary(A))/binary, ":", (integer_to_binary(B))/binary>>;
conv_key(<<"c2g">>, _FromId, Gid) ->
    <<"c2g:", (integer_to_binary(Gid))/binary>>.

%%-------------------------------------------------------------------
%% @doc  原子获取并递增 per-conversation 序列号
%%
%% 使用 INSERT ... ON CONFLICT DO UPDATE RETURNING seq 保证原子性。
%% 无论是否已存在记录，总是返回递增后的最新序列号。
%%
%% @param ConvKey 会话唯一键（见 conv_key/3）
%% @return {ok, SeqInteger} | {error, Reason}
%% @end
%%-------------------------------------------------------------------
%% @doc E2EE-2026-012 §7.2（Task 8/LT-03）：c2g 行在 staging 接受事务已预分配
%% conv_seq，archive 只搬运既定 seq；C2C 的旧 claimed NULL 读取迁移后权威值。
%% Preserve accepted seq; resolve a C2C worker's pre-migration claimed NULL copy.
-spec staged_or_next_seq(binary(), map()) -> {ok, integer()} | {error, term()}.
staged_or_next_seq(ConvKey, Row) ->
    case maps:get(<<"conv_seq">>, Row, undefined) of
        S when is_integer(S), S > 0 ->
            {ok, S};
        _ when map_get(<<"type">>, Row) =:= <<"c2c">> ->
            personal_message_order_repo:accepted_sequence(
                maps:get(<<"from_id">>, Row),
                maps:get(<<"to_id">>, Row),
                maps:get(<<"msg_id">>, Row)
            );
        _ ->
            next_conv_seq(ConvKey)
    end.

%%-------------------------------------------------------------------
-spec next_conv_seq(binary()) -> {ok, integer()} | {error, term()}.
next_conv_seq(ConvKey) ->
    Sql =
        <<"INSERT INTO ", ?SEQ_TABLE/binary, " (conv_key, seq) VALUES ($1, 1) ",
            "ON CONFLICT (conv_key) DO UPDATE ", "SET seq = msg_store_seq.seq + 1 ",
            "RETURNING seq">>,
    case elib_pg:query(Sql, [ConvKey]) of
        {ok, [Row | _]} ->
            {ok, maps:get(<<"seq">>, Row)};
        {ok, []} ->
            {error, no_seq_returned};
        {error, Reason} ->
            {error, Reason}
    end.

%%-------------------------------------------------------------------
%% @doc  归档一条消息到永久存储
%%
%% 接受 msg_store_staging 中的 Row（map），提取必要字段后写入 msg_store。
%% 受理序号保持不变；旧 C2G NULL 分配与归档 INSERT 同事务，重放沿用原值。
%%
%% 幂等性：msg_id + created_at 唯一约束，重复调用安全（返回 ok）。
%%
%% 注意：仅归档 c2c / c2g 类型；s2c / c2s 跳过（返回 ok）。
%%
%% @param Row staging 表中的消息 map
%% @return ok | {error, Reason}
%% @end
%%-------------------------------------------------------------------
-spec archive(map()) -> ok | {error, term()}.
archive(Row) ->
    TypeBin = maps:get(<<"type">>, Row),
    case build_archive_data(TypeBin, Row) of
        skip ->
            ok;
        {error, Reason} ->
            {error, Reason};
        {ok, ConvKey, Data} when TypeBin =:= <<"c2g">> ->
            archive_c2g(ConvKey, Row, Data);
        {ok, ConvKey, Data} ->
            case staged_or_next_seq(ConvKey, Row) of
                {ok, Seq} ->
                    GenId = elib_tsid:generate(msg_store),
                    FinalData = Data#{id => GenId, conv_seq => Seq},
                    case elib_pg:insert(?TABLE, FinalData) of
                        {ok, _} ->
                            ok;
                        %% Q06（AC11 真库注入修复）：真实 epgsql 唯一冲突形状
                        %% {error, {error, Severity, <<"23505">>, unique_violation, Msg, Extra}}
                        %% （与 msg_store_repo stage 同款修正；旧三层形状仅存在于 mock）
                        {error,
                            {error, _Severity, <<"23505">>, unique_violation, _Msg, Extra} = Reason} ->
                            archive_collision(TypeBin, FinalData, Extra, Reason);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% OA direct retention shares its caller's accepted-message transaction. The caller
%% already holds counter -> msg_c2c identity locks; never allocate another sequence.
-spec archive_tx(pid(), map()) -> ok.
archive_tx(Conn, #{<<"type">> := <<"c2c">>, <<"conv_seq">> := Seq} = Row) when
    is_integer(Seq), Seq > 0
->
    {ok, _Key, Data0} = build_archive_data(<<"c2c">>, Row),
    Data = Data0#{conv_seq => Seq, expire_at => maps:get(<<"expire_at">>, Row, null)},
    MsgId = maps:get(msg_id, Data),
    _ = archive_query(
        Conn, <<"SELECT pg_advisory_xact_lock(hashtextextended('msg_c2c:'||$1,0))">>, [MsgId]
    ),
    case archive_query(Conn, <<"SELECT id FROM public.msg_store WHERE msg_id=$1">>, [MsgId]) of
        [] -> insert_caller_archive(Conn, Data);
        [_] -> verify_caller_archive(Conn, Data);
        _ -> throw({rollback, archive_msg_id_conflict})
    end;
archive_tx(_Conn, _Row) ->
    throw({rollback, invalid_accepted_archive}).

insert_caller_archive(Conn, Data) ->
    {Sql, Params} = elib_pg_sql:insert(?TABLE, Data#{id => elib_tsid:generate(msg_store)}),
    %% DO NOTHING avoids an aborted transaction for the exact time-partition key.
    %% Unrelated 23505 (including a trigger) propagates and rolls back the caller.
    _ = archive_query(
        Conn, <<Sql/binary, " ON CONFLICT(msg_id,created_at) DO NOTHING RETURNING id">>, Params
    ),
    verify_caller_archive(Conn, Data).

verify_caller_archive(Conn, Data) ->
    case same_archive(Conn, Data) of
        {ok, [_]} ->
            case
                archive_query(
                    Conn,
                    <<
                        "SELECT id FROM public.msg_store WHERE msg_id=$1 AND expire_at IS NOT DISTINCT FROM "
                        "$2::timestamptz"
                    >>,
                    [maps:get(msg_id, Data), maps:get(expire_at, Data)]
                )
            of
                [_] -> ok;
                _ -> throw({rollback, archive_msg_id_conflict})
            end;
        {ok, _} ->
            throw({rollback, archive_msg_id_conflict});
        {error, Reason} ->
            throw({rollback, Reason})
    end.

%% Counter -> msg_c2g message namespace matches the formal dedup trigger.
%% Never take group/workspace locks or write formal rows while holding these locks.
archive_c2g(Key, Row, Data) ->
    case elib_pg:with_tx(fun(Conn) -> archive_c2g_tx(Conn, Key, Row, Data) end) of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

archive_c2g_tx(Conn, Key, Row, Data) ->
    archive_execute(
        Conn,
        <<"INSERT INTO public.msg_store_seq(conv_key,seq) VALUES($1,0) ON CONFLICT(conv_key) DO NOTHING">>,
        [Key]
    ),
    [#{<<"seq">> := Previous}] = archive_query(
        Conn, <<"SELECT seq FROM public.msg_store_seq WHERE conv_key=$1 FOR UPDATE">>, [Key]
    ),
    MsgId = maps:get(msg_id, Data),
    _ = archive_query(
        Conn, <<"SELECT pg_advisory_xact_lock(hashtextextended('msg_c2g:'||$1,0))">>, [MsgId]
    ),
    Existing = archive_query(Conn, <<"SELECT conv_seq FROM public.msg_store WHERE msg_id=$1">>, [
        MsgId
    ]),
    finish_c2g_archive(Conn, Key, Row, Data, Previous, Existing).

finish_c2g_archive(Conn, Key, Row, Data, Previous, Existing) ->
    case Existing of
        [] ->
            Seq = c2g_archive_sequence(Conn, Row, Data, Previous + 1),
            FinalData = Data#{id => elib_tsid:generate(msg_store), conv_seq => Seq},
            {Sql, Params} = elib_pg_sql:insert(?TABLE, FinalData),
            archive_execute(Conn, Sql, Params),
            archive_execute(Conn, <<"UPDATE public.msg_store_seq SET seq=$2 WHERE conv_key=$1">>, [
                Key, max(Previous, Seq)
            ]),
            ok;
        [#{<<"conv_seq">> := ExistingSeq}] ->
            Seq = c2g_archive_sequence(Conn, Row, Data, ExistingSeq),
            case same_archive(Conn, Data#{conv_seq => Seq}) of
                {ok, [_]} ->
                    case Seq > Previous of
                        true ->
                            archive_execute(
                                Conn,
                                <<"UPDATE public.msg_store_seq SET seq=$2 WHERE conv_key=$1">>,
                                [Key, Seq]
                            );
                        false ->
                            ok
                    end;
                {ok, _} ->
                    throw({rollback, archive_replay_conflict});
                {error, Reason} ->
                    throw({rollback, Reason})
            end;
        _ ->
            throw({rollback, archive_replay_conflict})
    end.

c2g_archive_sequence(Conn, Row, Data, Default) ->
    Rows = archive_query(
        Conn,
        <<
            "SELECT from_id,to_id AS group_id,conv_seq FROM public.msg_store_staging WHERE type='c2g' AND msg_id=$1 "
            "UNION ALL SELECT from_id,to_gid AS group_id,"
            "conv_seq FROM public.msg_c2g_recipient_snapshot WHERE msg_id=$1"
        >>,
        [maps:get(msg_id, Data)]
    ),
    case
        lists:all(
            fun(R) ->
                maps:get(<<"from_id">>, R) =:= maps:get(from_id, Data) andalso
                    maps:get(<<"group_id">>, R) =:= maps:get(group_id, Data)
            end,
            Rows
        )
    of
        true -> ok;
        false -> throw({rollback, archive_acceptance_conflict})
    end,
    Seqs = lists:usort([
        S
     || S <- [
            maps:get(<<"conv_seq">>, Row, null) | [maps:get(<<"conv_seq">>, R, null) || R <- Rows]
        ],
        S =/= null,
        S =/= undefined
    ]),
    case Seqs of
        [] -> Default;
        [S] when is_integer(S), S > 0 -> S;
        _ -> throw({rollback, archive_acceptance_conflict})
    end.

archive_query(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} -> Rows;
        {error, Reason} -> throw({rollback, Reason})
    end.
archive_execute(Conn, Sql, Params) ->
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> throw({rollback, Reason})
    end.

%% C2C collisions succeed only for the complete accepted message, never just a code.
archive_collision(<<"c2c">>, Data, _Extra, Reason) ->
    %% Timescale chunk constraint names differ; prove the actual row, not its name.
    same_c2c_archive(Data, Reason);
archive_collision(_Other, _Data, _Extra, Reason) ->
    {error, Reason}.

same_c2c_archive(Data, OriginalError) ->
    case same_archive(undefined, Data) of
        {ok, [_]} -> ok;
        {ok, _} -> {error, OriginalError};
        {error, Reason} -> {error, Reason}
    end.

same_archive(Conn, Data) ->
    Sql = <<
        "SELECT id FROM public.msg_store WHERE msg_id=$1 AND created_at=$2::timestamptz "
        "AND chat_type=$3 AND conv_key=$4 AND from_id=$5 "
        "AND to_id IS NOT DISTINCT FROM $6::bigint "
        "AND group_id IS NOT DISTINCT FROM $7::bigint AND conv_seq=$8 AND msg_type=$9 "
        "AND payload::jsonb IS NOT DISTINCT FROM $10::jsonb "
        "AND e2ee IS NOT DISTINCT FROM $11::jsonb "
        "AND sender_did IS NOT DISTINCT FROM $12::varchar "
        "AND server_ts IS NOT DISTINCT FROM $13::timestamptz"
    >>,
    Keys = [
        msg_id,
        created_at,
        chat_type,
        conv_key,
        from_id,
        to_id,
        group_id,
        conv_seq,
        msg_type,
        payload,
        e2ee,
        sender_did,
        server_ts
    ],
    Params = [archive_parameter(K, maps:get(K, Data)) || K <- Keys],
    case Conn of
        undefined -> elib_pg:query(Sql, Params);
        _ -> elib_pg:query(Conn, Sql, Params)
    end.

archive_parameter(K, Value) when (K =:= payload orelse K =:= e2ee), is_map(Value) ->
    jsone:encode(Value);
archive_parameter(_K, Value) ->
    Value.

%%-------------------------------------------------------------------
%% @doc  游标查询：按 conv_seq 升序拉取历史消息
%%
%% 客户端保存 last_conv_seq，每次请求传入，服务端返回比它大的消息。
%% 首次同步传入 0 即可获取全部历史（受 Limit 限制）。
%%
%% @param ConvKey  会话键（见 conv_key/3）
%% @param AfterSeq 游标值（上次最后一条消息的 conv_seq）
%% @param Limit    最多返回条数
%% @end
%%-------------------------------------------------------------------
-spec get_history(binary(), integer(), pos_integer()) ->
    {ok, list(map())} | {error, term()}.
get_history(ConvKey, AfterSeq, Limit) ->
    get_history(ConvKey, AfterSeq, Limit, asc).

%%-------------------------------------------------------------------
%% @doc  游标查询（带排序方向）
%%
%% @param Order asc（正序，用于增量拉取）| desc（倒序，用于向上翻页）
%% @end
%%-------------------------------------------------------------------
-spec get_history(binary(), integer(), pos_integer(), asc | desc) ->
    {ok, list(map())} | {error, term()}.
get_history(ConvKey, AfterSeq, Limit, Order) ->
    {Cmp, OrderBin} =
        case Order of
            desc -> {<<" < ">>, <<"DESC">>};
            _ -> {<<" > ">>, <<"ASC">>}
        end,
    Sql =
        <<"SELECT msg_id, chat_type, conv_seq, msg_type, from_id, to_id, group_id, ",
            "e2ee, sender_did, payload, created_at, server_ts ", "FROM ", ?TABLE/binary,
            " WHERE conv_key = $1 AND conv_seq", Cmp/binary, "$2 ", "ORDER BY conv_seq ",
            OrderBin/binary, " LIMIT $3">>,
    elib_pg:query(Sql, [ConvKey, AfterSeq, Limit]).

%%-------------------------------------------------------------------
%% @doc  批量游标查询：一次 LATERAL 拉取多个会话的增量消息(正序)
%%
%% 替代逐会话 N 次 get_history 往返。每个会话仍按各自 (conv_key, after_seq)
%% 游标取最多 Limit 条，复用索引 i_msg_store_conv_seq。输出多含 conv_key
%% 字段供上层按会话分组；Limit 服务端夹紧后内联(1..100 整数，无注入)。
%%
%% @param Cursors [{ConvKey :: binary(), AfterSeq :: integer()}]
%% @end
%%-------------------------------------------------------------------
-spec get_history_batch([{binary(), integer()}], pos_integer()) ->
    {ok, list(map())} | {error, term()}.
get_history_batch([], _Limit) ->
    {ok, []};
get_history_batch(Cursors, Limit) ->
    LimitInt = erlang:min(erlang:max(Limit, 1), 100),
    {ValuesSql, Params} = build_cursor_values(Cursors),
    Sql = <<
        "SELECT c.conv_key AS conv_key, h.msg_id, h.chat_type, h.conv_seq, h.msg_type, ",
        "h.from_id, h.to_id, h.group_id, h.e2ee, h.sender_did, h.payload, h.created_at, h.server_ts ",
        "FROM (VALUES ",
        ValuesSql/binary,
        ") AS c(conv_key, after_seq) ",
        "CROSS JOIN LATERAL ("
        "SELECT msg_id, chat_type, conv_seq, msg_type, from_id, to_id, group_id, "
        "e2ee, sender_did, payload, created_at, server_ts "
        "FROM ",
        ?TABLE/binary,
        " m "
        "WHERE m.conv_key = c.conv_key AND m.conv_seq > c.after_seq "
        "ORDER BY m.conv_seq ASC LIMIT ",
        (integer_to_binary(LimitInt))/binary,
        ") h "
        "ORDER BY c.conv_key, h.conv_seq ASC"
    >>,
    elib_pg:query(Sql, Params).

%% ==================== Internal Functions ====================

%% @private 构建 VALUES 占位符与参数：[{K,S}] → {"($1::text,$2::bigint),...", [K,S,...]}
-spec build_cursor_values([{binary(), integer()}]) -> {binary(), list()}.
build_cursor_values(Cursors) ->
    Indexed = lists:zip(lists:seq(1, length(Cursors)), Cursors),
    Parts = [
        begin
            P1 = integer_to_binary(2 * I - 1),
            P2 = integer_to_binary(2 * I),
            <<"($", P1/binary, "::text,$", P2/binary, "::bigint)">>
        end
     || {I, _} <- Indexed
    ],
    Params = lists:append([[K, S] || {_, {K, S}} <- Indexed]),
    {elib_cnv:implode(<<",">>, Parts), Params}.

%% ===================================================================
%% R02 / D05（IMBOY-TIMEZONE-REPAIR-V1）：归档增量的安全推进边界
%% ===================================================================

%% 补扫单次允许的最大序号跨度：(AfterSeq, SafeHead] 超过此值须分批。
-define(BACKFILL_RANGE_LIMIT, 1000).

-spec backfill_range_limit() -> pos_integer().
backfill_range_limit() ->
    ?BACKFILL_RANGE_LIMIT.

%%-------------------------------------------------------------------
%% @doc D05：会话安全推进边界 B(conv_key)。
%%
%% B = min( max(conv_seq) over 正式表(msg_store + msg_c2c/msg_c2g),
%%          (min(seq) over msg_store_staging 非终态行) - 1 )
%% 无非终态 staging 行时 B = 正式表 max。
%%
%% 终态失败（mark_terminal，processed_at 置位）、授权过滤、保留清理形成的
%% seq **排除在阻塞集外**（合法空洞不阻塞边界）——非终态判定即
%% processed_at IS NULL（claim 租约/mark_failed 重试均保持 NULL）。
%%
%% 单事务内计算：B 与其调用方 safe_history 的行查询共用同一连接快照序列，
%% B 只可能偏保守（read_committed 下晚到的归档使 B 变大不影响已返回值）。
%%-------------------------------------------------------------------
-spec safe_head(binary(), binary(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
safe_head(ConvKey, ChatType, PeerA, PeerB) ->
    elib_pg:with_tx(fun(Conn) -> safe_head_tx(Conn, ConvKey, ChatType, PeerA, PeerB) end).

safe_head_tx(Conn, ConvKey, ChatType, PeerA, PeerB) ->
    {FormalSql, StagingSql, Params} = head_scope(ConvKey, ChatType, PeerA, PeerB),
    Sql =
        <<"SELECT greatest(", FormalSql/binary, ") AS formal_max, ",
            "coalesce((SELECT min(conv_seq) FROM ", StagingSql/binary, "), 0) AS staging_min">>,
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [#{<<"formal_max">> := FormalMax, <<"staging_min">> := StagingMin}]} ->
            {ok, clamp_head(FormalMax, StagingMin)};
        {ok, _} ->
            {error, safe_head_shape};
        {error, Reason} ->
            {error, Reason}
    end.

%% B = min(formal_max, staging_min - 1)；staging_min=0 表示无非终态行 → B = formal_max。
clamp_head(FormalMax, StagingMin) when
    is_integer(FormalMax), FormalMax >= 0, is_integer(StagingMin), StagingMin >= 0
->
    case StagingMin of
        0 -> FormalMax;
        _ -> erlang:min(FormalMax, StagingMin - 1)
    end.

%% 正式表与 staging 的 per-chat_type 范围 SQL 片段与参数。
%% C2C 正式：msg_store(conv_key) + msg_c2c(min/max uid 对)；staging：type='c2c' 同 uid 对。
%% C2G 正式：msg_store(conv_key) + msg_c2g(to_id=gid)；staging：type='c2g' 同 gid。
head_scope(<<"c2c:", _/binary>> = ConvKey, <<"c2c">>, PeerA, PeerB) ->
    FormalSql =
        <<
            "coalesce((SELECT max(conv_seq) FROM public.msg_store WHERE chat_type='c2c' AND conv_key=$1),0), "
            "coalesce((SELECT max(conv_seq) FROM public.msg_c2c "
            "WHERE least(from_id,to_id)=$2 AND greatest(from_id,to_id)=$3),0)"
        >>,
    StagingSql =
        <<
            "public.msg_store_staging WHERE type='c2c' "
            "AND least(from_id,to_id)=$2 AND greatest(from_id,to_id)=$3 "
            "AND conv_seq IS NOT NULL AND processed_at IS NULL"
        >>,
    {FormalSql, StagingSql, [ConvKey, erlang:min(PeerA, PeerB), erlang:max(PeerA, PeerB)]};
head_scope(<<"c2g:", _/binary>> = ConvKey, <<"c2g">>, Gid, _PeerB) ->
    %% msg_c2g 正文表无 conv_seq 列（权威 seq 在接受事务分配、归档入 msg_store，
    %% timeline 行同源搬运）；C2G 正式表 max 以 msg_store 为准。
    FormalSql =
        <<"coalesce((SELECT max(conv_seq) FROM public.msg_store WHERE chat_type='c2g' AND conv_key=$1),0)">>,
    StagingSql =
        <<
            "public.msg_store_staging WHERE type='c2g' AND to_id=$2 "
            "AND conv_seq IS NOT NULL AND processed_at IS NULL"
        >>,
    {FormalSql, StagingSql, [ConvKey, Gid]};
head_scope(_ConvKey, _ChatType, _A, _B) ->
    erlang:error(badarg).

%%-------------------------------------------------------------------
%% @doc D05/R02：一致快照下的增量历史——行查询与 B 边界同一事务。
%%
%% 返回 {ok, Rows, SafeHead, TerminalGaps}：
%%   Rows         按 conv_seq 升序的正式表行（conv_seq > AfterSeq，最多 Limit）
%%   SafeHead     B 边界；调用方 next_seq 不得超过
%%   TerminalGaps [{Seq, Reason}] 终态失败 staging 行形成的合法空洞
%%                （seq ∈ (AfterSeq, B]，未出现在正式表、staging 已终态）
%% 授权过滤与保留清理空洞不在此列：前者由调用方授权下界（MinSeq）表达，
%% 后者被清理的 seq 既不在正式表也不在 staging，天然不参与 B 与行集。
%%-------------------------------------------------------------------
-spec safe_history(binary(), integer(), pos_integer(), binary(), integer(), integer()) ->
    {ok, list(map()), non_neg_integer(), [{integer(), binary()}]} | {error, term()}.
safe_history(ConvKey, AfterSeq, Limit, ChatType, PeerA, PeerB) ->
    elib_pg:with_tx(fun(Conn) ->
        {ok, SafeHead} = safe_head_tx(Conn, ConvKey, ChatType, PeerA, PeerB),
        Rows = history_rows_tx(Conn, ConvKey, AfterSeq, Limit),
        TerminalGaps = terminal_gap_tx(Conn, ConvKey, ChatType, PeerA, PeerB, AfterSeq, SafeHead),
        {ok, Rows, SafeHead, TerminalGaps}
    end).

history_rows_tx(Conn, ConvKey, AfterSeq, Limit) ->
    Sql =
        <<
            "SELECT msg_id, chat_type, conv_seq, msg_type, from_id, to_id, group_id, "
            "e2ee, sender_did, payload, created_at, server_ts "
            "FROM public.msg_store WHERE conv_key = $1 AND conv_seq > $2 "
            "ORDER BY conv_seq ASC LIMIT $3"
        >>,
    case elib_pg:query(Conn, Sql, [ConvKey, AfterSeq, Limit]) of
        {ok, Rows} -> Rows;
        {error, Reason} -> throw({rollback, Reason})
    end.

%% 终态失败空洞：staging 已终态（processed_at NOT NULL）、seq 在窗口内、
%% 且该 seq 未被正式表占用（正式表占用的不算空洞）。
terminal_gap_tx(Conn, ConvKey, ChatType, PeerA, PeerB, AfterSeq, SafeHead) ->
    {_F, _S, Params0} = head_scope(ConvKey, ChatType, PeerA, PeerB),
    {FromPh, ToPh} = gap_window_ph(ChatType),
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT conv_seq FROM public.msg_store_staging s WHERE ",
                (staging_scope_sql(ChatType))/binary,
                " AND s.conv_seq > ",
                FromPh/binary,
                " AND s.conv_seq <= ",
                ToPh/binary,
                " AND s.processed_at IS NOT NULL "
                "AND NOT EXISTS (SELECT 1 FROM public.msg_store m "
                "WHERE m.conv_key = $1 AND m.conv_seq = s.conv_seq) "
                "ORDER BY s.conv_seq LIMIT 200"
            >>,
            terminal_gap_params(ChatType, Params0, AfterSeq, SafeHead)
        )
    of
        {ok, Rows} ->
            [{maps:get(<<"conv_seq">>, R), <<"terminal_failure">>} || R <- Rows];
        {error, Reason} ->
            throw({rollback, Reason})
    end.

staging_scope_sql(<<"c2c">>) ->
    <<"s.type='c2c' AND least(s.from_id,s.to_id)=$2 AND greatest(s.from_id,s.to_id)=$3">>;
staging_scope_sql(<<"c2g">>) ->
    <<"s.type='c2g' AND s.to_id=$2">>.

terminal_gap_params(<<"c2c">>, [ConvKey, A, B], AfterSeq, SafeHead) ->
    [ConvKey, A, B, AfterSeq, SafeHead];
terminal_gap_params(<<"c2g">>, [ConvKey, Gid], AfterSeq, SafeHead) ->
    [ConvKey, Gid, AfterSeq, SafeHead].

%% 空洞窗口参数号随 staging 范围占位符数量而定：c2c 范围占 $2/$3 → 窗口
%% $4/$5；c2g 范围只占 $2 → 窗口 $3/$4。R23-A2 修复（c 类真缺陷）：原实现
%% 硬编码 $4/$5，c2g 绑定只有 4 参数 → $3 声明未用、$5 越界，PG 42P18
%% indeterminate_datatype，所有 c2g history 一致快照查询必然回滚。
gap_window_ph(<<"c2c">>) -> {<<"$4">>, <<"$5">>};
gap_window_ph(<<"c2g">>) -> {<<"$3">>, <<"$4">>}.

%%-------------------------------------------------------------------
%% @doc D05/R02：晚到补齐的有界范围补扫。
%%
%% 对已过冲（next_seq 越过晚到低序号）的客户端，在 (AfterSeq, SafeHead]
%% 显式重扫已归档行，按 msg_id 去重。范围跨度受 ?BACKFILL_RANGE_LIMIT
%% 约束（超出要求分批：终点=SafeHead、批次=Limit、恢复位置=返回 next_seq）。
%% 授权/保留/撤回约束由调用方（conv_key 授权校验 + 行为现存正式行）承担。
%%-------------------------------------------------------------------
-spec backfill(binary(), integer(), pos_integer(), binary(), integer(), integer()) ->
    {ok, list(map()), non_neg_integer(), boolean()} | {error, term()}.
backfill(ConvKey, AfterSeq, Limit, ChatType, PeerA, PeerB) when
    is_integer(AfterSeq), AfterSeq >= 0, is_integer(Limit), Limit >= 1
->
    elib_pg:with_tx(fun(Conn) ->
        {ok, SafeHead} = safe_head_tx(Conn, ConvKey, ChatType, PeerA, PeerB),
        case SafeHead - AfterSeq > ?BACKFILL_RANGE_LIMIT of
            true ->
                {error, backfill_range_exceeded};
            false ->
                Rows = history_rows_tx(Conn, ConvKey, AfterSeq, Limit),
                NextSeq =
                    case Rows of
                        [] -> AfterSeq;
                        _ -> maps:get(<<"conv_seq">>, lists:last(Rows))
                    end,
                %% has_more：本批取满且仍 < SafeHead（下一批从 NextSeq 续扫）
                HasMore = NextSeq < SafeHead andalso length(Rows) >= Limit,
                {ok, Rows, erlang:max(NextSeq, AfterSeq), HasMore}
        end
    end);
backfill(_ConvKey, _AfterSeq, _Limit, _ChatType, _A, _B) ->
    {error, badarg}.

%% @private 构建归档数据，返回 {ok, ConvKey, DataMap} | skip | {error, Reason}
build_archive_data(<<"c2c">>, Row) ->
    FromId = maps:get(<<"from_id">>, Row),
    ToId = maps:get(<<"to_id">>, Row, null),
    MsgId = maps:get(<<"msg_id">>, Row),
    MsgType = maps:get(<<"msg_type">>, Row, <<>>),
    E2EE = maps:get(<<"e2ee">>, Row, null),
    SenderDid = maps:get(<<"sender_did">>, Row, null),
    Payload = maps:get(<<"payload">>, Row),
    CreatedAt = maps:get(<<"created_at">>, Row),
    ServerTs = maps:get(<<"server_ts">>, Row, CreatedAt),
    %% R03/D04：首次接受时间随 staging 行/正式行搬运；旧行缺失为 null（低可信）。
    AcceptedAt = maps:get(<<"accepted_at">>, Row, null),
    ConvKey = conv_key(<<"c2c">>, FromId, ToId),
    Data = #{
        chat_type => <<"c2c">>,
        conv_key => ConvKey,
        msg_id => MsgId,
        msg_type => MsgType,
        from_id => FromId,
        to_id => ToId,
        group_id => null,
        e2ee => E2EE,
        sender_did => SenderDid,
        payload => Payload,
        created_at => CreatedAt,
        server_ts => ServerTs,
        accepted_at => AcceptedAt,
        %% R05-A：自毁绝对期限随行搬运（旧行/无自毁缺失为 null，不伪造）。
        expire_at => maps:get(<<"expire_at">>, Row, null)
    },
    {ok, ConvKey, Data};
build_archive_data(<<"c2g">>, Row) ->
    FromId = maps:get(<<"from_id">>, Row),
    MsgId = maps:get(<<"msg_id">>, Row),
    MsgType = maps:get(<<"msg_type">>, Row, <<>>),
    E2EE = maps:get(<<"e2ee">>, Row, null),
    SenderDid = maps:get(<<"sender_did">>, Row, null),
    Payload = maps:get(<<"payload">>, Row),
    CreatedAt = maps:get(<<"created_at">>, Row),
    ServerTs = maps:get(<<"server_ts">>, Row, CreatedAt),
    %% C2G 的 group_id 存在 payload 的 <<"to">> 字段（字符串格式）
    case safe_decode_group_id(Payload) of
        {ok, Gid} ->
            ConvKey = conv_key(<<"c2g">>, FromId, Gid),
            Data = #{
                chat_type => <<"c2g">>,
                conv_key => ConvKey,
                msg_id => MsgId,
                msg_type => MsgType,
                from_id => FromId,
                to_id => null,
                group_id => Gid,
                e2ee => E2EE,
                sender_did => SenderDid,
                payload => Payload,
                created_at => CreatedAt,
                server_ts => ServerTs,
                %% R03/D04：首次接受时间随 staging 行搬运；旧行缺失为 null。
                accepted_at => maps:get(<<"accepted_at">>, Row, null),
                %% R05-A：自毁绝对期限随行搬运（缺失 null，不伪造）。
                expire_at => maps:get(<<"expire_at">>, Row, null)
            },
            {ok, ConvKey, Data};
        {error, Reason} ->
            {error, {decode_group_id_failed, Reason}}
    end;
build_archive_data(_OtherType, _Row) ->
    %% s2c / c2s 不归档
    skip.

%% @private 从 payload JSON 解析 group_id
safe_decode_group_id(Payload) ->
    try
        PayloadMap = jsone:decode(Payload, [{object_format, map}]),
        GidEnc = maps:get(<<"to">>, PayloadMap),
        Gid = ec_cnv:to_integer(GidEnc),
        {ok, Gid}
    catch
        _:Reason ->
            {error, Reason}
    end.
