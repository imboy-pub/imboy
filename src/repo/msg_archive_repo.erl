-module(msg_archive_repo).
%%%-------------------------------------------------------------------
%%% @doc  永久消息存储仓库层
%%%
%%% 操作 msg_store（永久存储）和 msg_store_seq（序列号计数器）两张表。
%%% 由 msg_store_worker 在消息成功写入投递表后异步调用。
%%%
%%% == 设计原则 ==
%%% - 写入由 Worker 负责，不在消息发送热路径上
%%% - conv_seq 是 per-conversation 单调递增序列号，用于客户端增量同步
%%% - msg_id 唯一约束保证幂等（Worker 重试安全）
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
-export([archive/1]).
-export([get_history/3, get_history/4]).
-export([get_history_batch/2]).

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
%% 内部调用 next_conv_seq/1 获取序列号。
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
        {ok, ConvKey, Data} ->
            case next_conv_seq(ConvKey) of
                {ok, Seq} ->
                    GenId = elib_tsid:generate(msg_store),
                    FinalData = Data#{id => GenId, conv_seq => Seq},
                    case elib_pg:insert(?TABLE, FinalData) of
                        {ok, _} ->
                            ok;
                        {error, {error, {error, <<"23505">>, unique_violation, _, _}}} ->
                            %% 幂等：消息已归档（Worker 重试场景），视为成功
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

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
        server_ts => ServerTs
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
                server_ts => ServerTs
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
