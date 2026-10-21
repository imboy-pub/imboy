-module(talk_record_segment_repo).
-moduledoc "面聊转写片段仓储（T15：当前版本正序分页 + 人工修订 CAS；迁移 00000211）。".
%%%
% talk_record_segment 数据仓库层（T15 原文域）。
%
% 受控历史设计依据（任务卡 T15 交付 1「二选一」裁决）：
%   * 选择「人工修订写 origin=human（同 seq 行内版本化）」：
%     - 209 冻结的 UNIQUE(record_id, transcript_version, seq) 使「新 revision
%       行」（同 seq 第二行）在既有 schema 不可实现，且 T15 授权路径不含
%       新迁移；
%     - 契约 §2 transcript 表注释明确「人工修订由 segment revision 与
%       record.content_revision 追踪」——行内 revision 即契约指定机制；
%     - §4 GET segments 要求「稳定 ID」：行内版本化使 segment_id 恒定，
%       If-Match segment revision 语义自洽（新行方案每次编辑换 id，
%       If-Match 旧 id 将 404）。
%   * 模型原版保留：origin=model 且未编辑的行即完整模型输出；首次人工
%     编辑后 origin=human + edited_by 署名，行内容显式标记含人工修订。
%     「模型原文本的完整历史快照」需独立历史表，超出本卡 schema 授权，
%     已在 T15 报告登记为主控制器后续决策项。
%   * 模型重跑只创建新 transcript 版本，本仓储不提供覆盖人工行的路径
%     （§8「人工修订不得被默认覆盖」——写入口只有 update_human_tx）。
%%%

-export([find_tx/3, page_tx/4, update_human_tx/5, count_ids_tx/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SEGMENT_COLUMNS,
    <<"s.id,s.record_id,s.transcript_version,s.seq,s.speaker_id,",
        "s.start_ms,s.end_ms,s.text,s.confidence,s.is_final,s.origin,",
        "s.revision,s.edited_by,s.created_at,s.updated_at">>
).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 按复合主键取 segment（record_id + segment_id 同时匹配，§8；
%% 跨记录子 ID 一律 not_found → 404，不泄露存在性）。
-spec find_tx(any(), integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, RecordId, SegmentId) ->
    Sql =
        <<"SELECT ", ?SEGMENT_COLUMNS/binary, " FROM talk_record_segment s",
            " WHERE s.record_id = $1 AND s.id = $2">>,
    case elib_pg:query(Conn, Sql, [RecordId, SegmentId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 当前版本正序分页（start_ms/id ASC，§4；Filters：
%% #{speaker_id => integer() | undefined, q => binary() | <<>>,
%%    cursor => {StartMs, Id} | undefined}）。
%% q 为已限长原文子串（ILIKE + escape_like + ::text cast 消歧；命中高亮
%% 位置由 logic 层计算）。占位符随可选片段动态递增。
-spec page_tx(any(), integer(), integer(), map()) -> {ok, [map()]} | {error, term()}.
page_tx(Conn, RecordId, Version, Filters) ->
    Params0 = [RecordId, Version, maps:get(limit, Filters, 20)],
    {SpeakerFrag, Params1, N1} = speaker_frag(maps:get(speaker_id, Filters, undefined), 4, Params0),
    {QFrag, Params2, N2} = q_frag(maps:get(q, Filters, <<>>), N1, Params1),
    {CursorFrag, Params3, _N3} = cursor_frag(maps:get(cursor, Filters, undefined), N2, Params2),
    Sql =
        <<"SELECT ", ?SEGMENT_COLUMNS/binary, " FROM talk_record_segment s",
            " WHERE s.record_id = $1 AND s.transcript_version = $2", SpeakerFrag/binary,
            QFrag/binary, CursorFrag/binary, " ORDER BY s.start_ms ASC, s.id ASC LIMIT $3">>,
    elib_pg:query(Conn, Sql, Params3).

%% @doc 人工修订 CAS（text/speaker_id；origin := 'human'、edited_by 署名、
%% revision+1）。命中 0 行由上层区分 not_found / precondition_failed。
%% 设计依据见模块头：origin=model 行被人工编辑后转为 origin=human 行内
%% 版本化；本仓储不提供任何改写 origin 回 model 的入口。
-spec update_human_tx(any(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, not_found | term()}.
update_human_tx(Conn, RecordId, SegmentId, ExpectedRevision, Sets) ->
    Sets1 = [{K, maps:get(K, Sets)} || K <- [<<"text">>, <<"speaker_id">>], maps:is_key(K, Sets)],
    SetSql = join_set_clause(Sets1, 1),
    Base = length(Sets1) + 1,
    Sql =
        <<"UPDATE talk_record_segment s SET ", SetSql/binary, ", origin = 'human', edited_by = $",
            (integer_to_binary(Base))/binary, ", revision = revision + 1", ", updated_at = ", "$",
            (integer_to_binary(Base + 1))/binary, " WHERE s.record_id = $",
            (integer_to_binary(Base + 2))/binary, " AND s.id = $",
            (integer_to_binary(Base + 3))/binary, " AND s.revision = $",
            (integer_to_binary(Base + 4))/binary, " RETURNING ", ?SEGMENT_COLUMNS/binary>>,
    Now = elib_dt:now(),
    Params =
        [V || {_, V} <- Sets1] ++
            [maps:get(actor_id, Sets), Now, RecordId, SegmentId, ExpectedRevision],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 摘要 segment_ids 同记录校验支撑（§2：decision/action 引用服务端
%% 校验同记录）：统计 SegmentIds 中属于 (RecordId, Version) 的条数。
%% Ids 以字面量整数内联（来自已 parse_pos_int 校验的值，非用户原文）。
-spec count_ids_tx(any(), integer(), integer(), [integer()]) ->
    {ok, non_neg_integer()} | {error, term()}.
count_ids_tx(_Conn, _RecordId, _Version, []) ->
    {ok, 0};
count_ids_tx(Conn, RecordId, Version, SegmentIds) ->
    Ids = [integer_to_binary(I) || I <- SegmentIds],
    IdList = list_to_binary(join_binary(Ids, <<",">>)),
    Sql =
        <<"SELECT count(*) AS n FROM talk_record_segment s",
            " WHERE s.record_id = $1 AND s.transcript_version = $2", " AND s.id IN (",
            IdList/binary, ")">>,
    case elib_pg:query(Conn, Sql, [RecordId, Version]) of
        {ok, [#{<<"n">> := N}]} -> {ok, N};
        {ok, [_]} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal：过滤片段（占位符随片段出现动态递增；$1=record $2=version $3=limit）
%% ===================================================================

-spec speaker_frag(integer() | undefined, pos_integer(), [term()]) ->
    {binary(), [term()], pos_integer()}.
speaker_frag(undefined, N, Params) ->
    {<<>>, Params, N};
speaker_frag(SpeakerId, N, Params) ->
    {<<" AND s.speaker_id = $", (integer_to_binary(N))/binary>>, Params ++ [SpeakerId], N + 1}.

-spec q_frag(binary(), pos_integer(), [term()]) -> {binary(), [term()], pos_integer()}.
q_frag(<<>>, N, Params) ->
    {<<>>, Params, N};
q_frag(Q, N, Params) ->
    Pattern = <<"%", (elib_pg:escape_like(Q))/binary, "%">>,
    {
        <<" AND s.text ILIKE $", (integer_to_binary(N))/binary, "::text">>,
        Params ++ [Pattern],
        N + 1
    }.

-spec cursor_frag({integer(), integer()} | undefined, pos_integer(), [term()]) ->
    {binary(), [term()], pos_integer()}.
cursor_frag(undefined, N, Params) ->
    {<<>>, Params, N};
cursor_frag({LastStartMs, LastId}, N, Params) ->
    C1 = <<"$", (integer_to_binary(N))/binary>>,
    C2 = <<"$", (integer_to_binary(N + 1))/binary>>,
    {
        <<" AND (s.start_ms, s.id) > (", C1/binary, ", ", C2/binary, ")">>,
        Params ++ [LastStartMs, LastId],
        N + 2
    }.

-spec join_set_clause([{binary(), term()}], pos_integer()) -> binary().
join_set_clause(Sets, Start) ->
    {Parts, _N} =
        lists:mapfoldl(
            fun({Col, _V}, N) ->
                {<<Col/binary, " = $", (integer_to_binary(N))/binary>>, N + 1}
            end,
            Start,
            Sets
        ),
    iolist_to_binary(join_binary(Parts, <<", ">>)).

join_binary([], _Sep) ->
    [];
join_binary([P], _Sep) ->
    [P];
join_binary([P | Rest], Sep) ->
    [P, Sep | join_binary(Rest, Sep)].
