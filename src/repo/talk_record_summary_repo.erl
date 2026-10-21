-module(talk_record_summary_repo).
-moduledoc "面聊摘要仓储（T15：状态对象读取/人工编辑与确认 CAS/标 stale；迁移 00000211）。".
%%%
% talk_record_summary 数据仓库层（T15 摘要域）。
%
% 契约（§2/§4/§11.2）：
%   * status ∈ draft/confirmed/stale；正文校对（segments PATCH 成功）后
%     旧摘要标 stale（mark_stale_tx 由 segments 编辑路径调用）；
%   * 人工编辑：status 回 draft 且 source_content_revision 重基到当前
%     record.content_revision（人工校对以最新原文为基线吸收变化）；
%   * confirm：要求 source_content_revision = 当前 record.content_revision
%     （source revision 不匹配 → 409，logic 层判定），成功置 confirmed +
%     confirmed_by；确认文本不建任务/通知；
%   * summary 编辑/确认只推 summary.revision，不碰 record.revision /
%     record.content_revision（§11.2 版本域纪律）；
%   * jsonb decisions/actions：epgsql 返回 JSON binary，本层 jsone:decode
%     出站、jsone:encode + ::jsonb 入站（仓内 bot_repo 惯例）。
%%%

-export([
    find_by_record_tx/2,
    update_tx/5,
    mark_stale_tx/2
]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SUMMARY_COLUMNS,
    <<"sm.id,sm.record_id,sm.source_transcript_version,sm.source_content_revision,",
        "sm.status,sm.overview,sm.decisions,sm.actions,sm.model_version,",
        "sm.revision,sm.confirmed_by,sm.created_at,sm.updated_at">>
).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 记录当前摘要行（created_at/id 倒序取最新；V1 单摘要，多行时最新
% 为准，历史行不暴露）。未生成 → {error, not_found}（GET 层转状态对象，
% 不伪装 404）。
-spec find_by_record_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_by_record_tx(Conn, RecordId) ->
    Sql =
        <<"SELECT ", ?SUMMARY_COLUMNS/binary, " FROM talk_record_summary sm",
            " WHERE sm.record_id = $1 ORDER BY sm.created_at DESC, sm.id DESC LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, [Row]} -> {ok, decode_items(Row)};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc summary 编辑/确认 CAS（动态列 overview/decisions/actions/status/
%% source_content_revision/confirmed_by；revision+1）。命中 0 行由上层
%% 区分 not_found / precondition_failed。decisions/actions 以 JSON binary
%% 传入（::jsonb cast，logic 层负责 jsone:encode 与校验）。
-spec update_tx(any(), integer(), pos_integer(), map(), map()) ->
    {ok, map()} | {error, not_found | term()}.
update_tx(Conn, RecordId, SummaryId, ExpectedRevision, Sets) ->
    DynCols = [
        <<"overview">>,
        {<<"decisions">>, <<"::jsonb">>},
        {<<"actions">>, <<"::jsonb">>},
        <<"status">>,
        <<"source_content_revision">>,
        <<"confirmed_by">>
    ],
    {Sets1, _N} =
        lists:foldl(
            fun
                ({Col, Cast}, {Acc, Idx}) when is_map_key(Col, Sets) ->
                    SetSql = <<Col/binary, " = $", (integer_to_binary(Idx))/binary, Cast/binary>>,
                    {[{SetSql, maps:get(Col, Sets)} | Acc], Idx + 1};
                (Col, {Acc, Idx}) when is_map_key(Col, Sets) ->
                    SetSql = <<Col/binary, " = $", (integer_to_binary(Idx))/binary>>,
                    {[{SetSql, maps:get(Col, Sets)} | Acc], Idx + 1};
                (_, Acc) ->
                    Acc
            end,
            {[], 1},
            DynCols
        ),
    Ordered = lists:reverse(Sets1),
    SetSql = join_set_clause(Ordered),
    Base = length(Ordered) + 1,
    Sql =
        <<"UPDATE talk_record_summary sm SET ", SetSql/binary, ", revision = revision + 1",
            ", updated_at = $", (integer_to_binary(Base))/binary, " WHERE sm.record_id = $",
            (integer_to_binary(Base + 1))/binary, " AND sm.id = $",
            (integer_to_binary(Base + 2))/binary, " AND sm.revision = $",
            (integer_to_binary(Base + 3))/binary, " RETURNING ", ?SUMMARY_COLUMNS/binary>>,
    Params = [V || {_, V} <- Ordered] ++ [elib_dt:now(), RecordId, SummaryId, ExpectedRevision],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row]} -> {ok, decode_items(Row)};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 正文校对后旧摘要标 stale（§2）：draft/confirmed → stale（幂等，
% 已 stale 不动；不推进 revision——stale 是派生状态，非人工编辑）。
-spec mark_stale_tx(any(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
mark_stale_tx(Conn, RecordId) ->
    Sql =
        <<"UPDATE talk_record_summary SET status = 'stale', updated_at = $1",
            " WHERE record_id = $2 AND status IN ('draft', 'confirmed')">>,
    case elib_pg:execute_count(Conn, Sql, [elib_dt:now(), RecordId]) of
        {ok, N} -> {ok, N};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @private jsonb binary → Erlang term（epgsql 返回 JSON 文本 binary）。
-spec decode_items(map()) -> map().
decode_items(Row) ->
    Row#{
        <<"decisions">> => maybe_decode(maps:get(<<"decisions">>, Row, <<>>)),
        <<"actions">> => maybe_decode(maps:get(<<"actions">>, Row, <<>>))
    }.

-spec maybe_decode(term()) -> term().
maybe_decode(B) when is_binary(B) ->
    try
        jsone:decode(B)
    catch
        _:_ -> B
    end;
maybe_decode(V) ->
    V.

-spec join_set_clause([{binary(), term()}]) -> binary().
join_set_clause(Sets) ->
    Parts = [SqlFragment || {SqlFragment, _V} <- Sets],
    iolist_to_binary(join_binary(Parts, <<", ">>)).

join_binary([], _Sep) ->
    [];
join_binary([P], _Sep) ->
    [P];
join_binary([P | Rest], Sep) ->
    [P, Sep | join_binary(Rest, Sep)].
