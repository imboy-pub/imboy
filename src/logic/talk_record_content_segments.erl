-module(talk_record_content_segments).
-moduledoc "面聊原文片段逻辑（T15）：当前版本正序分页/q 高亮/人工修订 CAS。".
%%%
% segments 域业务逻辑（§4 GET/PATCH /:id/segments）。
%
% GET：只查当前可见转写版本（record.current_transcript_id，§2「只查询
% 当前可见版本」）；start_ms/id 正序 keyset 游标；speaker_id 精确过滤；
% q 子串过滤 + 每段高亮位置（codepoint 偏移）。未发布/处理中/失败/
% no_speech → 空清单 + 状态投影（与 §4「未生成返回状态对象」同精神，
% 不伪装 404）。游标经 talk_record_ds 签名游标（绑定 uid/record/q/
% speaker，跨人/换筛选沿用 → cursor_invalid）。
%
% PATCH（§4 + T15 卡）：
%   * text / speaker_id（至少其一）+ If-Match segment revision；
%   * ACL：owner/editor；viewer 403；不可见 404；
%   * speaker_id 必须属于同记录（复合 FK 保证 + 显式 404，§8）；
%   * 成功效果（同事务）：origin=human + edited_by 署名 + revision+1；
%     record.content_revision+1（§11.2）；既有摘要 draft/confirmed → stale
%     （§2「正文校对后旧摘要标 stale」）；审计 segment_edit（白名单
%     metadata，不写正文）；
%   * 受控历史：人工修订行内版本化（同 seq 稳定 ID），模型重跑只建新
%     transcript 版本、不触碰人工行——设计依据见
%     talk_record_segment_repo 模块头注释。
%%%

-export([list/5, update/5]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(MAX_SEGMENT_TEXT_CHARS, 20000).
-define(MAX_QUERY_CHARS, 100).
-define(CURSOR_FAMILY, <<"talk_record_segments">>).

%% ===================================================================
%% GET /:id/segments
%% ===================================================================

%% @doc 原文清单。Filters：#{speaker_id => integer() | undefined,
%% q => binary()}（Handler 已解析）；Cursor 为不透明令牌；Limit 已钳
%% 20..100。返回 #{transcript_status, transcript_version, segments,
%% next_cursor}。
-spec list(integer(), integer(), map(), binary() | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
list(Uid, RecordId, Filters, Cursor, Limit) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, Row} ->
            list_visible(RecordId, Row, Filters, Cursor, Limit);
        {error, Reason} ->
            {error, Reason}
    end.

-spec list_visible(integer(), map(), map(), binary() | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
list_visible(RecordId, Row, Filters, Cursor, Limit) ->
    Binding = segments_binding(RecordId, Filters),
    case talk_record_ds:decode_page_cursor(Cursor, ?CURSOR_FAMILY, Binding) of
        {ok, undefined} ->
            page_segments(RecordId, Row, Filters, undefined, Limit, normal);
        {ok, [LastStartMs, LastId]} when is_integer(LastStartMs), is_integer(LastId) ->
            page_segments(RecordId, Row, Filters, {LastStartMs, LastId}, Limit, normal);
        {ok, _} ->
            {error, cursor_invalid};
        {error, key_unavailable} ->
            %% 签名 key 缺失：仅禁用翻页（首页可用），与 T05 列表同策略
            page_segments(RecordId, Row, Filters, undefined, Limit, no_cursor);
        {error, _} ->
            {error, cursor_invalid}
    end.

-spec page_segments(
    integer(),
    map(),
    map(),
    {integer(), integer()} | undefined,
    pos_integer(),
    atom()
) ->
    {ok, map()} | {error, term()}.
page_segments(RecordId, Row, Filters, Cursor, Limit, CursorMode) ->
    Columns = <<"t.id,t.version,t.status">>,
    case talk_record_transcript_repo:find_current(RecordId, Columns) of
        {ok, #{<<"version">> := Version}} ->
            page_version(RecordId, Row, Filters, Cursor, Limit, CursorMode, Version);
        {error, not_found} ->
            {ok, status_projection(Row)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec page_version(
    integer(),
    map(),
    map(),
    {integer(), integer()} | undefined,
    pos_integer(),
    atom(),
    integer()
) ->
    {ok, map()} | {error, term()}.
page_version(RecordId, Row, Filters, Cursor, Limit, CursorMode, Version) ->
    Q = maps:get(q, Filters, <<>>),
    RepoFilters = Filters#{cursor => Cursor, limit => Limit + 1},
    case
        elib_pg:with_tx(
            fun(Conn) ->
                talk_record_segment_repo:page_tx(Conn, RecordId, Version, RepoFilters)
            end
        )
    of
        {ok, Rows} ->
            {HasMore, Page} = split_page(Rows, Limit),
            NextCursor = next_cursor(
                CursorMode, HasMore, segments_binding(RecordId, Filters), Page
            ),
            {ok, #{
                <<"transcript_status">> => maps:get(<<"transcript_status">>, Row),
                <<"transcript_version">> => Version,
                <<"segments">> => [segment_dto(S, Q) || S <- Page],
                <<"next_cursor">> => NextCursor
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 未发布转写：空清单 + 记录状态投影（不伪装 404）。
-spec status_projection(map()) -> map().
status_projection(Row) ->
    #{
        <<"transcript_status">> => maps:get(<<"transcript_status">>, Row),
        <<"transcript_version">> => null,
        <<"segments">> => [],
        <<"next_cursor">> => null
    }.

-spec split_page([map()], pos_integer()) -> {boolean(), [map()]}.
split_page(Rows, Limit) ->
    case length(Rows) > Limit of
        true -> {true, lists:sublist(Rows, Limit)};
        false -> {false, Rows}
    end.

-spec next_cursor(atom(), boolean(), map(), [map()]) -> binary() | null.
next_cursor(no_cursor, _HasMore, _Binding, _Page) ->
    null;
next_cursor(_Mode, false, _Binding, _Page) ->
    null;
next_cursor(_Mode, true, Binding, Page) ->
    Last = lists:last(Page),
    SortVals = [maps:get(<<"start_ms">>, Last), maps:get(<<"id">>, Last)],
    case talk_record_ds:encode_cursor(?CURSOR_FAMILY, Binding, SortVals) of
        {ok, Cursor} -> Cursor;
        {error, _} -> null
    end.

%% @private 游标绑定（uid/record/筛选；切人/换筛选沿用 → cursor_invalid）。
%% speaker_id 归一为整数（undefined → 0）保证 canonical_json 可编码。
-spec segments_binding(integer(), map()) -> map().
segments_binding(RecordId, Filters) ->
    #{
        <<"record_id">> => RecordId,
        <<"speaker_id">> => norm_int(maps:get(speaker_id, Filters, 0)),
        <<"q">> => maps:get(q, Filters, <<>>)
    }.

-spec norm_int(integer() | undefined) -> integer().
norm_int(undefined) ->
    0;
norm_int(N) when is_integer(N) ->
    N.

%% @private 片段出站 DTO（§4：稳定 ID、时间点、revision；q 命中附高亮）。
-spec segment_dto(map(), binary()) -> map().
segment_dto(S, Q) ->
    Revision = maps:get(<<"revision">>, S, 1),
    Id = maps:get(<<"id">>, S),
    Base = #{
        <<"id">> => Id,
        <<"seq">> => maps:get(<<"seq">>, S),
        <<"transcript_version">> => maps:get(<<"transcript_version">>, S),
        <<"speaker_id">> => maps:get(<<"speaker_id">>, S),
        <<"start_ms">> => maps:get(<<"start_ms">>, S),
        <<"end_ms">> => maps:get(<<"end_ms">>, S),
        <<"text">> => maps:get(<<"text">>, S),
        <<"confidence">> => maps:get(<<"confidence">>, S),
        <<"is_final">> => maps:get(<<"is_final">>, S),
        <<"origin">> => maps:get(<<"origin">>, S),
        <<"revision">> => Revision,
        <<"edited_by">> => maps:get(<<"edited_by">>, S),
        <<"etag">> => talk_record_content_common:segment_etag(Id, Revision),
        <<"created_at">> => maps:get(<<"created_at">>, S),
        <<"updated_at">> => maps:get(<<"updated_at">>, S)
    },
    case Q of
        <<>> ->
            Base;
        _ ->
            Base#{
                <<"highlights">> =>
                    talk_record_content_common:highlight_positions(
                        maps:get(<<"text">>, S, <<>>), Q
                    )
            }
    end.

%% ===================================================================
%% PATCH /:id/segments/:segment_id
%% ===================================================================

%% @doc 人工修订（text/speaker_id + If-Match segment revision）。
%% 成功返回 #{segment => DTO, content_revision => N}。
-spec update(integer(), integer(), integer(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
update(Uid, RecordId, SegmentId, IfMatch, Body) when is_map(Body) ->
    case talk_record_content_common:parse_resource_if_match(<<"segment">>, SegmentId, IfMatch) of
        {ok, ExpectedRevision} ->
            update_parsed(Uid, RecordId, SegmentId, ExpectedRevision, Body);
        {error, Reason} ->
            {error, Reason}
    end;
update(_Uid, _RecordId, _SegmentId, _IfMatch, _Body) ->
    {error, invalid_body}.

-spec update_parsed(integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_parsed(Uid, RecordId, SegmentId, ExpectedRevision, Body) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, Row} ->
            update_visible(Uid, RecordId, SegmentId, ExpectedRevision, Body, Row);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_visible(integer(), integer(), integer(), pos_integer(), map(), map()) ->
    {ok, map()} | {error, term()}.
update_visible(Uid, RecordId, SegmentId, ExpectedRevision, Body, Row) ->
    case talk_record_content_common:editable_role_gate(Uid, Row) of
        ok ->
            update_gated(Uid, RecordId, SegmentId, ExpectedRevision, Body);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_gated(integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_gated(Uid, RecordId, SegmentId, ExpectedRevision, Body) ->
    case parse_update_fields(Body) of
        {ok, Changes} ->
            update_fields(Uid, RecordId, SegmentId, ExpectedRevision, Changes);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 可编辑字段白名单：text（≤20000 codepoint）/ speaker_id
%% （正整数或 null=取消署名）；其余键忽略；空改 → missing_editable_field。
-spec parse_update_fields(map()) -> {ok, map()} | {error, atom()}.
parse_update_fields(Body) ->
    Picked = maps:with([<<"text">>, <<"speaker_id">>], Body),
    case map_size(Picked) of
        0 ->
            {error, missing_editable_field};
        _ ->
            validate_update_fields(Picked)
    end.

-spec validate_update_fields(map()) -> {ok, map()} | {error, atom()}.
validate_update_fields(Picked) ->
    Checks = [
        {<<"text">>, fun valid_segment_text/1, invalid_segment_text},
        {<<"speaker_id">>, fun valid_nullable_id/1, invalid_matched_user_id}
    ],
    talk_record_content_common:validate_fields(Checks, Picked, ok).

-spec valid_segment_text(term()) -> boolean().
valid_segment_text(Text) ->
    talk_record_content_common:valid_text(Text, ?MAX_SEGMENT_TEXT_CHARS).

%% @private speaker_id：正整数（换绑）或 null（取消署名）。
-spec valid_nullable_id(term()) -> boolean().
valid_nullable_id(null) ->
    true;
valid_nullable_id(N) ->
    case talk_record_content_common:parse_pos_int(N) of
        {ok, _} -> true;
        error -> false
    end.

-spec update_fields(integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_fields(Uid, RecordId, SegmentId, ExpectedRevision, Changes) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> update_tx(Conn, Uid, RecordId, SegmentId, ExpectedRevision, Changes) end,
            [{reraise, false}]
        )
    of
        {ok, Segment, ContentRevision} ->
            {ok, #{
                <<"segment">> => segment_dto(Segment, <<>>),
                <<"content_revision">> => ContentRevision
            }};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_segment_update_rolled_back, SegmentId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 事务体：行存在性（复合匹配，跨 record 404）→ 换绑 speaker
%% 同记录校验（复合 FK 兜底 + 显式 404）→ CAS 人工修订 → content_revision
%% 推进 → 摘要标 stale → 审计。
-spec update_tx(any(), integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map(), pos_integer()} | {error, term()}.
update_tx(Conn, Uid, RecordId, SegmentId, ExpectedRevision, Changes) ->
    case talk_record_segment_repo:find_tx(Conn, RecordId, SegmentId) of
        {ok, _Segment} ->
            reassign_speaker_tx(Conn, RecordId, Changes, fun(Sets) ->
                cas_human_update_tx(Conn, Uid, RecordId, SegmentId, ExpectedRevision, Sets)
            end);
        {error, not_found} ->
            {error, segment_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private speaker_id 换绑前置校验（同记录 speaker 才可绑定；§8）。
-spec reassign_speaker_tx(any(), integer(), map(), function()) ->
    {ok, map(), pos_integer()} | {error, term()}.
reassign_speaker_tx(Conn, RecordId, Changes, Next) ->
    case maps:find(<<"speaker_id">>, Changes) of
        {ok, SpeakerId} when is_integer(SpeakerId) ->
            case talk_record_speaker_repo:find_tx(Conn, RecordId, SpeakerId) of
                {ok, _} -> Next(Changes);
                {error, not_found} -> {error, speaker_not_found};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            Next(Changes)
    end.

-spec cas_human_update_tx(any(), integer(), integer(), integer(), pos_integer(), map()) ->
    {ok, map(), pos_integer()} | {error, term()}.
cas_human_update_tx(Conn, Uid, RecordId, SegmentId, ExpectedRevision, Changes) ->
    Sets = Changes#{actor_id => Uid},
    case
        talk_record_segment_repo:update_human_tx(Conn, RecordId, SegmentId, ExpectedRevision, Sets)
    of
        {ok, Updated} ->
            finish_segment_update_tx(Conn, Uid, RecordId, SegmentId, Updated, Changes);
        {error, not_found} ->
            %% CAS 0 行：行存在（前面已查）→ 版本过期 412
            {error, precondition_failed};
        {error, Reason} ->
            {error, Reason}
    end.

-spec finish_segment_update_tx(any(), integer(), integer(), integer(), map(), map()) ->
    {ok, map(), pos_integer()} | no_return().
finish_segment_update_tx(Conn, Uid, RecordId, SegmentId, Updated, Changes) ->
    case talk_record_repo:bump_content_revision_tx(Conn, RecordId) of
        {ok, ContentRevision} ->
            %% 写后失败必须回滚（content_revision/stale/审计同事务原子）：
            %% throw 触发 epgsql 回滚（marks 同款约定）。
            case talk_record_summary_repo:mark_stale_tx(Conn, RecordId) of
                {ok, _} ->
                    case
                        audit_segment_edit_tx(
                            Conn, Uid, RecordId, SegmentId, Updated, Changes, ContentRevision
                        )
                    of
                        ok -> {ok, Updated, ContentRevision};
                        {error, Reason} -> throw({talk_record_rollback, {error, Reason}})
                    end;
                {error, Reason} ->
                    throw({talk_record_rollback, {error, Reason}})
            end;
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private 审计（白名单 metadata；不写正文/新旧文本）。
-spec audit_segment_edit_tx(
    any(),
    integer(),
    integer(),
    integer(),
    map(),
    map(),
    pos_integer()
) -> ok | {error, term()}.
audit_segment_edit_tx(Conn, Uid, RecordId, SegmentId, Updated, Changes, ContentRevision) ->
    Metadata = #{
        <<"fields">> => lists:sort(maps:keys(maps:with([<<"text">>, <<"speaker_id">>], Changes))),
        <<"revision">> => maps:get(<<"revision">>, Updated),
        <<"content_revision">> => ContentRevision,
        <<"origin">> => maps:get(<<"origin">>, Updated)
    },
    case
        talk_record_repo:insert_audit_tx(
            Conn, RecordId, Uid, <<"segment_edit">>, SegmentId, Metadata
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.
