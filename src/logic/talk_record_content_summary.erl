-module(talk_record_content_summary).
-moduledoc "面聊摘要逻辑（T15）：状态对象读取/人工编辑与确认（不建任务不通知）。".
%%%
% summary 域业务逻辑（§4 GET/PATCH /:id/summary）。
%
% GET：
%   * 未生成 → 状态对象（#{status => not_generated, record 状态投影}），
%     **不伪装 404**（§4）；
%   * 已生成 → draft/confirmed/stale + overview/decisions/actions +
%     source_transcript_version/source_content_revision + ETag
%     （summary:<revision>，T16 契约同式）。
%
% PATCH（人工编辑与 confirm=true；§4/§11.2）：
%   * 可编辑：overview / decisions / actions（数组；每项可带 segment_ids，
%     服务端校验同记录同源转写版本——§2「segment_ids 引用并服务端校验」）；
%     confirm=true 可单独出现（纯确认）；
%   * ACL：owner/editor；viewer 403；不可见 404；
%   * If-Match summary revision（428/412）；
%   * 人工编辑效果：status → draft + source_content_revision 重基到当前
%     record.content_revision（正文校对会标 stale；人工编辑即以最新原文
%     复核，重基后 stale 语义由 status=draft 取代）；
%   * confirm 门禁：summary.source_content_revision 必须等于当前
%     record.content_revision，**不匹配 → 409 summary_source_outdated**
%     （source revision 不匹配 409，T15 卡）；成功 → confirmed +
%     confirmed_by；已确认且未再编辑 → 幂等返回现状；
%   * 仅确认文本：不建任务/不通知（无 job/消息副作用）；
%   * 版本域：summary 编辑/确认只推 summary.revision，不碰
%     record.revision / record.content_revision（§11.2）；
%   * 审计：summary_edit / summary_confirm（白名单 metadata，不写正文）。
%%%

-export([get/2, update/4]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(MAX_OVERVIEW_CHARS, 20000).
-define(MAX_ITEMS, 200).

%% ===================================================================
%% GET /:id/summary
%% ===================================================================

%% @doc 摘要读取：未生成返回状态对象；已生成返回摘要 + ETag。
-spec get(integer(), integer()) -> {ok, map()} | {error, term()}.
get(Uid, RecordId) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, Row} ->
            get_visible(RecordId, Row);
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_visible(integer(), map()) -> {ok, map()} | {error, term()}.
get_visible(RecordId, Row) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> talk_record_summary_repo:find_by_record_tx(Conn, RecordId) end
        )
    of
        {ok, Summary} ->
            {ok, summary_dto(Summary)};
        {error, not_found} ->
            %% §4：未生成返回状态对象，不伪装 404
            {ok, #{
                <<"status">> => <<"not_generated">>,
                <<"summary_status">> => maps:get(<<"summary_status">>, Row),
                <<"summary">> => null
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 出站 DTO（§4：状态 + overview/decisions/actions + 源版本标识 + ETag）。
-spec summary_dto(map()) -> map().
summary_dto(S) ->
    Revision = maps:get(<<"revision">>, S, 1),
    #{
        <<"id">> => maps:get(<<"id">>, S),
        <<"status">> => maps:get(<<"status">>, S),
        <<"overview">> => maps:get(<<"overview">>, S),
        <<"decisions">> => maps:get(<<"decisions">>, S),
        <<"actions">> => maps:get(<<"actions">>, S),
        <<"source_transcript_version">> => maps:get(<<"source_transcript_version">>, S),
        <<"source_content_revision">> => maps:get(<<"source_content_revision">>, S),
        <<"model_version">> => maps:get(<<"model_version">>, S),
        <<"confirmed_by">> => maps:get(<<"confirmed_by">>, S),
        <<"revision">> => Revision,
        <<"etag">> => talk_record_content_common:summary_etag(Revision),
        <<"created_at">> => maps:get(<<"created_at">>, S),
        <<"updated_at">> => maps:get(<<"updated_at">>, S)
    }.

%% ===================================================================
%% PATCH /:id/summary
%% ===================================================================

%% @doc 人工编辑与确认。成功返回 #{summary => DTO}。
-spec update(integer(), integer(), binary() | undefined, map()) ->
    {ok, map()} | {error, term()}.
update(Uid, RecordId, IfMatch, Body) when is_map(Body) ->
    case parse_if_match(IfMatch) of
        {ok, ExpectedRevision} ->
            update_parsed(Uid, RecordId, ExpectedRevision, Body);
        {error, Reason} ->
            {error, Reason}
    end;
update(_Uid, _RecordId, _IfMatch, _Body) ->
    {error, invalid_body}.

%% @private summary If-Match：`summary:<rev>` 令牌（T16 契约）或纯 revision
%% （T05 兼容）。复用通用解析（id 段恒缺省）。
-spec parse_if_match(binary() | undefined) -> {ok, pos_integer()} | {error, atom()}.
parse_if_match(IfMatch) ->
    talk_record_content_common:parse_resource_if_match(<<"summary">>, 0, IfMatch).

-spec update_parsed(integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_parsed(Uid, RecordId, ExpectedRevision, Body) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, Row} ->
            update_visible(Uid, RecordId, ExpectedRevision, Body, Row);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_visible(integer(), integer(), pos_integer(), map(), map()) ->
    {ok, map()} | {error, term()}.
update_visible(Uid, RecordId, ExpectedRevision, Body, Row) ->
    case talk_record_content_common:editable_role_gate(Uid, Row) of
        ok ->
            update_gated(Uid, RecordId, ExpectedRevision, Body);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_gated(integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_gated(Uid, RecordId, ExpectedRevision, Body) ->
    case parse_update_fields(Body) of
        {ok, Changes} ->
            update_fields(Uid, RecordId, ExpectedRevision, Changes);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 字段白名单：overview / decisions / actions / confirm；
%% 空改（无字段且无 confirm）→ missing_editable_field。返回的 Changes 携带
%% confirm 标志（后续编排消费，不可在校验中丢失）。
-spec parse_update_fields(map()) -> {ok, map()} | {error, atom()}.
parse_update_fields(Body) ->
    Confirm = maps:get(<<"confirm">>, Body, false),
    case Confirm of
        V when V =:= true; V =:= false ->
            Picked0 = maps:with([<<"overview">>, <<"decisions">>, <<"actions">>], Body),
            case {map_size(Picked0), Confirm} of
                {0, false} ->
                    {error, missing_editable_field};
                _ ->
                    validate_update_fields(Picked0#{<<"confirm">> => Confirm})
            end;
        _ ->
            {error, invalid_confirm}
    end.

-spec validate_update_fields(map()) -> {ok, map()} | {error, atom()}.
validate_update_fields(Picked) ->
    Checks = [
        {<<"overview">>, fun valid_overview/1, invalid_summary_overview},
        {<<"decisions">>, fun valid_items/1, invalid_summary_items},
        {<<"actions">>, fun valid_items/1, invalid_summary_items}
    ],
    talk_record_content_common:validate_fields(Checks, Picked, ok).

-spec valid_overview(term()) -> boolean().
valid_overview(Overview) ->
    talk_record_content_common:valid_text(Overview, ?MAX_OVERVIEW_CHARS).

%% @private decisions/actions：数组、每项 object、≤200 项；每项可选
%% segment_ids（正整数数组）。segment_ids 同记录校验在事务内做
%% （需源转写版本，见 validate_segment_refs_tx）。
-spec valid_items(term()) -> boolean().
valid_items(Items) when is_list(Items), length(Items) =< ?MAX_ITEMS ->
    lists:all(fun valid_item/1, Items);
valid_items(_) ->
    false.

-spec valid_item(term()) -> boolean().
valid_item(Item) when is_map(Item) ->
    case maps:find(<<"segment_ids">>, Item) of
        {ok, Ids} when is_list(Ids) ->
            lists:all(
                fun(I) ->
                    case talk_record_content_common:parse_pos_int(I) of
                        {ok, _} -> true;
                        error -> false
                    end
                end,
                Ids
            );
        {ok, _} ->
            false;
        error ->
            true
    end;
valid_item(_) ->
    false.

-spec update_fields(integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_fields(Uid, RecordId, ExpectedRevision, Changes) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> update_tx(Conn, Uid, RecordId, ExpectedRevision, Changes) end,
            [{reraise, false}]
        )
    of
        {ok, Summary} ->
            {ok, #{<<"summary">> => summary_dto(Summary)}};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_summary_update_rolled_back, RecordId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 事务体：摘要行存在 → segment_ids 同记录校验 → 读当前
%% content_revision →（编辑 → 重基 draft）→（confirm → 门禁 + confirmed）。
%% 同一请求内编辑后紧随确认时，确认步以编辑后 revision 继续（If-Match 已
%% 覆盖并发仲裁；两步同一事务原子）。
-spec update_tx(any(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, term()}.
update_tx(Conn, Uid, RecordId, ExpectedRevision, Changes) ->
    case talk_record_summary_repo:find_by_record_tx(Conn, RecordId) of
        {ok, Summary} ->
            validate_segment_refs_tx(Conn, RecordId, Summary, Changes, fun(ContentRevision) ->
                apply_changes_tx(
                    Conn, Uid, RecordId, ExpectedRevision, Changes, Summary, ContentRevision
                )
            end);
        {error, not_found} ->
            {error, summary_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private decision/action 引用的 segment_ids 必须属于同记录的源转写版本
%% （§2 服务端校验同记录）；通过后读当前 content_revision 一并下传。
-spec validate_segment_refs_tx(any(), integer(), map(), map(), function()) ->
    {ok, map()} | {error, term()}.
validate_segment_refs_tx(Conn, RecordId, Summary, Changes, Next) ->
    Version = maps:get(<<"source_transcript_version">>, Summary),
    Ids = collect_segment_ids(Changes),
    RefsOk = segment_refs_valid(Conn, RecordId, Version, Ids),
    case RefsOk of
        true ->
            case talk_record_repo:find_by_id_tx(Conn, RecordId, <<"content_revision">>) of
                {ok, #{<<"content_revision">> := ContentRev}} -> Next(ContentRev);
                {error, Reason} -> throw({talk_record_rollback, {error, Reason}})
            end;
        false ->
            {error, invalid_summary_segment_ref}
    end.

-spec segment_refs_valid(any(), integer(), integer(), [integer()]) -> boolean().
segment_refs_valid(_Conn, _RecordId, _Version, []) ->
    true;
segment_refs_valid(Conn, RecordId, Version, Ids) ->
    case talk_record_segment_repo:count_ids_tx(Conn, RecordId, Version, Ids) of
        {ok, N} -> N =:= length(lists:usort(Ids));
        {error, _} -> false
    end.

%% @private 引用收集（decisions/actions 各项的 segment_ids 并集，已去重）。
-spec collect_segment_ids(map()) -> [integer()].
collect_segment_ids(Changes) ->
    Keys = [<<"decisions">>, <<"actions">>],
    lists:usort(lists:append([ids_of_items(maps:get(K, Changes, [])) || K <- Keys])).

-spec ids_of_items([map()]) -> [integer()].
ids_of_items(Items) ->
    lists:append([ids_of_item(Item) || Item <- Items]).

-spec ids_of_item(map()) -> [integer()].
ids_of_item(Item) ->
    case maps:find(<<"segment_ids">>, Item) of
        {ok, SegIds} when is_list(SegIds) ->
            [
                I
             || SegId <- SegIds, {ok, I} <- [talk_record_content_common:parse_pos_int(SegId)]
            ];
        _ ->
            []
    end.

%% @private 编辑/确认编排：先应用编辑字段（如有），再按门禁确认（如要求）。
-spec apply_changes_tx(any(), integer(), integer(), pos_integer(), map(), map(), pos_integer()) ->
    {ok, map()} | {error, term()}.
apply_changes_tx(Conn, Uid, RecordId, ExpectedRevision, Changes, Summary, ContentRevision) ->
    EditFields = maps:with([<<"overview">>, <<"decisions">>, <<"actions">>], Changes),
    Confirm = maps:get(<<"confirm">>, Changes, false),
    case map_size(EditFields) of
        0 ->
            confirm_only_tx(Conn, Uid, RecordId, ExpectedRevision, Summary, ContentRevision);
        _ ->
            edit_then_confirm_tx(
                Conn,
                Uid,
                RecordId,
                ExpectedRevision,
                EditFields,
                Confirm,
                Summary,
                ContentRevision
            )
    end.

%% @private 纯确认：source revision 门禁（不匹配 409）→ confirmed；
%% 已确认幂等返回现状。
-spec confirm_only_tx(any(), integer(), integer(), pos_integer(), map(), pos_integer()) ->
    {ok, map()} | {error, term()}.
confirm_only_tx(Conn, Uid, RecordId, ExpectedRevision, Summary, ContentRevision) ->
    case maps:get(<<"status">>, Summary) of
        <<"confirmed">> ->
            %% 幂等重确认（If-Match 已过校验）：返回现状
            {ok, Summary};
        _ ->
            confirm_gated_tx(Conn, Uid, RecordId, ExpectedRevision, Summary, ContentRevision)
    end.

-spec edit_then_confirm_tx(
    any(), integer(), integer(), pos_integer(), map(), boolean(), map(), pos_integer()
) ->
    {ok, map()} | {error, term()}.
edit_then_confirm_tx(
    Conn, Uid, RecordId, ExpectedRevision, EditFields, Confirm, Summary, ContentRevision
) ->
    SummaryId = maps:get(<<"id">>, Summary),
    %% 编辑即人工复核最新原文：status → draft + source_content_revision 重基
    Sets = encode_items(
        EditFields#{
            <<"status">> => <<"draft">>,
            <<"source_content_revision">> => ContentRevision
        },
        [<<"decisions">>, <<"actions">>]
    ),
    case talk_record_summary_repo:update_tx(Conn, RecordId, SummaryId, ExpectedRevision, Sets) of
        {ok, Edited} ->
            ok = audit_tx(Conn, RecordId, Uid, <<"summary_edit">>, SummaryId, #{
                <<"fields">> => lists:sort(maps:keys(EditFields)),
                <<"revision">> => maps:get(<<"revision">>, Edited)
            }),
            case Confirm of
                true ->
                    confirm_gated_tx(
                        Conn,
                        Uid,
                        RecordId,
                        maps:get(<<"revision">>, Edited),
                        Edited,
                        ContentRevision
                    );
                false ->
                    {ok, Edited}
            end;
        {error, not_found} ->
            {error, precondition_failed};
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private 确认门禁：source_content_revision ≠ 当前 record.content_revision
%% → 409；重基后的编辑稿天然过门禁。CAS 失败（并发）→ 412。
-spec confirm_gated_tx(any(), integer(), integer(), pos_integer(), map(), pos_integer()) ->
    {ok, map()} | {error, term()}.
confirm_gated_tx(Conn, Uid, RecordId, WorkingRevision, Summary, ContentRevision) ->
    case maps:get(<<"source_content_revision">>, Summary) =:= ContentRevision of
        false ->
            {error, summary_source_outdated};
        true ->
            SummaryId = maps:get(<<"id">>, Summary),
            Sets = #{<<"status">> => <<"confirmed">>, <<"confirmed_by">> => Uid},
            case
                talk_record_summary_repo:update_tx(Conn, RecordId, SummaryId, WorkingRevision, Sets)
            of
                {ok, Confirmed} ->
                    ok = audit_tx(Conn, RecordId, Uid, <<"summary_confirm">>, SummaryId, #{
                        <<"confirmed">> => true,
                        <<"revision">> => maps:get(<<"revision">>, Confirmed),
                        <<"source_content_revision">> => ContentRevision
                    }),
                    {ok, Confirmed};
                {error, not_found} ->
                    {error, precondition_failed};
                {error, Reason} ->
                    throw({talk_record_rollback, {error, Reason}})
            end
    end.

%% @private jsonb 列编码（repo 以 ::jsonb cast 接 JSON binary）。
-spec encode_items(map(), [binary()]) -> map().
encode_items(Sets, Keys) ->
    lists:foldl(
        fun(K, Acc) ->
            case maps:find(K, Acc) of
                {ok, V} -> Acc#{K => jsone:encode(V)};
                error -> Acc
            end
        end,
        Sets,
        Keys
    ).

%% @private 审计写入（失败即回滚）。
-spec audit_tx(any(), integer(), integer(), binary(), integer(), map()) -> ok | no_return().
audit_tx(Conn, RecordId, Uid, Action, TargetId, Metadata) ->
    case talk_record_repo:insert_audit_tx(Conn, RecordId, Uid, Action, TargetId, Metadata) of
        {ok, _} -> ok;
        {error, Reason} -> throw({talk_record_rollback, {error, Reason}})
    end.
