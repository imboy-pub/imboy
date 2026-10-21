-module(talk_record_ds).
-moduledoc "面聊记录记录编排与兼容入口；上下文、游标、采集、分段职责独立。".
%%%
% talk_record 领域服务层（Handler→Logic→DS→Repo 的第三层）。
%
% 职责：
%   * create：幂等键事务（登记→归属核验→插入→回填快照）；业务键
%     (created_by, client_recording_id) 撞库转重放（设计 §11.1 离线补传）；
%   * detail/update：统一可见性谓词（repo）+ 角色策略（owner/editor/viewer）
%     + revision CAS（§11.2 revision 域：title/language/notes 走 record.revision）；
%   * list/creation_contexts：opaque 签名游标（HMAC-SHA256，绑定调用者与
%     筛选；格式对齐 enterprise_cursor_v2 的 base64url(canonical)+"."+mac，
%     family 域独立，不内嵌其白名单）；
%   * capabilities：部署能力面如实上报（V1 ASR/summary/voice_id 未配置
%     → false，不虚称）。
%
% 幂等 digest 公式对齐 enterprise_internal_idempotency（freeze §2.5）；
% TTL 默认 7 天。result_ref 只存首次结果的元数据快照，不含正文/URL。
%%%

-export([capabilities/0]).
-export([creation_contexts/3]).
-export([create/4]).
-export([detail/2]).
-export([update/4]).
-export([list/4]).
-export([etag/1]).
%% T07 采集域
-export([create_capture/6, list_captures/2, append_event/4, register_part/6]).
-export([confirm_part/5, recover_capture/4, abandon_capture/4, fold_events/1]).
%% T15 内容域复用：签名游标编解码（Family 由调用方传入，如
%% talk_record_content_segments 的 talk_record_segments 族）。
-export([encode_cursor/3, decode_page_cursor/3]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(RESOURCE_CREATE, <<"/api/v1/talk-records">>).
-define(CURSOR_FAMILY_LIST, <<"talk_record_list">>).

%% ===================================================================
%% 创建 / Create
%% ===================================================================

%% @doc 创建记录（事务：幂等登记 → 归属核验 → 插入 → 快照回填）。
%% Validated 来自 talk_record_logic:parse_create；Digest 为请求规范化指纹。
%% 返回 {ok, created | replay, Payload, ETag} | {error, Reason}。
-spec create(integer(), map(), binary(), binary()) ->
    {ok, created | replay, map(), binary()} | {error, term()}.
create(Uid, Validated, IdemKey, Digest) ->
    ExpiresAt = elib_dt:add(elib_dt:now(), {talk_record_view_ds:idempotency_ttl_seconds(), second}),
    %% reraise=false：业务回滚以 {rollback, Reason} 返回（reraise=true 时
    %% elib_pg:with_conn 会把 throw 包装为 {error, {db_exception, throw, _}}，
    %% 业务无法按原形态捕获）。
    case
        elib_pg:with_tx(
            fun(Conn) -> create_tx(Conn, Uid, Validated, IdemKey, Digest, ExpiresAt) end,
            [{reraise, false}]
        )
    of
        {ok, _, _, _} = Ok ->
            Ok;
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_create_rolled_back, Uid, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

-spec create_tx(any(), integer(), map(), binary(), binary(), binary()) ->
    {ok, created | replay, map(), binary()} | {error, term()}.
create_tx(Conn, Uid, Validated, IdemKey, Digest, ExpiresAt) ->
    case
        talk_record_idempotency_repo:record_tx(
            Conn, Uid, <<"POST">>, ?RESOURCE_CREATE, IdemKey, Digest, ExpiresAt
        )
    of
        {ok, inserted} ->
            case talk_record_context_ds:validate_context_tx(Conn, Uid, Validated) of
                ok ->
                    insert_record_tx(Conn, Uid, Validated, IdemKey);
                {error, Reason} ->
                    %% 归属核验失败：整体回滚（幂等键不被烧掉，修复后可重试）
                    throw({talk_record_rollback, {error, Reason}})
            end;
        {ok, existing, Row} ->
            case replay_existing(Row, Digest) of
                {ok, replay, P, _} = Replay ->
                    case
                        talk_record_recovery_ds:authorize_tx(
                            Conn,
                            Uid,
                            Validated,
                            maps:get(<<"id">>, P)
                        )
                    of
                        {ok, _} -> Replay;
                        Error -> throw({talk_record_rollback, Error})
                    end;
                Error ->
                    Error
            end;
        {error, Reason} ->
            ?ERROR_LOG([talk_record_idem_record_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec insert_record_tx(any(), integer(), map(), binary()) ->
    {ok, created | replay, map(), binary()} | {error, term()}.
insert_record_tx(Conn, Uid, Validated, IdemKey) ->
    Now = elib_dt:now(),
    Data0 = #{
        <<"id">> => elib_tsid:generate(talk_record),
        <<"owner_user_id">> => Uid,
        <<"created_by">> => Uid,
        <<"client_recording_id">> => maps:get(client_recording_id, Validated),
        <<"source_type">> => <<"live_recording">>,
        <<"scope">> => maps:get(scope, Validated),
        <<"title">> => maps:get(title, Validated),
        <<"language">> => maps:get(language, Validated),
        <<"processing_mode">> => maps:get(processing_mode, Validated),
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    },
    Data = append_org_fields(Validated, Data0),
    case talk_record_repo:insert_tx(Conn, Data) of
        {ok, Id} ->
            talk_record_recovery_ds:notice_tx(Conn, Uid, Id, Validated),
            finish_create_tx(Conn, Uid, Id, IdemKey, created);
        {error, unique_violation} ->
            %% 业务键撞库（§11.1 离线补传去重）：取既有记录转重放
            replay_by_business_key_tx(Conn, Uid, Validated, IdemKey);
        {error, Reason} ->
            ?ERROR_LOG([talk_record_insert_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec append_org_fields(map(), map()) -> map().
append_org_fields(#{scope := <<"organization">>, organization_id := OrgId} = V, Data) ->
    Data1 = Data#{<<"organization_id">> => OrgId},
    case maps:get(workspace_id, V, 0) of
        0 -> Data1;
        WsId -> Data1#{<<"workspace_id">> => WsId}
    end;
append_org_fields(_, Data) ->
    Data.

-spec replay_by_business_key_tx(any(), integer(), map(), binary()) ->
    {ok, replay, map(), binary()} | {error, term()}.
replay_by_business_key_tx(Conn, Uid, Validated, IdemKey) ->
    ClientRecordingId = maps:get(client_recording_id, Validated),
    case talk_record_repo:find_by_creator_client_tx(Conn, Uid, ClientRecordingId) of
        {ok, Row} ->
            Id = maps:get(<<"id">>, Row),
            case talk_record_recovery_ds:authorize_tx(Conn, Uid, Validated, Id) of
                {ok, _} -> finish_create_tx(Conn, Uid, Id, IdemKey, replay);
                Error -> throw({talk_record_rollback, Error})
            end;
        {error, Reason} ->
            ?ERROR_LOG([talk_record_business_key_replay_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @private 组装快照并回填幂等行（首次结果的 payload 形态冻结进
%% result_ref——重放按首次结果返回，不回读后续编辑）。
-spec finish_create_tx(any(), integer(), integer(), binary(), created | replay) ->
    {ok, created | replay, map(), binary()} | {error, term()}.
finish_create_tx(Conn, Uid, RecordId, IdemKey, Mark) ->
    case talk_record_repo:find_by_id_tx(Conn, RecordId, talk_record_repo:detail_columns()) of
        {ok, Row} ->
            Caps = talk_record_view_ds:capabilities_for_actor(Uid, Row),
            ETag = talk_record_view_ds:etag(maps:get(<<"revision">>, Row)),
            Payload = talk_record_view_ds:detail_payload(Uid, Row, Caps),
            Snapshot = jsone:encode(#{<<"payload">> => Payload, <<"etag">> => ETag}),
            case
                talk_record_idempotency_repo:complete_tx(
                    Conn, Uid, <<"POST">>, ?RESOURCE_CREATE, IdemKey, Snapshot
                )
            of
                ok -> {ok, Mark, Payload, ETag};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            ?ERROR_LOG([talk_record_reload_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

-spec replay_existing(map(), binary()) ->
    {ok, replay, map(), binary()} | {error, idempotency_conflict | idempotency_pending}.
replay_existing(Row, Digest) ->
    case maps:get(<<"payload_hash">>, Row, <<>>) of
        Digest ->
            case maps:get(<<"result_ref">>, Row, null) of
                SnapBin when is_binary(SnapBin) ->
                    #{<<"payload">> := P, <<"etag">> := E} = jsone:decode(SnapBin),
                    {ok, replay, P, E};
                _ ->
                    {error, idempotency_pending}
            end;
        _ ->
            {error, idempotency_conflict}
    end.

%% ===================================================================
%% 详情 / Detail
%% ===================================================================

%% @doc 可见行详情（无权/不存在统一 not_found）；附 capabilities_for_actor
%% 与资源 ETag（§4 GET /:id）。custody_status 读路径投影（§9 T19：owner
%% 失去归属访问时 handover_required）。
-spec detail(integer(), integer()) ->
    {ok, #{payload := map(), etag := binary(), capabilities_for_actor := map()}}
    | {error, not_found | term()}.
detail(Uid, Id) ->
    case talk_record_repo:find_visible(Uid, Id) of
        {ok, Row0} ->
            Row = talk_record_repo:project_custody(Row0),
            Caps = talk_record_view_ds:capabilities_for_actor(Uid, Row),
            ETag = talk_record_view_ds:etag(maps:get(<<"revision">>, Row)),
            {ok, #{
                payload => talk_record_view_ds:detail_payload(Uid, Row, Caps),
                etag => ETag,
                capabilities_for_actor => Caps
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% 编辑 / Update（PATCH title/language/notes + If-Match CAS）
%% ===================================================================

%% @doc 元信息编辑：ExpectedRevision 来自 If-Match；Changes 仅含
%% title/language/notes 子集（owner）或 notes（editor）。CAS 未命中且行可见
%% → precondition_failed（412）。
-spec update(integer(), integer(), pos_integer(), map()) ->
    {ok, #{payload := map(), etag := binary()}} | {error, term()}.
update(Uid, Id, ExpectedRevision, Changes) ->
    case talk_record_repo:find_visible(Uid, Id) of
        {error, not_found} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason};
        {ok, Row} ->
            Role = talk_record_view_ds:actor_role(Uid, Row),
            case edit_policy(Role, Changes) of
                ok ->
                    update_locked(Uid, Id, Row, ExpectedRevision, Changes);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec update_locked(integer(), integer(), map(), pos_integer(), map()) ->
    {ok, #{payload := map(), etag := binary()}} | {error, term()}.
update_locked(Uid, Id, Row, ExpectedRevision, Changes) ->
    Locked = maps:get(<<"transcript_status">>, Row, <<"not_requested">>),
    case {maps:is_key(<<"language">>, Changes), language_editable(Locked)} of
        {true, false} ->
            {error, language_locked};
        _ ->
            update_cas(Uid, Id, ExpectedRevision, Changes)
    end.

-spec update_cas(integer(), integer(), pos_integer(), map()) ->
    {ok, #{payload := map(), etag := binary()}} | {error, term()}.
update_cas(Uid, Id, ExpectedRevision, Changes) ->
    case
        elib_pg:with_tx(fun(Conn) ->
            talk_record_repo:update_meta_tx(Conn, Id, ExpectedRevision, Changes)
        end)
    of
        {ok, NewRow} ->
            Caps = talk_record_view_ds:capabilities_for_actor(Uid, NewRow),
            ETag = talk_record_view_ds:etag(maps:get(<<"revision">>, NewRow)),
            {ok, #{payload => talk_record_view_ds:detail_payload(Uid, NewRow, Caps), etag => ETag}};
        %% 行可见但 CAS 未命中 ⇒ revision 已过期（412），不误报 404
        {error, not_found} ->
            {error, precondition_failed};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 编辑角色策略（§1）：owner 全量；editor 仅 notes；viewer 拒绝。
-spec edit_policy(owner | editor | viewer, map()) -> ok | {error, term()}.
edit_policy(owner, _Changes) ->
    ok;
edit_policy(editor, Changes) ->
    case maps:keys(Changes) =:= [<<"notes">>] orelse Changes =:= #{} of
        true -> ok;
        false -> {error, forbidden_field}
    end;
edit_policy(viewer, _Changes) ->
    {error, forbidden}.

-spec language_editable(binary()) -> boolean().
language_editable(<<"not_requested">>) -> true;
language_editable(<<"unavailable">>) -> true;
language_editable(_) -> false.

%% ===================================================================
%% 列表 / List（keyset 游标）
%% ===================================================================

%% @doc 有权记录列表（§4 GET /）：filter=all|owned|shared；游标绑定调用者
%% 与筛选，不可跨 org 沿用。
-spec list(integer(), map(), binary() | undefined, pos_integer()) ->
    {ok, #{list := [map()], next_cursor := binary() | null}} | {error, term()}.
list(Uid, Filters, CursorBin, Limit) ->
    case
        talk_record_cursor_ds:decode_page_cursor(
            CursorBin, ?CURSOR_FAMILY_LIST, list_binding(Uid, Filters)
        )
    of
        {ok, undefined} ->
            page_list(Uid, Filters, undefined, Limit);
        {ok, [LastCreatedAt, LastId]} when is_binary(LastCreatedAt), is_integer(LastId) ->
            page_list(Uid, Filters, {LastCreatedAt, LastId}, Limit);
        {ok, _} ->
            {error, cursor_invalid};
        {error, key_unavailable} ->
            %% 无签名密钥时仅禁用翻页（首页可用），不 503 整个列表
            page_list(Uid, Filters, undefined, Limit, no_cursor);
        {error, _} ->
            {error, cursor_invalid}
    end.

-spec page_list(integer(), map(), {binary(), integer()} | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
page_list(Uid, Filters, Cursor, Limit) ->
    page_list(Uid, Filters, Cursor, Limit, normal).

-spec page_list(integer(), map(), {binary(), integer()} | undefined, pos_integer(), atom()) ->
    {ok, map()} | {error, term()}.
page_list(Uid, Filters, Cursor, Limit, CursorMode) ->
    case talk_record_repo:page_visible(Uid, Filters, Cursor, Limit) of
        {ok, Rows} ->
            Next =
                case {CursorMode, length(Rows) =:= Limit} of
                    {no_cursor, _} -> null;
                    {_, true} -> maybe_next_cursor(Uid, Filters, Rows);
                    _ -> null
                end,
            {ok, #{
                <<"list">> => [talk_record_view_ds:list_item(Uid, Row) || Row <- Rows],
                <<"next_cursor">> => Next
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec maybe_next_cursor(integer(), map(), [map()]) -> binary() | null.
maybe_next_cursor(Uid, Filters, Rows) when Rows =/= [] ->
    Last = lists:last(Rows),
    Sort = [maps:get(<<"created_at">>, Last), maps:get(<<"id">>, Last)],
    case
        talk_record_cursor_ds:encode_cursor(?CURSOR_FAMILY_LIST, list_binding(Uid, Filters), Sort)
    of
        {ok, Cursor} -> Cursor;
        {error, _} -> null
    end;
maybe_next_cursor(_Uid, _Filters, []) ->
    null.

-spec list_binding(integer(), map()) -> map().
list_binding(Uid, Filters) ->
    #{
        <<"uid">> => Uid,
        <<"scope">> => maps:get(scope, Filters, <<>>),
        <<"org">> => maps:get(organization_id, Filters, 0),
        <<"ws">> => maps:get(workspace_id, Filters, 0),
        <<"filter">> => maps:get(filter, Filters, all),
        <<"q">> => maps:get(q, Filters, <<>>)
    }.

-spec capabilities() -> map().
capabilities() ->
    talk_record_context_ds:capabilities().

-spec creation_contexts(integer(), binary() | undefined, pos_integer()) ->
    {ok, map()} | {error, term()}.
creation_contexts(A1, A2, A3) ->
    talk_record_context_ds:creation_contexts(A1, A2, A3).

-spec etag(integer()) -> binary().
etag(A1) ->
    talk_record_view_ds:etag(A1).

-spec create_capture(integer(), integer(), pos_integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
create_capture(A1, A2, A3, A4, A5, A6) ->
    talk_record_capture_ds:create_capture(A1, A2, A3, A4, A5, A6).

-spec list_captures(integer(), integer()) -> {ok, map()} | {error, term()}.
list_captures(A1, A2) ->
    talk_record_capture_ds:list_captures(A1, A2).

-spec append_event(integer(), integer(), integer(), map()) -> {ok, map()} | {error, term()}.
append_event(A1, A2, A3, A4) ->
    talk_record_capture_ds:append_event(A1, A2, A3, A4).

-spec register_part(integer(), integer(), integer(), map(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
register_part(A1, A2, A3, A4, A5, A6) ->
    talk_record_parts_ds:register_part(A1, A2, A3, A4, A5, A6).

-spec confirm_part(integer(), integer(), integer(), integer(), map()) ->
    {ok, confirmed | verifying | replay, map()} | {error, term()}.
confirm_part(A1, A2, A3, A4, A5) ->
    talk_record_parts_ds:confirm_part(A1, A2, A3, A4, A5).

-spec recover_capture(integer(), integer(), integer(), binary()) -> {ok, map()} | {error, term()}.
recover_capture(A1, A2, A3, A4) ->
    talk_record_capture_ds:recover_capture(A1, A2, A3, A4).

-spec abandon_capture(integer(), integer(), integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
abandon_capture(A1, A2, A3, A4) ->
    talk_record_capture_ds:abandon_capture(A1, A2, A3, A4).

-spec fold_events([map()]) -> {binary(), integer()}.
fold_events(A1) ->
    talk_record_capture_ds:fold_events(A1).

-spec encode_cursor(binary(), map(), [term()] | integer()) ->
    {ok, binary()} | {error, term()}.
encode_cursor(A1, A2, A3) ->
    talk_record_cursor_ds:encode_cursor(A1, A2, A3).

-spec decode_page_cursor(binary() | undefined, binary(), map()) ->
    {ok, term()} | {error, invalid | expired | key_unavailable}.
decode_page_cursor(A1, A2, A3) ->
    talk_record_cursor_ds:decode_page_cursor(A1, A2, A3).
