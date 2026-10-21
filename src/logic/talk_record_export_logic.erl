-module(talk_record_export_logic).
-moduledoc "面聊文字导出：异步任务、当前授权与附件产物。".

-export([
    create/4,
    state/3,
    download/3,
    parse_create/1,
    export_ttl_seconds/0,
    render/3,
    snapshot_tx/2,
    authorize_tx/3
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 单次文字产物防御上限（txt/md；最终字节仅由附件系统持有）。
-define(MAX_CONTENT_BYTES, 2097152).
-define(DEFAULT_TTL_SECONDS, 86400).

%% ===================================================================
%% POST /:id/exports
%% ===================================================================

%% @doc 创建导出。返回 {ok, created | replay, Payload} | {error, Reason}。
-spec create(integer(), integer(), binary() | undefined, map()) ->
    {ok, created | replay, map()} | {error, term()}.
create(Uid, RecordId, IdemKey, Body) ->
    case talk_record_logic:valid_idempotency_key(IdemKey) of
        false ->
            {error, idempotency_key_required};
        true ->
            create_keyed(Uid, RecordId, IdemKey, Body)
    end.

create_keyed(Uid, RecordId, IdemKey, Body) ->
    case parse_create(Body) of
        {ok, #{format := Format}} ->
            Resource = export_resource(RecordId),
            case talk_record_logic:request_digest(<<"POST">>, Resource, Body) of
                {ok, Digest} ->
                    create_tx(Uid, RecordId, Format, IdemKey, Resource, Digest);
                {error, non_canonical} ->
                    {error, invalid_body}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

create_tx(Uid, RecordId, Format, IdemKey, Resource, Digest) ->
    ExpiresAt = elib_dt:add(elib_dt:now(), {idempotency_ttl_seconds(), second}),
    case
        elib_pg:with_tx(
            fun(Conn) ->
                create_in_tx(Conn, Uid, RecordId, Format, IdemKey, Resource, Digest, ExpiresAt)
            end,
            [{reraise, false}]
        )
    of
        {ok, Mark, Payload} ->
            ok = talk_record_job_worker:kick(),
            {ok, Mark, Payload};
        {rollback, {talk_record_rollback, {error, Reason}}} ->
            {error, Reason};
        {rollback, Reason} ->
            ?ERROR_LOG([talk_record_export_create_rolled_back, RecordId, Reason]),
            {error, db_error};
        {error, Reason} ->
            {error, Reason}
    end.

create_in_tx(Conn, Uid, RecordId, Format, IdemKey, Resource, Digest, ExpiresAt) ->
    case authorize_tx(Conn, Uid, RecordId) of
        {ok, _} ->
            create_authorized_tx(Conn, Uid, RecordId, Format, IdemKey, Resource, Digest, ExpiresAt);
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

create_authorized_tx(Conn, Uid, RecordId, Format, IdemKey, Resource, Digest, ExpiresAt) ->
    case
        talk_record_idempotency_repo:record_tx(
            Conn, Uid, <<"POST">>, Resource, IdemKey, Digest, ExpiresAt
        )
    of
        {ok, inserted} ->
            create_fresh_tx(Conn, Uid, RecordId, Format, IdemKey, Resource);
        {ok, existing, Row} ->
            replay_idem(Row, Digest);
        {error, Reason} ->
            ?ERROR_LOG([talk_record_export_idem_failed, Uid, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

create_fresh_tx(Conn, Uid, RecordId, Format, IdemKey, Resource) ->
    case export_gate_tx(Conn, Uid, RecordId) of
        {ok, Row} ->
            create_export_rows_tx(Conn, Uid, RecordId, Format, Row, IdemKey, Resource);
        {error, Reason} ->
            throw({talk_record_rollback, {error, Reason}})
    end.

%% @private 导出门禁（§1/§8）：visible → owner（策略）/ grantee
%% allow_export（策略）；无 ACL → not_found（幂等登记随事务回滚）。
-spec export_gate_tx(any(), integer(), integer()) -> {ok, map()} | {error, atom()}.
export_gate_tx(Conn, Uid, RecordId) ->
    authorize_tx(Conn, Uid, RecordId).

%% 当前授权在父记录锁内重验；grant 修改/删除/移交共享父锁。
authorize_tx(Conn, Uid, RecordId) ->
    case talk_record_export_repo:lock_record_tx(Conn, RecordId) of
        {ok, _} -> authorize_visible_tx(Conn, Uid, RecordId);
        Error -> Error
    end.

authorize_visible_tx(Conn, Uid, RecordId) ->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, #{<<"owner_user_id">> := Uid} = Row} ->
            policy_gate(Row);
        {ok, Row} ->
            case talk_record_export_repo:grant_export_tx(Conn, RecordId, Uid) of
                {ok, true} -> policy_gate(Row);
                {ok, false} -> {error, forbidden};
                Error -> Error
            end;
        Error ->
            Error
    end.

%% @private 策略检查点（§8：owner 不能绕过企业 export capability；
%% 单一真源在 talk_record_export_policy——logic 门禁与 ds capabilities
%% 投影同源不漂移）。
-spec policy_gate(map()) -> {ok, map()} | {error, export_disabled_by_policy}.
policy_gate(#{<<"scope">> := Scope, <<"organization_id">> := OrgId} = Row) ->
    case talk_record_export_policy:allowed(Scope, OrgId) of
        true -> {ok, Row};
        false -> {error, export_disabled_by_policy}
    end.

-spec create_export_rows_tx(any(), integer(), integer(), binary(), map(), binary(), binary()) ->
    {ok, created, map()} | no_return().
create_export_rows_tx(Conn, Uid, RecordId, Format, Row, IdemKey, Resource) ->
    ExportId = elib_tsid:generate(talk_record),
    {ok, ExportId} = talk_record_export_repo:insert_queued_tx(Conn, #{
        id => ExportId, record_id => RecordId, requester_id => Uid, format => Format
    }),
    {ok, _Mark, JobId} = enqueue_export_job_tx(Conn, RecordId, ExportId, Row),
    ok = must(talk_record_export_repo:set_job_tx(Conn, ExportId, JobId)),
    {ok, ExportRow} = talk_record_export_repo:find_tx(Conn, RecordId, ExportId),
    {ok, _} = talk_record_repo:insert_audit_tx(
        Conn,
        RecordId,
        Uid,
        <<"export_create">>,
        ExportId,
        #{<<"job_id">> => JobId}
    ),
    Payload = export_payload(ExportRow, JobId),
    Snapshot = jsone:encode(#{<<"payload">> => Payload}),
    ok = must(
        talk_record_idempotency_repo:complete_tx(
            Conn, Uid, <<"POST">>, Resource, IdemKey, Snapshot
        )
    ),
    {ok, created, Payload}.

%% @private 投递 export kind job（T12 骨架接线：dedupe_key 绑 export 行，
%% 一次导出一 job；inline 路径同事务直接终态）。
-spec enqueue_export_job_tx(any(), integer(), integer(), map()) ->
    {ok, created | existing, integer()} | no_return().
enqueue_export_job_tx(Conn, RecordId, ExportId, Row) ->
    Job = #{
        record_id => RecordId,
        kind => <<"export">>,
        input_content_revision => maps:get(<<"content_revision">>, Row, 1),
        dedupe_key => <<"export:", (integer_to_binary(ExportId))/binary>>
    },
    case talk_record_job_repo:enqueue_tx(Conn, Job) of
        {ok, created, JobId} ->
            {ok, created, JobId};
        {ok, existing, ExRow} ->
            {ok, existing, maps:get(<<"id">>, ExRow)};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_export_job_enqueue_failed, RecordId, Reason]),
            throw({talk_record_rollback, {error, db_error}})
    end.

%% @private 幂等重放（快照形态回放；同键异 body 409）。
-spec replay_idem(map(), binary()) -> {ok, replay, map()} | {error, atom()}.
replay_idem(Row, Digest) ->
    case maps:get(<<"payload_hash">>, Row, <<>>) of
        Digest ->
            case maps:get(<<"result_ref">>, Row, null) of
                SnapBin when is_binary(SnapBin) ->
                    #{<<"payload">> := P} = jsone:decode(SnapBin),
                    {ok, replay, P};
                _ ->
                    {error, idempotency_pending}
            end;
        _ ->
            {error, idempotency_conflict}
    end.

%% ===================================================================
%% 文字快照仅构造内存中的有界内容；此函数不发布、不写附件。
%% ===================================================================

%% @doc worker 已持父记录锁时读取完整快照。任何读取失败均拒绝发布。
-spec snapshot_tx(any(), map()) -> {ok, map()} | {error, term()}.
snapshot_tx(Conn, #{
    <<"id">> := ExportId,
    <<"record_id">> := RecordId,
    <<"format">> := Format
}) ->
    case load_snapshot_tx(Conn, RecordId) of
        {error, Reason} ->
            {error, Reason};
        {ok, Snapshot} ->
            Content = render(Format, Snapshot, #{}),
            case byte_size(Content) > ?MAX_CONTENT_BYTES of
                true ->
                    {error, content_too_large};
                false ->
                    {ok, #{
                        <<"id">> => ExportId,
                        <<"content">> => Content,
                        <<"content_revision">> => maps:get(
                            <<"content_revision">>, maps:get(record, Snapshot)
                        )
                    }}
            end
    end.

%% @private 内容快照：记录元信息 + 当前转写版本全文 + 说话人 + 摘要状态。
%% 未发布转写（record_only/处理中）→ 正文空 + 状态如实注明（§3 不伪造）。
%% 不含参与者名单（§4 不包含未授权参与者信息）。软删记录（并发删除已
%% 提交）→ {error, record_deleted}（§2 删除中 job 不能重新发布内容）。
-spec load_snapshot_tx(any(), integer()) -> {ok, map()} | {error, record_deleted | term()}.
load_snapshot_tx(Conn, RecordId) ->
    case
        talk_record_repo:find_by_id_tx(
            Conn,
            RecordId,
            <<"tr.id,tr.title,tr.language,tr.scope,tr.duration_ms,tr.notes,",
                "tr.transcript_status,tr.summary_status,tr.content_revision,tr.deleted_at">>
        )
    of
        {ok, #{<<"deleted_at">> := null} = Row} ->
            case load_content_tx(Conn, RecordId) of
                {ok, Segments, Speakers, Summary} ->
                    {ok, #{
                        record => Row,
                        segments => Segments,
                        speakers => Speakers,
                        summary => Summary
                    }};
                Error ->
                    Error
            end;
        {ok, _} ->
            {error, record_deleted};
        {error, Reason} ->
            {error, Reason}
    end.

load_content_tx(Conn, RecordId) ->
    case talk_record_speaker_repo:list_by_record_tx(Conn, RecordId) of
        {ok, Speakers} -> load_transcript_tx(Conn, RecordId, Speakers);
        Error -> Error
    end.

load_transcript_tx(Conn, RecordId, Speakers) ->
    case
        talk_record_transcript_repo:find_current_tx(Conn, RecordId, <<"t.id,t.version,t.status">>)
    of
        {ok, #{<<"version">> := Version}} ->
            case load_segments_page_tx(Conn, RecordId, Version, undefined, {[], 0}) of
                {ok, Segments} -> load_summary_tx(Conn, RecordId, Segments, Speakers);
                Error -> Error
            end;
        {error, not_found} ->
            load_summary_tx(Conn, RecordId, [], Speakers);
        Error ->
            Error
    end.

load_segments_page_tx(Conn, RecordId, Version, Cursor, {Acc, Bytes}) ->
    Filters = #{speaker_id => undefined, q => <<>>, cursor => Cursor, limit => 200},
    case talk_record_segment_repo:page_tx(Conn, RecordId, Version, Filters) of
        {ok, Rows} ->
            Total =
                Bytes + lists:sum([byte_size(maps:get(<<"text">>, R, <<>>)) + 128 || R <- Rows]),
            case Total > ?MAX_CONTENT_BYTES of
                true -> {error, content_too_large};
                false -> continue_segments_tx(Conn, RecordId, Version, Cursor, Rows, {Acc, Total})
            end;
        Error ->
            Error
    end.

continue_segments_tx(_Conn, _Rid, _Version, _Cursor, Rows, {Acc, _}) when length(Rows) < 200 ->
    {ok, lists:append(lists:reverse([Rows | Acc]))};
continue_segments_tx(Conn, Rid, Version, Cursor, Rows, {Acc, Total}) ->
    Last = lists:last(Rows),
    Next = {maps:get(<<"start_ms">>, Last), maps:get(<<"id">>, Last)},
    case Next =:= Cursor of
        true -> {error, invalid_export_cursor};
        false -> load_segments_page_tx(Conn, Rid, Version, Next, {[Rows | Acc], Total})
    end.

load_summary_tx(Conn, RecordId, Segments, Speakers) ->
    case talk_record_summary_repo:find_by_record_tx(Conn, RecordId) of
        {ok, Row} -> {ok, Segments, Speakers, Row};
        {error, not_found} -> {ok, Segments, Speakers, undefined};
        Error -> Error
    end.

%% ===================================================================
%% 渲染（纯函数）
%% ===================================================================

%% @doc 文字渲染（txt/md 两格式；T19 卡：当前 segments+speakers+summary
%% 状态快照）。说话人标签优先 display_name，缺省 local_label；无转写时
%% 正文段为空并如实注明状态。Opts 预留（V1 空 map）。
-spec render(binary(), map(), map()) -> binary().
render(Format, Snapshot, _Opts) ->
    #{record := Record, segments := Segments, speakers := Speakers, summary := Summary} = Snapshot,
    SpeakerNames = speaker_name_map(Speakers),
    Header = render_header(Format, Record),
    SpeakerSection = render_speakers(Format, Speakers),
    Body = render_body(Format, Segments, SpeakerNames, Record),
    SummarySection = render_summary(Format, Summary),
    join_sections([Header, SpeakerSection, Body, SummarySection]).

%% -------------------------------------------------------------------
%% 渲染内部（二进制拼装；中文标点原样保留）
%% -------------------------------------------------------------------

-spec render_header(binary(), map()) -> binary().
render_header(Format, Record) ->
    Title = maps:get(<<"title">>, Record, <<>>),
    TitleLine =
        case {Format, Title} of
            {<<"md">>, <<>>} -> <<"# 面聊记录"/utf8>>;
            {<<"md">>, _} -> <<"# ", Title/binary>>;
            {_, <<>>} -> <<"面聊记录"/utf8>>;
            {_, _} -> Title
        end,
    Duration = maps:get(<<"duration_ms">>, Record, 0),
    Meta = [
        <<"时长："/utf8, (format_duration(Duration))/binary>>,
        <<"转写状态："/utf8, (maps:get(<<"transcript_status">>, Record, <<"not_requested">>))/binary>>,
        <<"摘要状态："/utf8, (maps:get(<<"summary_status">>, Record, <<"not_requested">>))/binary>>
    ],
    MetaLine = join_bin(Meta, <<"；"/utf8>>),
    <<TitleLine/binary, "\n", MetaLine/binary>>.

-spec render_speakers(binary(), [map()]) -> binary().
render_speakers(_Format, []) ->
    <<>>;
render_speakers(Format, Speakers) ->
    Heading = heading(Format, <<"说话人"/utf8>>),
    Lines = [speaker_label(S) || S <- Speakers],
    <<Heading/binary, "\n", (join_bin(Lines, <<"\n">>))/binary>>.

-spec render_body(binary(), [map()], map(), map()) -> binary().
render_body(_Format, [], _SpeakerNames, Record) ->
    Status = maps:get(<<"transcript_status">>, Record, <<"not_requested">>),
    <<"（正文为空：转写状态 "/utf8, Status/binary, "，未发布可导出的转写版本）"/utf8>>;
render_body(Format, Segments, SpeakerNames, _Record) ->
    Heading = heading(Format, <<"正文"/utf8>>),
    Lines = [render_segment(S, SpeakerNames) || S <- Segments],
    <<Heading/binary, "\n", (join_bin(Lines, <<"\n">>))/binary>>.

-spec render_segment(map(), map()) -> binary().
render_segment(Segment, SpeakerNames) ->
    StartMs = maps:get(<<"start_ms">>, Segment, 0),
    Text = maps:get(<<"text">>, Segment, <<>>),
    Label =
        case maps:find(maps:get(<<"speaker_id">>, Segment, null), SpeakerNames) of
            {ok, Name} -> Name;
            error -> <<"未知说话人"/utf8>>
        end,
    <<"[", (format_ms(StartMs))/binary, "] ", Label/binary, "："/utf8, Text/binary>>.

-spec render_summary(binary(), map() | undefined) -> binary().
render_summary(_Format, undefined) ->
    <<>>;
render_summary(Format, Summary) ->
    Status = maps:get(<<"status">>, Summary, <<"draft">>),
    Overview = maps:get(<<"overview">>, Summary, <<>>),
    Heading = heading(Format, <<"摘要"/utf8>>),
    StatusLine = <<"状态："/utf8, Status/binary>>,
    case Overview of
        <<>> -> <<Heading/binary, "\n", StatusLine/binary>>;
        _ -> <<Heading/binary, "\n", StatusLine/binary, "\n", Overview/binary>>
    end.

-spec heading(binary(), binary()) -> binary().
heading(<<"md">>, Text) ->
    <<"## ", Text/binary>>;
heading(_, Text) ->
    <<"【"/utf8, Text/binary, "】"/utf8>>.

-spec speaker_label(map()) -> binary().
speaker_label(Speaker) ->
    case maps:get(<<"display_name">>, Speaker, <<>>) of
        <<>> -> maps:get(<<"local_label">>, Speaker, <<"说话人"/utf8>>);
        Name -> Name
    end.

-spec speaker_name_map([map()]) -> map().
speaker_name_map(Speakers) ->
    maps:from_list([{maps:get(<<"id">>, S), speaker_label(S)} || S <- Speakers]).

-spec join_sections([binary()]) -> binary().
join_sections(Sections) ->
    join_bin([S || S <- Sections, byte_size(S) > 0], <<"\n\n">>).

-spec join_bin([binary()], binary()) -> binary().
join_bin([], _Sep) ->
    <<>>;
join_bin([P], _Sep) ->
    P;
join_bin([P | Rest], Sep) ->
    <<P/binary, Sep/binary, (join_bin(Rest, Sep))/binary>>.

-spec format_ms(integer()) -> binary().
format_ms(Ms) when is_integer(Ms), Ms >= 0 ->
    TotalSec = Ms div 1000,
    MM = (TotalSec div 60) rem 60,
    HH = TotalSec div 3600,
    SS = TotalSec rem 60,
    list_to_binary(io_lib:format("~2..0B:~2..0B:~2..0B", [HH, MM, SS])).

-spec format_duration(integer()) -> binary().
format_duration(Ms) when is_integer(Ms), Ms >= 0 ->
    format_ms(Ms).

%% ===================================================================
%% GET /:id/exports/:export_id（exportState 轮询，T21 冻结形状）
%% ===================================================================

%% @doc 导出产物状态。requester 本人 + 记录可见；到期行惰性清理后
%% 状态投影 expired。返回 {ok, Payload} | {error, Reason}。
-spec state(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
state(Uid, RecordId, ExportId) ->
    case export_access(Uid, RecordId, ExportId) of
        {ok, _VisRow, Row} ->
            {ok, Row1} = expire_if_due(Row),
            {ok, state_payload(Row1)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 轮询 DTO（T21 TalkRecordExportArtifact 消费形状：
%% export_id/state/format/expires_at）。
-spec state_payload(map()) -> map().
state_payload(Row) ->
    #{
        <<"export_id">> => maps:get(<<"id">>, Row),
        <<"state">> => maps:get(<<"state">>, Row),
        <<"format">> => maps:get(<<"format">>, Row),
        <<"expires_at">> => rfc3339_or_null(maps:get(<<"expires_at">>, Row, null)),
        <<"job_id">> => maps:get(<<"job_id">>, Row, null),
        <<"created_at">> => maps:get(<<"created_at">>, Row)
    }.

%% ===================================================================
%% GET /:id/exports/:export_id/download（开票；§10.3 每次重验）
%% ===================================================================

%% @doc 下载开票：重验 记录 ACL + requester 绑定 + 未过期（软删已被
%% visible 排除）→ 签发短时下载票据（**携带签发时 acl_revision**——
%% 撤销版本绑定进票据，验票时动态比对，§11.3 撤权阻止分段访问）。
%% 返回 {ok, #{url, ticket, expiry, expires_at, export_id}} | {error, Reason}。
%% 已导出副本不可收回（§11.3）：本端点只控后续签发，历史字节无回收。
-spec download(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
download(Uid, RecordId, ExportId) ->
    case export_access(Uid, RecordId, ExportId) of
        {ok, VisRow, Row} ->
            download_ready(VisRow, Row);
        {error, Reason} ->
            {error, Reason}
    end.

-spec download_ready(map(), map()) -> {ok, map()} | {error, term()}.
download_ready(VisRow, #{<<"state">> := <<"ready">>} = Row) ->
    case expired_row(Row) of
        true ->
            _ = elib_pg:with_conn(fun(Conn) ->
                talk_record_export_repo:expire_tx(Conn, maps:get(<<"id">>, Row))
            end),
            {error, export_expired};
        false ->
            talk_record_export_download:issue(
                maps:get(<<"record_id">>, Row),
                maps:get(<<"id">>, Row),
                maps:get(<<"requester_id">>, Row),
                maps:get(<<"expires_at">>, Row),
                maps:get(<<"acl_revision">>, VisRow, 1)
            );
        _ ->
            %% expires_at 为 NULL（数据残缺）：按到期拒绝（fail-closed）
            {error, export_expired}
    end;
download_ready(_VisRow, #{<<"state">> := <<"queued">>}) ->
    {error, export_not_ready};
download_ready(_VisRow, #{<<"state">> := <<"failed">>}) ->
    {error, export_failed};
download_ready(_VisRow, _) ->
    {error, export_expired}.

%% @private requester + visible 访问（轮询/下载共用门禁）：
%% 无 ACL → not_found；有 ACL 非 requester → forbidden；requester 且
%% visible → {ok, 记录行（含 acl_revision）, 导出行}（软删记录 visible
%% 失败 → not_found，§4 删除即禁下载）。记录行供开票取签发时
%% acl_revision（P1：撤销版本绑定进票据）。
-spec export_access(integer(), integer(), integer()) ->
    {ok, map(), map()} | {error, atom()}.
export_access(Uid, RecordId, ExportId) ->
    elib_pg:with_tx(
        fun(Conn) ->
            case authorize_tx(Conn, Uid, RecordId) of
                {ok, VisRow} ->
                    case talk_record_export_repo:find_tx(Conn, RecordId, ExportId) of
                        {ok, #{<<"requester_id">> := Uid} = ExportRow} -> {ok, VisRow, ExportRow};
                        {ok, _} -> {error, forbidden};
                        Error -> Error
                    end;
                Error ->
                    Error
            end
        end,
        [{reraise, false}]
    ).

%% @private 到期惰性清理（§10.3）：过期行置 expired + content=NULL 后
%% 返回（{ok, Row}）；未过期原样返回。
-spec expire_if_due(map()) -> {ok, map()}.
expire_if_due(Row) ->
    case
        expired_row(Row) =:= true andalso
            lists:member(maps:get(<<"state">>, Row), [<<"ready">>, <<"failed">>])
    of
        true ->
            case
                elib_pg:with_conn(fun(Conn) ->
                    talk_record_export_repo:expire_tx(Conn, maps:get(<<"id">>, Row))
                end)
            of
                {ok, Expired} -> {ok, Expired};
                {error, _} -> {ok, Row#{<<"state">> => <<"expired">>, <<"content">> => null}}
            end;
        false ->
            {ok, Row}
    end.

%% @private 行到期判定：expires_at 非空且 <= 当前秒（timestamptz 由
%% epgsql 回 binary/integer；统一经 rfc3339 折算比较）。
-spec expired_row(map()) -> boolean() | undefined.
expired_row(#{<<"expires_at">> := null}) ->
    undefined;
expired_row(#{<<"expires_at">> := undefined}) ->
    undefined;
expired_row(#{<<"expires_at">> := ExpiresAt}) when is_binary(ExpiresAt) ->
    case elib_dt:rfc3339_to(ExpiresAt, second) of
        Sec when is_integer(Sec) -> Sec =< os:system_time(second);
        _ -> undefined
    end;
expired_row(#{<<"expires_at">> := Sec}) when is_integer(Sec) ->
    Sec =< os:system_time(second);
expired_row(_) ->
    undefined.

-spec rfc3339_or_null(term()) -> binary() | null.
rfc3339_or_null(null) -> null;
rfc3339_or_null(undefined) -> null;
rfc3339_or_null(Bin) when is_binary(Bin) -> Bin;
rfc3339_or_null(Sec) when is_integer(Sec) -> elib_dt:to_rfc3339(Sec).

%% ===================================================================
%% 配置
%% ===================================================================

%% @doc 导出对象寿命（秒；默认 86400=24h，钳制 300..604800——"短期自动
%% 清理"§10.3；下载票据寿命独立且更短，见 talk_record_export_download）。
-spec export_ttl_seconds() -> pos_integer().
export_ttl_seconds() ->
    case config_ds:env(imboy, talk_record_export_ttl_seconds, ?DEFAULT_TTL_SECONDS) of
        N when is_integer(N), N >= 300, N =< 604800 -> N;
        _ -> ?DEFAULT_TTL_SECONDS
    end.

%% ===================================================================
%% 入参解析
%% ===================================================================

%% @doc 创建请求体：format ∈ txt|md（缺省 txt）；include_audio 缺省
%% false，true → 422（V1 排除，§11.4）。
-spec parse_create(map()) -> {ok, map()} | {error, atom()}.
parse_create(Body) when is_map(Body) ->
    Format = maps:get(<<"format">>, Body, <<"txt">>),
    IncludeAudio = maps:get(<<"include_audio">>, Body, false),
    case {valid_format(Format), IncludeAudio} of
        {true, false} ->
            {ok, #{format => Format}};
        {true, true} ->
            {error, export_audio_unsupported};
        {false, _} ->
            {error, invalid_export_format};
        _ ->
            {error, invalid_body}
    end;
parse_create(_) ->
    {error, invalid_body}.

-spec valid_format(term()) -> boolean().
valid_format(F) when F =:= <<"txt">>; F =:= <<"md">> -> true;
valid_format(_) -> false.

%% ===================================================================
%% 内部
%% ===================================================================

-spec export_resource(integer()) -> binary().
export_resource(RecordId) ->
    <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/exports">>.

-spec idempotency_ttl_seconds() -> pos_integer().
idempotency_ttl_seconds() ->
    case config_ds:env(imboy, talk_record_idempotency_ttl_seconds, 604800) of
        N when is_integer(N), N > 0 -> N;
        _ -> 604800
    end.

-spec export_payload(map(), integer()) -> map().
export_payload(Row, JobId) ->
    #{
        <<"export_id">> => maps:get(<<"id">>, Row),
        <<"state">> => maps:get(<<"state">>, Row),
        <<"format">> => maps:get(<<"format">>, Row),
        <<"job_id">> => JobId,
        <<"expires_at">> => rfc3339_or_null(maps:get(<<"expires_at">>, Row, null))
    }.

must(ok) -> ok;
must({error, Reason}) -> throw({talk_record_rollback, {error, Reason}}).
