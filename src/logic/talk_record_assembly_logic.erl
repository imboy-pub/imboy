-module(talk_record_assembly_logic).
-moduledoc "面聊音频拼装（T12）：manifest 构建/固化、缺段校验、processing lease、迟到结果 CAS 拒绝。".
%%%
% assemble job 的执行与发布逻辑（设计 §3/§4/§10/§11.2）。
%
% manifest（拼装清单，§10.1 唯一事实源登记）：
%   * 输入 = record 全部已确认 audio_part，按 (capture_id, seq) 稳定序；
%   * 每段 {seq, part_id, attachment_id, sha256, byte_size, offset_ms,
%     duration_ms}——offset 为有效音频时间轴累计位置（§11.1），duration =
%     end_ms - start_ms（排除暂停墙钟）；
%   * manifest_hash = SHA-256(canonical JSON)；canonical 复用
%     enterprise_cursor_v2（与幂等 digest 同一规范），段序稳定 → 哈希确定。
%
% V1 worker 产生真实 AAC 与 JSON manifest 两个生命周期附件后原子发布。
% 三段式执行（claim 与 finish 分事务，moya_ai_worker 范式）：
%   prepare_tx —— 记录门禁（未删除）+ manifest 重算比对 job 输入绑定 +
%                 每段 processing lease（§10.3 任务期间源文件有效引用）；
%   finish_tx  —— CAS（state=running+lease_token）+ 迟到结果拒绝（记录
%                 未删除 + manifest_hash/content_revision 未过时，§11.2）
%                 + 永久 audio_part 引用 + 释放 job lease + 发布
%                 media_status='available' + job succeeded，同一事务原子。
% 取消/删除后 finish CAS 落空或门禁拒绝 → 迟到结果不发布；已授 lease 由
% cancel 释放或到期清扫（release_expired_leases_tx，§10.3）。
%%%

-export([
    %% manifest 构建（纯函数 + 事务内持久化）
    build_manifest/2,
    confirmed_parts_tx/2,
    missing_sequences/2,
    persist_tx/4,
    find_tx/2,
    %% job 执行（worker 调用）
    prepare_tx/2,
    finish_tx/3,
    release_job_leases_tx/3,
    %% 工具
    lease_business_key/2, lease_business_key/3,
    part_business_key/1,
    manifest_hash_of/1,
    processing_lease_seconds/0
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(DEFAULT_LEASE_SECONDS, 900).
-define(PART_COLUMNS,
    <<"p.id,p.record_id,p.capture_id,p.seq,p.start_ms,p.end_ms,",
        "p.capture_offset_ms,p.attachment_id,p.sha256,p.byte_size,", "p.codec,p.upload_status">>
).

%% ===================================================================
%% manifest 构建
%% ===================================================================

%% @doc 纯函数：有序段清单 → manifest（含 manifest_hash）。
%% Parts 必须已按 (capture_id, seq) 排序（confirmed_parts_tx 保证）；
%% offset_ms 按段序累计 duration，乱序注册的分段在此归位。
-spec build_manifest(integer(), [map()]) -> map().
build_manifest(CaptureId, Parts) ->
    {SegsRev, TotalMs, TotalBytes, _Off} =
        lists:foldl(
            fun(Part, {Acc, Ms, Bytes, Off}) ->
                Duration = maps:get(<<"end_ms">>, Part) - maps:get(<<"start_ms">>, Part),
                Seg = #{
                    <<"seq">> => maps:get(<<"seq">>, Part),
                    <<"part_id">> => maps:get(<<"id">>, Part),
                    <<"attachment_id">> => maps:get(<<"attachment_id">>, Part),
                    <<"sha256">> => maps:get(<<"sha256">>, Part),
                    <<"byte_size">> => maps:get(<<"byte_size">>, Part),
                    <<"offset_ms">> => Off,
                    <<"duration_ms">> => Duration
                },
                {
                    [Seg | Acc],
                    Ms + Duration,
                    Bytes + maps:get(<<"byte_size">>, Part),
                    Off + Duration
                }
            end,
            {[], 0, 0, 0},
            Parts
        ),
    Manifest = #{
        <<"v">> => 1,
        <<"capture_id">> => CaptureId,
        <<"parts">> => lists:reverse(SegsRev),
        <<"total_duration_ms">> => TotalMs,
        <<"total_bytes">> => TotalBytes
    },
    Manifest#{<<"manifest_hash">> => manifest_hash_of(Manifest)}.

%% @doc manifest 整体指纹：canonical JSON（enterprise_cursor_v2 规范，
%% 与幂等 digest 同源）的 SHA-256 十六进制。构建时排除 manifest_hash 自身。
-spec manifest_hash_of(map()) -> binary().
manifest_hash_of(#{<<"manifest_hash">> := _} = Manifest) ->
    manifest_hash_of(maps:remove(<<"manifest_hash">>, Manifest));
manifest_hash_of(Manifest) ->
    case enterprise_cursor_v2:canonical_json(Manifest) of
        {ok, Canonical} ->
            binary:encode_hex(crypto:hash(sha256, Canonical), lowercase);
        {error, non_canonical} ->
            erlang:error({manifest_not_canonical, Manifest})
    end.

%% @doc 事务内取全部已确认分段，按 (capture_id, seq) 稳定序（乱序补传归位）。
-spec confirmed_parts_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
confirmed_parts_tx(Conn, RecordId) ->
    Sql =
        <<"SELECT ", ?PART_COLUMNS/binary, " FROM talk_record_audio_part p",
            " WHERE p.record_id = $1 AND p.upload_status = 'confirmed'",
            " ORDER BY p.capture_id ASC, p.seq ASC">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 纯函数：0..LastSeq 中缺失的 seq 清单（含未确认——调用方只喂
%% confirmed 段，未确认即缺）。
-spec missing_sequences([map()], non_neg_integer()) -> [integer()].
missing_sequences(ConfirmedParts, LastSeq) ->
    Present = [maps:get(<<"seq">>, P) || P <- ConfirmedParts],
    [S || S <- lists:seq(0, LastSeq), not lists:member(S, Present)].

%% @doc manifest 固化（§3：固化在 complete 事务中；V1 单 capture 单行，
%% UNIQUE(record_id) 幂等 upsert——同 hash 重放刷新，追加分采集聚合留后续）。
%% 注：job_id 列不在本函数写——manifest 先固化、assemble job 后投递，
%% 由 complete 事务内 enqueue 成功后回填（Q-L1，见 job_logic complete_jobs_tx）。
-spec persist_tx(any(), integer(), integer(), map()) ->
    {ok, integer(), boolean()} | {error, term()}.
persist_tx(Conn, RecordId, CaptureId, Manifest) ->
    Id = elib_tsid:generate(attachment),
    Segments = maps:get(<<"parts">>, Manifest),
    Data = #{
        <<"id">> => Id,
        <<"record_id">> => RecordId,
        <<"capture_id">> => CaptureId,
        <<"manifest_hash">> => maps:get(<<"manifest_hash">>, Manifest),
        %% epgsql 默认 json codec 只透传二进制（json_mod 未配置时非 binary
        %% 参数会坏连接）——此处显式 jsone 编码，行为不依赖连接 codec 配置
        <<"segments">> => jsone:encode(Segments),
        <<"part_count">> => length(Segments),
        <<"total_duration_ms">> => maps:get(<<"total_duration_ms">>, Manifest),
        <<"total_bytes">> => maps:get(<<"total_bytes">>, Manifest)
    },
    {Sql0, Params} = elib_pg_sql:insert(<<"talk_record_assembly">>, Data),
    %% upsert：complete 幂等重放/人工重跑刷新同一行（记录级唯一）
    Sql = [
        Sql0,
        <<" ON CONFLICT (record_id) DO UPDATE SET">>,
        <<" capture_id = EXCLUDED.capture_id,">>,
        <<" manifest_hash = EXCLUDED.manifest_hash,">>,
        <<" segments = EXCLUDED.segments,">>,
        <<" part_count = EXCLUDED.part_count,">>,
        <<" total_duration_ms = EXCLUDED.total_duration_ms,">>,
        <<" total_bytes = EXCLUDED.total_bytes,">>,
        <<" updated_at = now() RETURNING id, (xmax = 0) AS inserted">>
    ],
    case elib_pg:query(Conn, iolist_to_binary(Sql), Params) of
        {ok, [#{<<"id">> := RowId, <<"inserted">> := Inserted}]} ->
            {ok, RowId, Inserted =:= true};
        {ok, [#{<<"id">> := RowId}]} ->
            %% PG12+ 恒返回 inserted；防御旧形态
            {ok, RowId, false};
        {error, Reason} ->
            {error, Reason}
    end.

-spec find_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, RecordId) ->
    Sql =
        <<"SELECT id, record_id, capture_id, manifest_hash, segments,",
            " part_count, total_duration_ms, total_bytes, audio_attachment_id,",
            " created_at, updated_at FROM talk_record_assembly WHERE record_id = $1">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% job 执行：prepare（校验 + processing lease）
%% ===================================================================

%% @doc assemble 预备阶段（事务内）：
%%   1. 记录门禁——软删 → record_deleted（终态）；
%%   2. manifest 重算——confirmed 段空 → missing_sequences（终态）；
%%   3. 输入版本绑定——重算 hash ≠ job.input_manifest_hash → input_stale
%%      （终态，迟到/过时结果不得发布，§11.2）；
%%   4. 每段 processing lease（§10.3：任务期间源文件有效引用/期限明确）。
%% 返回 {ok, #{parts => Parts, manifest => Manifest}} | {error, Reason}。
%% Reason 分类：record_deleted/missing_sequences/input_stale 为终态；
%% lease 授予类失败按瞬时（worker 退避重试）。
-spec prepare_tx(any(), map()) -> {ok, map()} | {error, term()}.
prepare_tx(Conn, #{<<"record_id">> := RecordId} = Job) ->
    case record_state_tx(Conn, RecordId) of
        {ok, live} ->
            prepare_parts(Conn, Job);
        {ok, deleted} ->
            {error, record_deleted};
        {error, Reason} ->
            %% Q-M2 修复：DB 读错误不再误判为 record_deleted——瞬时抖动若走
            %% 终态 cancelled（不可 retry）会让 media 永停 assembling 且
            %% error_code 误导。db_error 由 worker 分类为瞬时 → retry_wait
            %% 退避（attempt 保留），恢复后自动重投递。
            {error, Reason}
    end.

prepare_parts(Conn, #{<<"record_id">> := RecordId, <<"input_manifest_hash">> := InputHash} = Job) ->
    case confirmed_parts_tx(Conn, RecordId) of
        {ok, []} ->
            {error, missing_sequences};
        {ok, [First | _] = Parts} ->
            CaptureId = maps:get(<<"capture_id">>, First),
            Manifest = build_manifest(CaptureId, Parts),
            case maps:get(<<"manifest_hash">>, Manifest) =:= InputHash of
                true ->
                    grant_part_leases(Conn, Job, Parts, Manifest);
                false ->
                    {error, input_stale}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

grant_part_leases(Conn, Job, Parts, Manifest) ->
    RecordId = maps:get(<<"record_id">>, Job),
    case grant_leases_loop(Conn, RecordId, Job, Parts) of
        ok ->
            {ok, #{parts => Parts, manifest => Manifest}};
        {error, _} = E ->
            %% 首个授予失败即短路（同事务后续语句在 aborted 事务上必然
            %% 25P02）；授予失败按瞬时退避（DB 抖动/锁冲突），资产行缺失类
            %% （attachment_not_found/confirm_missing/tombstone）由 worker
            %% 分类为 part_attachment_missing 终态。
            E
    end.

grant_leases_loop(_Conn, _RecordId, _Job, []) ->
    ok;
grant_leases_loop(Conn, RecordId, Job, [Part | Rest]) ->
    case
        talk_record_attachment_adapter:grant_processing_lease_tx(
            Conn,
            RecordId,
            maps:get(<<"attachment_id">>, Part),
            <<"audio_part">>,
            attempt_part_key(Job, Part),
            processing_lease_seconds()
        )
    of
        {ok, _, _} ->
            grant_leases_loop(Conn, RecordId, Job, Rest);
        {error, Reason} ->
            {error, {lease_grant_failed, [Reason]}}
    end.

%% ===================================================================
%% job 执行：finish（CAS 发布 + 迟到结果拒绝）
%% ===================================================================

%% @doc assemble 发布阶段（事务内，与 prepare 分事务）：
%%   1. job CAS——state='running' AND lease_token：取消/回收/他者接管 →
%%      lease_lost，不发布；
%%   2. 迟到结果拒绝——记录未删除（FOR UPDATE 重读）+ 输入版本未过时
%%      （manifest_hash 与固化行一致、content_revision 未推进，§11.2）；
%%   3. 发布——每段永久 audio_part 业务引用（bind_tx 幂等，GC 永久可见）
%%      + 释放 job processing lease（成功后任务期引用退场）+
%%      media_status='available' + job succeeded。
%% 返回 {ok, published} | {error, lease_lost | input_stale | record_deleted | term()}。
-spec finish_tx(any(), map(), map()) -> {ok, published} | {error, term()}.
finish_tx(Conn, Job, Prepared) ->
    JobId = maps:get(<<"id">>, Job),
    RecordId = maps:get(<<"record_id">>, Job),
    LeaseToken = maps:get(<<"lease_token">>, Job),
    case lock_job_tx(Conn, JobId, LeaseToken) of
        {ok, LockedJob} ->
            finish_locked_tx(Conn, LockedJob, RecordId, Prepared);
        {error, lease_lost} ->
            {error, lease_lost}
    end.

%% @private FOR UPDATE 锁 job 行并复核 CAS（防 TOCTOU：后续 UPDATE 以行锁串行）。
lock_job_tx(Conn, JobId, LeaseToken) ->
    Sql =
        <<"SELECT id, record_id, kind, input_manifest_hash,",
            " input_transcript_version, input_content_revision, state, attempt, lease_token",
            " FROM talk_record_job WHERE id=$1 AND state='running' AND lease_token=$2 AND "
            "lease_until>clock_timestamp()", " FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [JobId, LeaseToken]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, lease_lost};
        {error, Reason} -> {error, Reason}
    end.

finish_locked_tx(Conn, Job, RecordId, #{parts := Parts} = Prepared) ->
    case fencing_check_tx(Conn, Job, RecordId) of
        ok ->
            case talk_record_assembly_artifact:publish_tx(Conn, RecordId, Prepared) of
                ok -> publish_tx(Conn, Job, RecordId, Parts);
                Error -> Error
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 迟到结果拒绝（§3 删除后迟到/§11.2 输入过时）：
%% 记录行 FOR UPDATE（与删除事务互斥）；deleted_at 非空 → record_deleted；
%% job 绑定 manifest_hash ≠ 固化行当前 hash，或 input_content_revision ≠
%% 记录当前 content_revision → input_stale。
fencing_check_tx(Conn, Job, RecordId) ->
    RecSql = <<"SELECT deleted_at, content_revision FROM talk_record WHERE id=$1 FOR UPDATE">>,
    case elib_pg:query(Conn, RecSql, [RecordId]) of
        {ok, [#{<<"deleted_at">> := null, <<"content_revision">> := ContentRev}]} ->
            check_binding_tx(Conn, Job, ContentRev);
        {ok, [#{<<"deleted_at">> := _}]} ->
            {error, record_deleted};
        {ok, []} ->
            {error, record_deleted};
        {error, Reason} ->
            {error, Reason}
    end.

check_binding_tx(Conn, Job, ContentRev) ->
    RecordId = maps:get(<<"record_id">>, Job),
    InputHash = maps:get(<<"input_manifest_hash">>, Job),
    InputContentRev = maps:get(<<"input_content_revision">>, Job),
    HashOk =
        (InputHash =:= null) orelse
            (case find_tx(Conn, RecordId) of
                {ok, #{<<"manifest_hash">> := InputHash}} -> true;
                _ -> false
            end),
    RevOk = (InputContentRev =:= null) orelse InputContentRev =:= ContentRev,
    case {HashOk, RevOk} of
        {true, true} -> ok;
        _ -> {error, input_stale}
    end.

publish_tx(Conn, Job, RecordId, Parts) ->
    %% 1) 每段永久 audio_part 业务引用（幂等；成功后 GC 永久可见——
    %%    processing lease 退场，源音频成为记录的常驻受控引用）
    Binds = [bind_part_ref_tx(Conn, RecordId, Part) || Part <- Parts],
    case [E || {error, E} <- Binds] of
        [] ->
            publish_status_tx(Conn, Job, RecordId, Parts);
        Errors ->
            {error, {bind_failed, Errors}}
    end.

bind_part_ref_tx(Conn, RecordId, Part) ->
    talk_record_attachment_ref_repo:bind_tx(Conn, #{
        record_id => RecordId,
        attachment_id => maps:get(<<"attachment_id">>, Part),
        capture_id => maps:get(<<"capture_id">>, Part),
        role => <<"audio_part">>,
        business_key => part_business_key(maps:get(<<"id">>, Part))
    }).

publish_status_tx(Conn, Job, RecordId, Parts) ->
    JobId = maps:get(<<"id">>, Job),
    LeaseToken = maps:get(<<"lease_token">>, Job),
    %% 2) 释放 job processing lease（成功路径；§10.3 取消/超时另路径）
    ok = release_job_leases_tx(Conn, Job, Parts),
    %% 3) 发布 media_status=available（processing 投影，不动 record.revision §11.2）
    _ = elib_pg:query(
        Conn,
        <<"UPDATE talk_record SET media_status='available', updated_at=now()",
            " WHERE id=$1 AND deleted_at IS NULL">>,
        [RecordId]
    ),
    %% 4) job succeeded（同事务；失败由整体回滚承载）
    case talk_record_job_repo:finish_success_tx(Conn, JobId, LeaseToken) of
        ok -> {ok, published};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% lease 释放
%% ===================================================================

%% @doc 释放该 job 授予的全部 processing lease（取消/失败清理路径；
%% 成功路径在 publish_tx 内同事务调用）。
-spec release_job_leases_tx(any(), map() | integer(), [map()]) -> ok.
release_job_leases_tx(Conn, Job, Parts) when is_map(Job) ->
    lists:foreach(
        fun(Part) ->
            case
                talk_record_attachment_adapter:cancel_processing_lease_tx(
                    Conn,
                    maps:get(<<"record_id">>, Part),
                    <<"audio_part">>,
                    attempt_part_key(Job, Part)
                )
            of
                {ok, _} -> ok;
                {error, R} -> throw({talk_assembly_abort, R})
            end
        end,
        Parts
    ),
    ok;
%% Explicit whole-job cancellation only: release every attempt, plus legacy keys.
release_job_leases_tx(_Conn, _JobId, []) ->
    ok;
release_job_leases_tx(Conn, JobId, [Part | _]) when is_integer(JobId) ->
    case
        talk_record_attachment_ref_repo:release_job_processing_tx(
            Conn,
            maps:get(<<"record_id">>, Part),
            JobId
        )
    of
        {ok, _} -> ok;
        {error, R} -> throw({talk_assembly_abort, R})
    end.

attempt_part_key(Job, Part) ->
    lease_business_key(
        maps:get(<<"id">>, Job),
        maps:get(<<"attempt">>, Job),
        maps:get(<<"id">>, Part)
    ).

%% ===================================================================
%% 工具
%% ===================================================================

%% job 处理引用 business_key（UNIQUE(record_id, role, business_key) 域内
%% 稳定：同 job 同段重授幂等，异 job 不互撞）。
-spec lease_business_key(integer(), integer()) -> binary().
lease_business_key(JobId, PartId) ->
    <<"assemble_job:", (integer_to_binary(JobId))/binary, ":part:",
        (integer_to_binary(PartId))/binary>>.

%% A released reference is immutable; every claimed attempt owns fresh leases.
lease_business_key(JobId, Attempt, PartId) ->
    <<"assemble_job:", (integer_to_binary(JobId))/binary, ":attempt:",
        (integer_to_binary(Attempt))/binary, ":part:", (integer_to_binary(PartId))/binary>>.

%% 段永久业务引用 key（与 T06 adapter audio_part_business_key 同构——
%% 上传侧 confirm 绑定与 assemble 兜底绑定收敛到同一 key，幂等不重复）。
-spec part_business_key(integer()) -> binary().
part_business_key(PartId) ->
    <<"audio_part:", (integer_to_binary(PartId))/binary>>.

%% processing lease 时限（§10.3 期限明确）：now + N 秒；默认 900。
-spec processing_lease_seconds() -> pos_integer().
processing_lease_seconds() ->
    case config_ds:env(imboy, talk_record_processing_lease_seconds, ?DEFAULT_LEASE_SECONDS) of
        N when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_LEASE_SECONDS
    end.

%% @private 记录存在性三态（Q-M2 修复）：live / deleted / {error, db_error}。
%% DB 读错误不得误判为 deleted（终态 cancelled 不可 retry、media 永停）；
%% 行不存在（删除流程级联残缺的防御分支）按 deleted 处理。
-spec record_state_tx(any(), integer()) -> {ok, live | deleted} | {error, db_error}.
record_state_tx(Conn, RecordId) ->
    case elib_pg:query(Conn, <<"SELECT deleted_at FROM talk_record WHERE id=$1">>, [RecordId]) of
        {ok, [#{<<"deleted_at">> := null}]} ->
            {ok, live};
        {ok, [_]} ->
            {ok, deleted};
        {ok, []} ->
            {ok, deleted};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_assembly_record_load_failed, RecordId, Reason]),
            {error, db_error}
    end.
