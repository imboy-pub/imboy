-module(talk_record_job_worker).
-moduledoc "面聊持久处理 job worker（T12）：gen_server 壳 + claim 分事务 + kind 分派 + 过期租约 reaper。".
%%%
% talk_record_job 持久 worker（仿 moya_ai_worker 的 claim/finish 分事务 +
% attachment_delete_worker 的 gen_server 壳/SKIP LOCKED/退避/禁用默认）：
%
%   tick → run_batch（≤BATCH_LIMIT）→ 每 job：claim tx（租约）→ 分派 →
%   finish tx（CAS：state='running' AND lease_token）。处理期间不持锁；
%   进程崩溃留 running 行 → reaper 按 lease_until 零副作用回收（attempt
%   保留，毒行经 attempts 上限终止）。
%
% kind 分派（五 kind 骨架，§2）：
%   * assemble   —— 完整实现：prepare（记录门禁 + manifest 重算比对 +
%                   每段 processing lease）→ finish（CAS + 迟到结果拒绝 +
%                   真实AAC/JSON附件、永久引用 + media available）。
%   * transcribe —— 受控 adapter 占位：调用点 invoke_transcribe_adapter/2
%   * summarize  —— 受控 adapter 占位：invoke_summarize_adapter/2
%                   处理器未选型（T03 B：全仓无 ASR/说话人分离设施）→
%                   调用点返回 {error, waiting_processor}，job 转 retry_wait
%                   挂起（suspended=true，claim 谓词排除）——不伪造成功、
%                   不无限退避烧 attempts；T13/T14 换真实 adapter 后
%                   unsuspend_kind_tx 恢复调度。
%   * export     —— 骨架（talk_record_job_logic:execute_export_tx，T19 接线）
%   * purge      —— 骨架（talk_record_job_logic:execute_purge_tx，T20 接线）
%                   二者当前挂起 kind_not_wired；T12 无任何入口投递这两种
%                   kind，防御性分派不产生副产物。
%
% 默认禁用（{imboy, talk_record_job_worker_enabled, false}，对齐
% attachment_delete_worker 惯例）；supervisor 挂接为集成项（imboy_sup 不在
% 本卡写权限内，报告登记）。测试经 run_once/0、run_once_tx/1 直驱。
%%%

-behaviour(gen_server).

%% 生产/测试入口
-export([start_link/0, kick/0, run_once/0, run_batch/1, reap_expired/0, reap_expired_tx/1]).
-export([reconcile_tick/0, refresh_cleanup_pending/0]).
%% 纯函数（退避计算；eunit 直验）
-export([backoff_seconds/1]).
%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

-define(BATCH_LIMIT, 20).
-define(TICK_MS, 5000).
-define(MAX_TICK_MS, 60000).
-define(DEFAULT_LEASE_SECONDS, 120).
-define(DEFAULT_MAX_ATTEMPTS, 6).
-define(RETRY_BASE_SECONDS, 2).
-define(RETRY_MAX_SECONDS, 60).
%% T20：单 tick 删除状态回读上限（防御大库刷 tick）
-define(CLEANUP_REFRESH_LIMIT, 20).
%% prepare/finish 的结构性失败：同输入重试无意义，直接终态（白名单外
%% 的人工重试同样关闭）。
-define(TERMINAL_ERRORS, [
    record_deleted,
    input_stale,
    missing_sequences,
    manifest_hash_conflict,
    part_attachment_missing,
    audio_size_limit,
    audio_part_integrity_mismatch,
    audio_render_failed,
    audio_render_invalid_output,
    audio_render_invalid_duration,
    audio_duration_mismatch,
    artifact_integrity_mismatch
]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 投递后立即唤醒（best-effort：worker 未启动时静默忽略，任务由
%% 周期 tick 兜底）。
-spec kick() -> ok.
kick() ->
    try
        gen_server:cast(?MODULE, kick)
    catch
        _:_ -> ok
    end.

%% @doc 池入口：claim → 分派 → finish（单条；claim 与 finish 分开事务，
%% 处理期间不持锁——moya_ai_worker 范式）。
-spec run_once() -> {ok, done | map()} | {error, term()}.
run_once() ->
    Token = new_token(),
    case
        elib_pg:with_tx(
            fun(Conn) ->
                talk_record_job_repo:claim_next_tx(Conn, lease_owner(), Token, lease_seconds())
            end,
            [{reraise, false}]
        )
    of
        {ok, Job} ->
            dispatch_job(Job);
        {error, empty} ->
            {ok, done};
        {error, Reason} ->
            ?WARN_LOG("talk_record_job_worker claim error ~p", [Reason]),
            {error, claim_error}
    end.

%% @doc 批处理（tick 路径）：循环单条直至 empty/上限。
-spec run_batch(non_neg_integer()) -> {ok, non_neg_integer()} | {error, term()}.
run_batch(0) ->
    {ok, 0};
run_batch(N) when is_integer(N), N > 0 ->
    case run_once() of
        {ok, done} ->
            {ok, 0};
        {ok, _Outcome} ->
            case run_batch(N - 1) of
                {ok, Done} -> {ok, Done + 1};
                {error, R} -> {error, R}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 过期租约回收（池入口，tick/手动；零副作用：只回 queued）。
-spec reap_expired() -> {ok, non_neg_integer()} | {error, term()}.
reap_expired() ->
    elib_pg:with_tx(fun(Conn) -> reap_expired_tx(Conn) end, [{reraise, false}]).

%% @doc 直连形态（测试）。
-spec reap_expired_tx(any()) -> {ok, non_neg_integer()} | {error, term()}.
reap_expired_tx(Conn) ->
    talk_record_job_repo:reap_expired_tx(Conn).

%% @doc 退避秒数：base * 2^(attempt-1)，上限 60（msg_store_worker 同款
%% 指数曲线）。attempt 从 1 起（claim 时自增）。
-spec backoff_seconds(pos_integer()) -> pos_integer().
backoff_seconds(Attempt) when is_integer(Attempt), Attempt >= 1 ->
    min(?RETRY_BASE_SECONDS * (1 bsl min(Attempt - 1, 6)), ?RETRY_MAX_SECONDS).

%%%===================================================================
%%% kind 分派
%%%===================================================================

%% @private 单 job 全流程。崩溃兜底：任何异常收敛为 worker_crash 退避
%% 重试（attempt 上限兜底防毒行永动），gen_server 不因单 job 崩溃。
dispatch_job(Job) ->
    try
        case attempt_exhausted(maps:get(<<"attempt">>, Job, 1)) of
            true ->
                case finish_terminal(Job, <<"attempts_exhausted">>) of
                    ok -> {ok, #{outcome => failed, error_code => <<"attempts_exhausted">>}};
                    {error, lease_lost} -> {ok, #{outcome => late_result_rejected}};
                    E -> E
                end;
            false ->
                dispatch_kind(maps:get(<<"kind">>, Job), Job)
        end
    catch
        _:Unexpected ->
            ?WARN_LOG("talk_record_job_worker job ~p crashed ~p", [
                maps:get(<<"id">>, Job), Unexpected
            ]),
            finish_transient(Job, <<"worker_crash">>),
            {ok, #{outcome => crash}}
    end.

dispatch_kind(<<"assemble">>, Job) ->
    run_assemble(Job);
dispatch_kind(Kind, Job) when Kind =:= <<"transcribe">>; Kind =:= <<"summarize">> ->
    run_model_job(Kind, Job);
dispatch_kind(<<"export">>, Job) ->
    run_export(Job);
dispatch_kind(<<"purge">>, Job) ->
    %% T20 接线：删除协调执行体（只协调引用释放/删除意图/状态推进，
    %% 不执行存储 DELETE——§10.4）
    run_purge(Job);
dispatch_kind(_Unknown, Job) ->
    finish_terminal(Job, <<"unknown_kind">>),
    {ok, #{outcome => failed, error_code => <<"unknown_kind">>}}.

%% -------------------------------------------------------------------
%% purge：协调清理（T20 接线；claim/finish 分事务，处理期不持 job 锁）
%% -------------------------------------------------------------------

run_export(Job) ->
    case talk_record_export_job:run(Job) of
        {ok, Result} ->
            {ok, #{outcome => succeeded, kind => <<"export">>, detail => Result}};
        {error, lease_lost} ->
            {ok, #{outcome => late_result_rejected}};
        {error, Reason} when
            Reason =:= record_deleted;
            Reason =:= input_stale;
            Reason =:= not_found;
            Reason =:= forbidden;
            Reason =:= export_disabled_by_policy
        ->
            finish_terminal(Job, atom_to_binary(Reason, utf8)),
            {ok, #{outcome => failed, error_code => Reason}};
        _ ->
            finish_transient(Job, <<"export_failed">>),
            {ok, #{outcome => requeued, kind => <<"export">>}}
    end.

run_purge(Job) ->
    case
        elib_pg:with_tx(
            fun(Conn) -> talk_record_cleanup_logic:execute_purge_tx(Conn, Job) end,
            [{reraise, false}]
        )
    of
        {ok, Result} ->
            %% 协调成功 = purge job 终态 succeeded（cleanup_status 的
            %% complete 推进归 refresh 物理回读——队列进度不归本 job 等待）
            finish_success(Job),
            {ok, #{outcome => succeeded, kind => <<"purge">>, detail => Result}};
        {rollback, Reason} ->
            ?WARN_LOG("talk_record_job_worker purge rollback ~p", [Reason]),
            finish_transient(Job, <<"db_error">>),
            {ok, #{outcome => requeued, kind => <<"purge">>}};
        {error, record_not_deleted} ->
            %% 未删除记录的 purge 行（防御性投递/数据异常）：终态失败，
            %% 同输入重试无意义
            finish_terminal(Job, <<"record_not_deleted">>),
            {ok, #{outcome => failed, error_code => record_not_deleted}};
        {error, Reason} ->
            ?WARN_LOG("talk_record_job_worker purge error ~p", [Reason]),
            finish_transient(Job, <<"db_error">>),
            {ok, #{outcome => requeued, kind => <<"purge">>}}
    end.

%% -------------------------------------------------------------------
%% assemble：prepare tx → finish tx（迟到结果 CAS 拒绝在 finish 内）
%% -------------------------------------------------------------------

run_assemble(Job) ->
    run_assemble_attempt(Job).

run_assemble_attempt(Job) ->
    case talk_record_assembly_job:run(Job) of
        {ok, published} ->
            {ok, #{outcome => succeeded}};
        {error, lease_lost} ->
            release_leases_best_effort(Job),
            {ok, #{outcome => late_result_rejected}};
        {error, Reason} ->
            assemble_prepare_failure(Job, Reason);
        {rollback, _} ->
            assemble_prepare_failure(Job, db_error)
    end.

%% @private prepare 结构性失败 → 终态/取消 + 释放已授 lease；
%% 授予类失败按错误成分分流（资产缺失终态 / DB 瞬时退避）。
assemble_prepare_failure(Job, Reason) ->
    case classify_prepare_error(Reason) of
        terminal ->
            case Reason of
                record_deleted ->
                    %% 删除中 job 不重新发布内容（§2）：转 cancelled 终态
                    finish_cancelled(Job),
                    release_leases_best_effort(Job),
                    {ok, #{outcome => cancelled, error_code => record_deleted}};
                _ ->
                    case finish_terminal(Job, atom_error_code(Reason)) of
                        ok ->
                            release_leases_best_effort(Job),
                            {ok, #{outcome => failed, error_code => atom_error_code(Reason)}};
                        {error, lease_lost} ->
                            {ok, #{outcome => late_result_rejected}};
                        Error ->
                            Error
                    end
            end;
        transient ->
            Code =
                case Reason of
                    {lease_grant_failed, _} -> <<"lease_grant_failed">>;
                    R when is_atom(R) -> atom_error_code(R);
                    _ -> <<"db_error">>
                end,
            finish_transient(Job, Code),
            {ok, #{outcome => requeued, reason => Code}}
    end.

%% @private 资产类错误成分（T06 adapter 返回形态）→ part_attachment_missing。
classify_prepare_error(Reason) ->
    case lists:member(Reason, ?TERMINAL_ERRORS) of
        true ->
            terminal;
        false ->
            case Reason of
                {lease_grant_failed, Errors} ->
                    AssetMissing = [
                        E
                     || E <- Errors,
                        lists:member(E, [
                            attachment_not_found,
                            confirm_missing,
                            tombstone,
                            attachment_disabled,
                            scope_mismatch,
                            already_released
                        ])
                    ],
                    case AssetMissing of
                        [] -> transient;
                        _ -> terminal
                    end;
                _ ->
                    transient
            end
    end.

%% -------------------------------------------------------------------
%% transcribe / summarize：受控 adapter 占位（处理器未选型 → waiting_processor 挂起）
%% -------------------------------------------------------------------

run_model_job(Kind, Job) ->
    case talk_record_model_job:run(Kind, Job) of
        {ok, published} ->
            {ok, #{outcome => succeeded, kind => Kind}};
        {error, waiting_processor} ->
            case talk_record_model_job:suspend(Job, <<"waiting_processor">>) of
                ok -> {ok, #{outcome => suspended, kind => Kind, error_code => waiting_processor}};
                {error, lease_lost} -> {ok, #{outcome => late_result_rejected}};
                E -> E
            end;
        {error, R} when
            R =:= processor_stop_unconfirmed;
            R =:= processor_execution_unconfirmed;
            R =:= processor_cleanup_unconfirmed;
            R =:= processor_cleanup_pending
        ->
            case talk_record_model_job:suspend(Job, atom_error_code(R)) of
                ok -> {ok, #{outcome => suspended, kind => Kind, error_code => R}};
                {error, lease_lost} -> {ok, #{outcome => late_result_rejected}};
                E -> E
            end;
        {error, lease_lost} ->
            {ok, #{outcome => late_result_rejected, reason => lease_lost}};
        {error, R} when R =:= record_deleted; R =:= input_stale ->
            talk_record_model_job:finish_failed(Job, atom_error_code(R)),
            {ok, #{outcome => late_result_rejected, reason => R}};
        {error, R} when
            R =:= processor_schema_invalid;
            R =:= processing_consent_required;
            R =:= processor_output_limit;
            R =:= processor_input_limit
        ->
            case talk_record_model_job:finish_failed(Job, atom_error_code(R)) of
                ok -> {ok, #{outcome => failed, kind => Kind, error_code => R}};
                E -> E
            end;
        {error, R} ->
            model_requeue(Job, Kind, atom_error_code(R));
        {rollback, _} ->
            model_requeue(Job, Kind, <<"db_error">>)
    end.

model_requeue(Job, Kind, Code) ->
    case finish_transient(Job, Code) of
        ok -> {ok, #{outcome => requeued, kind => Kind}};
        {ok, _, _} -> {ok, #{outcome => requeued, kind => Kind}};
        {ok, _} -> {ok, #{outcome => requeued, kind => Kind}};
        {error, lease_lost} -> {ok, #{outcome => late_result_rejected}};
        E -> E
    end.

%%%===================================================================
%%% finish 封装（CAS：lease_token 丢失一律放弃）
%%%===================================================================

%% @private 退避重试（CAS；lease 丢失即停——行已被取消/回收/接管）。
finish_transient(#{<<"kind">> := Kind} = Job, ErrorCode) when
    Kind =:= <<"transcribe">>; Kind =:= <<"summarize">>
->
    talk_record_model_job:retry(Job, ErrorCode, backoff_seconds(maps:get(<<"attempt">>, Job, 1)));
finish_transient(Job, ErrorCode) ->
    Attempt = maps:get(<<"attempt">>, Job, 1),
    Delay = backoff_seconds(Attempt),
    _ =
        elib_pg:with_tx(
            fun(Conn) ->
                talk_record_job_repo:finish_retry_tx(
                    Conn, job_id(Job), lease_token(Job), ErrorCode, Delay
                )
            end,
            [{reraise, false}]
        ),
    ok.

%% @private 终态失败。
finish_terminal(#{<<"kind">> := Kind} = Job, ErrorCode) when
    Kind =:= <<"transcribe">>; Kind =:= <<"summarize">>
->
    talk_record_model_job:finish_failed(Job, ErrorCode);
finish_terminal(#{<<"kind">> := <<"assemble">>} = Job, ErrorCode) ->
    case talk_record_assembly_job:fail(Job, ErrorCode) of
        {ok, failed} -> ok;
        Error -> Error
    end;
finish_terminal(#{<<"kind">> := <<"export">>} = Job, ErrorCode) ->
    _ = talk_record_export_job:fail(Job, ErrorCode),
    ok;
finish_terminal(Job, ErrorCode) ->
    _ =
        elib_pg:with_tx(
            fun(Conn) ->
                talk_record_job_repo:finish_failed_tx(
                    Conn, job_id(Job), lease_token(Job), ErrorCode
                )
            end,
            [{reraise, false}]
        ),
    ok.

%% @private purge 协调成功终态（独立 finish 事务，与其他 kind 同款 CAS）。
finish_success(Job) ->
    _ =
        elib_pg:with_tx(
            fun(Conn) ->
                talk_record_job_repo:finish_success_tx(Conn, job_id(Job), lease_token(Job))
            end,
            [{reraise, false}]
        ),
    ok.

%% @private running → cancelled（删除中 job 不重新发布；幂等：已终态无副作用）。
finish_cancelled(Job) ->
    _ = elib_pg:with_tx(
        fun(Conn) -> talk_record_job_repo:cancel_job_tx(Conn, job_id(Job)) end,
        [{reraise, false}]
    ),
    ok.

%%%===================================================================
%%% lease 清理
%%%===================================================================

%% @private prepare 失败（分段未知）：重读记录分段尽力释放（残余由
%% release_expired_leases_tx 到期清扫兜底）。
release_leases_best_effort(Job) ->
    RecordId = maps:get(<<"record_id">>, Job),
    _ =
        elib_pg:with_tx(
            fun(Conn) ->
                case talk_record_assembly_logic:confirmed_parts_tx(Conn, RecordId) of
                    {ok, Parts} ->
                        talk_record_assembly_logic:release_job_leases_tx(Conn, Job, Parts);
                    {error, _} ->
                        ok
                end
            end,
            [{reraise, false}]
        ),
    ok.

%%%===================================================================
%%% gen_server 壳（对齐 attachment_delete_worker：默认禁用 + 周期 tick +
%%% DB 故障有界唤醒退避）
%%%===================================================================

init(_Opts) ->
    erlang:send_after(tick_ms(), self(), tick),
    {ok, #{enabled => enabled(), fails => 0}}.

handle_info(tick, State = #{enabled := true}) ->
    %% Q-M1 修复：reaper 与批处理的 DB 失败一律容错分支（记日志+退避），
    %% 绝不 badmatch 崩溃 gen_server（原 {ok,_} = reap_expired() 在 with_tx
    %% reraise:false 返回 {error,_} 时会 badmatch——与"DB 故障有界唤醒退避"
    %% 声明矛盾，attachment_delete_worker 原型将 reaper 纳入容错路径）。
    case reconcile_tick() of
        ok ->
            erlang:send_after(tick_ms(), self(), tick),
            {noreply, State#{fails := 0}};
        {error, Reason} ->
            ?WARN_LOG("talk_record_job_worker tick degraded: ~p", [Reason]),
            %% DB 故障有界唤醒：退避翻倍（上限 MAX_TICK_MS），不无限快轮询
            Fails = maps:get(fails, State, 0) + 1,
            Delay = min(tick_ms() * (1 bsl min(Fails, 4)), ?MAX_TICK_MS),
            erlang:send_after(Delay, self(), tick),
            {noreply, State#{fails := Fails}}
    end;
handle_info(tick, State) ->
    %% 禁用态：只续 tick 不消费（与 attachment_delete_worker 同款）
    erlang:send_after(tick_ms(), self(), tick),
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.

%% @doc 单 tick 调和：reaper + 批处理 + 删除状态回读（T20：cleanup
%% pending/failed 行从既有附件队列回读推进——不另建清理定时器，§10.5）。
%% 任一 DB 失败 → {error, _}（由 handle_info 退避承载；单项失败不阻断）。
-spec reconcile_tick() -> ok | {error, term()}.
reconcile_tick() ->
    case reconcile_leases() of
        {ok, _} ->
            case run_batch(?BATCH_LIMIT) of
                {ok, _} ->
                    case refresh_cleanup_pending() of
                        {ok, _} ->
                            case talk_record_export_artifact:sweep() of
                                {ok, _} -> ok;
                                {error, R} -> {error, {export_sweep, R}}
                            end;
                        {error, R} ->
                            {error, {cleanup_refresh, R}}
                    end;
                {error, R} ->
                    {error, {batch, R}}
            end;
        {error, R} ->
            {error, {reap, R}}
    end.

reconcile_leases() ->
    case reap_expired() of
        {ok, _} -> talk_record_processor_run_repo:unblock_waiting();
        E -> E
    end.

%% @doc 删除状态回读轮询（T20）：cleanup_status IN (pending,failed) 的
%% 软删记录（限量 ?CLEANUP_REFRESH_LIMIT），逐行事务内从附件队列回读
%% 物理 进度并推进 complete/failed（幂等；blocked_*/retained_* 不动）。
%% 只读队列 + 写本域状态列——物理删除语义仍全部归既有附件 worker。
-spec refresh_cleanup_pending() -> {ok, non_neg_integer()} | {error, term()}.
refresh_cleanup_pending() ->
    case
        elib_pg:query(
            <<"SELECT id FROM talk_record WHERE deleted_at IS NOT NULL",
                " AND cleanup_status IN ('pending','failed','retained_by_other_reference')",
                " ORDER BY updated_at ASC LIMIT ", (limit_bin())/binary>>,
            []
        )
    of
        {ok, Rows} ->
            Refreshed = [
                Id
             || #{<<"id">> := Id} <- Rows,
                refresh_one_ok(Id)
            ],
            {ok, length(Refreshed)};
        {error, Reason} ->
            {error, Reason}
    end.

refresh_one_ok(RecordId) ->
    case refresh_one(RecordId) of
        {ok, _} -> true;
        _ -> false
    end.

refresh_one(RecordId) ->
    elib_pg:with_tx(
        fun(Conn) -> talk_record_cleanup_logic:refresh_cleanup_state_tx(Conn, RecordId) end,
        [{reraise, false}]
    ).

-spec limit_bin() -> binary().
limit_bin() ->
    integer_to_binary(?CLEANUP_REFRESH_LIMIT).

handle_cast(kick, State = #{enabled := true}) ->
    self() ! tick,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% 内部
%%%===================================================================

-spec enabled() -> boolean().
enabled() ->
    case application:get_env(imboy, talk_record_job_worker_enabled, false) of
        true -> true;
        _ -> false
    end.

-spec tick_ms() -> pos_integer().
tick_ms() ->
    case application:get_env(imboy, talk_record_job_worker_tick_ms, ?TICK_MS) of
        N when is_integer(N), N >= 500 -> N;
        _ -> ?TICK_MS
    end.

-spec lease_seconds() -> pos_integer().
lease_seconds() ->
    case config_ds:env(imboy, talk_record_job_lease_seconds, ?DEFAULT_LEASE_SECONDS) of
        N when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_LEASE_SECONDS
    end.

-spec max_attempts() -> pos_integer().
max_attempts() ->
    case config_ds:env(imboy, talk_record_job_max_attempts, ?DEFAULT_MAX_ATTEMPTS) of
        N when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_MAX_ATTEMPTS
    end.

-spec attempt_exhausted(pos_integer()) -> boolean().
attempt_exhausted(Attempt) ->
    Attempt > max_attempts().

-spec lease_owner() -> binary().
lease_owner() ->
    atom_to_binary(node()).

-spec job_id(map()) -> integer().
job_id(Job) ->
    maps:get(<<"id">>, Job).

-spec lease_token(map()) -> binary().
lease_token(Job) ->
    maps:get(<<"lease_token">>, Job).

%% @private 终态 reason 原子 → 归一化 error_code 短码。
-spec atom_error_code(atom()) -> binary().
atom_error_code(record_deleted) -> <<"record_deleted">>;
atom_error_code(input_stale) -> <<"input_stale">>;
atom_error_code(missing_sequences) -> <<"missing_sequences">>;
atom_error_code(manifest_hash_conflict) -> <<"manifest_hash_conflict">>;
atom_error_code(part_attachment_missing) -> <<"part_attachment_missing">>;
atom_error_code(Other) when is_atom(Other) -> atom_to_binary(Other);
atom_error_code(_) -> <<"provider_error">>.

-spec new_token() -> binary().
new_token() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.
