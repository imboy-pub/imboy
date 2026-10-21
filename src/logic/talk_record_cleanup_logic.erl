-module(talk_record_cleanup_logic).
-moduledoc "talk_record 清理协调（T20：purge kind 执行体 + 物理状态回读投影）。".
%%%
% §10.4 删除状态拆分的协调侧（talk_record_job kind=purge 的执行体，
% T12 骨架接线）：
%
%   * execute_purge_tx/2 —— purge 只协调不执行存储 DELETE：
%       1. 门禁：记录必须已软删（未删记录的 purge 行 → record_not_deleted
%          防御拒绝）；
%       2. 经 T06 adapter request_record_cleanup_tx：治理门（retention/
%          hold → blocked_*，引用保持 bound，GC 谓词继续挡）→ 释放本域
%          bound 引用 → 逐附件写删除意图（retained_by_other_reference /
%          tombstone / asset_row_missing per-attachment 结果）；
%       3. 清理 jobs/audit 保留策略：治理元数据不删（§10.4"有效 hold/
%          retention 期间需保留的治理元数据不能随本域删除"）——本域
%          无删除动作，仅此声明；
%       4. 状态投影：outcomes → cleanup_status 单向推进；
%       5. 同事务末尾跑一次物理状态回读（刚写的意图通常仍在队列 =
%          pending；由 refresh_cleanup_state_tx 供后续轮询/重试推进）。
%   * refresh_cleanup_state_tx/2 —— 轮询函数（§10.4"附件物理状态从既有
%     队列回读"）：该记录全部曾绑定附件的队列进度投影 → complete/
%     failed/保持。purge 成功后可被 worker tick / 运维 retry / 后续周期
%     任务反复调用（幂等）。
%   * physical_delete_state_tx/2 —— 删除后 owner 白名单的
%     physical_delete_state 字段投影（聚合：deleting/deleted/failed/none）。
%
% 迟到模型结果不发布（§2"删除中 job 不能重新发布内容"）：DELETE 事务
% 经 talk_record_job_logic:cancel_record_jobs/1 取消全部非终态 job（T12
% 已备），worker finish CAS 落空——本模块不再重复取消（purge 行自身是
% 投递后的唯一在途任务）。
%%%

-export([
    execute_purge_tx/2,
    refresh_cleanup_state_tx/2,
    physical_delete_state_tx/2,
    cleanup_reason_tx/2,
    %% 纯函数（测试直验）
    project_outcomes/1,
    project_physical_state/1
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% purge 执行体（worker 分派调用；事务内形态）
%% ===================================================================

%% @doc purge kind 执行：协调引用释放 + 删除意图 + cleanup_status 推进。
%% 返回 {ok, #{cleanup_status => S, released => N, outcomes => [...]}} 供
%% worker finish（job 成功终态）；{error, record_not_deleted}（防御）/
%% {error, db_error}。
-spec execute_purge_tx(any(), map()) ->
    {ok, map()} | {error, record_not_deleted | db_error | term()}.
execute_purge_tx(Conn, #{<<"record_id">> := RecordId}) ->
    case talk_record_delete_repo:cleanup_status_tx(Conn, RecordId) of
        {ok, <<"none">>} ->
            %% 未软删记录的 purge 行（防御性投递/数据异常）：拒绝——
            %% purge 只属于删除后的清理协调（§10.4），不主动删业务数据
            {error, record_not_deleted};
        {ok, <<"pending">>} ->
            purge_pending_tx(Conn, RecordId);
        {ok, <<"failed">>} ->
            %% 重试路径（retry_purge 复位 pending 后走 purge_pending_tx）；
            %% 未复位直接执行时同款协调（幂等）
            purge_pending_tx(Conn, RecordId);
        {ok, _Progressed} ->
            %% blocked_*/retained_*/complete：协调已完成或被治理阻断——
            %% 幂等重跑只做物理状态回读（blocked 期间队列无行，保持原状）
            {ok, refresh_summary_tx(Conn, RecordId)};
        {error, Reason} ->
            {error, Reason}
    end.

purge_pending_tx(Conn, RecordId) ->
    case talk_record_attachment_adapter:request_record_cleanup_tx(Conn, RecordId) of
        {ok, #{blocked := Blocker}} ->
            Status = blocker_status(Blocker),
            {ok, _} = talk_record_delete_repo:update_cleanup_status_tx(Conn, RecordId, Status),
            {ok, #{cleanup_status => Status, released => 0, outcomes => [], blocked => Blocker}};
        {ok, #{released := Released, outcomes := Outcomes}} ->
            Status = project_outcomes(Outcomes),
            {ok, _} = talk_record_delete_repo:update_cleanup_status_tx(Conn, RecordId, Status),
            %% 意图刚入队（job_state=ready）——同事务回读通常仍 deleting；
            %% complete 推进交给 refresh（worker ack 后轮询/重试触发）
            Physical = physical_delete_state_tx(Conn, RecordId),
            {ok, #{
                cleanup_status => Status,
                released => Released,
                outcomes => Outcomes,
                physical_delete_state => Physical
            }};
        {error, Reason} ->
            ?ERROR_LOG([talk_record_purge_coordination_failed, RecordId, Reason]),
            {error, db_error}
    end.

-spec blocker_status(term()) -> binary().
blocker_status({blocked_retention, _Until}) ->
    <<"blocked_retention">>;
blocker_status({blocked_hold, _Count}) ->
    <<"blocked_hold">>;
blocker_status(_) ->
    <<"failed">>.

%% @private outcomes → cleanup_status（§10.4）：
%%   全 intended/tombstone → pending（物理删除归队列，未完成不伪报）；
%%   任一 retained_by_other_reference → retained_by_other_reference；
%%   任一 asset_row_missing/错误 → failed（审计异常，不伪报完成）。
-spec project_outcomes([map()]) -> binary().
project_outcomes(Outcomes) ->
    Bad = [O || O <- Outcomes, outcome_bad(O)],
    Retained = [O || O <- Outcomes, maps:get(outcome, O) =:= retained_by_other_reference],
    case {Bad, Retained} of
        {[], []} -> <<"pending">>;
        {[], _} -> <<"retained_by_other_reference">>;
        {_, _} -> <<"failed">>
    end.

-spec outcome_bad(map()) -> boolean().
outcome_bad(#{outcome := Outcome}) when
    Outcome =:= intended; Outcome =:= tombstone
->
    false;
outcome_bad(#{outcome := retained_by_other_reference}) ->
    false;
outcome_bad(_) ->
    true.

refresh_summary_tx(Conn, RecordId) ->
    Physical = physical_delete_state_tx(Conn, RecordId),
    {ok, Status} = talk_record_delete_repo:cleanup_status_tx(Conn, RecordId),
    #{cleanup_status => Status, released => 0, outcomes => [], physical_delete_state => Physical}.

%% ===================================================================
%% 物理状态回读投影（轮询函数；§10.4）
%% ===================================================================

%% @doc 队列进度 → cleanup_status 推进（幂等，可反复调用）：
%%   全部附件 deleted（队列 done / 资产 lifecycle=deleted）→ complete；
%%   任一附件队列终态失败（blocked 且 attempts 耗尽）→ failed；
%%   其余（ready/leased/retry_wait/delete_intent 进行中）→ 保持现值。
%% 仅处理 pending/failed 的记录；blocked_*/retained_*/complete 不动。
-spec refresh_cleanup_state_tx(any(), integer()) -> {ok, binary()} | {error, term()}.
refresh_cleanup_state_tx(Conn, RecordId) ->
    %% Same order as purge/delete: record lock before attachment/ref/intent locks.
    case
        elib_pg:query(
            Conn,
            <<"SELECT id FROM talk_record WHERE id=$1 AND deleted_at IS NOT NULL FOR UPDATE">>,
            [RecordId]
        )
    of
        {ok, [_]} -> refresh_locked_tx(Conn, RecordId);
        {ok, []} -> {error, record_not_deleted};
        E -> E
    end.
refresh_locked_tx(Conn, RecordId) ->
    case talk_record_delete_repo:write_blocker_tx(Conn, RecordId) of
        {ok, null} -> refresh_unblocked_tx(Conn, RecordId);
        {ok, _} -> talk_record_delete_repo:mark_write_pending_tx(Conn, RecordId);
        Error -> Error
    end.

refresh_unblocked_tx(Conn, RecordId) ->
    case talk_record_delete_repo:cleanup_status_tx(Conn, RecordId) of
        {ok, Current} when
            Current =:= <<"pending">>;
            Current =:= <<"failed">>;
            Current =:= <<"retained_by_other_reference">>
        ->
            %% Upload capabilities and other references may expire after the original purge.
            %% Re-enter the existing governance/GC intent path; never shorten their lifetime.
            case purge_pending_tx(Conn, RecordId) of
                {ok, #{cleanup_status := <<"pending">>}} -> refresh_pending_tx(Conn, RecordId);
                {ok, #{cleanup_status := Status}} -> {ok, Status};
                E -> E
            end;
        {ok, Other} ->
            {ok, Other};
        {error, Reason} ->
            {error, Reason}
    end.

refresh_pending_tx(Conn, RecordId) ->
    case talk_record_delete_repo:purged_attachment_paths_tx(Conn, RecordId) of
        {ok, []} ->
            %% 无曾绑定附件（纯文本草稿/未上传记录）：协调即完成
            talk_record_delete_repo:update_cleanup_status_tx(Conn, RecordId, <<"complete">>);
        {ok, Rows} ->
            States = [project_physical_state(R) || R <- Rows],
            Target =
                case
                    {lists:member(failed, States), lists:all(fun(S) -> S =:= deleted end, States)}
                of
                    {true, _} -> <<"failed">>;
                    {false, true} -> <<"complete">>;
                    {false, false} -> <<"pending">>
                end,
            talk_record_delete_repo:update_cleanup_status_tx(Conn, RecordId, Target);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 单附件物理状态投影（行来自 purged_attachment_paths_tx）：
%%   lifecycle=deleted（或队列 done）→ deleted；
%%   队列 blocked（attempts 耗尽）→ failed；
%%   有队列行 / delete_intent → deleting；
%%   live 且无队列行 → none（意图未及登记，保守视未开始）。
-spec project_physical_state(map()) -> deleted | deleting | failed | none.
project_physical_state(#{<<"derived_write_state">> := <<"unknown">>}) ->
    failed;
project_physical_state(#{<<"derived_write_state">> := S}) when
    S =:= <<"reserved">>; S =:= <<"writing">>
->
    deleting;
project_physical_state(#{<<"lifecycle_state">> := <<"deleted">>}) ->
    deleted;
project_physical_state(#{<<"queue_done">> := true}) ->
    deleted;
project_physical_state(#{<<"queue_state">> := <<"blocked">>, <<"queue_exhausted">> := true}) ->
    failed;
project_physical_state(#{<<"queue_state">> := null, <<"lifecycle_state">> := <<"live">>}) ->
    none;
project_physical_state(_) ->
    deleting.

%% @doc 删除后 owner 白名单的 physical_delete_state 聚合（§10.4 拆分字段）：
%%   空 → none；全 none → none（意图未及登记）；全 deleted → deleted；
%%   任一 failed → failed；其余（删除中/混合）→ deleting。
-spec physical_delete_state_tx(any(), integer()) ->
    binary().
physical_delete_state_tx(Conn, RecordId) ->
    case talk_record_delete_repo:purged_attachment_paths_tx(Conn, RecordId) of
        {ok, []} ->
            <<"none">>;
        {ok, Rows} ->
            States = [project_physical_state(R) || R <- Rows],
            case
                {
                    lists:member(failed, States),
                    lists:all(fun(S) -> S =:= deleted end, States),
                    lists:all(fun(S) -> S =:= none end, States)
                }
            of
                {true, _, _} -> <<"failed">>;
                {_, true, _} -> <<"deleted">>;
                {_, _, true} -> <<"none">>;
                {_, _, _} -> <<"deleting">>
            end;
        {error, _} ->
            %% 投影失败不阻塞协调——白名单显示 unknown 由读侧兜底
            <<"unknown">>
    end.

cleanup_reason_tx(Conn, Rid) ->
    case talk_record_delete_repo:write_blocker_tx(Conn, Rid) of
        {ok, Reason} -> Reason;
        _ -> <<"cleanup_status_unavailable">>
    end.
