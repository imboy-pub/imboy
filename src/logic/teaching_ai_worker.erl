-module(teaching_ai_worker).
%%%
% 墨芽书法 AI 视频回课 Worker（Step 11 骨架 + AI-03 降级闭环优先）
% AI video review worker
%
% 降级口径（AI-03）：provider_unavailable 仍是一条正常路径（provider 名未配置 /
% registry 未命中 / vision 声明缺失 / api_key 为空），落 status=failed +
% error_code=provider_unavailable → 老师人工队列照常工作（Step 9 队列不过滤
% ai_status，failed 仍显示）。
% （原文写「imboy_llm 现有 provider vision 全 false，真实多模态调用被
% BLOCKED_EXTERNAL 阻塞，provider_unavailable 是当前主路径」——已于 2026-09-11
% 接入视觉 provider 后失效。）
%
% ⚠️ 视频可见性：附件 URL 由 maybe_attach_view_url/1 按开关补，**默认关闭**；
% 关闭时模型只拿到 object_key（回课内容盲）。见该函数注释。
%
% 执行路径（计划 §7.3）：
%   claim（原子）→ submission/附件绑定校验 → 媒体复核 → provider →
%   成功：白名单 Schema 落库；失败：retry（瞬时）或 failed 终态
%
% 载荷纪律：行内只有业务 ID 与版本（00000097 列集），不存任何 URL；
% 运行时经 submission_scope/submission_asset 解析附件 object_key。
%%%

-export([run_once/0, run_once_tx/1, process_tx/2]).
-export([reclaim_stuck/0, reclaim_stuck_tx/2]).

-ifdef(TEST).
%% 纯函数，导出供单测直接验收「开关关闭时附件原样不变」这一 fail-closed 语义
-export([maybe_attach_view_url/1]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 瞬时错误可重试；provider_unavailable/bad_output/attachment/媒体类不重试
-define(TRANSIENT_ERRORS, [timeout, provider_error]).
-define(DEFAULT_MAX_RETRIES, 2).

%% stuck-row 回收（R7）：running 行卡死 = worker 进程在 claim 后崩溃，行永远
%% 停在 running。回收阈值下限强制（cleanup_unbound 同款保守模式）：正常处理
%% 为秒级，下限 300s 内绝不回收，防误伤正在处理的行。
-define(MIN_STUCK_AGE_SECONDS, 300).
-define(DEFAULT_STUCK_AGE_SECONDS, 900).

%% 视频取用 URL 有效期：与 attach_logic 的 ?GET_EXPIRES 同值（受限资源统一 600s）
-define(VIEW_URL_EXPIRES, 600).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 生产入口（ecron 每分钟一次；也可手动触发）：池连接事务链
%% claim 与 finish 分开事务——处理期间不持锁（进程崩溃会留 running 行，
%% 遗留：stuck-row 回收器，见 STEP-11/notes.md）
-spec run_once() -> {ok, done | {processed, integer()}} | {error, term()}.
run_once() ->
    ClaimTx = fun(Conn) -> teaching_review_repo:claim_next_queued_tx(Conn) end,
    case elib_pg:with_tx(ClaimTx, [{reraise, false}]) of
        {ok, undefined} ->
            {ok, done};
        {ok, Draft} ->
            FinishTx = fun(Conn) -> process_tx(Conn, Draft) end,
            case elib_pg:with_tx(FinishTx, [{reraise, false}]) of
                {ok, Outcome} ->
                    {ok, {processed, Outcome}};
                Other ->
                    ?LOG_WARNING("teaching_ai_worker process error ~p", [Other]),
                    {error, worker_error}
            end;
        Other ->
            ?LOG_WARNING("teaching_ai_worker claim error ~p", [Other]),
            {error, claim_error}
    end.

%% @doc 直连/测试入口：同事务内 claim+process（BEGIN/ROLLBACK 可控）
-spec run_once_tx(any()) -> {ok, done | map()} | {error, term()}.
run_once_tx(Conn) ->
    case teaching_review_repo:claim_next_queued_tx(Conn) of
        {ok, undefined} ->
            {ok, done};
        {ok, Draft} ->
            process_tx(Conn, Draft);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 单草稿执行（Conn 事务内）：返回处理结果 map（测试断言用）
-spec process_tx(any(), map()) -> {ok, map()} | {error, term()}.
process_tx(Conn, #{<<"id">> := DraftId, <<"submission_id">> := SubmissionId} = Draft) ->
    case load_context(SubmissionId) of
        {error, ErrorCode} ->
            %% 资源级失败（附件删除/撤回/链路残缺）：不重试，直接终态
            ok = teaching_review_repo:ai_finish_failed_tx(Conn, DraftId, ErrorCode),
            {ok, #{outcome => failed, error_code => ErrorCode}};
        {ok, #{scope := Scope, attachment := Attachment}} ->
            TaskId = maps:get(<<"ai_task_id">>, Draft, undefined),
            run_provider(Conn, DraftId, TaskId, Draft, Scope, Attachment)
    end.

%% @doc stuck-row 回收（池入口，供 ecron/手动）：running 超龄行回 queued。
%% 阈值经 config_ds:env 可配（teaching_ai_stuck_age_seconds）；下限钳制在
%% reclaim_stuck_tx 内强制（对一切调用方生效，cleanup_unbound 同款保守模式）。
%% ai_task_id 保留不重置——重试计数延续，坏行快速达上限 failed，防毒行永动。
-spec reclaim_stuck() -> {ok, non_neg_integer()} | {error, term()}.
reclaim_stuck() ->
    AgeSeconds = config_ds:env(teaching_ai_stuck_age_seconds, ?DEFAULT_STUCK_AGE_SECONDS),
    Tx = fun(Conn) -> reclaim_stuck_tx(Conn, AgeSeconds) end,
    case elib_pg:with_tx(Tx, [{reraise, false}]) of
        {ok, Count} ->
            case Count > 0 of
                true ->
                    ?LOG_INFO(["teaching_ai_worker reclaimed stuck running rows: ", Count]),
                    {ok, Count};
                false ->
                    {ok, 0}
            end;
        {rollback, Reason} ->
            ?LOG_WARNING("teaching_ai_worker reclaim rollback ~p", [Reason]),
            {error, reclaim_error};
        {error, Reason} ->
            ?LOG_WARNING("teaching_ai_worker reclaim error ~p", [Reason]),
            {error, reclaim_error}
    end.

%% @doc stuck-row 回收（事务内/直连测试入口）：仅 running 且 created_at 超龄行
%% 回 queued（清 error_code/completed_at；claim 时不覆盖 ai_task_id，见 repo 注释）。
%% AgeSeconds 经下限 300s 强制钳制——正常处理为秒级，阈值内绝不误回收在处理行。
%% 返回回收行数。
-spec reclaim_stuck_tx(any(), non_neg_integer()) -> {ok, non_neg_integer()} | {error, term()}.
reclaim_stuck_tx(Conn, AgeSeconds0) ->
    AgeSeconds = max(?MIN_STUCK_AGE_SECONDS, AgeSeconds0),
    Tb = elib_pg_sql:public_tablename(<<"calligraphy_review_draft">>),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'queued', error_code = NULL, completed_at = NULL "
            " WHERE status = 'running' "
            "   AND created_at < now() - ($1 || ' seconds')::interval "
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [integer_to_binary(AgeSeconds)]) of
        {ok, Rows} ->
            {ok, length(Rows)};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% ---- 资源门（附件删除/撤回/链路残缺 → failed，AI-01 路径 5） ----

-spec load_context(integer()) ->
    {ok, #{scope := map(), attachment := map()}} | {error, binary()}.
load_context(SubmissionId) ->
    case teaching_context_repo:submission_scope(SubmissionId) of
        {ok, Scope} when is_map(Scope) ->
            case maps:get(<<"submission_status">>, Scope, undefined) of
                <<"submitted">> ->
                    load_attachment(SubmissionId, Scope);
                _ ->
                    %% 已撤回：AI 不再处理，证据经审计路径保留
                    {error, <<"submission_withdrawn">>}
            end;
        _ ->
            {error, <<"attachment_missing">>}
    end.

-spec load_attachment(integer(), map()) ->
    {ok, #{scope := map(), attachment := map()}} | {error, binary()}.
load_attachment(SubmissionId, Scope) ->
    SaTb = elib_pg_sql:public_tablename(<<"submission_asset">>),
    AttTb = elib_pg_sql:public_tablename(<<"attachment">>),
    Sql =
        <<
            "SELECT att.id, att.path, att.mime_type, att.size, att.scope "
            "FROM ",
            SaTb/binary,
            " sa "
            "JOIN ",
            AttTb/binary,
            " att ON att.id = sa.attachment_id "
            "WHERE sa.submission_id = $1 AND sa.kind = 'practice_video' "
            "  AND att.status >= 0 LIMIT 1"
        >>,
    case elib_pg:query(Sql, [SubmissionId]) of
        {ok, [Attachment | _]} ->
            {ok, #{scope => Scope, attachment => maybe_attach_view_url(Attachment)}};
        _ ->
            %% 附件已删除/未绑定（authorize 语义在系统侧等价物：绑定存在性）
            {error, <<"attachment_missing">>}
    end.

%% @doc 给附件补「模型侧可抓取」的可达 URL —— **默认关闭**。
%% 关闭时附件原样返回：provider 的 build_messages 走纯文本分支，模型只拿到
%% object_key（回课内容盲，但链路完整、不报错）。
%% 开启后每条回课会把一段短时有效的视频 URL 交给第三方多模态模型——这是涉及
%% 未成年人媒体内容的对外数据流，故必须显式打开：
%%     {teaching_ai_attach_video_url, true}   %% 或 IMBOY_ 前缀环境变量
%% 签名原语与 attach_logic:view_url/2 同源，签名主机取 elib_oss:public_endpoint()
%% （生产 https://s3.imboy.pub 公网可抓；本地为内网端点时模型抓不到，会以
%% provider_error 降级到老师人工队列，不会静默出错）。
%% 不落库、不写日志（MEDIA-03：不得持久化 presigned URL）。
-spec maybe_attach_view_url(map()) -> map().
maybe_attach_view_url(Attachment) ->
    case config_ds:env(teaching_ai_attach_video_url, false) of
        true -> presign_view_url(Attachment);
        _ -> Attachment
    end.

-spec presign_view_url(map()) -> map().
presign_view_url(Attachment) ->
    Path = maps:get(<<"path">>, Attachment, <<>>),
    Scope = maps:get(<<"scope">>, Attachment, <<"teaching">>),
    case is_binary(Path) andalso Path =/= <<>> of
        true ->
            try elib_oss:presign_get_for_key(elib_oss:get_bucket(Scope), Path, ?VIEW_URL_EXPIRES) of
                Url when is_binary(Url), Url =/= <<>> ->
                    Attachment#{<<"url">> => Url};
                _ ->
                    Attachment
            catch
                %% 签名失败（garage 未配置/端点异常）不阻断回课：退回不带 url，
                %% 与开关关闭时的行为一致（内容盲但链路完整）。
                _:_ ->
                    Attachment
            end;
        false ->
            Attachment
    end.

%% ---- provider 调用与结果分派 ----

-spec run_provider(any(), integer(), binary() | null | undefined, map(), map(), map()) ->
    {ok, map()} | {error, term()}.
run_provider(Conn, DraftId, TaskId, Draft, Scope, Attachment) ->
    DraftMeta = #{
        submission_id => maps:get(<<"submission_id">>, Draft, 0),
        prompt_version => maps:get(<<"prompt_version">>, Draft, <<>>),
        rubric_version => maps:get(<<"rubric_version">>, Draft, <<>>),
        org_id => maps:get(<<"org_id">>, Scope, undefined)
    },
    %% 媒体复核（服务端 HEAD 值；时长客户端未上报不在此路径）
    case
        teaching_attach_logic:verify_upload(
            maps:get(<<"mime_type">>, Attachment, <<>>),
            maps:get(<<"size">>, Attachment, 0),
            #{}
        )
    of
        {error, Reason} ->
            ok = teaching_review_repo:ai_finish_failed_tx(
                Conn,
                DraftId,
                media_error_code(Reason)
            ),
            {ok, #{outcome => failed, error_code => media_error_code(Reason)}};
        ok ->
            dispatch_provider_result(Conn, DraftId, TaskId, DraftMeta, Attachment)
    end.

-spec dispatch_provider_result(any(), integer(), binary() | null | undefined, map(), map()) ->
    {ok, map()} | {error, term()}.
dispatch_provider_result(Conn, DraftId, TaskId, DraftMeta, Attachment) ->
    case teaching_ai_draft_logic:analyze_video(DraftMeta, Attachment) of
        {ok, Result} ->
            ModelProfile = config_ds:env(teaching_ai_llm_provider, <<"">>),
            ok = teaching_review_repo:ai_finish_success_tx(Conn, DraftId, ModelProfile, Result),
            {ok, #{outcome => succeeded, result => Result}};
        {error, Reason} ->
            dispatch_failure(Conn, DraftId, TaskId, Reason)
    end.

-spec dispatch_failure(any(), integer(), binary() | null | undefined, atom()) ->
    {ok, map()} | {error, term()}.
dispatch_failure(Conn, DraftId, TaskId, Reason) ->
    MaxRetries = config_ds:env(teaching_ai_max_retries, ?DEFAULT_MAX_RETRIES),
    Attempt = attempt_of(TaskId),
    Transient = lists:member(Reason, ?TRANSIENT_ERRORS),
    case Transient andalso Attempt < MaxRetries of
        true ->
            Next = <<"run:", (integer_to_binary(Attempt + 1))/binary>>,
            ok = teaching_review_repo:ai_requeue_tx(Conn, DraftId, Next),
            {ok, #{outcome => requeued, attempt => Attempt + 1, reason => Reason}};
        false ->
            ErrorCode = error_code_of(Reason),
            ok = teaching_review_repo:ai_finish_failed_tx(Conn, DraftId, ErrorCode),
            {ok, #{outcome => failed, error_code => ErrorCode}}
    end.

%% ai_task_id 兼任尝试计数（NULL=首次→1；"run:N"→N）
-spec attempt_of(binary() | atom() | undefined) -> integer().
attempt_of(<<"run:", N/binary>>) ->
    case
        try
            binary_to_integer(N)
        catch
            _:_ -> bad_int
        end
    of
        Int when is_integer(Int), Int > 0 -> Int;
        _ -> 1
    end;
attempt_of(_) ->
    1.

-spec error_code_of(atom()) -> binary().
error_code_of(provider_unavailable) ->
    <<"provider_unavailable">>;
error_code_of(timeout) ->
    <<"timeout">>;
error_code_of(provider_error) ->
    <<"provider_error">>;
error_code_of(bad_output) ->
    <<"bad_schema">>;
error_code_of(R) when is_atom(R) ->
    atom_to_binary(R, utf8).

-spec media_error_code(term()) -> binary().
media_error_code(file_too_large) -> <<"media_too_large">>;
media_error_code(invalid_file_type) -> <<"media_invalid_type">>;
media_error_code(_) -> <<"media_invalid">>.
