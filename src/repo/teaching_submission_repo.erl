-module(teaching_submission_repo).
%%%
% 墨芽作业提交数据仓库（Step 9）
% Homework submission repository
%
% 事务配方（严格照抄 STEP-08-DB/notes.md，Agent C 冻结）：
%   ① 幂等提交 CTE：ON CONFLICT (submitted_by, assignment_id, idempotency_key)
%      WHERE idempotency_key IS NOT NULL（必须带索引谓词）+ digest 回读判 5460
%   ② attempt 取号：先 SELECT ... FOR UPDATE assignment 行，再 MAX+1，
%      uk_homework_submission_attempt 兜底
%   ③ 撤回/发布互斥：先 SELECT ... FOR UPDATE homework_submission 行，
%      再条件 UPDATE（READ COMMITTED 单语句守卫有快照洞，DB 触发器仅兜底）
%
% 所有写函数均为 _tx(Conn, ...) 形态：生产经 elib_pg:with_tx 借池连接，
% 集成测试直接传 epgsql 连接（同事务可控 BEGIN/COMMIT/ROLLBACK）。
%%%

-export([tablename/1]).
-export([lock_assignment_tx/2, next_attempt_tx/2, create_idempotent_tx/2]).
-export([insert_assets_tx/4, mark_submitted_by_tx/3, enqueue_ai_draft_tx/2]).
-export([withdraw_tx/3, lock_submission_tx/2, find_tx/2, assets_tx/2, find/1, assets/1]).
-export([assignments_for_learner/3, assignments_for_learner_tx/4]).
-export([assignments_for_learner/4, assignments_for_learner_tx/5]).
-export([assignment_detail/1, assignment_detail_tx/2]).
-export([queue/4, history/3]).
-export([history_unread_count/2]).
-export([
    submission_for_asset_path/1,
    submission_for_asset_path_tx/2,
    unbound_teaching_attachments/1,
    unbound_teaching_attachments_tx/2
]).
-export([validate_assets/2]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec tablename(binary()) -> binary().
tablename(Tb) ->
    elib_pg_sql:public_tablename(Tb).

%% ------------------------------------------------------------------
%% 配方②：assignment 行锁（并发提交的串行化点）
%% ------------------------------------------------------------------

-spec lock_assignment_tx(any(), integer()) -> {ok, integer() | undefined} | {error, term()}.
lock_assignment_tx(Conn, AssignmentId) ->
    Sql = <<"SELECT id FROM ", (tb(group_task_assignment))/binary, " WHERE id = $1 FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [AssignmentId]) of
        {ok, [#{<<"id">> := Id} | _]} -> {ok, Id};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% ------------------------------------------------------------------
%% 配方②：锁后取号（新鲜快照）
%% ------------------------------------------------------------------

-spec next_attempt_tx(any(), integer()) -> {ok, integer()} | {error, term()}.
next_attempt_tx(Conn, AssignmentId) ->
    Sql =
        <<"SELECT COALESCE(MAX(attempt_no), 0) + 1 AS next_no FROM ",
            (tb(homework_submission))/binary, " WHERE assignment_id = $1">>,
    case elib_pg:query(Conn, Sql, [AssignmentId]) of
        {ok, [#{<<"next_no">> := Next} | _]} -> {ok, Next};
        {ok, []} -> {ok, 1};
        {error, Reason} -> {error, Reason}
    end.

%% ------------------------------------------------------------------
%% 配方①：幂等提交（插入或回读同一行；digest 不一致 → idempotency_conflict）
%% 返回 {ok, #{id, attempt_no, request_digest, created}} —— created=本次新插入
%% ------------------------------------------------------------------

-spec create_idempotent_tx(any(), map()) ->
    {ok, map()} | {error, idempotency_conflict | term()}.
create_idempotent_tx(Conn, #{
    id := Id,
    assignment_id := AssignmentId,
    learner_id := LearnerId,
    uid := Uid,
    attempt_no := AttemptNo,
    idempotency_key := IdemKey,
    request_digest := Digest
}) ->
    Sql =
        <<
            "WITH ins AS ( "
            "INSERT INTO ",
            (tb(homework_submission))/binary,
            " (id, assignment_id, learner_id, submitted_by, attempt_no,"
            "  idempotency_key, request_digest) "
            "VALUES ($1, $2, $3, $4, $5, $6, $7) "
            "ON CONFLICT (submitted_by, assignment_id, idempotency_key) "
            "  WHERE idempotency_key IS NOT NULL "
            "DO NOTHING "
            %% submitted_at 以 Rfc3339（UTC）字符串返回：create 响应契约字段
            %% （moya SubmissionCreated DTO），幂等重放与新建同源同值
            "RETURNING id, attempt_no, request_digest, "
            "  to_char(submitted_at AT TIME ZONE 'UTC', "
            "    'YYYY-MM-DD\"T\"HH24:MI:SS.MS\"Z\"') AS submitted_at "
            ") "
            "SELECT id, attempt_no, request_digest, submitted_at FROM ins "
            "UNION ALL "
            "SELECT id, attempt_no, request_digest, "
            "  to_char(submitted_at AT TIME ZONE 'UTC', "
            "    'YYYY-MM-DD\"T\"HH24:MI:SS.MS\"Z\"') AS submitted_at "
            "FROM ",
            (tb(homework_submission))/binary,
            " WHERE submitted_by = $4 AND assignment_id = $2 AND idempotency_key = $6 "
            "   AND NOT EXISTS (SELECT 1 FROM ins) "
            "LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [Id, AssignmentId, LearnerId, Uid, AttemptNo, IdemKey, Digest]) of
        {ok, [Row | _]} ->
            digest_check(Row, Digest, AttemptNo);
        {ok, []} ->
            {error, insert_failed};
        {error, Reason} ->
            {error, Reason}
    end.

%% ------------------------------------------------------------------
%% 附件（submission_asset 只存 attachment_id；禁止 presigned URL）
%% Assets = [{AttId, Kind, SortOrder}]
%% ------------------------------------------------------------------

-spec insert_assets_tx(any(), integer(), integer(), [{integer(), binary(), integer()}]) ->
    ok | {error, term()}.
insert_assets_tx(_Conn, _SubmissionId, _Uid, []) ->
    ok;
insert_assets_tx(Conn, SubmissionId, Uid, Assets) ->
    Sql =
        <<"INSERT INTO ", (tb(submission_asset))/binary,
            " (id, submission_id, attachment_id, kind, sort_order, created_by) "
            "VALUES ($1, $2, $3, $4, $5, $6)">>,
    Results =
        [
            elib_pg:execute(Conn, Sql, [
                elib_tsid:generate(), SubmissionId, AttId, Kind, Order, Uid
            ])
         || {AttId, Kind, Order} <- Assets
        ],
    case [E || {error, E} <- Results] of
        [] -> ok;
        [First | _] -> {error, First}
    end.

%% @doc 附件归属校验：attachment 行存在且 creator_user_id = 提交人
%% 返回行含 mime_type/status，供 Logic 层做 kind↔MIME 与 confirm 状态校验（5441）
%% ------------------------------------------------------------------
%% W2-A2-HARDEN：家长提交附件守卫（对齐 review 侧 validate_assets_tx 口径）：
%%   1. 全部存在（缺失 → not_found）
%%   2. creator_user_id == 提交人（非本人 → not_found，防存在性探测——
%%      与 review 侧 check_each_asset 同折叠口径）
%%   3. status >= 0（软删拒绝）+ scope = 'teaching'（私聊等跨 scope 拒绝）
%%   4. MIME ↔ kind 匹配（practice_video=video/*，final_photo=image/*）
%% 任一不过 → {error, assets_invalid}（调用方统一映射 5441）。
%% 入参为 [{AttId, Kind, SortOrder}] 三元组（Kind 参与 MIME 校验）。
%% ------------------------------------------------------------------
-spec validate_assets(integer(), [{integer(), binary(), integer()}]) ->
    {ok, [map()]} | {error, not_found | assets_invalid}.
validate_assets(Uid, Assets) when is_list(Assets), Assets =/= [] ->
    Ids = [AttId || {AttId, _, _} <- Assets],
    Sql =
        <<"SELECT id, mime_type, status, creator_user_id, scope FROM ", (tb(attachment))/binary,
            " WHERE id = ANY($1)">>,
    case elib_pg:query(Sql, [Ids]) of
        {ok, Rows} ->
            %% 防御性守卫：Assets 非空但无任何 {AttId,_,_} 三元组元素时 Ids = []，
            %% 0 =:= 0 会短路放行（fail-open）；显式要求 Ids 非空，防未来调用方绕过 normalize_assets。
            case Ids =/= [] andalso length(Rows) =:= length(lists:usort(Ids)) of
                false ->
                    {error, not_found};
                true ->
                    check_submission_assets(Rows, Assets, Uid)
            end;
        {error, Reason} ->
            ?LOG_ERROR("teaching_submission_repo validate_assets db error ~p", [Reason]),
            {error, not_found}
    end;
validate_assets(_Uid, []) ->
    {error, not_found}.

%% 逐项：归属 → active+scope → MIME↔kind（review 侧 check_each_asset 同款折叠）
-spec check_submission_assets([map()], [{integer(), binary(), integer()}], integer()) ->
    {ok, [map()]} | {error, not_found | assets_invalid}.
check_submission_assets(Rows, Assets, Uid) ->
    ById = maps:from_list([{maps:get(<<"id">>, R), R} || R <- Rows]),
    case check_submission_asset(Assets, ById, Uid) of
        ok -> {ok, Rows};
        {error, _} = E -> E
    end.

-spec check_submission_asset(
    [{integer(), binary(), integer()}], #{integer() => map()}, integer()
) -> ok | {error, not_found | assets_invalid}.
check_submission_asset([], _ById, _Uid) ->
    ok;
check_submission_asset([{AttId, Kind, _Order} | Rest], ById, Uid) ->
    case maps:get(AttId, ById, undefined) of
        %% 不存在 / 非本人创建：同 not_found（防存在性探测）
        undefined ->
            {error, not_found};
        #{<<"creator_user_id">> := Uid} = Row ->
            case
                submission_asset_active_scoped(Row) andalso submission_mime_kind_match(Row, Kind)
            of
                true -> check_submission_asset(Rest, ById, Uid);
                false -> {error, assets_invalid}
            end;
        _ ->
            {error, not_found}
    end.

-spec submission_asset_active_scoped(map()) -> boolean().
submission_asset_active_scoped(#{<<"status">> := S, <<"scope">> := <<"teaching">>}) when S >= 0 ->
    true;
submission_asset_active_scoped(_) ->
    false.

-spec submission_mime_kind_match(map(), binary()) -> boolean().
submission_mime_kind_match(#{<<"mime_type">> := <<"video/", _/binary>>}, <<"practice_video">>) ->
    true;
submission_mime_kind_match(#{<<"mime_type">> := <<"image/", _/binary>>}, <<"final_photo">>) ->
    true;
submission_mime_kind_match(_, _) ->
    false.

%% ------------------------------------------------------------------
%% assignment 快捷字段（真源在 submission）
%% ------------------------------------------------------------------

-spec mark_submitted_by_tx(any(), integer(), integer()) -> ok | {error, term()}.
mark_submitted_by_tx(Conn, AssignmentId, Uid) ->
    Sql = <<"UPDATE ", (tb(group_task_assignment))/binary, " SET submitted_by = $2 WHERE id = $1">>,
    case elib_pg:execute(Conn, Sql, [AssignmentId, Uid]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% ------------------------------------------------------------------
%% AI 草稿入队（Step 11 Worker 前占位 queued；每 submission 一个有效草稿）
%% ------------------------------------------------------------------

-spec enqueue_ai_draft_tx(any(), integer()) -> ok | {error, term()}.
enqueue_ai_draft_tx(Conn, SubmissionId) ->
    Sql =
        <<"INSERT INTO ", (tb(calligraphy_review_draft))/binary,
            " (id, submission_id, status, model_profile, prompt_version, rubric_version) "
            "VALUES ($1, $2, 'queued', '', '', '') "
            "ON CONFLICT DO NOTHING">>,
    case elib_pg:execute(Conn, Sql, [elib_tsid:generate(), SubmissionId]) of
        {ok, _} ->
            ok;
        {error, {pgsql_error, #{code := <<"23505">>}}} ->
            %% uk_crd_active_per_submission：已有有效草稿（幂等重放路径）
            ok;
        {error, {error, error, _, unique_violation, _, _}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% ------------------------------------------------------------------
%% 配方③：撤回（lock-first 两语句；0 行更新 → 已评/已撤回/不存在）
%% 返回 {ok, withdrawn} | {error, not_found | not_submitted | already_reviewed}
%% ------------------------------------------------------------------

-spec withdraw_tx(any(), integer(), integer()) ->
    {ok, withdrawn} | {error, not_found | not_submitted | already_reviewed | term()}.
withdraw_tx(Conn, SubmissionId, Uid) ->
    case lock_submission_tx(Conn, SubmissionId) of
        {ok, #{<<"status">> := <<"submitted">>}} ->
            Sql =
                <<"UPDATE ", (tb(homework_submission))/binary,
                    " SET status = 'withdrawn', withdrawn_at = now(), withdrawn_by = $2 "
                    " WHERE id = $1 AND status = 'submitted' "
                    "   AND NOT EXISTS (SELECT 1 FROM ", (tb(teacher_review))/binary,
                    "                    WHERE submission_id = $1 AND status = 'published')">>,
            case elib_pg:execute(Conn, Sql, [SubmissionId, Uid]) of
                {ok, 1} ->
                    {ok, withdrawn};
                {ok, 0} ->
                    %% 锁后守卫拒绝：唯一可能是有 published review（5481）
                    {error, already_reviewed};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, #{<<"status">> := _}} ->
            {error, not_submitted};
        {ok, undefined} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec lock_submission_tx(any(), integer()) ->
    {ok, map() | undefined} | {error, term()}.
lock_submission_tx(Conn, SubmissionId) ->
    Sql =
        <<"SELECT id, status, attempt_no FROM ", (tb(homework_submission))/binary,
            " WHERE id = $1 FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [SubmissionId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% ------------------------------------------------------------------
%% 读取
%% ------------------------------------------------------------------

-spec find_tx(any(), integer()) -> {ok, map() | undefined} | {error, term()}.
find_tx(Conn, SubmissionId) ->
    find_run(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, SubmissionId).

%% @doc 自动连接池版本（生产读取路径）
-spec find(integer()) -> {ok, map() | undefined} | {error, term()}.
find(SubmissionId) ->
    find_run(fun elib_pg:query/2, SubmissionId).

-spec find_run(fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}), integer()) ->
    {ok, map() | undefined} | {error, term()}.
find_run(Exec, SubmissionId) ->
    Sql = <<
        "SELECT id, assignment_id, learner_id, submitted_by, attempt_no, status, "
        "submitted_at, withdrawn_at, created_at FROM ",
        (tb(homework_submission))/binary,
        " WHERE id = $1 LIMIT 1"
    >>,
    case Exec(Sql, [SubmissionId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

-spec assets_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
assets_tx(Conn, SubmissionId) ->
    assets_run(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, SubmissionId).

%% @doc 自动连接池版本
-spec assets(integer()) -> {ok, [map()]} | {error, term()}.
assets(SubmissionId) ->
    assets_run(fun elib_pg:query/2, SubmissionId).

-spec assets_run(fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}), integer()) ->
    {ok, [map()]} | {error, term()}.
assets_run(Exec, SubmissionId) ->
    Sql = <<
        "SELECT a.id, a.attachment_id, a.kind, a.sort_order, att.mime_type, att.path "
        "FROM ",
        (tb(submission_asset))/binary,
        " a "
        "JOIN ",
        (tb(attachment))/binary,
        " att ON att.id = a.attachment_id "
        "WHERE a.submission_id = $1 ORDER BY a.kind, a.sort_order"
    >>,
    case Exec(Sql, [SubmissionId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% 列表查询（家长作业列表 / 老师队列 / 学员历史）
%%%===================================================================

%% @doc 家长视角作业列表：教学 assignment + 最新 submission 概要 + 推导状态 + 作品预览句柄
%%
%% latest_asset（作品预览，moya 家长首页缩略图）：取**最新 submission** 的首张
%% final_photo，ORDER BY (sort_order, id) 与 assets_run/2 同口径。只给 object_key
%% 句柄——签名 URL 由客户端按需调 /api/v1/attachment/view_url 换取（MEDIA-03：
%% 本层不持久化也不预签 URL，列表 size 份签名会签出未渲染的浪费）。
%% 不回退 practice_video：视频非图片（首帧 vs 静图），取不到即 NULL，由客户端
%% 渲染「无作品图」态。att.status >= 0 与读授权路径 asset_path_run/2 同口径——
%% 凡 view_url 会放行的对象才出现在预览里。
%%
%% CM-F4（Wave 2）：StatusOpt 四态过滤（pending/submitted/reviewing/reviewed）。
%% 推导口径与 teaching_assignment_logic:derived_status/1 严格一致：
%%   pending   = 无最新提交（s.id IS NULL）
%%   reviewing = 有最新提交 ∧ 无 published ∧ 最新提交已有老师草稿（has_draft）
%%   submitted = 有最新提交 ∧ 无 published ∧ 无草稿
%%   reviewed  = 存在 published（任一 submission，与既有 has_published 同源）
%% has_draft 只锚定「老师是否已开始批改」这一粗粒度状态位——草稿内容本身
%% 仍按 D-10 全量剥离，不随本查询外泄。
-spec assignments_for_learner(integer(), integer(), integer()) ->
    {ok, [map()], integer()} | {error, term()}.
assignments_for_learner(LearnerId, Page, Size) ->
    assignments_for_learner_run(fun elib_pg:query/2, LearnerId, Page, Size, undefined).

%% @doc CM-F4：带四态 status 过滤的列表（StatusOpt = undefined | binary）
-spec assignments_for_learner(integer(), integer(), integer(), binary() | undefined) ->
    {ok, [map()], integer()} | {error, term()}.
assignments_for_learner(LearnerId, Page, Size, StatusOpt) ->
    assignments_for_learner_run(fun elib_pg:query/2, LearnerId, Page, Size, StatusOpt).

%% @doc 事务内版本（集成测试直连）
-spec assignments_for_learner_tx(any(), integer(), integer(), integer()) ->
    {ok, [map()], integer()} | {error, term()}.
assignments_for_learner_tx(Conn, LearnerId, Page, Size) ->
    assignments_for_learner_run(
        fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, LearnerId, Page, Size, undefined
    ).

%% @doc 事务内版本 + status 过滤（CM-F4）
-spec assignments_for_learner_tx(
    any(), integer(), integer(), integer(), binary() | undefined
) -> {ok, [map()], integer()} | {error, term()}.
assignments_for_learner_tx(Conn, LearnerId, Page, Size, StatusOpt) ->
    assignments_for_learner_run(
        fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, LearnerId, Page, Size, StatusOpt
    ).

%% @doc 作业详情富行（CM-F2）：与列表同 SELECT 形状（含 description/has_draft），
%% 按 assignment id 单行取。不存在 → {ok, undefined}。
-spec assignment_detail(integer()) -> {ok, map() | undefined} | {error, term()}.
assignment_detail(AssignmentId) ->
    assignment_detail_run(fun elib_pg:query/2, AssignmentId).

-spec assignment_detail_tx(any(), integer()) -> {ok, map() | undefined} | {error, term()}.
assignment_detail_tx(Conn, AssignmentId) ->
    assignment_detail_run(
        fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, AssignmentId
    ).

%% published 存在性（assignment 级，任一 submission；与 has_published 列同源）
-define(HAS_PUB_EXPR,
    <<"(EXISTS (SELECT 1 FROM ", (tb(teacher_review))/binary,
        " trp "
        " JOIN ", (tb(homework_submission))/binary,
        " hsp ON hsp.id = trp.submission_id "
        " WHERE hsp.assignment_id = a.id AND trp.status = 'published'))">>
).

%% 最新 submission 是否已有老师草稿（reviewing 状态位；草稿内容不外泄）
-define(HAS_DRAFT_EXPR,
    <<"(EXISTS (SELECT 1 FROM ", (tb(teacher_review))/binary,
        " trd WHERE trd.submission_id = s.id AND trd.status = 'draft'))">>
).

%% 四态过滤片段（与 derived_status/1 推导口径一致）。纯字面 SQL、不含 $N
%% 占位符：调用方拼在 "WHERE a.learner_id = $1" 之后，参数位仍由调用方独占
%% （$1=learner_id，$2=size，$3=offset），本片段不引入也不偏移参数。
-spec status_condition(binary() | undefined) -> binary().
status_condition(undefined) ->
    <<>>;
status_condition(<<"pending">>) ->
    <<" AND s.id IS NULL">>;
status_condition(<<"submitted">>) ->
    <<" AND s.id IS NOT NULL AND NOT ", (?HAS_PUB_EXPR)/binary, " AND NOT ",
        (?HAS_DRAFT_EXPR)/binary>>;
status_condition(<<"reviewing">>) ->
    <<" AND s.id IS NOT NULL AND NOT ", (?HAS_PUB_EXPR)/binary, " AND ", (?HAS_DRAFT_EXPR)/binary>>;
status_condition(<<"reviewed">>) ->
    <<" AND ", (?HAS_PUB_EXPR)/binary>>;
status_condition(_) ->
    %% 调用方（handler）已白名单；仓内防御：非法值按无过滤处理由 logic 拒绝
    <<>>.

-spec assignments_for_learner_run(
    fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}),
    integer(),
    integer(),
    integer(),
    binary() | undefined
) -> {ok, [map()], integer()} | {error, term()}.
assignments_for_learner_run(Exec, LearnerId, Page, Size, StatusOpt) ->
    Offset = (Page - 1) * Size,
    StatusCond = status_condition(StatusOpt),
    Sql =
        <<
            "SELECT a.id AS assignment_id, a.task_id, a.learner_id AS learner_id, "
            "a.status AS assignment_status, "
            "gt.id AS task_gid, gt.title, gt.description, gt.deadline, gt.status AS task_status, "
            "g.id AS group_id, g.title AS group_title, "
            "s.id AS latest_submission_id, s.attempt_no AS latest_attempt_no, "
            "s.status AS latest_submission_status, "
            "la.object_key AS latest_asset_key, la.kind AS latest_asset_kind, "
            "(SELECT count(*) FROM ",
            (tb(homework_submission))/binary,
            "  hs2 WHERE hs2.assignment_id = a.id) AS submission_count, ",
            (?HAS_PUB_EXPR)/binary,
            " AS has_published, ",
            (?HAS_DRAFT_EXPR)/binary,
            " AS has_draft "
            "FROM ",
            (tb(group_task_assignment))/binary,
            " a "
            "JOIN ",
            (tb(group_task))/binary,
            " gt ON gt.task_id = a.task_id "
            "JOIN ",
            (tb(group))/binary,
            " g ON g.id = gt.group_id "
            "LEFT JOIN LATERAL ( "
            "  SELECT id, attempt_no, status FROM ",
            (tb(homework_submission))/binary,
            "  WHERE assignment_id = a.id ORDER BY attempt_no DESC LIMIT 1) s ON true "
            "LEFT JOIN LATERAL ( "
            "  SELECT att.path AS object_key, sa.kind AS kind FROM ",
            (tb(submission_asset))/binary,
            " sa JOIN ",
            (tb(attachment))/binary,
            " att ON att.id = sa.attachment_id "
            "  WHERE sa.submission_id = s.id AND sa.kind = 'final_photo' "
            "    AND att.status >= 0 "
            "  ORDER BY sa.sort_order, sa.id LIMIT 1) la ON true "
            "WHERE a.learner_id = $1",
            StatusCond/binary,
            " ORDER BY gt.created_at DESC, a.id DESC LIMIT $2 OFFSET $3"
        >>,
    case Exec(Sql, [LearnerId, Size, Offset]) of
        {ok, Rows} ->
            Total = count_assignments_run(Exec, LearnerId, StatusOpt),
            {ok, Rows, Total};
        {error, Reason} ->
            {error, Reason}
    end.

-spec assignment_detail_run(
    fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}),
    integer()
) -> {ok, map() | undefined} | {error, term()}.
assignment_detail_run(Exec, AssignmentId) ->
    Sql =
        <<
            "SELECT a.id AS assignment_id, a.task_id, a.learner_id AS learner_id, "
            "a.status AS assignment_status, "
            "gt.id AS task_gid, gt.title, gt.description, gt.deadline, gt.status AS task_status, "
            "g.id AS group_id, g.title AS group_title, "
            "s.id AS latest_submission_id, s.attempt_no AS latest_attempt_no, "
            "s.status AS latest_submission_status, "
            "la.object_key AS latest_asset_key, la.kind AS latest_asset_kind, "
            "(SELECT count(*) FROM ",
            (tb(homework_submission))/binary,
            "  hs2 WHERE hs2.assignment_id = a.id) AS submission_count, ",
            (?HAS_PUB_EXPR)/binary,
            " AS has_published, ",
            (?HAS_DRAFT_EXPR)/binary,
            " AS has_draft "
            "FROM ",
            (tb(group_task_assignment))/binary,
            " a "
            "JOIN ",
            (tb(group_task))/binary,
            " gt ON gt.task_id = a.task_id "
            "JOIN ",
            (tb(group))/binary,
            " g ON g.id = gt.group_id "
            "LEFT JOIN LATERAL ( "
            "  SELECT id, attempt_no, status FROM ",
            (tb(homework_submission))/binary,
            "  WHERE assignment_id = a.id ORDER BY attempt_no DESC LIMIT 1) s ON true "
            "LEFT JOIN LATERAL ( "
            "  SELECT att.path AS object_key, sa.kind AS kind FROM ",
            (tb(submission_asset))/binary,
            " sa JOIN ",
            (tb(attachment))/binary,
            " att ON att.id = sa.attachment_id "
            "  WHERE sa.submission_id = s.id AND sa.kind = 'final_photo' "
            "    AND att.status >= 0 "
            "  ORDER BY sa.sort_order, sa.id LIMIT 1) la ON true "
            "WHERE a.id = $1 LIMIT 1"
        >>,
    case Exec(Sql, [AssignmentId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 老师待评队列：submitted 状态即时可见，withdrawn 恒过滤；
%% ai_status 来自有效 AI 草稿（queued/running/succeeded/failed，无则 none）
-spec queue([integer()], map(), integer(), integer()) ->
    {ok, [map()], integer()} | {error, term()}.
queue(GroupIds, Filters, Page, Size) when is_list(GroupIds), GroupIds =/= [] ->
    Offset = (Page - 1) * Size,
    AssignmentId = maps:get(assignment_id, Filters, undefined),
    AiStatus = maps:get(ai_status, Filters, undefined),
    Params0 = [GroupIds, Size, Offset],
    {ACond, Params1} =
        case AssignmentId of
            undefined -> {<<"">>, Params0};
            Aid -> {<<" AND a.id = $4">>, Params0 ++ [Aid]}
        end,
    {AiCond, Params2} =
        case AiStatus of
            undefined ->
                {<<"">>, Params1};
            none ->
                {<<" AND crd.id IS NULL">>, Params1};
            St when is_binary(St) ->
                N = integer_to_binary(length(Params1) + 1),
                {<<" AND crd.status = $", N/binary>>, Params1 ++ [St]};
            _ ->
                {<<"">>, Params1}
        end,
    Sql =
        <<
            "SELECT hs.id AS submission_id, hs.assignment_id, hs.attempt_no, "
            "hs.submitted_at, l.id AS learner_id, l.display_name, "
            "a.task_id, gt.title AS task_title, g.id AS group_id, g.title AS group_title, "
            "crd.status AS ai_status, "
            "(EXISTS (SELECT 1 FROM ",
            (tb(teacher_review))/binary,
            "  tr WHERE tr.submission_id = hs.id AND tr.status = 'published')) AS has_published "
            "FROM ",
            (tb(homework_submission))/binary,
            " hs "
            "JOIN ",
            (tb(group_task_assignment))/binary,
            " a ON a.id = hs.assignment_id "
            "JOIN ",
            (tb(learner))/binary,
            " l ON l.id = hs.learner_id "
            "JOIN ",
            (tb(group_task))/binary,
            " gt ON gt.task_id = a.task_id "
            "JOIN ",
            (tb(group))/binary,
            " g ON g.id = gt.group_id "
            "LEFT JOIN LATERAL (SELECT id, status FROM ",
            (tb(calligraphy_review_draft))/binary,
            "  WHERE submission_id = hs.id AND status IN ('queued','running','succeeded','failed') "
            "  ORDER BY created_at DESC LIMIT 1) crd ON true "
            "WHERE hs.status = 'submitted' AND g.id = ANY($1)",
            ACond/binary,
            AiCond/binary,
            " "
            "ORDER BY hs.submitted_at ASC LIMIT $2 OFFSET $3"
        >>,
    case elib_pg:query(Sql, Params2) of
        {ok, Rows} ->
            {ok, Rows, count_queue(GroupIds, AiCond, Params2)};
        {error, Reason} ->
            {error, Reason}
    end;
queue([], _Filters, _Page, _Size) ->
    {ok, [], 0}.

%% @doc 学员历史：全部 submission（withdrawn 标记可见）+ 已发布回评引用
-spec history(integer(), integer(), integer()) -> {ok, [map()], integer()} | {error, term()}.
history(LearnerId, Page, Size) ->
    Offset = (Page - 1) * Size,
    Sql =
        <<
            "SELECT hs.id AS submission_id, hs.assignment_id, hs.attempt_no, hs.status, "
            "hs.submitted_at, hs.withdrawn_at, "
            "a.task_id, gt.title AS task_title, g.id AS group_id, g.title AS group_title, "
            "w.id AS workspace_id, "
            "tr.id AS published_review_id, tr.positive_point, tr.focus_problem, "
            "tr.practice_action, tr.comment, tr.char_reviews, tr.video_attachment_id, "
            "tr.rework_required, tr.published_at, "
            %% CM-F2：老师署名行内解析（与 load_submission_bundle 的
            %% reviewer_display_name 同语义——查无/空昵称 → null，前端隐藏署名位）。
            %% 行内 JOIN 单查询取回，不新增 Erlang 层真连库路径。
            "NULLIF(uu.nickname, '') AS reviewer_display_name "
            "FROM ",
            (tb(homework_submission))/binary,
            " hs "
            "JOIN ",
            (tb(group_task_assignment))/binary,
            " a ON a.id = hs.assignment_id "
            "JOIN ",
            (tb(group_task))/binary,
            " gt ON gt.task_id = a.task_id "
            "JOIN ",
            (tb(group))/binary,
            " g ON g.id = gt.group_id "
            "JOIN ",
            (tb(workspace))/binary,
            " w ON w.id = g.workspace_id "
            "LEFT JOIN ",
            (tb(teacher_review))/binary,
            " tr "
            " ON tr.submission_id = hs.id AND tr.status = 'published' "
            "LEFT JOIN \"user\" uu ON uu.id = tr.reviewer_uid "
            "WHERE hs.learner_id = $1 "
            "ORDER BY hs.submitted_at DESC LIMIT $2 OFFSET $3"
        >>,
    case elib_pg:query(Sql, [LearnerId, Size, Offset]) of
        {ok, Rows} ->
            {ok, Rows, count_history(LearnerId)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 家长未读点评数：learner 的 published 回评中 published_at > Since 的条数。
%% Since 为 RFC3339 binary（客户端原样回传上一响应的 published_at 值域）；
%% undefined/<<>> 计全部已发布。join/状态口径与 history/3 一致。
-spec history_unread_count(integer(), binary() | undefined) ->
    {ok, non_neg_integer()} | {error, term()}.
history_unread_count(LearnerId, Since) ->
    {SinceClause, Args} =
        case Since of
            B when is_binary(B), B =/= <<>> ->
                {<<" AND tr.published_at > $2::timestamptz">>, [LearnerId, B]};
            _ ->
                {<<>>, [LearnerId]}
        end,
    Sql =
        <<"SELECT COUNT(*)::bigint AS cnt FROM ", (tb(homework_submission))/binary, " hs JOIN ",
            (tb(teacher_review))/binary,
            " tr ON tr.submission_id = hs.id AND tr.status = 'published' "
            "WHERE hs.learner_id = $1", SinceClause/binary>>,
    case elib_pg:query(Sql, Args) of
        {ok, [#{<<"cnt">> := Cnt}]} ->
            {ok, ec_cnv:to_integer(Cnt)};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec digest_check(map(), binary(), integer()) -> {ok, map()} | {error, idempotency_conflict}.
digest_check(#{<<"request_digest">> := Digest} = Row, Digest, AttemptNo) ->
    {ok, Row#{created => maps:get(<<"attempt_no">>, Row, 0) =:= AttemptNo}};
digest_check(#{<<"request_digest">> := _Other}, _Digest, _AttemptNo) ->
    %% 同 key 不同载荷：5460，不覆盖原结果（契约 T8b）
    {error, idempotency_conflict};
digest_check(Row, _Digest, AttemptNo) ->
    %% 历史行无 digest（NULL）：仅当 attempt 一致才视为重放
    case maps:get(<<"attempt_no">>, Row, 0) of
        AttemptNo -> {ok, Row#{created => false}};
        _ -> {error, idempotency_conflict}
    end.

-spec count_assignments(integer()) -> integer().
count_assignments(LearnerId) ->
    count_assignments_run(fun elib_pg:query/2, LearnerId, undefined).

-spec count_assignments_run(
    fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}),
    integer(),
    binary() | undefined
) -> integer().
%% 无过滤：维持既有简单计数（零 JOIN，与历史 total 口径一致）。
count_assignments_run(Exec, LearnerId, undefined) ->
    Sql =
        <<"SELECT count(*) AS c FROM ", (tb(group_task_assignment))/binary,
            " WHERE learner_id = $1">>,
    case Exec(Sql, [LearnerId]) of
        {ok, [#{<<"c">> := C} | _]} -> C;
        _ -> 0
    end;
%% CM-F4：status 过滤态计数必须与列表行同一 FROM/JOIN/推导谓词，
%% 否则 total 与 list 脱钩（分页 UI 会算错 hasNext）。
count_assignments_run(Exec, LearnerId, StatusOpt) ->
    Sql =
        <<"SELECT count(*) AS c FROM ", (tb(group_task_assignment))/binary,
            " a "
            "JOIN ", (tb(group_task))/binary,
            " gt ON gt.task_id = a.task_id "
            "JOIN ", (tb(group))/binary,
            " g ON g.id = gt.group_id "
            "LEFT JOIN LATERAL ( "
            "  SELECT id, attempt_no, status FROM ", (tb(homework_submission))/binary,
            "  WHERE assignment_id = a.id ORDER BY attempt_no DESC LIMIT 1) s ON true "
            "WHERE a.learner_id = $1", (status_condition(StatusOpt))/binary>>,
    case Exec(Sql, [LearnerId]) of
        {ok, [#{<<"c">> := C} | _]} -> C;
        _ -> 0
    end.

-spec count_queue([integer()], binary(), [term()]) -> integer().
count_queue(_GroupIds, AiCond, [GroupIds | Rest]) ->
    Base =
        <<"SELECT count(*) AS c FROM ", (tb(homework_submission))/binary,
            " hs "
            "JOIN ", (tb(group_task_assignment))/binary,
            " a ON a.id = hs.assignment_id "
            "JOIN ", (tb(group_task))/binary,
            " gt ON gt.task_id = a.task_id "
            "JOIN ", (tb(group))/binary,
            " g ON g.id = gt.group_id "
            "LEFT JOIN LATERAL (SELECT id, status FROM ", (tb(calligraphy_review_draft))/binary,
            "  WHERE submission_id = hs.id AND status IN ('queued','running','succeeded','failed') "
            "  ORDER BY created_at DESC LIMIT 1) crd ON true "
            "WHERE hs.status = 'submitted' AND g.id = ANY($1)", AiCond/binary>>,
    %% 计数参数 = [GroupIds] + 附加筛选（去掉第 2/3 位 LIMIT/OFFSET）
    CountParams =
        [GroupIds] ++
            case Rest of
                [_Limit, _Offset | Extra] -> Extra;
                _ -> []
            end,
    case elib_pg:query(Base, CountParams) of
        {ok, [#{<<"c">> := C} | _]} -> C;
        _ -> 0
    end.

-spec count_history(integer()) -> integer().
count_history(LearnerId) ->
    Sql =
        <<"SELECT count(*) AS c FROM ", (tb(homework_submission))/binary,
            " WHERE learner_id = $1">>,
    case elib_pg:query(Sql, [LearnerId]) of
        {ok, [#{<<"c">> := C} | _]} -> C;
        _ -> 0
    end.

%% @doc 按附件 object_key（attachment.path）查绑定的 submission（Step 10 读授权用）：
%% submission_asset → homework_submission；任一跳缺失 → {ok, undefined}
-spec submission_for_asset_path(binary()) -> {ok, map() | undefined} | {error, term()}.
submission_for_asset_path(Path) ->
    asset_path_run(fun elib_pg:query/2, Path).

%% @doc 事务内版本（集成测试直连）
-spec submission_for_asset_path_tx(any(), binary()) -> {ok, map() | undefined} | {error, term()}.
submission_for_asset_path_tx(Conn, Path) ->
    asset_path_run(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, Path).

-spec asset_path_run(fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}), binary()) ->
    {ok, map() | undefined} | {error, term()}.
asset_path_run(Exec, Path) ->
    Sql =
        <<
            "SELECT sa.submission_id, hs.status AS submission_status "
            "FROM ",
            (tb(submission_asset))/binary,
            " sa "
            "JOIN ",
            (tb(attachment))/binary,
            " att ON att.id = sa.attachment_id "
            "JOIN ",
            (tb(homework_submission))/binary,
            " hs ON hs.id = sa.submission_id "
            "WHERE att.path = $1 AND att.status >= 0 LIMIT 1"
        >>,
    case Exec(Sql, [Path]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 超龄未绑定的教学附件（Step 10 孤儿清理用）：
%% scope='teaching' 且 created_at 超龄 且 NOT EXISTS submission_asset 且
%% NOT EXISTS review_asset（P0-4 双排除：被任一业务关联引用——含草稿引用——
%% 的附件一律不列出，MEDIA-02 不误删；撤回 submission 的证据附件同保护）
-spec unbound_teaching_attachments(integer()) -> {ok, [map()]} | {error, term()}.
unbound_teaching_attachments(AgeHours) ->
    unbound_run(fun elib_pg:query/2, AgeHours).

%% @doc 事务内版本（集成测试直连）
-spec unbound_teaching_attachments_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
unbound_teaching_attachments_tx(Conn, AgeHours) ->
    unbound_run(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, AgeHours).

-spec unbound_run(fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}), integer()) ->
    {ok, [map()]} | {error, term()}.
unbound_run(Exec, AgeHours) ->
    Sql =
        <<"SELECT id, path, creator_user_id FROM ", (tb(attachment))/binary,
            " WHERE scope = 'teaching' AND status >= 0 "
            "  AND created_at < now() - ($1 || ' hours')::interval "
            "  AND NOT EXISTS (SELECT 1 FROM ", (tb(submission_asset))/binary,
            "   sa WHERE sa.attachment_id = ", (tb(attachment))/binary,
            ".id) "
            "  AND NOT EXISTS (SELECT 1 FROM ", (tb(review_asset))/binary,
            "   ra WHERE ra.attachment_id = ", (tb(attachment))/binary,
            ".id) "
            "LIMIT 100">>,
    case Exec(Sql, [AgeHours]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

-spec tb(atom()) -> binary().
tb(group) ->
    %% GROUP 是保留字，SQL 内必须带双引号引用——只引表名段；
    %% sql_driver=pgsql 时 tablename 已带 public. 前缀，整段加引号会生成
    %% "public.group"（单个带点标识符 → 42P01 undefined_table，
    %% R8 契约实测在冒烟节点发现）。
    case tablename(<<"group">>) of
        <<"public.", T/binary>> ->
            <<"public.\"", T/binary, "\"">>;
        T ->
            <<"\"", T/binary, "\"">>
    end;
tb(Tb) ->
    tablename(ec_cnv:to_binary(Tb)).
