-module(teaching_review_repo).
%%%
% 墨芽老师回评数据仓库（Step 9）
% Teacher review repository
%
% 配方③（STEP-08-DB/notes.md）：发布必须 lock-first —— 先 FOR UPDATE
% homework_submission 行，再对 teacher_review 做 draft→published 一次性条件更新；
%% DB 触发器（trg_teacher_review_publish_guard）为兜底而非主防线。
%%%

-export([tablename/1]).
-export([upsert_draft_tx/3, find_draft/2, find_published/1, find_published_tx/2, publish_tx/3]).
-export([ai_draft/1]).
-export([
    claim_next_queued_tx/1,
    ai_finish_success_tx/4,
    ai_finish_failed_tx/3,
    ai_requeue_tx/3
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(REVIEW_FIELDS, <<
    "positive_point, focus_problem, practice_action, comment, "
    "video_attachment_id, rework_required"
>>).

%%%===================================================================
%%% API
%%%===================================================================

-spec tablename(binary()) -> binary().
tablename(Tb) ->
    elib_pg_sql:public_tablename(Tb).

%% ------------------------------------------------------------------
%% 草稿 upsert：每 (submission, reviewer) 至多一份有效草稿（重复 PUT 覆盖）
%% ------------------------------------------------------------------

-spec upsert_draft_tx(any(), integer(), map()) -> {ok, map()} | {error, term()}.
upsert_draft_tx(Conn, SubmissionId, #{uid := Uid} = Fields) ->
    case find_draft_tx(Conn, SubmissionId, Uid) of
        {ok, #{<<"id">> := DraftId}} ->
            Sql =
                <<"UPDATE ", (tb(teacher_review))/binary,
                    " SET positive_point = $4, focus_problem = $5, practice_action = $6, "
                    "comment = $7, video_attachment_id = $8, rework_required = $9, "
                    "updated_at = now() "
                    " WHERE id = $1 AND submission_id = $2 AND status = 'draft' RETURNING *">>,
            unwrap_row(
                elib_pg:query(Conn, Sql, [DraftId, SubmissionId, Uid | field_values(Fields)])
            );
        {ok, undefined} ->
            Sql =
                <<"INSERT INTO ", (tb(teacher_review))/binary,
                    " (id, submission_id, reviewer_uid, ", (?REVIEW_FIELDS)/binary,
                    ") "
                    "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9) RETURNING *">>,
            unwrap_row(
                elib_pg:query(
                    Conn,
                    Sql,
                    [
                        elib_tsid:generate(),
                        SubmissionId,
                        Uid
                        | field_values(Fields)
                    ]
                )
            );
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 指定老师的草稿（自动连接池）
-spec find_draft(integer(), integer()) -> {ok, map() | undefined} | {error, term()}.
find_draft(SubmissionId, Uid) ->
    Sql =
        <<"SELECT * FROM ", (tb(teacher_review))/binary,
            " WHERE submission_id = $1 AND reviewer_uid = $2 AND status = 'draft' LIMIT 1">>,
    case elib_pg:query(Sql, [SubmissionId, Uid]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc submission 的已发布回评（uk_tr_published_per_submission 至多一条）
-spec find_published(integer()) -> {ok, map() | undefined} | {error, term()}.
find_published(SubmissionId) ->
    find_published_run(fun elib_pg:query/2, SubmissionId).

%% @doc 事务内版本（与 publish_tx 同连接）
-spec find_published_tx(any(), integer()) -> {ok, map() | undefined} | {error, term()}.
find_published_tx(Conn, SubmissionId) ->
    find_published_run(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, SubmissionId).

-spec find_published_run(fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}), integer()) ->
    {ok, map() | undefined} | {error, term()}.
find_published_run(Exec, SubmissionId) ->
    Sql =
        <<"SELECT * FROM ", (tb(teacher_review))/binary,
            " WHERE submission_id = $1 AND status = 'published' LIMIT 1">>,
    case Exec(Sql, [SubmissionId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% ------------------------------------------------------------------
%% 配方③：发布（draft→published 一次性条件更新）
%% 前置：调用方已 lock submission 行（lock_submission_tx）。
%% 返回 {ok, published, Review} | {ok, already_published, Review}
%%     | {error, no_draft | withdrawn | term()}
%% ------------------------------------------------------------------

-spec publish_tx(any(), integer(), integer()) ->
    {ok, published | already_published, map()} | {error, no_draft | withdrawn | term()}.
publish_tx(Conn, SubmissionId, Uid) ->
    Sql =
        <<"UPDATE ", (tb(teacher_review))/binary,
            " SET status = 'published', published_at = now(), updated_at = now() "
            " WHERE submission_id = $1 AND reviewer_uid = $2 AND status = 'draft' "
            "   AND (SELECT status FROM ", (tb(homework_submission))/binary,
            "        WHERE id = $1) = 'submitted' "
            " RETURNING *">>,
    case elib_pg:query(Conn, Sql, [SubmissionId, Uid]) of
        {ok, [Review | _]} ->
            {ok, published, Review};
        {ok, []} ->
            publish_zero_rows(Conn, SubmissionId, Uid);
        {error, Reason} ->
            {error, Reason}
    end.

-spec unwrap_row({ok, [map()]} | {error, term()}) -> {ok, map()} | {error, term()}.
unwrap_row({ok, [Row | _]}) ->
    {ok, Row};
unwrap_row({ok, []}) ->
    {error, no_returned_row};
unwrap_row({error, Reason}) ->
    {error, Reason}.

%% @doc AI 草稿（老师视角）：有效草稿行 + result_json
-spec ai_draft(integer()) -> {ok, map() | undefined} | {error, term()}.
ai_draft(SubmissionId) ->
    Sql =
        <<
            "SELECT id, submission_id, status, model_profile, prompt_version, rubric_version, "
            "input_digest, result_json, error_code, created_at, completed_at "
            "FROM ",
            (tb(calligraphy_review_draft))/binary,
            " WHERE submission_id = $1 AND status IN ('queued','running','succeeded','failed') "
            " ORDER BY created_at DESC LIMIT 1"
        >>,
    case elib_pg:query(Sql, [SubmissionId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% ------------------------------------------------------------------
%% AI Worker 队列操作（Step 11）：
%% 队列载体 = calligraphy_review_draft 行本身（不加新表——评审记录见
%% STEP-11/notes.md「队列载体决策」）。原子取任务 = 单语句条件 UPDATE +
%% 子查询 FOR UPDATE SKIP LOCKED：0 行更新 = 被其他 worker 抢走。
%% ------------------------------------------------------------------

%% @doc 抢占最老的 queued 草稿（原子；SKIP LOCKED 防多 worker 互堵）。
%% 不覆盖 ai_task_id——它兼任重试计数（NULL=首次，"run:N"=第 N 次尝试）。
-spec claim_next_queued_tx(any()) -> {ok, map() | undefined} | {error, term()}.
claim_next_queued_tx(Conn) ->
    Sql =
        <<"UPDATE ", (tb(calligraphy_review_draft))/binary,
            " SET status = 'running' "
            " WHERE id = (SELECT id FROM ", (tb(calligraphy_review_draft))/binary,
            "  WHERE status = 'queued' ORDER BY created_at LIMIT 1 FOR UPDATE SKIP LOCKED) "
            " RETURNING id, submission_id, ai_task_id">>,
    case elib_pg:query(Conn, Sql, []) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 成功终态：仅 running 行可写（防半成品覆盖 succeeded/failed 终态）
-spec ai_finish_success_tx(any(), integer(), binary(), map()) ->
    ok | {error, term()}.
ai_finish_success_tx(Conn, DraftId, ModelProfile, ResultJson) ->
    Sql =
        <<"UPDATE ", (tb(calligraphy_review_draft))/binary,
            " SET status = 'succeeded', result_json = $2, model_profile = $3, "
            " completed_at = now() "
            " WHERE id = $1 AND status = 'running'">>,
    case elib_pg:execute(Conn, Sql, [DraftId, jsone:encode(ResultJson), ModelProfile]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, not_running};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 失败终态：error_code 落库；老师人工队列仍显示该 submission（Step 9 队列
%% 查询不过滤 ai_status，failed 仍进老师工作台）
-spec ai_finish_failed_tx(any(), integer(), binary()) -> ok | {error, term()}.
ai_finish_failed_tx(Conn, DraftId, ErrorCode) ->
    Sql =
        <<"UPDATE ", (tb(calligraphy_review_draft))/binary,
            " SET status = 'failed', error_code = $2, completed_at = now() "
            " WHERE id = $1 AND status = 'running'">>,
    case elib_pg:execute(Conn, Sql, [DraftId, ErrorCode]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, not_running};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 瞬时失败重试：回到 queued、清审计字段（uk_crd_active_per_submission
%% 仍保证每 submission 单一有效草稿）
-spec ai_requeue_tx(any(), integer(), binary()) -> ok | {error, term()}.
ai_requeue_tx(Conn, DraftId, NextTaskId) ->
    Sql =
        <<"UPDATE ", (tb(calligraphy_review_draft))/binary,
            " SET status = 'queued', ai_task_id = $2, error_code = NULL, completed_at = NULL "
            " WHERE id = $1 AND status = 'running'">>,
    case elib_pg:execute(Conn, Sql, [DraftId, NextTaskId]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, not_running};
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec find_draft_tx(any(), integer(), integer()) -> {ok, map() | undefined} | {error, term()}.
find_draft_tx(Conn, SubmissionId, Uid) ->
    Sql =
        <<"SELECT * FROM ", (tb(teacher_review))/binary,
            " WHERE submission_id = $1 AND reviewer_uid = $2 AND status = 'draft' LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [SubmissionId, Uid]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

-spec publish_zero_rows(any(), integer(), integer()) ->
    {ok, already_published, map()} | {error, no_draft | withdrawn}.
publish_zero_rows(Conn, SubmissionId, Uid) ->
    case teaching_submission_repo:lock_submission_tx(Conn, SubmissionId) of
        {ok, #{<<"status">> := <<"withdrawn">>}} ->
            {error, withdrawn};
        _ ->
            case find_published_tx(Conn, SubmissionId) of
                {ok, Pub} when is_map(Pub) ->
                    {ok, already_published, Pub};
                _ ->
                    case find_draft_tx(Conn, SubmissionId, Uid) of
                        {ok, undefined} -> {error, no_draft};
                        _ -> {error, no_draft}
                    end
            end
    end.

%% 字段值顺序 = ?REVIEW_FIELDS：positive_point, focus_problem, practice_action,
%% comment, video_attachment_id, rework_required（$4..$9）
-spec field_values(map()) -> [term()].
field_values(F) ->
    [
        maps:get(positive_point, F, <<>>),
        maps:get(focus_problem, F, <<>>),
        maps:get(practice_action, F, <<>>),
        maps:get(comment, F, <<>>),
        maps:get(video_attachment_id, F, null),
        maps:get(rework_required, F, false)
    ].

-spec tb(atom()) -> binary().
tb(Tb) ->
    tablename(ec_cnv:to_binary(Tb)).
