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
-export([validate_assets_tx/3, replace_assets_tx/4, assets/1, assets_tx/2]).
-export([review_for_asset_path/1, review_for_asset_path_tx/2]).
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
                    " WHERE id = $1 AND submission_id = $2 AND reviewer_uid = $3 "
                    "   AND status = 'draft' RETURNING *">>,
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

%% ------------------------------------------------------------------
%% P0-4 回评媒体（review_asset，MN-MEDIA-01/02）：校验 / 原子替换 / 读取 / 读授权数据面
%% ------------------------------------------------------------------

%% @doc 回评附件归属校验（save_draft 事务内、写入前调用）：
%%   * attachment 行必须存在——不存在与非本人创建同响应 not_found（防存在性探测，
%%     与 teaching_acl T14 同哲学）
%%   * status >= 0（active，-1 软删拒绝）
%%   * scope = 'teaching'
%%   * creator_user_id = 当前 reviewer（强归属）
%%   * MIME 前缀与 kind 匹配（video/* ↔ feedback_video，image/* ↔ feedback_image）
%% 数量上限（0-1 video + 0-3 image）由 Logic 解析层先行；DB 触发器/部分唯一索引兜底。
%% Assets = [{AttId, Kind, SortOrder}]，校验通过原样返回（保持请求顺序）。
-spec validate_assets_tx(any(), integer(), [{integer(), binary(), integer()}]) ->
    {ok, [{integer(), binary(), integer()}]} | {error, not_found | assets_invalid | term()}.
validate_assets_tx(_Conn, _Uid, []) ->
    {ok, []};
validate_assets_tx(Conn, Uid, Assets) when is_list(Assets) ->
    Ids = [AttId || {AttId, _, _} <- Assets],
    Sql =
        <<"SELECT id, mime_type, status, creator_user_id, scope FROM ", (tb(attachment))/binary,
            " WHERE id = ANY($1)">>,
    case elib_pg:query(Conn, Sql, [Ids]) of
        {ok, Rows} ->
            case length(Rows) =:= length(lists:usort(Ids)) of
                false ->
                    {error, not_found};
                true ->
                    ById = maps:from_list([{maps:get(<<"id">>, R), R} || R <- Rows]),
                    case check_each_asset(ById, Assets, Uid) of
                        ok -> {ok, Assets};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 原子替换 review 的媒体集合（delete+insert 同事务，save_draft 调用）：
%% 只解除 review_asset 关联，绝不物理删 attachment 对象（对象生命周期独立）。
%% 任一 INSERT 撞约束（attachment 全表唯一/单视频/3 图触发器）由调用方回滚整单。
-spec replace_assets_tx(any(), integer(), integer(), [{integer(), binary(), integer()}]) ->
    ok | {error, term()}.
replace_assets_tx(Conn, ReviewId, Uid, Assets) ->
    Del =
        <<"DELETE FROM ", (tb(review_asset))/binary, " WHERE review_id = $1">>,
    case elib_pg:execute(Conn, Del, [ReviewId]) of
        {ok, _} ->
            insert_review_assets(Conn, ReviewId, Uid, Assets);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc review 的媒体集合（池连接；DTO 组装用）
-spec assets(integer()) -> {ok, [map()]} | {error, term()}.
assets(ReviewId) ->
    assets_run(fun elib_pg:query/2, ReviewId).

%% @doc 事务内版本（与 save_draft/publish 同连接）
-spec assets_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
assets_tx(Conn, ReviewId) ->
    assets_run(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, ReviewId).

%% @doc 按附件 object_key（attachment.path）查 review_asset 绑定（P0-4 读授权数据面）：
%% review_asset → teacher_review → homework_submission；任一跳缺失 → {ok, undefined}。
%% 返回行含 review_status/reviewer_uid/submission_id/submission_status，
%% 供 Logic 层分流（draft 仅 reviewer / published 复用 submission_access / withdrawn fail closed）。
-spec review_for_asset_path(binary()) -> {ok, map() | undefined} | {error, term()}.
review_for_asset_path(Path) ->
    review_asset_path_run(fun elib_pg:query/2, Path).

%% @doc 事务内版本（集成测试直连）
-spec review_for_asset_path_tx(any(), binary()) -> {ok, map() | undefined} | {error, term()}.
review_for_asset_path_tx(Conn, Path) ->
    review_asset_path_run(fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end, Path).

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

%% ---- P0-4 review_asset internals ----

-spec check_each_asset(map(), [{integer(), binary(), integer()}], integer()) ->
    ok | {error, not_found | assets_invalid}.
check_each_asset(_ById, [], _Uid) ->
    ok;
check_each_asset(ById, [{AttId, Kind, _Order} | Rest], Uid) ->
    case maps:get(AttId, ById, undefined) of
        %% 不存在 / 非本人创建：同 not_found（防存在性探测）
        undefined ->
            {error, not_found};
        #{<<"creator_user_id">> := Uid} = Row ->
            case asset_active_scoped(Row) andalso mime_kind_match(Row, Kind) of
                true -> check_each_asset(ById, Rest, Uid);
                false -> {error, assets_invalid}
            end;
        _ ->
            {error, not_found}
    end.

-spec asset_active_scoped(map()) -> boolean().
asset_active_scoped(#{<<"status">> := S, <<"scope">> := <<"teaching">>}) when S >= 0 ->
    true;
asset_active_scoped(_) ->
    false.

-spec mime_kind_match(map(), binary()) -> boolean().
mime_kind_match(#{<<"mime_type">> := <<"video/", _/binary>>}, <<"feedback_video">>) ->
    true;
mime_kind_match(#{<<"mime_type">> := <<"image/", _/binary>>}, <<"feedback_image">>) ->
    true;
mime_kind_match(_, _) ->
    false.

-spec insert_review_assets(any(), integer(), integer(), [{integer(), binary(), integer()}]) ->
    ok | {error, term()}.
insert_review_assets(_Conn, _ReviewId, _Uid, []) ->
    ok;
insert_review_assets(Conn, ReviewId, Uid, Assets) ->
    Sql =
        <<"INSERT INTO ", (tb(review_asset))/binary,
            " (id, review_id, attachment_id, kind, sort_order, created_by) "
            "VALUES ($1, $2, $3, $4, $5, $6)">>,
    Results =
        [
            elib_pg:execute(Conn, Sql, [
                elib_tsid:generate(), ReviewId, AttId, Kind, Order, Uid
            ])
         || {AttId, Kind, Order} <- Assets
        ],
    case [E || {error, E} <- Results] of
        [] -> ok;
        [First | _] -> {error, First}
    end.

-spec assets_run(fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}), integer()) ->
    {ok, [map()]} | {error, term()}.
assets_run(Exec, ReviewId) ->
    Sql =
        <<
            "SELECT ra.review_id, ra.attachment_id, ra.kind, ra.sort_order, "
            "att.path AS object_key FROM ",
            (tb(review_asset))/binary,
            " ra JOIN ",
            (tb(attachment))/binary,
            " att ON att.id = ra.attachment_id "
            "WHERE ra.review_id = $1 ORDER BY kind, sort_order"
        >>,
    case Exec(Sql, [ReviewId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

-spec review_asset_path_run(
    fun((binary(), [term()]) -> {ok, [map()]} | {error, term()}), binary()
) ->
    {ok, map() | undefined} | {error, term()}.
review_asset_path_run(Exec, Path) ->
    Sql =
        <<
            "SELECT ra.review_id, tr.status AS review_status, tr.reviewer_uid, ",
            "tr.submission_id, hs.status AS submission_status ",
            "FROM ",
            (tb(review_asset))/binary,
            " ra ",
            "JOIN ",
            (tb(attachment))/binary,
            " att ON att.id = ra.attachment_id ",
            "JOIN ",
            (tb(teacher_review))/binary,
            " tr ON tr.id = ra.review_id ",
            "JOIN ",
            (tb(homework_submission))/binary,
            " hs ON hs.id = tr.submission_id ",
            "WHERE att.path = $1 AND att.status >= 0 LIMIT 1"
        >>,
    case Exec(Sql, [Path]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

-spec tb(atom()) -> binary().
tb(Tb) ->
    tablename(ec_cnv:to_binary(Tb)).
