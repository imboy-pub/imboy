-module(moya_review_logic).
%%%
% 墨芽老师侧回评 + 提交详情/撤回/历史业务逻辑（Step 9）
% Teacher-side review queue/workbench/draft/publish + view-aware detail
%
% 发布配方③（STEP-08-DB）：lock-first —— with_tx 内先 lock_submission_tx
%% 再 publish_tx 条件更新；重复发布返回 already_published（幂等友好）。
%%%

-export([
    queue/3,
    workbench/2,
    save_draft/3,
    publish/3,
    withdraw/2,
    submission_detail/2,
    history/3,
    history_unread_count/3,
    %% 纯函数导出供 eunit 直测（v3 N5/P1-3 补测）
    review_has_content/2,
    %% 纯函数导出供 eunit 直测：PublishedReview DTO（老师署名字段）
    published_review_payload/3,
    %% 纯函数导出供 eunit 直测：逐字点评字卡（char_reviews Phase A）解析与读侧解码
    parse_char_reviews/1,
    char_reviews_payload/1
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 契约保留字段（T7：伪造 reviewer_uid/status/published_at → 5484）
-define(RESERVED_BODY_KEYS, [<<"reviewer_uid">>, <<"status">>, <<"published_at">>]).

%% 逐字点评字卡（char_reviews 契约：docs/plans/2026-09-12-char-review-dto-proposal.md）
-define(CHAR_REVIEWS_MAX, 50).
-define(CHAR_GRADES, [<<"good">>, <<"fair">>, <<"poor">>]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 老师待评队列（withdrawn 恒过滤；ai_status 含 queued/failed 展示）
-spec queue(integer(), map(), {integer(), integer()}) -> {ok, map()} | {error, atom()}.
queue(Uid, Filters, {Page, Size}) ->
    case moya_context_repo:staff_contexts(Uid) of
        {ok, StaffCtxs} when StaffCtxs =/= [] ->
            GroupIds = [maps:get(<<"group_id">>, R) || R <- StaffCtxs],
            queue_with_groups(Uid, GroupIds, Filters, Page, Size);
        {ok, []} ->
            %% 无任何任教班级：空队列（deny-by-default 的空态，不暴露他人数据）
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?LOG_ERROR("queue staff contexts db error ~p", [Reason]),
            {error, db_error}
    end.

%% @doc 回评工作台聚合（仅本班 staff；assistant 只读可进）
-spec workbench(integer(), integer()) -> {ok, map()} | {error, atom()}.
workbench(Uid, SubmissionId) ->
    case moya_acl:submission_access(Uid, SubmissionId) of
        {ok, staff, _Scope} ->
            build_workbench(Uid, SubmissionId);
        {ok, guardian, _} ->
            %% 家长身份不进工作台（T6：AI 草稿仅老师可见）
            {error, not_staff};
        {error, not_found} ->
            {error, not_found};
        {error, _} ->
            {error, not_staff}
    end.

%% @doc 保存/更新回评草稿（upsert per reviewer；忽略并拒绝保留字段）
%% 事务内先 lock submission 行（与 publish/withdraw 同串行化点）：防并发
%% 双请求同时判定"无草稿"而双 INSERT（每 (submission,reviewer) 至多一份有效草稿）
%% P0-4（MN-MEDIA-02/03）：Body 可携带 assets[{attachment_id,kind,sort_order}]
%% （0-1 feedback_video + 0-3 feedback_image，TSID string；空数组合法）。
%% 同一事务内校验（存在/active/scope=teaching/creator=reviewer/MIME↔kind）
%% → 原子替换 review_asset 集合；任一非法整单 rollback 零部分写入。
%% 旧字段 video_attachment_id 兼容：assets 缺席时按单视频解析；两者同时
%% 提供且不一致 → assets_invalid（双写歧义拒绝）。
%% teacher_review.video_attachment_id 旧列从 assets 第一条 video 派生冗余写
%% （兼容读窗口），不再作为写入真源。
-spec save_draft(integer(), integer(), map()) -> {ok, map()} | {error, atom()}.
save_draft(Uid, SubmissionId, Body) ->
    case reserved_keys(Body) of
        true ->
            {error, reserved_field};
        false ->
            case parse_review_assets(Body) of
                {ok, Assets} ->
                    case parse_char_reviews(Body) of
                        {ok, CharReviews} ->
                            save_draft_with_assets(Uid, SubmissionId, Body, Assets, CharReviews);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec save_draft_with_assets(
    integer(), integer(), map(), [{integer(), binary(), integer()}], [map()] | null
) ->
    {ok, map()} | {error, atom()}.
save_draft_with_assets(Uid, SubmissionId, Body, Assets, CharReviews) ->
    case draft_guard(Uid, SubmissionId) of
        {ok, _GroupId} ->
            Fields = draft_fields(Uid, Body, Assets, CharReviews),
            Tx = fun(Conn) ->
                case moya_submission_repo:lock_submission_tx(Conn, SubmissionId) of
                    {ok, Row} when is_map(Row) ->
                        %% R22-WITHDRAW-RACE-01：draft_guard 的 ACL 在事务外，
                        %% 监护人可在 ACL 通过后撤回；锁行（与 publish/withdraw
                        %% 同串行化点）后必须复查 status——withdrawn 提交
                        %% 不得写入草稿，映射 {error, withdrawn} → 5482
                        case maps:get(<<"status">>, Row, <<>>) of
                            <<"withdrawn">> -> {rollback, withdrawn};
                            _ -> draft_assets_tx(Conn, SubmissionId, Uid, Fields, Assets)
                        end;
                    {ok, undefined} ->
                        {rollback, not_found};
                    {error, Reason} ->
                        {rollback, {db, Reason}}
                end
            end,
            case elib_pg:with_tx(Tx, [{reraise, false}]) of
                {ok, {Row, AssetRows}} when is_map(Row) ->
                    {ok, review_payload(Row, AssetRows)};
                {rollback, not_found} ->
                    {error, not_found};
                {rollback, withdrawn} ->
                    {error, withdrawn};
                {rollback, review_published} ->
                    {error, review_published};
                {rollback, assets_invalid} ->
                    {error, assets_invalid};
                {rollback, not_found_asset} ->
                    {error, not_found};
                {rollback, _} ->
                    {error, db_error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 草稿 + 媒体同事务：upsert（旧列冗余写派生视频）→ 校验（写入前）→ 原子替换 → 回读
%% R22-PUBLISHED-DRAFT-01：lock 之后、upsert 之前查已发布回评——
%% upsert_draft_tx 内 find_draft_tx 只认 status='draft'，该 reviewer 草稿
%% 已 publish 后无 draft 行会让 upsert 走 INSERT 建第二条草稿；命中已发布
%% 行必须拒绝。DC-2：存草稿场景用独立 reason review_published（→ 5486），
%% already_reviewed（5481）保留给撤回场景，避免「不可撤回」文案语境错位
-spec draft_assets_tx(
    any(), integer(), integer(), map(), [{integer(), binary(), integer()}]
) ->
    {ok, {map(), [map()]}} | {rollback, term()}.
draft_assets_tx(Conn, SubmissionId, Uid, Fields, Assets) ->
    case moya_review_repo:find_published_tx(Conn, SubmissionId) of
        {ok, undefined} ->
            upsert_draft_flow_tx(Conn, SubmissionId, Uid, Fields, Assets);
        {ok, _Published} ->
            {rollback, review_published};
        {error, Reason} ->
            {rollback, {db, Reason}}
    end.

-spec upsert_draft_flow_tx(
    any(), integer(), integer(), map(), [{integer(), binary(), integer()}]
) ->
    {ok, {map(), [map()]}} | {rollback, term()}.
upsert_draft_flow_tx(Conn, SubmissionId, Uid, Fields, Assets) ->
    case moya_review_repo:upsert_draft_tx(Conn, SubmissionId, Fields) of
        {ok, #{<<"id">> := ReviewId} = Row} ->
            case moya_review_repo:validate_assets_tx(Conn, Uid, Assets) of
                {ok, _} ->
                    replace_review_assets_tx(Conn, ReviewId, Uid, Assets, Row);
                {error, not_found} ->
                    {rollback, not_found_asset};
                {error, _Reason} ->
                    {rollback, assets_invalid}
            end;
        {ok, _} ->
            {rollback, {db, no_returned_row}};
        {error, Reason} ->
            {rollback, {db, Reason}}
    end.

-spec replace_review_assets_tx(
    any(), integer(), integer(), [{integer(), binary(), integer()}], map()
) ->
    {ok, {map(), [map()]}} | {rollback, term()}.
replace_review_assets_tx(Conn, ReviewId, Uid, Assets, Row) ->
    case moya_review_repo:replace_assets_tx(Conn, ReviewId, Uid, Assets) of
        ok ->
            case moya_review_repo:assets_tx(Conn, ReviewId) of
                {ok, AssetRows} ->
                    {ok, {Row, AssetRows}};
                {error, Reason} ->
                    {rollback, {db, Reason}}
            end;
        {error, Reason} ->
            %% DB 兜底约束（全表唯一/单视频/3 图触发器）语义化为 assets_invalid
            _ = Reason,
            {rollback, assets_invalid}
    end.

%% @doc 发布回评（draft→published 一次性；重复请求返回已发布结果）
-spec publish(integer(), integer(), map()) ->
    {ok, map(), boolean()} | {error, atom()}.
publish(Uid, SubmissionId, Body) ->
    case reserved_keys(Body) of
        true ->
            {error, reserved_field};
        false ->
            case confirm_mismatch(Body, SubmissionId) of
                true ->
                    {error, confirm_mismatch};
                false ->
                    do_publish(Uid, SubmissionId)
            end
    end.

%% @doc 撤回（配方③ lock-first；仅 can_submit 监护人，无已发布回评）
-spec withdraw(integer(), integer()) -> {ok, withdrawn} | {error, atom()}.
withdraw(Uid, SubmissionId) ->
    case moya_acl:submission_access(Uid, SubmissionId) of
        {ok, Perspective, #{<<"learner_id">> := LearnerId, <<"org_id">> := OrgId}} when
            Perspective =:= guardian; Perspective =:= staff
        ->
            case moya_acl:resolve_guardian(Uid, LearnerId, submit) of
                {ok, _} ->
                    %% submission_access/2 读取时优先返回 staff；双身份用户仍需按
                    %% guardian 的 submit 权限撤回，并再次钉死 learner 与资源同机构。
                    case moya_context_repo:learner_org(LearnerId) of
                        {ok, OrgId} -> run_withdraw_tx(Uid, SubmissionId);
                        _ -> {error, not_guardian}
                    end;
                _ ->
                    %% 监护人无提交权 = 亦无撤回权（状态机 §1）
                    {error, not_guardian}
            end;
        {error, not_found} ->
            {error, not_found};
        {error, _} ->
            {error, not_guardian}
    end.

%% @doc 提交详情（视角感知：staff=超集含 AI 草稿；guardian=永不返 AI 字段 D-10）
-spec submission_detail(integer(), integer()) -> {ok, map()} | {error, atom()}.
submission_detail(Uid, SubmissionId) ->
    case moya_acl:submission_access(Uid, SubmissionId) of
        {ok, Perspective, Scope} ->
            build_detail(Uid, SubmissionId, Perspective, Scope);
        {error, not_found} ->
            {error, not_found};
        {error, _Reason} ->
            {error, forbidden}
    end.

%% @doc 学员历史（监护人 can_view_review 或同学机构班 staff；仅已发布回评）
-spec history(integer(), integer(), {integer(), integer()}) -> {ok, map()} | {error, atom()}.
history(Uid, LearnerId, {Page, Size}) ->
    case history_access(Uid, LearnerId) of
        ok ->
            case moya_submission_repo:history(LearnerId, Page, Size) of
                {ok, Rows, Total} ->
                    {ok, #{
                        <<"list">> => [history_item(R) || R <- Rows],
                        <<"page">> => Page,
                        <<"size">> => Size,
                        <<"total">> => Total
                    }};
                {error, Reason} ->
                    ?LOG_ERROR("history db error ~p", [Reason]),
                    {error, db_error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 家长未读点评数（moya 首页角标）：与 history/2 同权限同口径。
%% Since = 客户端自持水位（上次看到的 published_at，RFC3339）；缺省计全部。
%% 已读水位存客户端本地，服务端无状态（不建 seen 表）。
-spec history_unread_count(integer(), integer(), binary() | undefined) ->
    {ok, map()} | {error, atom()}.
history_unread_count(Uid, LearnerId, Since) ->
    case history_access(Uid, LearnerId) of
        ok ->
            case moya_submission_repo:history_unread_count(LearnerId, Since) of
                {ok, Count} ->
                    {ok, #{<<"count">> => Count}};
                {error, Reason} ->
                    ?LOG_ERROR("history unread count db error ~p", [Reason]),
                    {error, db_error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% ---- 队列 ----

%% repo 契约（moya_submission_repo:queue/4）：none(atom) → crd.id IS NULL；
%% binary → crd.status = $N；undefined → 不过滤。值白名单在 handler 层把关。
-spec ai_status_filter(term()) -> none | binary() | undefined.
ai_status_filter(<<"none">>) -> none;
ai_status_filter(undefined) -> undefined;
ai_status_filter(St) when is_binary(St) -> St;
ai_status_filter(_) -> undefined.

-spec queue_with_groups(integer(), [integer()], map(), integer(), integer()) ->
    {ok, map()} | {error, atom()}.
queue_with_groups(_Uid, GroupIds, Filters0, Page, Size) ->
    %% 线格式键（binary）在 logic 边界统一归一为 atom 键，repo 只读 atom 键；
    %% maps:without 显式丢弃 binary 原件——此前两套键空间并存，
    %% ai_status 因未归一面被 repo 静默忽略（老师队列过滤恒失效）。
    Filters = maps:filter(
        fun(_K, V) -> V =/= undefined end,
        (maps:without([<<"assignment_id">>, <<"ai_status">>], Filters0))#{
            %% CM-F3 修复补丁：tsid_opt 返回 {ok,Int}，此前整元组入 SQL 触发
            %% epgsql int8 integer_overflow 崩连接（真 HTTP code=1 根因）
            assignment_id =>
                case tsid_opt(maps:get(<<"assignment_id">>, Filters0, undefined)) of
                    {ok, Aid} -> Aid;
                    _ -> undefined
                end,
            ai_status => ai_status_filter(maps:get(<<"ai_status">>, Filters0, undefined))
        }
    ),
    case maps:get(<<"group_id">>, Filters0, undefined) of
        undefined ->
            run_queue(GroupIds, Filters, Page, Size);
        ClaimedGroup ->
            case tsid_opt(ClaimedGroup) of
                {ok, Gid} ->
                    case lists:member(Gid, GroupIds) of
                        true -> run_queue([Gid], Filters, Page, Size);
                        false -> {error, not_staff}
                    end;
                _ ->
                    {error, bad_param}
            end
    end.

-spec run_queue([integer()], map(), integer(), integer()) -> {ok, map()} | {error, atom()}.
run_queue(GroupIds, Filters, Page, Size) ->
    case moya_submission_repo:queue(GroupIds, Filters, Page, Size) of
        {ok, Rows, Total} ->
            {ok, #{
                <<"list">> => [queue_item(R) || R <- Rows],
                <<"page">> => Page,
                <<"size">> => Size,
                <<"total">> => Total
            }};
        {error, Reason} ->
            ?LOG_ERROR("queue db error ~p", [Reason]),
            {error, db_error}
    end.

%% ---- 工作台 / 详情 ----

-spec build_workbench(integer(), integer()) -> {ok, map()} | {error, atom()}.
build_workbench(Uid, SubmissionId) ->
    case load_submission_bundle(SubmissionId) of
        {ok, Bundle} ->
            %% DC-1：顶层 my_review_draft 复用 teacher_view 已算好的草稿
            %% （find_draft(Sid,Uid) + assets，不二次查库）——load_submission_bundle
            %% 构造的 Bundle 无 draft 键，直读恒 null（v1 引入缺陷，挡死老师
            %% 重开工作台的草稿恢复）。submission 内嵌值保持不动（兼容窗口）。
            TView = teacher_view(Uid, Bundle),
            {ok, #{
                <<"submission">> => TView,
                <<"my_review_draft">> => maps:get(<<"my_review_draft">>, TView, null),
                <<"learner_display_name">> => maps:get(<<"display_name">>, Bundle, <<>>),
                <<"assignment_title">> => maps:get(<<"title">>, Bundle, <<>>)
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec build_detail(integer(), integer(), staff | guardian, map()) ->
    {ok, map()} | {error, atom()}.
build_detail(_Uid, SubmissionId, Perspective, _Scope) ->
    case load_submission_bundle(SubmissionId) of
        {ok, Bundle} ->
            case Perspective of
                staff -> {ok, teacher_view(_Uid, Bundle)};
                guardian -> {ok, parent_view(Bundle)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% bundle: submission row + assets + learner/task names + ai draft + published review
%% （P0-4：+ published review 的 review_asset 集合，家长侧 DTO 数据源）
-spec load_submission_bundle(integer()) -> {ok, map()} | {error, atom()}.
load_submission_bundle(SubmissionId) ->
    case moya_context_repo:submission_scope(SubmissionId) of
        {ok, Scope} when is_map(Scope) ->
            case moya_submission_repo:find(SubmissionId) of
                {ok, undefined} ->
                    {error, not_found};
                {ok, Sub} ->
                    {ok, Assets} = moya_submission_repo:assets(SubmissionId),
                    {ok, AiDraft} = moya_review_repo:ai_draft(SubmissionId),
                    {ok, Published} = moya_review_repo:find_published(SubmissionId),
                    ReviewAssets =
                        case Published of
                            #{<<"id">> := PubId} ->
                                case moya_review_repo:assets(PubId) of
                                    {ok, Rows} -> Rows;
                                    _ -> []
                                end;
                            _ ->
                                []
                        end,
                    #{<<"learner_id">> := LearnerId} = Scope,
                    Bundle = #{
                        submission => Sub,
                        assets => Assets,
                        ai_draft => AiDraft,
                        published => Published,
                        review_assets => ReviewAssets,
                        scope => Scope,
                        display_name => learner_name(LearnerId),
                        reviewer_name => reviewer_display_name(Published),
                        title => task_title(maps:get(<<"task_id">>, Scope, <<>>))
                    },
                    {ok, Bundle};
                {error, Reason} ->
                    ?LOG_ERROR("bundle find db error ~p", [Reason]),
                    {error, db_error}
            end;
        {ok, undefined} ->
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("bundle scope db error ~p", [Reason]),
            {error, db_error}
    end.

-spec teacher_view(integer(), map()) -> map().
teacher_view(Uid, Bundle) ->
    Base = parent_view(Bundle),
    #{submission := Sub} = Bundle,
    Sid = maps:get(<<"id">>, Sub, 0),
    MyDraft =
        case moya_review_repo:find_draft(Sid, Uid) of
            {ok, D} when is_map(D) -> D;
            _ -> undefined
        end,
    DraftAssets =
        case MyDraft of
            #{<<"id">> := DraftId} ->
                case moya_review_repo:assets(DraftId) of
                    {ok, Rows} -> Rows;
                    _ -> []
                end;
            _ ->
                []
        end,
    Base#{
        <<"ai_draft">> => ai_draft_payload(maps:get(ai_draft, Bundle)),
        <<"my_review_draft">> => draft_ref(MyDraft, DraftAssets),
        <<"learner_display_name">> => maps:get(display_name, Bundle, <<>>)
    }.

-spec parent_view(map()) -> map().
parent_view(Bundle) ->
    #{submission := Sub} = Bundle,
    #{
        <<"submission_id">> => integer_to_binary(maps:get(<<"id">>, Sub, 0)),
        <<"assignment_id">> => integer_to_binary(maps:get(<<"assignment_id">>, Sub, 0)),
        <<"learner_id">> => integer_to_binary(maps:get(<<"learner_id">>, Sub, 0)),
        <<"attempt_no">> => maps:get(<<"attempt_no">>, Sub, 0),
        <<"status">> => maps:get(<<"status">>, Sub, <<"submitted">>),
        <<"submitted_at">> => elib_dt:rfc3339_or_null(maps:get(<<"submitted_at">>, Sub, null)),
        <<"withdrawn_at">> => elib_dt:rfc3339_or_null(maps:get(<<"withdrawn_at">>, Sub, null)),
        <<"assets">> => [asset_payload(A) || A <- maps:get(assets, Bundle, [])],
        %% 三态提示（processing/done/none）；家长 payload 永无 AI 草稿字段（D-10）
        <<"ai_status_hint">> => ai_hint(maps:get(ai_draft, Bundle)),
        %% FLOW-01 终点：家长只看已发布回评（published only；无则 null）。
        %% P0-4：published_review.assets 为回评媒体（家长侧仅 published 可见）
        <<"published_review">> =>
            published_review_payload(
                maps:get(published, Bundle),
                maps:get(review_assets, Bundle, []),
                maps:get(reviewer_name, Bundle, null)
            )
    }.

%% 家长可见的已发布回评子集（PublishedReview；不含 reviewer 内部字段与
%% 任何 AI 内部字段——map 字面量白名单构造，防御性剥离下游异常字段）
-spec published_review_payload(map() | undefined, [map()], binary() | null) -> map() | null.
published_review_payload(undefined, _Assets, _ReviewerName) ->
    null;
published_review_payload(Pub, Assets, ReviewerName) ->
    #{
        <<"review_id">> => integer_to_binary(maps:get(<<"id">>, Pub, 0)),
        <<"reviewer_display_name">> => ReviewerName,
        <<"positive_point">> => maps:get(<<"positive_point">>, Pub, <<>>),
        <<"focus_problem">> => maps:get(<<"focus_problem">>, Pub, <<>>),
        <<"practice_action">> => maps:get(<<"practice_action">>, Pub, <<>>),
        <<"comment">> => maps:get(<<"comment">>, Pub, <<>>),
        %% 逐字点评字卡（char_reviews 契约）：jsonb 读侧解码透传，null=不渲染字卡区
        <<"char_reviews">> => char_reviews_payload(maps:get(<<"char_reviews">>, Pub, null)),
        %% P0-4：回评媒体集合（TSID 一律 string）
        <<"assets">> => [review_asset_payload(A) || A <- Assets],
        %% 兼容字段只读派生：从 assets 第一条 feedback_video 派生，非写入真源
        <<"video_attachment_id">> => nullable_tsid(derive_video_id_rows(Assets)),
        <<"rework_required">> => maps:get(<<"rework_required">>, Pub, false) =:= true,
        <<"published_at">> => elib_dt:rfc3339_or_null(maps:get(<<"published_at">>, Pub, null))
    }.

%% ---- 草稿/发布 ----

-spec draft_guard(integer(), integer()) -> {ok, integer()} | {error, atom()}.
draft_guard(Uid, SubmissionId) ->
    case moya_acl:submission_access(Uid, SubmissionId) of
        {ok, staff, #{<<"group_id">> := GroupId}} ->
            %% 写权限：manager/teacher（assistant 5425）
            case moya_acl:resolve_staff(Uid, GroupId, write) of
                {ok, _} -> {ok, GroupId};
                {error, role_denied} -> {error, role_denied};
                _ -> {error, not_staff}
            end;
        _ ->
            {error, not_staff}
    end.

-spec draft_fields(
    integer(), map(), [{integer(), binary(), integer()}], [map()] | null
) -> map().
draft_fields(Uid, Body, Assets, CharReviews) ->
    #{
        uid => Uid,
        positive_point => text(maps:get(<<"positive_point">>, Body, <<>>)),
        focus_problem => text(maps:get(<<"focus_problem">>, Body, <<>>)),
        practice_action => text(maps:get(<<"practice_action">>, Body, <<>>)),
        comment => text(maps:get(<<"comment">>, Body, <<>>)),
        %% P0-4：旧列从 assets 第一条 feedback_video 派生冗余写（兼容读窗口），
        %% 不再从 Body 直读（写入真源 = review_asset 集合）
        video_attachment_id =>
            case derive_video_id(Assets) of
                undefined -> null;
                AttId -> AttId
            end,
        rework_required => maps:get(<<"rework_required">>, Body, false) =:= true,
        %% 逐字点评字卡（Phase A）：已过 parse_char_reviews 白名单；null=无逐字数据
        char_reviews => CharReviews
    }.

%% ---- P0-4：请求 assets 解析（结构/数量/重复/新旧字段一致性，快失败不落库） ----

%% 返回 [{AttId, Kind, SortOrder}]（Kind 为 binary 原文）
-spec parse_review_assets(map()) ->
    {ok, [{integer(), binary(), integer()}]} | {error, assets_invalid}.
parse_review_assets(Body) when is_map(Body) ->
    case maps:get(<<"assets">>, Body, undefined) of
        undefined ->
            %% 兼容：旧字段 video_attachment_id（无 assets 键时按单视频解析）
            case tsid_opt(maps:get(<<"video_attachment_id">>, Body, undefined)) of
                {ok, AttId} -> {ok, [{AttId, <<"feedback_video">>, 0}]};
                undefined -> {ok, []}
            end;
        Assets when is_list(Assets) ->
            parse_asset_items(Assets, Body, []);
        _ ->
            {error, assets_invalid}
    end;
parse_review_assets(_) ->
    {error, assets_invalid}.

-spec parse_asset_items(list(), map(), [{integer(), binary(), integer()}]) ->
    {ok, [{integer(), binary(), integer()}]} | {error, assets_invalid}.
parse_asset_items([], Body, Acc) ->
    Assets = lists:reverse(Acc),
    case assets_shape_ok(Assets) andalso legacy_video_consistent(Body, Assets) of
        true -> {ok, Assets};
        false -> {error, assets_invalid}
    end;
parse_asset_items([Item | Rest], Body, Acc) when is_map(Item) ->
    AttRaw = maps:get(<<"attachment_id">>, Item, undefined),
    Kind = maps:get(<<"kind">>, Item, undefined),
    case {tsid_opt(AttRaw), valid_kind(Kind), sort_order(Item)} of
        {{ok, AttId}, true, {ok, Order}} ->
            parse_asset_items(Rest, Body, [{AttId, Kind, Order} | Acc]);
        _ ->
            {error, assets_invalid}
    end;
parse_asset_items(_, _Body, _Acc) ->
    {error, assets_invalid}.

-spec valid_kind(term()) -> boolean().
valid_kind(<<"feedback_video">>) -> true;
valid_kind(<<"feedback_image">>) -> true;
valid_kind(_) -> false.

-spec sort_order(map()) -> {ok, integer()} | error.
sort_order(Item) ->
    case maps:get(<<"sort_order">>, Item, 0) of
        N when is_integer(N), N >= 0, N =< 999 -> {ok, N};
        _ -> error
    end.

%% 数量与重复：0-1 video + 0-3 image + attachment_id 不得重复
-spec assets_shape_ok([{integer(), binary(), integer()}]) -> boolean().
assets_shape_ok(Assets) ->
    Ids = [AttId || {AttId, _, _} <- Assets],
    Videos = [A || {_, <<"feedback_video">>, _} = A <- Assets],
    Images = [A || {_, <<"feedback_image">>, _} = A <- Assets],
    length(Ids) =:= length(lists:usort(Ids)) andalso
        length(Videos) =< 1 andalso
        length(Images) =< 3.

%% 新旧字段双写一致性：Body 同时携带 video_attachment_id 与 assets 时，
%% 旧字段必须与 assets 派生视频一致（含"均无视频"），否则拒绝（防双写歧义）
-spec legacy_video_consistent(map(), [{integer(), binary(), integer()}]) -> boolean().
legacy_video_consistent(Body, Assets) ->
    case maps:get(<<"video_attachment_id">>, Body, undefined) of
        undefined ->
            true;
        Legacy ->
            tsid_opt(Legacy) =:=
                case derive_video_id(Assets) of
                    undefined -> undefined;
                    AttId -> {ok, AttId}
                end
    end.

%% 第一条 feedback_video 的 attachment_id（无则 undefined）
-spec derive_video_id([{integer(), binary(), integer()}]) -> integer() | undefined.
derive_video_id(Assets) ->
    case [AttId || {AttId, <<"feedback_video">>, _} <- Assets] of
        [First | _] -> First;
        [] -> undefined
    end.

%% DB 行版本（assets_tx 返回行；video_attachment_id 兼容字段派生源）
-spec derive_video_id_rows([map()]) -> integer() | undefined.
derive_video_id_rows(AssetRows) ->
    case
        [AttId || #{<<"kind">> := <<"feedback_video">>, <<"attachment_id">> := AttId} <- AssetRows]
    of
        [First | _] -> First;
        [] -> undefined
    end.

-spec do_publish(integer(), integer()) -> {ok, map(), boolean()} | {error, atom()}.
do_publish(Uid, SubmissionId) ->
    case draft_guard(Uid, SubmissionId) of
        {ok, _GroupId} ->
            case publish_precheck(Uid, SubmissionId) of
                ok ->
                    run_publish_tx(Uid, SubmissionId);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 发布前置：草稿存在且至少一种有效反馈内容（5480/5485）
-spec publish_precheck(integer(), integer()) -> ok | {error, atom()}.
publish_precheck(Uid, SubmissionId) ->
    case moya_review_repo:find_published(SubmissionId) of
        {ok, Pub} when is_map(Pub) ->
            %% 已发布：直接走幂等重放路径
            ok;
        _ ->
            case moya_review_repo:find_draft(SubmissionId, Uid) of
                {ok, Draft} when is_map(Draft) ->
                    %% v3 P1-3 修复：feedback_image 也计有效内容（与前端
                    %% canPublish assets>0 口径对齐；纯图片回评可发布）
                    Assets =
                        case moya_review_repo:assets(maps:get(<<"id">>, Draft, 0)) of
                            {ok, Rows} -> Rows;
                            _ -> []
                        end,
                    case review_has_content(Draft, Assets) of
                        true -> ok;
                        false -> {error, empty_content}
                    end;
                _ ->
                    {error, no_draft}
            end
    end.

%% v3 P1-3 修复：签名加 Assets（review_asset 行集）；
%% 有效内容 = 文本 ∨ 视频（旧列兼容）∨ ≥1 张反馈图片
-spec review_has_content(map(), [map()]) -> boolean().
review_has_content(Draft, Assets) ->
    Fields =
        [
            maps:get(<<"positive_point">>, Draft, <<>>),
            maps:get(<<"focus_problem">>, Draft, <<>>),
            maps:get(<<"practice_action">>, Draft, <<>>),
            maps:get(<<"comment">>, Draft, <<>>)
        ],
    HasText = lists:any(fun(F) -> is_binary(F) andalso F =/= <<>> end, Fields),
    HasVideo = maps:get(<<"video_attachment_id">>, Draft, null) =/= null,
    HasImage =
        lists:any(
            fun(A) -> maps:get(<<"kind">>, A, <<>>) =:= <<"feedback_image">> end,
            Assets
        ),
    HasText orelse HasVideo orelse HasImage.

%% 配方③：lock-first 发布事务（P0-4：发布/幂等重放均回读 assets 进 DTO）
-spec run_publish_tx(integer(), integer()) -> {ok, map(), boolean()} | {error, atom()}.
run_publish_tx(Uid, SubmissionId) ->
    Tx = fun(Conn) ->
        case moya_submission_repo:lock_submission_tx(Conn, SubmissionId) of
            {ok, Row} when is_map(Row) ->
                case moya_review_repo:publish_tx(Conn, SubmissionId, Uid) of
                    {ok, Tag, Review} ->
                        ReviewId = maps:get(<<"id">>, Review, 0),
                        case moya_review_repo:assets_tx(Conn, ReviewId) of
                            {ok, AssetRows} ->
                                {ok, {Tag, Review, AssetRows}};
                            {error, Reason} ->
                                {rollback, {db, Reason}}
                        end;
                    {error, Reason} ->
                        %% repo 侧 _tx 守卫失败返回裸 {error, Atom}（不触发 rollback）
                        {error, Reason}
                end;
            {ok, undefined} ->
                {rollback, not_found};
            {error, Reason} ->
                {rollback, {db, Reason}}
        end
    end,
    case elib_pg:with_tx(Tx, [{reraise, false}]) of
        {ok, {published, Review, AssetRows}} ->
            {ok, review_payload(Review, AssetRows), false};
        {ok, {already_published, Review, AssetRows}} ->
            {ok, review_payload(Review, AssetRows), true};
        {rollback, not_found} ->
            {error, not_found};
        {rollback, withdrawn} ->
            {error, withdrawn};
        {rollback, no_draft} ->
            {error, no_draft};
        %% repo 侧 _tx 守卫失败返回裸 {error, Atom}（不触发 rollback）——
        %% 须原样透传，否则 5482 等语义码被折叠成 db_error
        %% （R8 契约实测：有草稿 + 已撤回的 publish 曾返回 code=1）
        {error, Reason} when is_atom(Reason) ->
            {error, Reason};
        _ ->
            {error, db_error}
    end.

-spec run_withdraw_tx(integer(), integer()) -> {ok, withdrawn} | {error, atom()}.
run_withdraw_tx(Uid, SubmissionId) ->
    Tx = fun(Conn) ->
        moya_submission_repo:withdraw_tx(Conn, SubmissionId, Uid)
    end,
    case elib_pg:with_tx(Tx, [{reraise, false}]) of
        {ok, withdrawn} ->
            {ok, withdrawn};
        {rollback, Reason} when Reason =/= withdrawn ->
            {error, withdraw_reason(Reason)};
        {error, Reason} when is_atom(Reason) ->
            {error, withdraw_reason(Reason)};
        _ ->
            {error, db_error}
    end.

-spec withdraw_reason(term()) -> atom().
withdraw_reason(already_reviewed) -> already_reviewed;
withdraw_reason(not_submitted) -> already_withdrawn;
withdraw_reason(not_found) -> not_found;
withdraw_reason(_) -> db_error.

%% ---- 历史访问 ----

-spec history_access(integer(), integer()) -> ok | {error, atom()}.
history_access(Uid, LearnerId) ->
    GuardianOk =
        case moya_acl:resolve_guardian(Uid, LearnerId, view_review) of
            {ok, _} -> true;
            _ -> false
        end,
    case GuardianOk of
        true ->
            ok;
        false ->
            %% 账号本人（Step 16：管理侧绑定后 learner.user_id == Uid，
            %% 本人可查自己历史；解绑置 NULL 即立即失效，无需额外清理）
            case self_bound(Uid, LearnerId) of
                true ->
                    ok;
                false ->
                    %% staff 路径：学员所在班级（同机构）的任课老师
                    case learner_group_ids(LearnerId) of
                        {ok, []} ->
                            {error, not_guardian};
                        {ok, GroupIds} ->
                            staff_in_any(Uid, GroupIds);
                        {error, _} ->
                            {error, db_error}
                    end
            end
    end.

%% 学员档案与账号本人绑定（active）判定——history_access 第三分支依据
-spec self_bound(integer(), integer()) -> boolean().
self_bound(Uid, LearnerId) ->
    Sql =
        <<"SELECT id FROM ", (elib_pg_sql:public_tablename(<<"learner">>))/binary,
            " WHERE id = $1 AND user_id = $2 AND status = 'active' LIMIT 1">>,
    case elib_pg:query(Sql, [LearnerId, Uid]) of
        {ok, [_ | _]} -> true;
        _ -> false
    end.

-spec learner_group_ids(integer()) -> {ok, [integer()]} | {error, term()}.
learner_group_ids(LearnerId) ->
    Sql =
        <<"SELECT group_id FROM ", (elib_pg_sql:public_tablename(<<"class_enrollment">>))/binary,
            " WHERE learner_id = $1 AND status = 'active'">>,
    case elib_pg:query(Sql, [LearnerId]) of
        {ok, Rows} -> {ok, [maps:get(<<"group_id">>, R) || R <- Rows]};
        {error, Reason} -> {error, Reason}
    end.

-spec staff_in_any(integer(), [integer()]) -> ok | {error, atom()}.
staff_in_any(Uid, GroupIds) ->
    ListsAny =
        lists:any(
            fun(Gid) ->
                case moya_acl:resolve_staff(Uid, Gid) of
                    {ok, _} -> true;
                    _ -> false
                end
            end,
            GroupIds
        ),
    case ListsAny of
        true -> ok;
        false -> {error, not_guardian}
    end.

%% ---- payload ----

-spec queue_item(map()) -> map().
queue_item(R) ->
    #{
        <<"submission_id">> => integer_to_binary(maps:get(<<"submission_id">>, R, 0)),
        <<"assignment_id">> => integer_to_binary(maps:get(<<"assignment_id">>, R, 0)),
        <<"task_title">> => maps:get(<<"task_title">>, R, <<>>),
        <<"group_id">> => integer_to_binary(maps:get(<<"group_id">>, R, 0)),
        <<"group_name">> => maps:get(<<"group_title">>, R, <<>>),
        <<"learner_id">> => integer_to_binary(maps:get(<<"learner_id">>, R, 0)),
        <<"learner_display_name">> => maps:get(<<"display_name">>, R, <<>>),
        <<"submitted_at">> => elib_dt:rfc3339_or_null(maps:get(<<"submitted_at">>, R, null)),
        <<"attempt_no">> => maps:get(<<"attempt_no">>, R, 0),
        <<"ai_status">> => ai_status(maps:get(<<"ai_status">>, R, null)),
        <<"has_published_review">> => maps:get(<<"has_published">>, R, false) =:= true
    }.

-spec history_item(map()) -> map().
history_item(R) ->
    #{
        <<"submission_id">> => integer_to_binary(maps:get(<<"submission_id">>, R, 0)),
        <<"assignment_id">> => integer_to_binary(maps:get(<<"assignment_id">>, R, 0)),
        <<"task_title">> => maps:get(<<"task_title">>, R, <<>>),
        <<"group_id">> => integer_to_binary(maps:get(<<"group_id">>, R, 0)),
        <<"group_name">> => maps:get(<<"group_title">>, R, <<>>),
        <<"workspace_id">> => integer_to_binary(maps:get(<<"workspace_id">>, R, 0)),
        <<"attempt_no">> => maps:get(<<"attempt_no">>, R, 0),
        <<"submitted_at">> => elib_dt:rfc3339_or_null(maps:get(<<"submitted_at">>, R, null)),
        <<"status">> => maps:get(<<"status">>, R, <<"submitted">>),
        <<"published_review_id">> =>
            nullable_tsid(maps:get(<<"published_review_id">>, R, null)),
        %% 契约 LearnerHistoryPage.list[].published_review（nullable，
        %% /components/schemas/PublishedReview）：history SQL 已 LEFT JOIN
        %% teacher_review 取回各回评字段，直接行内构造（无 assets 集合）
        <<"published_review">> => history_published_review(R)
    }.

%% history 行的已发布回评子集（无 published_review_id = 无已发布回评 → null）
-spec history_published_review(map()) -> map() | null.
history_published_review(R) ->
    case maps:get(<<"published_review_id">>, R, null) of
        null ->
            null;
        Id when is_integer(Id) ->
            #{
                <<"review_id">> => integer_to_binary(Id),
                %% CM-F2：老师署名（history SQL 行内 LEFT JOIN user 解析；与
                %% submission_detail 的 PublishedReview 同字段，DTO 一致）。
                %% 查无/空昵称 → null，前端隐藏署名位
                <<"reviewer_display_name">> =>
                    nullable_bin(maps:get(<<"reviewer_display_name">>, R, null)),
                <<"positive_point">> => maps:get(<<"positive_point">>, R, <<>>),
                <<"focus_problem">> => maps:get(<<"focus_problem">>, R, <<>>),
                <<"practice_action">> => maps:get(<<"practice_action">>, R, <<>>),
                <<"comment">> => maps:get(<<"comment">>, R, <<>>),
                <<"char_reviews">> =>
                    char_reviews_payload(maps:get(<<"char_reviews">>, R, null)),
                <<"video_attachment_id">> =>
                    nullable_tsid(maps:get(<<"video_attachment_id">>, R, null)),
                <<"rework_required">> => maps:get(<<"rework_required">>, R, false) =:= true,
                <<"published_at">> => elib_dt:rfc3339_or_null(maps:get(<<"published_at">>, R, null))
            };
        _ ->
            null
    end.

%% P0-4：review_payload 带 assets 集合（save_draft/publish/draft_ref 调用）。
%% video_attachment_id 从 assets 派生（只读兼容字段）；map 字面量白名单构造。
-spec review_payload(map(), [map()]) -> map().
review_payload(R, Assets) ->
    #{
        <<"review_id">> => integer_to_binary(maps:get(<<"id">>, R, 0)),
        <<"submission_id">> => integer_to_binary(maps:get(<<"submission_id">>, R, 0)),
        <<"reviewer_uid">> => integer_to_binary(maps:get(<<"reviewer_uid">>, R, 0)),
        <<"positive_point">> => maps:get(<<"positive_point">>, R, <<>>),
        <<"focus_problem">> => maps:get(<<"focus_problem">>, R, <<>>),
        <<"practice_action">> => maps:get(<<"practice_action">>, R, <<>>),
        <<"comment">> => maps:get(<<"comment">>, R, <<>>),
        <<"char_reviews">> => char_reviews_payload(maps:get(<<"char_reviews">>, R, null)),
        <<"assets">> => [review_asset_payload(A) || A <- Assets],
        <<"video_attachment_id">> => nullable_tsid(derive_video_id_rows(Assets)),
        <<"rework_required">> => maps:get(<<"rework_required">>, R, false) =:= true,
        <<"status">> => maps:get(<<"status">>, R, <<"draft">>),
        <<"published_at">> => elib_dt:rfc3339_or_null(maps:get(<<"published_at">>, R, null))
    }.

%% P0-4：回评媒体 DTO（attachment_id TSID 一律 string）；
%% v3 P1-1 修复：补 object_key（经 view_url 授权展示的唯一句柄，与
%% asset_payload 同口径——缺失即家长侧回评媒体无法展示）
-spec review_asset_payload(map()) -> map().
review_asset_payload(A) ->
    #{
        <<"attachment_id">> => integer_to_binary(maps:get(<<"attachment_id">>, A, 0)),
        <<"object_key">> => maps:get(<<"object_key">>, A, <<>>),
        <<"kind">> => maps:get(<<"kind">>, A, <<>>),
        <<"sort_order">> => maps:get(<<"sort_order">>, A, 0)
    }.

-spec asset_payload(map()) -> map().
asset_payload(A) ->
    #{
        <<"attachment_id">> => integer_to_binary(maps:get(<<"attachment_id">>, A, 0)),
        <<"object_key">> => maps:get(<<"path">>, A, <<>>),
        <<"kind">> => maps:get(<<"kind">>, A, <<>>),
        <<"sort_order">> => maps:get(<<"sort_order">>, A, 0)
    }.

-spec ai_draft_payload(map() | undefined) -> map() | null.
ai_draft_payload(undefined) ->
    null;
ai_draft_payload(D) ->
    #{
        <<"draft_id">> => integer_to_binary(maps:get(<<"id">>, D, 0)),
        <<"submission_id">> => integer_to_binary(maps:get(<<"submission_id">>, D, 0)),
        <<"status">> => maps:get(<<"status">>, D, <<"queued">>),
        <<"error_code">> => maps:get(<<"error_code">>, D, null),
        <<"model_profile">> => maps:get(<<"model_profile">>, D, <<>>),
        <<"prompt_version">> => maps:get(<<"prompt_version">>, D, <<>>),
        <<"rubric_version">> => maps:get(<<"rubric_version">>, D, <<>>),
        <<"result">> => maps:get(<<"result_json">>, D, null),
        <<"created_at">> => elib_dt:rfc3339_or_null(maps:get(<<"created_at">>, D, null)),
        <<"completed_at">> => elib_dt:rfc3339_or_null(maps:get(<<"completed_at">>, D, null))
    }.

-spec draft_ref(map() | undefined, [map()]) -> map() | null.
draft_ref(undefined, _Assets) ->
    null;
draft_ref(D, Assets) ->
    review_payload(D, Assets).

-spec ai_status(null | binary()) -> binary().
ai_status(null) -> <<"none">>;
ai_status(St) when is_binary(St) -> St.

-spec ai_hint(map() | undefined) -> binary().
ai_hint(#{<<"status">> := <<"queued">>}) -> <<"processing">>;
ai_hint(#{<<"status">> := <<"running">>}) -> <<"processing">>;
ai_hint(#{<<"status">> := _}) -> <<"done">>;
ai_hint(_) -> <<"none">>.

-spec learner_name(integer()) -> binary().
learner_name(LearnerId) ->
    Sql =
        <<"SELECT display_name FROM ", (elib_pg_sql:public_tablename(<<"learner">>))/binary,
            " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [LearnerId]) of
        {ok, [#{<<"display_name">> := Name} | _]} -> Name;
        _ -> <<>>
    end.

%% ---- 逐字点评字卡（char_reviews，Phase A：docs/plans/2026-09-12-char-review-dto-proposal.md）----

%% 请求体解析：无键 → null（覆盖式 upsert 与其他字段同语义：PUT 不带即清空）；
%% 非 list → {error, char_reviews_invalid}（整体结构错误=调用方 bug，整体拒绝）；
%% list → 单项白名单校验（index≥0 整数、char 非空≤8字节、grade 枚举、comment≤300
%% 字节；必填缺失或越界 → 该项丢弃不整体拒绝，AI 输出容错）；
%% 空数组/全部丢弃 → 归一化 null；超过 50 项按输入顺序截取前 50。
-spec parse_char_reviews(map()) -> {ok, [map()] | null} | {error, char_reviews_invalid}.
parse_char_reviews(Body) when is_map(Body) ->
    case maps:get(<<"char_reviews">>, Body, null) of
        null ->
            {ok, null};
        Items when is_list(Items) ->
            {ok, normalize_char_reviews(Items)};
        _ ->
            {error, char_reviews_invalid}
    end;
parse_char_reviews(_) ->
    {error, char_reviews_invalid}.

-spec normalize_char_reviews([term()]) -> [map()] | null.
normalize_char_reviews(Items) ->
    Kept = lists:sublist(lists:filtermap(fun char_review_item/1, Items), ?CHAR_REVIEWS_MAX),
    case Kept of
        [] -> null;
        _ -> Kept
    end.

%% 单项白名单（fail-per-item）+ 防御性剥离：只保留契约四字段
-spec char_review_item(term()) -> {true, map()} | false.
char_review_item(Item) when is_map(Item) ->
    Index = maps:get(<<"index">>, Item, undefined),
    Char = maps:get(<<"char">>, Item, undefined),
    Grade = maps:get(<<"grade">>, Item, undefined),
    Comment = maps:get(<<"comment">>, Item, <<>>),
    IsValid =
        is_integer(Index) andalso Index >= 0 andalso
            is_binary(Char) andalso Char =/= <<>> andalso byte_size(Char) =< 8 andalso
            is_binary(Grade) andalso lists:member(Grade, ?CHAR_GRADES) andalso
            is_binary(Comment) andalso byte_size(Comment) =< 300,
    case IsValid of
        true ->
            {true, #{
                <<"index">> => Index,
                <<"char">> => Char,
                <<"grade">> => Grade,
                <<"comment">> => Comment
            }};
        false ->
            false
    end;
char_review_item(_) ->
    false.

%% char_reviews jsonb 读侧：elib_pg 连接未配 json codec，jsonb 读回为 JSON
%% 文本（moderation_action_logic 同款兜底解码）；写入侧已白名单，解码后透传。
%% 连接若未来配上 json codec（读回 Erlang 项）list 分支直通。
-spec char_reviews_payload(term()) -> [map()] | null.
char_reviews_payload(null) ->
    null;
char_reviews_payload(undefined) ->
    null;
char_reviews_payload(Bin) when is_binary(Bin) ->
    try jsone:decode(Bin, [{object_format, map}]) of
        List when is_list(List) -> List;
        _ -> null
    catch
        _:_ -> null
    end;
char_reviews_payload(List) when is_list(List) ->
    List;
char_reviews_payload(_) ->
    null.

%% @doc 回评老师展示名（家长点评卡署名位）：reviewer_uid → user.nickname。
%% 查无（账号注销等）返回 null，前端隐藏署名位而非显示空文本。
-spec reviewer_display_name(map() | undefined) -> binary() | null.
reviewer_display_name(#{<<"reviewer_uid">> := Uid}) when is_integer(Uid), Uid > 0 ->
    case user_repo:find_by_uid(Uid) of
        {ok, #{<<"nickname">> := Name}} when is_binary(Name), Name =/= <<>> -> Name;
        _ -> null
    end;
reviewer_display_name(_) ->
    null.

-spec task_title(binary()) -> binary().
task_title(TaskId) ->
    Sql =
        <<"SELECT title FROM ", (elib_pg_sql:public_tablename(<<"group_task">>))/binary,
            " WHERE task_id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [TaskId]) of
        {ok, [#{<<"title">> := Title} | _]} -> Title;
        _ -> <<>>
    end.

-spec reserved_keys(map()) -> boolean().
reserved_keys(Body) when is_map(Body) ->
    lists:any(fun(K) -> maps:is_key(K, Body) end, ?RESERVED_BODY_KEYS).

-spec confirm_mismatch(map(), integer()) -> boolean().
confirm_mismatch(Body, SubmissionId) ->
    case maps:get(<<"confirm_learner_id">>, Body, undefined) of
        undefined ->
            false;
        Claimed ->
            case moya_context_repo:submission_scope(SubmissionId) of
                {ok, #{<<"learner_id">> := L}} ->
                    tsid_opt(Claimed) =/= {ok, L};
                _ ->
                    true
            end
    end.

-spec text(term()) -> binary().
text(B) when is_binary(B) -> B;
text(_) -> <<>>.

-spec tsid_opt(term()) -> {ok, integer()} | undefined.
tsid_opt(undefined) ->
    undefined;
tsid_opt(Bin) when is_binary(Bin) ->
    try binary_to_integer(Bin) of
        Int when Int > 0 -> {ok, Int};
        _ -> undefined
    catch
        _:_ -> undefined
    end;
tsid_opt(_) ->
    undefined.

-spec nullable_tsid(integer() | null | undefined) -> binary() | null.
nullable_tsid(Id) when is_integer(Id) -> integer_to_binary(Id);
nullable_tsid(_) -> null.

%% 二进制字段归一：非空 binary 透传；null/undefined/其他 → null（不编造）
-spec nullable_bin(binary() | null | undefined) -> binary() | null.
nullable_bin(V) when is_binary(V), V =/= <<>> -> V;
nullable_bin(_) -> null.

-spec empty_page(integer(), integer()) -> map().
empty_page(Page, Size) ->
    #{<<"list">> => [], <<"page">> => Page, <<"size">> => Size, <<"total">> => 0}.
