-module(moya_assignment_logic).
%%%
% 墨芽家长侧作业/提交业务逻辑（Step 9）
% Parent-side assignments & submissions logic
%
% 守卫链（全部服务端解析，客户端自报字段一律忽略）：
%   列表：resolve_guardian(Uid, LearnerId)（active）
%   提交：assignment_scope → learner_id 匹配 + resolve_guardian(_, _, submit)
%         + 附件归属 + task 开放（status=1 进行中且未过 deadline，A1-D01）
%%%

%% list/4 = CM-F4 status 过滤（真 HTTP undef 根因：函数在而导出漏加）
-export([list/3, list/4, detail/2, create_submission/4]).

-include_lib("kernel/include/logger.hrl").
%% 纯 payload 组装函数；导出用于 eunit 直接验收契约字段（无 -ifdef(TEST)——
%% 干净 make compile 的 ebin 同样携带导出，避免测试口径与生产 beam 分叉，
%% 改法同 09484ffd 之于 msg_store_repo）
-export([submission_created/6, assignment_summary/1]).
%% 附件请求形状解析（纯函数）；导出用于 eunit 直测去重/数量规则
-export([normalize_assets/1]).

-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 家长作业列表（按 learner；CM-F4 四态 status 过滤透传）
%% {ok, PagePayload} | {error, Reason}
%% Reason：missing_learner_id(422) not_guardian(5422) db_error
-spec list(integer(), integer(), {integer(), integer()}) ->
    {ok, map()} | {error, atom()}.
list(Uid, LearnerId, {Page, Size}) ->
    list(Uid, LearnerId, {Page, Size}, undefined).

%% @doc StatusOpt = undefined | pending | submitted | reviewing | reviewed
%% （白名单在 handler 校验；repo status_condition 与 derived_status 同口径）
-spec list(integer(), integer(), {integer(), integer()}, binary() | undefined) ->
    {ok, map()} | {error, atom()}.
list(Uid, LearnerId, {Page, Size}, StatusOpt) ->
    case moya_acl:resolve_guardian(Uid, LearnerId) of
        {ok, _} ->
            case
                moya_submission_repo:assignments_for_learner(
                    LearnerId, Page, Size, StatusOpt
                )
            of
                {ok, Rows, Total} ->
                    {ok, #{
                        <<"list">> => [assignment_summary(R) || R <- Rows],
                        <<"page">> => Page,
                        <<"size">> => Size,
                        <<"total">> => Total
                    }};
                {error, Reason} ->
                    ?LOG_ERROR("assignment list db error ~p", [Reason]),
                    {error, db_error}
            end;
        _ ->
            %% 显式拒绝而非空列表（T3：防 learner 枚举）
            {error, not_guardian_list}
    end.

%% @doc 作业详情（监护人或本班 staff；CM-F2 富 payload：title/description/
%% deadline/真实推导状态——与列表 DTO 同形，moya AssignmentDetail extends
%% AssignmentSummary）。ACL 门仍走 assignment_scope（learner_id/group_id 真源），
%% 富行从 submission_repo 单行查询取。
%% Reason：not_found(5440) forbidden(403) db_error
-spec detail(integer(), integer()) -> {ok, map()} | {error, atom()}.
detail(Uid, AssignmentId) ->
    case moya_context_repo:assignment_scope(AssignmentId) of
        {ok, #{<<"learner_id">> := LearnerId, <<"group_id">> := GroupId}} when
            is_integer(LearnerId), is_integer(GroupId)
        ->
            case check_assignment_access(Uid, LearnerId, GroupId) of
                ok ->
                    detail_payload(AssignmentId);
                staff ->
                    detail_payload(AssignmentId);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, _} ->
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("assignment scope db error ~p", [Reason]),
            {error, db_error}
    end.

%% @doc 幂等创建提交（STEP-08-DB 配方①②）
%% {ok, SubmissionCreated} | {error, Reason}
-spec create_submission(integer(), integer(), binary(), map()) ->
    {ok, map()} | {error, atom()}.
create_submission(Uid, AssignmentId, IdemKey, Body) ->
    case precheck_create(Uid, AssignmentId, Body) of
        {ok, #{scope := Scope, assets := Assets, learner_id := LearnerId}} ->
            Digest = request_digest(LearnerId, Assets, maps:get(<<"note">>, Body, <<>>)),
            run_create_tx(Uid, AssignmentId, IdemKey, Digest, LearnerId, Assets, Scope);
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_assignment_access(integer(), integer(), integer()) ->
    ok | staff | {error, forbidden | db_error}.
check_assignment_access(Uid, LearnerId, GroupId) ->
    case moya_acl:resolve_guardian(Uid, LearnerId) of
        {ok, _} ->
            ok;
        _ ->
            case moya_acl:resolve_staff(Uid, GroupId) of
                {ok, _} -> staff;
                _ -> {error, forbidden}
            end
    end.

%% ---- 创建前置校验 ----

-spec precheck_create(integer(), integer(), map()) ->
    {ok, map()} | {error, atom()}.
precheck_create(Uid, AssignmentId, Body) ->
    case moya_context_repo:assignment_scope(AssignmentId) of
        {ok, #{<<"learner_id">> := LearnerId, <<"task_status">> := TaskStatus} = Scope} when
            is_integer(LearnerId)
        ->
            check_create_guards(Uid, LearnerId, TaskStatus, Scope, Body);
        {ok, _} ->
            %% 普通群作业（learner_id NULL）或链路残缺：对教学 API 不可见
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("assignment scope db error ~p", [Reason]),
            {error, db_error}
    end.

-spec check_create_guards(integer(), integer(), integer(), map(), map()) ->
    {ok, map()} | {error, atom()}.
check_create_guards(Uid, LearnerId, TaskStatus, Scope, Body) ->
    ClaimedLearner = elib_tsid:from_binary(maps:get(<<"learner_id">>, Body, <<>>)),
    BooleanGuards =
        [
            {claimed_learner_mismatch, ClaimedLearner =:= {ok, LearnerId}},
            {not_guardian, guard_can_submit(Uid, LearnerId)},
            %% A1-D01：开放 = status=1 且 deadline 未过（assignment_scope 注释与
            %% group_task 先例的完整语义；此前只查 status，截止后仍可提交）
            {assignment_closed, TaskStatus =:= 1 andalso not deadline_passed(Scope)}
        ],
    case lists:keyfind(false, 2, BooleanGuards) of
        {Reason, false} ->
            {error, Reason};
        false ->
            case normalize_assets(maps:get(<<"assets">>, Body, [])) of
                {ok, Assets} ->
                    case moya_submission_repo:validate_assets(Uid, Assets) of
                        {ok, _} ->
                            {ok, #{scope => Scope, assets => Assets, learner_id => LearnerId}};
                        _ ->
                            {error, assets_invalid}
                    end;
                {error, invalid} ->
                    {error, assets_invalid}
            end
    end.

-spec guard_can_submit(integer(), integer()) -> boolean().
guard_can_submit(Uid, LearnerId) ->
    case moya_acl:resolve_guardian(Uid, LearnerId, submit) of
        {ok, _} -> true;
        _ -> false
    end.

%% A1-D01：截止判定，语义照抄 group_task_logic:check_deadline/1 先例——
%% Now > Deadline 严格大于（恰等于 deadline 时刻未过期，仍可提交）；
%% 无 deadline（null/缺失）或不可解析 → 不视为过期。
-spec deadline_passed(map()) -> boolean().
deadline_passed(Scope) ->
    case maps:get(<<"task_deadline">>, Scope, undefined) of
        Deadline when is_binary(Deadline), Deadline =/= <<>> ->
            try
                elib_dt:millisecond() > elib_dt:rfc3339_to(Deadline)
            catch
                _:_ -> false
            end;
        _ ->
            false
    end.

%% 附件规则：恰好 1 个 practice_video，0..3 个 final_photo（契约 minItems/maxItems）。
%% W2-A2-HARDEN 补去重（对齐 review 侧口径）：attachment_id 不得重复；
%% 同 kind + 同 sort_order 不得重复（review_asset 触发器同语义；moya
%% submit-flow 实际发送 video=0 / photos 按序 0..n-1，真实客户端不受影响）。
-spec normalize_assets(term()) -> {ok, [{integer(), binary(), integer()}]} | {error, invalid}.
normalize_assets(Assets) when is_list(Assets), length(Assets) >= 1, length(Assets) =< 4 ->
    Parsed = [parse_asset(A) || A <- Assets],
    case lists:member(invalid, Parsed) of
        true ->
            {error, invalid};
        false ->
            Videos = [P || {_, <<"practice_video">>, _} = P <- Parsed],
            Photos = [P || {_, <<"final_photo">>, _} = P <- Parsed],
            Ids = [AttId || {AttId, _, _} <- Parsed],
            KindOrders = [{K, O} || {_, K, O} <- Parsed],
            case
                {
                    length(Videos),
                    length(Photos),
                    length(Ids) =:= length(lists:usort(Ids)),
                    length(KindOrders) =:= length(lists:usort(KindOrders))
                }
            of
                {1, N, true, true} when N =< 3 ->
                    {ok, lists:sort(Parsed)};
                _ ->
                    {error, invalid}
            end
    end;
normalize_assets(_) ->
    {error, invalid}.

-spec parse_asset(map()) -> {integer(), binary(), integer()} | invalid.
parse_asset(#{<<"attachment_id">> := AttId0, <<"kind">> := Kind} = A) ->
    case {elib_tsid:from_binary(AttId0), Kind} of
        {{ok, AttId}, K} when K =:= <<"practice_video">>; K =:= <<"final_photo">> ->
            {AttId, K, order_int(maps:get(<<"sort_order">>, A, 0))};
        _ ->
            invalid
    end;
parse_asset(_) ->
    invalid.

-spec order_int(term()) -> integer().
order_int(N) when is_integer(N), N >= 0, N =< 9 -> N;
order_int(_) -> 0.

%% ---- 事务体（配方①②） ----

-spec run_create_tx(integer(), integer(), binary(), binary(), integer(), list(), map()) ->
    {ok, map()} | {error, atom()}.
run_create_tx(Uid, AssignmentId, IdemKey, Digest, LearnerId, Assets, _Scope) ->
    Tx = fun(Conn) ->
        case moya_submission_repo:lock_assignment_tx(Conn, AssignmentId) of
            {ok, AssignmentId} ->
                create_in_tx(Conn, Uid, AssignmentId, IdemKey, Digest, LearnerId, Assets);
            {ok, undefined} ->
                {rollback, not_found};
            {error, Reason} ->
                {rollback, {db, Reason}}
        end
    end,
    case elib_pg:with_tx(Tx, [{reraise, false}]) of
        {ok, _} = Ok ->
            Ok;
        {rollback, not_found} ->
            {error, not_found};
        {rollback, idempotency_conflict} ->
            %% 5460（T8b 同 key 不同 body）：业务错误原子透传，非 db_error
            %% （R8 契约实测：原 case 漏此分支 → case_clause 崩溃 → HTTP 500）
            {error, idempotency_conflict};
        {rollback, {db, _}} ->
            {error, db_error};
        {error, _} ->
            {error, db_error}
    end.

-spec create_in_tx(any(), integer(), integer(), binary(), binary(), integer(), list()) ->
    {ok, map()} | {rollback, atom() | {db, any()}}.
create_in_tx(Conn, Uid, AssignmentId, IdemKey, Digest, LearnerId, Assets) ->
    {ok, NextAttempt} = moya_submission_repo:next_attempt_tx(Conn, AssignmentId),
    case
        moya_submission_repo:create_idempotent_tx(Conn, #{
            id => elib_tsid:generate(),
            assignment_id => AssignmentId,
            learner_id => LearnerId,
            uid => Uid,
            attempt_no => NextAttempt,
            idempotency_key => IdemKey,
            request_digest => Digest
        })
    of
        {ok, #{<<"id">> := Sid, <<"attempt_no">> := Attempt} = Row} ->
            finish_create(Conn, Uid, AssignmentId, LearnerId, Assets, Row, Sid, Attempt);
        {error, idempotency_conflict} ->
            {rollback, idempotency_conflict};
        {error, Reason} ->
            {rollback, {db, Reason}}
    end.

-spec finish_create(any(), integer(), integer(), integer(), list(), map(), integer(), integer()) ->
    {ok, map()} | {rollback, atom()}.
finish_create(Conn, Uid, AssignmentId, LearnerId, Assets, Row, Sid, Attempt) ->
    case maps:get(created, Row, false) of
        true ->
            ok = moya_submission_repo:insert_assets_tx(Conn, Sid, Uid, Assets),
            ok = moya_submission_repo:mark_submitted_by_tx(Conn, AssignmentId, Uid),
            ok = moya_submission_repo:enqueue_ai_draft_tx(Conn, Sid),
            {ok, submission_created(Sid, AssignmentId, LearnerId, Attempt, true, Row)};
        false ->
            %% 幂等重放：返回既有 submission，不重复入队/挂附件（IDEMP-01）
            {ok, submission_created(Sid, AssignmentId, LearnerId, Attempt, false, Row)}
    end.

%% ---- payload 组装（TSID 一律字符串） ----

%% Step 17 联调补齐：响应补 learner_id（moya SubmissionCreated required，
%% 与 create_idempotent_tx 入参行对齐，家长端 DTO 校验依赖）
%% 2026-09-11：响应补 submitted_at（Rfc3339，create_idempotent_tx 行内返回；
%% moya 此前兜底空串）。幂等重放与新建同源同值。
-spec submission_created(integer(), integer(), integer(), integer(), boolean(), map()) -> map().
submission_created(Sid, AssignmentId, LearnerId, Attempt, Created, Row) ->
    #{
        <<"submission_id">> => integer_to_binary(Sid),
        <<"assignment_id">> => integer_to_binary(AssignmentId),
        <<"learner_id">> => integer_to_binary(LearnerId),
        <<"attempt_no">> => Attempt,
        <<"submitted_at">> => elib_dt:rfc3339_or_null(maps:get(<<"submitted_at">>, Row, null)),
        <<"status">> => <<"submitted">>,
        <<"ai_status">> => <<"queued">>,
        <<"idempotent_replayed">> => not Created
    }.

-spec assignment_summary(map()) -> map().
assignment_summary(R) ->
    #{
        <<"assignment_id">> => tsid(maps:get(<<"assignment_id">>, R)),
        %% v3 P0-1 修复：task_id 对外 = group_task.id（task_gid）十进制字符串
        <<"task_id">> => tsid(maps:get(<<"task_gid">>, R, 0)),
        %% Step 17 联调补齐：行内自含归属学员与截止时间（家长端 DTO 依赖）
        <<"learner_id">> => nullable_tsid(maps:get(<<"learner_id">>, R, null)),
        <<"deadline">> => nullable_bin(maps:get(<<"deadline">>, R, null)),
        %% CM-F2：说明字段（moya AssignmentSummary.description 可选；缺行→null）
        <<"description">> => nullable_bin(maps:get(<<"description">>, R, null)),
        <<"group_id">> => tsid(maps:get(<<"group_id">>, R)),
        <<"group_name">> => maps:get(<<"group_title">>, R, <<>>),
        <<"title">> => maps:get(<<"title">>, R, <<>>),
        <<"status">> => derived_status(R),
        <<"latest_submission">> => nullable_tsid(maps:get(<<"latest_submission_id">>, R, null)),
        <<"latest_attempt_no">> => maps:get(<<"latest_attempt_no">>, R, null),
        <<"latest_asset">> => latest_asset(R),
        <<"has_published_review">> => maps:get(<<"has_published">>, R, false) =:= true,
        <<"rework_required">> => false
    }.

%% @doc 作品预览句柄（moya 家长首页缩略图）：最新提交的首张 final_photo。
%% 只给 object_key——客户端持句柄按需调 /api/v1/attachment/view_url 换签名 URL
%% （MEDIA-03：后端既不持久化也不预签 URL）。无提交 / 最新提交无 final_photo
%% → null（不回退 practice_video：视频非图片，渲染语义不同）。
-spec latest_asset(map()) -> map() | null.
latest_asset(R) ->
    case maps:get(<<"latest_asset_key">>, R, null) of
        Key when is_binary(Key), Key =/= <<>> ->
            #{
                <<"object_key">> => Key,
                <<"kind">> => maps:get(<<"latest_asset_kind">>, R, <<"final_photo">>)
            };
        _ ->
            null
    end.

%% CM-F2：详情富行（repo 单行查询与列表同形状）；ACL 已过，此处只组装。
%% scope 兜底：富行缺失（链路残缺）按 not_found 拒绝，不回退薄 payload。
-spec detail_payload(integer()) -> {ok, map()} | {error, atom()}.
detail_payload(AssignmentId) ->
    case moya_submission_repo:assignment_detail(AssignmentId) of
        {ok, Row} when is_map(Row) ->
            {ok, assignment_summary(Row)};
        {ok, undefined} ->
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("assignment detail db error ~p", [Reason]),
            {error, db_error}
    end.

%% 推导态（CM-F4 四态，与 repo status_condition 同口径）：
%% pending 无提交 / reviewing 有最新提交+老师草稿+无 published /
%% submitted 有最新提交无草稿无 published / reviewed 有 published；
%% published 优先级最高（重练场景：已回评又有新草稿仍是 reviewed）。
%% 注：最新提交 withdrawn 时仍按有提交处理（与既有口径一致，
%% moya 无 withdrawn 作业态）。
-spec derived_status(map()) -> binary().
derived_status(#{<<"latest_submission_id">> := null}) ->
    <<"pending">>;
derived_status(#{<<"has_published">> := true}) ->
    <<"reviewed">>;
derived_status(#{<<"has_draft">> := true}) ->
    <<"reviewing">>;
derived_status(#{<<"latest_submission_id">> := _}) ->
    <<"submitted">>;
derived_status(_) ->
    <<"pending">>.

%% ---- 小工具 ----

-spec request_digest(integer(), list(), binary()) -> binary().
request_digest(LearnerId, Assets, Note) ->
    AttParts = [<<(integer_to_binary(AttId))/binary, ":">> || {AttId, _, _} <- Assets],
    Canonical = iolist_to_binary([
        integer_to_binary(LearnerId), <<"|">>, AttParts, <<"|">>, Note
    ]),
    binary:encode_hex(crypto:hash(sha256, Canonical), lowercase).

-spec tsid(integer() | null | undefined) -> binary().
tsid(Id) when is_integer(Id) -> integer_to_binary(Id);
tsid(_) -> <<"">>.

-spec nullable_tsid(integer() | null | undefined) -> binary() | null.
nullable_tsid(Id) when is_integer(Id) -> integer_to_binary(Id);
nullable_tsid(_) -> null.

%% 截止时间：elib_pg 返回 ISO8601 binary 时透传；其余（null/未设）归 null。
-spec nullable_bin(term()) -> binary() | null.
nullable_bin(V) when is_binary(V) -> V;
nullable_bin(_) -> null.
