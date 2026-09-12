-module(teaching_assignment_logic).
%%%
% 墨芽家长侧作业/提交业务逻辑（Step 9）
% Parent-side assignments & submissions logic
%
% 守卫链（全部服务端解析，客户端自报字段一律忽略）：
%   列表：resolve_guardian(Uid, LearnerId)（active）
%   提交：assignment_scope → learner_id 匹配 + resolve_guardian(_, _, submit)
%         + 附件归属 + task 开放（status=1 进行中）
%%%

-export([list/3, detail/2, create_submission/4]).

-include_lib("kernel/include/logger.hrl").
-ifdef(TEST).
%% 响应 payload 组装为纯函数，导出供 eunit 直接验收契约字段
-export([submission_created/6, assignment_summary/1]).
-endif.

-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 家长作业列表（按 learner）
%% {ok, PagePayload} | {error, Reason}
%% Reason：missing_learner_id(422) not_guardian(5422) db_error
-spec list(integer(), integer(), {integer(), integer()}) ->
    {ok, map()} | {error, atom()}.
list(Uid, LearnerId, {Page, Size}) ->
    case teaching_acl:resolve_guardian(Uid, LearnerId) of
        {ok, _} ->
            case teaching_submission_repo:assignments_for_learner(LearnerId, Page, Size) of
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

%% @doc 作业详情（监护人或本班 staff）
%% Reason：not_found(5440) forbidden(403) db_error
-spec detail(integer(), integer()) -> {ok, map()} | {error, atom()}.
detail(Uid, AssignmentId) ->
    case teaching_context_repo:assignment_scope(AssignmentId) of
        {ok, #{<<"learner_id">> := LearnerId, <<"group_id">> := GroupId} = Scope} when
            is_integer(LearnerId), is_integer(GroupId)
        ->
            case check_assignment_access(Uid, LearnerId, GroupId) of
                ok ->
                    {ok, assignment_detail(Scope)};
                staff ->
                    {ok, assignment_detail(Scope)};
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
    case teaching_acl:resolve_guardian(Uid, LearnerId) of
        {ok, _} ->
            ok;
        _ ->
            case teaching_acl:resolve_staff(Uid, GroupId) of
                {ok, _} -> staff;
                _ -> {error, forbidden}
            end
    end.

%% ---- 创建前置校验 ----

-spec precheck_create(integer(), integer(), map()) ->
    {ok, map()} | {error, atom()}.
precheck_create(Uid, AssignmentId, Body) ->
    case teaching_context_repo:assignment_scope(AssignmentId) of
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
    ClaimedLearner = tsid_to_int(maps:get(<<"learner_id">>, Body, <<>>)),
    BooleanGuards =
        [
            {claimed_learner_mismatch, ClaimedLearner =:= {ok, LearnerId}},
            {not_guardian, guard_can_submit(Uid, LearnerId)},
            {assignment_closed, TaskStatus =:= 1}
        ],
    case lists:keyfind(false, 2, BooleanGuards) of
        {Reason, false} ->
            {error, Reason};
        false ->
            case normalize_assets(maps:get(<<"assets">>, Body, [])) of
                {ok, Assets} ->
                    case
                        teaching_submission_repo:validate_assets(Uid, [A || {A, _, _} <- Assets])
                    of
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
    case teaching_acl:resolve_guardian(Uid, LearnerId, submit) of
        {ok, _} -> true;
        _ -> false
    end.

%% 附件规则：恰好 1 个 practice_video，0..3 个 final_photo（契约 minItems/maxItems）
-spec normalize_assets(term()) -> {ok, [{integer(), binary(), integer()}]} | {error, invalid}.
normalize_assets(Assets) when is_list(Assets), length(Assets) >= 1, length(Assets) =< 4 ->
    Parsed = [parse_asset(A) || A <- Assets],
    case lists:member(invalid, Parsed) of
        true ->
            {error, invalid};
        false ->
            Videos = [P || {_, <<"practice_video">>, _} = P <- Parsed],
            Photos = [P || {_, <<"final_photo">>, _} = P <- Parsed],
            case {length(Videos), length(Photos)} of
                {1, N} when N =< 3 ->
                    {ok, lists:sort(Parsed)};
                _ ->
                    {error, invalid}
            end
    end;
normalize_assets(_) ->
    {error, invalid}.

-spec parse_asset(map()) -> {integer(), binary(), integer()} | invalid.
parse_asset(#{<<"attachment_id">> := AttId0, <<"kind">> := Kind} = A) ->
    case {tsid_to_int(AttId0), Kind} of
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
        case teaching_submission_repo:lock_assignment_tx(Conn, AssignmentId) of
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
    {ok, NextAttempt} = teaching_submission_repo:next_attempt_tx(Conn, AssignmentId),
    case
        teaching_submission_repo:create_idempotent_tx(Conn, #{
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
            ok = teaching_submission_repo:insert_assets_tx(Conn, Sid, Uid, Assets),
            ok = teaching_submission_repo:mark_submitted_by_tx(Conn, AssignmentId, Uid),
            ok = teaching_submission_repo:enqueue_ai_draft_tx(Conn, Sid),
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
        <<"submitted_at">> => dt_ms(maps:get(<<"submitted_at">>, Row, null)),
        <<"status">> => <<"submitted">>,
        <<"ai_status">> => <<"queued">>,
        <<"idempotent_replayed">> => not Created
    }.

%% 时间契约（与 teaching_review_logic:dt_ms/1 同风格）：整型毫秒 → Rfc3339；
%% create_idempotent_tx 已在 SQL 层 to_char 出 Rfc3339 字符串 → 原样透传；
%% 缺失/异常输入 → null（不编造时间）。
-spec dt_ms(integer() | binary() | null | undefined) -> binary() | null.
dt_ms(Ts) when is_integer(Ts) ->
    elib_dt:to_rfc3339(Ts);
dt_ms(Ts) when is_binary(Ts), Ts =/= <<>> ->
    Ts;
dt_ms(_) ->
    null.

-spec assignment_summary(map()) -> map().
assignment_summary(R) ->
    #{
        <<"assignment_id">> => tsid(maps:get(<<"assignment_id">>, R)),
        %% v3 P0-1 修复：task_id 对外 = group_task.id（task_gid）十进制字符串
        <<"task_id">> => tsid(maps:get(<<"task_gid">>, R, 0)),
        %% Step 17 联调补齐：行内自含归属学员与截止时间（家长端 DTO 依赖）
        <<"learner_id">> => nullable_tsid(maps:get(<<"learner_id">>, R, null)),
        <<"deadline">> => nullable_bin(maps:get(<<"deadline">>, R, null)),
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

-spec assignment_detail(map()) -> map().
assignment_detail(Scope) ->
    #{
        <<"assignment_id">> => tsid(maps:get(<<"assignment_id">>, Scope)),
        %% v3 P0-1 修复：task_id 对外 = group_task.id（task_gid）十进制字符串
        <<"task_id">> => tsid(maps:get(<<"task_gid">>, Scope, 0)),
        <<"group_id">> => tsid(maps:get(<<"group_id">>, Scope)),
        <<"learner_id">> => nullable_tsid(maps:get(<<"learner_id">>, Scope, null)),
        <<"status">> => <<"pending">>
    }.

%% 推导态：pending 无提交 / submitted 有提交无已发布 / reviewed 有已发布
-spec derived_status(map()) -> binary().
derived_status(#{<<"latest_submission_id">> := null}) ->
    <<"pending">>;
derived_status(#{<<"has_published">> := true}) ->
    <<"reviewed">>;
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

-spec tsid_to_int(binary()) -> {ok, integer()} | {error, badarg}.
tsid_to_int(Bin) when is_binary(Bin) ->
    try binary_to_integer(Bin) of
        Int when is_integer(Int), Int > 0 -> {ok, Int};
        _ -> {error, badarg}
    catch
        _:_ -> {error, badarg}
    end;
tsid_to_int(_) ->
    {error, badarg}.
