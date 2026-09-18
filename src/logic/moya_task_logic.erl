-module(moya_task_logic).
%%%
% 墨芽教师教学作业业务逻辑（MN-TASK-01，P0-3）
% Teacher-side teaching task logic：老师作业列表 + 发布教学作业。
%
% 守卫链（全部服务端解析，客户端自报字段一律忽略）：
%   列表：active_staff_group_ids（manager/teacher/assistant 均可读）；
%         指定 group 必须是本人 active staff 班级，否则 class_not_visible(5430)
%   创建：resolve_staff(write)（仅 manager/teacher；assistant→role_denied）
%         → group_org 机构解析（NULL→cross_org fail closed）
%         → learner_readiness（active enrollment + 同机构 + 恰一可提交监护人）
%         → ds 同事务创建 + 106 持久幂等（同 key 同 digest 重放 / 异 digest 5460）
%%%

-export([list/3, create/3]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 老师作业列表（MN-TASK-01）
%% GroupIdOpt = undefined 汇总全部 active staff 班级 | integer 指定班
%% {ok, PagePayload} | {error, Reason}
%% Reason：class_not_visible(5430) db_error
-spec list(integer(), integer() | undefined, {integer(), integer()}) ->
    {ok, map()} | {error, atom()}.
list(Uid, GroupIdOpt, {Page, Size}) ->
    case moya_task_repo:active_staff_group_ids(Uid) of
        {ok, []} ->
            %% 非任何班 active staff：deny-by-default（不返回空列表）
            {error, class_not_visible};
        {ok, GroupIds} when is_list(GroupIds) ->
            case GroupIdOpt of
                undefined ->
                    fetch_list(Uid, undefined, Page, Size);
                GroupId ->
                    case lists:member(GroupId, GroupIds) of
                        true -> fetch_list(Uid, GroupId, Page, Size);
                        false -> {error, class_not_visible}
                    end
            end;
        {error, Reason} ->
            ?LOG_ERROR("teaching task list staff groups db error ~p", [Reason]),
            {error, db_error}
    end.

%% @doc 发布教学作业（MN-TASK-01/02）
%% Body：{group_id, title, description?, deadline?, learner_ids}（TSID string）
%% {ok, CreatePayload} | {error, Reason}
%% Reason：bad_param not_staff(5424) role_denied(5425) cross_org(5426)
%%         assignment_closed(5442, deadline 已过)
%%         learner_not_in_class(5431) guardian_setup_required(5432)
%%         idempotency_conflict(5460) idempotency_key_required(5461) db_error
-spec create(integer(), binary(), map()) -> {ok, map()} | {error, atom()}.
create(Uid, IdemKey, Body) when is_binary(IdemKey), IdemKey =/= <<>> ->
    case parse_body(Body) of
        {ok, Parsed} ->
            create_guarded(Uid, IdemKey, Parsed);
        {error, Reason} ->
            {error, Reason}
    end;
create(_Uid, _IdemKey, _Body) ->
    {error, idempotency_key_required}.

%%%===================================================================
%%% Internal functions：list
%%%===================================================================

-spec fetch_list(integer(), integer() | undefined, integer(), integer()) ->
    {ok, map()} | {error, atom()}.
fetch_list(Uid, GroupIdOpt, Page, Size) ->
    case moya_task_repo:list_tasks(Uid, GroupIdOpt, Page, Size) of
        {ok, Rows} ->
            case moya_task_repo:count_tasks(Uid, GroupIdOpt) of
                {ok, Total} ->
                    {ok, #{
                        <<"list">> => [task_item(R) || R <- Rows],
                        <<"page">> => Page,
                        <<"size">> => Size,
                        <<"total">> => Total
                    }};
                {error, Reason} ->
                    ?LOG_ERROR("teaching task count db error ~p", [Reason]),
                    {error, db_error}
            end;
        {error, Reason} ->
            ?LOG_ERROR("teaching task list db error ~p", [Reason]),
            {error, db_error}
    end.

%% payload 组装（group_id/task_id TSID 一律 string——v3 后 task_id=bigint id；
%% 时间为 RFC3339）
-spec task_item(map()) -> map().
task_item(R) ->
    #{
        <<"task_id">> => tsid(maps:get(<<"task_id">>, R, <<>>)),
        <<"group_id">> => tsid(maps:get(<<"group_id">>, R)),
        <<"group_name">> => maps:get(<<"group_name">>, R, <<>>),
        <<"title">> => maps:get(<<"title">>, R, <<>>),
        <<"description">> => maps:get(<<"description">>, R, <<>>),
        <<"deadline">> => nullable(maps:get(<<"deadline">>, R, null)),
        <<"learner_count">> => maps:get(<<"learner_count">>, R, 0),
        <<"submitted_count">> => maps:get(<<"submitted_count">>, R, 0),
        <<"pending_review_count">> => maps:get(<<"pending_review_count">>, R, 0),
        <<"created_at">> => maps:get(<<"created_at">>, R, <<>>)
    }.

%%%===================================================================
%%% Internal functions：create 前置解析与守卫
%%%===================================================================

-spec parse_body(map()) -> {ok, map()} | {error, atom()}.
parse_body(Body) when is_map(Body) ->
    case
        {
            tsid_field(<<"group_id">>, Body),
            parse_title(maps:get(<<"title">>, Body, <<>>)),
            parse_description(maps:get(<<"description">>, Body, undefined)),
            parse_deadline(maps:get(<<"deadline">>, Body, undefined)),
            parse_learner_ids(maps:get(<<"learner_ids">>, Body, undefined))
        }
    of
        {{ok, GroupId}, {ok, Title}, {ok, Desc}, {ok, Deadline}, {ok, LearnerIds}} ->
            {ok, #{
                group_id => GroupId,
                title => Title,
                description => Desc,
                deadline => Deadline,
                learner_ids => LearnerIds
            }};
        {_G, _T, _D, deadline_invalid, _L} ->
            {error, bad_param};
        {_G, _T, _D, deadline_past, _L} ->
            {error, assignment_closed};
        _ ->
            {error, bad_param}
    end;
parse_body(_Body) ->
    {error, bad_param}.

%% title：trim 后非空、≤200 字符
-spec parse_title(binary()) -> {ok, binary()} | error.
parse_title(Title) when is_binary(Title) ->
    Trimmed = string:trim(Title, both, " \t\r\n"),
    case Trimmed of
        <<>> ->
            error;
        _ ->
            case string:length(Trimmed) =< 200 of
                true -> {ok, Trimmed};
                false -> error
            end
    end;
parse_title(_) ->
    error.

%% description：可选，缺省 <<>>
-spec parse_description(term()) -> {ok, binary()} | error.
parse_description(undefined) -> {ok, <<>>};
parse_description(null) -> {ok, <<>>};
parse_description(Desc) when is_binary(Desc) -> {ok, Desc};
parse_description(_) -> error.

%% deadline：可选（缺失/null/undefined）；合法 RFC3339 且未来
%% 返回 {ok, binary()} | deadline_invalid | deadline_past
-spec parse_deadline(term()) -> {ok, binary()} | deadline_invalid | deadline_past.
parse_deadline(undefined) ->
    {ok, undefined};
parse_deadline(null) ->
    {ok, undefined};
parse_deadline(<<>>) ->
    {ok, undefined};
parse_deadline(Dl) when is_binary(Dl) ->
    case elib_dt:rfc3339_to(Dl) of
        Ts when is_integer(Ts), Ts > 0 ->
            case Ts > elib_dt:millisecond() of
                true -> {ok, Dl};
                false -> deadline_past
            end;
        _ ->
            deadline_invalid
    end;
parse_deadline(_) ->
    deadline_invalid.

%% learner_ids：非空 list、全为合法 TSID string、去重（重复拒绝）。
%% A1-D09：数量上限 200（class 常见规模之上限；仓内无既有列表 cap 常量可复用
%% ——moya_ai_draft_logic 白名单 50 项是 Schema 字段数组上限，语义不同不共用）。
%% 超限 → error → bad_param(422)：readiness IN 子句与逐条 insert_assignment
%% 随列表线性膨胀，上限拒绝超长事务/巨型 SQL。
-define(MAX_LEARNER_IDS, 200).

-spec parse_learner_ids(term()) -> {ok, [integer()]} | error.
parse_learner_ids(Ids) when is_list(Ids), Ids =/= [], length(Ids) =< ?MAX_LEARNER_IDS ->
    Parsed = [elib_tsid:from_binary(I) || I <- Ids],
    case lists:member(error, Parsed) of
        true ->
            error;
        false ->
            Ints = [I || {ok, I} <- Parsed],
            Deduped = lists:usort(Ints),
            case length(Deduped) =:= length(Ints) of
                true -> {ok, Ints};
                false -> error
            end
    end;
parse_learner_ids(_) ->
    error.

-spec tsid_field(binary(), map()) -> {ok, integer()} | error.
tsid_field(Key, Body) ->
    elib_tsid:from_binary(maps:get(Key, Body, undefined)).

-spec create_guarded(integer(), binary(), map()) -> {ok, map()} | {error, atom()}.
create_guarded(Uid, IdemKey, #{
    group_id := GroupId,
    title := Title,
    description := Desc,
    deadline := Deadline,
    learner_ids := LearnerIds
}) ->
    case moya_acl:resolve_staff(Uid, GroupId, write) of
        {ok, _Staff} ->
            check_org_then_learners(Uid, IdemKey, GroupId, Title, Desc, Deadline, LearnerIds);
        {error, inactive} ->
            %% 已移除的 staff 关系统一按 not_staff 拒绝（deny-by-default）
            {error, not_staff};
        {error, Reason} when Reason =:= role_denied; Reason =:= not_staff ->
            {error, Reason};
        {error, Reason} ->
            ?LOG_ERROR("teaching task create resolve_staff db error ~p", [Reason]),
            {error, db_error}
    end.

-spec check_org_then_learners(
    integer(), binary(), integer(), binary(), binary(), binary() | undefined, [integer()]
) ->
    {ok, map()} | {error, atom()}.
check_org_then_learners(Uid, IdemKey, GroupId, Title, Desc, Deadline, LearnerIds) ->
    case moya_context_repo:group_org(GroupId) of
        {ok, OrgId} when is_integer(OrgId) ->
            check_learners(Uid, IdemKey, GroupId, OrgId, Title, Desc, Deadline, LearnerIds);
        {ok, _NoOrg} ->
            %% 班级机构解析为 NULL → fail closed（T1/T2 跨机构防御）
            {error, cross_org};
        {error, Reason} ->
            ?LOG_ERROR("teaching task create group_org db error ~p", [Reason]),
            {error, db_error}
    end.

-spec check_learners(
    integer(),
    binary(),
    integer(),
    integer(),
    binary(),
    binary(),
    binary() | undefined,
    [integer()]
) ->
    {ok, map()} | {error, atom()}.
check_learners(Uid, IdemKey, GroupId, OrgId, Title, Desc, Deadline, LearnerIds) ->
    case moya_task_repo:learner_readiness(GroupId, OrgId, LearnerIds) of
        {ok, ResultMap} ->
            case first_not_ready(LearnerIds, ResultMap) of
                {error, Reason} ->
                    {error, Reason};
                ok ->
                    Learners = [
                        {L, G}
                     || L <- LearnerIds, {ok, G} <- [maps:get(L, ResultMap, {error, ok})]
                    ],
                    Digest = request_digest(GroupId, Title, Desc, Deadline, LearnerIds),
                    moya_task_ds:create(
                        Uid,
                        GroupId,
                        IdemKey,
                        Digest,
                        #{
                            title => Title, description => Desc, deadline => Deadline
                        },
                        Learners
                    )
            end;
        {error, Reason} ->
            ?LOG_ERROR("teaching task learner_readiness db error ~p", [Reason]),
            {error, db_error}
    end.

%% 逐 learner 判定；缺行（跨班/不存在）= learner_not_in_class
-spec first_not_ready([integer()], map()) -> ok | {error, atom()}.
first_not_ready([], _ResultMap) ->
    ok;
first_not_ready([LearnerId | Rest], ResultMap) ->
    case maps:get(LearnerId, ResultMap, missing) of
        missing -> {error, learner_not_in_class};
        {ok, _} -> first_not_ready(Rest, ResultMap);
        {error, Reason} -> {error, Reason}
    end.

%% 请求载荷摘要（sha256 hex，64 字符）：v3 改长度前缀框架化——每段前置
%% 32 位字节长，消除原 `|` 拼接的歧义碰撞（字段值本身含 `|` 时不同载荷
%% 可能拼出同一规范串）。
-spec request_digest(integer(), binary(), binary(), binary() | undefined, [integer()]) ->
    binary().
request_digest(GroupId, Title, Desc, Deadline, LearnerIds) ->
    Dl =
        case Deadline of
            undefined -> <<"">>;
            D when is_binary(D) -> D
        end,
    LearnerPart = iolist_to_binary([
        <<(integer_to_binary(L))/binary, ",">>
     || L <- LearnerIds
    ]),
    Framed = iolist_to_binary([
        begin
            B = iolist_to_binary(Seg),
            <<(byte_size(B)):32/integer, B/binary>>
        end
     || Seg <-
            [integer_to_binary(GroupId), Title, Desc, Dl, LearnerPart]
    ]),
    binary:encode_hex(crypto:hash(sha256, Framed), lowercase).

%%%===================================================================
%%% 小工具
%%%===================================================================

-spec tsid(integer() | null | undefined) -> binary().
tsid(Id) when is_integer(Id) -> integer_to_binary(Id);
tsid(_) -> <<"">>.

-spec nullable(binary() | null | undefined) -> binary() | null.
nullable(V) when is_binary(V) -> V;
nullable(_) -> null.
