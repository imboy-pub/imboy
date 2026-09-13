-module(moya_roster_logic).
%%%
% 墨芽教师侧只读班级学员名单业务逻辑（MN-ROSTER-01，P0-2）
% Read-only class roster logic：GET /api/v1/moya/classes/:group_id/learners。
%
% 守卫链（全部服务端解析，deny-by-default）：
%   1. resolve_staff(Uid, GroupId, undefined)——manager/teacher/assistant
%      三角色均可读（不带 write 白名单）；非 staff / removed staff（inactive）
%      统一折叠 class_not_visible（5430 语义：不泄漏班级存在性）
%   2. group_org 机构解析（NULL → cross_org fail closed，T1/T2 跨机构防御）
%   3. ds 名单读取（active enrollment + 同机构过滤，SQL 层不取隐私列）
%
% 三分支（P0-2 契约）：
%   submit_guardians == 1 → {assignment_ready:true,  setup_reason:null}
%   submit_guardians == 0 → {assignment_ready:false, setup_reason:no_submit_guardian}
%   submit_guardians >= 2 → {assignment_ready:false,
%                            setup_reason:multiple_submit_guardians}
% payload 冻结：{group_id, learners:[{learner_id, display_name,
% assignment_ready, setup_reason}]}；TSID 一律 string。
%%%

-export([list/2]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 只读班级学员名单（MN-ROSTER-01）
%% {ok, #{<<"group_id">> => binary(), <<"learners">> => [map()]}}
%% Reason：class_not_visible(5430) cross_org(5426) db_error
-spec list(integer(), integer()) -> {ok, map()} | {error, atom()}.
list(Uid, GroupId) when is_integer(Uid), Uid > 0, is_integer(GroupId), GroupId > 0 ->
    case moya_acl:resolve_staff(Uid, GroupId, undefined) of
        {ok, _StaffRow} ->
            check_org_then_fetch(GroupId);
        {error, not_staff} ->
            %% 非 staff：deny-by-default，不泄漏班级存在性
            {error, class_not_visible};
        {error, inactive} ->
            %% removed staff：与不可见同口径（5430，不区分存在性）
            {error, class_not_visible};
        {error, Reason} ->
            ?LOG_ERROR("teaching roster resolve_staff db error ~p", [Reason]),
            {error, db_error}
    end;
list(_Uid, _GroupId) ->
    {error, class_not_visible}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_org_then_fetch(integer()) -> {ok, map()} | {error, atom()}.
check_org_then_fetch(GroupId) ->
    case moya_context_repo:group_org(GroupId) of
        {ok, OrgId} when is_integer(OrgId) ->
            fetch_roster(GroupId, OrgId);
        {ok, _NoOrg} ->
            %% 班级机构解析为 NULL → fail closed（T1/T2 跨机构防御）
            {error, cross_org};
        {error, Reason} ->
            ?LOG_ERROR("teaching roster group_org db error ~p", [Reason]),
            {error, db_error}
    end.

-spec fetch_roster(integer(), integer()) -> {ok, map()} | {error, atom()}.
fetch_roster(GroupId, OrgId) ->
    case moya_roster_ds:list(GroupId, OrgId) of
        {ok, Rows} when is_list(Rows) ->
            {ok, roster_payload(GroupId, Rows)};
        {error, Reason} ->
            ?LOG_ERROR("teaching roster ds db error ~p", [Reason]),
            {error, db_error}
    end.

%% payload 组装（group_id/learner_id TSID 一律 string；按 learner_id 升序稳定）
-spec roster_payload(integer(), [map()]) -> map().
roster_payload(GroupId, Rows) ->
    Sorted = lists:sort(
        fun(A, B) ->
            maps:get(<<"learner_id">>, A) =< maps:get(<<"learner_id">>, B)
        end,
        Rows
    ),
    #{
        <<"group_id">> => integer_to_binary(GroupId),
        <<"learners">> => [learner_item(R) || R <- Sorted]
    }.

%% 三分支：恰一 active can_submit 监护人才 ready（P0-3 同语义，不猜默认监护人）
-spec learner_item(map()) -> map().
learner_item(#{<<"learner_id">> := LearnerId, <<"display_name">> := DisplayName} = Row) ->
    Guardians = maps:get(<<"submit_guardians">>, Row, 0),
    {Ready, Reason} =
        case Guardians of
            1 -> {true, null};
            0 -> {false, <<"no_submit_guardian">>};
            _ -> {false, <<"multiple_submit_guardians">>}
        end,
    #{
        <<"learner_id">> => integer_to_binary(LearnerId),
        <<"display_name">> => DisplayName,
        <<"assignment_ready">> => Ready,
        <<"setup_reason">> => Reason
    }.
