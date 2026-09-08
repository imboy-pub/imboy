-module(moderation_appeal_logic).

%% R-04：处置申诉链（operational baseline）。
%% * 可用性：`appeal` 为平台内建 feature 键（imboy_feature:enabled/1 门），
%%   按 profile/runtime 配置开关——是否对用户开放由政策/法务结论决定，
%%   工程侧默认开放且可随时按 profile 关闭（Configure availability by
%%   policy profile, not country if statements）；
%% * one appeal：action_id+appellant_uid 唯一（库约束+前置查询双保险）；
%% * eligible：action 存在、status=executed、申诉人=被处置人、未超窗口；
%% * independent reviewer：复审 Admin 不得是该 action 的原执行者；
%% * accepted 联动 moderation_action_logic:reverse（终审翻案 = 撤销动作）；
%% * reporter privacy：appeal 出参不含 case 举报人字段（本表无 reporter；
%%   用户出参连 case_id 也不透出）；
%% * notification 口径：主动推送依赖既有通知设施（R-02 打磨项），本链先
%%   保证"用户可自助查询处置理由与申诉结果"（decision notice 可达性）。

-export([submit/3]).
-export([review/4]).
-export([my_list/1]).
-export([my_actions/1]).
-export([admin_list/3]).
-export([appeal_window_ms/0]).

-include("log.hrl").

-define(DEFAULT_WINDOW_DAYS, 30).
-define(MAX_REASON_LEN, 1000).

%% ===================================================================
%% 用户侧
%% ===================================================================

%% @doc 被处置用户对处置动作发起一次申诉。
-spec submit(integer(), integer(), binary()) ->
    {ok, map()} | {error, binary()}.
submit(AppellantUid, ActionId, Reason) when
    is_integer(AppellantUid), AppellantUid > 0, is_integer(ActionId), ActionId > 0
->
    case imboy_feature:enabled(appeal) of
        true ->
            do_submit(AppellantUid, ActionId, Reason);
        false ->
            {error, <<"申诉通道未开放"/utf8>>}
    end;
submit(_, _, _) ->
    {error, <<"申诉参数无效"/utf8>>}.

do_submit(AppellantUid, ActionId, Reason0) ->
    Reason = normalize_reason(Reason0),
    case Reason =/= <<>> of
        false ->
            {error, <<"申诉理由不能为空"/utf8>>};
        true ->
            case moderation_action_repo:find_by_id(ActionId) of
                {ok, Action} ->
                    case eligible(AppellantUid, Action) of
                        ok ->
                            create_appeal(AppellantUid, Action, Reason);
                        {error, Msg} ->
                            {error, Msg}
                    end;
                _ ->
                    {error, <<"处置记录不存在"/utf8>>}
            end
    end.

%% eligible 判定：申诉人=被处置人 + action 已执行 + 未超申诉窗口
eligible(AppellantUid, Action) ->
    TargetUid = row_int(maps:get(<<"target_uid">>, Action, 0)),
    Status = maps:get(<<"status">>, Action, <<>>),
    case TargetUid =:= AppellantUid of
        false ->
            {error, <<"只能对针对自己的处置提出申诉"/utf8>>};
        true ->
            case Status =:= <<"executed">> of
                false ->
                    {error, <<"该处置状态不可申诉"/utf8>>};
                true ->
                    case within_window(Action) of
                        true -> ok;
                        false -> {error, <<"已超过申诉期限"/utf8>>}
                    end
            end
    end.

create_appeal(AppellantUid, Action, Reason) ->
    ActionId = row_int(maps:get(<<"id">>, Action, 0)),
    case moderation_appeal_repo:find_by_action_appellant(ActionId, AppellantUid) of
        {ok, _} ->
            {error, <<"您已对该处置提出过申诉"/utf8>>};
        _ ->
            case
                moderation_appeal_repo:insert(#{
                    action_id => ActionId,
                    case_id => row_int(maps:get(<<"case_id">>, Action, 0)),
                    appellant_uid => AppellantUid,
                    reason => Reason
                })
            of
                {ok, Row} ->
                    {ok, user_view(Row, Action)};
                {error, Reason2} ->
                    ?ERROR_LOG(["moderation_appeal insert failed: ", Reason2]),
                    {error, <<"申诉提交失败"/utf8>>}
            end
    end.

%% @doc 我的申诉列表（含处置摘要与终审结果——decision notice 可达性）。
%% 出参不含 case_id/reporter：举报人隐私在用户面不可见。
-spec my_list(integer()) -> {ok, [map()]} | {error, binary()}.
my_list(AppellantUid) ->
    case moderation_appeal_repo:list_by_appellant(AppellantUid) of
        {ok, Rows} ->
            {ok, [user_view(Row, lookup_action(Row)) || Row <- Rows]};
        {error, _} ->
            {error, <<"查询失败"/utf8>>}
    end.

%% @doc R-04.1：针对我的处置动作列表（appeal/actions 端点）——
%% 用户据此发现可申诉的处置并看到申诉状态（appealed 标记）。
-spec my_actions(integer()) -> {ok, [map()]} | {error, binary()}.
my_actions(AppellantUid) ->
    case moderation_action_repo:list_by_target(AppellantUid) of
        {ok, Rows} ->
            Appealed =
                case moderation_appeal_repo:list_by_appellant(AppellantUid) of
                    {ok, AppealRows} ->
                        lists:usort([row_int(maps:get(<<"action_id">>, R, 0)) || R <- AppealRows]);
                    _ ->
                        []
                end,
            {ok, [action_user_view(Row, Appealed) || Row <- Rows]};
        {error, _} ->
            {error, <<"查询失败"/utf8>>}
    end.

%% 处置动作的用户面视图：无 actor_id/case_id/result（执行细节与举报
%% 关联不外露），带 appealed 标记供前端区分「可申诉/已申诉」。
action_user_view(Row, AppealedActionIds) ->
    ActionId = row_int(maps:get(<<"id">>, Row, 0)),
    #{
        <<"id">> => ActionId,
        <<"action">> => maps:get(<<"action">>, Row, <<>>),
        <<"reason">> => maps:get(<<"reason">>, Row, <<>>),
        <<"status">> => maps:get(<<"status">>, Row, <<>>),
        <<"reversed_at">> => maps:get(<<"reversed_at">>, Row, null),
        <<"created_at">> => maps:get(<<"created_at">>, Row, null),
        <<"appealed">> => lists:member(ActionId, AppealedActionIds)
    }.

%% ===================================================================
%% Admin 侧
%% ===================================================================

%% @doc Admin 分页列表。
-spec admin_list(integer(), integer(), binary() | undefined) ->
    {ok, [map()]} | {error, binary()}.
admin_list(Page, Size, Status) when is_integer(Page), Page > 0, is_integer(Size), Size > 0 ->
    case moderation_appeal_repo:list_page(Page, Size, Status) of
        {ok, Rows} -> {ok, Rows};
        {error, _} -> {error, <<"查询失败"/utf8>>}
    end;
admin_list(_, _, _) ->
    {error, <<"参数错误"/utf8>>}.

%% @doc 独立复审终审：accept=翻案（联动 reverse）；reject=维持。
%% ReviewerId 不得为原处置执行者（actor_id）——independent reviewer。
-spec review(integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, binary()}.
review(ReviewerId, AppealId, Verdict, ReviewReason0) when
    is_integer(ReviewerId), ReviewerId > 0, is_integer(AppealId), AppealId > 0
->
    Normalized =
        case Verdict of
            <<"accept">> -> <<"accepted">>;
            <<"reject">> -> <<"rejected">>;
            _ -> undefined
        end,
    case Normalized of
        undefined ->
            {error, <<"无效的复审结论"/utf8>>};
        _ ->
            case moderation_appeal_repo:find_by_id(AppealId) of
                {ok, Appeal} ->
                    do_review(ReviewerId, Appeal, Normalized, normalize_reason(ReviewReason0));
                _ ->
                    {error, <<"申诉不存在"/utf8>>}
            end
    end;
review(_, _, _, _) ->
    {error, <<"参数错误"/utf8>>}.

do_review(ReviewerId, Appeal, Verdict, ReviewReason) ->
    ActionId = row_int(maps:get(<<"action_id">>, Appeal, 0)),
    case moderation_action_repo:find_by_id(ActionId) of
        {ok, Action} ->
            ActorId = row_int(maps:get(<<"actor_id">>, Action, 0)),
            case ActorId =:= ReviewerId of
                true ->
                    {error, <<"原处置执行者不能复审该申诉"/utf8>>};
                false ->
                    finish_review(ReviewerId, Appeal, Action, Verdict, ReviewReason)
            end;
        _ ->
            {error, <<"关联处置记录不存在"/utf8>>}
    end.

finish_review(ReviewerId, Appeal, Action, Verdict, ReviewReason) ->
    AppealId = row_int(maps:get(<<"id">>, Appeal, 0)),
    ActionId = row_int(maps:get(<<"action_id">>, Appeal, 0)),
    NowMs = elib_dt:millisecond(),
    case moderation_appeal_repo:mark_reviewed(AppealId, Verdict, ReviewerId, ReviewReason, NowMs) of
        {ok, _} ->
            case Verdict of
                <<"accepted">> ->
                    %% 终审翻案：联动撤销原处置动作。撤销失败不回滚终审
                    %% 判定（终审已 truthful 落库，撤销幂等可重试）。
                    case
                        moderation_action_logic:reverse(
                            ReviewerId,
                            ActionId,
                            <<"申诉翻案: "/utf8, ReviewReason/binary>>
                        )
                    of
                        {ok, _} ->
                            ok;
                        {error, RevReason} ->
                            ?ERROR_LOG(["appeal reversal failed: ", ActionId, RevReason]),
                            ok
                    end;
                _ ->
                    ok
            end,
            {ok, maps:put(<<"status">>, Verdict, Action#{<<"appeal_id">> => AppealId})};
        {error, Reason} ->
            ?ERROR_LOG(["moderation_appeal review failed: ", Reason]),
            {error, <<"复审操作失败"/utf8>>}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

lookup_action(Row) ->
    case moderation_action_repo:find_by_id(row_int(maps:get(<<"action_id">>, Row, 0))) of
        {ok, Action} -> Action;
        _ -> #{}
    end.

%% 出参（用户面）：处置类型/理由/状态 + 申诉状态/终审理由。
%% 刻意不含 case_id/reporter/reviewer_id。
user_view(AppealRow, Action) ->
    #{
        <<"id">> => maps:get(<<"id">>, AppealRow, 0),
        <<"reason">> => maps:get(<<"reason">>, AppealRow, <<>>),
        <<"status">> => maps:get(<<"status">>, AppealRow, <<>>),
        <<"review_reason">> => maps:get(<<"review_reason">>, AppealRow, <<>>),
        <<"reviewed_at">> => maps:get(<<"reviewed_at">>, AppealRow, null),
        <<"created_at">> => maps:get(<<"created_at">>, AppealRow, null),
        <<"action">> =>
            #{
                <<"id">> => maps:get(<<"id">>, Action, 0),
                <<"action">> => maps:get(<<"action">>, Action, <<>>),
                <<"reason">> => maps:get(<<"reason">>, Action, <<>>),
                <<"status">> => maps:get(<<"status">>, Action, <<>>),
                <<"reversed_at">> => maps:get(<<"reversed_at">>, Action, null)
            }
    }.

within_window(Action) ->
    CreatedAt = maps:get(<<"created_at">>, Action, null),
    WindowMs = appeal_window_ms(),
    case elib_dt:rfc3339_to(CreatedAt, millisecond) of
        CreatedMs when is_integer(CreatedMs) ->
            elib_dt:millisecond() - CreatedMs =< WindowMs;
        _ ->
            %% 时间基线缺失（异常行）不放宽：按超期处理，避免绕过期限
            false
    end.

%% @doc 申诉窗口（毫秒）。`appeal_window_days` 配置，默认 30 天；0=不设限。
-spec appeal_window_ms() -> integer().
appeal_window_ms() ->
    Days = config_ds:get(<<"appeal_window_days">>, ?DEFAULT_WINDOW_DAYS),
    case ec_cnv:to_integer(Days) of
        N when N =:= 0 -> 16 * 365 * 24 * 3600 * 1000;
        N when N > 0 -> N * 24 * 3600 * 1000;
        _ -> ?DEFAULT_WINDOW_DAYS * 24 * 3600 * 1000
    end.

normalize_reason(Reason) when is_binary(Reason) ->
    Str = string:trim(Reason),
    case byte_size(Str) > ?MAX_REASON_LEN of
        true -> binary:part(Str, 0, ?MAX_REASON_LEN);
        false -> Str
    end;
normalize_reason(Reason) ->
    normalize_reason(ec_cnv:to_binary(Reason)).

row_int(V) when is_integer(V) -> V;
row_int(V) -> ec_cnv:to_integer(V).
