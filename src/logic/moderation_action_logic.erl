-module(moderation_action_logic).

%% R-02：把已确认的举报变成小而可审计的动作。
%% * 动作集（MVP）：warning / group_mute / group_kick / reject；
%%   content_removal / account_restrict 因现有 primitives 不支持，
%%   显式 unsupported（fail-closed），不落 executed 行。
%% * 幂等：同 case 同 action 已有 executed 行 → 拒绝重复执行。
%% * truthful：primitive 失败也落 failed 审计行，case 不被误推进。
%% * reversal：group_mute 撤销同步调 unmute（提前解除）；其余动作
%%   仅翻审计状态（可逆语义第二段扩展）。

-export([execute/5]).
-export([reverse/3]).
-export([list_by_case/1]).

-include("error_code.hrl").

-define(SUPPORTED_ACTIONS, [<<"warning">>, <<"group_mute">>, <<"group_kick">>, <<"reject">>]).

-opaque opts() :: #{
    reason := binary(),
    gid => integer(),
    duration_minutes => integer(),
    target_type => binary(),
    target_id => integer()
}.
-export_type([opts/0]).

%% @doc 执行处置动作。
%% 返回 {ok, ActionRow} | {error, binary()}（失败分支同样落 failed 行）。
-spec execute(integer(), integer(), binary(), integer(), opts()) ->
    {ok, map()} | {error, binary()}.
execute(AdmUid, CaseId, Action, TargetUid, Opts) when
    is_integer(AdmUid),
    AdmUid > 0,
    is_integer(CaseId),
    CaseId > 0,
    is_binary(Action),
    is_integer(TargetUid),
    TargetUid >= 0
->
    case lists:member(Action, ?SUPPORTED_ACTIONS) of
        true ->
            case validate_opts(Action, Opts) of
                ok ->
                    case report_ticket_ds:find_by_id(CaseId) of
                        Row when is_map(Row), map_size(Row) > 0 ->
                            case
                                moderation_action_repo:has_executed_same_action(
                                    CaseId, Action, TargetUid
                                )
                            of
                                {ok, false} ->
                                    do_execute(AdmUid, CaseId, Action, TargetUid, Opts, Row);
                                {ok, true} ->
                                    {error, <<"重复动作：同举报已执行过同款处置"/utf8>>};
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        _ ->
                            {error, <<"举报单不存在"/utf8>>}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, <<"暂不支持的处置动作"/utf8>>}
    end;
execute(_, _, _, _, _) ->
    {error, <<"处置参数无效"/utf8>>}.

%% @doc 撤销动作。group_mute 额外调 unmute 提前解除禁言。
%% mark_reversed 返回 0 行 → 已撤销/失败行不可撤销（幂等拒绝）。
-spec reverse(integer(), integer(), binary()) ->
    {ok, map()} | {error, binary()}.
reverse(AdmUid, ActionId, Reason) when is_integer(AdmUid), AdmUid > 0 ->
    case moderation_action_repo:find_by_id(ActionId) of
        {ok, Row} when is_map(Row) ->
            case maps:get(<<"status">>, Row, <<>>) of
                <<"executed">> ->
                    PreUndo = pre_undo(Row, AdmUid),
                    case PreUndo of
                        ok ->
                            case
                                moderation_action_repo:mark_reversed(
                                    ActionId, AdmUid, Reason
                                )
                            of
                                {ok, 1} ->
                                    {ok, maps:put(<<"status">>, <<"reversed">>, Row)};
                                {ok, _} ->
                                    {error, <<"动作不可撤销"/utf8>>};
                                {error, R} ->
                                    {error, R}
                            end;
                        {error, R} ->
                            {error, R}
                    end;
                <<"reversed">> ->
                    {error, <<"动作已撤销"/utf8>>};
                _ ->
                    {error, <<"仅已执行的动作可撤销"/utf8>>}
            end;
        {ok, undefined} ->
            {error, <<"动作不存在"/utf8>>};
        {error, R} ->
            {error, R}
    end;
reverse(_, _, _) ->
    {error, <<"撤销参数无效"/utf8>>}.

-spec list_by_case(integer()) -> {ok, [map()]} | {error, binary()}.
list_by_case(CaseId) when is_integer(CaseId), CaseId > 0 ->
    moderation_action_repo:find_by_case(CaseId);
list_by_case(_) ->
    {error, <<"参数无效"/utf8>>}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec validate_opts(binary(), opts()) -> ok | {error, binary()}.
validate_opts(<<"group_mute">>, Opts) ->
    Gid = maps:get(gid, Opts, 0),
    Duration = maps:get(duration_minutes, Opts, 0),
    case Gid > 0 andalso Duration > 0 of
        true -> ok;
        false -> {error, <<"群禁言需要 gid 与 duration_minutes"/utf8>>}
    end;
validate_opts(<<"group_kick">>, Opts) ->
    case maps:get(gid, Opts, 0) > 0 of
        true -> ok;
        false -> {error, <<"踢出群需要 gid"/utf8>>}
    end;
validate_opts(_, _) ->
    ok.

%% @doc 先执行 primitive 再落审计行：primitive 失败同样落 failed 行
%% （审计 truthful——「action failure leaves case truthful」验收口径）。
-spec do_execute(integer(), integer(), binary(), integer(), opts(), map()) ->
    {ok, map()} | {error, binary()}.
do_execute(AdmUid, CaseId, Action, TargetUid, Opts0, CaseRow) ->
    Opts = maps:put(actor, AdmUid, Opts0),
    case run_primitive(Action, TargetUid, Opts, CaseRow) of
        ok ->
            insert_action(
                AdmUid,
                CaseId,
                Action,
                TargetUid,
                Opts,
                <<"executed">>,
                #{},
                <<>>
            );
        {error, Reason} ->
            case
                insert_action(
                    AdmUid,
                    CaseId,
                    Action,
                    TargetUid,
                    Opts,
                    <<"failed">>,
                    #{},
                    Reason
                )
            of
                {ok, _Row} ->
                    {error, Reason};
                {error, DbR} ->
                    {error, <<Reason/binary, " | audit: ", DbR/binary>>}
            end
    end.

%% @doc 动作行落库。end_at 由 duration_minutes 换算（秒粒度原语）。
-spec insert_action(
    integer(),
    integer(),
    binary(),
    integer(),
    opts(),
    binary(),
    map(),
    binary()
) -> {ok, map()} | {error, binary()}.
insert_action(AdmUid, CaseId, Action, TargetUid, Opts, Status, Result, FailReason) ->
    DurationMinutes = maps:get(duration_minutes, Opts, 0),
    EndAt =
        case Action =:= <<"group_mute">> andalso DurationMinutes > 0 of
            true ->
                elib_dt:add(elib_dt:now(), {DurationMinutes, minute});
            false ->
                null
        end,
    ActionRow = #{
        case_id => CaseId,
        action => Action,
        target_type => maps:get(target_type, Opts, <<>>),
        target_id => maps:get(target_id, Opts, 0),
        target_uid => TargetUid,
        scope => #{
            <<"gid">> => maps:get(gid, Opts, 0),
            <<"duration_minutes">> => DurationMinutes
        },
        reason => maps:get(reason, Opts, <<>>),
        actor_id => AdmUid,
        status => Status,
        result => Result,
        fail_reason => FailReason,
        end_at => EndAt
    },
    elib_pg:with_tx(fun(Conn) ->
        moderation_action_repo:insert_tx(Conn, ActionRow)
    end).

%% @doc 动作原语分发。返回 ok | {error, Reason}。
-spec run_primitive(binary(), integer(), opts(), map()) -> ok | {error, binary()}.
run_primitive(<<"reject">>, _TargetUid, _Opts, _CaseRow) ->
    %% explicit no-action decision：本身就是验收认可的结论之一
    ok;
run_primitive(<<"warning">>, TargetUid, Opts, _CaseRow) ->
    Reason = maps:get(reason, Opts, <<>>),
    send_warning_notice(TargetUid, Reason);
run_primitive(<<"group_mute">>, TargetUid, Opts, _CaseRow) ->
    Gid = maps:get(gid, Opts, 0),
    DurationSeconds = maps:get(duration_minutes, Opts, 0) * 60,
    AdmUid0 = maps:get(actor, Opts, 0),
    case AdmUid0 of
        0 -> {error, <<"禁言缺少操作者群内身份"/utf8>>};
        _ -> group_member_logic:mute(AdmUid0, Gid, TargetUid, DurationSeconds)
    end;
run_primitive(<<"group_kick">>, TargetUid, Opts, _CaseRow) ->
    Gid = maps:get(gid, Opts, 0),
    group_member_logic:admin_kick(TargetUid, Gid, maps:get(actor, Opts, 0));
run_primitive(_, _, _, _) ->
    {error, <<"unsupported action">>}.

%% @doc group_mute 撤销的提前解除。
-spec pre_undo(map(), integer()) -> ok | {error, binary()}.
pre_undo(
    #{<<"action">> := <<"group_mute">>, <<"target_uid">> := TargetUid} = Row,
    AdmUid
) ->
    Scope = maps:get(<<"scope">>, Row, #{}),
    Gid = ec_cnv:to_integer(maps:get(<<"gid">>, Scope, 0)),
    case Gid > 0 of
        true -> group_member_logic:unmute(AdmUid, Gid, TargetUid);
        false -> ok
    end;
pre_undo(_Row, _AdmUid) ->
    ok.

%% @doc warning 通知：S2C 系统消息送达目标用户全部设备。
%% 举报人身份不进 payload（R-04 隐私红线）。
-spec send_warning_notice(integer(), binary()) -> ok | {error, binary()}.
send_warning_notice(TargetUid, Reason) ->
    MsgId = elib_id:gen("moderation_warning"),
    Action = <<"moderation_warning">>,
    Payload = #{<<"reason">> => Reason},
    Msg = message_ds:assemble_msg(<<"S2C">>, <<>>, TargetUid, Payload, MsgId, <<>>, Action, null),
    Msg2 = jsone:encode(Msg, [native_utf8]),
    MsLi = elib_retry_config:intervals(<<"notice">>),
    _ = message_ds:send_next(TargetUid, MsgId, Msg2, MsLi, [], true),
    ok.
