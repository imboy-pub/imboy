-module(moderation_action_logic).

%% R-02：把已确认的举报变成小而可审计的动作。
%% * 动作集（MVP）：warning / group_mute / group_kick / reject；
%%   account_restrict 复用后台禁用语义（status=0，scope 记 prev_status）
%%   并踢全部设备；content_removal 对 c2c/c2g 抹除 payload、对 channel 走
%%   显式 unsupported（fail-closed），不落 executed 行。
%% * 幂等：同 case 同 action 已有 executed 行 → 拒绝重复执行。
%% * truthful：primitive 失败也落 failed 审计行，case 不被误推进。
%% * reversal：group_mute 撤销同步调 unmute（提前解除）；其余动作
%%   仅翻审计状态（可逆语义第二段扩展）。

-export([execute/5]).
-export([reverse/3]).
-export([list_by_case/1]).
-export([expire_due/0]).

-include("error_code.hrl").

-define(SUPPORTED_ACTIONS, [
    <<"warning">>,
    <<"group_mute">>,
    <<"group_kick">>,
    <<"reject">>,
    <<"account_restrict">>,
    <<"content_removal">>
]).

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
            case validate_opts(Action, TargetUid, Opts) of
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

-spec validate_opts(binary(), integer(), opts()) -> ok | {error, binary()}.
validate_opts(<<"group_mute">>, _TargetUid, Opts) ->
    Gid = maps:get(gid, Opts, 0),
    Duration = maps:get(duration_minutes, Opts, 0),
    case Gid > 0 andalso Duration > 0 of
        true -> ok;
        false -> {error, <<"群禁言需要 gid 与 duration_minutes"/utf8>>}
    end;
validate_opts(<<"group_kick">>, _TargetUid, Opts) ->
    case maps:get(gid, Opts, 0) > 0 of
        true -> ok;
        false -> {error, <<"踢出群需要 gid"/utf8>>}
    end;
validate_opts(<<"account_restrict">>, TargetUid, _Opts) when is_integer(TargetUid), TargetUid > 0 ->
    ok;
validate_opts(<<"account_restrict">>, _TargetUid, _Opts) ->
    {error, <<"account restrict requires target_uid"/utf8>>};
validate_opts(_, _, _) ->
    ok.

%% @doc 先执行 primitive 再落审计行：primitive 失败同样落 failed 行
%% （审计 truthful——「action failure leaves case truthful」验收口径）。
-spec do_execute(integer(), integer(), binary(), integer(), opts(), map()) ->
    {ok, map()} | {error, binary()}.
do_execute(AdmUid, CaseId, Action, TargetUid, Opts0, CaseRow) ->
    Opts1 = maps:put(actor, AdmUid, Opts0),
    Opts =
        case Action of
            <<"account_restrict">> ->
                case user_ds:find_by_id(TargetUid, <<"status">>) of
                    Prev when is_map(Prev) ->
                        maps:put(prev_status, maps:get(<<"status">>, Prev, 1), Opts1);
                    _ ->
                        Opts1
                end;
            _ ->
                Opts1
        end,
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
        case
            (Action =:= <<"group_mute">> orelse Action =:= <<"account_restrict">>) andalso
                DurationMinutes > 0
        of
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
            <<"duration_minutes">> => DurationMinutes,
            <<"prev_status">> => maps:get(prev_status, Opts, 1)
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
run_primitive(<<"content_removal">>, _TargetUid, Opts, CaseRow) ->
    SubType = maps:get(<<"target_sub_type">>, CaseRow, <<>>),
    Evidence = normalize_scope(maps:get(<<"evidence">>, CaseRow, #{})),
    ServerMsgId = maps:get(<<"server_msg_id">>, Evidence, <<>>),
    RowId = ec_cnv:to_integer(maps:get(<<"target_id">>, CaseRow, 0)),
    Actor = maps:get(actor, Opts, 0),
    RevokedAt = elib_dt:now(),
    case SubType of
        <<"c2c">> ->
            wipe_user_msg(msg_c2c_repo, ServerMsgId);
        <<"c2g">> ->
            wipe_user_msg(msg_c2g_repo, ServerMsgId);
        <<"channel">> when RowId > 0 ->
            case channel_message_repo:revoke(RowId, Actor, RevokedAt) of
                {ok, N} when is_integer(N), N > 0 -> ok;
                {ok, _} -> {error, <<"内容不存在或已删除"/utf8>>};
                {error, R} -> {error, elib_cnv:safe_to_binary(R)}
            end;
        <<"channel">> ->
            {error, <<"channel 内容缺少有效行 ID"/utf8>>};
        _ ->
            {error, <<"content_removal 仅支持 c2c/c2g/channel 内容"/utf8>>}
    end;
run_primitive(<<"account_restrict">>, TargetUid, _Opts, _CaseRow) ->
    %% 复用后台禁用语义：status=0（登录签发门已拒绝 0）。
    %% prev_status 已由 do_execute 采集进 Opts，到期/撤销时按它恢复。
    case user_ds:update(TargetUid, #{status => 0}) of
        {ok, _} ->
            _ = user_device_logic:kick_all_other_devices(
                TargetUid, {<<"all">>, <<"admin_action">>}
            ),
            ok;
        {error, R} ->
            {error, elib_cnv:safe_to_binary(R)}
    end;
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
    #{
        <<"action">> := <<"account_restrict">>,
        <<"target_uid">> := TargetUid,
        <<"scope">> := Scope
    },
    _AdmUid
) ->
    %% 撤销限制：恢复禁用前的账号状态（仅当前仍为禁用态时）
    PrevStatus = ec_cnv:to_integer(maps:get(<<"prev_status">>, Scope, 1)),
    case user_ds:find_by_id(TargetUid, <<"status">>) of
        #{<<"status">> := 0} when PrevStatus > 0 ->
            case user_ds:update(TargetUid, #{status => PrevStatus}) of
                {ok, _} -> ok;
                {error, R} -> {error, elib_cnv:safe_to_binary(R)}
            end;
        _ ->
            ok
    end;
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

%% @doc 到期 sweep（供 moderation_sweep_logic 周期调用与运维手动触发）。
%% 把 end_at 已过期的 executed 禁言/限制动作翻转为 expired——业务失效
%% 由原语 until 时间戳保证，本函数闭环审计状态，并对 account_restrict
%% 按 scope.prev_status 恢复账号状态（仅当前仍为禁用态时）。
-spec expire_due() -> {ok, map()} | {error, binary()}.
expire_due() ->
    case moderation_action_repo:expire_due() of
        {ok, Rows} ->
            Restored = lists:sum(
                [
                    restore_restriction_status(Row)
                 || Row <- Rows,
                    maps:get(<<"action">>, Row, <<>>) =:= <<"account_restrict">>
                ]
            ),
            {ok, #{expired => length(Rows), restored => Restored}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc account_restrict 到期恢复：仅当当前仍为禁用态(0)时按 prev_status
%% 恢复，避免覆盖管理员后续的人工处置。
-spec restore_restriction_status(map()) -> integer().
restore_restriction_status(Row) ->
    TargetUid = maps:get(<<"target_uid">>, Row, 0),
    %% elib_pg 连接未配 json codec：jsonb 读回是文本，需兜底解码
    Scope = normalize_scope(maps:get(<<"scope">>, Row, #{})),
    PrevStatus = ec_cnv:to_integer(maps:get(<<"prev_status">>, Scope, 1)),
    case user_ds:find_by_id(TargetUid, <<"status">>) of
        #{<<"status">> := 0} when PrevStatus > 0 ->
            case user_ds:update(TargetUid, #{status => PrevStatus}) of
                {ok, _} -> 1;
                _ -> 0
            end;
        _ ->
            0
    end.

%% @doc scope 列 jsonb 在未配 json codec 的连接上读回为文本，此处兜底解码。
-spec normalize_scope(term()) -> map().
normalize_scope(Scope) when is_map(Scope) ->
    Scope;
normalize_scope(Bin) when is_binary(Bin) ->
    try jsone:decode(Bin, [{object_format, map}]) of
        M when is_map(M) -> M;
        _ -> #{}
    catch
        _:_ -> #{}
    end;
normalize_scope(_) ->
    #{}.

%% @doc content_removal 的用户消息抹除：payload 置合规占位（行保留，
%% 工单 evidence 已有内容快照供审计）；0 行 = 内容不存在或已删除。
-spec wipe_user_msg(atom(), binary()) -> ok | {error, binary()}.
wipe_user_msg(Repo, ServerMsgId) when is_binary(ServerMsgId), ServerMsgId =/= <<>> ->
    case Repo:update_payload_by_msg_id(ServerMsgId, <<"{\"admin_removed\":true}">>) of
        {ok, N} when is_integer(N), N > 0 -> ok;
        {ok, _} -> {error, <<"内容不存在或已删除"/utf8>>};
        {error, R} -> {error, elib_cnv:safe_to_binary(R)}
    end;
wipe_user_msg(_Repo, _) ->
    {error, <<"缺少 server_msg_id"/utf8>>}.
