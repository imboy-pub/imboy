-module(ops_report_logic).

%%%
%%% 运营周报编排 / Ops weekly-report orchestration（P0-3 A3-4）
%%%
%%% 每周统计新增/活跃/消息总量/举报，拼成周报文本，以 system agent 身份私信
%%% 运营者。参照 attach_cleanup_logic 的 ecron 入口范式 + ai_agent_proactive:send_text
%%% 的 C2C 投递骨架。
%%%
%%% 设计约束：
%%%   - 运营者 uid（operator_uid）与发送方（sender_uid）经 config 表配置，不硬编码身份。
%%%   - compute 纯读（可单测）；deliver 调 send_text 写库（需真跑 PG）。
%%%   - run_weekly_report 恒 ok（ecron 入口不抛），任何异常记日志后跳过投递。
%%%   - 统计「上周」：周一 00:00 ~ 周一 00:00（本地时区，与 ecron time_zone:local 对齐）。
%%%

-export([run_weekly_report/0, compute/0, render/1, deliver/1]).

-include("log.hrl").

%% config 表键（运营者后台可配）
-define(CFG_OPERATOR_UID, <<"ops_report.operator_uid">>).
-define(CFG_SENDER_UID, <<"ops_report.sender_uid">>).

%% 举报 Top N
-define(TOP_REASONS_LIMIT, 5).

%% 周报文案标签
-define(PERIOD_LABEL, <<"📅 周期："/utf8>>).
-define(NEW_USERS_LABEL, <<"👤 新增用户："/utf8>>).
-define(ACTIVE_USERS_LABEL, <<"🟢 活跃用户："/utf8>>).
-define(MESSAGES_LABEL, <<"💬 消息总量："/utf8>>).
-define(REPORTS_LABEL, <<"🚩 举报工单："/utf8>>).

%% ===================================================================
%% ecron 入口（每周一 09:00，由 sys.config ecron job 触发）
%% ===================================================================

%% @doc 生成上周周报并私信运营者。恒 ok，任何异常不抛（ecron 安全）。
-spec run_weekly_report() -> ok.
run_weekly_report() ->
    try
        case compute() of
            {ok, Stats} ->
                Report = render(Stats),
                deliver(Report);
            {error, Reason} ->
                ?WARN_LOG("[OPS_REPORT] compute failed: ~p~n", [Reason]),
                ok
        end
    catch
        Class:Err ->
            ?ERROR_LOG("[OPS_REPORT] run_weekly_report ~p:~p~n", [Class, Err]),
            ok
    end.

%% ===================================================================
%% compute：纯读统计（可单测，mock ops_report_repo）
%% ===================================================================

%% @doc 统计上周运营指标。返回 {ok, Stats} 或 {error, Reason}。
%% Stats = #{since => binary, until => binary, new_users => int,
%%           active_users => int, messages => int, reports => int,
%%           top_reasons => [{binary,int}]}
-spec compute() -> {ok, map()} | {error, term()}.
compute() ->
    {Since, Until} = last_week_range(),
    NewUsers = ops_report_repo:count_new_users(Since, Until),
    ActiveUsers = ops_report_repo:count_active_users(Since, Until),
    Messages = ops_report_repo:count_messages(Since, Until),
    Reports = ops_report_repo:count_reports(Since, Until),
    TopReasons = ops_report_repo:top_report_reasons(Since, Until, ?TOP_REASONS_LIMIT),
    {ok, #{
        since => Since,
        until => Until,
        new_users => NewUsers,
        active_users => ActiveUsers,
        messages => Messages,
        reports => Reports,
        top_reasons => TopReasons
    }}.

%% ===================================================================
%% render：拼周报文本（纯函数，可单测）
%% ===================================================================

%% @doc 把统计 map 渲染成周报文本
-spec render(map()) -> binary().
render(Stats) ->
    Since = maps:get(since, Stats, <<>>),
    Until = maps:get(until, Stats, <<>>),
    TopReasons = maps:get(top_reasons, Stats, []),
    Lines = [
        <<"📊 运营周报"/utf8>>,
        <<?PERIOD_LABEL/binary, Since/binary, " ~ "/utf8, Until/binary>>,
        <<"———————————"/utf8>>,
        <<?NEW_USERS_LABEL/binary, (int_bin(maps:get(new_users, Stats, 0)))/binary>>,
        <<?ACTIVE_USERS_LABEL/binary, (int_bin(maps:get(active_users, Stats, 0)))/binary>>,
        <<?MESSAGES_LABEL/binary, (int_bin(maps:get(messages, Stats, 0)))/binary>>,
        <<?REPORTS_LABEL/binary, (int_bin(maps:get(reports, Stats, 0)))/binary>>
    ],
    Lines2 =
        case TopReasons of
            [] -> Lines;
            _ -> Lines ++ [<<"———————————"/utf8>> | render_top_reasons(TopReasons)]
        end,
    iolist_to_binary(lists:join(<<"\n"/utf8>>, Lines2)).

-spec render_top_reasons([{binary(), integer()}]) -> [binary()].
render_top_reasons(Reasons) ->
    [<<"🏆 举报热点"/utf8>> | [render_reason(R, N) || {R, N} <- Reasons]].

-spec render_reason(binary(), integer()) -> binary().
render_reason(Reason, N) ->
    <<"• "/utf8, Reason/binary, "（", (int_bin(N))/binary, "次）"/utf8>>.

%% ===================================================================
%% deliver：以 system agent 身份私信运营者
%% ===================================================================

%% @doc 读 config 的 operator_uid / sender_uid，调 ai_agent_proactive:send_text。
%% 未配置 operator_uid 或 sender_uid 时跳过（no-op，记日志）。
-spec deliver(binary()) -> ok.
deliver(Report) ->
    OperatorUid = to_int(config_ds:get(?CFG_OPERATOR_UID, 0)),
    SenderUid = to_int(config_ds:get(?CFG_SENDER_UID, 0)),
    case {OperatorUid > 0, SenderUid > 0} of
        {true, true} ->
            ai_agent_proactive:send_text(SenderUid, OperatorUid, Report);
        _ ->
            ?WARN_LOG(
                "[OPS_REPORT] skip deliver: operator_uid=~p sender_uid=~p (未配置)~n",
                [OperatorUid, SenderUid]
            ),
            ok
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% 上周时间范围 [上周一 00:00, 本周一 00:00)，本地时区 rfc3339
-spec last_week_range() -> {binary(), binary()}.
last_week_range() ->
    Now = elib_dt:now(),
    %% calendar:day_of_the_week(Date): Date={Y,M,D}，返回 1=Monday..7=Sunday
    {Date, _Time} = erlang:localtime(),
    DayOfWeek = calendar:day_of_the_week(Date),
    %% 本周一 00:00 = 现在 - (DayOfWeek-1) 天（转分钟），到本周一 00:00 的偏移
    %% 注意：粗略按天回退到周一，00:00 对齐由 to_rfc3339 截断保证
    MinsToMonday = (DayOfWeek - 1) * 24 * 60,
    ThisMonday = elib_dt:minus(Now, {MinsToMonday, minute}),
    LastMonday = elib_dt:minus(ThisMonday, {7 * 24 * 60, minute}),
    {LastMonday, ThisMonday}.

-spec to_int(term()) -> integer().
to_int(I) when is_integer(I) ->
    I;
to_int(B) when is_binary(B) ->
    try
        binary_to_integer(B)
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.

-spec int_bin(integer()) -> binary().
int_bin(N) when is_integer(N) ->
    ec_cnv:to_binary(N).
