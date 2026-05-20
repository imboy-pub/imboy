-module(channel_ws_push_benchmark).

-export([run/0]).
-export([run/1]).
-export([run_and_write_report/1]).

-define(DEFAULT_SAMPLE_SIZE, 50).
-define(DEFAULT_THRESHOLD_MS, 1000.0).
-define(DEFAULT_RECEIVE_TIMEOUT_MS, 2000).

-spec run() -> {ok, map()} | {error, term()}.
run() ->
    run(#{}).

-spec run(map()) -> {ok, map()} | {error, term()}.
run(Opts0) when is_map(Opts0) ->
    try
        Opts = normalize_opts(Opts0),
        ensure_db_connection(),
        Ctx = prepare_context(),
        try
            SampleSize = maps:get(sample_size, Opts),
            ThresholdMs = maps:get(threshold_ms, Opts),
            ReceiveTimeoutMs = maps:get(receive_timeout_ms, Opts),
            Samples = measure(
                SampleSize,
                fun(N) -> run_ws_push_case(Ctx, N, ReceiveTimeoutMs) end
            ),
            Metric = summarize(Samples, ThresholdMs),
            Metrics = #{ws_push_latency => Metric},
            {ok, #{
                generated_at => elib_dt:now(),
                sample_size => SampleSize,
                threshold_ms => ThresholdMs,
                receive_timeout_ms => ReceiveTimeoutMs,
                sender_uid => maps:get(sender_uid, Ctx),
                receiver_uid => maps:get(receiver_uid, Ctx),
                metric_name => <<"ws_push_latency">>,
                metrics => Metrics,
                pass => metrics_all_pass(Metrics)
            }}
        after
            cleanup_context(Ctx)
        end
    catch
        throw:{benchmark_error, Reason} ->
            {error, Reason};
        Class:Reason:Stack ->
            {error, {Class, Reason, Stack}}
    end;
run(_) ->
    {error, bad_options}.

-spec run_and_write_report(undefined | string() | binary()) -> {ok, string()} | {error, {term(), string()}}.
run_and_write_report(ReportPath0) ->
    ReportPath = resolve_report_path(ReportPath0),
    ok = filelib:ensure_dir(ReportPath),
    case run() of
        {ok, Result} ->
            case write_report(ReportPath, render_report(Result)) of
                ok ->
                    {ok, ReportPath};
                {error, Reason} ->
                    {error, {{write_report_failed, Reason}, ReportPath}}
            end;
        {error, Reason} ->
            FailureReport = render_failure_report(Reason),
            case write_report(ReportPath, FailureReport) of
                ok ->
                    {error, {Reason, ReportPath}};
                {error, WriteReason} ->
                    {error, {{Reason, write_report_failed, WriteReason}, ReportPath}}
            end
    end.

%% ===================================================================
%% Benchmark cases
%% ===================================================================

-spec run_ws_push_case(map(), pos_integer(), pos_integer()) -> ok.
run_ws_push_case(Ctx, Seq, ReceiveTimeoutMs) ->
    SenderUid = maps:get(sender_uid, Ctx),
    ReceiverUid = maps:get(receiver_uid, Ctx),
    Payload = #{
        <<"seq">> => Seq,
        <<"kind">> => <<"channel_ws_push_latency">>
    },
    ok = msg_s2c_ds:send(
        SenderUid,
        [ReceiverUid],
        <<"channel_ws_push_latency">>,
        <<>>,
        null,
        Payload,
        nosave
    ),
    case wait_for_seq(Seq, ReceiveTimeoutMs) of
        ok ->
            ok;
        {error, Reason} ->
            throw({benchmark_error, Reason})
    end.

%% ===================================================================
%% Setup helpers
%% ===================================================================

-spec normalize_opts(map()) -> map().
normalize_opts(Opts0) ->
    SampleSize0 = maps:get(sample_size, Opts0, ?DEFAULT_SAMPLE_SIZE),
    Threshold0 = maps:get(threshold_ms, Opts0, ?DEFAULT_THRESHOLD_MS),
    ReceiveTimeout0 = maps:get(receive_timeout_ms, Opts0, ?DEFAULT_RECEIVE_TIMEOUT_MS),
    #{
        sample_size => normalize_positive_int(SampleSize0, ?DEFAULT_SAMPLE_SIZE),
        threshold_ms => normalize_float(Threshold0, ?DEFAULT_THRESHOLD_MS),
        receive_timeout_ms => normalize_positive_int(ReceiveTimeout0, ?DEFAULT_RECEIVE_TIMEOUT_MS)
    }.

-spec ensure_db_connection() -> ok.
ensure_db_connection() ->
    case elib_pg:one(<<"SELECT 1 AS ok">>, []) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            throw({benchmark_error, {db_unavailable, Reason}})
    end.

-spec prepare_context() -> map().
prepare_context() ->
    SenderUid = normalize_uid(integer_to_binary(elib_tsid:generate())),
    ReceiverUid = normalize_uid(integer_to_binary(elib_tsid:generate())),
    ensure_user(SenderUid, <<"ws_perf_sender">>),
    ensure_user(ReceiverUid, <<"ws_perf_receiver">>),
    ReceiverPid = start_receiver(ReceiverUid, self()),
    #{
        sender_uid => SenderUid,
        receiver_uid => ReceiverUid,
        receiver_pid => ReceiverPid
    }.

-spec cleanup_context(map()) -> ok.
cleanup_context(Ctx) ->
    ReceiverPid = maps:get(receiver_pid, Ctx, undefined),
    stop_receiver(ReceiverPid).

-spec ensure_user(integer(), binary()) -> ok.
ensure_user(Uid, Prefix) ->
    UidBin = integer_to_binary(Uid),
    Account = <<Prefix/binary, "_", UidBin/binary>>,
    Mobile = build_mobile(UidBin),
    Email = <<Account/binary, "@perf.local">>,
    User = #{
        id => Uid,
        account => Account,
        nickname => Account,
        password => <<"password123">>,
        mobile => Mobile,
        email => Email,
        created_at => elib_dt:now()
    },
    case user_repo:create(User) of
        ok ->
            ok;
        {error, _} ->
            case user_repo:find_by_id(Uid, <<"id">>) of
                #{} = Row when map_size(Row) > 0 -> ok;
                _ -> throw({benchmark_error, {ensure_user_failed, Uid}})
            end
    end.

-spec normalize_uid(integer() | binary()) -> integer().
normalize_uid(Uid) when is_integer(Uid), Uid > 0 ->
    Uid;
normalize_uid(UidBin) when is_binary(UidBin) ->
    try binary_to_integer(UidBin) of
        Int when is_integer(Int), Int > 0 ->
            Int;
        _ ->
            throw({benchmark_error, {invalid_uid, UidBin}})
    catch
        _:_ ->
            throw({benchmark_error, {invalid_uid, UidBin}})
    end;
normalize_uid(Uid) ->
    throw({benchmark_error, {invalid_uid, Uid}}).

-spec build_mobile(binary()) -> binary().
build_mobile(UidBin) ->
    Tail10 = if
        byte_size(UidBin) >= 10 ->
            binary:part(UidBin, byte_size(UidBin) - 10, 10);
        true ->
            PaddingLen = 10 - byte_size(UidBin),
            <<(list_to_binary(lists:duplicate(PaddingLen, $0)))/binary, UidBin/binary>>
    end,
    <<"1", Tail10/binary>>.

-spec start_receiver(integer(), pid()) -> pid().
start_receiver(Uid, Parent) ->
    Pid = spawn_link(fun() -> receiver_init(Uid, Parent) end),
    receive
        {ws_receiver_ready, Pid} ->
            Pid;
        {ws_receiver_failed, Pid, Reason} ->
            throw({benchmark_error, {receiver_start_failed, Reason}})
    after 2000 ->
        throw({benchmark_error, receiver_start_timeout})
    end.

-spec stop_receiver(pid() | undefined) -> ok.
stop_receiver(undefined) ->
    ok;
stop_receiver(Pid) when is_pid(Pid) ->
    Pid ! stop,
    receive
        {ws_receiver_stopped, Pid} ->
            ok
    after 1000 ->
        exit(Pid, kill),
        ok
    end;
stop_receiver(_) ->
    ok.

-spec receiver_init(integer(), pid()) -> no_return().
receiver_init(Uid, Parent) ->
    case imboy_syn:join(Uid, <<"bench">>, self(), <<"ws_push_latency_bench">>) of
        ok ->
            Parent ! {ws_receiver_ready, self()},
            receiver_loop(Uid, Parent);
        {error, Reason} ->
            Parent ! {ws_receiver_failed, self(), Reason},
            exit(normal)
    end.

-spec receiver_loop(integer(), pid()) -> no_return().
receiver_loop(Uid, Parent) ->
    receive
        stop ->
            _ = imboy_syn:leave(Uid, self()),
            Parent ! {ws_receiver_stopped, self()},
            exit(normal);
        {timeout, _Ref, BinFrame} when is_binary(BinFrame) ->
            Parent ! {ws_push_frame, erlang:monotonic_time(microsecond), BinFrame},
            receiver_loop(Uid, Parent);
        _Other ->
            receiver_loop(Uid, Parent)
    end.

-spec wait_for_seq(pos_integer(), pos_integer()) -> ok | {error, term()}.
wait_for_seq(Seq, TimeoutMs) ->
    DeadlineUs = erlang:monotonic_time(microsecond) + TimeoutMs * 1000,
    wait_for_seq(Seq, DeadlineUs, TimeoutMs).

-spec wait_for_seq(pos_integer(), integer(), pos_integer()) -> ok | {error, term()}.
wait_for_seq(Seq, DeadlineUs, TimeoutMs) ->
    NowUs = erlang:monotonic_time(microsecond),
    RemainingUs = DeadlineUs - NowUs,
    case RemainingUs =< 0 of
        true ->
            {error, {push_timeout, Seq, TimeoutMs}};
        false ->
            RemainingMs = erlang:max(1, trunc(math:ceil(RemainingUs / 1000))),
            receive
                {ws_push_frame, _RecvUs, BinFrame} ->
                    case frame_seq(BinFrame) of
                        Seq ->
                            ok;
                        _ ->
                            wait_for_seq(Seq, DeadlineUs, TimeoutMs)
                    end
            after RemainingMs ->
                {error, {push_timeout, Seq, TimeoutMs}}
            end
    end.

-spec frame_seq(binary()) -> pos_integer() | undefined.
frame_seq(BinFrame) ->
    try
        Msg = jsone:decode(BinFrame),
        Payload = maps:get(<<"payload">>, Msg, #{}),
        Seq0 = maps:get(<<"seq">>, Payload, undefined),
        case Seq0 of
            Seq when is_integer(Seq), Seq > 0 ->
                Seq;
            SeqBin when is_binary(SeqBin) ->
                try binary_to_integer(SeqBin) of
                    SeqInt when is_integer(SeqInt), SeqInt > 0 -> SeqInt;
                    _ -> undefined
                catch
                    _:_ -> undefined
                end;
            _ ->
                undefined
        end
    catch
        _:_ ->
            undefined
    end.

%% ===================================================================
%% Statistics helpers
%% ===================================================================

-spec measure(pos_integer(), fun((pos_integer()) -> ok)) -> [float()].
measure(SampleSize, Fun) when SampleSize > 0, is_function(Fun, 1) ->
    measure(SampleSize, Fun, 1, []).

-spec measure(pos_integer(), fun((pos_integer()) -> ok), pos_integer(), [float()]) -> [float()].
measure(SampleSize, _Fun, N, Acc) when N > SampleSize ->
    lists:reverse(Acc);
measure(SampleSize, Fun, N, Acc) ->
    StartUs = erlang:monotonic_time(microsecond),
    ok = Fun(N),
    EndUs = erlang:monotonic_time(microsecond),
    Ms = (EndUs - StartUs) / 1000,
    measure(SampleSize, Fun, N + 1, [Ms | Acc]).

-spec summarize([float()], float()) -> map().
summarize([], _ThresholdMs) ->
    throw({benchmark_error, empty_samples});
summarize(Times, ThresholdMs) ->
    Sorted = lists:sort(Times),
    Count = length(Sorted),
    Min = lists:nth(1, Sorted),
    Max = lists:nth(Count, Sorted),
    Avg = lists:sum(Sorted) / Count,
    P50 = percentile(Sorted, 50),
    P95 = percentile(Sorted, 95),
    P99 = percentile(Sorted, 99),
    #{
        count => Count,
        min_ms => Min,
        max_ms => Max,
        avg_ms => Avg,
        p50_ms => P50,
        p95_ms => P95,
        p99_ms => P99,
        pass => (P50 < ThresholdMs)
    }.

-spec percentile([float()], pos_integer()) -> float().
percentile(Sorted, P) ->
    Count = length(Sorted),
    Pos = erlang:max(1, erlang:min(Count, trunc(math:ceil(Count * P / 100)))),
    lists:nth(Pos, Sorted).

-spec metrics_all_pass(map()) -> boolean().
metrics_all_pass(Metrics) ->
    lists:all(
        fun(Value) ->
            maps:get(pass, Value, false)
        end,
        maps:values(Metrics)
    ).

%% ===================================================================
%% Result rendering
%% ===================================================================

-spec resolve_report_path(undefined | string() | binary()) -> string().
resolve_report_path(undefined) ->
    "docs/operations/channel_ws_push_latency_baseline_" ++ date_string() ++ ".md";
resolve_report_path(Path) when is_binary(Path) ->
    binary_to_list(Path);
resolve_report_path(Path) when is_list(Path) ->
    Path.

-spec render_report(map()) -> iolist().
render_report(Result) ->
    Metrics = maps:get(metrics, Result, #{}),
    WsPush = maps:get(ws_push_latency, Metrics, #{}),
    ThresholdMs = maps:get(threshold_ms, Result, ?DEFAULT_THRESHOLD_MS),
    OverallPass = maps:get(pass, Result, false),
    GeneratedAt = maps:get(generated_at, Result, <<"">>),
    SampleSize = maps:get(sample_size, Result, 0),
    ReceiveTimeoutMs = maps:get(receive_timeout_ms, Result, ?DEFAULT_RECEIVE_TIMEOUT_MS),
    [
        "# 频道 WebSocket 推送延迟基线（", date_string(), "）\n\n",
        "## 1. 目标与口径\n",
        "- 指标：`ws_push_latency`\n",
        "- 目标：`p50 < ", fmt_float(ThresholdMs), "ms`（即中位延迟 < 1s）\n",
        "- 口径：`msg_s2c_ds:send/7` 发送后，syn 已连接会话进程收到帧消息的耗时\n\n",
        "## 2. 执行参数\n",
        "- 生成时间：`", GeneratedAt, "`\n",
        "- 样本数：`", integer_to_list(SampleSize), "`\n",
        "- 接收超时：`", integer_to_list(ReceiveTimeoutMs), "ms`\n",
        "- 阈值：`", fmt_float(ThresholdMs), "ms`\n\n",
        "## 3. 执行结果\n",
        "| 项目 | p50(ms) | p95(ms) | p99(ms) | avg(ms) | min(ms) | max(ms) | 结论 |\n",
        "|---|---:|---:|---:|---:|---:|---:|---|\n",
        render_metric_row(<<"ws_push_latency">>, WsPush),
        "\n",
        "总体结论：`", pass_to_bin(OverallPass), "`\n"
    ].

-spec render_failure_report(term()) -> iolist().
render_failure_report(Reason) ->
    [
        "# 频道 WebSocket 推送延迟基线（", date_string(), "）\n\n",
        "## 1. 执行状态\n",
        "- 状态：`BLOCKED`\n",
        "- 生成时间：`", elib_dt:now(), "`\n",
        "- 错误原因：`", format_reason(Reason), "`\n\n",
        "## 2. 指标结果\n",
        "| 项目 | p50(ms) | p95(ms) | p99(ms) | 结论 |\n",
        "|---|---:|---:|---:|---|\n",
        "| `ws_push_latency` | N/A | N/A | N/A | BLOCKED |\n\n",
        "## 3. 后续动作\n",
        "1. 修复数据库连接、登录态与 syn 会话可用性。\n",
        "2. 重新执行 `channel_ws_push_benchmark:run_and_write_report/1` 补录指标。\n"
    ].

-spec render_metric_row(binary(), map()) -> iolist().
render_metric_row(Name, Metric) ->
    [
        "| `", Name, "` | ",
        fmt_float(maps:get(p50_ms, Metric, 0.0)), " | ",
        fmt_float(maps:get(p95_ms, Metric, 0.0)), " | ",
        fmt_float(maps:get(p99_ms, Metric, 0.0)), " | ",
        fmt_float(maps:get(avg_ms, Metric, 0.0)), " | ",
        fmt_float(maps:get(min_ms, Metric, 0.0)), " | ",
        fmt_float(maps:get(max_ms, Metric, 0.0)), " | ",
        pass_to_bin(maps:get(pass, Metric, false)),
        " |\n"
    ].

-spec date_string() -> string().
date_string() ->
    {{Year, Month, Day}, _} = calendar:local_time(),
    lists:flatten(
        io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day])
    ).

-spec fmt_float(term()) -> iolist().
fmt_float(Value) when is_integer(Value) ->
    io_lib:format("~.3f", [Value * 1.0]);
fmt_float(Value) when is_float(Value) ->
    io_lib:format("~.3f", [Value]);
fmt_float(_) ->
    <<"0.000">>.

-spec pass_to_bin(boolean()) -> binary().
pass_to_bin(true) ->
    <<"PASS">>;
pass_to_bin(false) ->
    <<"FAIL">>.

-spec format_reason(term()) -> iolist().
format_reason(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Primitive normalizers
%% ===================================================================

-spec normalize_positive_int(term(), pos_integer()) -> pos_integer().
normalize_positive_int(Value, Default) ->
    Int = ec_cnv:to_integer(Value),
    case Int > 0 of
        true -> Int;
        false -> Default
    end.

-spec normalize_float(term(), float()) -> float().
normalize_float(Value, _Default) when is_float(Value), Value > 0 ->
    Value;
normalize_float(Value, _Default) when is_integer(Value), Value > 0 ->
    Value * 1.0;
normalize_float(Value, Default) ->
    Float = ec_cnv:to_float(Value),
    case Float > 0 of
        true -> Float;
        false -> Default
    end.

-spec write_report(string(), iolist()) -> ok | {error, term()}.
write_report(ReportPath, Content) ->
    case unicode:characters_to_binary(Content) of
        Bin when is_binary(Bin) ->
            file:write_file(ReportPath, Bin);
        {error, Reason, _Tail} ->
            {error, {unicode_conversion_failed, Reason}};
        {incomplete, Reason, _Tail} ->
            {error, {unicode_conversion_incomplete, Reason}}
    end.
