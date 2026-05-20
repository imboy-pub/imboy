-module(channel_perf_benchmark).

-export([run/0]).
-export([run/1]).
-export([run_and_write_report/1]).

-define(DEFAULT_SAMPLE_SIZE, 50).
-define(DEFAULT_SEED_MESSAGES, 30).
-define(DEFAULT_LIMIT, 20).
-define(DEFAULT_THRESHOLD_MS, 200.0).

-spec run() -> {ok, map()} | {error, term()}.
run() ->
    run(#{}).

-spec run(map()) -> {ok, map()} | {error, term()}.
run(Opts0) when is_map(Opts0) ->
    try
        Opts = normalize_opts(Opts0),
        ensure_db_connection(),
        Ctx = prepare_context(Opts),
        SampleSize = maps:get(sample_size, Opts),
        ThresholdMs = maps:get(threshold_ms, Opts),
        Metrics = #{
            messages => summarize(
                measure(SampleSize, fun(_N) -> run_messages_case(Ctx, Opts) end),
                ThresholdMs
            ),
            publish_message => summarize(
                measure(SampleSize, fun(N) -> run_publish_case(Ctx, N) end),
                ThresholdMs
            ),
            stats => summarize(
                measure(SampleSize, fun(_N) -> run_stats_case(Ctx) end),
                ThresholdMs
            ),
            sync => summarize(
                measure(SampleSize, fun(_N) -> run_sync_case(Ctx) end),
                ThresholdMs
            )
        },
        {ok, #{
            generated_at => elib_dt:now(),
            sample_size => SampleSize,
            threshold_ms => ThresholdMs,
            channel_id => maps:get(channel_id, Ctx),
            channel_id_hash => maps:get(channel_id_bin, Ctx),
            metrics => Metrics,
            pass => metrics_all_pass(Metrics)
        }}
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

-spec run_messages_case(map(), map()) -> ok.
run_messages_case(Ctx, Opts) ->
    SubscriberUid = maps:get(subscriber_uid, Ctx),
    ChannelIdBin = maps:get(channel_id_bin, Ctx),
    Limit = maps:get(limit, Opts),
    expect_ok(channel_logic:get_messages(SubscriberUid, ChannelIdBin, 0, Limit)).

-spec run_publish_case(map(), pos_integer()) -> ok.
run_publish_case(Ctx, N) ->
    AdminUid = maps:get(admin_uid, Ctx),
    ChannelIdBin = maps:get(channel_id_bin, Ctx),
    Content = <<"perf_publish_", (integer_to_binary(N))/binary>>,
    expect_ok(channel_logic:publish_message(AdminUid, ChannelIdBin, Content, <<"text">>, #{})).

-spec run_stats_case(map()) -> ok.
run_stats_case(Ctx) ->
    ChannelIdBin = maps:get(channel_id_bin, Ctx),
    expect_ok(channel_logic:get_channel_stats(ChannelIdBin)).

-spec run_sync_case(map()) -> ok.
run_sync_case(Ctx) ->
    SubscriberUid = maps:get(subscriber_uid, Ctx),
    expect_ok(channel_logic:sync_channels(SubscriberUid, 0)).

%% ===================================================================
%% Setup helpers
%% ===================================================================

-spec normalize_opts(map()) -> map().
normalize_opts(Opts0) ->
    SampleSize0 = maps:get(sample_size, Opts0, ?DEFAULT_SAMPLE_SIZE),
    SeedMessages0 = maps:get(seed_messages, Opts0, ?DEFAULT_SEED_MESSAGES),
    Limit0 = maps:get(limit, Opts0, ?DEFAULT_LIMIT),
    Threshold0 = maps:get(threshold_ms, Opts0, ?DEFAULT_THRESHOLD_MS),
    #{
        sample_size => normalize_positive_int(SampleSize0, ?DEFAULT_SAMPLE_SIZE),
        seed_messages => normalize_non_negative_int(SeedMessages0, ?DEFAULT_SEED_MESSAGES),
        limit => normalize_positive_int(Limit0, ?DEFAULT_LIMIT),
        threshold_ms => normalize_float(Threshold0, ?DEFAULT_THRESHOLD_MS)
    }.

-spec ensure_db_connection() -> ok.
ensure_db_connection() ->
    case elib_pg:one(<<"SELECT 1 AS ok">>, []) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            throw({benchmark_error, {db_unavailable, Reason}})
    end.

-spec prepare_context(map()) -> map().
prepare_context(Opts) ->
    AdminUid = normalize_uid(integer_to_binary(elib_tsid:generate())),
    SubscriberUid = normalize_uid(integer_to_binary(elib_tsid:generate())),
    ensure_user(AdminUid, <<"ch_perf_admin">>),
    ensure_user(SubscriberUid, <<"ch_perf_subscriber">>),
    ChannelName = <<"perf_channel_", (integer_to_binary(AdminUid))/binary>>,
    ChannelOpts = #{description => <<"channel performance baseline">>},
    ChannelId = case channel_ds:create_channel(AdminUid, ChannelName, 0, ChannelOpts) of
        {ok, Id} ->
            Id;
        {error, Reason} ->
            throw({benchmark_error, {create_channel_failed, Reason}})
    end,
    expect_ok(channel_ds:subscribe(ChannelId, SubscriberUid)),
    ChannelIdBin = integer_to_binary(ChannelId),
    seed_messages(
        AdminUid,
        ChannelIdBin,
        maps:get(seed_messages, Opts)
    ),
    #{
        admin_uid => AdminUid,
        subscriber_uid => SubscriberUid,
        channel_id => ChannelId,
        channel_id_bin => ChannelIdBin
    }.

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

-spec seed_messages(integer(), binary(), non_neg_integer()) -> ok.
seed_messages(_AdminUid, _ChannelIdBin, 0) ->
    ok;
seed_messages(AdminUid, ChannelIdBin, Count) when Count > 0 ->
    lists:foreach(fun(N) ->
        Content = <<"perf_seed_", (integer_to_binary(N))/binary>>,
        expect_ok(channel_logic:publish_message(AdminUid, ChannelIdBin, Content, <<"text">>, #{}))
    end, lists:seq(1, Count)).

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
        pass => (P95 < ThresholdMs)
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
    "docs/operations/channel_performance_baseline_" ++ date_string() ++ ".md";
resolve_report_path(Path) when is_binary(Path) ->
    binary_to_list(Path);
resolve_report_path(Path) when is_list(Path) ->
    Path.

-spec render_report(map()) -> iolist().
render_report(Result) ->
    Metrics = maps:get(metrics, Result, #{}),
    Messages = maps:get(messages, Metrics, #{}),
    Publish = maps:get(publish_message, Metrics, #{}),
    Stats = maps:get(stats, Metrics, #{}),
    Sync = maps:get(sync, Metrics, #{}),
    ThresholdMs = maps:get(threshold_ms, Result, ?DEFAULT_THRESHOLD_MS),
    OverallPass = maps:get(pass, Result, false),
    GeneratedAt = maps:get(generated_at, Result, <<"">>),
    SampleSize = maps:get(sample_size, Result, 0),
    [
        "# 频道性能基线记录（", date_string(), "）\n\n",
        "## 1. 目标与口径\n",
        "- 目标接口：`messages`、`publish_message`、`stats`、`sync`\n",
        "- 目标：`p95 < ", fmt_float(ThresholdMs), "ms`（测试环境）\n\n",
        "## 2. 执行参数\n",
        "- 生成时间：`", GeneratedAt, "`\n",
        "- 样本数：`", integer_to_list(SampleSize), "`\n",
        "- 阈值：`", fmt_float(ThresholdMs), "ms`\n\n",
        "## 3. 执行结果\n",
        "| 项目 | p50(ms) | p95(ms) | p99(ms) | avg(ms) | min(ms) | max(ms) | 结论 |\n",
        "|---|---:|---:|---:|---:|---:|---:|---|\n",
        render_metric_row(<<"messages">>, Messages),
        render_metric_row(<<"publish_message">>, Publish),
        render_metric_row(<<"stats">>, Stats),
        render_metric_row(<<"sync">>, Sync),
        "\n",
        "总体结论：`", pass_to_bin(OverallPass), "`\n"
    ].

-spec render_failure_report(term()) -> iolist().
render_failure_report(Reason) ->
    [
        "# 频道性能基线记录（", date_string(), "）\n\n",
        "## 1. 执行状态\n",
        "- 状态：`BLOCKED`\n",
        "- 生成时间：`", elib_dt:now(), "`\n",
        "- 错误原因：`", format_reason(Reason), "`\n\n",
        "## 2. 指标结果\n",
        "| 项目 | p50(ms) | p95(ms) | p99(ms) | 结论 |\n",
        "|---|---:|---:|---:|---|\n",
        "| `messages` | N/A | N/A | N/A | BLOCKED |\n",
        "| `publish_message` | N/A | N/A | N/A | BLOCKED |\n",
        "| `stats` | N/A | N/A | N/A | BLOCKED |\n",
        "| `sync` | N/A | N/A | N/A | BLOCKED |\n\n",
        "## 3. 后续动作\n",
        "1. 修复数据库连接与迁移依赖（如 PostgreSQL/PostGIS）。\n",
        "2. 重新执行 `channel_perf_benchmark:run_and_write_report/1` 补录指标。\n"
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

-spec normalize_non_negative_int(term(), non_neg_integer()) -> non_neg_integer().
normalize_non_negative_int(Value, Default) ->
    Int = ec_cnv:to_integer(Value),
    case Int >= 0 of
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

-spec expect_ok(term()) -> ok.
expect_ok(ok) ->
    ok;
expect_ok({ok, _}) ->
    ok;
expect_ok({error, Reason}) ->
    throw({benchmark_error, Reason});
expect_ok(Other) ->
    throw({benchmark_error, {unexpected_result, Other}}).
