-module(metrics_handler).

-behavior(cowboy_rest).

-export([init/2]).
-export([format_prometheus/1]).
-export([is_internal_ip/1]).

%% @doc Metrics endpoint for runtime observability.
%% 支持两种格式：
%% - Accept: text/plain → Prometheus text exposition format
%% - 默认 → JSON 格式
%% 安全：仅允许内网 IP 访问（loopback / RFC-1918 私有网段）
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    {PeerIp, _Port} = cowboy_req:peer(Req0),
    case is_internal_ip(PeerIp) of
        false ->
            Req1 = cowboy_req:reply(403, #{}, <<>>, Req0),
            {ok, Req1, State0};
        true ->
            serve_metrics(Req0, State0)
    end.

-spec serve_metrics(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
serve_metrics(Req0, State0) ->
    Accept = cowboy_req:header(<<"accept">>, Req0, <<>>),
    Req1 =
        case binary:match(Accept, <<"text/plain">>) of
            nomatch ->
                %% JSON 格式
                case fetch_metrics() of
                    {ok, Metrics} ->
                        elib_response:success(Req0, Metrics, "success.");
                    {error, Reason} ->
                        elib_response:error(Req0, format_error(Reason))
                end;
            _ ->
                %% Prometheus text 格式
                case fetch_metrics() of
                    {ok, Metrics} ->
                        Body = format_prometheus(Metrics),
                        cowboy_req:reply(
                            200,
                            #{
                                <<"content-type">> => <<"text/plain; version=0.0.4; charset=utf-8">>
                            },
                            Body,
                            Req0
                        );
                    {error, Reason} ->
                        cowboy_req:reply(500, #{}, format_error(Reason), Req0)
                end
        end,
    {ok, Req1, State0}.

%% @doc 判断是否为内网 IP（loopback + RFC-1918 私有网段）
-spec is_internal_ip(inet:ip_address()) -> boolean().
is_internal_ip({127, _, _, _}) -> true;
is_internal_ip({10, _, _, _}) -> true;
is_internal_ip({172, N, _, _}) when N >= 16, N =< 31 -> true;
is_internal_ip({192, 168, _, _}) -> true;
% IPv6 loopback ::1
is_internal_ip({0, 0, 0, 0, 0, 0, 0, 1}) -> true;
is_internal_ip(_) -> false.

-spec fetch_metrics() -> {ok, map()} | {error, term()}.
fetch_metrics() ->
    try
        AppMetrics = elib_metric:get_all_metrics(),
        SystemMetrics = collect_system_metrics(),
        Counters = maps:get(counters, AppMetrics, #{}),
        MergedCounters = maps:merge(Counters, SystemMetrics),
        {ok, AppMetrics#{counters => MergedCounters}}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @doc 收集系统级指标
-spec collect_system_metrics() -> map().
collect_system_metrics() ->
    %% 进程数
    ProcessCount = erlang:system_info(process_count),
    %% 内存（字节）
    [{total, MemTotal}, {processes, MemProc}, {ets, MemEts}] =
        [lists:keyfind(K, 1, erlang:memory()) || K <- [total, processes, ets]],
    %% 连接池状态
    PoolStatus =
        try pooler:pool_stats(pgsql) of
            Stats when is_list(Stats) ->
                #{
                    db_pool_free => proplists:get_value(free_count, Stats, 0),
                    db_pool_in_use => proplists:get_value(in_use_count, Stats, 0)
                }
        catch
            _:_:Err1 ->
                logger:warning(#{event => metrics_pool_stats_failed, error => Err1}),
                #{}
        end,
    %% WebSocket 在线用户数（syn 注册的唯一用户）
    OnlineCount =
        try syn:registry_count(imboy) of
            Count when is_integer(Count) -> Count
        catch
            _:_:Err2 ->
                logger:warning(#{event => metrics_syn_count_failed, error => Err2}),
                0
        end,
    %% 活跃 TCP/WebSocket 连接数（ranch listener 统计）
    WsConnections =
        try ranch:info(imboy_listener) of
            Info when is_map(Info) ->
                maps:get(active_connections, Info, 0)
        catch
            _:_:Err3 ->
                logger:warning(#{event => metrics_ranch_info_failed, error => Err3}),
                0
        end,

    %% License 指标（fail-safe，避免 metrics 因 License 异常不可用）
    LicenseMetrics = license_metrics(),
    %% B-26 指标名对账：下列名字**逐字**对齐 deploy/grafana/dashboards/imboy-overview.json
    %% 与 deploy/prometheus/rules/imboy-alerts.yml。此前导出的是
    %% erlang_process_count / erlang_memory_total_bytes / ws_connections_current，
    %% 而面板和告警引用的是 erlang_vm_* / imboy_ws_connections_total —— 名字对不上，
    %% 面板永远 "No data"、告警永远不触发。改名而不是留双份：旧名零消费者
    %% （deploy/ 下 0 命中），留着只会让下一个人再对一次账。
    %% 改这里之前先 grep deploy/ 确认新名字确实是那边引用的那个。
    maps:merge(
        maps:merge(
            #{
                erlang_vm_process_count => ProcessCount,
                erlang_vm_port_count => erlang:system_info(port_count),
                %% kind 标签对应面板的 legendFormat "{{kind}}"
                {erlang_vm_memory_bytes_total, #{kind => total}} => MemTotal,
                {erlang_vm_memory_bytes_total, #{kind => processes}} => MemProc,
                {erlang_vm_memory_bytes_total, #{kind => ets}} => MemEts,
                process_uptime_seconds => uptime_seconds(),
                imboy_online_users => OnlineCount,
                imboy_ws_connections_total => WsConnections
            },
            PoolStatus
        ),
        LicenseMetrics
    ).

%% @doc 节点运行时长（秒）。告警 IMBoyBackendRestarted 用它判断"刚重启"，
%% 面板也直接展示；此前根本没导出，那条告警从来不会响。
-spec uptime_seconds() -> non_neg_integer().
uptime_seconds() ->
    {Ms, _} = erlang:statistics(wall_clock),
    Ms div 1000.

-spec license_metrics() -> map().
license_metrics() ->
    try imboy_license:info() of
        Info ->
            Valid =
                case maps:get(valid, Info, false) of
                    true -> 1;
                    false -> 0
                end,
            CurUsers =
                try
                    user_ds:count()
                catch
                    _:_ -> 0
                end,
            #{
                imboy_license_valid => Valid,
                imboy_license_users_current => CurUsers,
                imboy_license_users_max => maps:get(max_users, Info, 0),
                imboy_license_nodes_current => length(nodes()) + 1,
                imboy_license_nodes_max => maps:get(max_nodes, Info, 0),
                imboy_license_expires_at => maps:get(expires_at, Info, 0)
            }
    catch
        _:_ -> #{}
    end.

%% @doc 将指标格式化为 Prometheus text exposition format
%% 支持带标签的计数器：metric_name{plugin="channel"} 42
-spec format_prometheus(map()) -> iodata().
format_prometheus(Metrics) ->
    Counters = maps:get(counters, Metrics, #{}),
    Histograms = maps:get(histograms, Metrics, #{}),

    %% 按 metric name 分组带标签的计数器，用于合并 TYPE 声明
    CounterLines = maps:fold(
        fun
            ({Name, Labels}, Value, Acc) when is_map(Labels) ->
                %% 带标签的计数器: {Name, #{plugin => channel}} => metric_name{plugin="channel"}
                NameBin = metric_name(Name),
                LabelsBin = format_labels_map(Labels),
                [
                    Acc,
                    NameBin,
                    <<"{">>,
                    LabelsBin,
                    <<"} ">>,
                    integer_to_binary(Value),
                    <<"\n">>
                ];
            (Name, Value, Acc) ->
                NameBin = metric_name(Name),
                [
                    Acc,
                    <<"# TYPE ">>,
                    NameBin,
                    <<" gauge\n">>,
                    NameBin,
                    <<" ">>,
                    integer_to_binary(Value),
                    <<"\n">>
                ]
        end,
        [],
        Counters
    ),

    HistLines = maps:fold(
        fun(Name, Hist, Acc) -> [Acc, format_histogram(metric_name(Name), Hist)] end,
        [],
        Histograms
    ),

    iolist_to_binary([CounterLines, HistLines]).

%% @doc 导出 Prometheus histogram：`_bucket{le="..."}` 累积序列 + `_sum` + `_count`。
%%
%% B-27：此前导出的是 `# TYPE ... summary` 且只有 _sum/_count，**没有任何
%% _bucket 序列** —— `histogram_quantile()` 需要的输入根本不存在，
%% p50/p95/p99 面板在数据模型层就不可能算出来，不是 Grafana 配错了。
%%
%% 注意 _bucket 必须是**累积**的（le=0.1 的值包含所有 <=0.1 的观测），
%% 且必须有 le="+Inf" 那一行，否则 Prometheus 认为序列不完整。
-spec format_histogram(binary(), map()) -> iodata().
format_histogram(NameBin, #{counts := Counts, sum := Sum, count := Count}) ->
    Bounds = elib_metric:bucket_bounds(),
    {Lines, _} = lists:mapfoldl(
        fun(B, Acc0) ->
            Acc1 = Acc0 + maps:get(B, Counts, 0),
            {bucket_line(NameBin, bound_to_binary(B), Acc1), Acc1}
        end,
        0,
        Bounds
    ),
    [
        <<"# TYPE ">>,
        NameBin,
        <<" histogram\n">>,
        Lines,
        %% +Inf 桶等于总观测数（含超出最大边界的 infinity 那部分）
        bucket_line(NameBin, <<"+Inf">>, Count),
        NameBin,
        <<"_sum ">>,
        number_to_binary(Sum),
        <<"\n">>,
        NameBin,
        <<"_count ">>,
        integer_to_binary(Count),
        <<"\n">>
    ];
format_histogram(NameBin, _Other) ->
    %% 兼容旧形态（升级瞬间 ETS 里可能还留着老结构）：不导出残缺序列，
    %% 宁可这一轮没数据，也不要导出会让分位数算错的半截桶。
    [<<"# TYPE ">>, NameBin, <<" histogram\n">>].

-spec bucket_line(binary(), binary(), non_neg_integer()) -> iodata().
bucket_line(NameBin, Le, Cumulative) ->
    [
        NameBin,
        <<"_bucket{le=\"">>,
        Le,
        <<"\"} ">>,
        integer_to_binary(Cumulative),
        <<"\n">>
    ].

%% @doc 桶边界必须**原值**输出，不能走 number_to_binary（它只留 2 位小数）：
%% 那样 0.005 与 0.01 会双双渲染成 "0.01"（同一个 le 出现两次 = 无效的
%% Prometheus 输出），0.025 会变成 "0.03"（边界被改写，分位数插值随之错位）。
%% float_to_binary(short) 给最短往返表示：0.005 → "0.005"，2.5 → "2.5"。
-spec bound_to_binary(number()) -> binary().
bound_to_binary(B) when is_integer(B) -> integer_to_binary(B);
bound_to_binary(B) when is_float(B) -> float_to_binary(B, [short]);
bound_to_binary(B) -> number_to_binary(B).

%% @doc 格式化标签 map 为 Prometheus label 格式: plugin="channel",method="get"
-spec format_labels_map(map()) -> iodata().
format_labels_map(Labels) ->
    SortedPairs = lists:sort(maps:to_list(Labels)),
    Parts = [
        [
            atom_to_binary(K, utf8),
            <<"=\"">>,
            format_label_value(V),
            <<"\"">>
        ]
     || {K, V} <- SortedPairs
    ],
    lists:join(<<",">>, Parts).

-spec format_label_value(term()) -> binary().
format_label_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
format_label_value(V) when is_binary(V) -> V;
format_label_value(V) when is_integer(V) -> integer_to_binary(V);
format_label_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).

%% @doc 将指标名转为合法的 Prometheus metric name
-spec metric_name(atom() | tuple()) -> binary().
metric_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
metric_name(Name) when is_tuple(Name) ->
    Parts = [atom_to_list(E) || E <- tuple_to_list(Name), is_atom(E)],
    list_to_binary(string:join(Parts, "_"));
metric_name(Name) ->
    iolist_to_binary(io_lib:format("~p", [Name])).

-spec number_to_binary(number()) -> binary().
number_to_binary(N) when is_integer(N) -> integer_to_binary(N);
number_to_binary(N) when is_float(N) -> float_to_binary(N, [{decimals, 2}]).

-spec format_error(term()) -> binary().
format_error(Reason) ->
    unicode:characters_to_binary(io_lib:format("metrics unavailable: ~p", [Reason])).
