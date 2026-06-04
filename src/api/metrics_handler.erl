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
                };
            _ ->
                #{}
        catch
            _:_:Err1 ->
                logger:warning(#{event => metrics_pool_stats_failed, error => Err1}),
                #{}
        end,
    %% WebSocket 在线用户数（syn 注册的唯一用户）
    OnlineCount =
        try syn:count(imboy) of
            Count when is_integer(Count) -> Count;
            _ -> 0
        catch
            _:_:Err2 ->
                logger:warning(#{event => metrics_syn_count_failed, error => Err2}),
                0
        end,
    %% 活跃 TCP/WebSocket 连接数（ranch listener 统计）
    WsConnections =
        try ranch:info(imboy_listener) of
            Info when is_map(Info) ->
                maps:get(active_connections, Info, 0);
            _Info when is_list(_Info) ->
                proplists:get_value(active_connections, _Info, 0);
            _ ->
                0
        catch
            _:_:Err3 ->
                logger:warning(#{event => metrics_ranch_info_failed, error => Err3}),
                0
        end,

    maps:merge(
        #{
            erlang_process_count => ProcessCount,
            erlang_memory_total_bytes => MemTotal,
            erlang_memory_processes_bytes => MemProc,
            erlang_memory_ets_bytes => MemEts,
            imboy_online_users => OnlineCount,
            ws_connections_current => WsConnections
        },
        PoolStatus
    ).

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
        fun(Name, Buckets, Acc) ->
            NameBin = metric_name(Name),
            Count = length(Buckets),
            Sum = lists:foldl(
                fun
                    (#{value := V}, S) -> S + V;
                    (_, S) -> S
                end,
                0,
                Buckets
            ),
            [
                Acc,
                <<"# TYPE ">>,
                NameBin,
                <<" summary\n">>,
                NameBin,
                <<"_count ">>,
                integer_to_binary(Count),
                <<"\n">>,
                NameBin,
                <<"_sum ">>,
                number_to_binary(Sum),
                <<"\n">>
            ]
        end,
        [],
        Histograms
    ),

    iolist_to_binary([CounterLines, HistLines]).

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
