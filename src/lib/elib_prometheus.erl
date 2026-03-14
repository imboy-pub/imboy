-module(elib_prometheus).
%%% Prometheus metrics exporter module
%%% Exports elib_metric metrics to Prometheus text format
%%% Provides /metrics endpoint for Prometheus scraping

-export([scrape_handler/1]).
-export([collect_metrics/0]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc Prometheus scrape handler for Cowboy HTTP handler
-spec scrape_handler(cowboy_req:req()) -> {ok, iolist(), cowboy_req:req()}.
scrape_handler(Req) ->
    Metrics = collect_metrics(),
    Body = format_prometheus_metrics(Metrics),
    {ok, Body, Req}.

%% @doc Collect all metrics from elib_metric
-spec collect_metrics() -> [{binary(), binary(), binary(), integer() | float()}].
collect_metrics() ->
    AllMetrics = elib_metric:get_all_metrics(),

    % Collect counters
    Counters = maps:get(counters, AllMetrics, #{}),
    CounterMetrics = lists:map(fun({Name, Value}) when is_integer(Value), Value >= 0 ->
        MetricName = normalize_metric_name(Name),
        {<<"counter">>, MetricName, <<>>, Value}
    end, maps:to_list(Counters)),

    % Collect histograms
    Histograms = maps:get(histograms, AllMetrics, #{}),
    HistogramMetrics = lists:flatmap(fun({Name, Buckets}) ->
        MetricName = normalize_metric_name(Name),
        Count = lists:sum([C || #{count := C} <- Buckets]),
        Avg = case Count of
            0 -> 0;
            _ -> lists:sum([C * (Min + Max) div 2 || #{min := Min, max := Max, count := C} <- Buckets]) div Count
        end,
        [
            {<<"histogram">>, <<MetricName/binary, "_count">>, <<>>, Count},
            {<<"histogram">>, <<MetricName/binary, "_sum">>, <<>>, Avg},
            {<<"histogram">>, <<MetricName/binary, "_avg">>, <<>>, Avg}
        ]
    end, maps:to_list(Histograms)),

    CounterMetrics ++ HistogramMetrics.

%% @doc Format metrics to Prometheus text format
-spec format_prometheus_metrics([{binary(), binary(), binary(), integer() | float()}]) -> iolist().
format_prometheus_metrics(Metrics) ->
    lists:map(fun
        ({<<"counter">>, Name, <<>>, Value}) ->
            [<<"# TYPE ">>, Name, <<" counter\n">>,
             <<"# HELP ">>, Name, <<" Total count\n">>,
             Name, <<" ">>, format_val(Value), <<"\n\n">>];
        ({<<"histogram">>, Name, <<>>, Value}) ->
            [<<"# TYPE ">>, Name, <<" gauge\n">>,
             <<"# HELP ">>, Name, <<" Histogram value\n">>,
             Name, <<" ">>, format_val(Value), <<"\n\n">>];
        (_) ->
            <<>>
    end, Metrics).

%% @doc Format metric value to binary
-spec format_val(integer() | float()) -> binary().
format_val(Value) when is_integer(Value) ->
    integer_to_binary(Value);
format_val(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 2}, compact]).

%% @doc Normalize metric name to Prometheus format
-spec normalize_metric_name(atom() | list() | binary()) -> binary().
normalize_metric_name(Name) when is_atom(Name) ->
    normalize_metric_name(atom_to_list(Name));
normalize_metric_name(Name) when is_list(Name) ->
    ListName = Name,
    BinaryName = list_to_binary(ListName),
    re:replace(BinaryName, <<"[^a-z0-9_]">>, <<"_">>, [{return, binary}, global]);
normalize_metric_name(Name) when is_binary(Name) ->
    Name.
