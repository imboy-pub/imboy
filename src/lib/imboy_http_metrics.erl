-module(imboy_http_metrics).
%%%===================================================================
%%% @doc HTTP 请求指标 / HTTP request metrics
%%%
%%% B-26：`deploy/grafana/dashboards/imboy-overview.json` 与
%%% `deploy/prometheus/rules/imboy-alerts.yml` 都引用
%%% `imboy_http_requests_total{method,status}`，但此前**没有任何代码产出它** ——
%%% 面板永久 "No data"，5xx 错误率告警永远不触发。
%%%
%%% 产出点选 cowboy 的 metrics_callback 而非自写中间件：中间件拿不到最终响应
%%% 状态码，而告警正是按 `status=~"5.."` 过滤的。
%%%
%%% 本函数跑在**每一个 HTTP 请求**的收尾路径上，因此：
%%%   - 恒 ok，任何异常自行吞掉（指标不值得掀掉一个已经完成的请求）
%%%   - 不做任何 IO / 不查库；elib_metric:increment 是 gen_server:cast，异步
%%% @end
%%%===================================================================

-export([observe/1]).

-spec observe(map()) -> ok.
observe(Metrics) when is_map(Metrics) ->
    try
        Method = normalize_method(maps:get(req, Metrics, #{})),
        Status = normalize_status(maps:get(resp_status, Metrics, undefined)),
        _ = elib_metric:increment(
            imboy_http_requests_total, 1, #{method => Method, status => Status}
        ),
        ok
    catch
        _:_ -> ok
    end;
observe(_) ->
    ok.

%% @doc 方法名做白名单归一 —— 直接透传会让攻击者用任意 method 打出无限基数的
%% 标签值，把 Prometheus 的序列数撑爆（标签基数爆炸是真实的 DoS 面）。
-spec normalize_method(map()) -> binary().
normalize_method(Req) when is_map(Req) ->
    case maps:get(method, Req, <<"OTHER">>) of
        M when
            M =:= <<"GET">>;
            M =:= <<"POST">>;
            M =:= <<"PUT">>;
            M =:= <<"DELETE">>;
            M =:= <<"PATCH">>;
            M =:= <<"HEAD">>;
            M =:= <<"OPTIONS">>
        ->
            M;
        _ ->
            <<"OTHER">>
    end;
normalize_method(_) ->
    <<"OTHER">>.

%% @doc 状态码转 binary。非整数（连接中断等 cowboy 不给状态的情形）记 "000"，
%% 不丢弃 —— 那类请求同样是流量，丢掉会让分母偏小、错误率虚高。
-spec normalize_status(term()) -> binary().
normalize_status(S) when is_integer(S), S >= 100, S =< 599 ->
    integer_to_binary(S);
normalize_status(_) ->
    <<"000">>.
