-module(metrics_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

init_returns_metrics_payload_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'peer', 1, fun(_Req) -> {{127, 0, 0, 1}, 12345} end},
                {'header', 3, fun(_Name, _Req, _Default) -> <<>> end}
            ]},
            {elib_metric, [
                {'get_all_metrics', 0, fun() ->
                    #{connections => 2, queue_depth => 5}
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, Msg) ->
                    Req#{response_status => 200, payload => Payload, msg => Msg}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, State} = metrics_handler:init(#{}, #{}),
            ?assertEqual(#{}, State),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(2, maps:get(connections, maps:get(payload, RespReq))),
            ?assertEqual("success.", maps:get(msg, RespReq))
        end
    ).

init_returns_error_when_metric_fetch_fails_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'peer', 1, fun(_Req) -> {{127, 0, 0, 1}, 12345} end},
                {'header', 3, fun(_Name, _Req, _Default) -> <<>> end}
            ]},
            {elib_metric, [
                {'get_all_metrics', 0, fun() ->
                    erlang:error(metric_down)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(Req, Msg) ->
                    Req#{response_status => 400, error_msg => Msg}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = metrics_handler:init(#{}, #{}),
            ?assertEqual(400, maps:get(response_status, RespReq)),
            Msg = maps:get(error_msg, RespReq),
            ?assert(binary:match(Msg, <<"metrics unavailable">>) =/= nomatch)
        end
    ).

%% ===================================================================
%% B-27：Prometheus histogram 导出格式
%%
%% histogram_quantile() 的输入是 `_bucket{le="..."}` **累积**序列，且必须有
%% le="+Inf" 那一行。此前导出的是 `# TYPE ... summary` + 只有 _sum/_count，
%% 分位数面板在数据模型层就不可能有数据。
%% ===================================================================

histogram_exports_cumulative_buckets_test() ->
    Hist = #{
        counts => #{0.005 => 2, 0.25 => 3, infinity => 1},
        sum => 31.0,
        count => 6
    },
    Body = iolist_to_binary(
        metrics_handler:format_prometheus(#{counters => #{}, histograms => #{lat_seconds => Hist}})
    ),
    %% 类型必须是 histogram 而不是 summary —— summary 没有 le 维度
    ?assert(binary:match(Body, <<"# TYPE lat_seconds histogram">>) =/= nomatch),
    %% 累积：le=0.005 是 2；到 le=0.25 时应为 2+3=5（而不是 3）
    ?assert(binary:match(Body, <<"lat_seconds_bucket{le=\"0.005\"} 2">>) =/= nomatch),
    ?assert(binary:match(Body, <<"lat_seconds_bucket{le=\"0.25\"} 5">>) =/= nomatch),
    %% +Inf 必须存在且等于总观测数，否则 Prometheus 认为序列不完整
    ?assert(binary:match(Body, <<"lat_seconds_bucket{le=\"+Inf\"} 6">>) =/= nomatch),
    ?assert(binary:match(Body, <<"lat_seconds_count 6">>) =/= nomatch),
    ?assert(binary:match(Body, <<"lat_seconds_sum ">>) =/= nomatch).

%% 升级瞬间 ETS 里可能还留着旧结构：宁可这一轮没数据，也不导出会让分位数算错的半截桶
histogram_legacy_shape_is_not_exported_test() ->
    Body = iolist_to_binary(
        metrics_handler:format_prometheus(#{
            counters => #{}, histograms => #{old_seconds => [#{value => 1}]}
        })
    ),
    ?assertEqual(nomatch, binary:match(Body, <<"old_seconds_bucket">>)).
