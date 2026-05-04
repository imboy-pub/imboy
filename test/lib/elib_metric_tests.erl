-module(elib_metric_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% elib_metric 的 EUnit 测试（P6-T2: plugin label 支持）
%%%
%%% 覆盖 / Coverage:
%%%   1. increment/1,2: 无标签计数器（向后兼容）
%%%   2. increment/3: 带标签计数器
%%%   3. 同名指标不同标签组合
%%%   4. 无标签和有标签同名指标互不干扰
%%%   5. Prometheus 格式输出包含标签
%%%   6. increment/3 Delta 校验
%%% @end
%%%-------------------------------------------------------------------

%% 启动/停止 elib_metric gen_server
setup_metric() ->
    catch gen_server:stop(elib_metric),
    timer:sleep(10),
    {ok, Pid} = elib_metric:start_link(),
    Pid.

cleanup_metric(_Pid) ->
    catch gen_server:stop(elib_metric),
    timer:sleep(10).

%% ===================================================================
%% 1. 无标签计数器（向后兼容）
%% ===================================================================

metric_increment_no_labels_test_() ->
    {setup,
     fun setup_metric/0,
     fun cleanup_metric/1,
     fun(_) ->
         [
             ?_test(begin
                 ok = elib_metric:increment(requests),
                 ok = elib_metric:increment(requests),
                 ok = elib_metric:increment(requests, 7),
                 #{counters := Counters} = elib_metric:get_all_metrics(),
                 ?assertEqual(9, maps:get(requests, Counters))
             end)
         ]
     end}.

%% ===================================================================
%% 2. 带标签计数器
%% ===================================================================

metric_increment_with_labels_test_() ->
    {setup,
     fun setup_metric/0,
     fun cleanup_metric/1,
     fun(_) ->
         [
             ?_test(begin
                 ok = elib_metric:increment(msg_sent, 1, #{plugin => channel}),
                 ok = elib_metric:increment(msg_sent, 1, #{plugin => channel}),
                 ok = elib_metric:increment(msg_sent, 3, #{plugin => moment}),
                 #{counters := Counters} = elib_metric:get_all_metrics(),
                 ChannelKey = {msg_sent, #{plugin => channel}},
                 MomentKey = {msg_sent, #{plugin => moment}},
                 ?assertEqual(2, maps:get(ChannelKey, Counters)),
                 ?assertEqual(3, maps:get(MomentKey, Counters))
             end)
         ]
     end}.

%% ===================================================================
%% 3. 同名指标不同标签组合
%% ===================================================================

metric_same_name_different_labels_test_() ->
    {setup,
     fun setup_metric/0,
     fun cleanup_metric/1,
     fun(_) ->
         [
             ?_test(begin
                 ok = elib_metric:increment(http_requests, 1, #{plugin => channel, method => get}),
                 ok = elib_metric:increment(http_requests, 1, #{plugin => channel, method => post}),
                 ok = elib_metric:increment(http_requests, 1, #{plugin => moment}),
                 #{counters := Counters} = elib_metric:get_all_metrics(),
                 Labels1 = #{plugin => channel, method => get},
                 Labels2 = #{plugin => channel, method => post},
                 Labels3 = #{plugin => moment},
                 ?assertEqual(1, maps:get({http_requests, Labels1}, Counters)),
                 ?assertEqual(1, maps:get({http_requests, Labels2}, Counters)),
                 ?assertEqual(1, maps:get({http_requests, Labels3}, Counters))
             end)
         ]
     end}.

%% ===================================================================
%% 4. 无标签和有标签同名指标互不干扰
%% ===================================================================

metric_labeled_and_unlabeled_coexist_test_() ->
    {setup,
     fun setup_metric/0,
     fun cleanup_metric/1,
     fun(_) ->
         [
             ?_test(begin
                 ok = elib_metric:increment(msg_total, 5),
                 ok = elib_metric:increment(msg_total, 3, #{plugin => channel}),
                 #{counters := Counters} = elib_metric:get_all_metrics(),
                 ?assertEqual(5, maps:get(msg_total, Counters)),
                 ?assertEqual(3, maps:get({msg_total, #{plugin => channel}}, Counters))
             end)
         ]
     end}.

%% ===================================================================
%% 5. Prometheus 格式输出包含标签
%% ===================================================================

metric_prometheus_format_labels_test_() ->
    {setup,
     fun setup_metric/0,
     fun cleanup_metric/1,
     fun(_) ->
         [
             ?_test(begin
                 ok = elib_metric:increment(msg_sent, 1, #{plugin => channel}),
                 ok = elib_metric:increment(msg_sent, 2, #{plugin => moment}),
                 ok = elib_metric:increment(requests),
                 Metrics = elib_metric:get_all_metrics(),
                 Body = metrics_handler:format_prometheus(Metrics),
                 BodyBin = iolist_to_binary(Body),
                 BodyStr = binary_to_list(BodyBin),
                 ?assert(string:find(BodyStr, "plugin=") =/= nomatch),
                 ?assert(string:find(BodyStr, "requests") =/= nomatch)
             end)
         ]
     end}.

%% ===================================================================
%% 6. increment/3 Delta 校验
%% ===================================================================

metric_increment_labels_validation_test_() ->
    {setup,
     fun setup_metric/0,
     fun cleanup_metric/1,
     fun(_) ->
         [
             ?_test(begin
                 Result = elib_metric:increment(test_zero, 0, #{plugin => x}),
                 ?assertEqual({error, invalid_delta}, Result)
             end),
             ?_test(begin
                 Result2 = elib_metric:increment(test_neg, -1, #{plugin => x}),
                 ?assertEqual({error, invalid_delta}, Result2)
             end)
         ]
     end}.
