-module(imboy_plugin_sup_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% P1-V2: 插件 supervisor 重启次数指标暴露给 metric
%%%
%%% 覆盖 / Coverage:
%%%   1. generic_sup init 记录 plugin_sup_starts 指标
%%%   2. collect_metrics 不崩溃（sup 未运行时）
%%% @end
%%%-------------------------------------------------------------------

setup_metric() ->
    %% 并发套件会抢同一个命名 metric server：stop/start 竞态下
    %% already_started 需收割重试，否则 setup 必红
    start_metric(5).

start_metric(0) ->
    erlang:error({elib_metric_start_failed, exhausted});
start_metric(N) ->
    case erlang:whereis(elib_metric) of
        undefined ->
            ok;
        P ->
            catch gen_server:stop(P),
            timer:sleep(20)
    end,
    case elib_metric:start_link() of
        {ok, Pid} ->
            Pid;
        {error, {already_started, _}} ->
            timer:sleep(50),
            start_metric(N - 1)
    end.

cleanup_metric(_Pid) ->
    catch gen_server:stop(elib_metric),
    timer:sleep(10).

%% ===================================================================
%% 1. generic_sup 启动时记录 plugin_sup_starts 指标
%% ===================================================================

sup_start_records_metric_test_() ->
    {setup, fun setup_metric/0, fun cleanup_metric/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = imboy_plugin_generic_sup:start_link(test_metrics_sup),
                #{counters := Counters} = elib_metric:get_all_metrics(),
                ?assertEqual(
                    1, maps:get({plugin_sup_starts, #{plugin => test_metrics_sup}}, Counters)
                ),
                gen_server:stop(Pid)
            end)
        ]
    end}.

%% ===================================================================
%% 2. collect_metrics 不崩溃（sup 未运行时）
%% ===================================================================

collect_metrics_no_crash_test_() ->
    ?_test(begin
        ?assertEqual(ok, imboy_plugin_sup:collect_metrics())
    end).
