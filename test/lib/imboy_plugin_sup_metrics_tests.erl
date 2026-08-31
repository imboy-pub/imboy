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
    %% 并发套件会抢同一个命名 metric server：让位应用实例——terminate_child
    %% 不触发 imboy_sup 自动重启（gen_server:stop 会触发，自动重启与测试
    %% start_link 竞态 already_started 必红），测试结束 restart_child 复原；
    %% solo/应用未启动时为 no-op。
    Preempted =
        case erlang:whereis(elib_metric) of
            undefined ->
                false;
            _ ->
                ok = supervisor:terminate_child(imboy_sup, elib_metric),
                true
        end,
    case elib_metric:start_link() of
        {ok, Pid} ->
            {Pid, Preempted};
        {error, {already_started, _}} ->
            erlang:error({elib_metric_start_failed, preempt_failed})
    end.

cleanup_metric({Pid, Preempted}) ->
    catch gen_server:stop(Pid),
    case Preempted of
        true -> {ok, _} = supervisor:restart_child(imboy_sup, elib_metric);
        false -> ok
    end,
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
