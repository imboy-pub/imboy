%% IMBOY_ERLANG_QUALITY P1-03：EUnit listener 适配器（OTP29 eunit_listener 5 回调）。
%% 回调接口针对本机 OTP 29（eunit-2.11）自测；版本升级需重验（RUN10）。
%% case 时长取 handle_end Data 的 time 字段（eunit_proc wall_clock 差值，毫秒），
%% 不用消息接收时刻差（RUN09）。
-module(imboy_quality_listener).
-export([start/1, init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

start(Opts) -> eunit_listener:start(?MODULE, Opts).

init(Opts) ->
    case proplists:get_value(collector, Opts) of
        Pid when is_pid(Pid) -> Pid;
        undefined -> error(collector_required)
    end.

handle_begin(test, Data, Collector) ->
    Collector ! {test_begin, id(Data)},
    Collector;
handle_begin(group, _Data, Collector) ->
    Collector.

handle_end(test, Data, Collector) ->
    Collector !
        {test_end, id(Data), proplists:get_value(status, Data), proplists:get_value(time, Data)},
    Collector;
handle_end(group, Data, Collector) ->
    Collector ! {group_end, id(Data), proplists:get_value(time, Data)},
    Collector.

handle_cancel(Kind, Data, Collector) when Kind =:= test; Kind =:= group ->
    Collector ! {test_cancel, Kind, id(Data), proplists:get_value(reason, Data)},
    Collector.

terminate(Status, Collector) ->
    Collector ! {result, Status},
    Collector.

id(Data) -> proplists:get_value(id, Data).
