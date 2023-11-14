-module(imboy_dt).
%%%
% datetime 工具箱
%%%

-export([microsecond/0,
         millisecond/0,
         second/0]).
-export([now/0, now/1]).
-export([to_rfc3339/2]).


%% 返回当前Erlang系统时间秒
second() ->
    erlang:system_time(second).


%% 返回当前Erlang系统时间毫秒
millisecond() ->
    erlang:system_time(millisecond).


%% 返回当前Erlang系统时间微秒
microsecond() ->
    erlang:system_time(microsecond).


% 默认当前时间微秒单位
now() ->
    now(microsecond).


now(second) ->
    list_to_binary(to_rfc3339(erlang:system_time(second), second));
now(millisecond) ->
    list_to_binary(to_rfc3339(erlang:system_time(millisecond), millisecond));
now(microsecond) ->
    list_to_binary(to_rfc3339(erlang:system_time(microsecond), microsecond)).


%% link https://www.erlang.org/doc/man/calendar.html#system_time_to_rfc3339-2
%
% imboy_dt:to_rfc3339(imboy_dt:second(), second).
% offset，可以是字符串或整数。默认情况下的空字符串被解释为当地时间。按原样包含非空字符串。整数的时间单位与Time 的时间单位相同。
% time_designator the date and time separator. The default is $T.
%
%%
to_rfc3339(Val, second) ->
    calendar:system_time_to_rfc3339(Val, [{unit, second}, {time_designator, $\s}, {offset, ""}]);
% imboy_dt:to_rfc3339(imboy_dt:millisecond(), millisecond).
to_rfc3339(Val, millisecond) ->
    calendar:system_time_to_rfc3339(Val, [{unit, millisecond}, {time_designator, $\s}, {offset, ""}]);
% imboy_dt:to_rfc3339(imboy_dt:microsecond(), microsecond).
to_rfc3339(Val, microsecond) ->
    calendar:system_time_to_rfc3339(Val, [{unit, microsecond}, {time_designator, $\s}, {offset, ""}]);
% Nanosecond from erlang:system_time().
% imboy_dt:to_rfc3339(erlang:system_time(), nanosecond).
to_rfc3339(Nanosecond, nanosecond) ->
    calendar:system_time_to_rfc3339(Nanosecond, [{unit, nanosecond}, {time_designator, $\s}, {offset, ""}]).

% https://www.erlang.org/doc/man/calendar#rfc3339_to_system_time-1
% https://www.erlang.org/doc/man/calendar#rfc3339_to_system_time-2
% 1> calendar:rfc3339_to_system_time("2018-02-01T16:17:58+01:00").
% 1517498278
% 2> calendar:rfc3339_to_system_time("2018-02-01 15:18:02.088Z",
%    [{unit, nanosecond}]).
% 1517498282088000000

% erlang:system_time() 返回native时间单位的虚拟机时间erlang system time，虚拟机时间由两部分构成：time_offset和monotonic_time。
% erlang:system_time() 等价于 erlang:monotonic_time() + erlang:time_offset()

% https://wudeng.github.io/2018/03/29/erlang-time/
% UT1：世界时
% UTC：Coordinated Universal Time，协调世界时，对秒的定义跟UT1有差异，包含闰秒。UTC的一天可能为86399, 86400, 86401秒。
% POSIX Time(aka Unix/Epoch time): Time since EPOCH (UTC 1970-01-01 00:00:00)，POSIX Time的一天刚好为86400秒。奇怪的是EPOCH被定义为UTC时间。
% OS System Time：操作系统视角的POSIX time。存在时间跳跃。
% Erlang System Time: Erlang运行时视角的POSIX time。跟操作系统时间可能有偏差。
% Erlang monotonic time: events, timers, time interval, 单调，但是不严格单调递增。
% Time offset: 通过时间偏移来同步操作系统时间，无需修改单调时间的频率。
