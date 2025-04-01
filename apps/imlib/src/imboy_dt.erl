-module(imboy_dt).
%%%
% datetime 工具箱
%%%

-export([microsecond/0,
         millisecond/0,
         second/0]).
-export([utc/1]).

-export([add/2, minus/2]).
-export([compare_rfc3339/3]).
-export([now/0, now/1]).
-export([to_rfc3339/2, to_rfc3339/3]).
-export([rfc3339_to/2]).
-export([timezone_offset/1]).

% imboy_dt:add(Dt, {10, minute}).
% imboy_dt:add(Dt, {600, second}).
add(Dt, {Num, minute}) ->
    add(Dt, {Num * 60, second});
add(Dt, {Num, second}) ->
    add(Dt, {Num * 1000, millisecond});
add(Dt, {Num, millisecond}) ->
    S = rfc3339_to(Dt, microsecond),
    Val = S + Num*1000,
    list_to_binary(to_rfc3339(Val, microsecond)).

minus(Dt, {Num, minute}) ->
    minus(Dt, {Num * 60, second});
minus(Dt, {Num, second}) ->
    S = rfc3339_to(Dt, microsecond),
    Val = S - Num*1000000,
    list_to_binary(to_rfc3339(Val, microsecond)).

% Dt = imboy_dt:now().
% Dt2 = imboy_dt:add(Dt, {10, minute}).
% imboy_dt:compare_rfc3339(Dt, Dt2, gt).
compare_rfc3339(A, B, Opt) ->
    A1 = rfc3339_to(A, microsecond),
    B1 = rfc3339_to(B, microsecond),
    case Opt of
        eq ->
            A1 == B1;
        gt ->
            A1 > B1;
        egt ->
            A1 >= B1;
        lt ->
            A1 < B1;
        elt ->
            A1 =< B1
    end.

%% 返回的时间为 UTC 时间戳，与操作系统时区设置无关。
utc(millisecond) ->
    erlang:system_time(millisecond);
utc(second) ->
    erlang:system_time(second).

%% 获取系统当前时区 （单位：秒）
%% imboy_dt:timezone_offset(second).
timezone_offset(second) ->
    %% 获取当前的UTC时间
    UtcTime = calendar:universal_time(),
    %% 将UTC时间转换为本地时间
    LocalTime = calendar:local_time(),
    %% 计算时差（单位为秒）
    {Day,  T1} = calendar:time_difference(UtcTime, LocalTime),
    TimeDiffSeconds = calendar:time_to_seconds(T1) + Day * 86400,
    % TimeZoneOffset = -TimeDiffSeconds * 1000, % 计算与UTC相对于东西经180度的时区偏移量（单位：毫秒）
    TimeDiffSeconds.

%% 返回的时间为 UTC 时间戳，与操作系统时区设置无关。
second() ->
    erlang:system_time(second).

%% 返回毫秒
%% 返回的时间为 UTC 时间戳，与操作系统时区设置无关。
%% https://currentmillis.com/
%% Methods to get the time in milliseconds since the UNIX epoch (January 1, 1970 00:00:00 UTC) in various programming languages
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
% imboy_dt:to_rfc3339(imboy_dt:millisecond(), millisecond).
% offset，可以是字符串或整数。默认情况下的空字符串被解释为当地时间。按原样包含非空字符串。整数的时间单位与Time 的时间单位相同。
% time_designator the date and time separator. The default is $T.
%
%%
to_rfc3339(Num, second) ->
    to_rfc3339(Num, second, "");
% imboy_dt:to_rfc3339(imboy_dt:millisecond(), millisecond).
to_rfc3339(Num, millisecond) ->
    to_rfc3339(Num, millisecond, "");
% imboy_dt:to_rfc3339(imboy_dt:microsecond(), microsecond).
to_rfc3339(Num, microsecond) ->
    to_rfc3339(Num, microsecond, "");
to_rfc3339(Num, nanosecond) ->
    to_rfc3339(Num, nanosecond, "").

% imboy_dt:to_rfc3339(1707198019, second, "+08:00").
% imboy_dt:to_rfc3339(imboy_dt:utc(second), second, "+08:00").
to_rfc3339(Num, Unit, Offset) ->
    calendar:system_time_to_rfc3339(Num, [{unit, Unit}, {time_designator, $\s}, {offset, Offset}]).

% Dt = imboy_dt:now(),
% imboy_dt:rfc3339_to(Dt, millisecond).
% imboy_dt:rfc3339_to(Dt, microsecond).
% imboy_dt:rfc3339_to({{2024,10,29},{2,34,30.776}}, millisecond).
rfc3339_to({{Y,Mo,D}, {H,Mi,S}}, Unit) when is_number(S) ->
    % Handle Erlang datetime tuple like {{2024,10,29},{2,34,30.776}}
    try
        % 分离整数秒和小数秒
        IntS = trunc(S),
        FracS = S - IntS,
        % 计算整数秒部分的时间戳
        GregorianSecs = calendar:datetime_to_gregorian_seconds({{Y,Mo,D}, {H,Mi,IntS}}),
        UnixEpochSecs = GregorianSecs - 62167219200,  % 从1970-01-01开始计算
        % 根据单位处理小数部分
        case Unit of
            second ->
                UnixEpochSecs + FracS;
            millisecond ->
                UnixEpochSecs * 1000 + round(FracS * 1000);
            microsecond ->
                UnixEpochSecs * 1000000 + round(FracS * 1000000);
            nanosecond ->
                UnixEpochSecs * 1000000000 + round(FracS * 1000000000);
            _ ->
                {error, "unsupported unit"}
        end
    catch
        _:_ -> {error, "invalid datetime tuple"}
    end;
rfc3339_to(Dt, Unit) when is_binary(Dt) ->
    rfc3339_to(binary_to_list(Dt), Unit);
rfc3339_to(Dt, Unit) ->
    try
        Offset = lists:sublist(Dt, length(Dt) - 5, 6),
        calendar:rfc3339_to_system_time(Dt, [{unit, Unit}, {time_designator, $\s}, {offset, Offset}])
    of
        Num ->
            Num
    catch
        _:_ ->
            {error, "时间格式有误"}
    end.

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
