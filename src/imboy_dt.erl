-module(imboy_dt).
%%%
% datetime 工具箱
%%%

-export([microsecond/0, millisecond/0, second/0]).
-export([utc_date/1]).
-export([to_rfc3339/1]).


%% 返回当前Erlang系统时间秒
second() ->
    os:system_time(second).

%% 返回当前Erlang系统时间毫秒
millisecond() ->
    os:system_time(millisecond).

%% 返回当前Erlang系统时间微秒
microsecond() ->
    os:system_time(microsecond).

utc_date("Y-m-d\TH:i:s\Z") ->
    TS = {_, _, _Micro} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_universal_time(TS),
    Dt =
        io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
                      [Year, Month, Day, Hour, Minute, Second]),
    list_to_binary(Dt).

% Nanosecond from erlang:system_time().
% link https://www.erlang.org/doc/man/calendar.html#system_time_to_rfc3339-2
to_rfc3339(Nanosecond) ->
    calendar:system_time_to_rfc3339(
        Nanosecond
        , [{unit, nanosecond}, {time_designator, $\s}, {offset, ""}]
    ).
