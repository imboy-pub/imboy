-module(imboy_dt).
%%%
% datetime 工具箱
%%%

-export([millisecond/0, second/0, timestamp/0]).
-export([utc_date/1]).


timestamp() ->
    os:system_time(second).

second() ->
    os:system_time(second).

%% 得到现在在制时间毫秒
millisecond() ->
    os:system_time(millisecond).

utc_date("Y-m-d\TH:i:s\Z") ->
    TS = {_, _, _Micro} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_universal_time(TS),
    Dt =
        io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
                      [Year, Month, Day, Hour, Minute, Second]),
    list_to_binary(Dt).
