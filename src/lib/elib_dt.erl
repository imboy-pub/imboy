-module(elib_dt).
-dialyzer({nowarn_function, [add/2, minus/2, rfc3339_to/1, rfc3339_to/2]}).
%%%
% datetime 工具箱
%%%

-export([
    microsecond/0,
    millisecond/0,
    second/0,
    timestamp/0
]).
-export([utc/1]).

-export([add/2, minus/2]).
-export([compare_rfc3339/3]).
-export([now/0, now/1]).
-export([to_rfc3339/1, to_rfc3339/2, to_rfc3339/3]).
-export([rfc3339_to/1, rfc3339_to/2]).
%% datetime_to/2 是内部函数，不导出

%% @doc Add time duration to a datetime value
%% @param Dt Datetime value (timestamp or RFC3339 string)
%% @param Duration {Num, Unit} where Unit is minute | second | millisecond
%% @returns New datetime value as RFC3339 binary
-spec add(integer() | binary(), {pos_integer(), minute | second | millisecond}) ->
    binary() | {error, invalid_datetime}.
% elib_dt:add(Dt, {10, minute}).
% elib_dt:add(Dt, {600, second}).
add(Dt, {Num, minute}) ->
    add(Dt, {Num * 60, second});
add(Dt, {Num, second}) ->
    add(Dt, {Num * 1000, millisecond});
add(Dt, {Num, millisecond}) when is_binary(Dt); is_integer(Dt) ->
    Result = rfc3339_to(Dt, microsecond),
    if
        is_integer(Result) ->
            Val = Result + Num * 1000,
            to_rfc3339(Val, microsecond);
        true ->
            {error, invalid_datetime}
    end.

%% @doc Subtract time duration from a datetime value
%% @param Dt Datetime value (timestamp or RFC3339 string)
%% @param Duration {Num, Unit} where Unit is minute | second | millisecond
%% @returns New datetime value as RFC3339 binary
-spec minus(integer() | binary(), {pos_integer(), minute | second | millisecond}) ->
    binary() | {error, invalid_datetime}.
minus(Dt, {Num, minute}) ->
    minus(Dt, {Num * 60, second});
minus(Dt, {Num, second}) ->
    minus(Dt, {Num * 1000, millisecond});
minus(Dt, {Num, millisecond}) when is_binary(Dt); is_integer(Dt) ->
    Result = rfc3339_to(Dt, microsecond),
    if
        is_integer(Result) ->
            Val = Result - Num * 1000,
            to_rfc3339(Val, microsecond);
        true ->
            {error, invalid_datetime}
    end.

% Dt = elib_dt:now().
% Dt2 = elib_dt:add(Dt, {10, minute}).
% Dt = elib_dt:now().
% Dt2 = elib_dt:add(Dt, {10, minute}).
% elib_dt:compare_rfc3339(Dt, Dt2, gt).
-spec compare_rfc3339(binary() | integer(), binary() | integer(), atom()) -> boolean().
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
-spec utc(millisecond | second) -> integer().
utc(millisecond) ->
    erlang:system_time(millisecond);
utc(second) ->
    erlang:system_time(second).

%% 返回的时间为 UTC 时间戳，与操作系统时区设置无关。
-spec second() -> integer().
second() ->
    erlang:system_time(second).

%% 兼容旧接口，返回毫秒级 Unix 时间戳。
-spec timestamp() -> integer().
timestamp() ->
    millisecond().

%% 返回毫秒
%% 返回的时间为 UTC 时间戳，与操作系统时区设置无关。
%% https://currentmillis.com/
%% Methods to get the time in milliseconds since the UNIX epoch (January 1, 1970 00:00:00 UTC) in various programming languages
-spec millisecond() -> integer().
millisecond() ->
    erlang:system_time(millisecond).

%% 返回当前Erlang系统时间微秒
-spec microsecond() -> integer().
microsecond() ->
    erlang:system_time(microsecond).

% 默认当前时间微秒单位
-spec now() -> binary().
now() ->
    now(microsecond).

-spec now(second | millisecond | microsecond) -> binary().
now(second) ->
    to_rfc3339(erlang:system_time(second), second);
now(millisecond) ->
    to_rfc3339(erlang:system_time(millisecond), millisecond);
now(microsecond) ->
    to_rfc3339(erlang:system_time(microsecond), microsecond).

%%%===================================================================
%%% @doc 将整数时间戳（秒/毫秒/微秒/纳秒）自动转换为 RFC3339 binary
%%%===================================================================
-spec to_rfc3339(integer() | binary()) -> binary().
to_rfc3339(Num) when is_integer(Num) ->
    %% 转成字符串判断长度
    Len = length(integer_to_list(Num)),
    case Len of
        L when L =< 10 ->
            %% 秒级时间戳
            to_rfc3339(Num, second);
        L when L =< 13 ->
            %% 毫秒级
            to_rfc3339(Num, millisecond);
        L when L =< 16 ->
            %% 微秒级
            to_rfc3339(Num, microsecond);
        _ ->
            %% 超长数字默认按纳秒处理
            to_rfc3339(Num, nanosecond)
    end;
to_rfc3339(Bin) when is_binary(Bin) ->
    case Bin of
        <<>> ->
            Ts = now(microsecond),
            to_rfc3339(Ts);
        _ ->
            case elib_type:is_numeric(Bin) of
                true ->
                    to_rfc3339(binary_to_integer(Bin));
                % 假设已经是 RFC3339 格式
                false ->
                    Bin
            end
    end.

%% link https://www.erlang.org/docs/man/calendar.html#system_time_to_rfc3339-2
%
% elib_dt:to_rfc3339(elib_dt:second(), second).
% elib_dt:to_rfc3339(elib_dt:millisecond(), millisecond).
% offset，可以是字符串或整数。默认情况下的空字符串被解释为当地时间。按原样包含非空字符串。整数的时间单位与Time 的时间单位相同。
% time_designator the date and time separator. The default is $T.
%
%%
-spec to_rfc3339(integer(), second | millisecond | microsecond | nanosecond) -> binary().
to_rfc3339(Num, second) ->
    to_rfc3339(Num, second, "");
% elib_dt:to_rfc3339(elib_dt:millisecond(), millisecond).
to_rfc3339(Num, millisecond) ->
    to_rfc3339(Num, millisecond, "");
% elib_dt:to_rfc3339(elib_dt:microsecond(), microsecond).
to_rfc3339(Num, microsecond) ->
    to_rfc3339(Num, microsecond, "");
to_rfc3339(Num, nanosecond) ->
    to_rfc3339(Num, nanosecond, "").

% elib_dt:to_rfc3339(1707198019, second, "+08:00").
% elib_dt:to_rfc3339(elib_dt:utc(second), second, "+08:00").
% 使用标准 T 分隔符（RFC3339/ISO 8601 标准）
to_rfc3339(Num, Unit, Offset) when is_integer(Num) andalso Num >= 0 ->
    % 值验证：拒绝超出合理范围的时间戳（防止数据库损坏值导致崩溃）
    % 合理范围：1970-01-01 ~ 2100-01-01

    % 2100-01-01 00:00:00 UTC
    MaxSeconds = 4102444800,
    MaxValue =
        case Unit of
            second -> MaxSeconds;
            millisecond -> MaxSeconds * 1000;
            microsecond -> MaxSeconds * 1000000;
            nanosecond -> MaxSeconds * 1000000000
        end,
    case Num =< MaxValue of
        true ->
            list_to_binary(
                calendar:system_time_to_rfc3339(Num, [
                    {unit, Unit}, {time_designator, $T}, {offset, Offset}
                ])
            );
        false ->
            error_logger:warning_msg(
                "Invalid timestamp value exceeds maximum: ~p ~p (max: ~p)~n",
                [Num, Unit, MaxValue]
            ),
            % 返回 epoch 时间作为安全默认值
            <<"1970-01-01T00:00:00Z">>
    end;
to_rfc3339(Num, _Unit, _Offset) ->
    error_logger:warning_msg("Invalid timestamp value type or negative: ~p~n", [Num]),
    <<"1970-01-01T00:00:00Z">>.

% elib_dt:datetime_to({{2024,10,29},{2,34,30.776}}, millisecond).
-spec datetime_to(
    {{integer(), integer(), integer()}, {integer(), integer(), float()}},
    millisecond
) -> integer() | {error, term()}.
datetime_to({{Y, Mo, D}, {H, Mi, S}}, millisecond) when is_number(S) ->
    try
        IntS = trunc(S),
        FracS = S - IntS,
        GregorianSecs = calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, Mi, IntS}}),
        UnixEpochSecs = GregorianSecs - 62167219200,
        UnixEpochSecs * 1000 + round(FracS * 1000)
    catch
        _:_ -> {error, "invalid datetime tuple"}
    end.

-spec rfc3339_to(integer() | tuple() | list() | binary() | undefined | atom()) ->
    integer() | binary() | null | term().
%% 空值提前返回 null：避免 <<>>/[]/undefined/null 流入 /2 报错路径，
%% 进而被 JSON 序列化为 "{error,empty_input}" 污染客户端数据（参见 friend list last_seen_at）。
rfc3339_to(<<>>) ->
    null;
rfc3339_to([]) ->
    null;
rfc3339_to(undefined) ->
    null;
rfc3339_to(null) ->
    null;
rfc3339_to(Val) when is_integer(Val) ->
    Val;
rfc3339_to(Val) when is_tuple(Val) ->
    datetime_to(Val, millisecond);
rfc3339_to(Val) when is_list(Val); is_binary(Val) ->
    case elib_type:is_numeric(Val) of
        true ->
            ec_cnv:to_integer(Val);
        false ->
            case rfc3339_to(Val, millisecond) of
                %% 解析失败亦返回 null，防止 tuple 穿透到 JSON
                {error, _} -> null;
                Ms -> Ms
            end
    end;
% 非时间字符串保持原样
rfc3339_to(Value) ->
    Value.

% Dt = elib_dt:now(),
% elib_dt:rfc3339_to(Dt, millisecond).
% elib_dt:rfc3339_to(Dt, microsecond).
-spec rfc3339_to(binary() | list() | undefined, atom()) -> integer() | {error, empty_input}.
rfc3339_to(Dt, Unit) when is_binary(Dt) andalso Dt =/= <<>> ->
    rfc3339_to(binary_to_list(Dt), Unit);
rfc3339_to(Dt, _Unit) when Dt =:= []; Dt =:= undefined ->
    {error, empty_input};
rfc3339_to(Dt, _Unit) when is_list(Dt), length(Dt) =:= 0 ->
    {error, empty_input};
rfc3339_to(Dt, Unit) when is_list(Dt); is_binary(Dt) ->
    % 先尝试标准 T 分隔符，失败后尝试空格分隔符（向后兼容）
    try
        Result = calendar:rfc3339_to_system_time(Dt, [{unit, Unit}, {time_designator, $T}]),
        try
            Result2 = calendar:rfc3339_to_system_time(Dt, [{unit, Unit}, {time_designator, $\s}]),
            if
                is_integer(Result2) ->
                    Result2;
                true ->
                    if
                        is_integer(Result) ->
                            Result;
                        true ->
                            {error, empty_input}
                    end
            end
        catch
            _:_ ->
                if
                    is_integer(Result) ->
                        Result;
                    true ->
                        {error, empty_input}
                end
        end
    catch
        _:_ ->
            {error, empty_input}
    end;
rfc3339_to(_Dt, _Unit) ->
    {error, empty_input}.

% https://www.erlang.org/docs/man/calendar#rfc3339_to_system_time-1
% https://www.erlang.org/docs/man/calendar#rfc3339_to_system_time-2
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
