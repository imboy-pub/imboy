-module(license_expiry_notice).
%%%===================================================================
%%% @doc License 到期提醒——纯逻辑内核（到期阈值判定 + 提醒文案）。
%%%
%%% 本模块只做「给定到期时间与当前时间，判断当前命中哪一档到期提醒、
%%% 并渲染主题/正文」。**不发信、不定时、不读收件人**——发送动作、定时
%%% 扫描、防重复、收件人与启用开关均属上层接线（由 sys.config 决定），
%%% 故意不在此实现：避免擅自对外发信或代填联系方式。
%%%
%%% 上线接线（待补，需运维提供收件人）：
%%%   1) 一个定时进程每日调用 due_threshold/2（入参 imboy_license:expires_at()
%%%      与 erlang:system_time(millisecond)，单位均为毫秒）；
%%%   2) 命中档位且该档尚未发过 → render/2 组装文案 → elib_email:send/3
%%%      发给 sys.config {license_notice_to, [Email]}（默认空=不发）。
%%% @end
%%%===================================================================

-export([due_threshold/2, render/2]).

-include_lib("eunit/include/eunit.hrl").

%% 到期前提醒天数档（升序）；DaysLeft 落入的最紧急档即触发
-define(THRESHOLDS, [1, 7, 30]).

-define(MS_PER_DAY, 86400000).

%% @doc 判定当前应提醒的到期档。入参单位均为毫秒（UTC epoch）。
%% 返回 expired（已到期）| 命中的最紧急天数档 | none（尚未进入任何档）。
-spec due_threshold(integer(), integer()) -> none | expired | 1 | 7 | 30.
due_threshold(ExpiresAtMs, NowMs) when is_integer(ExpiresAtMs), is_integer(NowMs) ->
    DaysLeft = ceil_div(ExpiresAtMs - NowMs, ?MS_PER_DAY),
    if
        DaysLeft =< 0 ->
            expired;
        true ->
            case [T || T <- ?THRESHOLDS, DaysLeft =< T] of
                [] -> none;
                Hit -> hd(Hit)
            end
    end.

%% @doc 渲染提醒主题与正文。Licensee 为授权方名称（imboy_license:licensee()）。
-spec render(expired | 1 | 7 | 30, binary()) -> {binary(), binary()}.
render(expired, Licensee) when is_binary(Licensee) ->
    {<<"IMBoy 授权已过期"/utf8>>, <<"授权（"/utf8, Licensee/binary, "）已过期，请尽快续费以恢复全部规模配额。"/utf8>>};
render(Days, Licensee) when is_integer(Days), is_binary(Licensee) ->
    DaysBin = integer_to_binary(Days),
    {
        <<"IMBoy 授权将在 "/utf8, DaysBin/binary, " 天内到期"/utf8>>,
        <<"授权（"/utf8, Licensee/binary, "）将在 "/utf8, DaysBin/binary, " 天内到期，请及时续费以免触达规模上限。"/utf8>>
    }.

%%%===================================================================
%%% Internal
%%%===================================================================

%% 向上取整除法；A 为负（已过期）时结果 =< 0，交由 due_threshold 归入 expired
-spec ceil_div(integer(), pos_integer()) -> integer().
ceil_div(A, B) when A > 0 -> (A + B - 1) div B;
ceil_div(A, B) -> A div B.
