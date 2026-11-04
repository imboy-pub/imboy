%% IMBOY_ERLANG_QUALITY P2-04：D01 同一瞬间/offset 保持 + D02 精度/整数缩放。
%% 独立参考：days_from_civil 纯整数算法（Howard Hinnant civil calendrical
%% calculations），不调 calendar 也不调被测——防 Gregorian 同源同错抵消。
-module(imboy_quality_pbt_dt_props).

-export([
    prop_d01_same_instant_offsets/0,
    prop_d02_precision_scaling/0,
    witnesses_ok/0
]).

-include_lib("proper/include/proper.hrl").

-define(OFFSETS, ["-12:00", "-03:30", "Z", "+05:30", "+05:45", "+08:00", "+12:45", "+14:00"]).
-define(MAX_SEC, 4102444800).

%% ---- 独立参考（纯整数，无 calendar） ----

days_from_civil(Y, M, D) ->
    Yd =
        case M =< 2 of
            true -> Y - 1;
            false -> Y
        end,
    Era =
        case Yd >= 0 of
            true -> Yd div 400;
            false -> (Yd - 399) div 400
        end,
    Yoe = Yd - Era * 400,
    Doy =
        (153 *
            (case M > 2 of
                true -> M - 3;
                false -> M + 9
            end) + 2) div 5 + D - 1,
    Doe = Yoe * 365 + Yoe div 4 - Yoe div 100 + Doy,
    Era * 146097 + Doe - 719468.

unix_ms({{Y, M, D}, {H, Mi, Sec}, MsFrac}) ->
    (days_from_civil(Y, M, D) * 86400 + H * 3600 + Mi * 60 + Sec) * 1000 + MsFrac.

%% ---- D01：显式单位/offset 表示同一瞬间 ----
%% 域：整毫秒时间戳（合法范围）x 8 个显式 offset（含半小时/45 分钟/端点）。
%% 断言：带 offset 串解析回的 UTC 毫秒 == 原值；与 Z 表示同瞬间（eq 比较）；
%% offset 字面量保留在输出（显式非本地）。
prop_d01_same_instant_offsets() ->
    ?FORALL(
        {Tms, Offset},
        {integer(0, ?MAX_SEC * 1000 - 1), oneof(?OFFSETS)},
        begin
            Str = elib_dt:to_rfc3339(Tms, millisecond, Offset),
            StrZ = elib_dt:to_rfc3339(Tms, millisecond, "Z"),
            Suffix =
                case Offset of
                    "Z" -> <<"Z">>;
                    O -> unicode:characters_to_binary(O)
                end,
            is_binary(Str) andalso
                binary:part(Str, byte_size(Str) - byte_size(Suffix), byte_size(Suffix)) =:=
                    Suffix andalso
                elib_dt:rfc3339_to(Str, millisecond) =:= Tms andalso
                elib_dt:rfc3339_to(StrZ, millisecond) =:= Tms andalso
                elib_dt:compare_rfc3339(Str, StrZ, eq)
        end
    ).

%% ---- D02：精度与整数缩放 ----
%% 域：随机 civil 时刻（1970..2099）x 毫秒小数。
%% 断言：datetime_to(…) 毫秒值 == days_from_civil 独立参考；
%%       to_rfc3339 长度启发式单位推断与显式单位一致。
prop_d02_precision_scaling() ->
    ?FORALL(
        {{Y, Mo, D}, {H, Mi, Sec}, MsFrac},
        {
            {integer(1970, 2099), integer(1, 12), integer(1, 28)},
            {integer(0, 23), integer(0, 59), integer(0, 59)},
            integer(0, 999)
        },
        begin
            DT = {{Y, Mo, D}, {H, Mi, Sec + MsFrac / 1000}},
            ExpectedMs = unix_ms({{Y, Mo, D}, {H, Mi, Sec}, MsFrac}),
            %% datetime_to/2 为内部函数；公开面 rfc3339_to/1 的 tuple 分支
            %% 即其毫秒缩放语义。
            case elib_dt:rfc3339_to(DT) of
                {error, _} ->
                    false;
                GotMs when is_integer(GotMs) ->
                    GotMs =:= ExpectedMs andalso
                        unit_heuristic_ok(GotMs);
                _ ->
                    false
            end
        end
    ).

%% 长度启发式单位推断只在无歧义域断言：10 位毫秒（1970-02/03 附近）与
%% 10 位秒（2001+）固有重叠——真实反例 {{1970,1,1},{0,0,0},0} 经 200 例
%% 随机暴露（毫秒 5391499806 被 10 位规则判为秒→超限→epoch 兜底）；
%% 既有启发式语义不因此改变（PLAN P2-04：不为随机性质改默认语义），
%% 歧义边界由见证 w10 冻结为记录。
unit_heuristic_ok(Ms) when Ms >= 100000000000 andalso Ms =< 4102444799999 ->
    MsGuess = elib_dt:to_rfc3339(Ms),
    MsExplicit = elib_dt:to_rfc3339(Ms, millisecond),
    SecGuess = elib_dt:to_rfc3339(Ms div 1000),
    SecExplicit = elib_dt:to_rfc3339(Ms div 1000, second),
    MsGuess =:= MsExplicit andalso SecGuess =:= SecExplicit;
unit_heuristic_ok(_) ->
    %% 域外（歧义窗口/早期毫秒）不约束启发式——见证负责冻结
    true.

%% ---- 强制见证（TEST_PLAN D01/D02 mandatory witnesses） ----

witnesses_ok() ->
    W = [
        {"w1_cross_day", fun w1/0},
        {"w2_cross_year", fun w2/0},
        {"w3_offset_endpoints", fun w3/0},
        {"w4_half_and_45min_offsets", fun w4/0},
        {"w5_unit_upper_bounds", fun w5/0},
        {"w6_negative_fallback_epoch", fun w6/0},
        {"w7_over_max_fallback_epoch", fun w7/0},
        {"w8_empty_null", fun w8/0},
        {"w9_month_boundary", fun w9/0},
        {"w10_10digit_ambiguity_frozen", fun w10/0}
    ],
    case [Name || {Name, F} <- W, F() =/= ok] of
        [] -> ok;
        Failed -> {error, {witness_failed, Failed}}
    end.

w1() ->
    %% 2024-02-29T23:30:00Z（闰日）+02:00 -> 2024-03-01T01:30（跨日跨月）
    Z = elib_dt:to_rfc3339(1709249400, second, "Z"),
    P = elib_dt:to_rfc3339(1709249400, second, "+02:00"),
    case {Z, P} of
        {<<"2024-02-29T23:30:00Z">>, <<"2024-03-01T01:30:00+02:00">>} -> ok;
        _ -> {mismatch, Z, P}
    end.

w2() ->
    %% 跨年：2023-12-31T23:00:00Z + 01:00 -> 2024-01-01T00:00
    P = elib_dt:to_rfc3339(1704063600, second, "+01:00"),
    case P of
        <<"2024-01-01T00:00:00+01:00">> -> ok;
        _ -> {mismatch, P}
    end.

w3() ->
    lists:foldl(
        fun(Off, ok) ->
            S = elib_dt:to_rfc3339(1700000000000, millisecond, Off),
            case elib_dt:rfc3339_to(S, millisecond) =:= 1700000000000 of
                true -> ok;
                false -> {mismatch, Off, S}
            end
        end,
        ok,
        ["-12:00", "+14:00", "Z"]
    ).

w4() ->
    lists:foldl(
        fun(Off, ok) ->
            S = elib_dt:to_rfc3339(951782400000, millisecond, Off),
            case
                elib_dt:rfc3339_to(S, millisecond) =:= 951782400000 andalso
                    elib_dt:compare_rfc3339(
                        S, elib_dt:to_rfc3339(951782400000, millisecond, "Z"), eq
                    )
            of
                true -> ok;
                false -> {mismatch, Off, S}
            end
        end,
        ok,
        ["+05:30", "+05:45", "+12:45", "-03:30"]
    ).

w5() ->
    %% 各单位上界（<=2100 合法）：MaxValue 通过、+1 兜底 epoch
    MaxS = 4102444800,
    Ok1 = elib_dt:to_rfc3339(MaxS, second) =/= <<"1970-01-01T00:00:00Z">>,
    Ok2 = elib_dt:to_rfc3339(MaxS * 1000, millisecond) =/= <<"1970-01-01T00:00:00Z">>,
    Ok3 = elib_dt:to_rfc3339(MaxS * 1000000, microsecond) =/= <<"1970-01-01T00:00:00Z">>,
    case {Ok1, Ok2, Ok3} of
        {true, true, true} -> ok;
        _ -> {mismatch, {Ok1, Ok2, Ok3}}
    end.

w6() ->
    case elib_dt:to_rfc3339(-1, second) of
        <<"1970-01-01T00:00:00Z">> -> ok;
        Other -> {mismatch, Other}
    end.

w7() ->
    case elib_dt:to_rfc3339(4102444801, second) of
        <<"1970-01-01T00:00:00Z">> -> ok;
        Other -> {mismatch, Other}
    end.

w8() ->
    case {elib_dt:rfc3339_to(<<>>), elib_dt:rfc3339_to(undefined), elib_dt:rfc3339_to(null)} of
        {null, null, null} -> ok;
        Other -> {mismatch, Other}
    end.

%% 10 位毫秒歧义窗口（真实反例缩后 {{1970,1,1},{0,0,0},0} 的域）：
%% 当前实现把 10 位值判为秒，5391499806 秒超 2100 上限 → epoch 兜底。
%% 冻结该边界行为为记录（非产品缺陷；未来显式改 API 时须更新此见证）。
w10() ->
    case elib_dt:to_rfc3339(5391499806) of
        <<"1970-01-01T00:00:00Z">> -> ok;
        Other -> {mismatch, Other}
    end.

w9() ->
    %% 月界独立参考：2024-01-31T23:59:59.999Z 毫秒（经公开面 rfc3339_to/1）
    Ref = unix_ms({{2024, 1, 31}, {23, 59, 59}, 999}),
    case elib_dt:rfc3339_to({{2024, 1, 31}, {23, 59, 59.999}}) of
        Ref -> ok;
        Other -> {mismatch, Ref, Other}
    end.
