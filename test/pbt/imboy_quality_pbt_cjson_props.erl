%% IMBOY_ERLANG_QUALITY P2-05：C01 canonical JSON 独立参考编码 + C02 非法点
%% 注入拒绝。SUT = enterprise_cursor_v2:canonical_json/1。
%% 独立参考：手写 RFC8259 转义 + 手写结构/排序（不调 jsone、不调 SUT）；
%% 转义子集与 jsone 输出的对齐由金样本见证钉死。
%% 域限制（TEST_PLAN §3.1）：深度 ≤3、节点 ≤64、字符串 ≤64 字节。
-module(imboy_quality_pbt_cjson_props).

-export([
    prop_c01_canonical_reference/0,
    prop_c02_illegal_injection/0,
    witnesses_ok/0
]).

-include_lib("proper/include/proper.hrl").

%% ---- 独立参考编码器 ----

oracle(T) ->
    case enc(T) of
        {ok, Io} -> {ok, iolist_to_binary(Io)};
        error -> {error, non_canonical}
    end.

enc(M) when is_map(M) ->
    case keys_binary(maps:keys(M)) of
        {ok, Ks} -> enc_map(M, lists:sort(Ks), ["{"]);
        error -> error
    end;
enc(L) when is_list(L) ->
    enc_list(L, ["["]);
enc(B) when is_binary(B) ->
    case is_valid_utf8(B) of
        true -> {ok, [$", esc(B), $"]};
        false -> error
    end;
enc(I) when is_integer(I) -> {ok, integer_to_binary(I)};
enc(true) ->
    {ok, <<"true">>};
enc(false) ->
    {ok, <<"false">>};
enc(null) ->
    {ok, <<"null">>};
enc(_) ->
    error.

keys_binary(Ks) ->
    case lists:all(fun is_binary/1, Ks) of
        true -> {ok, Ks};
        false -> error
    end.

enc_map(_M, [], Acc) ->
    %% 覆盖空 map（初始 Acc=["{"] 单元素）与非空累积
    {ok, lists:reverse([$} | Acc])};
enc_map(M, [K | Rest], Acc) ->
    case enc(maps:get(K, M)) of
        {ok, V} ->
            Sep =
                case Acc of
                    ["{"] -> "";
                    _ -> ","
                end,
            enc_map(M, Rest, [V, $:, [$", esc(K), $"], Sep | Acc]);
        error ->
            error
    end.

enc_list([], Acc) ->
    {ok, lists:reverse([$] | Acc])};
enc_list([V | Rest], Acc) ->
    case enc(V) of
        {ok, Vb} ->
            Sep =
                case Acc of
                    ["["] -> "";
                    _ -> ","
                end,
            enc_list(Rest, [Vb, Sep | Acc]);
        error ->
            error
    end.

%% 转义合同 = jsone 实际形态（w4 金样本钉死）：控制字符 \uXXXX（4 位
%% 小写 hex）、ASCII 可见原样、非 ASCII 一律 \uXXXX（>0xFFFF 代理对）。
esc(<<>>) ->
    <<>>;
esc(<<C/utf8, R/binary>>) ->
    E =
        case C of
            $" -> <<"\\\"">>;
            $\\ -> <<"\\\\">>;
            $\n -> <<"\\n">>;
            $\r -> <<"\\r">>;
            $\t -> <<"\\t">>;
            $\b -> <<"\\b">>;
            $\f -> <<"\\f">>;
            _ when C < 16#20 -> u4(C);
            _ when C < 16#80 -> <<C/utf8>>;
            _ when C =< 16#FFFF -> u4(C);
            _ -> u_surrogate(C)
        end,
    <<E/binary, (esc(R))/binary>>.

u4(C) ->
    unicode:characters_to_binary(io_lib:format("\\u~4.16.0b", [C])).

u_surrogate(C) ->
    Hi = 16#D800 + ((C - 16#10000) bsr 10),
    Lo = 16#DC00 + ((C - 16#10000) band 16#3FF),
    <<(u4(Hi))/binary, (u4(Lo))/binary>>.

is_valid_utf8(B) ->
    case unicode:characters_to_binary(B, utf8, utf8) of
        Re when is_binary(Re) -> true;
        _ -> false
    end.

%% ---- 受限生成器 ----

cp() ->
    %% 有效 codepoint（排除代理区）；含控制字符以覆盖转义路径
    oneof([integer(1, 16#D7FF), integer(16#E000, 16#10FFFF)]).

json_str() ->
    ?LET(
        Size,
        integer(0, 8),
        begin
            %% ≤64 字节硬限制：每 CP ≤4 字节，8 CP 上限 32 字节
            case Size of
                0 ->
                    <<>>;
                _ ->
                    CPs = [cp_fixed(S) || S <- lists:seq(1, Size)],
                    unicode:characters_to_binary(CPs, utf8)
            end
        end
    ).

cp_fixed(_) ->
    %% 由 ?LET 每次取同一 seed 的固定派生：用 proper 参数化——简单化：
    %% json_str 直接用 flatmap 尺寸，见下
    16#41.

%% 生成器（正确形态）：尺寸参数经 ?LET 传递
str_gen() ->
    ?LET(
        {Size, Seed},
        {integer(0, 8), integer(0, 16#7FFFFFFF)},
        case Size of
            0 -> <<>>;
            _ -> unicode:characters_to_binary(deterministic_cps(Size, Seed), utf8)
        end
    ).

deterministic_cps(0, _) ->
    [];
deterministic_cps(N, Seed) ->
    [1 + (Seed * 2654435761 + N * 40503) rem 16#D7FF | deterministic_cps(N - 1, Seed)].

map_layer(0) ->
    oneof([
        return(null),
        return(true),
        return(false),
        integer(-(1 bsl 53), 1 bsl 53),
        str_gen()
    ]);
map_layer(D) ->
    frequency([
        {4, map_layer(0)},
        {3, list(map_layer(D - 1))},
        {3, sized_map(D - 1)}
    ]).

sized_map(D) ->
    ?LET(
        {N, Seed},
        {integer(0, 6), integer(0, 16#7FFFFFFF)},
        maps:from_list([{key_i(Seed, I), val_i(Seed, I, D)} || I <- lists:seq(1, N)])
    ).

key_i(Seed, I) ->
    unicode:characters_to_binary(
        ["k", integer_to_list((Seed bsl 3 + I * 31) rem 100000)]
    ).

val_i(Seed, I, 0) ->
    case (Seed + I) rem 4 of
        0 -> (Seed + I) rem 1000;
        1 -> true;
        2 -> null;
        3 -> key_i(Seed, I)
    end;
val_i(Seed, I, D) ->
    maps:from_list([
        {key_i(Seed + I, J), val_i(Seed + I, J, D - 1)}
     || J <- lists:seq(1, (Seed + I) rem 3)
    ]).

json_fixed(0, _, _, _) ->
    null;
json_fixed(D, N, Seed, I) when N > 0 ->
    Pick = (Seed + I) rem 4,
    case Pick of
        0 -> (Seed + I) rem 1000;
        1 -> true;
        2 -> null;
        3 -> key_i(Seed, I)
    end;
json_fixed(_, _, _, _) ->
    null.

%% ---- C01 ----

prop_c01_canonical_reference() ->
    ?FORALL(
        T,
        map_layer(3),
        begin
            Expected = oracle(T),
            Got = enterprise_cursor_v2:canonical_json(T),
            case {Expected, Got} of
                {{ok, E}, {ok, G}} -> E =:= G;
                {{error, non_canonical}, {error, non_canonical}} -> true;
                _ -> false
            end
        end
    ).

%% ---- C02：非法点注入（shrink 后仍含非法点） ----

illegal() ->
    oneof([
        return(1.5),
        return(bad_atom),
        return(<<16#FF, 16#FE, 16#00>>),
        return({tuple, not_json})
    ]).

prop_c02_illegal_injection() ->
    ?FORALL(
        {T, Path},
        {map_layer(3), list(integer(0, 5))},
        ?FORALL(
            Bad,
            illegal(),
            begin
                Injected = inject(T, Path, Bad),
                %% 注入后必含非法点（inject 保证至少顶层或路径处替换）
                enterprise_cursor_v2:canonical_json(Injected) =:=
                    {error, non_canonical}
            end
        )
    ).

inject(T, [], Bad) ->
    Bad;
inject(M, [I | Rest], Bad) when is_map(M), M =/= #{} ->
    Ks = lists:sort(maps:keys(M)),
    K = lists:nth(1 + (I rem length(Ks)), Ks),
    M#{K => inject(maps:get(K, M), Rest, Bad)};
inject(M, [_ | Rest], Bad) when is_map(M) ->
    %% 空 map：无法导航，退化为顶层注入
    inject(M, Rest, Bad);
inject(L, [I | Rest], Bad) when is_list(L), L =/= [] ->
    Idx = 1 + (I rem length(L)),
    {H, [E | Tl]} = lists:split(Idx - 1, L),
    H ++ [inject(E, Rest, Bad) | Tl];
inject(L, [_ | Rest], Bad) when is_list(L) ->
    inject(L, Rest, Bad);
inject(_, _, Bad) ->
    %% 标量处：无法再导航——包一层 list 保住非法点
    [Bad].

%% ---- 强制见证 ----

witnesses_ok() ->
    W = [
        {"w1_key_order", fun w1/0},
        {"w2_array_order", fun w2/0},
        {"w3_nesting", fun w3/0},
        {"w4_unicode_escape_golden", fun w4/0},
        {"w5_empty_containers", fun w5/0},
        {"w6_float_rejected", fun w6/0},
        {"w7_atom_rejected", fun w7/0},
        {"w8_bad_key_rejected", fun w8/0},
        {"w9_invalid_utf8_rejected", fun w9/0},
        {"w10_legal_atoms_pass", fun w10/0}
    ],
    case [Name || {Name, F} <- W, F() =/= ok] of
        [] -> ok;
        Failed -> {error, {witness_failed, Failed}}
    end.

w1() ->
    %% key 顺序与插入序无关：两种插入序同产出（oracle 与 SUT 逐字节一致）
    expect(
        enterprise_cursor_v2:canonical_json(#{<<"b">> => 2, <<"a">> => 1}) =:=
            {ok, <<"{\"a\":1,\"b\":2}">>} andalso
            oracle(#{<<"b">> => 2, <<"a">> => 1}) =:= {ok, <<"{\"a\":1,\"b\":2}">>}
    ).

w2() ->
    expect(
        oracle([3, 1, 2]) =:= {ok, <<"[3,1,2]">>} andalso
            enterprise_cursor_v2:canonical_json([3, 1, 2]) =:= {ok, <<"[3,1,2]">>}
    ).

w3() ->
    Deep = #{<<"o">> => [#{<<"i">> => null}, true]},
    {ok, _} = oracle(Deep),
    {ok, _} = enterprise_cursor_v2:canonical_json(Deep),
    ok.

w4() ->
    %% 金样本：转义合同钉死（SUT=jsone 与手写 oracle 必须逐字节一致）
    Str = <<"a\"b\\c\nd\te", 1, 16#7F, "中文"/utf8>>,
    expect(
        oracle(Str) =:= enterprise_cursor_v2:canonical_json(Str) andalso
            oracle(Str) =/= {error, non_canonical}
    ).

w5() ->
    expect(
        enterprise_cursor_v2:canonical_json(#{}) =:= {ok, <<"{}">>} andalso
            enterprise_cursor_v2:canonical_json([]) =:= {ok, <<"[]">>}
    ).

w6() ->
    expect(
        enterprise_cursor_v2:canonical_json(#{<<"x">> => 1.5}) =:= {error, non_canonical}
    ).

w7() ->
    expect(
        enterprise_cursor_v2:canonical_json(#{<<"x">> => some_atom}) =:=
            {error, non_canonical}
    ).

w8() ->
    expect(enterprise_cursor_v2:canonical_json(#{1 => 2}) =:= {error, non_canonical}).

w9() ->
    expect(
        enterprise_cursor_v2:canonical_json([<<16#FF, 16#FE>>]) =:=
            {error, non_canonical}
    ).

w10() ->
    expect(
        enterprise_cursor_v2:canonical_json(#{<<"a">> => true, <<"b">> => false, <<"c">> => null}) =:=
            {ok, <<"{\"a\":true,\"b\":false,\"c\":null}">>}
    ).

expect(true) -> ok;
expect(false) -> error.
