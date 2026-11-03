%% IMBOY_ERLANG_QUALITY P2-09：ORACLE01-04 业务错误注入检出。
%% 每类注入必须被既有真实业务 oracle 检出（不是独立 toy false property）：
%%   ORACLE01 帧协议字段损坏 → F01 wire/decode 完整成功形态判据
%%   ORACLE02 时间 offset 方向反转 → D01 roundtrip 恒等合同
%%   ORACLE03 TSID 槽位重用   → Q01 序列合同（长度/严格推进/cursor 推进）；
%%          witness 内含专用随机子运行取得缩减反例并函数级重放
%%   ORACLE04 旧 live 覆盖新 delete → M01 每键最高 revision 独立 oracle；
%%          正向兼容控制（live/unavailable 同 rev 裁决）同性质内仍通过
%% 检出 = oracle 对注入输出判定失败；每个性质同时带未注入正控。
-module(imboy_quality_pbt_oracle_faults).

-export([
    prop_oracle01_frame_corruption/0,
    prop_oracle02_offset_reversal/0,
    prop_oracle03_tsid_slot_reuse/0,
    prop_oracle04_stale_live_over_delete/0,
    witnesses_ok/0
]).

-include_lib("proper/include/proper.hrl").

-define(SIGNED_OFFSETS, ["-12:00", "-03:30", "+05:30", "+05:45", "+08:00", "+12:45", "+14:00"]).
-define(MAX_SEC, 4102444800).

%% ==================== ORACLE01：帧字段损坏 → F01 oracle 检出 ====================

%% F01 冻结的完整成功形态：decode 得 {ok, Frame, <<>>} 且字段等于原始语义。
oracle01_ok(Wire, Type, Flags, Payload) ->
    case imboy_frame:decode(Wire) of
        {ok, Frame, <<>>} ->
            imboy_frame:version(Frame) =:= 2 andalso
                imboy_frame:type(Frame) =:= Type andalso
                imboy_frame:flags(Frame) =:= Flags andalso
                imboy_frame:payload(Frame) =:= Payload;
        _ ->
            false
    end.

%% 注入域：协议合同字段（magic 位翻转 / version ≠ 2 / 长度 ± / 截断 / 尾部追加），
%% 全部保证字节级改变；payload 内容翻转不属协议违规，不在注入域。
mutation_gen() ->
    oneof([
        {magic, integer(0, 7)},
        {version, integer(3, 255)},
        {len_add, integer(1, 9)},
        {len_sub, integer(1, 4)},
        {truncate, integer(1, 4)},
        {append, integer(1, 9)}
    ]).

apply_mutation(magic, I, <<B:8, Rest/binary>>) ->
    <<(B bxor (1 bsl I)):8, Rest/binary>>;
apply_mutation(version, V, <<M:16, _Ver:8, Rest/binary>>) ->
    <<M:16, V:8, Rest/binary>>;
apply_mutation(len_add, D, Wire) ->
    {Pre, L0, P} = split_wire(Wire),
    <<Pre/binary, (L0 + D):32, P/binary>>;
apply_mutation(len_sub, D, Wire) ->
    {Pre, L0, P} = split_wire(Wire),
    D2 = 1 + (D rem L0),
    <<Pre/binary, (L0 - D2):32, P/binary>>;
apply_mutation(truncate, K, Wire) ->
    Size = byte_size(Wire),
    K2 = 1 + (K rem (Size - 9)),
    binary:part(Wire, 0, Size - K2);
apply_mutation(append, N, Wire) ->
    Tail = binary:copy(<<16#A5>>, N),
    <<Wire/binary, Tail/binary>>.

split_wire(<<M:16, V:8, F:8, T:8, L:32, P/binary>>) ->
    {<<M:16, V:8, F:8, T:8>>, L, P}.

prop_oracle01_frame_corruption() ->
    ?FORALL(
        {Type, Flags, Payload, Mutation},
        {integer(0, 255), integer(0, 255), non_empty(binary()), mutation_gen()},
        begin
            Wire = imboy_frame:encode(Type, Flags, Payload),
            %% 正控：未注入 wire 满足完整成功形态
            Oracle = oracle01_ok(Wire, Type, Flags, Payload),
            Mutated = apply_mutation(element(1, Mutation), element(2, Mutation), Wire),
            Oracle andalso
                Mutated =/= Wire andalso
                %% 检出：注入后 F01 oracle 不再成立
                not oracle01_ok(Mutated, Type, Flags, Payload)
        end
    ).

%% ==================== ORACLE02：offset 方向反转 → D01 合同检出 ====================

reverse_offset(<<$+, Rest/binary>>) -> <<$-, Rest/binary>>;
reverse_offset(<<$-, Rest/binary>>) -> <<$+, Rest/binary>>.

reverse_suffix(Str) when is_binary(Str) ->
    N = byte_size(Str),
    true = N > 6,
    <<Body:(N - 6)/binary, Off:6/binary>> = Str,
    <<Body/binary, (reverse_offset(Off))/binary>>.

prop_oracle02_offset_reversal() ->
    ?FORALL(
        {Tms, Offset},
        {integer(0, ?MAX_SEC * 1000 - 1), oneof(?SIGNED_OFFSETS)},
        begin
            Str = elib_dt:to_rfc3339(Tms, millisecond, Offset),
            %% 正控：D01 合同——带 offset 串解析回同一瞬间
            Roundtrip = elib_dt:rfc3339_to(Str, millisecond) =:= Tms,
            Reversed = reverse_suffix(Str),
            %% 检出：方向反转后瞬间必偏移 2×|offset|，D01 oracle 不再恒等
            Roundtrip andalso elib_dt:rfc3339_to(Reversed, millisecond) =/= Tms
        end
    ).

%% ==================== ORACLE03：TSID 槽位重用 → Q01 序列合同检出 ====================
%% 合同级抽象（Q01 冻结不变量，不冒称 CAS/多节点）：
%%   (a) L - F + 1 = Count；(b) F_next > L（严格推进、不重叠）；(c) Cursor 推进到 L。
%% 注入实现 fake_reserve：Count 为偶数的批次重用上一批区间（槽位重用缺陷）。

fake_reserve(Prev, Count) when Count rem 2 =:= 0 ->
    %% 注入：重用上一批 [F, L]（首批 Prev={0,0,0} 退化为零区间，同样违反 (a)）
    {element(1, Prev), element(2, Prev), element(2, Prev), Count};
fake_reserve({F, L, _}, Count) ->
    NF = L + 1,
    NL = NF + Count - 1,
    {NF, NL, NL, Count}.

%% Q01 检出器：对批次输出序列检查 (a)(b)(c)。
seq_detect(Outputs) ->
    seq_detect(Outputs, none).

seq_detect([], _) ->
    ok;
seq_detect([{F, L, Cursor, Count} | Rest], Prev) ->
    LengthOk = L - F + 1 =:= Count,
    CursorOk = Cursor =:= L,
    AdvanceOk =
        case Prev of
            none -> true;
            {_, PL, _, _} -> F > PL
        end,
    case LengthOk andalso CursorOk andalso AdvanceOk of
        true -> seq_detect(Rest, {F, L, Cursor, Count});
        false -> {violation, {F, L, Cursor, Count}}
    end;
seq_detect([_Bad | _], _) ->
    {violation, bad_batch}.

fake_seq(Counts) ->
    fake_seq(Counts, {0, 0, 0}).

fake_seq([], _) ->
    [];
fake_seq([C | Rest], State) ->
    {F, L, Cursor, C} = fake_reserve(State, C),
    [{F, L, Cursor, C} | fake_seq(Rest, {F, L, C})].

%% 注入性质的布尔面（供 witness 内层 quickcheck 缩减与重放）。
fake_seq_passes(Counts) ->
    seq_detect(fake_seq(Counts)) =:= ok.

contains_even(Counts) ->
    lists:any(fun(C) -> C rem 2 =:= 0 end, Counts).

prop_oracle03_tsid_slot_reuse() ->
    ?FORALL(
        Counts,
        list(integer(1, 2048)),
        begin
            %% 注入语义：含偶数批必检出（重用区间违反 (a)/(b)）；全奇批按真实
            %% 合同推进，检出器判 ok（正控）。两者与注入条件一一对应。
            Detected = seq_detect(fake_seq(Counts)) =/= ok,
            Detected =:= contains_even(Counts)
        end
    ).

%% ==================== ORACLE04：旧 live 覆盖新 delete → M01 oracle 检出 ====================

row(Wire) -> imboy_cache_policy:snapshot_manifest_entry(Wire).

final(Pages, Limits) ->
    imboy_cache_sync_coverage:manifest(
        #{
            pages => length(Pages),
            manifest => lists:zip(lists:seq(1, length(Pages)), Pages)
        },
        Limits,
        0
    ).

%% M01 独立 oracle：每键保留 rev 最大的行。
expected_final(Rows7) ->
    lists:foldl(
        fun(R, Acc) ->
            K = element(2, R),
            case maps:get(K, Acc, undefined) of
                undefined ->
                    Acc#{K => R};
                Old ->
                    case element(3, R) > element(3, Old) of
                        true -> Acc#{K => R};
                        false -> Acc#{K => Old}
                    end
            end
        end,
        #{},
        Rows7
    ).

prop_oracle04_stale_live_over_delete() ->
    ?FORALL(
        {K, LiveC, DelC, V, ExpOff},
        ?LET(
            {K0, LiveC0, Gap, V0, ExpOff0},
            {integer(1, 9), integer(1, 8), integer(1, 8), integer(1, 50), integer(0, 3)},
            {K0, LiveC0, erlang:min(9, LiveC0 + Gap), V0, ExpOff0}
        ),
        begin
            Exp = 4999999999999 + ExpOff * 1000,
            Live = row({K, V, Exp, {LiveC, node()}}),
            Delete = row({deleted, K, undefined, Exp, {DelC, node()}}),
            Limits = {1000000, 1000},
            %% 真实归并合同：更高 revision 的新 delete 胜出（两序）
            RealOk =
                case final([[Live], [Delete]], Limits) of
                    {ok, F1} ->
                        R1 = maps:get(K, F1),
                        element(1, R1) =:= deleted andalso
                            element(3, R1) =:= {DelC, node()};
                    _ ->
                        false
                end andalso
                    case final([[Delete], [Live]], Limits) of
                        {ok, F2} ->
                            R2 = maps:get(K, F2),
                            element(1, R2) =:= deleted andalso
                                element(3, R2) =:= {DelC, node()};
                        _ ->
                            false
                    end,
            %% 注入：旧 live 覆盖新 delete（fake merge 输出）
            FakeFinal = #{K => Live},
            %% 检出：M01 每键最高 revision oracle 对注入输出判定失败
            Detected =
                maps:get(K, FakeFinal) =/= maps:get(K, expected_final([Live, Delete])),
            %% 正控：兼容裁决（live/unavailable 同 rev → live 胜）在真实归并仍通过
            Unavail = row({unavailable, K, undefined, Exp, {LiveC, node()}}),
            CompatOk =
                case final([[Live], [Unavail]], Limits) of
                    {ok, F3} -> maps:get(K, F3) =:= Live;
                    _ -> false
                end,
            RealOk andalso Detected andalso CompatOk
        end
    ).

%% ==================== 强制固定见证 ====================

witnesses_ok() ->
    W = [
        {"o1_magic_flip_fixed", fun w_o1/0},
        {"o2_offset_reversal_fixed", fun w_o2/0},
        {"o3_slot_reuse_shrink_replay", fun w_o3/0},
        {"o4_stale_live_fixed", fun w_o4/0}
    ],
    case [Name || {Name, F} <- W, F() =/= ok] of
        [] -> ok;
        Failed -> {error, {witness_failed, Failed}}
    end.

e(true) -> ok;
e(false) -> erlang:error(witness_assertion).

w_o1() ->
    Type = 7,
    Flags = 1,
    Payload = <<"hello-corruption-probe">>,
    Wire = imboy_frame:encode(Type, Flags, Payload),
    %% 正控
    e(oracle01_ok(Wire, Type, Flags, Payload)),
    %% 固定注入：magic 位翻转 / version=9 / len_sub=3 / 尾部追加
    e(not oracle01_ok(apply_mutation(magic, 1, Wire), Type, Flags, Payload)),
    e(not oracle01_ok(apply_mutation(version, 9, Wire), Type, Flags, Payload)),
    e(not oracle01_ok(apply_mutation(len_sub, 3, Wire), Type, Flags, Payload)),
    e(not oracle01_ok(apply_mutation(append, 2, Wire), Type, Flags, Payload)),
    ok.

w_o2() ->
    %% 固定反例：同一时刻 +05:30 表示，offset 方向反转后瞬间偏移 11 小时
    Tms = 1768468800000,
    Str = elib_dt:to_rfc3339(Tms, millisecond, "+05:30"),
    e(elib_dt:rfc3339_to(Str, millisecond) =:= Tms),
    Rev = reverse_suffix(Str),
    e(elib_dt:rfc3339_to(Rev, millisecond) =/= Tms),
    ok.

w_o3() ->
    %% 专用随机子运行：对注入实现跑「输出满足 Q01 合同」性质 → 必失败并缩减；
    %% 取得反例后函数级重放，检出保持。
    Prop = ?FORALL(Counts, list(integer(1, 2048)), fake_seq_passes(Counts)),
    Engine =
        proper:quickcheck(Prop, [
            {numtests, 100},
            {on_output, fun(_F, _A) -> ok end}
        ]),
    e(Engine =:= false),
    %% counterexample() 返回最外层 FORALL 的参数包层 [Inst]；单层性质即 [Counts]
    Cex = proper:counterexample(),
    e(is_list(Cex)),
    Counts0 = hd(Cex),
    e(is_list(Counts0)),
    %% 重放：同一反例在检出器下仍为违规
    e(seq_detect(fake_seq(Counts0)) =/= ok),
    ok.

w_o4() ->
    K = 3,
    Exp = 4999999999999,
    Live = row({K, 41, Exp, {1, node()}}),
    Delete = row({deleted, K, undefined, Exp, {2, node()}}),
    Limits = {1000000, 1000},
    %% 真实归并：新 delete 胜
    {ok, F} = final([[Live], [Delete]], Limits),
    e(element(1, maps:get(K, F)) =:= deleted),
    %% 注入：旧 live 覆盖 → M01 oracle 检出
    FakeFinal = #{K => Live},
    e(maps:get(K, FakeFinal) =/= maps:get(K, expected_final([Live, Delete]))),
    ok.
