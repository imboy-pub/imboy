%% IMBOY_ERLANG_QUALITY P2-08：M01 合法完整 manifest 模型 / M02 逻辑条目幂等
%% 与页完整性 / M03 同版本冲突及兼容控制。
%% 被测合同（冻结自 src/lib/imboy_cache_sync_coverage.erl 的
%% manifest/3 + merge_manifest_row/3 + choose_manifest_row/2，以及既有
%% imboy_cache_manifest_merge_tests）：manifest 行为 7 元组
%% {Kind,Key,Rev,Exp,Origin,WireHash,ValueHash}，rev 在位 3；页号须排序后
%% 恰为 1..Pages；同键相遇按 rev 裁决，equal 须 expiry 相等、origin 同源或
%% 含 unknown；{live,live} 须 value hash 相等；live/deleted 混合同 rev 为
%% snapshot_kind_conflict；live 不被 unavailable/expired 遮蔽；unknown
%% 来源不抹掉已知来源。
%% 独立 oracle：expected_final/1 按「每键保留最高 revision 行」独立折叠，
%% 不复用 merge_manifest_row/choose_manifest_row（防同错抵消）。M01/M02
%% 生成器保证同键 revision counter 互异（expected_final 全序无 equal 分支），
%% 同版本裁决专门留给 M03。
-module(imboy_quality_pbt_manifest_props).

-export([
    prop_m01_full_manifest_model/0,
    prop_m02_entry_idempotence_page_integrity/0,
    prop_m03_revision_conflict_compat/0,
    witnesses_ok/0
]).

-include_lib("proper/include/proper.hrl").

%% ---- 行构造与独立 oracle ----

limits() -> {1000000, 1000}.
base_ms() -> erlang:system_time(millisecond) + 60000.

row(Wire) -> imboy_cache_policy:snapshot_manifest_entry(Wire).

wrap(unknown, Body) -> Body;
wrap(Origin, Body) -> {with_origin, Origin, Body}.

%% 行规格 {Key, Kind, Value, Origin, Rev}；exp 由 rev counter 派生，
%% 保证同 (键, counter) 组合 expiry 唯一，纯化 M01/M02 的重排/幂等义务。
wire_of({K, live, V, O, Rev}) ->
    wrap(O, {K, V, exp_of(Rev), Rev});
wire_of({K, Kind, _V, O, Rev}) when Kind =:= deleted; Kind =:= expired; Kind =:= unavailable ->
    wrap(O, {Kind, K, undefined, exp_of(Rev), Rev}).

exp_of({C, _Node}) -> base_ms() + C * 1000.

%% 同键 revision counter 按出现序递增 → 互异，M01/M02 无同 rev 相遇。
assign_revs(Raws) ->
    {RevRows, _} =
        lists:mapfoldl(
            fun({K, Kind, V, O}, Acc) ->
                C = maps:get(K, Acc, 0) + 1,
                {{K, Kind, V, O, {C, node()}}, Acc#{K => C}}
            end,
            #{},
            Raws
        ),
    RevRows.

%% 独立 oracle：每键保留 rev 最大的行（同键 counter 互异 → 全序）。
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

%% ---- 页集构造 ----

final_raw(Manifest, NPages, Limits) ->
    imboy_cache_sync_coverage:manifest(#{pages => NPages, manifest => Manifest}, Limits, 0).

final(Pages, Limits) ->
    final_raw(lists:zip(lists:seq(1, length(Pages)), Pages), length(Pages), Limits).

final_round(PeerMap, Limits) ->
    imboy_cache_sync_coverage:manifest(#{deadline_ms => 60000, peers => PeerMap}, Limits, 0).

peer(Rows) -> #{authority => true, pages => 1, manifest => [{1, Rows}]}.

%% 均分为 P 页（末页允许空）。
pages_of(Rows, P) when P >= 1, P =< length(Rows) + 1 ->
    Len = length(Rows),
    Base = Len div P,
    Extra = Len rem P,
    Sizes = [Base + erlang:min(1, erlang:max(0, Extra - I + 1)) || I <- lists:seq(1, P)],
    {Pages, _} =
        lists:mapfoldl(
            fun(Sz, Rest) ->
                {Take, Next} = lists:split(Sz, Rest),
                {Take, Next}
            end,
            Rows,
            Sizes
        ),
    Pages.

ok_case({ok, F}, Expected) -> F =:= Expected;
ok_case(_, _) -> false.

%% 随机排列（proper v1.5 无 ?SHUFFLE）：等长随机键排序，元素多重集不变。
shuffled(List) ->
    N = length(List),
    ?LET(
        Keys,
        vector(N, integer(1, 1000000)),
        [E || {_, E} <- lists:sort(lists:zip(Keys, List))]
    ).

prefixes_ok(Rows7, Limits) ->
    lists:all(
        fun(I) ->
            Prefix = lists:sublist(Rows7, I),
            ok_case(final([Prefix], Limits), expected_final(Prefix))
        end,
        lists:seq(1, length(Rows7))
    ).

kind_gen() -> frequency([{6, live}, {2, deleted}, {1, expired}, {1, unavailable}]).

origin_gen() ->
    frequency([
        {2, unknown},
        {3, ?LET({B, S}, {integer(1, 4), integer(1, 9)}, {node(), <<B:16>>, S})}
    ]).

row_spec() ->
    ?LET(
        {K, Kind, V, O},
        {integer(1, 5), kind_gen(), integer(1, 50), origin_gen()},
        {K, Kind, V, O}
    ).

%% ---- M01：合法完整 manifest 模型 ----
%% ① 兼容最高 revision：归并结果 = 每键最高 rev 行（独立 oracle 相等）
%% ② 真实重排：随机页数 + 页内/页间随机排列 → 结果不变
%% ③ 真实重新分页：行序列随机重新切页 → 结果不变
%% ④ 所有中间前缀预算充足：每个前 i 行前缀都归并成功且为独立前缀模型
prop_m01_full_manifest_model() ->
    ?FORALL(
        Raws,
        non_empty(list(row_spec())),
        begin
            Rows7 = [row(wire_of(R)) || R <- assign_revs(Raws)],
            Expected = expected_final(Rows7),
            Limits = limits(),
            Len = length(Rows7),
            ?FORALL(
                {PagesPerm, P2},
                {
                    ?LET(
                        {P, Perm},
                        {integer(1, Len), shuffled(Rows7)},
                        pages_of(Perm, P)
                    ),
                    integer(1, Len + 1)
                },
                begin
                    Repaged = pages_of(lists:append(PagesPerm), P2),
                    ok_case(final(PagesPerm, Limits), Expected) andalso
                        ok_case(final(Repaged, Limits), Expected) andalso
                        prefixes_ok(Rows7, Limits)
                end
            )
        end
    ).

%% ---- M02：逻辑条目幂等与页完整性（两者分开） ----
%% (a) 同一逻辑条目重复 2..4 次 → 结果与出现一次相同；
%% (b) 页号缺失 / 页号重复 / 页号跳号必须拒绝；页乱序合法（与重复区分）。
prop_m02_entry_idempotence_page_integrity() ->
    ?FORALL(
        Raws,
        non_empty(list(row_spec())),
        %% 嵌套 ?FORALL（性质组合）：?LET 的 body 是内层生成器位置，
        %% 纯断言放那里会变成 bind 类型的实例值而非测试结果。
        ?FORALL(
            {Victim, DupN, J, Mut},
            {integer(1, 12), integer(2, 4), integer(1, 8), integer(1, 3)},
            begin
                Rows7 = [row(wire_of(R)) || R <- assign_revs(Raws)],
                Expected = expected_final(Rows7),
                Limits = limits(),
                VictimRow = lists:nth(1 + (Victim rem length(Rows7)), Rows7),
                DupRows = Rows7 ++ lists:duplicate(DupN, VictimRow),
                IdemOk = ok_case(final([DupRows], Limits), Expected),
                P = erlang:min(2, length(Rows7)),
                Legal = pages_of(Rows7, P),
                J2 = 1 + (J rem P),
                IdemOk andalso page_integrity_ok(Legal, P, J2, Mut, Limits)
            end
        )
    ).

page_integrity_ok(Legal, P, J, Mut, Limits) ->
    {Manifest, NPages} =
        case Mut of
            1 ->
                %% 页号缺失：P 页集去掉第 J 页仍声明 pages=P
                {lists:sublist(Legal, J - 1) ++ lists:nthtail(J, Legal), P};
            2 ->
                %% 页号重复：第 J 页原样追加
                {Legal ++ [lists:nth(J, Legal)], P + 1};
            _ ->
                %% 页号跳号：1..P+1 去掉 J（J..P 页右移一位）
                {
                    [{N, Rows} || {N, Rows} <- Legal, N < J] ++
                        [{N + 1, Rows} || {N, Rows} <- Legal, N >= J],
                    P + 1
                }
        end,
    %% 负例：页号集不完整/重复必须拒绝
    case final_raw(Manifest, NPages, Limits) of
        {error, _} ->
            %% 正控区分：同样的页内容乱序（页号仍完整 1..P）合法
            ok_case(final(lists:reverse(Legal), Limits), expected_final(lists:append(Legal)));
        {ok, _} ->
            false
    end.

%% ---- M03：同版本冲突及兼容控制 ----
%% 四类同 rev 冲突（payload/expiry/live-vs-deleted/已知来源不同）必须拒绝，
%% 且不被干扰键或随机分页/排列掩盖；live/unavailable、live/expired、
%% plain/known 兼容正控；只有真正更高的 revision 才能合法定局（A、B 直接
%% 相遇时，即使存在更高 rev 行也不能把冲突洗白成 ok）。
prop_m03_revision_conflict_compat() ->
    ?FORALL(
        Case,
        oneof([
            conflict_payload,
            conflict_expiry,
            conflict_kind,
            conflict_origin,
            compat_live_unavailable,
            compat_live_expired,
            compat_plain_known,
            revkind_live_over_delete,
            revkind_delete_over_live
        ]),
        %% 同 M02：嵌套 ?FORALL 组合，?LET 只保留在生成器位置（shuffled/1）。
        ?FORALL(
            {K, Perm, Split},
            {integer(1, 9), shuffled([a, b, d1, d2]), integer(1, 5)},
            m03_case(Case, K, Perm, Split)
        )
    ).

m03_case(Case, K, Perm, Split) ->
    Exp = base_ms(),
    R1 = {1, node()},
    R2 = {2, node()},
    R9 = {9, node()},
    Live = fun(V, Rev) -> row({K, V, Exp, Rev}) end,
    Gone = fun(Kind, Rev) -> row({Kind, K, undefined, Exp, Rev}) end,
    {A, B, Winner} =
        case Case of
            conflict_payload ->
                {Live(41, R2), Live(42, R2), error};
            conflict_expiry ->
                {row({K, 41, Exp, R2}), row({K, 41, Exp + 1, R2}), error};
            conflict_kind ->
                {Live(41, R2), Gone(deleted, R2), error};
            conflict_origin ->
                {
                    row({with_origin, {node(), <<1:16>>, 1}, {K, 41, Exp, R2}}),
                    row({with_origin, {node(), <<2:16>>, 1}, {K, 41, Exp, R2}}),
                    error
                };
            compat_live_unavailable ->
                {Live(41, R2), Gone(unavailable, R2), Live(41, R2)};
            compat_live_expired ->
                {Live(41, R2), Gone(expired, R2), Live(41, R2)};
            compat_plain_known ->
                Known = row({with_origin, {node(), <<7:16>>, 3}, {K, 41, Exp, R2}}),
                {Live(41, R2), Known, Known};
            revkind_live_over_delete ->
                {Live(41, R2), Gone(deleted, R1), Live(41, R2)};
            revkind_delete_over_live ->
                {Gone(deleted, R2), Live(41, R1), Gone(deleted, R2)}
        end,
    DRows = [row({K + 10 + N, 7, Exp, R1}) || N <- [1, 2]],
    RowMap = #{a => A, b => B, d1 => hd(DRows), d2 => lists:nth(2, DRows)},
    PermRows = [maps:get(T, RowMap) || T <- Perm],
    Higher = row({K, 99, Exp, R9}),
    Limits = limits(),
    %% ① 两序相邻相遇
    P1 = final([[A], [B]], Limits),
    P2 = final([[B], [A]], Limits),
    %% ② 干扰键 + 随机排列 + 随机分页：结论不被掩盖
    P3 = final(pages_of(PermRows, Split), Limits),
    %% ③ 更高 revision：冲突对直接相邻相遇仍拒绝（不被「存在更高 rev」洗白）；
    %%    兼容对相遇按合同裁出胜者后，更高 rev 合法定局为其行
    P4 = final([[A, B, Higher]], Limits),
    %% ④ 更高 revision 真正介入归并 → 合法定局为其行
    P5 = final([[A, Higher, B]], Limits),
    P6 = final([[Higher, A, B]], Limits),
    %% ⑤ 跨来源同回合：冲突拒绝 / 兼容同规则
    P7 = final_round(#{a => peer([A]), b => peer([B])}, Limits),
    P8 = final_round(#{b => peer([B]), a => peer([A])}, Limits),
    P4Expect =
        case Winner of
            error -> error;
            _ -> Higher
        end,
    check(P1, K, Winner) andalso
        check(P2, K, Winner) andalso
        check(P3, K, Winner) andalso
        keeps_distractions(P3, DRows, Winner) andalso
        check(P4, K, P4Expect) andalso
        check(P5, K, Higher) andalso
        check(P6, K, Higher) andalso
        check(P7, K, Winner) andalso
        check(P8, K, Winner).

%% 期待拒绝（Winner=error）时结果必须是 {error, _}。
check(Res, _K, error) ->
    case Res of
        {error, _} -> true;
        _ -> false
    end;
check(Res, K, W) ->
    case Res of
        {ok, Final} -> maps:get(K, Final, undefined) =:= W;
        _ -> false
    end.

%% 归并成功时干扰键行必须原样保留（未被波及）；期待拒绝时归并失败即符合。
keeps_distractions({ok, Final}, DRows, _Winner) ->
    lists:all(fun(D) -> maps:get(element(2, D), Final, undefined) =:= D end, DRows);
keeps_distractions({error, _}, _DRows, error) ->
    true;
keeps_distractions({error, _}, _DRows, _Winner) ->
    false.

%% ---- 强制固定见证 ----

witnesses_ok() ->
    W = [
        {"m01_multi_key_model", fun w_m01_multi_key/0},
        {"m01_single_row", fun w_m01_single_row/0},
        {"m02_duplicate_entry_idempotent", fun w_m02_idempotent/0},
        {"m02_page_integrity_variants", fun w_m02_page_variants/0},
        {"m03_conflict_matrix", fun w_m03_matrix/0}
    ],
    case [Name || {Name, F} <- W, F() =/= ok] of
        [] -> ok;
        Failed -> {error, {witness_failed, Failed}}
    end.

e(true) -> ok;
e(false) -> erlang:error(witness_assertion).

w_m01_multi_key() ->
    %% 多键多 kind、known/unknown 来源混合的固定模型
    Raws = [
        {1, live, 11, unknown, {1, node()}},
        {1, live, 12, {node(), <<1:16>>, 2}, {2, node()}},
        {2, deleted, 0, unknown, {1, node()}},
        {2, unavailable, 0, {node(), <<2:16>>, 1}, {2, node()}},
        {3, live, 7, {node(), <<3:16>>, 5}, {3, node()}}
    ],
    Rows7 = [row(wire_of(R)) || R <- Raws],
    Expected = expected_final(Rows7),
    Limits = limits(),
    e(ok_case(final([Rows7], Limits), Expected)),
    %% 重排为 2 页（页内反序 + 页序反转）结果不变
    P1 = lists:reverse(lists:sublist(Rows7, 3)),
    P2 = lists:reverse(lists:nthtail(3, Rows7)),
    e(ok_case(final([P2, P1], Limits), Expected)),
    %% 重新分页 3 页（全覆盖切分）结果不变
    e(
        ok_case(
            final(
                [
                    lists:sublist(Rows7, 2),
                    [lists:nth(3, Rows7)],
                    lists:nthtail(3, Rows7)
                ],
                Limits
            ),
            Expected
        )
    ),
    %% 全部中间前缀预算充足
    e(prefixes_ok(Rows7, Limits)),
    %% 最高 revision 兼容：键 1 取 counter 2 的行，键 2 取 counter 2
    e(element(3, maps:get(1, Expected)) =:= {2, node()}),
    e(element(1, maps:get(2, Expected)) =:= unavailable),
    ok.

w_m01_single_row() ->
    Exp = base_ms() + 120000,
    R = row({4, single, Exp, {1, node()}}),
    e(ok_case(final([[R]], limits()), expected_final([R]))),
    ok.

w_m02_idempotent() ->
    Raws = [
        {5, live, 21, unknown, {1, node()}},
        {6, live, 22, {node(), <<4:16>>, 1}, {2, node()}}
    ],
    Rows7 = [row(wire_of(R)) || R <- Raws],
    Expected = expected_final(Rows7),
    %% victim（known 来源行）重复 3 次穿插，结果不变
    V = lists:nth(2, Rows7),
    Dup = [V, hd(Rows7), V, V],
    e(ok_case(final([Dup], limits()), Expected)),
    ok.

w_m02_page_variants() ->
    Raws = [
        {7, live, 31, unknown, {1, node()}},
        {8, deleted, 0, unknown, {2, node()}},
        {9, live, 33, {node(), <<5:16>>, 1}, {3, node()}}
    ],
    Rows7 = [row(wire_of(R)) || R <- Raws],
    Legal = [lists:sublist(Rows7, 2), lists:nthtail(2, Rows7)],
    Limits = limits(),
    %% 缺页（页 2 缺席仍声明 pages=2）
    e(element(1, final_raw([hd(Legal)], 2, Limits)) =:= error),
    %% 重复页号
    e(element(1, final_raw(Legal ++ [hd(Legal)], 3, Limits)) =:= error),
    %% 跳号（1..3 去掉 2）
    e(
        element(1, final_raw([{1, hd(Legal)}, {3, lists:nth(2, Legal)}], 3, Limits)) =:=
            error
    ),
    %% 区分正控：页乱序合法
    e(ok_case(final(lists:reverse(Legal), Limits), expected_final(Rows7))),
    ok.

w_m03_matrix() ->
    K = 3,
    Exp = base_ms() + 120000,
    R1 = {1, node()},
    R2 = {2, node()},
    Live = fun(V, Rev) -> row({K, V, Exp, Rev}) end,
    Gone = fun(Kind, Rev) -> row({Kind, K, undefined, Exp, Rev}) end,
    Cases = [
        %% 四类同 rev 冲突 → error（两序）
        {Live(41, R2), Live(42, R2), error},
        {row({K, 41, Exp, R2}), row({K, 41, Exp + 1, R2}), error},
        {Live(41, R2), Gone(deleted, R2), error},
        {
            row({with_origin, {node(), <<1:16>>, 1}, {K, 41, Exp, R2}}),
            row({with_origin, {node(), <<2:16>>, 1}, {K, 41, Exp, R2}}),
            error
        },
        %% 兼容正控
        {Live(41, R2), Gone(unavailable, R2), Live(41, R2)},
        {Live(41, R2), Gone(expired, R2), Live(41, R2)},
        begin
            Known = row({with_origin, {node(), <<7:16>>, 3}, {K, 41, Exp, R2}}),
            {Live(41, R2), Known, Known}
        end,
        %% 高 revision 与 kind 无关
        {Live(41, R2), Gone(deleted, R1), Live(41, R2)},
        {Gone(deleted, R2), Live(41, R1), Gone(deleted, R2)}
    ],
    lists:foreach(
        fun({A, B, Winner}) ->
            e(check(final([[A], [B]], limits()), K, Winner)),
            e(check(final([[B], [A]], limits()), K, Winner))
        end,
        Cases
    ),
    %% 反遮蔽：冲突对直接相遇，即便存在更高 rev 行也拒绝
    {A1, B1, _} = hd(Cases),
    Higher = row({K, 99, Exp, {9, node()}}),
    e(check(final([[A1, B1, Higher]], limits()), K, error)),
    %% 更高 rev 真正介入归并 → 定局为其行
    e(check(final([[A1, Higher, B1]], limits()), K, Higher)),
    e(check(final([[Higher, A1, B1]], limits()), K, Higher)),
    ok.
