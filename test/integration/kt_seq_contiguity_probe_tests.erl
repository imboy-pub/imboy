%% @doc E2EE-065 Slice 1：**`bigserial` 与「唯一连续位置」的冲突**——真 PG 实证探针。
%%
%% == 为什么需要它 ==
%%
%% `28-e2ee-065-066-key-transparency-research.md` §2.1 给 KT 日志设计的
%% `identity_log` 用 `bigserial seq`（与既有 `trust_audit` 同范式），而
%% playbook E2EE-033 的验收标准写着「并发 append 1000 events 得到**唯一连续**位置」。
%%
%% 设计文档把两者的冲突标为 **未实证**，并明确要求：
%%   「不得凭 PostgreSQL 通例推断，须在真 PG 上验证后再定表结构」。
%%
%% 本文件就是那次验证。**不实施 KT，不新增任何生产代码**——只建一张一次性探针表，
%% 测完即 DROP。
%%
%% == 探针问题 ==
%%
%% 1. 顺序提交时 seq 是否连续？（对照组：若这条红，说明探针本身坏了）
%% 2. 事务回滚后序号是否回收？
%% 3. **序号分配顺序是否等于提交可见顺序？**
%%    这条才是对 Merkle 日志致命的：若不等，按 seq 扫描建树的读者会看到一个
%%    「空洞」，而该空洞稍后**追溯填上**——同一 tree size 先后算出不同 root，
%%    consistency proof 直接失效。
-module(kt_seq_contiguity_probe_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TB, <<"public.kt_seq_probe">>).
%% 等待对端事务动作的上限：超时即让用例红，而不是挂住
-define(SYNC_TIMEOUT, 15000).

kt_seq_probe_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {setup, fun setup/0, fun cleanup/1, [
                {"对照组：顺序提交时 seq 连续", fun sequential_is_contiguous/0},
                {"回滚不回收序号 → 产生空洞", fun rollback_leaves_gap/0},
                {"序号分配顺序 ≠ 提交可见顺序 → 空洞会追溯填上", fun visibility_hole_backfills/0}
            ]};
        {error, _Reason} ->
            {"Database not available", fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    _ = elib_pg:execute(<<"DROP TABLE IF EXISTS ", ?TB/binary>>, []),
    _ = elib_pg:execute(
        <<"CREATE TABLE ", ?TB/binary, " (seq bigserial PRIMARY KEY, note text)">>, []
    ),
    ok.

cleanup(_) ->
    _ = elib_pg:execute(<<"DROP TABLE IF EXISTS ", ?TB/binary>>, []),
    ok.

%% 每个用例独立清表，避免结果依赖执行顺序（序列不重置，故断言用「相对关系」）
truncate() ->
    _ = elib_pg:execute(<<"TRUNCATE ", ?TB/binary>>, []),
    ok.

%% ⚠️ elib_pg:query/2 返回 {ok, [Map]}（map 列表），**不是** epgsql 的
%% {ok, Cols, Rows} 三元组。本文件第一次运行时对照组即因此变红——
%% 按「对照组红=harness 缺陷」的规则停下重估后修正，见 evidence §2.1。
insert_one(Note) ->
    {ok, [Row]} = elib_pg:query(
        <<"INSERT INTO ", ?TB/binary, " (note) VALUES ($1) RETURNING seq">>, [Note]
    ),
    binary_to_integer_maybe(maps:get(<<"seq">>, Row)).

binary_to_integer_maybe(N) when is_integer(N) -> N;
binary_to_integer_maybe(B) when is_binary(B) -> binary_to_integer(B).

visible_seqs() ->
    {ok, Rows} = elib_pg:query(<<"SELECT seq FROM ", ?TB/binary, " ORDER BY seq">>, []),
    [binary_to_integer_maybe(maps:get(<<"seq">>, R)) || R <- Rows].

%% ===================================================================

%% 对照组：这条红 = 探针本身坏了（连不上库 / 表没建对 / 查询有误），
%% 此时后两条的任何结论都不成立，必须停下重估。
sequential_is_contiguous() ->
    truncate(),
    Seqs = [insert_one(<<"seq">>) || _ <- lists:seq(1, 5)],
    Diffs = diffs(Seqs),
    ?assertEqual(
        [1, 1, 1, 1],
        Diffs,
        "顺序提交时相邻 seq 必须差 1；若不成立说明探针本身有问题，后续结论不可信"
    ),
    ?assertEqual(Seqs, visible_seqs()).

rollback_leaves_gap() ->
    truncate(),
    Before = insert_one(<<"before">>),
    %% 事务内取号后主动回滚
    {rollback, probe} = elib_pg:with_tx(fun(Conn) ->
        {ok, 1, _, _} = epgsql:equery(
            Conn, "INSERT INTO public.kt_seq_probe (note) VALUES ($1) RETURNING seq", [
                <<"doomed">>
            ]
        ),
        throw({rollback, probe})
    end),
    After = insert_one(<<"after">>),
    ?assert(
        After > Before + 1,
        "回滚的事务不归还序号 → 产生空洞；"
        "若 KT 把 seq 直接当 leaf index，空洞即树里的洞"
    ),
    %% 回滚那行不可见：空洞是「永久缺席」，不是「稍后出现」
    ?assertEqual([Before, After], visible_seqs()).

%% ⚠️ 本探针的核心问题。
%% A 先取号但**不提交**，B 后取号并提交。此刻按 seq 扫描的读者看得到 B、看不到 A；
%% A 提交后，那个低序号**追溯出现**。
visibility_hole_backfills() ->
    truncate(),
    Parent = self(),
    Worker = spawn_link(fun() ->
        elib_pg:with_tx(fun(Conn) ->
            {ok, 1, _, [{SeqA}]} = epgsql:equery(
                Conn, "INSERT INTO public.kt_seq_probe (note) VALUES ($1) RETURNING seq", [
                    <<"slow">>
                ]
            ),
            Parent ! {taken, self(), binary_to_integer_maybe(SeqA)},
            %% 卡在事务里，等父进程发话再提交
            receive
                commit_now -> ok
            after ?SYNC_TIMEOUT -> ok
            end,
            ok
        end),
        Parent ! {committed, self()}
    end),

    SeqA =
        receive
            {taken, Worker, S} -> S
        after ?SYNC_TIMEOUT -> erlang:error(worker_never_took_a_seq)
        end,

    %% B：后取号、先提交
    SeqB = insert_one(<<"fast">>),
    ?assert(SeqB > SeqA, "B 的序号必须大于 A（A 先取号）"),

    %% 此刻扫描：只见 B，不见 A —— 这就是那个「洞」
    MidScan = visible_seqs(),
    ?assertEqual(
        [SeqB],
        MidScan,
        "先分配的 SeqA 尚未提交，按 seq 扫描的读者会看到一个空洞"
    ),

    %% A 提交 → 低序号追溯出现
    Worker ! commit_now,
    receive
        {committed, Worker} -> ok
    after ?SYNC_TIMEOUT -> erlang:error(worker_never_committed)
    end,

    FinalScan = visible_seqs(),
    ?assertEqual(
        [SeqA, SeqB],
        FinalScan,
        "空洞被追溯填上：同一 tree size 先后算出不同 root，consistency proof 失效"
    ).

diffs([_]) ->
    [];
diffs([A, B | Rest]) ->
    [B - A | diffs([B | Rest])].
