%% IMBOY_ERLANG_QUALITY P2-06：S01 TSID 位布局/slot 往返 + S02 区间预留。
%% 合同核验：42/10/11 属实（TIMESTAMP/NODE/SEQUENCE_BITS），EPOCH=1735689600000。
%% 独立模型（MODEL01）：手写位算术常量（不 include elib_tsid 头、不复用其
%% 宏/函数），有界整数推导，无逐毫秒遍历。
-module(imboy_quality_pbt_tsid_props).

-export([
    prop_s01_bit_layout_slot_roundtrip/0,
    prop_s02_interval_reservation/0,
    witnesses_ok/0
]).

-include_lib("proper/include/proper.hrl").

%% 独立模型常量（与 SUT 宏同值由见证钉死；模型自身不引用 SUT）
-define(M_TS_BITS, 42).
-define(M_NODE_BITS, 10).
-define(M_SEQ_BITS, 11).
-define(M_EPOCH, 1735689600000).
-define(M_TS_SHIFT, 21).
-define(M_NODE_SHIFT, 11).
-define(M_SEQ_MASK, 2047).
-define(M_MAX_REL_TS, (1 bsl 42) - 1).
-define(M_MAX_CHUNK, 2048).

m_id(Ts, Node, Seq) ->
    (Ts bsl ?M_TS_SHIFT) bor (Node bsl ?M_NODE_SHIFT) bor Seq.

m_slot(Ts, Seq) ->
    (Ts bsl ?M_SEQ_BITS) bor Seq.

%% S02 独立预留模型：镜像"滚入"合同（起点中段顶边界 → 下一毫秒 seq 0）。
m_reserve(OldCursor, NowRel, Count) ->
    First0 = max(OldCursor + 1, NowRel bsl ?M_SEQ_BITS),
    First =
        case First0 band ?M_SEQ_MASK of
            Seq0 when Seq0 > 0, Seq0 + Count > ?M_MAX_CHUNK ->
                ((First0 bsr ?M_SEQ_BITS) + 1) bsl ?M_SEQ_BITS;
            _ ->
                First0
        end,
    {First, First + Count - 1}.

%% ---- S01 ----
prop_s01_bit_layout_slot_roundtrip() ->
    ?FORALL(
        {Ts, Node, Seq},
        {integer(0, ?M_MAX_REL_TS), integer(0, 1023), integer(0, ?M_SEQ_MASK)},
        begin
            Id = m_id(Ts, Node, Seq),
            Id >= 1 andalso
                %% SUT slot_to_id/id_to_slot 与独立模型一致（往返保 slot）
                (elib_tsid:slot_to_id(m_slot(Ts, Seq), Node) =:= Id) andalso
                (elib_tsid:id_to_slot(Id) =:= m_slot(Ts, Seq)) andalso
                %% SUT 访问器与独立模型一致
                (elib_tsid:timestamp(Id) =:= Ts + ?M_EPOCH) andalso
                (elib_tsid:node_id(Id) =:= Node) andalso
                (maps:get(id, elib_tsid:parse(Id)) =:= Id)
        end
    ).

%% ---- S02 ----
prop_s02_interval_reservation() ->
    ?FORALL(
        {Ts0, Seq0, Count, NowRel},
        {
            integer(0, ?M_MAX_REL_TS),
            integer(0, ?M_SEQ_MASK),
            integer(1, ?M_MAX_CHUNK + 1),
            integer(0, ?M_MAX_REL_TS)
        },
        begin
            OldCursor = m_slot(Ts0, Seq0),
            case elib_tsid:reserve_candidate(OldCursor, NowRel, Count) of
                {ok, First, Last} ->
                    Count =< ?M_MAX_CHUNK andalso
                        (m_reserve(OldCursor, NowRel, Count) =:= {First, Last}) andalso
                        First > OldCursor andalso
                        Last - First + 1 =:= Count andalso
                        %% 区间绝不跨毫秒
                        (First bsr ?M_SEQ_BITS) =:= (Last bsr ?M_SEQ_BITS) andalso
                        First >= (NowRel bsl ?M_SEQ_BITS);
                {error, {elib_tsid_invalid_input, #{count := Count}}} ->
                    Count > ?M_MAX_CHUNK;
                {error, {elib_tsid_timestamp_exhausted, _}} ->
                    %% 顶到 42 位上界（独立判定）
                    {_, LastM} = m_reserve(OldCursor, NowRel, min(Count, ?M_MAX_CHUNK)),
                    (LastM bsr ?M_SEQ_BITS) > ?M_MAX_REL_TS;
                {error, _Other} ->
                    false
            end
        end
    ).

%% ---- 强制见证（TEST_PLAN S01/S02 mandatory witnesses） ----

witnesses_ok() ->
    W = [
        {"s1_node_extremes", fun s1/0},
        {"s2_seq_extremes", fun s2/0},
        {"s3_max_timestamp", fun s3/0},
        {"s4_all_zero_id_excluded", fun s4/0},
        {"s5_count_1_2048_2049", fun s5/0},
        {"s6_clock_rollback_resists", fun s6/0},
        {"s7_before_epoch", fun s7/0},
        {"s8_last_bucket", fun s8/0},
        {"s9_layout_constants", fun s9/0}
    ],
    case [Name || {Name, F} <- W, F() =/= ok] of
        [] -> ok;
        Failed -> {error, {witness_failed, Failed}}
    end.

s1() ->
    e(elib_tsid:node_id(m_id(5, 0, 0)) =:= 0 andalso elib_tsid:node_id(m_id(5, 1023, 0)) =:= 1023).

s2() ->
    e(elib_tsid:parse(m_id(7, 3, 0)) =/= error andalso elib_tsid:parse(m_id(7, 3, 2047)) =/= error).

s3() ->
    Id = m_id(?M_MAX_REL_TS, 1023, 2047),
    e(elib_tsid:timestamp(Id) =:= ?M_MAX_REL_TS + ?M_EPOCH).

s4() ->
    %% 全 0 ID 不在合法域（1..MAX_ID）；error/1 异常 catch 后形态 {'EXIT', Reason}
    case catch elib_tsid:parse(0) of
        {'EXIT', {{elib_tsid_invalid_input, _}, _Stack}} -> ok;
        Other -> {mismatch, Other}
    end.

s5() ->
    {ok, F1, L1} = elib_tsid:reserve_candidate(0, 100, 1),
    {ok, F2, L2} = elib_tsid:reserve_candidate(0, 100, 2048),
    {error, {elib_tsid_invalid_input, _}} = elib_tsid:reserve_candidate(0, 100, 2049),
    e(F1 =:= L1 andalso L2 - F2 + 1 =:= 2048 andalso F2 =:= 100 bsl 11).

s6() ->
    %% 回拨：NowRel 落后于旧 cursor → 区间仍从 Old+1 起步（不回拨）
    Old = m_slot(200, 5),
    {ok, F, _} = elib_tsid:reserve_candidate(Old, 100, 3),
    e(F =:= Old + 1).

s7() ->
    e(
        element(1, elib_tsid:reserve_candidate(0, -1, 1)) =:= error andalso
            element(1, element(2, elib_tsid:reserve_candidate(0, -1, 1))) =:=
                elib_tsid_clock_before_epoch
    ).

s8() ->
    %% 最后桶：MAX_REL_TS 从 seq0 预留 2048 恰好满（ok，末位 slot=2047）；
    %% 已在最后 slot 之后再预留 1 → timestamp_exhausted
    {ok, _, Last} = elib_tsid:reserve_candidate(
        m_slot(?M_MAX_REL_TS, 0) - 1, ?M_MAX_REL_TS, ?M_MAX_CHUNK
    ),
    {error, {elib_tsid_timestamp_exhausted, _}} =
        elib_tsid:reserve_candidate(m_slot(?M_MAX_REL_TS, ?M_SEQ_MASK), ?M_MAX_REL_TS, 1),
    e(Last =:= m_slot(?M_MAX_REL_TS, ?M_SEQ_MASK)).

s9() ->
    %% 布局常量核验：独立模型与 SUT 实际行为一致（42/10/11 + EPOCH）
    e(
        elib_tsid:timestamp(m_id(0, 0, 1)) =:= ?M_EPOCH andalso
            elib_tsid:timestamp(1) =:= ?M_EPOCH andalso
            elib_tsid:node_id(1 bsl ?M_NODE_SHIFT) =:= 1
    ).

e(true) -> ok;
e(false) -> error.
