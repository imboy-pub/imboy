%% IMBOY_ERLANG_QUALITY P2-07：Q01 TSID 短序列独立解释器模型。
%% 每步 = set_time(Ts) | reserve(Count)；模型逐步折叠并与 SUT
%% reserve_candidate 对照。只声称纯序列保证（不冒称 CAS/多节点）。
%% MODEL02：初态每例重建（模型局部状态，无进程全局 cursor 重置）。
-module(imboy_quality_pbt_tsid_seq_props).

-export([prop_q01_reserve_sequences/0, witnesses_ok/0]).

-include_lib("proper/include/proper.hrl").

-define(SEQ_BITS, 11).
-define(MAX_CHUNK, 2048).
-define(MAX_REL_TS, (1 bsl 42) - 1).

command() ->
    oneof([
        {set_time, integer(0, ?MAX_REL_TS)},
        {reserve, integer(1, ?MAX_CHUNK + 1)}
    ]).

prop_q01_reserve_sequences() ->
    ?FORALL(
        Cmds,
        resize(20, list(command())),
        run_model(Cmds, #{cursor => 0, now => 0, last_issued => none})
    ).

%% 独立解释器：模型状态折叠；任何 SUT/模型分歧立即 false。
run_model([], _State) ->
    true;
run_model([{set_time, Ts} | Rest], State) ->
    run_model(Rest, State#{now := Ts});
run_model([{reserve, Count} | Rest], #{cursor := Cursor, now := Now} = State) ->
    case elib_tsid:reserve_candidate(Cursor, Now, Count) of
        {ok, First, Last} ->
            %% 模型预期：成功区间严格推进、不重叠、不跨毫秒、不回拨
            ModelOk =
                First > Cursor andalso
                    Last >= First andalso
                    (First bsr ?SEQ_BITS) =:= (Last bsr ?SEQ_BITS) andalso
                    First >= (Now bsl ?SEQ_BITS),
            case ModelOk of
                true -> run_model(Rest, State#{cursor := Last});
                false -> false
            end;
        {error, _Reason} ->
            %% 失败不推进：模型 cursor 不变（且后续行为仍可预测）
            run_model(Rest, State)
    end.

%% ---- 强制见证 ----

witnesses_ok() ->
    W = [
        {"q1_normal_entry_fixed", fun q1/0},
        {"q2_failure_no_advance", fun q2/0},
        {"q3_rollback_sequence", fun q3/0},
        {"q4_nonoverlap_adjacent", fun q4/0},
        {"q5_midms_rollover_seq", fun q5/0}
    ],
    case [Name || {Name, F} <- W, F() =/= ok] of
        [] -> ok;
        Failed -> {error, {witness_failed, Failed}}
    end.

q1() ->
    %% 正常入口固定见证：now=100 连续两批
    {ok, F1, L1} = elib_tsid:reserve_candidate(0, 100, 10),
    {ok, F2, L2} = elib_tsid:reserve_candidate(L1, 100, 10),
    e(F1 =:= 100 bsl ?SEQ_BITS andalso L1 =:= F1 + 9 andalso F2 =:= L1 + 1 andalso L2 =:= F2 + 9).

q2() ->
    %% 失败不推进：exhausted 后 cursor 保持，重试同参仍同失败
    E1 = elib_tsid:reserve_candidate(?MAX_REL_TS bsl ?SEQ_BITS + 2047, ?MAX_REL_TS, 1),
    E2 = elib_tsid:reserve_candidate(?MAX_REL_TS bsl ?SEQ_BITS + 2047, ?MAX_REL_TS, 1),
    e(element(1, E1) =:= error andalso E1 =:= E2).

q3() ->
    %% 回拨序列：时间倒退后区间仍前进（Old+1 优先）
    {ok, A, _} = elib_tsid:reserve_candidate(500 bsl ?SEQ_BITS, 500, 1),
    {ok, B, _} = elib_tsid:reserve_candidate(A, 100, 1),
    e(B =:= A + 1).

q4() ->
    %% 相邻批次不重叠：F_{n+1} = L_n + 1
    {ok, _, L1} = elib_tsid:reserve_candidate(0, 7, 2048),
    {ok, F2, _} = elib_tsid:reserve_candidate(L1, 7, 1),
    e(F2 =:= L1 + 1).

q5() ->
    %% 毫秒中段顶界滚入：起点 seq=1000、Count=1500 → 滚入下一毫秒 seq0
    Start = ((7 bsl ?SEQ_BITS) + 1000),
    {ok, F, L} = elib_tsid:reserve_candidate(Start - 1, 7, 1500),
    e(F =:= 8 bsl ?SEQ_BITS andalso L =:= F + 1499).

e(true) -> ok;
e(false) -> error.
