%%% @doc TSID V10 W2 专用：时间算法双候选原型（临时模块，W6 删除）。
%%%
%%% 候选 A（fixed_offset）：init 时采集 Offset = system_time - monotonic_time，
%%%   运行期 wall = monotonic_time + Offset。系统时钟运行期跳变对生成器不可见
%%%   （转化为纯时间戳漂移）。
%%% 候选 B（system）：每次读 erlang:system_time。真实墙钟直读。
%%%
%%% 两候选共用同一套 reserve（CAS + V10 无借支规则）：
%%%   禁止 Sequence 耗尽时主动推进逻辑毫秒——仅当 First 开启的新毫秒
%%%   既超出时钟当前 Now、也超出游标当前毫秒时，才有界等待真实时间推进；
%%%   超时 typed error。游标自身毫秒内的剩余槽位可继续消费（吸收回拨/前跳，
%%%   单次运行内绝不重复）。
%%%
%%% 时钟注入：测试进程 put({tsid_w2_proto, mono}, Ms) / put({tsid_w2_proto, sys}, Ms)
%%% 覆盖两个时钟源；未注入时用真实时钟。
-module(tsid_w2_proto).

-export([
    new_clock/1,
    clock_ms/1,
    clock_drift_ms/1,
    offset_ms/1,
    reserve/2,
    slot_to_id/2
]).

-define(EPOCH_MS, 1735689600000).
-define(SEQUENCE_BITS, 11).
-define(NODE_BITS, 10).
-define(TIMESTAMP_SHIFT, (?NODE_BITS + ?SEQUENCE_BITS)).
-define(SEQUENCE_MASK, ((1 bsl ?SEQUENCE_BITS) - 1)).
-define(MAX_REL_TS, ((1 bsl 42) - 1)).

-type clock_cfg() :: #{
    type := fixed_offset | system,
    offset_ms := integer(),
    wait_timeout_ms := pos_integer()
}.

%% @doc 构造时钟候选。fixed_offset 在此刻采集 Offset。
-spec new_clock(fixed_offset | system) -> clock_cfg().
new_clock(fixed_offset) ->
    #{
        type => fixed_offset,
        offset_ms => erlang:system_time(millisecond) - erlang:monotonic_time(millisecond),
        wait_timeout_ms => 100
    };
new_clock(system) ->
    #{type => system, offset_ms => 0, wait_timeout_ms => 100}.

-spec offset_ms(clock_cfg()) -> integer().
offset_ms(#{offset_ms := O}) ->
    O.

%% @doc 候选时钟的当前毫秒（绝对墙钟语义）
-spec clock_ms(clock_cfg()) -> integer().
clock_ms(#{type := fixed_offset, offset_ms := O}) ->
    mono_ms() + O;
clock_ms(#{type := system}) ->
    sys_ms().

%% @doc 观测：候选时钟与真实系统时钟的当前偏差（漂移测量用）
-spec clock_drift_ms(clock_cfg()) -> integer().
clock_drift_ms(Cfg) ->
    clock_ms(Cfg) - erlang:system_time(millisecond).

%% @doc V10 无借支 reserve：单值。deadline 顶层建立并在递归中守恒。
-spec reserve(atomics:atomics_ref(), clock_cfg()) ->
    {ok, pos_integer()} | {error, term()}.
reserve(Cursor, Cfg) ->
    Deadline = erlang:monotonic_time(millisecond) + maps:get(wait_timeout_ms, Cfg),
    do_reserve(Cursor, Cfg, Deadline).

do_reserve(Cursor, Cfg, Deadline) ->
    Now = clock_ms(Cfg) - ?EPOCH_MS,
    case Now < 0 of
        true ->
            {error, {clock_before_epoch, Now}};
        false ->
            case Now > ?MAX_REL_TS of
                true -> {error, {timestamp_exhausted, Now}};
                false -> do_reserve_cas(Cursor, Cfg, Now, Deadline)
            end
    end.

do_reserve_cas(Cursor, Cfg, Now, Deadline) ->
    Old = atomics:get(Cursor, 1),
    First = max(Old + 1, Now bsl ?SEQUENCE_BITS),
    FirstTs = First bsr ?SEQUENCE_BITS,
    CurTs = Old bsr ?SEQUENCE_BITS,
    case FirstTs > Now andalso FirstTs > CurTs of
        true ->
            %% 无借支铁律：不开辟超出时钟与游标的新毫秒——有界等待
            case wait_step(Deadline, FirstTs - Now) of
                ok -> do_reserve(Cursor, Cfg, Deadline);
                timeout -> {error, {capacity_exhausted, #{lead_ms => FirstTs - Now}}}
            end;
        false ->
            case FirstTs > ?MAX_REL_TS of
                true ->
                    {error, {timestamp_exhausted, FirstTs}};
                false ->
                    case atomics:compare_exchange(Cursor, 1, Old, First) of
                        ok -> {ok, slot_to_id(First, 0)};
                        _ -> do_reserve_cas(Cursor, Cfg, clock_ms(Cfg) - ?EPOCH_MS, Deadline)
                    end
            end
    end.

-spec slot_to_id(non_neg_integer(), 0..1023) -> pos_integer().
slot_to_id(Slot, Node) ->
    Ts = Slot bsr ?SEQUENCE_BITS,
    Seq = Slot band ?SEQUENCE_MASK,
    (Ts bsl ?TIMESTAMP_SHIFT) bor (Node bsl ?SEQUENCE_BITS) bor Seq.

%% ------------------------------------------------------------------
%% 内部
%% ------------------------------------------------------------------

wait_step(Deadline, NeededMs) when NeededMs > 0 ->
    Before = erlang:monotonic_time(millisecond),
    case Before >= Deadline of
        true ->
            timeout;
        false ->
            Sleep = erlang:max(1, erlang:min(NeededMs, Deadline - Before)),
            timer:sleep(Sleep),
            case erlang:monotonic_time(millisecond) > Before of
                true -> ok;
                false -> timeout
            end
    end.

mono_ms() ->
    case get({?MODULE, mono}) of
        undefined -> erlang:monotonic_time(millisecond);
        Ms -> Ms
    end.

sys_ms() ->
    case get({?MODULE, sys}) of
        undefined -> erlang:system_time(millisecond);
        Ms -> Ms
    end.
