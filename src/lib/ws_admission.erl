-module(ws_admission).
%%%===================================================================
%%% R0（P1-1 修复；属主批准 D0–D5）单节点 WebSocket 准入控制
%%%
%%% 背景：ranch `max_connections` 约束不了已建立的 WebSocket——cowboy 在
%%% 升级时调用 `ranch:remove_connection/1`（deps/cowboy/src/
%%% cowboy_websocket.erl:325-332）。要给 WS 设上限必须在应用层准入。
%%%
%%% 预算语义（属主批准的强制约束）：
%%% - 上限 `{imboy, max_ws_connections}`（env `IMBOY_MAX_WS_CONNECTIONS`，
%%%   默认 20000）是**单节点总准入上限**，不是 per-listener；http_tls
%%%   双 listener 天然共享同一张全局表。
%%% - 并发握手不超额准入：admit 用 `ets:update_counter` 原子自增 + 超限
%%%   回滚（OTP 29.0.2 实证：阈值式 `{Pos,Incr,Threshold,SetValue,Reset}`
%%%   在本仓场景恒 badarg，不可用；2-op 形态 50 轮 × 8 进程竞态零违例）。
%%% - 名额生命周期 = ws 循环进程生命周期：`websocket_handler:init/2` 只做
%%%   满载软闸（503，不占名额——cowboy supervisor 模式下升级会替换该进程），
%%%   名额由幸存进程在 `websocket_init/1` 原子认领，`terminate/3` 释放；
%%%   硬杀/异常退出由 sweep 清道夫按 `is_process_alive` 兜底
%%%   （take 配对递减，幂等，无双重释放）。
%%% - 计数自愈：每次 sweep 末尾做 count↔rows 对账，漂移先静置复核再修复，
%%%   不允许计数泄漏导致永久拒绝。
%%% - 在线状态真源仍是 syn（?CHAT_SCOPE，含 AI agent 成员）；本表是容量
%%%   预算，两者语义不同，漂移可经 metrics 观测。
%%% - 服务重启后表随 VM 重建为 0，与"重启后无连接"的真实状态天然一致。
%%%
%%% 表持有：由长驻 application master 进程建表（imboy_app:start 调
%%% init_table/0）——同 ack_retry_cache/init_table 惯用法，避免表被首个
%%% 短命 WS 进程随断线销毁。
%%%===================================================================

-export([
    validate_config/0,
    init_table/0,
    admit/0,
    release/0,
    count/0,
    rejected_total/0,
    limit/0,
    sweep/0
]).

-include("log.hrl").

-define(TAB, ws_admission_tab).
-define(COUNT_KEY, admitted_count).
-define(REJECTED_KEY, rejected_total).
%% C2 档（评审稿 §4.1，D1 批准值）
-define(DEFAULT_MAX_WS_CONNECTIONS, 20000).
%% 与 +P/+Q 65536 的预算模型一致的上界（评审稿 §3.4）
-define(LIMIT_CEILING, 1000000).
%% 清道夫周期：异常退出（brutal kill 等 terminate 不运行的路径）后的
%% 最长泄漏窗口。20k 行全表 is_process_alive 扫描为毫秒级，5s 成本可忽略。
-define(SWEEP_INTERVAL_MS, 5000).
%% 对账静置窗：admit/release 都是两步 ets 操作（µs 级），漂移需在静置后
%% 复测仍在才修复——瞬时窗口（count>rows 方向，偏保守）不触发修复。
-define(REPAIR_SETTLE_MS, 500).
-define(REJECT_LOG_INTERVAL_MS, 10000).
-define(SWEEP_TIMER_KEY, ws_admission_sweep_timer).

-type admit_result() :: {ok, non_neg_integer()} | {rejected, non_neg_integer()}.
-export_type([admit_result/0]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 启动期 fail-fast 校验（imboy_app:start，validate_drain_config 同款）。
%% 非整数/非正数/超上界 → 拒启；**不静默回退为无限制**。
-spec validate_config() -> ok.
validate_config() ->
    case application:get_env(imboy, max_ws_connections, ?DEFAULT_MAX_WS_CONNECTIONS) of
        N when is_integer(N), N >= 1, N =< ?LIMIT_CEILING ->
            ok;
        Bad ->
            logger:error(
                "[imboy] invalid {imboy, max_ws_connections} = ~p: expected an integer "
                "in 1..~b. Refusing to start with an unusable WebSocket admission "
                "budget (no silent unlimited fallback).",
                [Bad, ?LIMIT_CEILING]
            ),
            erlang:error({invalid_config, max_ws_connections, Bad})
    end.

%% @doc 建表 + 武装清道夫定时器。必须由长驻 application master 进程调用
%% （imboy_app:start/2）；否则表会随首个短命 WS 连接进程退出而销毁。
%% 幂等：表已存在（eunit/重复 init）时不重建、定时器先取消再武装。
-spec init_table() -> ok.
init_table() ->
    ok = ensure_table(),
    arm_sweep_timer(),
    logger:info(
        "[capacity] process_limit=~b port_limit=~b schedulers_online=~b "
        "max_ws_connections=~b ws_admitted=~b",
        [
            erlang:system_info(process_limit),
            erlang:system_info(port_limit),
            erlang:system_info(schedulers_online),
            limit(),
            count()
        ]
    ),
    ok.

%% @doc 原子认领一个 WebSocket 连接名额（由幸存的 ws 循环进程在
%% websocket_init/1、注册在线之前自调用）。
%% 原子性：`ets:update_counter` 单格自增是原子的；观察到值 ≤ 上限才准入，
%% 超限立即回滚自增——并发握手不可能同时观察到 ≤ 上限的重复票号，
%% 故已提交的准入数永不超上限（EUnit 并发竞态用例守恒）。
-spec admit() -> admit_result().
admit() ->
    ok = ensure_table(),
    N = bump(?COUNT_KEY, 1),
    case N > limit() of
        true ->
            ok = add_count(-1),
            _ = bump(?REJECTED_KEY, 1),
            maybe_log_rejected(),
            {rejected, max(N - 1, 0)};
        false ->
            %% insert（非 insert_new）：若同 pid 留有陈旧行（pid 复用/重复
            %% admit 的异常态），覆写行、名额计数自愈于 release/sweep 对账
            ets:insert(?TAB, {{conn, self()}, erlang:system_time(millisecond)}),
            {ok, N}
    end.

%% @doc 释放当前连接进程的名额。以 `ets:take` 的行存在性为配对依据：
%% 未准入/已释放/已被 sweep 清理的进程调用均为幂等 no-op，不产生
%% 双重递减；terminate 与显式释放路径重复调用安全。
-spec release() -> ok.
release() ->
    case ets:whereis(?TAB) of
        undefined ->
            ok;
        _Tab ->
            case ets:take(?TAB, {conn, self()}) of
                [] ->
                    ok;
                [_Taken] ->
                    ok = add_count(-1),
                    ok
            end
    end.

%% @doc 当前已准入（存活）WS 名额数。与 {{conn, pid()}, _} 行数按构造
%% 相等（每行增删都配对一个计数增减），sweep 对账兜底纠偏。
-spec count() -> non_neg_integer().
count() ->
    case ets:whereis(?TAB) of
        undefined ->
            0;
        _Tab ->
            case ets:lookup(?TAB, ?COUNT_KEY) of
                [{?COUNT_KEY, N}] when is_integer(N), N >= 0 -> N;
                _ -> 0
            end
    end.

%% @doc 本 VM 启动以来因超上限被拒绝的准入尝试数（Prometheus counter）。
-spec rejected_total() -> non_neg_integer().
rejected_total() ->
    case ets:whereis(?TAB) of
        undefined ->
            0;
        _Tab ->
            case ets:lookup(?TAB, ?REJECTED_KEY) of
                [{?REJECTED_KEY, N}] when is_integer(N), N >= 0 -> N;
                _ -> 0
            end
    end.

-spec limit() -> pos_integer().
limit() ->
    application:get_env(imboy, max_ws_connections, ?DEFAULT_MAX_WS_CONNECTIONS).

%% @doc 清道夫：清理"行存在但进程已死"的名额（brutal kill 等 terminate
%% 不运行的异常退出路径），随后对账纠偏。由 timer:apply_interval 周期
%% 触发（新进程每次独立运行，无状态，可安全重入）；测试可直接调用。
-spec sweep() -> ok.
sweep() ->
    case ets:whereis(?TAB) of
        undefined ->
            ok;
        _Tab ->
            ok = sweep_dead_slots(),
            ok = repair_drift(),
            ok
    end.

%% 行在、进程亡 → take 配对递减。先集中 take 再统一递减：取走与递减之间的
%% 瞬时窗口方向为 count>rows（偏保守），且不会与并发的 release 双重递减。
sweep_dead_slots() ->
    Pids = ets:select(?TAB, [{{{conn, '$1'}, '_'}, [], ['$1']}]),
    Swept = lists:foldl(fun take_dead_slot/2, 0, Pids),
    case Swept of
        0 ->
            ok;
        _ ->
            ok = add_count(-Swept),
            ok = ?WARN_LOG({ws_admission_swept_dead_slots, Swept, count()})
    end.

%% take 到行才计 1：其他进程（release / 并发 sweep）抢先清理时为 no-op。
take_dead_slot(Pid, Acc) ->
    case erlang:is_process_alive(Pid) of
        true ->
            Acc;
        false ->
            case ets:take(?TAB, {conn, Pid}) of
                [] -> Acc;
                [_] -> Acc + 1
            end
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_table() ->
    case ets:whereis(?TAB) of
        undefined ->
            try
                %% named table 返回表名本身（非 true）
                _ = ets:new(?TAB, [
                    set,
                    public,
                    named_table,
                    {keypos, 1},
                    {read_concurrency, true},
                    {write_concurrency, true}
                ]),
                %% 增量 0 的 update_counter = "缺则补 0、有不覆盖"：并发惰性建表
                %% 时后来者的初始化不会覆写先到者已自增的计数（insert 会）
                _ = ets:update_counter(?TAB, ?COUNT_KEY, 0, {?COUNT_KEY, 0}),
                _ = ets:update_counter(?TAB, ?REJECTED_KEY, 0, {?REJECTED_KEY, 0}),
                ok
            catch
                %% 并发建表竞态（首个 WS 进程惰性 ensure 撞上 app 启动）：
                %% 表已存在即目标态
                error:badarg -> ok
            end;
        _Tab ->
            ok
    end.

%% 计数格原子增减；缺格按 0 补（表被外部清空后的自愈起点）。
bump(Key, Delta) ->
    ets:update_counter(?TAB, Key, Delta, {Key, 0}).

%% 带下限夹取的准入计数增减：计数按构造不可为负，负值意味着外部损坏
%% （表被清/行被删），夹回 0 自愈，避免"负计数→永久拒绝"。
add_count(Delta) ->
    case bump(?COUNT_KEY, Delta) of
        N when N < 0 ->
            _ = bump(?COUNT_KEY, -N),
            ok = ?WARN_LOG({ws_admission_counter_clamped, N}),
            ok;
        _ ->
            ok
    end.

%% count↔rows 对账：漂移静置 ?REPAIR_SETTLE_MS 后复测仍在才修复——
%% admit/release 的两步窗口（µs 级）造成的瞬时漂移方向恒为 count>rows
%% （偏保守、宁可少收），不修复即自行消散。
repair_drift() ->
    Drift = count() - conn_rows(),
    case Drift of
        0 ->
            ok;
        _ ->
            timer:sleep(?REPAIR_SETTLE_MS),
            case count() - conn_rows() of
                Drift when Drift =/= 0 ->
                    _ = bump(?COUNT_KEY, -Drift),
                    ok = ?WARN_LOG({ws_admission_drift_repaired, Drift, count()}),
                    ok;
                _ ->
                    ok
            end
    end.

conn_rows() ->
    %% 固定键恒为两行（COUNT_KEY + REJECTED_KEY，ensure_table 初始化）
    case ets:info(?TAB, size) of
        undefined -> 0;
        Size -> max(Size - 2, 0)
    end.

%% 拒绝日志限速：风暴期最多每 ?REJECT_LOG_INTERVAL_MS 一条，防日志洪水
%% （阶段 B 实证：打满 +P 时 163 次失败可产出 2378 行错误日志）。
maybe_log_rejected() ->
    Now = erlang:system_time(millisecond),
    case persistent_term:get(ws_admission_last_reject_log, 0) of
        Last when Now - Last >= ?REJECT_LOG_INTERVAL_MS ->
            persistent_term:put(ws_admission_last_reject_log, Now),
            ok = ?WARN_LOG({ws_admission_rejected, count(), limit(), rejected_total()}),
            ok;
        _ ->
            ok
    end.

%% msg_rate_logic:init_table 同款定时器惯用法：先取消旧引用再武装，
%% 防多次 init 产生多个并行清道夫。旧引用的读取单列为带契约的函数——
%% persistent_term 对 dialyzer 是 term()，而 timer:cancel 只收 opaque 的
%% timer:tref()（该键只由本函数写入 TRef，不靠 nowarn_function 消音）。
arm_sweep_timer() ->
    case sweep_timer() of
        undefined ->
            ok;
        OldTRef ->
            _ = timer:cancel(OldTRef),
            ok
    end,
    {ok, TRef} =
        timer:apply_interval(?SWEEP_INTERVAL_MS, ?MODULE, sweep, []),
    persistent_term:put(?SWEEP_TIMER_KEY, TRef),
    ok.

-spec sweep_timer() -> timer:tref() | undefined.
sweep_timer() ->
    persistent_term:get(?SWEEP_TIMER_KEY, undefined).
