-module(agent_rate_limiter).

%%%
% Agent/Bot 触发限流 / LLM trigger rate limiter（Phase 1 遗留：金钱 DoS 闸门）
%
% 威胁：认证用户给 AI 发消息即触发 LLM 调用（花钱，属主/平台账单）。触发门控
% （agent_trigger_policy）只判「要不要回」，不判「回多快/多少」。恶意高频私聊/群 @/
% bot_* C2S 可打爆 LLM 计费。本模块在两条 LLM 触发路径的调用前加双维固定窗口闸门：
%   - per-(scope,requester)：单请求者对单 scope 的速率（挡单账号 flood）
%   - per-scope 总量        ：单 scope 的总速率（属主账单硬顶，挡多账号分布式刷）
% 覆盖的 scope：C2C agent 账号（ai_agent_reply:dispatch）+ bot_* C2S（msg_c2s_logic）。
%
% 实现镜像 msg_rate_logic 的房屋模式：
%   - ets:update_counter/4 单操作**原子**自增（读+判+写一次完成，无 read-then-write 竞态）；
%   - key 内嵌时间桶 Bucket=now div Window → **真固定窗口**，跨桶自然归零，
%     正确性不依赖任何定时器（定时器仅回收旧桶内存，故障也不影响限流正确性）；
%   - ensure_ready 惰性自愈建表（免 gen_server / 免 supervisor 接线，表恒在）。
%
% deny 时调用方静默丢弃（不回复/不花钱/不给攻击者信号）。
%
% ponytail: 单节点计数（各节点独立，够用于金钱 DoS 兜底）。仅有分钟级速率闸门，
% 无小时/日级预算硬顶——长期低速蚕食不在本闸门范围（留给 billing/license 配额位）。
%%%

-export([allow/2]).
%% 供定时器回调（旧桶内存回收）
-export([cleanup/0]).

-define(TAB, agent_rate_limiter_ets).
-define(TIMER_PT, agent_rate_limiter_timer).

-define(DEF_PER_REQUESTER, 30).
-define(DEF_PER_AGENT, 600).
-define(DEF_WINDOW_SECS, 60).

%% @doc 判定一次 LLM 触发是否放行。Scope=agent uid(integer) 或 bot 名(binary)。
%% deny 时调用方应静默丢弃（不触发 LLM）。
-spec allow(Scope :: integer() | binary(), FromUid :: integer()) ->
    allow | {deny, requester_rate | agent_rate}.
allow(Scope, FromUid) ->
    ok = ensure_ready(),
    Bucket = erlang:system_time(second) div window_secs(),
    case bump({req, Scope, FromUid, Bucket}, per_requester()) of
        over ->
            {deny, requester_rate};
        ok ->
            case bump({agent, Scope, Bucket}, per_agent()) of
                over -> {deny, agent_rate};
                ok -> allow
            end
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% 原子自增（update_counter 单操作原子，带默认值 {Key,0}）。首个请求返回 1，
%% 恰好允许 Max 次/窗口，第 Max+1 次返回 over。
-spec bump(term(), pos_integer()) -> ok | over.
bump(Key, Max) ->
    case ets:update_counter(?TAB, Key, {2, 1}, {Key, 0}) of
        Count when Count > Max -> over;
        _ -> ok
    end.

%% 惰性建表 + 惰性挂旧桶回收定时器（persistent_term 守卫，极端并发下可能重挂，
%% 无害：多一次清扫。正确性不依赖它，故不追求严格单例）。
-spec ensure_ready() -> ok.
ensure_ready() ->
    case ets:whereis(?TAB) of
        undefined ->
            try
                _ = ets:new(?TAB, [
                    set,
                    public,
                    named_table,
                    {write_concurrency, true},
                    {read_concurrency, true}
                ]),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end,
    ensure_timer().

-spec ensure_timer() -> ok.
ensure_timer() ->
    case persistent_term:get(?TIMER_PT, undefined) of
        Ref when is_reference(Ref) ->
            ok;
        _ ->
            %% 清理是非关键（正确性靠时间桶），timer 起不来也不能拖垮 allow
            case timer:apply_interval(window_secs() * 1000, ?MODULE, cleanup, []) of
                {ok, TRef} ->
                    persistent_term:put(?TIMER_PT, TRef),
                    ok;
                _ ->
                    ok
            end
    end.

%% @doc 删除早于「当前桶-1」的过期桶条目（仅内存回收；current 与 current-1 保留）
-spec cleanup() -> ok.
cleanup() ->
    case ets:whereis(?TAB) of
        undefined ->
            ok;
        _ ->
            Threshold = (erlang:system_time(second) div window_secs()) - 1,
            %% req key = {req, Scope, Uid, Bucket}；agent key = {agent, Scope, Bucket}
            _ = ets:select_delete(?TAB, [
                {{{req, '_', '_', '$1'}, '_'}, [{'<', '$1', Threshold}], [true]},
                {{{agent, '_', '$1'}, '_'}, [{'<', '$1', Threshold}], [true]}
            ]),
            ok
    end.

-spec per_requester() -> pos_integer().
per_requester() ->
    env_pos(agent_rl_per_requester, ?DEF_PER_REQUESTER).

-spec per_agent() -> pos_integer().
per_agent() ->
    env_pos(agent_rl_per_agent, ?DEF_PER_AGENT).

-spec window_secs() -> pos_integer().
window_secs() ->
    env_pos(agent_rl_window_secs, ?DEF_WINDOW_SECS).

-spec env_pos(atom(), pos_integer()) -> pos_integer().
env_pos(Key, Default) ->
    case application:get_env(imboy, Key) of
        {ok, Val} when is_integer(Val), Val > 0 -> Val;
        _ -> Default
    end.
