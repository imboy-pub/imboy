%% IMBOY_ERLANG_QUALITY P1-09：等待协议 seam（纯 OTP）。
%% 替代 sleep/轮询的两类原语（EUNIT_120S §3.6）：
%%  - await_ack/3：按 Ref 匹配本次协议的完成 ack；错误 Ref/重复 ack 不提前成功；
%%    发送方 DOWN 显式失败；deadline 真实超时（WAIT01/WAIT02）。
%%  - 虚拟时钟：显式推进到期事件（TTL/retry 边界、"一段时间内不应发生"），
%%    不改生产 TTL（WAIT03/WAIT05/WAIT06）。
%% 真实 OTP timer 适配测试仍保留在既有套件（WAIT04），本模块不替代。
-module(imboy_quality_wait).
-export([
    await_ack/3,
    vclock_new/1,
    vclock_schedule/3,
    vclock_advance/2,
    vclock_now/1,
    vclock_pending/1
]).

%% ---- 事件等待 ----

%% await_ack(Ref, SenderPid | undefined, TimeoutMs) ->
%%   {ok, Payload} | {error, timeout} | {error, {sender_down, Reason}}
%% 只接受 {ack, Ref, Payload}；其他 Ref 的 ack 留在邮箱不消费（不误匹配）。
await_ack(Ref, Sender, TimeoutMs) when is_reference(Ref) ->
    Mon =
        case Sender of
            undefined -> undefined;
            Pid when is_pid(Pid) -> erlang:monitor(process, Pid)
        end,
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    try
        wait_loop(Ref, Mon, Deadline)
    after
        case Mon of
            undefined -> ok;
            _ -> erlang:demonitor(Mon, [flush])
        end
    end.

wait_loop(Ref, Mon, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {ack, Ref, Payload} ->
            {ok, Payload};
        {'DOWN', Mon, process, _Pid, Reason} when Mon =/= undefined ->
            {error, {sender_down, Reason}}
    after Remaining ->
        {error, timeout}
    end.

%% ---- 虚拟时钟 ----

%% 纯数据结构：#{now := integer(), pending := [{At, Id, Msg}]}（按 At 升序）。
vclock_new(Start) when is_integer(Start) ->
    #{now => Start, pending => []}.

vclock_schedule(#{pending := P} = C, At, {Id, Msg}) when is_integer(At) ->
    C#{pending => lists:keysort(1, [{At, Id, Msg} | P])}.

%% 推进到 NewNow，按时间序触发所有 At =< NewNow 的事件；返回 {Fired, C2}。
%% 到期前（At > NewNow）不触发：实现"恰好到期/之后"边界与否定时间断言。
vclock_advance(#{now := Now} = C, NewNow) when NewNow >= Now ->
    {Due, Later} = lists:partition(
        fun({At, _, _}) -> At =< NewNow end,
        maps:get(pending, C)
    ),
    {[{Id, Msg} || {_, Id, Msg} <- Due], C#{now => NewNow, pending => Later}}.

vclock_now(#{now := N}) -> N.
vclock_pending(#{pending := P}) -> length(P).
