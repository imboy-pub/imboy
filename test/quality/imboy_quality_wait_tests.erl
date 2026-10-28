%% IMBOY_ERLANG_QUALITY P1-09 等待协议回归（全部通过；进入 U）。
%% WAIT01 合法 ack/错误 Ref/重复 ack/发送方 DOWN；WAIT02 事件先于注册/晚于 deadline；
%% WAIT03 TTL/retry 虚拟时间边界（到期前/恰好/之后、顺序、次数）；
%% WAIT05 否定时间断言（推进到期前无事件）；WAIT06 不降 timeout、不改生产 TTL。
-module(imboy_quality_wait_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------- WAIT01 ----------

ack_only_matching_ref_test() ->
    Ref = make_ref(),
    Other = make_ref(),
    %% 错误 Ref：不得导致提前成功
    self() ! {ack, Other, wrong},
    self() ! {ack, Ref, right},
    ?assertEqual({ok, right}, imboy_quality_wait:await_ack(Ref, undefined, 1000)),
    %% 错误 Ref 的消息仍留在邮箱（未被误消费）
    receive
        {ack, Other, wrong} -> ok
    after 0 -> ?assert(false)
    end.

duplicate_ack_does_not_double_count_test() ->
    Ref = make_ref(),
    self() ! {ack, Ref, one},
    self() ! {ack, Ref, two},
    ?assertEqual({ok, one}, imboy_quality_wait:await_ack(Ref, undefined, 1000)),
    %% 第二个 ack 留存，下一次等待才消费——不会把一次完成算两次
    ?assertEqual({ok, two}, imboy_quality_wait:await_ack(Ref, undefined, 1000)),
    ?assertEqual({error, timeout}, imboy_quality_wait:await_ack(Ref, undefined, 10)).

sender_down_is_explicit_failure_test() ->
    Ref = make_ref(),
    Sender = spawn(fun() -> exit(boom) end),
    ?assertMatch(
        {error, {sender_down, boom}},
        imboy_quality_wait:await_ack(Ref, Sender, 2000)
    ).

%% ---------- WAIT02 ----------

ack_before_registration_not_lost_test() ->
    %% 完成事件先于 await 注册到达：邮箱保序，不丢消息
    Ref = make_ref(),
    self() ! {ack, Ref, early},
    ?assertEqual({ok, early}, imboy_quality_wait:await_ack(Ref, undefined, 100)).

ack_after_deadline_is_timeout_test() ->
    Ref = make_ref(),
    Self = self(),
    spawn(fun() ->
        timer:sleep(300),
        Self ! {ack, Ref, late}
    end),
    ?assertEqual({error, timeout}, imboy_quality_wait:await_ack(Ref, undefined, 50)),
    %% 迟到的 ack 仍可被后续显式等待看到（真实超时未吞消息）
    ?assertEqual({ok, late}, imboy_quality_wait:await_ack(Ref, undefined, 2000)).

%% ---------- WAIT03 / WAIT05 ----------

vclock_ttl_boundaries_test() ->
    C0 = imboy_quality_wait:vclock_new(0),
    C1 = imboy_quality_wait:vclock_schedule(C0, 100, {ttl, expire}),
    C2 = imboy_quality_wait:vclock_schedule(C1, 50, {retry, attempt1}),
    C3 = imboy_quality_wait:vclock_schedule(C2, 150, {retry, attempt2}),
    %% 到期前：无事件（WAIT05 否定断言）
    {[], C4} = imboy_quality_wait:vclock_advance(C3, 49),
    ?assertEqual(3, imboy_quality_wait:vclock_pending(C4)),
    %% 恰好到期：触发且顺序正确
    {[{retry, attempt1}], C5} = imboy_quality_wait:vclock_advance(C4, 50),
    %% 跨多个到期点一次推进：按时间序触发
    {[{ttl, expire}, {retry, attempt2}], C6} = imboy_quality_wait:vclock_advance(C5, 200),
    ?assertEqual(0, imboy_quality_wait:vclock_pending(C6)),
    ?assertEqual(200, imboy_quality_wait:vclock_now(C6)),
    %% 之后：不重复触发
    {[], _} = imboy_quality_wait:vclock_advance(C6, 1000).

%% WAIT03 重试次数符合合同
vclock_retry_count_test() ->
    C = lists:foldl(
        fun(I, Acc) -> imboy_quality_wait:vclock_schedule(Acc, I * 10, {retry, I}) end,
        imboy_quality_wait:vclock_new(0),
        lists:seq(1, 5)
    ),
    {Fired, _} = imboy_quality_wait:vclock_advance(C, 30),
    ?assertEqual([{retry, 1}, {retry, 2}, {retry, 3}], Fired).

%% ---------- WAIT06 ----------

no_timeout_reduction_in_seam_test() ->
    %% seam 不修改任何生产 TTL/timeout：模块不导出可改配置的函数
    Exports = [F || {F, _} <- imboy_quality_wait:module_info(exports)],
    ?assertNot(lists:member(set_production_ttl, Exports)),
    ?assertNot(lists:member(scale_timeouts, Exports)),
    %% 真实时间仍真实：await_ack 的 30ms 超时确实消耗约 30ms（不是立即返回）
    T0 = erlang:monotonic_time(millisecond),
    {error, timeout} = imboy_quality_wait:await_ack(make_ref(), undefined, 30),
    ?assert(erlang:monotonic_time(millisecond) - T0 >= 25).
