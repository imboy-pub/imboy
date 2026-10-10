-module(cache_w0_timeout_probe).
-export([run/0]).

%% depcache 30s writer 锁超时行为探针（W0 补充实测；只测真实安装的 depcache）。
%% 源码预期（deps/depcache/src/depcache.erl）：
%%   :1019 writer 锁 now+?MAX_GET_WAIT(30s)；锁过期后新 miss 成为第二并发 writer，
%%   旧等待列表被丢弃（:1003-1011 去监控旧 writer、:1019 覆盖 wait_pids 条目）；
%%   被丢弃的等待者无人回复，只能靠 gen_server:call 30s 超时退出（:230/:472）；
%%   旧 writer 迟到的 set 会拿走新 writer 的 wait_pids 条目并回复其等待者（:1130-1142）。
%% 隔离 BEAM、合成值、不启动业务与数据库。

run() ->
    Output = os:getenv("CACHE_PROBE_OUTPUT"),
    {ok, _} = application:ensure_all_started(syn),
    {ok, _} = depcache:start_link(cache_w0_probe_data, #{memory_max => 64}),
    Parent = self(),
    %% T0：首个 miss 成为 writer，持锁 35s（跨过 30s 锁过期线）后才 set。
    spawn(fun() ->
        First0 = depcache:get_wait(probe_key, cache_w0_probe_data),
        Parent ! {writer_started, First0},
        timer:sleep(35000),
        ok = depcache:set(probe_key, v1, 60, cache_w0_probe_data),
        Parent ! writer_set_done
    end),
    First = receive {writer_started, F} -> F after 5000 -> error(writer_start_timeout) end,
    %% T0+0.2：等待者 1 加入首 writer 的等待列表；预期 ~30s 后调用超时退出。
    spawn(fun() ->
        Result = try
            {timeout_protection, depcache:get_wait(probe_key, cache_w0_probe_data)}
        catch
            exit:{timeout, _} -> timed_out;
            Class:Reason -> {unexpected, Class, Reason}
        end,
        Parent ! {waiter1, Result}
    end),
    %% T0+31：锁已过期，第二个调用者成为第二并发 writer（单飞在 >30s 负载下破坏）。
    timer:sleep(30800),
    Second = depcache:get_wait(probe_key, cache_w0_probe_data),
    %% T0+31.3：等待者 2 加入第二 writer 的等待列表。
    spawn(fun() ->
        Raw = depcache:get_wait(probe_key, cache_w0_probe_data),
        Result = case Raw of
            {ok, V} -> #{got => V};
            Other -> #{got_unexpected => Other}
        end,
        Parent ! {waiter2, Result}
    end),
    %% 等待者 1 的调用超时窗口（T0+0.2 起 30s）先到期。
    Waiter1Result = receive {waiter1, R1} -> R1 after 40000 -> error(waiter1_timeout) end,
    _ = receive writer_set_done -> ok after 20000 -> error(writer_set_timeout) end,
    Waiter2Result = receive {waiter2, R2} -> R2 after 5000 -> error(waiter2_timeout) end,
    %% 第二 writer 迟到自设覆盖；末次 set 胜出。
    timer:sleep(500),
    ok = depcache:set(probe_key, v2, 60, cache_w0_probe_data),
    Final = depcache:get(probe_key, cache_w0_probe_data),
    Result = #{otp => erlang:system_info(otp_release),
        first_caller_became_writer => First,
        second_writer_after_lock_expiry => Second,
        waiter1_result => Waiter1Result,
        waiter2_result => Waiter2Result,
        final_value_after_last_set => Final},
    ok = file:write_file(Output, jsone:encode(Result)),
    halt(0).
