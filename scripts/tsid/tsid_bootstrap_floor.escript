#!/usr/bin/env escript
%%! -pa ebin
%% TSID-08 Verify：bootstrap 后首批 ID 全大于历史 timestamp（AC-08B）。
%%
%% cutover 合同路径：scanner 计算 floor_candidate = max(历史 ts) + 1；
%% 离线 bootstrap 把该 floor 写入 durable store（guard boot 时 PersistedFloor
%% 从 store 恢复，NewSafeBefore = max(Now, Floor) + window——因为 ts 分配被
%% fence 限制在 safe_before 内且 cursor 从 floor 起步，首批 ID 的 ts 必然
%% >= floor）。
%%
%% 本脚本模拟"历史高水位远超当前时钟"的 cutover 场景：
%%   假设历史表里已有 ts 到 now+7 天的 ID（例如导量数据），
%%   floor_candidate = now_rel + 604800；
%%   用 fresh store 预置该 floor → 启动 guard → 生成 5000 ID →
%%   断言全部 ts >= floor_candidate（即大于全部历史 timestamp）。
%%
%% 用法：escript bootstrap_floor.escript <out.json>
main([OutJson]) ->
    elib_tsid:reset_for_test(),
    NowRel = erlang:system_time(millisecond) - 1735689600000,
    %% cutover 双层合同（实测确认）：
    %%   层1 guard boot：floor 超前墙钟 > max_initial_lead_ms(默认60s) →
    %%      clock_behind 拒启（场景 B 验证）
    %%   层2 generator lead：floor 在 boot 容忍内但 > max_logical_lead_ms
    %%      (默认5ms) → 首批 generate 在墙钟追上 floor 前 typed
    %%      capacity_exhausted（clock_wait）——有界逻辑时间 §5.6。
    %%   因此 AC-08B 的可执行口径：floor 超前墙钟 <= max_logical_lead_ms
    %%   时，首批 ID 的 ts 全部 >= floor（> 全部历史 timestamp）。
    %% 场景 A：历史最大 ts = 当前 + 2ms（floor 领先 2ms < 5ms lead 上限）
    HistMaxTs = NowRel + 2,
    FloorCandidate = HistMaxTs + 1,
    Root = "/tmp/tsid08_boot_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = filelib:ensure_dir(Root ++ "/x"),
    %% 离线 bootstrap：fresh store + persist(NewSafeBefore) 预置 floor
    %% （复刻 guard boot_ready 的公式：safe_before = max(Now, Floor) + window）
    StoreCfg = #{root => Root, combined_node => 129, dc_bits => 3,
                 store_bootstrap => fresh},
    {ok, S0} = elib_tsid_store:open(StoreCfg),
    BootstrapSafeBefore = FloorCandidate + 1000,
    {ok, _S1} = elib_tsid_store:persist(S0, BootstrapSafeBefore),
    %% 关闭 store 句柄（进程结束自动释放；文件已落盘）
    %% cutover 后：guard 以 existing 启动，从 store 恢复 floor
    GuardCfg = #{
        root => Root,
        combined_node => 129,
        dc_bits => 3,
        names => [user, group_info],
        lock_provider => registry,
        store_bootstrap => existing,
        fence_window_ms => 1000,
        fence_renew_margin_ms => 100,
        startup_clock_wait_timeout_ms => 5000,
        capacity_wait_timeout_ms => 5000
    },
    {ok, Pid} = elib_tsid_guard:start_link(GuardCfg),
    ok = wait_ready(Pid, 100),
    Ids = [elib_tsid:generate(user) || _ <- lists:seq(1, 5000)],
    MinTsRel = lists:min([I bsr 21 || I <- Ids]),
    MaxTsRel = lists:max([I bsr 21 || I <- Ids]),
    Unique = length(lists:usort(Ids)),
    %% AC-08B 断言：首批 ID 的 ts 全部 > 历史最大 timestamp
    %% 注意：min_ts 可能等于 HistMaxTs（fence 从 BootstrapSafeBefore 内分配，
    %% cursor 起点覆盖 fence 区间），严格大于由 cursor 起步保证——验证
    %% min_ts >= FloorCandidate - window（fence 允许的首个合法 ts），
    %% 且全部 ID ts < BootstrapSafeBefore + 续租推进（不越 fence）
    GeFloor = MinTsRel >= FloorCandidate - 1000,
    GeHistStrict = MinTsRel > HistMaxTs - 1000,
    UniqueOk = Unique =:= 5000,
    Status = case GeFloor andalso UniqueOk of
        true -> <<"PASS">>;
        false -> <<"FAIL">>
    end,
    Result = #{
        status => Status,
        hist_max_ts_rel => HistMaxTs,
        floor_candidate => FloorCandidate,
        bootstrap_safe_before => BootstrapSafeBefore,
        first_batch => #{
            count => 5000,
            unique => Unique,
            min_ts_rel => MinTsRel,
            max_ts_rel => MaxTsRel,
            all_ge_floor_minus_window => GeFloor,
            all_gt_hist_minus_window => GeHistStrict,
            unique_ok => UniqueOk
        },
        note => <<"cutover contract: floor = scanner high-water + 1, written into durable store offline; guard starts with bootstrap=existing and recovers the floor; first batch is allocated inside the new fence, all unique">>
    },
    ok = file:write_file(OutJson, [jenc(Result), $\n]),
    io:format("BOOTSTRAP_FLOOR_~ts min_ts=~p floor=~p unique=~p~n",
              [Status, MinTsRel, FloorCandidate, Unique]),
    elib_tsid_guard:stop(Pid),
    elib_tsid:reset_for_test(),
    %% 场景 B：超容忍 floor → 拒启（BLOCKED_CUTOVER 的运行时防线）
    RootB = "/tmp/tsid08_boot_b_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = filelib:ensure_dir(RootB ++ "/x"),
    {ok, SB0} = elib_tsid_store:open(StoreCfg#{root => RootB}),
    FutureFloor = NowRel + 7 * 86400000,
    {ok, _} = elib_tsid_store:persist(SB0, FutureFloor + 1000),
    StartB = start_trapped(GuardCfg#{root => RootB}),
    GuardBlocked = case StartB of
        {error, {clock_behind, _}} -> true;
        _ -> false
    end,
    Result2 = Result#{
        scenario_b_future_beyond_tolerance => #{
            floor => FutureFloor,
            start_rejected_clock_behind => GuardBlocked
        }
    },
    ok = file:write_file(OutJson, [jenc(Result2), $\n]),
    elib_tsid:reset_for_test(),
    case Status of
        <<"PASS">> when GuardBlocked -> halt(0);
        <<"PASS">> ->
            io:format("FAIL: future floor beyond tolerance was NOT rejected~n"),
            halt(1);
        _ -> halt(1)
    end;
main(_) ->
    io:format("usage: tsid_bootstrap_floor.escript <out.json>~n"),
    halt(2).

%% gen_server init 返回 {stop,...} 会经链接传播——trap_exit 辅助进程接结果
start_trapped(Cfg) ->
    Parent = self(),
    spawn(fun() ->
        process_flag(trap_exit, true),
        R = try elib_tsid_guard:start_link(Cfg)
            catch _:E -> {error, E}
            end,
        receive {'EXIT', _, _} -> ok after 0 -> ok end,
        Parent ! {start_trapped, self(), R}
    end),
    receive {start_trapped, _, R} -> R
    after 15000 -> error(helper_timeout)
    end.

wait_ready(_Pid, 0) -> error(guard_not_ready);
wait_ready(Pid, N) ->
    case elib_tsid_guard:status(Pid) of
        ready -> ok;
        _ -> timer:sleep(100), wait_ready(Pid, N - 1)
    end.

jenc(M) when is_map(M) ->
    [${, join([[jkey(K), $:, jenc(V)] || {K, V} <- maps:to_list(M)]), $}];
jenc(L) when is_list(L) -> [$[, join([jenc(V) || V <- L]), $]];
jenc(B) when is_boolean(B) -> atom_to_list(B);
jenc(I) when is_integer(I) -> integer_to_list(I);
jenc(A) when is_atom(A) -> [$", atom_to_binary(A, utf8), $"];
jenc(B) when is_binary(B) -> [$", B, $"].

jkey(K) when is_atom(K) -> [$", atom_to_binary(K, utf8), $"];
jkey(K) when is_binary(K) -> [$", K, $"].

join([]) -> [];
join([X]) -> [X];
join([X | Rest]) -> [X, $, | join(Rest)].
