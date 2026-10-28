%% IMBOY_ERLANG_QUALITY P1-03：peer worker 执行器（本机、无公开 distribution、单调时钟）。
%% 在独立 peer VM 中以 EUnit 本体执行测试模块；结果（含事件/计数）经控制连接
%% 返回值传回（纯数据），不跨 VM 发消息、不传 opaque fun。
%% deadline 到期有界回收本 VM（RUN05）；总墙钟来自控制器单调时钟。
-module(imboy_quality_runner).
-export([
    run_in_worker/3,
    run_group_in_worker/4,
    worker_main/1,
    worker_main_group/1,
    worker_main/2,
    worker_main_group/2,
    worker_main_group/3,
    worker_env/0,
    stream_read/1
]).

-spec run_in_worker(TestMod :: module(), Opts :: map(), DeadlineMs :: pos_integer()) ->
    #{
        exit := ok | test_failed | timeout_or_cancelled | worker_crashed,
        wall_ms := non_neg_integer(),
        counts := #{
            pass => non_neg_integer(),
            fail => non_neg_integer(),
            skip => non_neg_integer(),
            cancel => non_neg_integer()
        },
        events := list(),
        node := unicode:chardata()
    }.
run_in_worker(TestMod, Opts, DeadlineMs) ->
    true = is_atom(TestMod),
    CodePath = maps:get(code_path, Opts),
    Schedulers = maps:get(schedulers, Opts, 2),
    FrameworkPath =
        case code:which(?MODULE) of
            Beam when is_list(Beam) -> filename:dirname(Beam);
            _ -> erlang:error(runner_not_file_loaded)
        end,
    T0 = erlang:monotonic_time(millisecond),
    {ok, Peer, _WorkerNode} = peer:start_link(#{
        %% loopback TCP，无需 distribution/epmd
        connection => {{127, 0, 0, 1}, 0},
        args => [
            "-noshell",
            "-noinput",
            "-boot",
            "no_dot_erlang",
            "+S",
            integer_to_list(Schedulers),
            "-pa",
            CodePath,
            "-pa",
            FrameworkPath
        ]
    }),
    try
        Node = peer:call(Peer, erlang, node, [], 5000),
        Timeout = max(1000, DeadlineMs - (erlang:monotonic_time(millisecond) - T0)),
        {EunitReturn, Counts, Events} =
            peer:call(Peer, ?MODULE, worker_main, [TestMod], Timeout),
        Wall = erlang:monotonic_time(millisecond) - T0,
        finish(EunitReturn, Counts, Events, Wall, Node)
    catch
        Class:Reason when Class =:= exit; Class =:= error ->
            %% worker 崩溃 / peer 控制连接丢失 / deadline：有界停本 VM
            stop_peer(Peer),
            WallC = erlang:monotonic_time(millisecond) - T0,
            #{
                exit => classify(Class, Reason),
                wall_ms => WallC,
                counts => zero_counts(),
                events => [],
                node => down
            }
    after
        stop_peer(Peer)
    end.

stop_peer(Peer) ->
    try
        peer:stop(Peer)
    catch
        _:_ -> ok
    end.

classify(_, timeout) -> timeout_or_cancelled;
classify(_, {timeout, _}) -> timeout_or_cancelled;
classify(_, _) -> worker_crashed.

finish(ok, Counts, Events, Wall, Node) ->
    #{exit => ok, wall_ms => Wall, counts => Counts, events => Events, node => Node};
%% eunit:test 失败返回裸 atom error
finish(error, Counts, Events, Wall, Node) ->
    #{
        exit => test_failed,
        wall_ms => Wall,
        counts => Counts,
        events => Events,
        node => Node
    };
finish({error, _}, Counts, Events, Wall, Node) ->
    #{
        exit => test_failed,
        wall_ms => Wall,
        counts => Counts,
        events => Events,
        node => Node
    }.

zero_counts() -> #{pass => 0, fail => 0, skip => 0, cancel => 0}.

%% ---- 组模式：同 peer VM 内串行执行多个 root（P1-06 分片） ----
%% 返回 per-root 结果 map：#{Mod => #{exit, wall_ms, counts, events}}，
%% 外层 wall 为组总墙钟；单 root 失败继续后续 root（SCHED05）。
run_group_in_worker(TestMods, Opts, DeadlineMs, ExtraPaths) ->
    true = is_list(TestMods),
    true = TestMods =/= [],
    Schedulers = maps:get(schedulers, Opts, 2),
    FrameworkPath =
        case code:which(?MODULE) of
            Beam when is_list(Beam) -> filename:dirname(Beam);
            _ -> erlang:error(runner_not_file_loaded)
        end,
    T0 = erlang:monotonic_time(millisecond),
    %% 业务 worker 需要 sys config（-config <path 无 .config 后缀>）与进程上限；
    %% 由 Opts 注入，纯合成自测不传。
    ExtraArgs =
        case maps:get(config, Opts, undefined) of
            undefined -> [];
            Cfg -> ["-config", Cfg]
        end ++
            case maps:get(process_limit, Opts, undefined) of
                undefined -> [];
                PL -> ["+P", integer_to_list(PL)]
            end,
    %% env 覆盖项（如 ERL_LIBS=deps——legacy 由 erlang.mk export，此处显式传入；
    %% deps 以 ERL_LIBS 进路径与 legacy 同优先级：低于全部 -pa 目录）。
    EnvArgs = maps:get(env, Opts, []),
    %% -pa 后者优先（后出现的旗标排路径最前）。ExtraPaths 按升优先级排列：
    %% [imboy/ebin, ebin, test, feature-test-ebin]，FrameworkPath 垫底
    %% （imboy_quality_* 命名无冲突，仅兜底加载）。
    PaArgs = lists:append([["-pa", P] || P <- [FrameworkPath | ExtraPaths]]),
    {ok, Peer, _N} = peer:start_link(#{
        connection => {{127, 0, 0, 1}, 0},
        env => EnvArgs,
        args => [
            "-noshell",
            "-noinput",
            "-boot",
            "no_dot_erlang",
            "+S",
            integer_to_list(Schedulers)
        ] ++ ExtraArgs ++ PaArgs
    }),
    try
        %% worker 环境取证（FRAMEWORK_DESIGN §3：code:which/加载摘要区分
        %% src/lib 与 test/common 同名模块；lib_dir 别名是否可解析）
        EnvInfo = peer:call(Peer, ?MODULE, worker_env, [], 15000),
        Timeout = max(1000, DeadlineMs - (erlang:monotonic_time(millisecond) - T0)),
        PerRoot = peer:call(
            Peer,
            ?MODULE,
            worker_main_group,
            [
                TestMods,
                maps:get(spec_wrapper, Opts, undefined),
                maps:get(stream, Opts, undefined)
            ],
            Timeout
        ),
        Wall = erlang:monotonic_time(millisecond) - T0,
        #{exit => ok, wall_ms => Wall, per_root => PerRoot, env => EnvInfo, node => group}
    catch
        Class:Reason when Class =:= exit; Class =:= error ->
            stop_peer(Peer),
            WallC = erlang:monotonic_time(millisecond) - T0,
            #{
                exit => classify(Class, Reason),
                reason => Reason,
                wall_ms => WallC,
                per_root => #{},
                node => down
            }
    after
        stop_peer(Peer)
    end.

worker_main_group(TestMods) ->
    worker_main_group(TestMods, undefined).

worker_main_group(TestMods, SpecWrapper) ->
    worker_main_group(TestMods, SpecWrapper, undefined).

%% SpecWrapper :: undefined | {M, F}：每根 EUnit 规格由 M:F(Mod) 产出
%% （项目 Makefile:188 EUNIT_TEST_SPEC 以 talk_record_owned_eunit_fixture:entry/1
%% 包裹每个根；等价对照必须同规格——budgets 内根在 legacy 外层只计 1 叶）。
%% StreamPath：每根开始/完成即写长度前缀 term 并 sync（SCHED05：VM 死亡只丢
%% 在途根，已完成结果由控制器从流恢复——opt-run4 两组整组丢失的教训）。
worker_main_group(TestMods, SpecWrapper, StreamPath) ->
    File =
        case StreamPath of
            undefined ->
                undefined;
            P ->
                {ok, FH} = file:open(P, [append, binary, raw]),
                FH
        end,
    try
        maps:from_list(
            lists:map(
                fun(Mod) ->
                    stream_event(File, {start, Mod}),
                    T0 = erlang:monotonic_time(millisecond),
                    {EunitReturn, Counts, Events} = worker_main(Mod, SpecWrapper),
                    Wall = erlang:monotonic_time(millisecond) - T0,
                    Exit =
                        case EunitReturn of
                            ok -> ok;
                            _ -> test_failed
                        end,
                    R = #{
                        exit => Exit,
                        wall_ms => Wall,
                        counts => Counts,
                        events => Events
                    },
                    stream_event(File, {done, Mod, R}),
                    {Mod, R}
                end,
                TestMods
            )
        )
    after
        case File of
            undefined -> ok;
            FH2 -> ok = file:close(FH2)
        end
    end.

stream_event(undefined, _Event) ->
    ok;
stream_event(FH, Event) ->
    B = term_to_binary(Event),
    ok = file:write(FH, [integer_to_binary(byte_size(B)), $\n, B, $\n]),
    ok = file:sync(FH).

%% 读取流文件（容忍尾部截断：VM 死亡时最后一条可能不完整）。
stream_read(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> {ok, stream_parse(Bin, [])};
        {error, enoent} -> {ok, []};
        {error, _} = E -> E
    end.

stream_parse(<<>>, Acc) ->
    lists:reverse(Acc);
stream_parse(Bin, Acc) ->
    case binary:split(Bin, <<"\n">>) of
        [LenB, Rest] ->
            try binary_to_integer(LenB) of
                Len when byte_size(Rest) >= Len + 1 ->
                    <<Body:Len/binary, $\n, Tail/binary>> = Rest,
                    %% 流由本任务 worker 写出（可信 ETF）；模块原子在控制器 VM
                    %% 可能尚不存在，故不用 safe 选项
                    stream_parse(Tail, [binary_to_term(Body) | Acc]);
                _ ->
                    lists:reverse(Acc)
            catch
                _:_ -> lists:reverse(Acc)
            end;
        _ ->
            lists:reverse(Acc)
    end.

%% worker VM 环境摘要（纯数据，不崩溃：未加载模块返回 non_existing/bad_name）
worker_env() ->
    #{
        node => node(),
        lib_dir_imboy => code:lib_dir(imboy),
        priv_dir_imboy => code:priv_dir(imboy),
        eunit_runner_beam => code:which(eunit_runner),
        product_terminology_beam => code:which(product_terminology),
        spec_wrapper_beam => code:which(talk_record_owned_eunit_fixture),
        imboyenv => os:getenv("IMBOYENV"),
        erl_libs => os:getenv("ERL_LIBS"),
        code_path_head => lists:sublist(code:get_path(), 6)
    }.

%% worker 侧入口：collector 为 worker 内进程；结束把计数与事件作为返回值带回。
worker_main(TestMod) ->
    worker_main(TestMod, undefined).

worker_main(TestMod, SpecWrapper) ->
    Parent = self(),
    Ref = make_ref(),
    Collector = spawn(fun() -> collect(Parent, Ref, []) end),
    Spec =
        case SpecWrapper of
            undefined -> TestMod;
            {M, F} -> M:F(TestMod)
        end,
    EunitReturn = eunit:test(
        [Spec],
        %% 超时回收由 runner 级 peer deadline 权威执行（RUN05）
        [
            {timeout, infinity},
            {report, {imboy_quality_listener, [{collector, Collector}]}}
        ]
    ),
    Collector ! {stop, Parent, Ref},
    {Counts, Events} =
        receive
            {collected, Ref, C, E} -> {C, E}
        after 10000 -> {zero_counts(), []}
        end,
    {EunitReturn, Counts, Events}.

collect(Parent, Ref, Acc) ->
    receive
        {stop, Parent, Ref} ->
            Parent ! {collected, Ref, counts(Acc, zero_counts()), lists:reverse(Acc)};
        %% listener 事件先入 Acc（否则滞留 mailbox 导致计数恒零）
        Msg ->
            collect(Parent, Ref, [Msg | Acc])
    after 30000 ->
        Parent ! {collected, Ref, counts(Acc, zero_counts()), lists:reverse(Acc)}
    end.

counts([E | Rest], C) ->
    case E of
        {test_end, _, ok, _} -> counts(Rest, C#{pass => maps:get(pass, C) + 1});
        {test_end, _, _, _} -> counts(Rest, C#{fail => maps:get(fail, C) + 1});
        {test_cancel, _, _, _} -> counts(Rest, C#{cancel => maps:get(cancel, C) + 1});
        _ -> counts(Rest, C)
    end;
counts([], C) ->
    C.
