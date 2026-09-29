-module(eunit_runner).
-export([
    run/0,
    run/1,
    run_fast/0,
    ct_suite_setup/1,
    ct_suite_cleanup/1,
    eunit_setup/0,
    eunit_cleanup/1,
    eunit_cleanup_db/1,
    eunit_try_db/0,
    eunit_setup_with_db/0,
    eunit_setup_db_or_skip/0,
    ensure_named_server/1,
    ensure_boot_coordinator/0
]).

%%%===================================================================
%%% @doc
%%% 简化的 EUnit 运行器
%%% 可以直接在 erlang shell 中调用
%%%===================================================================

%% @doc 运行所有测试
run() ->
    run([]).

%% @doc 运行指定模块的测试
%% @param Modules 模块列表，如 [user_repo_tests, group_repo_tests]
run(Modules) when is_list(Modules) ->
    % 使用 eunit_setup 启动应用
    State = eunit_setup(),
    try
        case Modules of
            [] -> eunit:test([], [verbose]);
            _ -> eunit:test(Modules, [verbose])
        end
    after
        eunit_cleanup(State)
    end.

%% @doc 快速测试（只测试不需要数据库的模块）
run_fast() ->
    % 不启动应用，只测试纯函数模块
    FastTestModules = [
        elib_pg_sql_tests
    ],
    eunit:test(FastTestModules, [verbose]).

%% @doc 为 Common Test suite 设置统一的项目根目录和应用启动流程
ct_suite_setup(Config) ->
    {ok, OldCwd} = file:get_cwd(),
    ProjectRoot = project_root_dir(OldCwd),
    case file:set_cwd(ProjectRoot) of
        ok ->
            SetupState = eunit_setup(),
            [{setup_state, SetupState}, {old_cwd, OldCwd} | Config];
        {error, Reason} ->
            {skip,
                io_lib:format("Unable to set cwd to project root (~p): ~p", [ProjectRoot, Reason])}
    end.

%% @doc 清理 Common Test suite 运行状态
ct_suite_cleanup(Config) ->
    case lists:keyfind(setup_state, 1, Config) of
        {setup_state, SetupState} ->
            eunit_cleanup(SetupState);
        false ->
            ok
    end,
    case lists:keyfind(old_cwd, 1, Config) of
        {old_cwd, OldCwd} ->
            _ = file:set_cwd(OldCwd),
            ok;
        false ->
            ok
    end.

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 启动所有必要的应用（测试环境）
%% @return {app_started, imboy} | {app_not_started, test_continues}
%% 为测试环境设置必要的配置并启动应用
eunit_setup() ->
    % 设置测试环境变量
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok;
        _ -> ok
    end,

    load_test_config(),
    enforce_pool_capacity(),
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, env, test),
    application:set_env(imboy, http_port, test_http_port()),
    application:set_env(imboy, dsync_enabled, false),
    %% 测试环境社区版用户配额拉高：注册/好友全链流程测试会真注册用户，
    %% 本地库测试数据积累极易越过社区版默认 max_users=100 触发 402
    application:set_env(imboy, community_max_users, 1000000),

    % 启动核心依赖应用
    CoreApps = [crypto, asn1, public_key, ssl, inets, jsone, lager, depcache],
    lists:foreach(
        fun(App) ->
            case application:ensure_all_started(App) of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                _ -> ok
            end
        end,
        CoreApps
    ),

    % 启动 imboy 应用：全部经由单例启动协调进程串行化。
    % eunit 各模块 setup 并发进入时，若无协调器会出现并发 cleanup/并发
    % ensure_all_started：互杀对方 boot 中的 barrel/ranch listener，
    % 制造半启动孤儿（run #6 的开机 eaddrinuse + 'm:imboy_cache' 表丢失
    % 连锁 badarg 即此类）。协调器由长驻进程持有 boot，调用方 5s 测试
    % 超时被杀也不再中止启动，下个 setup 自愈拿到已启动的 app。
    ensure_boot_coordinator(),
    Ref = make_ref(),
    eunit_boot_coordinator ! {boot, self(), Ref},
    State =
        receive
            {Ref, {ok, S}} ->
                S;
            {Ref, {error, Reason}} ->
                io:format("Warning: Failed to start imboy app: ~p~n", [Reason]),
                io:format("Tests that require app will be skipped~n"),
                {app_not_started, test_continues}
        after 60000 ->
            io:format("Warning: boot coordinator timeout~n"),
            {app_not_started, test_continues}
        end,
    %% 禁用 sync 热重载器（IMBOYENV=local 时它随 DEPS 进 VM）。实证
    %% （2026-09-29 probe3/probe4）：sync 重编任一模块（如 config_ds）后
    %% 数百毫秒内 msg_store_worker 无声消失（无 crash、无 terminate 日志，
    %% whereis=undefined）→ staging 永不转运 → 全部消息等待型集成测试
    %% c2c/c2g_message_not_ready（eunit 全量 16932 行 staging 0 处理的根因）。
    %% 主防线在 do_boot：预启动 sync 并 pause（防 boot 窗口内的编译赛跑）。
    %% 此处 post-boot stop(sync) 是第二道保险：sync 是开发期工具，测试 VM
    %% 里零价值且直接破坏被测系统——boot 返回后彻底移除（含防 scanner
    %% 崩溃重启后 paused 复位的尾部窗口）；不在依赖链上，stop 不影响
    %% imboy 生命周期。
    case State of
        {app_started, _} -> catch application:stop(sync);
        _ -> ok
    end,
    %% WH-01 让位：imboy_sup 的 bot_webhook_delivery_worker 每 1s poll
    %% claim_due，会抢在用例断言前认领测试刚插入的 pending 投递并真实
    %% 外发 HTTP（重试耗尽后 status=dead，claim_due 断言 false——实证
    %% be-02 第七轮 claim_due_only_due 偶发失败）。测试 VM 里 terminate
    %% 让位（elib_metric/plugin 族同款惯例）；一次性 VM 不复原；worker
    %% 专属单测走 execute/1 直调、不经 sup 实例，不受影响。
    case State of
        {app_started, _} ->
            _ = catch supervisor:terminate_child(imboy_sup, bot_webhook_delivery_worker),
            ok;
        _ ->
            ok
    end,
    % 缓存实例自愈（详见 do_ensure_cache）：每次 setup 顺带检查命名表
    Ref2 = make_ref(),
    eunit_boot_coordinator ! {ensure_cache, self(), Ref2},
    receive
        {Ref2, ok} -> ok
    after 10000 -> ok
    end,
    State.

ensure_boot_coordinator() ->
    case whereis(eunit_boot_coordinator) of
        undefined ->
            Pid = erlang:spawn(fun boot_coord_loop/0),
            case
                try
                    erlang:register(eunit_boot_coordinator, Pid)
                catch
                    _:_ -> error
                end
            of
                true ->
                    ok;
                _ ->
                    % 并发竞争输家：赢家已注册，回收自己的进程
                    exit(Pid, kill),
                    ok
            end;
        _Pid ->
            ok
    end.

boot_coord_loop() ->
    %% A1c（CP-TD-A02）：trap_exit——do_ensure_cache 重建的 imboy_cache 挂在本
    %% 协程名下，cleanup_start_orphans 黑名单击杀幸存缓存时 EXIT(shutdown/killed)
    %% 会传染本协程（非 trap 即死），后续所有 eunit_setup 的 send 全部 badarg、
    %% 整段 app 套件连锁 cancel。trap 后 EXIT 信号降级为可丢弃消息。
    process_flag(trap_exit, true),
    boot_coord_loop_().
boot_coord_loop_() ->
    receive
        {boot, From, Ref} ->
            From ! {Ref, do_boot()},
            boot_coord_loop_();
        {ensure_cache, From, Ref} ->
            From ! {Ref, do_ensure_cache()},
            boot_coord_loop_();
        {'EXIT', _Pid, _Reason} ->
            %% 被杀链接子进程的 EXIT 残留，丢弃
            boot_coord_loop_();
        _ ->
            boot_coord_loop_()
    end.

do_ensure_cache() ->
    case ets:whereis('m:imboy_cache') of
        undefined ->
            %% 套件隔离治理自愈：imboy_cache:start_link 内部
            %% `_ = depcache:start_link(...)` 吞掉 {already_started} 且返回
            %% {ok, self()}，使 sup 记录的 child pid 失真——测试窗口中的
            %% imboy_cache/depcache meck 误杀实例后，sup 既感知不到也不重启，
            %% 命名表 'm:imboy_cache' 永久消失 → 所有缓存调用 badarg。
            %% 此处由长驻协调进程重建一个挂在自身上的实例恢复命名表。
            case whereis(imboy_cache) of
                undefined -> ok;
                StalePid -> catch gen_server:stop(StalePid, normal, 300)
            end,
            timer:sleep(30),
            _ = imboy_cache:start_link([{depcache_memory_max, 100}]),
            ok;
        _ ->
            ok
    end.

do_boot() ->
    case app_running(imboy) of
        true ->
            {ok, {app_already_started, imboy}};
        false ->
            %% sync 禁编（主防线）：imboy.app applications 含 sync（DEPS +=
            %% sync），ensure_all_started 链式拉起 sync_scanner 后，其扫描链
            %% compare_beams（首跑 2s 定时）→ compare_src_files →
            %% process_queue 会在 imboy start/2 完成之前重编过期 beam 并
            %% code:purge 静默击杀在役 worker（probe4 铁证：Recompiled →
            %% 250ms 内 msg_store_worker whereis=undefined，无 crash 无
            %% terminate）。post-boot stop(sync) 必输赛跑（solo-mr5：编译在
            %% stop 前 85ms 已发生，即 boot 返回之前）。故在拉起 imboy 链
            %% 之前先预启动 sync 并立即 pause：paused=true 吸收全部扫描/
            %% 编译 cast（首编译最早 ~2s 后，pause 在 ~ms 内落地且早于一切
            %% 定时器首跑），后续 ensure_all_started(imboy) 对已启动的 sync
            %% 直接跳过。双保险见 eunit_setup 的 post-boot stop(sync)。
            _ =
                (case application:ensure_all_started(sync) of
                    {ok, _} ->
                        catch gen_server:cast(sync_scanner, pause);
                    _ ->
                        ok
                end),
            cleanup_start_orphans(),
            %% 测试模式禁用 msg_store_worker 周期 tick（纯 kick 驱动）：
            %% worker 现已常驻（见 PERIODIC_WORKER_STOP_SPECS 注释），但其
            %% 每秒 tick 的异步 drain 会在其他套件的 meck 窗口内调用 elib_pg
            %% 池化版（mark_processed 走 query/2），污染全局调用计数类白盒
            %% 断言（adm_message_handler 审计 fails-closed 用例 R1B1 实证：
            %% num_calls(elib_pg, query, 2) 期望 0）。正常发送路径 stage 后
            %% enqueue 必发 kick，集成套件的真实异步转正不受影响；须在 app
            %% 启动前 set_env，worker init 读取后决定是否武装 tick 定时器。
            application:set_env(imboy, msg_store_worker_tick_ms, 0),
            %% A1c（CP-TD-A02）：上一次启动中途夭折会遗留 ranch 监听孤儿——
            %% 黑名单（按原子名 whereis）够不到 {ranch_listener_sup, Ref} 元组名
            %% sup，残留 19980/19970 绑定 → 重试恒 eaddrinuse（run14/16 实证
            %% 重试风暴 169 连发）。显式停监听后再重试。
            _ =
                (try
                    ranch:stop_listener(imboy_listener)
                catch
                    _:_ -> ok
                end),
            _ =
                (try
                    cowboy:stop_listener(imboy_listener)
                catch
                    _:_ -> ok
                end),
            _ =
                (try
                    ranch:stop_listener(imboy_listener_tls)
                catch
                    _:_ -> ok
                end),
            _ =
                (try
                    cowboy:stop_listener(imboy_listener_tls)
                catch
                    _:_ -> ok
                end),
            case application:ensure_all_started(imboy) of
                {ok, _} ->
                    quiesce_periodic_workers(),
                    {ok, {app_started, imboy}};
                {error, {already_started, imboy}} ->
                    {ok, {app_already_started, imboy}};
                {error, _StartReason} ->
                    % 首次尝试若被 eunit 5s 测试超时打断（caller 被杀 → app
                    % master 中止），会留下 barrel 单例与 ranch listener 孤儿；
                    % 清掉后重试一次，避免一次超时毒化整轮。
                    cleanup_start_orphans(),
                    case application:ensure_all_started(imboy) of
                        {ok, _} ->
                            quiesce_periodic_workers(),
                            {ok, {app_started, imboy}};
                        {error, {already_started, imboy}} ->
                            {ok, {app_already_started, imboy}};
                        {error, RetryReason} ->
                            {error, RetryReason}
                    end
            end
    end.

app_running(App) ->
    lists:keymember(App, 1, application:which_applications()).

%% @doc 清理上次启动残骸（仅在 imboy 未运行时调用）。
%% 两类孤儿都会毒化后续所有 app 启动，必须先清：
%% ① barrel_mcp_registry / barrel_mcp_session 单例：mcp 测试 fixture 直接
%%    start_link 后随 fixture 正常退出——normal exit 信号被对端忽略，进程
%%    不死（套件隔离治理前的历史泄漏）；或 app 启动中途被杀泄漏。名字被占
%%    → 之后每次 imboy_sup child start 都 {already_started}。
%% ② ranch listener：启动中途被杀时已绑定 http_port，之后每次启动 eaddrinuse。
cleanup_start_orphans() ->
    lists:foreach(
        fun(Name) ->
            case whereis(Name) of
                undefined ->
                    ok;
                Pid ->
                    exit(Pid, kill)
            end
        end,
        %% imboy 单例孤儿黑名单（覆盖 imboy_sup/imboy_plugin_sup/msg_store_sup
        %% 全部 {local} 注册的子进程）：app 死亡时这些 gen_server 可能经
        %% normal-exit 吞信号幸存（run #11 实测 imboy_ws_action_registry 幸存
        %% → 之后每次 boot 的 plugin_sup child 必 {already_started} → 级联）。
        %% 仅在 app 未运行时执行，杀掉的必然是幸存残骸而非在役实例。
        [
            ack_retry_cache,
            agent_payment_compensation_worker,
            agent_rate_limiter,
            ai_agent_runtime,
            barrel_mcp_registry,
            barrel_mcp_session,
            billing_invoice_worker,
            elib_metric,
            imboy_cache,
            imboy_cache_sync,
            imboy_domain_event,
            imboy_mcp_tools,
            imboy_plugin_loader,
            imboy_plugin_sup,
            imboy_router_registry,
            imboy_ws_action_registry,
            license_notice_worker,
            login_attempt_ds,
            msg_burn_logic,
            msg_store_ds,
            msg_store_sup,
            msg_store_worker,
            olm_otk_cleanup_worker,
            user_deletion_logic,
            user_server
        ]
    ),
    %% registry 的 ETS 表随属主进程消失；persistent_term 由其 terminate 清理，
    %% kill 路径不触发 terminate，这里兜底擦除。
    try
        persistent_term:erase(barrel_mcp_handlers)
    catch
        _:_ -> ok
    end,
    timer:sleep(20),
    try
        ranch:stop_listener(imboy_listener)
    catch
        _:_ -> ok
    end,
    try
        ranch:stop_listener(imboy_listener_tls)
    catch
        _:_ -> ok
    end,
    ok.

%% ===================================================================
%% 周期 worker 测试静默（CP-TD-01F 跨套件隔离）
%% ===================================================================
%% 问题（W3 全量两次实证）：imboy_sup 拉起的周期 worker 在全量 eunit 期间
%% 持续真实轮询 PG，与套件的 elib_pg meck 竞态：
%%   * msg_store_worker（1s tick staging 表）：meck 期间撞套件期望（收到
%%     mock_conn 参数）→ meck:unload 窗口 undef（'function not exported'
%%     {elib_pg,query,3}/{elib_pg,with_tx,1}）→ sup 反复重启风暴；
%%   * bot_webhook_delivery_worker（1s claim_due outbox）：[WH01] batch
%%     crash error:undef；
%%   * msg_burn / moderation_sweep / user_deletion / credential_retention /
%%     agent_payment_compensation 等周期 sweep 在 meck 窗口内打真库
%%     （pg_down / purge failed / release_failed），白占 pooler 名额
%%     （max_count=80 vs PG max_connections=100，并发套件高峰偶发
%%     econnrefused）。
%% 修法（仅测试基建）：app 首次启动成功后由本长驻协调进程串行
%% terminate_child 静默下列周期 worker。测试侧一律直调导出函数
%% （msg_store_worker:do_write / bot_webhook_delivery_worker:execute /
%% agent_payment_compensation_worker:process_once）或 whereis-undefined
%% 时自建受控实例（user_deletion_logic / msg_burn_logic 套件为该写法），
%% 均不依赖常驻实例；套件自身的让位/自建实例不经 sup 注册，不受影响。
%% terminate_child 为显式管理操作，permanent child 不会因此自动重启；
%% app_already_started 分支不重复执行，不干扰运行中的套件状态。
-define(PERIODIC_WORKER_STOP_SPECS, [
    %% 每秒真实轮询型（W3 全量 CRASH 主角）。msg_store_worker 已移出本名单
    %% （2026-09-29 probe6 铁证）：worker 本体健康——探针 VM boot 后 1s tick
    %% 即清空 36 行 staging 积压（pending 36→0, processed 36）；而集成套件
    %% （msg_reaction 等 c2c/c2g_message_not_ready 9 败）stage 后轮询等真实
    %% 异步转正，依赖常驻 worker。历史上它是"W3 全量 CRASH 主角"，但该不稳
    %% 的真实根因已被本轮修复消除：sync 热重载 code:purge 静默杀 worker
    %% （do_boot 现预启动+pause+post stop 三重禁编）与双库分裂（补缺不覆写）。
    %% claim 设计本身并发安全（FOR UPDATE SKIP LOCKED + 30s 租约 + 退避重试）。
    {imboy_sup, bot_webhook_delivery_worker},
    %% 周期 sweep / 清理 / 补偿型（meck 窗口内真库错误 + 池名额占用）
    {imboy_sup, msg_burn_logic},
    {imboy_sup, moderation_sweep_logic},
    {imboy_sup, user_deletion_logic},
    {imboy_sup, credential_retention_worker},
    {imboy_sup, agent_payment_compensation_worker}
]).

quiesce_periodic_workers() ->
    lists:foreach(
        fun({Sup, Id}) ->
            _ = catch supervisor:terminate_child(Sup, Id),
            ok
        end,
        ?PERIODIC_WORKER_STOP_SPECS
    ).

%% @doc 清理资源
%% @param State setup 返回的状态
%% 套件隔离治理：app 在全量跑期间常驻，不再逐模块 stop/start。
%% 逐模块启停（数百次/全量）会撞启动竞态：barrel_mcp_registry 单例
%% {already_started} 使 imboy 拒绝启动、连接池反复重建、syn 集群表重置，
%% 之后所有模块连锁 noproc/{503,数据库忙}。各模块的 env/meck 隔离
%% 仍由各自 setup/after 负责，应用级状态由 imboy_app 的 test 分支保证。
eunit_cleanup({app_started, imboy}) ->
    ok;
eunit_cleanup({app_already_started, imboy}) ->
    ok;
eunit_cleanup({app_not_started, test_continues}) ->
    % 应用没有启动，不需要清理
    ok;
eunit_cleanup(_State) ->
    ok.

%% @doc 确保 imboy app（及其 sup 持有的命名 gen_server：barrel_mcp_*、
%% imboy_router_registry、imboy_ws_action_registry 等）可用，返回实例 pid。
%% 测试进程绝不能对这些 app 级命名服务自行 start_link：链接父进程（eunit
%% fixture）正常退出时 normal 信号杀不死未 trap_exit 的 gen_server，僵尸
%% 持名后 imboy app 每次启动都在 sup 失败，半启动/拆除循环直至
%% "Too many processes"（CI-00 run5 全量翻车根因）。
ensure_named_server(Mod) when is_atom(Mod) ->
    _ = eunit_setup(),
    case erlang:whereis(Mod) of
        Pid when is_pid(Pid) -> {ok, Pid};
        undefined -> {error, {not_started, Mod}}
    end.

%% @doc 尝试建立数据库连接
%% @return {ok, Conn} | {error, Reason}
eunit_try_db() ->
    eunit_try_db(30).

eunit_try_db(0) ->
    {error, no_connection};
eunit_try_db(AttemptsLeft) ->
    Driver = test_sql_driver(),
    try pooler:take_member(Driver, 100) of
        Pid when is_pid(Pid) ->
            {ok, Driver, Pid};
        error_no_members ->
            timer:sleep(100),
            case AttemptsLeft of
                1 -> {error, no_members};
                _ -> eunit_try_db(AttemptsLeft - 1)
            end
    catch
        exit:{noproc, _} ->
            timer:sleep(100),
            case AttemptsLeft of
                1 -> {error, no_pool};
                _ -> eunit_try_db(AttemptsLeft - 1)
            end;
        _:_ ->
            timer:sleep(100),
            case AttemptsLeft of
                1 -> {error, no_connection};
                _ -> eunit_try_db(AttemptsLeft - 1)
            end
    end.

%% @doc 启动应用并尝试连接数据库
%% @return {ok, Conn} | {error, Reason}
eunit_setup_with_db() ->
    SetupState = eunit_setup(),
    case eunit_try_db() of
        {ok, Driver, ConnPid} ->
            persistent_term:put({?MODULE, db_conn, ConnPid}, {Driver, SetupState}),
            {ok, ConnPid};
        {error, Reason} ->
            eunit_cleanup(SetupState),
            {error, Reason}
    end.

%% @doc 归还测试数据库连接并清理应用状态
eunit_cleanup_db(ConnPid) when is_pid(ConnPid) ->
    Key = {?MODULE, db_conn, ConnPid},
    case persistent_term:get(Key, undefined) of
        {Driver, SetupState} ->
            persistent_term:erase(Key),
            _ = catch pooler:return_member(Driver, ConnPid, ok),
            eunit_cleanup(SetupState);
        undefined ->
            ok
    end;
eunit_cleanup_db(_) ->
    ok.

%% @doc 启动应用，如果数据库不可用则返回 skip
%% @return ok | {skip, Reason}
eunit_setup_db_or_skip() ->
    case eunit_setup_with_db() of
        {ok, _Conn} -> ok;
        {error, Reason} -> {skip, "Database connection not available", Reason}
    end.

load_test_config() ->
    ConfigPath = test_config_path(),
    case file:consult(ConfigPath) of
        {ok, [ConfigList]} when is_list(ConfigList) ->
            load_config_entries(ConfigList);
        {ok, ConfigList} when is_list(ConfigList) ->
            load_config_entries(ConfigList);
        {error, Reason} ->
            io:format("Warning: Failed to load config ~p: ~p~n", [ConfigPath, Reason])
    end.

%% 补缺不覆写：-config（eunit-local 的 sys.eunit-relay，或 CI 物化口径）先于本函数
%% 加载并提供全套 env；此处再无条件 set_env 会用 config/sys.config（example 物化）
%% 的库指向覆盖它，造成「池化连接（app 启动时 env）与测试体连接（覆盖后 env）
%% 指向不同数据库」——实证（2026-09-29）：sys.local 指向 imboy_test_v1，覆盖后
%% 测试写 staging 进 imboy_v1，而 msg_store_worker 的池仍在 imboy_test_v1 claim
%% （恒空）→ staging 16932 行 0 转运 → 全部消息等待型集成测试
%% c2c/c2g_message_not_ready。改为仅补 undefined 键：有 -config 时保持其口径
%% （写/读同库），无 -config 裸跑时 fallback 加载语义不变。
load_config_entries(ConfigList) ->
    lists:foreach(
        fun
            ({App, Env}) when is_atom(App) andalso is_list(Env) ->
                lists:foreach(
                    fun({Key, Value}) ->
                        case application:get_env(App, Key) of
                            undefined ->
                                application:set_env(App, Key, Value);
                            {ok, _} ->
                                ok
                        end
                    end,
                    Env
                );
            (_) ->
                ok
        end,
        ConfigList
    ).

test_config_path() ->
    case os:getenv("IMBOY_TEST_CONFIG") of
        false ->
            filename:join([project_root_dir(), "config", "sys.config"]);
        "" ->
            filename:join([project_root_dir(), "config", "sys.config"]);
        Path ->
            filename:absname(Path)
    end.

%% ===================================================================
%% 测试池容量下限（CP-TD-01F 跨套件隔离）
%% ===================================================================
%% pooler:take_member/1（elib_pg:with_conn 所用）在无空闲成员时立即返回
%% error_no_members——该 API 形态（Timeout=0）不入等待队列、不触发扩容，
%% 池成员数永远停留在建池时的 init_count。配置漂移/污染路径会把 init_count
%% 压回个位数（cs_preflight_facts_pg_tests:ensure_test_pg_conf 的
%% set_env(imboy, pg_conf, #{init_count => 5, max_count => 40, ...}) 无条件
%% 覆写且不恢复），重并发用例（agent_task 32 worker 等）在共享的小池上
%% 集中 no_connection（W3/CP12 全量三轮实证）。
%% 此处在 app 启动前把测试池容量钉到下限之上：只影响本测试节点的基础
%% 设备就绪度，不改变任何业务语义与断言。
-define(MIN_POOL_INIT_COUNT, 40).

enforce_pool_capacity() ->
    case application:get_env(imboy, pg_conf) of
        {ok, PgConf} when is_map(PgConf) ->
            Init = maps:get(init_count, PgConf, 0),
            case Init >= ?MIN_POOL_INIT_COUNT of
                true ->
                    ok;
                false ->
                    Max = maps:get(max_count, PgConf, 0),
                    NewMax = erlang:max(Max, ?MIN_POOL_INIT_COUNT + 8),
                    application:set_env(
                        imboy,
                        pg_conf,
                        PgConf#{init_count := ?MIN_POOL_INIT_COUNT, max_count := NewMax}
                    )
            end;
        _ ->
            ok
    end.

test_http_port() ->
    case os:getenv("TEST_HTTP_PORT") of
        false ->
            19800;
        "" ->
            19800;
        Value ->
            try list_to_integer(Value) of
                Port -> Port
            catch
                _:_ -> 19800
            end
    end.

test_sql_driver() ->
    case application:get_env(imboy, sql_driver) of
        {ok, Driver} when is_atom(Driver) ->
            Driver;
        _ ->
            pgsql
    end.

project_root_dir() ->
    project_root_dir(".").

project_root_dir(StartDir) ->
    find_project_root(filename:absname(StartDir), 10).

find_project_root(Dir, 0) ->
    Dir;
find_project_root(Dir, N) ->
    ConfigPath = filename:join([Dir, "config", "sys.config"]),
    MakefilePath = filename:join([Dir, "Makefile"]),
    case filelib:is_regular(ConfigPath) andalso filelib:is_regular(MakefilePath) of
        true ->
            Dir;
        false ->
            Parent = filename:dirname(Dir),
            case Parent =:= Dir of
                true ->
                    Dir;
                false ->
                    find_project_root(Parent, N - 1)
            end
    end.
