-module(imboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @doc 启动 supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 初始化 supervisor
%% Note: Side effects in init/1 may cause issues, use ok to ignore return values
init([]) ->
    % application:ensure_all_started(pgo),
    % {PoolName, PoolConfig} = config_ds:env(pgo),
    % PgoChildSpec = #{
    %     id => pgo_pool
    %     , start => {pgo_pool, start_link, [PoolName, PoolConfig]}
    %     , shutdown => 1000
    % },

    % 启动 pooler 应用和连接池
    _ = application:start(pooler),
    PgConf = config_ds:env(pg_conf),
    _ = pooler:new_pool(PgConf),

    % https://blog.csdn.net/Dylan_2018/article/details/110150142
    % child_spec() = #{id => child_id(),             % mandatory
    %     start => mfargs(),            % mandatory
    %     restart => restart(),         % optional
    %     significant => significant(), % boolean() optional
    %     shutdown => shutdown(),       % brutal_kill | timeout() optional 当子进程为supervisor进程时，应该设置成infinity。默认为50000;
    %     type => worker(),             % optional
    %     modules => modules()}         % optional
    UserServer = #{
        id => user_server,
        start => {user_server, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [user_server]
    },

    % KVProps default is [{depcache_memory_max, 100}],
    KVProps = config_ds:env(depcache, [{depcache_memory_max, 100}]),
    IMBoyCache = #{
        id => imboy_cache,
        start => {imboy_cache, start_link, [KVProps]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => dynamic
    },
    % 根据配置决定是否启动分布式缓存同步服务器
    CacheSyncSpec =
        case application:get_env(imboy, dsync_enabled, false) of
            true ->
                [
                    #{
                        id => imboy_cache_sync,
                        start => {imboy_cache_sync, start_link, []},
                        restart => permanent,
                        shutdown => 5000,
                        type => worker,
                        modules => [imboy_cache_sync]
                    }
                ];
            false ->
                []
        end,

    % 领域事件总线（DDD 充血改造地基 / T0.1）
    % Domain event bus: 聚合根产出的领域事件经此派发给订阅者执行副作用。
    % 需早于业务 worker 启动，确保 logic 外壳 publish 时总线已就绪。
    DomainEventBus = #{
        id => imboy_domain_event,
        start => {imboy_domain_event, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => dynamic
    },

    % 消息写入队列监督树
    MsgWriteQueueSup = #{
        id => msg_store_sup,
        start => {msg_store_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [msg_store_sup]
    },

    % 消息自毁定时清理工作进程
    MsgBurnWorker = #{
        id => msg_burn_logic,
        start => {msg_burn_logic, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [msg_burn_logic]
    },

    % 账号注销自动清理工作进程
    UserDeletionWorker = #{
        id => user_deletion_logic,
        start => {user_deletion_logic, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [user_deletion_logic]
    },

    % License 到期提醒定时 worker（默认禁用 + 收件人空 = 不发，需 sys.config 显式启用）
    LicenseNoticeWorker = #{
        id => license_notice_worker,
        start => {license_notice_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [license_notice_worker]
    },

    % SaaS 账单生成定时 worker（默认禁用，需 sys.config 显式启用；只生成账单不扣款）
    BillingInvoiceWorker = #{
        id => billing_invoice_worker,
        start => {billing_invoice_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [billing_invoice_worker]
    },

    % 登录失败次数限制（ETS 表 login_attempt_ets 由此 gen_server 创建）
    LoginAttemptServer = #{
        id => login_attempt_ds,
        start => {login_attempt_ds, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [login_attempt_ds]
    },

    % Prometheus 风格指标收集器（metrics_handler / message_ds / msg_ack_logic 依赖）
    MetricWorker = #{
        id => elib_metric,
        start => {elib_metric, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [elib_metric]
    },

    % 插件清单加载器（启动期扫描 priv/plugins/*/plugin.config，写入 persistent_term）
    % Plugin manifest loader (scans priv/plugins/*/plugin.config at startup, writes persistent_term)
    % 详见 docs/plugin/contract.md §10、.claude/plan/industrial-plugin-architecture-roadmap.md P0-T3
    %
    % restart=transient：loader 是可选基础设施，崩溃后正常退出（reason=normal/shutdown）
    % 不重启；异常崩溃才重启。这与 loader "插件失败不影响 core" 的语义一致。
    % V2 评审 MEDIUM-2 修复：原 permanent 在 priv_dir bad_name 等场景会拖垮 sup tree。
    PluginLoader = #{
        id => imboy_plugin_loader,
        start => {imboy_plugin_loader, start_link, []},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [imboy_plugin_loader]
    },

    % 插件总监督树（P1-T1）：监督 4 个生产插件的 supervisor
    % Plugin top-level supervisor: supervises channel/moment/location/group_collab sups
    % 启动顺序在 PluginLoader 之后：先 loader 把 manifest 写入 persistent_term，
    % 再启动 plugin sup，确保插件 worker 启动时可安全查询 manifest。
    % 详见 docs/plugin/contract.md §8、.claude/plan/industrial-plugin-architecture-roadmap.md P1-T1
    PluginSup = #{
        id => imboy_plugin_sup,
        start => {imboy_plugin_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [imboy_plugin_sup]
    },

    % AI Agent 常驻在线 runtime（T1.3）：把启用中 agent 注册进 syn 在线态（presence）。
    % transient：DB 未就绪时 refresh 优雅跳过并定时重试，非核心，崩溃正常退出不重启。
    AiAgentRuntime = #{
        id => ai_agent_runtime,
        start => {ai_agent_runtime, start_link, []},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [ai_agent_runtime]
    },

    % MCP 协议引擎 gen_server（T3.1 vendored barrel_mcp）：registry + session。
    % transient：非核心，崩溃正常退出不重启。MCP handler（T3.2）依赖其在线。
    McpRegistry = #{
        id => barrel_mcp_registry,
        start => {barrel_mcp_registry, start_link, []},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [barrel_mcp_registry]
    },
    McpSession = #{
        id => barrel_mcp_session,
        start => {barrel_mcp_session, start_link, []},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [barrel_mcp_session]
    },

    % MCP tool wrapper 注册器（T3.4）：registry 就绪后注册全部只读 tool。
    % transient：非核心；监听 registry DOWN，异常重启后自动重注册（reg_all 幂等）。
    McpTools = #{
        id => imboy_mcp_tools,
        start => {imboy_mcp_tools, start_link, []},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [imboy_mcp_tools]
    },

    Specs =
        [
            IMBoyCache,
            % , PgoChildSpec
            DomainEventBus,
            MetricWorker,
            PluginLoader,
            PluginSup,
            LoginAttemptServer,
            UserServer,
            MsgWriteQueueSup,
            MsgBurnWorker,
            UserDeletionWorker,
            LicenseNoticeWorker,
            BillingInvoiceWorker,
            AiAgentRuntime,
            McpRegistry,
            McpSession,
            McpTools
        ] ++ CacheSyncSpec,
    % intensity/period 放宽：顶层 supervisor 下挂了十余个 worker，
    % 5次/50s 门槛偏紧，短时多个worker同时重启（如DB抖动）易触发supervisor整体退出
    Restart = #{strategy => one_for_one, intensity => 10, period => 60},
    {ok, {Restart, Specs}}.
