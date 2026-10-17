-module(imboy_sup).
-behaviour(supervisor).

-include("generated/imboy_product_features.hrl").

-export([start_link/0]).
-export([init/1]).
%% TSID-07：部署配置链合同测试直调（env > application env > 默认 的合成结果）
-export([tsid_init_config/0]).

%% @doc 启动 supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private TSID init 配置（env > application env > 默认 的合成结果）。
%% V10 无持久化、无 guard 进程：init 纯内存（fixed offset 采集 + atomics
%% 游标数组 + 静态槽位表），在 sup init 内直调即可，失败即 app 拒启。
tsid_init_config() ->
    ok = forbidden_tsid_env_check(),
    #{
        dc_id => application:get_env(imboy, tsid_dc_id, 1),
        node_id => application:get_env(imboy, tsid_node_id, 1),
        dc_bits => application:get_env(imboy, tsid_dc_bits, 3),
        capacity_wait_timeout_ms =>
            application:get_env(imboy, tsid_capacity_wait_timeout_ms, 100),
        max_clock_drift_ms => application:get_env(imboy, tsid_max_clock_drift_ms, 5000)
    }.

%% 误配拒启检查：tsid_lock_provider 键已随 V10 删除（V10 单 BEAM 无跨实例
%% 互斥，唯一性边界见仓库 docs 与 .Codex/tsid_v10/risks.md R4——部署侧
%% 保证 (dc_id, node_id) 全局唯一）。任何环境显式设置即为误配，error →
%% imboy_sup init 失败 → app 拒启（fail-fast，部署时爆炸远好于带病静默运行）。
forbidden_tsid_env_check() ->
    case application:get_env(imboy, tsid_lock_provider) of
        undefined ->
            ok;
        {ok, Value} ->
            error(
                {tsid_config_forbidden, #{
                    key => tsid_lock_provider,
                    value => Value,
                    reason =>
                        <<
                            "tsid_lock_provider is not configurable: V10 TSID has no "
                            "cross-instance lock. Remove {imboy, tsid_lock_provider} "
                            "from config and ensure unique (dc_id, node_id) per node."
                        >>
                }}
            )
    end.

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
    PgConf = configure_pg_pool(config_ds:env(pg_conf)),
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
    % V2.2.1：缓存同步服务器无条件启动——它现在同时承担本地职责
    % （generation 栅栏元数据表 19.3、版本元数据），跨节点发布仍由
    % dsync_enabled 控制（should_broadcast / invalidate 的 legacy 路径）。
    CacheSyncSpec = [
        #{
            id => imboy_cache_sync,
            start => {imboy_cache_sync, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [imboy_cache_sync]
        }
    ],

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

    % R-02：处置动作到期 sweep（审计状态闭环，默认启用 5 分钟一轮）
    ModerationSweepWorker = #{
        id => moderation_sweep_logic,
        start => {moderation_sweep_logic, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [moderation_sweep_logic]
    },

    ModerationShadowWorker = #{
        id => moderation_shadow,
        start => {moderation_shadow, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [moderation_shadow]
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

    % Olm OTK 已消费审计行清理定时 worker（默认禁用，需 sys.config 显式启用；不删任何审计行直到启用）
    OlmOtkCleanupWorker = #{
        id => olm_otk_cleanup_worker,
        start => {olm_otk_cleanup_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [olm_otk_cleanup_worker]
    },

    % 过期凭证（验证码，含 PII）保留清理 worker（T-02；默认禁用，需 sys.config 显式启用）
    % WH-01：Bot 出站 webhook 交付 worker（outbox 拉取+有界重试+死信）
    BotDeliveryWorker = #{
        id => bot_webhook_delivery_worker,
        start => {bot_webhook_delivery_worker, start_link, []},
        type => worker,
        shutdown => 5000,
        restart => permanent,
        significant => false
    },

    % 过期凭证（验证码，含 PII）保留清理 worker（T-02；默认禁用，需 sys.config 显式启用）
    CredentialRetentionWorker = #{
        id => credential_retention_worker,
        start => {credential_retention_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [credential_retention_worker]
    },

    % Agent 支付预留补偿 worker：持久化 outbox 崩溃恢复，默认必须启用
    AgentPaymentCompensationWorker = #{
        id => agent_payment_compensation_worker,
        start => {agent_payment_compensation_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [agent_payment_compensation_worker]
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

    %% V10：TSID 无进程化。init 纯内存（fixed offset + atomics 游标），
    %% 必须早于任何可生成 ID 的 worker；失败（配置非法/重复初始化冲突）
    %% 即 sup init 崩 → app 拒启（fail-closed）。监听器早于本 sup 启动，
    %% 就绪窗口由 /readyz（elib_tsid:readiness/0）挡住。
    ok = elib_tsid:init(tsid_init_config()),
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
            ModerationSweepWorker,
            ModerationShadowWorker,
            UserDeletionWorker,
            LicenseNoticeWorker,
            BillingInvoiceWorker,
            OlmOtkCleanupWorker,
            BotDeliveryWorker,
            CredentialRetentionWorker,
            AgentPaymentCompensationWorker
        ] ++
            %% Q02 二期域常驻 runtime 条件启动（AC03"无任务/外呼"）：
            %% AI Agent presence 与 MCP 协议引擎仅在其域 feature 开启时
            %% 进入 supervision tree；phase1_delivery 下不启动、零后台触发。
            %% 注意 BotDeliveryWorker 不在此列——bot_delivery outbox 是
            %% OA 企业 webhook 共享投递设施（F08），交付关闭 Bot 产品时仍
            %% 必须运行。
            feature_gated_specs([
                {ai_assistant, AiAgentRuntime},
                {mcp, McpRegistry},
                {mcp, McpSession},
                {mcp, McpTools}
            ]) ++
            eb_retention_purge_children() ++ CacheSyncSpec,
    % intensity/period 放宽：顶层 supervisor 下挂了十余个 worker，
    % 5次/50s 门槛偏紧，短时多个worker同时重启（如DB抖动）易触发supervisor整体退出
    % CI-00：再放宽到 50/60s——全量 eunit 实测单个 child（imboy_cache）被
    % 外部异常 kill 时 ~80ms/次的重启风暴 10 连杀即触发 intensity 上限，
    % 拖垮整个 app（run10）；单子进程崩溃风暴不应带崩整个 IM 服务。
    Restart = #{strategy => one_for_one, intensity => 50, period => 60},
    {ok, {Restart, Specs}}.

%% @private feature 门控的 child spec 过滤：域 feature 关闭时不启动。
%% imboy_feature:enabled/1 读 policy 有效特性（profile 白名单 + 运行时
%% 覆盖 + 交付锁），在 sup init 时求值——profile 由 config_ds/env 提供，
%% 早于 DB 覆盖可用时回落 profile 预设。
-spec feature_gated_specs([{atom(), map()}]) -> [map()].
feature_gated_specs(Specs) ->
    [
        Child
     || {Feature, Child} <- Specs,
        imboy_feature:enabled(Feature)
    ].

%% 企业留存 bounded purge 定时 worker（R4-②：默认禁用 + 目标空 = 不清理任何
%% 行，需 sys.config 显式启用；补全 F-1 孤儿资产清理的调度入口）。模块属于
%% enterprise_business 特性（生成器 FEATURE_BACKEND_MODULES 已登记）：特性未
%% 选中时 worker 被物理裁剪，本子进程组随之整体剔除——避免 boot 期 undef
%% （F-EB10-1 同类崩溃面，与 imboy_router 的特性段同一保护口径；条件编译放
%% 文件级整函数分支，erlfmt 不支持函数体内 -ifdef）。
-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS).
eb_retention_purge_children() ->
    [
        #{
            id => eb_retention_purge_worker,
            start => {eb_retention_purge_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [eb_retention_purge_worker]
        }
    ].
-else.
eb_retention_purge_children() ->
    [].
-endif.

configure_pg_pool(#{start_mfa := {epgsql, connect, Args}} = PgConf) ->
    PgConf#{start_mfa := {imboy_pg_connection, connect, Args}};
configure_pg_pool(#{start_mfa := {imboy_pg_connection, connect, _}} = PgConf) ->
    PgConf;
configure_pg_pool(PgConf) ->
    erlang:error({unsupported_pg_start_mfa, PgConf}).
