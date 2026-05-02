-module(imboy_plugin).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin behaviour - 应用级插件契约
%%% imboy_plugin behaviour - application-level plugin contract
%%%
%%% 本模块定义 IMBoy 后端"插件"的工程契约接口。任何符合本 behaviour
%%% 的 Erlang/OTP application 均可被 imboy_plugin_loader 发现、加载
%%% 并接入运行时。
%%%
%%% 重要 / Important:
%%%   本 behaviour 是 application 级的（pure function callbacks），
%%%   NOT gen_server。插件内部的 gen_server worker 由 <name>_sup
%%%   直接监督，与本 behaviour 无关。
%%%
%%%   This is an application-level behaviour (pure function callbacks),
%%%   NOT gen_server. Plugin-internal gen_server workers are supervised
%%%   by <name>_sup directly, unrelated to this behaviour.
%%%
%%% Source of truth: doc/plugin/contract.md
%%% Contract version: 1.0
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-include("imboy_plugin.hrl").

%% ===================================================================
%% Type exports
%% ===================================================================

-export_type([
    plugin_name/0,
    plugin_version/0,
    contract_version/0,
    manifest/0,
    route_spec/0,
    migration_spec/0,
    audience_spec/0,
    feature_spec/0,
    limits_spec/0,
    budget_spec/0,
    degrade_spec/0,
    circuit_breaker_spec/0,
    topic_meta/0,
    capability/0,
    health_report/0
]).

%% ===================================================================
%% Behaviour callbacks (7)
%% Source of truth: doc/plugin/contract.md §4.1
%% ===================================================================

%% @doc 插件清单 / Plugin manifest.
%% 实现要求：从 persistent_term 读取 loader 在 install 阶段已解析好的 manifest。
%% Implementation: read pre-parsed manifest from persistent_term.
%% 必须 < 10ms，禁止运行时 toml 解析。
%% MUST be < 10ms; MUST NOT re-parse toml at runtime.
-callback manifest() -> manifest().

%% @doc 启动钩子 / Start hook.
%% 插件 supervisor 启动后调用，进行业务级初始化。
%% Called after plugin supervisor starts; for business-level init.
%% 不得阻塞超过 5 秒；I/O 操作应放到独立 worker。
%% MUST NOT block > 5s; I/O should be deferred to separate workers.
-callback start(StartArgs :: map()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

%% @doc 停止钩子 / Stop hook.
%% 插件 disable 或 uninstall 时调用，应清理 ETS / 注销 syn / 关闭外部连接。
%% Called on disable or uninstall; cleanup ETS / unregister syn / close external connections.
-callback stop(State :: term()) -> ok.

%% @doc 数据库迁移钩子 / Migration hook.
%% install/upgrade 时调用 up；uninstall 且 preserve_on_uninstall=false 时调用 down。
%% Called with up on install/upgrade; with down on uninstall when preserve_on_uninstall=false.
-callback migrate(
    Direction :: up | down,
    FromVersion :: binary(),
    ToVersion :: binary()
) -> ok | {error, Reason :: term()}.

%% @doc 路由列表 / Routes list.
%% 实现：直接 maps:get(routes, manifest())，不重复 I/O。
%% Implementation: just maps:get(routes, manifest()); no extra I/O.
-callback routes() -> [route_spec()].

%% @doc 能力声明 / Capability declaration.
%% 声明本插件向外提供的 capability，供其他插件 requires_capabilities 引用。
%% Capabilities exposed to other plugins via requires_capabilities.
-callback capabilities() -> [capability()].

%% @doc 健康检查 / Health check.
%% loader 强制 100ms 硬超时（通过 timer kill 实现）；插件不应在此 callback 内写 try/catch。
%% Loader enforces 100ms hard timeout via timer kill; plugin authors MUST NOT write try/catch here.
-callback health() -> health_report().

%% ===================================================================
%% Public API
%% ===================================================================

-export([contract_version/0]).

%% @doc 当前 core 支持的契约版本（major, minor）。
%% Current core-supported contract version (major, minor).
%% 见 doc/plugin/contract.md §9.1 兼容矩阵。
%% See doc/plugin/contract.md §9.1 compatibility matrix.
-spec contract_version() -> contract_version().
contract_version() ->
    {1, 0}.
