%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin behaviour 类型定义文件
%%% imboy_plugin behaviour type definitions
%%%
%%% Source of truth: doc/plugin/contract.md §4.2
%%% Behaviour module: src/lib/imboy_plugin.erl
%%% Contract version: 1.0
%%%
%%% 本文件定义 imboy_plugin behaviour 所需的全部 type spec。
%%% This file defines all type specs required by the imboy_plugin behaviour.
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-ifndef(IMBOY_PLUGIN_HRL).
-define(IMBOY_PLUGIN_HRL, true).

%% ===================================================================
%% 基础类型 / Primitive types
%% ===================================================================

-type plugin_name() :: atom().
-type plugin_version() :: binary().     %% e.g., <<"1.0.0">>
-type contract_version() :: {Major :: pos_integer(), Minor :: non_neg_integer()}.

%% ===================================================================
%% 路由 / Routes
%% ===================================================================

-type route_spec() :: #{
    method := binary(),
    path := binary(),
    handler := atom(),
    action := atom(),
    required_feature => atom() | undefined
}.

%% ===================================================================
%% 数据库迁移 / Migrations
%% ===================================================================

-type migration_spec() :: #{
    dir := binary(),
    table_prefix := binary(),
    preserve_on_uninstall := boolean(),
    data_export_hook => mfa() | undefined
}.

%% ===================================================================
%% 特性开关 / Features
%% ===================================================================

-type audience_spec() ::
    #{kind := all} |
    #{kind := tenant, tenant_ids := [binary()]} |
    #{kind := uid_hash, buckets := [0..99]}.

-type feature_spec() :: #{
    default := boolean(),
    description := binary(),
    rollout := always | percentage | canary,
    percentage => 0..100,
    audience := audience_spec()
}.

%% ===================================================================
%% 限流与配额 / Limits and budget
%% ===================================================================

-type limits_spec() :: #{
    rps_per_uid := non_neg_integer(),
    max_concurrent_calls := non_neg_integer(),
    msg_quota_per_day := non_neg_integer()
}.

-type budget_spec() :: #{
    max_connections := non_neg_integer(),
    max_subscriptions := non_neg_integer(),
    mem_mb_soft_limit := pos_integer()
}.

%% ===================================================================
%% 降级与熔断 / Degrade and circuit breaker
%% ===================================================================

-type degrade_spec() :: #{
    on_unhealthy := isolate | route_404 | pass_through
}.

-type circuit_breaker_spec() :: #{
    enabled := boolean(),
    threshold_pct := 1..99,
    window_sec := pos_integer()
}.

%% ===================================================================
%% 通信 / Communication
%% ===================================================================

-type topic_meta() :: #{
    e2ee_passthrough => boolean(),
    requires_user_consent => boolean()
}.

-type capability() :: #{
    name := binary(),
    version := pos_integer(),
    api_module := atom(),
    default_timeout_ms := pos_integer(),
    idempotent := boolean()
}.

%% ===================================================================
%% 健康检查 / Health
%% ===================================================================

-type health_report() :: #{
    status := healthy | degraded | unhealthy,
    reason => binary(),
    metrics => #{atom() => term()}
}.

%% ===================================================================
%% 完整清单 / Full manifest
%% ===================================================================

-type manifest() :: #{
    name := plugin_name(),
    version := plugin_version(),
    contract_version := contract_version(),
    kind := plugin | aggregate_plugin,
    description := binary(),
    depends_on := #{plugin_name() => binary()},
    requires_capabilities := [binary()],
    min_core_version := binary(),
    children := [plugin_name()],
    routes := [route_spec()],
    migrations := migration_spec(),
    features := #{atom() => feature_spec()},
    limits := limits_spec(),
    budget := budget_spec(),
    degrade := degrade_spec(),
    circuit_breaker := circuit_breaker_spec(),
    entries := #{app => [atom()], admin => [atom()]},
    i18n := #{keys := [binary()]},
    subscribes := [binary()],
    publishes := [binary()],
    publishes_meta := #{binary() => topic_meta()},
    audit := #{events := [binary()]},
    meta := map()
}.

-endif.  %% IMBOY_PLUGIN_HRL
