# 插件契约 / Plugin Contract

> **Last Updated**: 2026-04-29（架构评审反馈整合版）
> **Status**: 长期插件架构契约文档（Phase 0 立项版，contract_version=1.0）
> **Scope**: 定义 IMBoy 后端"插件"的形态、清单格式、behaviour 接口、命名约定与通信规则
> **Source of truth**: `src/lib/imboy_plugin.erl`（待 P0-T1 实现）、`include/imboy_plugin.hrl`、`priv/plugins/*/plugin.toml`
> **Related docs**: `.claude/plan/industrial-plugin-architecture-roadmap.md`、`doc/architecture/overview.md`、`doc/architecture/adr/2026-03-15-modular-monolith-boundaries.md`、`priv/plugin-examples/example/plugin.config`（P0-T5 教学样板，第三方插件作者参考）
> **简体中文为权威版本，本文采用 Pattern A 同节并排双语 / Chinese is authoritative; bilingual sections side-by-side**

---

## 0. 术语表 / Glossary

| 术语 / Term | 定义 / Definition |
|------------|-------------------|
| **plugin（插件）** | 独立 OTP application，符合本契约，可被 loader 发现并启用/禁用 |
| **manifest（清单）** | 插件描述数据，源文件为 `plugin.toml`，运行时表现为 Erlang map |
| **capability（能力）** | 插件向外暴露的版本化 API；通过 `capability call` 跨插件调用，**同步** |
| **feature（特性）** | 插件内部可独立开关的子功能（开/关/灰度），运维或前端可控 |
| **entry（入口）** | 客户端 UI 显示位的符号占位，由 manifest 声明，前端按需渲染 |
| **route（路由）** | HTTP/WS 端点，cowboy dispatch 注册项 |
| **topic（主题）** | `imboy_plugin_bus` 异步事件主题 |
| **core（核心）** | 不可禁用的基础能力集（cache / syn / pool / tsid / ws_push / msg_seq …） |
| **contract_version（契约版本）** | 本文档定义的 behaviour + manifest schema 兼容版本号；与 plugin version 不同 |

---

## 1. 概述 / Overview

**中文**：本文档定义 IMBoy 后端"插件"的工程契约。任何符合本契约的 Erlang/OTP application 均可被 `imboy_plugin_loader` 发现、加载并接入运行时。本契约是路线图中 Phase 0 的核心交付物，后续所有 Phase 在此基础上扩展。

**English**: This document defines the engineering contract for IMBoy backend "plugins". Any Erlang/OTP application that conforms to this contract can be discovered, loaded, and wired into the runtime by `imboy_plugin_loader`. This contract is the central deliverable of Phase 0 in the roadmap; subsequent phases build on it.

### 设计目标 / Design Goals

| 目标 / Goal | 含义 / Meaning |
|------------|---------------|
| 隔离 / Isolation | 插件崩溃不影响 core 与其他插件 / A plugin crash MUST NOT affect core or other plugins |
| 显式契约 / Explicit contract | 插件能力、依赖、副作用全部声明在 manifest / All capabilities, dependencies, side-effects MUST be declared in manifest |
| 可演进 / Evolvable | 契约本身遵循 semver；不兼容变更需 ADR / The contract itself follows semver; breaking changes require an ADR |
| Let It Crash 友好 / Let-It-Crash friendly | 插件按 OTP 监督树管理，禁止业务级 try/catch 兜底 / Plugins managed via OTP supervision; no business-level try/catch fallbacks |

---

## 2. 插件定义 / Plugin Definition

### 2.1 什么是插件 / What is a plugin

**中文**：插件是一个**独立的 OTP application**，提供以下若干能力中的一项或多项：
- 新的 HTTP / WebSocket 路由
- 新的业务领域（logic + ds + repo）
- 新的 supervisor 监督的后台 worker
- 新的 admin 管理面板入口
- 新的客户端（Flutter）UI 入口

**English**: A plugin is a **standalone OTP application** that provides one or more of:
- New HTTP / WebSocket routes
- New business domain (logic + ds + repo)
- New background workers under its own supervisor
- New admin panel entries
- New client (Flutter) UI entries

### 2.2 不是插件的东西 / What is NOT a plugin

| 类型 / Type | 应放在 / Belongs to | 原因 / Reason |
|------------|-------------------|---------------|
| 通用工具函数 / Generic utilities | `src/lib/elib_*` | 无副作用、无监督树需求 / No side effects, no supervision needs |
| 协议编解码 / Protocol codec | `src/lib/imboy_codec`、`imboy_frame` | 跨插件共享，不应耦合特定业务 / Shared cross-plugin, should not couple to specific business |
| 核心基础设施 / Core infrastructure | `src/lib/imboy_cache`、`imboy_syn`、`elib_pg` | 插件依赖之，不可被插件替换 / Depended on by plugins, not replaceable |
| 必选业务 / Mandatory business | `src/api/auth_*`、`passport_*`、`user_*`、`msg_*` | 系统不可缺，不存在禁用语义 / System cannot operate without; no "disable" semantic |

> **判定原则 / Decision rule**：能被运维**禁用且系统仍可正常 IM 收发**的功能 → 插件；否则 → core。
> If ops can **disable it and the system still functions as IM** → plugin; otherwise → core.

---

## 3. 清单格式 / Manifest Format

### 3.1 文件位置 / File location

```
priv/plugins/<plugin_name>/plugin.config   # Phase 0 实际加载格式（Erlang terms）
priv/plugins/<plugin_name>/plugin.toml     # 未来扩展（Phase 1+，引入 tomerl）
```

**Phase 0 文件格式决策 / Phase 0 file format decision**：

| 维度 / Aspect | Phase 0：`plugin.config` (Erlang terms) | Future：`plugin.toml` (TOML 1.0) |
|---------------|----------------------------------------|----------------------------------|
| 解析 / Parser | `file:consult/1`（OTP 内置） | `tomerl`（待 Phase 1+ 引入 dep） |
| 依赖 / Deps | 零依赖 | 新增 dep |
| 文件内容 / Content | 单一 Erlang map term + `.` | TOML 1.0 |
| 类型校验 / Type check | dialyzer 静态校验 | 运行时 + dialyzer |
| 与 imboy 现有生态 | 与 `sys.config` / `relx.config` 一致 | 新格式 |

> 决策理由 / Rationale：Phase 0 优先零依赖与 imboy 生态一致性。`imboy_plugin_toml` 模块名保留为历史命名（向后兼容已发布契约），实际加载 `plugin.config`。Phase 1+ 引入 `tomerl` 后支持 `.toml`，同时考虑模块重命名为 `imboy_plugin_manifest`。
>
> Phase 0 prioritizes zero-dependency and ecosystem consistency. Module name `imboy_plugin_toml` is retained for historical backward compatibility (already published in v1.0 contract); actually loads `plugin.config`. Phase 1+ adds `.toml` support via `tomerl`, with a possible rename to `imboy_plugin_manifest`.

### 3.1.1 plugin.config 示例 / plugin.config example

```erlang
%% priv/plugins/channel/plugin.config
%% 单一 map term，以点号 `.` 结尾
%% Single map term, terminated by `.`
#{
    name => channel,
    version => <<"1.0.0">>,
    contract_version => {1, 0},
    kind => plugin,
    description => <<"频道 (Channel) 订阅与邀请系统"/utf8>>,
    %% ... (其余字段同 §3.2 toml schema 的对应键)
    %% ... (other fields map 1:1 to §3.2 toml schema keys)
    meta => #{
        author => <<"imboy-pub">>,
        license => <<"Apache-2.0">>,
        created_at => <<"2026-03-15">>
    }
}.
```

### 3.2 完整 schema 与示例 / Full schema and example

```toml
# ===== 必填字段 / Required fields =====

# 插件名（snake_case，全集群唯一，与 OTP application 名一致）
# Plugin name (snake_case, unique cluster-wide, matches OTP application name)
name = "channel"

# 版本（严格遵循 semver 2.0）
# Version (strict semver 2.0)
version = "1.0.0"

# 一句话描述（中文为主，英文括注关键术语）
# One-line description (Chinese primary, English in parens for key terms)
description = "频道 (Channel) 订阅与邀请系统"

# 契约版本（声明本插件兼容哪个版本的 imboy_plugin behaviour）
# 兼容规则见 §9.1：major 必须等于 core；minor 必须 ≤ core
# Contract version (declares which imboy_plugin behaviour this plugin targets)
# Compatibility: §9.1 — major must equal core; minor must be ≤ core
contract_version = "1.0"

# 插件类型 / Plugin kind
# - "plugin": 标准独立插件 / standard standalone
# - "aggregate_plugin": 聚合插件，含子插件 / aggregate, contains children
kind = "plugin"

# ===== 依赖 / Dependencies =====

# 依赖的其他插件（拓扑顺序解析；semver 约束）
# Other plugins this depends on (resolved by topological sort; semver constraints)
[depends_on]
# 当前 Phase 0 仅支持 ^X.Y.Z 与精确版本；Phase 4 扩展 ~> >= < ||
# Phase 0 supports only ^X.Y.Z and exact versions; ~> >= < || in Phase 4
# 例 / Example: moment = "^1.0"

# 依赖的 core 能力（capability，由 core 提供并版本化）
# Required core capabilities (versioned, provided by core)
requires_capabilities = [
    "imboy.cache@1",
    "imboy.syn@1",
    "imboy.tsid@1",
    "imboy.ws_push@2",
    "imboy.msg_seq@1",   # conv_seq 严格顺序，禁止插件自实现 / strict conv_seq, plugins MUST NOT reinvent
]

# 最低 core 版本
# Minimum core version
min_core_version = "1.0.0-rc.2"

# ===== 子插件（仅 aggregate_plugin 使用）/ Children (only for aggregate_plugin) =====

# children = ["vote", "schedule", "task"]

# ===== 路由 / Routes =====

# 本插件提供的 HTTP/WS 路由（cowboy 格式）
# 强约束：path 必须以 /v{n}/<plugin_name>/ 开头（除非显式声明 route_namespace_override + ADR）
# Hard constraint: path MUST start with /v{n}/<plugin_name>/ (unless route_namespace_override + ADR)
[[routes]]
method = "POST"
path = "/v1/channel/discover"
handler = "channel_handler"
action = "discover"
required_feature = "channel_discover"

[[routes]]
method = "POST"
path = "/v1/channel/invitation"
handler = "channel_handler"
action = "create_invitation"
required_feature = "channel_invitation"

# ===== 数据库迁移 / Database migrations =====

[migrations]
# 迁移文件目录（相对插件根，从 plugin.toml 所在位置算起）
# Migrations directory (relative to plugin.toml location)
dir = "priv/migrations"

# 表名前缀（强制约束，避免命名冲突）
# Table name prefix (enforced to avoid naming collision)
table_prefix = "channel_"

# 卸载时是否保留数据（true=保留，false=删除；默认 true）
# Whether to keep data on uninstall (true=keep, false=drop; default true)
preserve_on_uninstall = true

# GDPR / 合规导出钩子（Phase 4 实现，Phase 0 仅留位）
# GDPR / compliance data export hook (implemented in Phase 4, Phase 0 reserves the slot)
# data_export_hook = "channel_logic:export_user_data/2"

# ===== 特性开关 / Feature flags（含灰度）=====

# 本插件提供的可独立开关的子特性
# Sub-features that can be independently toggled
#
# rollout: always | percentage | canary
# audience: all | tenant | uid_hash
# Phase 0 仅校验字段格式，Phase 4 兑现灰度语义
# Phase 0 only validates field shape; Phase 4 honors rollout semantics
[features.channel]
default = true
description = "频道基础订阅"
rollout = "always"
audience = { kind = "all" }

[features.channel_discover]
default = true
description = "频道发现"
rollout = "always"
audience = { kind = "all" }

[features.channel_invitation]
default = true
description = "频道邀请"
rollout = "percentage"
percentage = 100
audience = { kind = "all" }

[features.channel_order]
default = false
description = "频道付费订阅（实验性）"
rollout = "canary"
audience = { kind = "uid_hash", buckets = [0, 1, 2] }   # 取 uid mod 100，落在 0/1/2 桶（即 3% 灰度）

# ===== 限流与配额 / Rate limit and quota（Phase 0 留位，Phase 4 实现）=====

[limits]
# 每用户每秒请求数上限（0 表示不限）
# Per-uid requests-per-second cap (0 = unlimited)
rps_per_uid = 100
# 同时进行的 capability call 数上限
# Concurrent capability call cap
max_concurrent_calls = 1000
# 每用户每天消息配额（0 表示不限）
# Per-uid daily message quota (0 = unlimited)
msg_quota_per_day = 0

# ===== 资源预算 / Resource budget（Phase 1 监督树软限即引用）=====

[budget]
# 单插件最大持有 TCP 连接数（用于 100 万连接预算分配）
# Max TCP connections held by this plugin
max_connections = 0           # 0 = 无限制
# 单插件最大订阅 syn group 数
# Max syn group subscriptions
max_subscriptions = 100000
# 内存软限（MB），超过将触发 metric 告警，不强制 kill
# Memory soft limit (MB); over-limit triggers metric alert, no kill
mem_mb_soft_limit = 512

# ===== 降级与熔断 / Degrade and circuit breaker（Phase 4 实现）=====

[degrade]
# 当 health=unhealthy 时的处置：isolate（路由 404）| route_404 | pass_through
# Behavior on health=unhealthy: isolate / route_404 / pass_through
on_unhealthy = "route_404"

[circuit_breaker]
# capability call 是否启用熔断器
# Enable circuit breaker on capability calls
enabled = true
# 触发阈值：60 秒内 50% 请求失败
# Trigger threshold: 50% failure rate within 60s
threshold_pct = 50
window_sec = 60

# ===== 客户端入口 / Client entries =====

# Flutter 客户端入口（与 imboyapp 中 PluginRegistry 对应）
# Flutter client entries (matches PluginRegistry in imboyapp)
[entries]
app = ["channel_tab", "channel_discover_page"]
admin = ["channels_page"]

# i18n key 占位（Phase 5 兑现）
# i18n key placeholders (delivered in Phase 5)
[i18n]
keys = ["channel.tab.title", "channel.discover.empty"]

# ===== 通信契约 / Communication contract =====

# 本插件订阅的事件主题（imboy_plugin_bus）
# Event topics this plugin subscribes to
subscribes = [
    "user.deleted",
    "group.dismissed",
]

# 本插件发布的事件主题
# Event topics this plugin publishes
publishes = [
    "channel.created",
    "channel.subscribed",
    "channel.unsubscribed",
]

# 主题元数据（Phase 4 兑现）/ Topic metadata (delivered in Phase 4)
# 当事件主题需要承载客户端解密后的 E2EE 明文时，必须在此显式声明
# When a topic carries client-decrypted E2EE plaintext, e2ee_passthrough MUST be declared
[publishes_meta]
# 例 / Example:
# "channel.body_decrypted" = { e2ee_passthrough = true, requires_user_consent = true }

# ===== 审计 / Audit log（Phase 4 实现）=====

[audit]
# 写入 plugin_audit_log 表的事件名单
# Events written to plugin_audit_log
events = ["channel.created", "channel.deleted", "channel.member_kicked"]

# ===== 元信息 / Metadata =====

[meta]
author = "imboy-pub"
license = "Apache-2.0"
homepage = "https://gitee.com/imboy-pub/imboy"
created_at = "2026-03-15"
```

### 3.3 字段验证规则 / Field validation rules

| 字段 / Field | 类型 / Type | 约束 / Constraint |
|-------------|------------|-------------------|
| `name` | string → atom | `^[a-z][a-z0-9_]{2,31}$`；loader 启动期 `binary_to_existing_atom/2`，未注册原子拒绝加载 |
| `version` | string | semver 2.0 严格匹配 / strict semver 2.0 |
| `contract_version` | string | 形如 `"<major>.<minor>"`；解析为 `{Major, Minor}`；兼容性见 §9.1 |
| `kind` | enum | `"plugin"` \| `"aggregate_plugin"` |
| `min_core_version` | string | semver 范围或固定版本 / semver range or fixed |
| `routes[].path` | string | **强约束**：必须以 `/v{n}/<name>/` 开头（n=1\|2\|...）；违反则 loader 拒绝并记录 `invalid_route_namespace` |
| `migrations.table_prefix` | string | 必须等于 `name + "_"` / MUST equal `name + "_"` |
| `migrations.preserve_on_uninstall` | boolean | 默认 `true` |
| `features.<key>.rollout` | enum | `"always"` \| `"percentage"` \| `"canary"` |
| `features.<key>.audience.kind` | enum | `"all"` \| `"tenant"` \| `"uid_hash"` |
| `features.<key>.audience.buckets` | int[] | `uid_hash` 时必填；长度 ≤ 100；元素去重；元素 `0..99` |
| `limits.rps_per_uid` | non_neg_integer | `0` 表示不限 |
| `limits.max_concurrent_calls` | non_neg_integer | — |
| `budget.max_connections` | non_neg_integer | — |
| `budget.mem_mb_soft_limit` | pos_integer | — |
| `degrade.on_unhealthy` | enum | `"isolate"` \| `"route_404"` \| `"pass_through"` |
| `circuit_breaker.threshold_pct` | int 1..99 | — |
| `subscribes / publishes` | string[] | 主题命名 `<plugin>.<event>`；`<plugin>` 必须是已声明的 plugin name |
| `publishes_meta.<topic>.e2ee_passthrough` | boolean | 仅当主题承载客户端解密后明文时为 `true`；声明 `true` 时必须同时声明 `requires_user_consent = true` |
| `publishes_meta.<topic>.requires_user_consent` | boolean | 用户必须 opt-in 一次方可订阅此主题 |
| `audit.events` | string[] | 必须是 `publishes` 子集 |

> **任一字段违反约束 → loader 拒绝加载该插件并记录结构化错误，不影响其他插件。**
> Any violation → loader rejects the plugin with structured error log; other plugins unaffected.

---

## 4. Behaviour 契约 / Behaviour Contract

### 4.1 核心 callbacks / Core callbacks

> **关键澄清 / Key clarification**：`imboy_plugin` 是 **application 级 behaviour**，不是 `gen_server`。插件主模块（如 `imboy_plugin_channel`）只实现下列纯函数 callbacks。插件**内部**的 worker 进程（gen_server / gen_statem / etc.）由 `<name>_sup` 直接监督，遵循 OTP 标准独立实现 — 它们与 `imboy_plugin` behaviour **无关**。
>
> `imboy_plugin` is an **application-level behaviour**, NOT a `gen_server`. The plugin's main module (e.g. `imboy_plugin_channel`) implements only the pure-function callbacks below. Plugin-**internal** worker processes are supervised by `<name>_sup` directly per standard OTP — they are **unrelated** to the `imboy_plugin` behaviour.

```erlang
%% src/lib/imboy_plugin.erl
%% Application-level behaviour (NOT gen_server).

-module(imboy_plugin).

%% 插件清单（运行时调用，必须 < 10ms 返回）
%% 实现要求：从 persistent_term 读取 loader 在 install 阶段已解析好的 manifest，禁止运行时再读 toml
%% Plugin manifest (called at runtime, MUST return < 10ms)
%% Implementation requirement: read pre-parsed manifest from persistent_term;
%% MUST NOT re-parse toml at runtime
-callback manifest() -> imboy_plugin:manifest().

%% 启动钩子（插件 supervisor 启动后调用，进行业务级初始化）
%% 不得阻塞超过 5 秒；I/O 操作应放到独立 worker
%% Start hook (called after plugin supervisor starts; for business-level init)
%% MUST NOT block > 5s; I/O should be deferred to separate workers
-callback start(StartArgs :: map()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

%% 停止钩子（插件 disable 或 uninstall 时调用，应清理 ETS / 注销 syn / 关闭外部连接）
%% Stop hook (called on disable or uninstall; cleanup ETS / unregister syn / close external connections)
-callback stop(State :: term()) -> ok.

%% 数据库迁移钩子（install 与 upgrade 时调用 up；uninstall 且 preserve_on_uninstall=false 时调用 down）
%% DB migration hook (called on install/upgrade with up; on uninstall with down if preserve_on_uninstall=false)
-callback migrate(Direction :: up | down, FromVersion :: binary(), ToVersion :: binary()) ->
    ok |
    {error, Reason :: term()}.

%% 路由列表（启用插件时调用，结果写入 imboy_router_registry）
%% 实现要求：直接 maps:get(routes, manifest())，不重复 I/O
%% Routes list (called on enable; result written to imboy_router_registry)
%% Implementation: just maps:get(routes, manifest()); no extra I/O
-callback routes() -> [imboy_plugin:route_spec()].

%% 能力声明（声明本插件向外提供的 capability，供其他插件 requires_capabilities 引用）
%% Capability declaration (capabilities exposed to other plugins via requires_capabilities)
-callback capabilities() -> [imboy_plugin:capability()].

%% 健康检查（admin 面板调用，loader 强制 100ms timer kill；不可阻塞）
%% Health check (called by admin panel; loader enforces 100ms hard timeout via timer kill)
-callback health() -> imboy_plugin:health_report().
```

### 4.2 类型规范 / Type specifications

```erlang
%% include/imboy_plugin.hrl

-type plugin_name() :: atom().
-type plugin_version() :: binary().     %% e.g., <<"1.0.0">>
-type contract_version() :: {Major :: pos_integer(), Minor :: non_neg_integer()}.

-type manifest() :: #{
    name := plugin_name(),
    version := plugin_version(),
    contract_version := contract_version(),
    kind := plugin | aggregate_plugin,
    description := binary(),
    depends_on := #{plugin_name() => binary()},      %% semver constraint
    requires_capabilities := [binary()],
    min_core_version := binary(),
    children := [plugin_name()],                     %% only for aggregate
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

-type route_spec() :: #{
    method := binary(),                              %% <<"GET">> | <<"POST">> | ...
    path := binary(),                                %% cowboy 格式 / cowboy format
    handler := atom(),
    action := atom(),
    required_feature => atom() | undefined
}.

-type migration_spec() :: #{
    dir := binary(),
    table_prefix := binary(),
    preserve_on_uninstall := boolean(),
    data_export_hook => mfa() | undefined            %% Phase 4
}.

-type feature_spec() :: #{
    default := boolean(),
    description := binary(),
    rollout := always | percentage | canary,
    percentage => 0..100,
    audience := audience_spec()
}.

-type audience_spec() ::
    #{kind := all} |
    #{kind := tenant, tenant_ids := [binary()]} |
    #{kind := uid_hash, buckets := [0..99]}.

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

-type degrade_spec() :: #{
    on_unhealthy := isolate | route_404 | pass_through
}.

-type circuit_breaker_spec() :: #{
    enabled := boolean(),
    threshold_pct := 1..99,
    window_sec := pos_integer()
}.

-type topic_meta() :: #{
    e2ee_passthrough => boolean(),                   %% 是否承载客户端解密明文 / carries client-decrypted plaintext
    requires_user_consent => boolean()               %% e2ee_passthrough=true 时必须为 true / mandatory when e2ee_passthrough=true
}.

-type capability() :: #{
    name := binary(),                                %% e.g., <<"channel.subscribe@1">>
    version := pos_integer(),
    api_module := atom(),                            %% 该 capability 的 API 模块
    default_timeout_ms := pos_integer(),             %% 同步调用强制超时 / mandatory sync timeout
    idempotent := boolean()                          %% 调用方据此决定是否重试 / retry decision hint
}.

-type health_report() :: #{
    status := healthy | degraded | unhealthy,
    reason => binary(),                              %% degraded/unhealthy 时必填
    metrics => #{                                    %% 机读指标，Phase 6 接 Prometheus
        rps => float(),
        queue_depth => non_neg_integer(),
        error_rate => float(),
        atom() => term()
    }
}.
```

### 4.3 callback 实现示例 / Implementation example

```erlang
%% apps/imboy_plugin_channel/src/imboy_plugin_channel.erl

-module(imboy_plugin_channel).
-behaviour(imboy_plugin).

-export([manifest/0, start/1, stop/1, migrate/3, routes/0, capabilities/0, health/0]).

%% manifest 由 loader 在 install 阶段一次性解析并写入 persistent_term
%% Manifest is pre-parsed by loader during install and stored in persistent_term
manifest() ->
    persistent_term:get({imboy_plugin_manifest, channel}).

start(_StartArgs) ->
    %% 注册 syn group、初始化 ETS 等（不得阻塞 > 5s）
    %% Register syn groups, initialize ETS, etc. (MUST NOT block > 5s)
    ok = channel_subscriber_registry:init(),
    {ok, #{started_at => erlang:system_time(millisecond)}}.

stop(_State) ->
    %% 清理 ETS、关闭外部连接
    %% Cleanup ETS, close external connections
    catch ets:delete(channel_subscriber_registry),
    ok.

migrate(up, _From, _To) ->
    imboy_migrate:run_plugin(channel, up);
migrate(down, _From, _To) ->
    imboy_migrate:run_plugin(channel, down).

routes() ->
    maps:get(routes, manifest(), []).

capabilities() ->
    [#{
        name => <<"channel.subscribe@1">>,
        version => 1,
        api_module => channel_subscribe_api,
        default_timeout_ms => 500,
        idempotent => false
    }].

health() ->
    case channel_subscriber_registry:size() of
        N when is_integer(N), N >= 0 ->
            #{status => healthy,
              metrics => #{queue_depth => N}};
        _ ->
            #{status => unhealthy,
              reason => <<"registry unavailable">>}
    end.
```

---

## 5. 命名约定 / Naming Conventions

| 资源 / Resource | 约定 / Convention | 示例 / Example |
|----------------|-------------------|----------------|
| OTP application 名 | `imboy_plugin_<name>` | `imboy_plugin_channel` |
| 主模块（实现 behaviour） | `imboy_plugin_<name>` | `imboy_plugin_channel` |
| Supervisor 模块 | `<name>_sup` | `channel_sup` |
| Logic 模块 | `<name>_logic`、`<name>_<subdomain>_logic` | `channel_logic`、`channel_logic_invitation` |
| DS 模块 | `<name>_<subdomain>_ds` | `channel_subscriber_ds` |
| Repo 模块 | `<name>_<subdomain>_repo` | `channel_repo` |
| Handler 模块 | `<name>_handler`、`<name>_<subdomain>_handler` | `channel_handler` |
| 数据库表 | `<name>_<entity>` | `channel`、`channel_subscriber`、`channel_invitation` |
| 数据库索引 | `idx_<name>_<entity>_<column>` | `idx_channel_subscriber_uid` |
| ETS 表名 | `<name>_<purpose>` | `channel_subscriber_registry` |
| syn group | 与现有 `imboy_syn` 约定对齐（见下注） | `{channel, subscribers, ChannelId}` |
| Capability ID | `<name>.<feature>@<major>` | `channel.subscribe@1` |
| 事件主题 | `<name>.<event>` | `channel.subscribed` |
| 日志 facility（Phase 6） | `<name>` | `lager:info(channel, ...)` |
| Prometheus 标签（Phase 6） | `plugin="<name>"` | `imboy_messages_total{plugin="channel"}` |

> **syn group 命名注 / Note on syn group naming**：现有 `src/lib/imboy_syn.erl` 使用 tuple 形式的 group key（如 `{user, online, Uid}`）。新插件的 syn group 应延续此风格 `{<plugin_name>, <entity>, <id>}`，**不**使用 binary `:` 分隔。Phase 1 拆分监督树时由 `architect` 复审，必要时撰写 ADR。
>
> Existing `imboy_syn` uses tuple-form group keys (e.g. `{user, online, Uid}`). New plugin syn groups should follow this convention `{<plugin_name>, <entity>, <id>}`, **not** binary `:` separators. Reviewed in Phase 1 supervisor split.

> **强约束 / Hard constraint**：违反命名约定的插件，`imboy_plugin_loader` 拒绝加载并记录错误。
> Plugins violating naming conventions are rejected by `imboy_plugin_loader` with an error log.

---

## 6. 通信规则 / Communication Rules

### 6.1 禁止事项 / Prohibitions

**中文**：
- ❌ **禁止**插件直接 `module:fun()` 调用其他插件的内部模块
- ❌ **禁止**插件直接读写其他插件的数据库表
- ❌ **禁止**插件直接读写其他插件的 ETS 表
- ❌ **禁止**插件 `link` 其他插件的进程（用 `monitor` 替代）
- ❌ **禁止**插件自实现跨设备/跨节点严格消息顺序（必须通过 `imboy.msg_seq@1` capability 复用 core 的 `conv_seq`）
- ❌ **禁止**插件接触 `ciphertext` 字段或尝试在服务端解密 E2EE 消息（见 §6.7）

**English**:
- ❌ **Forbidden** for a plugin to directly call `module:fun()` of another plugin's internal modules
- ❌ **Forbidden** to read/write another plugin's database tables directly
- ❌ **Forbidden** to read/write another plugin's ETS tables directly
- ❌ **Forbidden** to `link` to another plugin's processes (use `monitor` instead)
- ❌ **Forbidden** to reinvent cross-device/cross-node strict message ordering (MUST use `imboy.msg_seq@1` capability backed by `conv_seq`)
- ❌ **Forbidden** to touch `ciphertext` fields or attempt server-side E2EE decryption (see §6.7)

### 6.2 允许的通信方式 / Allowed communication

| 方式 / Way | 用途 / Purpose | 同步性 / Sync |
|----------|---------------|--------------|
| **Capability API** | 插件 A 调用插件 B 声明的 `api_module` | 同步 sync |
| **imboy_plugin_bus**（pub/sub） | 异步事件通知，松耦合 | 异步 async |
| **syn process group** | 进程级订阅（如同会话内多设备同步） | 异步 async |
| **core 能力**（cache / tsid / ws_push / msg_seq） | 共享基础设施 | 同步 sync |

### 6.3 capability 调用语义 / Capability call semantics

```erlang
%% 1. 在 plugin.toml 中声明依赖 / Declare dependency in plugin.toml:
%%      depends_on = { channel = "^1.0" }
%%      requires_capabilities = ["channel.subscribe@1"]

%% 2. 通过 capability registry 调用（必带超时）
%%    Call via capability registry (timeout mandatory):
case imboy_plugin_capability:call(<<"channel.subscribe@1">>, subscribe, [Uid, ChannelId], #{timeout => 500}) of
    {ok, SubscriptionId} -> ...;
    {error, capability_not_available} -> ...;  %% 插件 B 未启用 / Plugin B disabled
    {error, version_mismatch} -> ...;
    {error, timeout} -> ...;                   %% 超时按 callee 的 degrade 策略处理
    {error, circuit_open} -> ...               %% 熔断器开启 / circuit breaker tripped
end.
```

**调用语义规则 / Call semantics rules**：

| 规则 / Rule | 含义 / Meaning |
|------------|---------------|
| 同步路由 | 默认走 `apply/3`（纯函数）；`api_module` 中函数**必须**避免长时间阻塞 |
| 超时强制 | 调用方必须传 `timeout`；未传则取 `capability.default_timeout_ms`；超时由 `imboy_plugin_capability` 用 `erlang:send_after + monitor` 实现 |
| 熔断 | 若 callee 的 `circuit_breaker.enabled=true`，连续失败超阈值返回 `{error, circuit_open}` |
| 降级 | 若 callee 处于 `degraded`/`unhealthy`，按 callee 的 `degrade.on_unhealthy` 策略处理（isolate=拒绝调用；pass_through=照旧；route_404=返回 `not_found`） |
| 幂等提示 | `capability.idempotent=true` 表示调用方可在网络抖动后安全重试 |

### 6.4 事件总线示例 / Event bus example

```erlang
%% 发布 / Publish:
imboy_plugin_bus:publish(<<"channel.subscribed">>, #{
    channel_id => ChannelId,
    user_id => Uid,
    timestamp => erlang:system_time(millisecond)
}).

%% 订阅（在 start/1 中注册）/ Subscribe (register in start/1):
imboy_plugin_bus:subscribe(<<"user.deleted">>, fun handle_user_deleted/1).
```

### 6.5 通信反模式 / Communication anti-patterns

| 反模式 / Anti-pattern | 后果 / Consequence | 替代方案 / Alternative |
|----------------------|-------------------|----------------------|
| **同步调用链 > 2 跳** | 雪崩、超时放大 | 第 3 跳起改为 publish 异步事件 |
| **publish 回调内同步 publish 同主题** | 事件风暴、栈深爆炸 | 用 `erlang:send_after(0, self(), {republish, Topic})` 解耦 |
| **capability call 内副作用** | 调用方栈帧承担副作用，难以追溯 | 副作用必须走异步事件或 syn group cast |
| **未声明的 publish** | 订阅方静默失效 | manifest `publishes` 必须声明所有发布主题；loader 校验 |
| **跨插件直接 ETS 读** | Phase 5 客户端联动时表结构不可推导 | 通过 capability 暴露读取 API |

### 6.6 conv_seq 严格顺序约束 / conv_seq strict ordering constraint

**中文**：IMBoy 的消息时间线严格顺序由 core 的 `conv_seq`（per-conversation 单调递增）保证，**不**依赖 TSID（TSID 仅近似时间有序）。任何插件需要消息严格顺序时：

1. 必须在 `requires_capabilities` 中声明 `"imboy.msg_seq@1"`
2. 通过 `imboy_plugin_capability:call(<<"imboy.msg_seq@1">>, next, [ConvKey])` 获取下一序列号
3. 禁止用 TSID、`erlang:system_time/1`、自维护 counter 替代

**English**: IMBoy's message timeline strict ordering is guaranteed by core's `conv_seq` (per-conversation monotonic), **not** TSID (TSID is only approximately time-ordered). Plugins requiring strict order MUST declare `"imboy.msg_seq@1"` in `requires_capabilities` and call core, never reinvent.

### 6.7 E2EE 边界 / E2EE boundary

**中文**：
- 插件代码运行在服务端，**永远不会**持有用户私钥，因此**不可能**正确解密 E2EE 消息
- 插件**禁止**读取消息的 `ciphertext` 字段
- 插件**禁止**尝试在服务端"解密"消息内容
- 如插件需要消息正文（如内容审核、AI 摘要），必须由**客户端**解密后通过 `imboy_plugin_bus` publish 明文事件，且事件主题必须显式声明 `e2ee_passthrough = true`，由用户授权一次

**English**:
- Plugins run server-side and **never** hold user private keys; thus **cannot** correctly decrypt E2EE messages
- Plugins **MUST NOT** read message `ciphertext` fields
- Plugins **MUST NOT** attempt server-side "decryption"
- If plugin needs message body (e.g. moderation, AI summary), the **client** decrypts and publishes plaintext via `imboy_plugin_bus`; topic MUST declare `e2ee_passthrough = true` and require user opt-in

---

## 7. 生命周期状态 / Lifecycle States

> 详细状态机由 Phase 4 (`imboy_plugin_lifecycle` gen_statem) 实现。本节仅声明状态枚举与回滚策略字段，供 manifest / behaviour 提前对齐。
> Detailed state machine is implemented in Phase 4. This section only declares the state enum and rollback fields for early alignment.

```
unknown ──install──→ installing ──ok──→ installed ──enable──→ enabling ──ok──→ enabled
                          │                                        │
                          fail                                     fail
                          │                                        │
                          ↓                                        ↓
                       failed                                   failed

enabled ──disable──→ disabling ──ok──→ disabled
enabled ──upgrade──→ upgrading ──ok──→ enabled (new version)
                                  ─fail──→ enabled (rolled back per rollback_strategy)
disabled ──uninstall──→ uninstalling ──ok──→ unknown
```

### 各状态下 callback 调用 / Callbacks per state

| 状态转移 / Transition | 调用 callback / Callback called |
|----------------------|-------------------------------|
| install | `manifest/0`, `migrate(up, undefined, V)` |
| enable | `start/1`, `routes/0`, `capabilities/0` |
| disable | `stop/State` |
| upgrade(V→V') | `stop/1`, `migrate(up, V, V')`, `start/1`, `routes/0` |
| uninstall (preserve=false) | `migrate(down, V, undefined)` |
| 任意时刻 / any time | `health/0` |

### 回滚策略 / Rollback strategy

```toml
[lifecycle]
# 回滚原子性 / Rollback atomicity
# - "atomic": migrate + start + routes 必须全部成功，否则全部回滚（事务语义）
# - "best_effort": 尽力回滚，失败仅记录告警（可能残留中间状态）
# - "manual": 不自动回滚，进入 failed 状态等待运维介入
rollback_strategy = "manual"   # Phase 0 默认；Phase 4 实现 atomic / best_effort
```

| 字段 / Field | 类型 / Type | Phase 0 状态 / Status |
|-------------|------------|---------------------|
| `lifecycle.rollback_strategy` | enum: atomic\|best_effort\|manual | 仅记录字段，Phase 4 兑现语义 |

---

## 8. 错误处理边界 / Error Handling Boundary

### 8.1 Let It Crash 适用 / Let It Crash applies

**中文**：插件内部业务逻辑遵循 OTP 标准做法 — 不写防御式 try/catch，由 supervisor 重启。具体边界：
- 插件 worker 崩溃 → 插件 sup 重启（`intensity=5/period=10`）
- 插件 sup 自身崩溃 → `imboy_plugin_sup` 标记插件为 `failed`，**不**自动重启 core
- core 不感知插件崩溃细节，仅通过 `imboy_plugin_lifecycle` 状态机处理

**English**: Plugin business logic follows standard OTP — no defensive try/catch; rely on supervisor restart. Boundaries:
- Plugin worker crash → plugin sup restarts (`intensity=5/period=10`)
- Plugin sup itself crashes → `imboy_plugin_sup` marks the plugin as `failed`, does **not** auto-restart core
- Core is unaware of plugin crash internals; handled solely via `imboy_plugin_lifecycle` state machine

### 8.2 必须捕获的边界 / Required catches

仅在以下边界**必须** try/catch（防止崩溃越界）/ try/catch is **required** only at these boundaries (to contain blast radius):

| 边界 / Boundary | 实现位置 / Where | 原因 / Reason |
|----------------|-----------------|---------------|
| `imboy_plugin:manifest/0` 调用 | `imboy_plugin_loader` 内 | manifest 解析错误不能影响其他插件 |
| `imboy_plugin:start/1` 调用 | `imboy_plugin_lifecycle` 内 | start 失败应进入 `failed` 状态 |
| `imboy_plugin:health/0` 调用 | `imboy_plugin_loader` 内（用 `erlang:send_after + monitor` 强制 100ms timeout，超时 kill 调用进程并返回 `{unhealthy, timeout}`） | health 必须 100ms 内返回；**插件自身不写 try/catch** |
| `imboy_plugin_bus:publish` 的 subscriber 回调 | `imboy_plugin_bus` 内 | 一个订阅者崩溃不影响其他订阅者 |

> **注意 / Note**：插件作者**不应**在自己的 callback 实现里写 try/catch — 上述捕获由 core 的中间层（loader / lifecycle / bus）统一负责。
>
> Plugin authors **should NOT** write try/catch in their callback implementations — the catches above are handled by core middleware (loader / lifecycle / bus).

---

## 9. 兼容性 / Compatibility

### 9.1 契约版本与 Loader 兼容矩阵 / Contract version & loader compatibility matrix

```erlang
%% Loader 加载插件时的兼容性判定（伪代码 / pseudocode）:

is_compatible(PluginContractVer, CoreContractVer) ->
    {PMajor, PMinor} = parse_contract_version(PluginContractVer),
    {CMajor, CMinor} = parse_contract_version(CoreContractVer),
    case {PMajor, PMinor} of
        {CMajor, M} when M =< CMinor ->
            ok;                            %% 同 major + plugin minor ≤ core minor → 兼容
        {CMajor, M} when M > CMinor ->
            {error, plugin_too_new};       %% plugin 用了 core 不认识的字段
        _ ->
            {error, major_version_mismatch}
    end.
```

| 场景 / Scenario | core | plugin | 结果 / Result |
|----------------|------|--------|--------------|
| 兼容 / Compatible | 1.0 | 1.0 | ✅ load |
| 兼容 / Compatible | 1.2 | 1.0 | ✅ load（plugin 旧但核心向后兼容） |
| 拒绝 / Rejected | 1.0 | 1.1 | ❌ `plugin_too_new` |
| 拒绝 / Rejected | 1.x | 2.0 | ❌ `major_version_mismatch` |
| 拒绝 / Rejected | 2.0 | 1.x | ❌ `major_version_mismatch` |

### 9.2 升级语义 / Versioning policy

- 增加新可选字段、扩枚举值 → minor bump（如 1.1）
- 删除字段、改类型、改语义、改约束 → major bump（如 2.0），并撰写 ADR
- 所有 1.x 版本 plugin 都能在 1.y (y ≥ x) 的 core 上运行

### 9.3 OTP / Erlang 版本

- 最低 OTP 28（与 IMBoy core 一致）
- 使用 `atomics`、`persistent_term`、`maps` 模块的现代 API

### 9.4 破坏性变更政策 / Breaking change policy

破坏性变更必须满足 / Breaking changes MUST:
1. 撰写 ADR（`doc/architecture/adr/YYYY-MM-DD-<topic>.md`）
2. major bump `contract_version`
3. 提供至少一个 minor 版本的迁移期，旧契约继续受支持

---

## 10. Phase 0 范围之外 / Out of Scope for Phase 0

以下项**不在** Phase 0 交付，由后续 Phase 实现 / The following are **NOT** delivered in Phase 0:

| 项 / Item | 交付 Phase / Delivered in |
|-----------|---------------------------|
| 独立 OTP application 形态（apps/* 多 app 重构） | Phase 1 |
| 隔离监督树（`<name>_sup`） | Phase 1 |
| budget 软限实际生效（max_connections / mem 软限告警） | Phase 1 |
| 动态路由热更（cowboy dispatch 重编） | Phase 2 |
| DB 迁移命名空间执行器（`imboy_migrate:run_plugin/2`） | Phase 3 |
| 生命周期状态机 `imboy_plugin_lifecycle` gen_statem | Phase 4 |
| 依赖解析 + semver 约束求解（扩展到 `~>` `>=` `<` `\|\|`） | Phase 4 |
| `imboy_plugin_bus` pub/sub 实现 | Phase 4 |
| `imboy_plugin_capability:call/4` 同步路由 + 超时 + 熔断 | Phase 4 |
| 限流 / 配额执行（`limits` 字段语义兑现） | Phase 4 |
| 灰度 / canary（`features.rollout/audience` 语义兑现） | Phase 4 |
| 降级（`degrade.on_unhealthy` 语义兑现） | Phase 4 |
| `migrations.data_export_hook` GDPR 导出 | Phase 4 |
| Admin REST API（install/enable/disable） | Phase 4 |
| 客户端 manifest 端点 + Flutter/Vue 联动 | Phase 5 |
| `i18n.keys` 实际接入翻译系统 | Phase 5 |
| Ed25519 签名验证 | Phase 6 |
| Prometheus per-plugin label / lager facility | Phase 6 |

### Phase 0 安全约束 / Phase 0 security constraints

**atom 表保护 / Atom table protection**：

- `plugin.config` 由 `file:consult/1` 解析，所有 atom 字面量会被自动注册到全局 atom 表
- atom 表上限 1,048,576，**不可回收**
- Phase 0 实施约束：
  - `priv/plugins/<name>/plugin.config` **仅** imboy-pub 维护者写入（不接受第三方提交）
  - 文件中 atom 字面量必须为已声明的合法值（name/kind/feature_keys/entry IDs/handler 名等）
  - **禁止**包含动态生成的 atom（如随机字符串作 atom）
- Phase 1+ 引入第三方插件市场前必须强化：
  - 切换到 JSON/TOML 格式（避免 Erlang term eval 语义）
  - 解析时仅允许 `binary_to_existing_atom/2`（拒绝未注册 atom）
  - 上传插件时静态扫描 atom 字面量集合

> **HIGH-1 风险已识别但部分缓解 / HIGH-1 identified, partially mitigated**：当前依赖代码审查与受控写入边界，技术防护待 Phase 1+ 落地。

**路径白名单 / Path whitelist**：

- `imboy_plugin_loader:start_link/1` 接受任意路径，**不**做白名单校验
- 生产部署：仅由 `imboy_sup` 的 child spec 调用 `start_link/0`，路径固定为 `code:priv_dir(imboy) ++ "/plugins"`
- Phase 1+ Admin REST API（install）必须强制白名单

### Phase 0 实际交付 / What Phase 0 actually delivers

- ✅ 本契约文档（双语）
- ✅ `include/imboy_plugin.hrl`（types + records）
- ✅ `src/lib/imboy_plugin.erl`（application 级 behaviour 定义）
- ✅ `src/lib/imboy_plugin_toml.erl`（toml 解析与 schema 校验，含所有新增字段格式校验）
  - 必须导出 `parse_contract_version/1 :: binary() -> {pos_integer(), non_neg_integer()} | {error, invalid_format}`
- ✅ `src/lib/imboy_plugin_loader.erl`（**gen_server**；启动期一次性扫描 `priv/plugins/*/plugin.toml`，解析后写入 `persistent_term:put({imboy_plugin_manifest, Name}, M)`，注入旧 `imboy_plugin_registry`）
  - 必须以 child spec 形式加入 `imboy_sup`：`#{id => imboy_plugin_loader, start => {imboy_plugin_loader, start_link, []}, type => worker}`
- ✅ 现有 4 个 hardcoded 插件镜像为 toml（`channel`、`moment`、`location`、`group_collab`）
- ✅ 等价性测试（toml 加载结果与 `imboy_plugin_registry:raw_manifests/0` 等价）

---

## 11. 评审反馈处理记录 / Review feedback log

### 2026-04-29 · architect agent 评审（NEEDS WORK → 修订后 PASS 候选）

| ID | 严重度 | 章节 | 问题 | 处理 |
|----|--------|------|------|------|
| C1 | CRITICAL | §4.1 | `imboy_plugin` 与 gen_server 身份混淆 | 已澄清：application 级 behaviour，与 gen_server 无关；roadmap §5 同步措辞 |
| C2 | CRITICAL | §4.1+§4.3 | manifest/0 < 10ms 但示例每次读盘 | 已改为 `persistent_term`；loader 在 install 阶段一次性解析 |
| C3 | CRITICAL | §3.2+§3.3 | contract_version 解析规则缺失 | 已加 §9.1 Loader 兼容矩阵 + 伪代码 |
| C4 | CRITICAL | §6.3 | capability call 路由进程未定义、无超时 | 已加 §6.3 调用语义规则表（同步 apply、强制 timeout、熔断、降级） |
| H1 | HIGH | §3.2 routes | path 缺 `<name>` 前缀强约束 | 已加 §3.3 强约束行 |
| H2 | HIGH | §7 | 缺 rollback_strategy 字段 | 已加 §7 回滚策略小节 |
| H3 | HIGH | §4.1 health/0 | 三态返回缺机读 metrics | 已改为 `health_report()` map（含 metrics） |
| H4 | HIGH | §3.2 features | 缺 rollout/audience（灰度） | 已扩展 features schema 与类型规范 |
| H5 | HIGH | §6 | 缺通信反模式声明 | 已加 §6.5 反模式表 |
| M1 | MEDIUM | §3.2 depends_on | semver 子集未声明 | 已注 Phase 0 仅 `^X.Y.Z` 与精确版本 |
| M2 | MEDIUM | §3.2 migrations | 缺 GDPR 导出钩子 | 已留 `data_export_hook` 字段（Phase 4 兑现） |
| M3 | MEDIUM | §4.2 | name 字符串→atom 转换未规范 | 已加 §3.3 验证规则行 |
| M4 | MEDIUM | §5 syn group | 与现有 `imboy_syn` 约定未对齐 | 已加 §5 注，沿用 tuple 风格 |
| M5 | MEDIUM | §10 | loader 是 worker 还是模块函数未声明 | 已声明 gen_server，须挂入 `imboy_sup` |
| L1 | LOW | §3.2 | 注释中英不一致 | 已统一双语注释 |
| L2 | LOW | §8.2 | health 超时机制 | 已澄清：loader 强制 timer kill，插件不写 try/catch |
| L3 | LOW | §0 | 缺术语表 | 已加 §0 |
| 缺口-1 | 工业级 | §3.2 | 限流 / 配额 | 已加 `[limits]` 段（Phase 4 兑现） |
| 缺口-2 | 工业级 | §3.2 | 资源预算 | 已加 `[budget]` 段（Phase 1 软限） |
| 缺口-3 | 工业级 | §3.2 | 灰度 / 多租户 | 见 H4 |
| 缺口-4 | 工业级 | §3.2 | 降级 / 熔断 | 已加 `[degrade]` `[circuit_breaker]` 段 |
| 缺口-5 | 工业级 | §6 | conv_seq 严格顺序 | 已加 §6.6 + `imboy.msg_seq@1` 必依赖 |
| 缺口-6 | 工业级 | §6 | E2EE 边界 | 已加 §6.7 |
| 缺口-7 | 工业级 | §3.2 | 审计日志 | 已加 `[audit]` 段（Phase 4 兑现） |
| 缺口-8 | 工业级 | §3.2 | i18n keys | 已加 `[i18n]` 段（Phase 5 兑现） |

> **状态 / Status**：评审反馈 18 项已全部落地（CRITICAL 4/4、HIGH 5/5、MEDIUM 5/5、LOW 3/3、工业级缺口 8/8），合计 25 项。

### 2026-04-29 · architect agent 第 2 轮评审（PASS）

第 2 轮终判 **PASS（带 3 项非阻塞微补丁）**。25 项反馈全部核查通过，无矛盾，文档自洽。

发现并已修补的 3 项 P0-D5 微补丁 / 3 P0-D5 patches discovered and applied:

| ID | 章节 | 问题 | 修补 |
|----|------|------|------|
| P5-1 | §3.2 / §3.3 / §4.2 | E2EE 边界 §6.7 要求声明 `e2ee_passthrough` 但 schema 无此字段 | 新增 `[publishes_meta]` 段；types 加 `topic_meta()`；§3.3 加校验规则 |
| P5-2 | §10 | `parse_contract_version/1` 函数归属未明 | §10 明确导出于 `imboy_plugin_toml` 并给出 spec |
| P5-3 | §3.3 features.audience.buckets | 缺长度上限与去重约束 | §3.3 补"长度 ≤ 100；元素去重" |

**P0-T1 开工判断**：可立即开工。契约已具备工程实施基础，与 imboy_syn 实证对齐，与 roadmap §5/§7 自洽。

---

## 12. 变更记录 / Changelog

| 日期 / Date | 变更 / Change | contract_version | 作者 / Author |
|------------|---------------|-------------------|---------------|
| 2026-04-29 | 文档创建（Phase 0 立项版） | 1.0 | leeyi + Claude |
| 2026-04-29 | architect 评审反馈整合（25 项落地） | 1.0（未发布） | leeyi + Claude |
| 2026-04-29 | architect 第 2 轮评审 PASS + P0-D5 微补丁（publishes_meta / parse_contract_version / buckets 约束） | 1.0（未发布） | leeyi + Claude + architect |
| 2026-04-29 | §3.1 加 Phase 0 文件格式决策：plugin.config (Erlang terms) 优先，plugin.toml 推迟 Phase 1+ | 1.0（未发布） | leeyi + Claude |
