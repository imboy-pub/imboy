# 插件生命周期状态机 / Plugin Lifecycle State Machine

> ⚠️ **架构定位声明（2026-07 补）/ Architecture Status**
>
> 本文档描述的是**动态插件平台的远期设计**，不是当前交付形态。
> 生产环境实际运行的是 **模块化功能开关**：`imboy_plugin_registry` 的静态清单
> + policy 特性旗标（`required_feature/3`），由 `priv/plugins/*/plugin.config` 声明，
> 启动期一次性加载，**不支持运行时热加载**。
>
> 本文所述 `imboy_plugin_lifecycle` / `imboy_plugin_dependency` / `imboy_plugin_signature`
> 均标注 `@status FROZEN`，为 roadmap-only。**不得据此对外宣称"插件热加载生态"。**
>
> This document describes the **long-term dynamic plugin platform design**, not the
> current delivery. Production runs modular feature toggles only — static manifests
> loaded once at boot, no runtime hot-loading.

> **Last Updated**: 2026-04-29（Phase 4-T1 实施前置设计）
> **Status**: Phase 4 实施前置设计文档（lifecycle_version=1.0）
> **Scope**: 定义 `imboy_plugin_lifecycle` gen_statem 的状态枚举、事件、转换、回滚、超时、依赖联动、Admin REST API 与审计日志
> **Source of truth**: 本文档 + `docs/reference/plugin/contract.md` §7（简化状态枚举）
> **Related modules**: `imboy_plugin`, `imboy_plugin_loader`, `imboy_plugin_dependency`, `imboy_plugin_migrate`, `imboy_router_registry`, `imboy_plugin_sup`, `imboy_plugin_signature`
> **简体中文为权威版本，本文采用 Pattern A 同节并排双语 / Chinese is authoritative; bilingual sections side-by-side**

---

## 0. 术语表 / Glossary

| 术语 / Term | 定义 / Definition |
|------------|-------------------|
| **state（状态）** | gen_statem 当前所处的离散状态，如 `installed`、`enabled`、`failed` |
| **event（事件）** | 触发状态转换的输入，包括外部事件（`{install, Path}`）与内部事件（`step_done`、`step_failed`、`timeout`） |
| **transition（转换）** | 状态机从一个 state 在某个 event 触发下变迁到另一个 state |
| **step（步骤）** | 单个状态内顺序执行的子操作，例如 install 状态包含验签 / 解析 / 依赖校验 / 迁移等 step |
| **rollback（回滚）** | 当某个 step 失败时，按照声明的策略撤销已完成 step 的副作用 |
| **callback** | 插件主模块实现的 7 个 `imboy_plugin` behaviour 函数 |
| **lifecycle action** | 由 admin 发起的语义级操作：`install / enable / disable / upgrade / uninstall` |
| **cascade（联动）** | enable/disable 时按依赖关系自动牵连相关插件的处理 |
| **state data** | gen_statem 的 Data 字段，承载 plugin manifest、from_state、step 进度、deadline 等运行时上下文 |
| **deadline** | 状态最大停留时间，超过则触发 `timeout` 内部事件，落入 `failed` |

---

## 1. 概述 / Overview

### 中文

`imboy_plugin_lifecycle` 是基于 OTP `gen_statem`（`callback_mode = handle_event_function`）实现的**单插件**生命周期状态机。每个插件在运行时拥有**独立的 statem 进程**（local 注册名 `{plugin_lifecycle, Name}` 或通过 syn 全局注册），负责管理该插件的 install / enable / disable / upgrade / uninstall 全过程。

状态机不直接执行业务逻辑，而是**编排已有组件**：
- `imboy_plugin_signature:verify_file/2` 完成签名验证
- `imboy_plugin_toml:load/1` 解析 manifest
- `imboy_plugin_dependency:validate_constraints/1` + `topological_sort/1` 完成依赖检查
- `imboy_plugin_migrate:run/2`（Phase 3.2 待实施）执行 SQL 迁移
- `imboy_plugin_loader:scan/0` 触发 manifest 重扫并写入 persistent_term
- `imboy_plugin_sup` 挂载 / 卸载插件 worker（当真实 worker 出现）
- `imboy_router_registry:register/2` / `unregister/1` 路由热更
- 插件主模块的 `start/1`、`stop/1`、`migrate/3` 等 callback

### English

`imboy_plugin_lifecycle` is an OTP `gen_statem` (callback_mode = `handle_event_function`) implementing a **per-plugin** lifecycle state machine. Each plugin owns an **independent statem process** at runtime, responsible for the install / enable / disable / upgrade / uninstall flow.

The state machine does not execute business logic itself; it **orchestrates existing components**:
- `imboy_plugin_signature:verify_file/2` for signature verification
- `imboy_plugin_toml:load/1` for manifest parsing
- `imboy_plugin_dependency:validate_constraints/1` + `topological_sort/1` for dependency checks
- `imboy_plugin_migrate:run/2` (Phase 3.2 pending) for SQL migration
- `imboy_plugin_loader:scan/0` for manifest persistent_term refresh
- `imboy_plugin_sup` for plugin supervisor mount / unmount (when real workers appear)
- `imboy_router_registry:register/2` / `unregister/1` for hot route swap
- Plugin main module callbacks: `start/1`, `stop/1`, `migrate/3`

### 设计目标 / Design Goals

| 目标 / Goal | 落地策略 / Strategy |
|------------|--------------------|
| 可观测 / Observable | 每次状态转换写 `plugin_audit_log`；statem state + data 可通过 `sys:get_status` 查询（敏感字段 redact） |
| 可中断 / Interruptible | 每个状态有 deadline；admin 可发 `cancel` 事件强制进入 `failed` |
| 可回滚 / Rollback-capable | 三档策略 atomic / best_effort / manual，由 manifest `lifecycle.rollback_strategy` 声明 |
| 隔离 / Isolated | 单插件 statem 崩溃不影响其他插件；statem 由 `imboy_plugin_lifecycle_sup` 一对一监督 |
| 幂等 / Idempotent | install→install / enable→enable 等重复事件返回 `ok`，不重做副作用 |
| 与已有契约对齐 / Contract-aligned | 状态枚举与回滚字段与 `contract.md` §7 严格一致，仅做扩展不做破坏 |

---

## 2. 状态枚举 / States

完整 10 个状态。每个状态记录 / Each state records:
- 含义 / Meaning
- state data 关键字段 / Key state data
- 允许事件 / Allowed events
- 不变量 / Invariants

### 2.1 `unknown`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | 默认状态：插件**尚未被 lifecycle 接管**（未 install 或卸载完成） |
| state data | `#{name => Name}` |
| 允许事件 | `{install, Path}` |
| 不变量 | persistent_term 中**没有** `{imboy_plugin_manifest, Name}` 条目；`schema_migrations_<name>` 表不存在或空 |

### 2.2 `installing`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | 正在执行 install 流程 |
| state data | `#{name, path, manifest_pending, step, deadline, rollback_strategy}` |
| 允许事件 | 内部 `step_done`、`step_failed`、`timeout`；外部 `cancel` |
| 不变量 | persistent_term **尚未**写入；migrations 可能部分应用，由 rollback 处理 |

### 2.3 `installed`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | DB schema 已迁移、manifest 已写入 persistent_term，但路由 / worker **未挂载**，插件不接受流量 |
| state data | `#{name, manifest, version}` |
| 允许事件 | `enable`、`{upgrade, V}`、`{uninstall, Mode}`、`health_check` |
| 不变量 | `imboy_plugin_loader:get_manifest(Name)` 非 undefined；`imboy_router_registry:plugin_routes(Name) == []`；插件 sup（如 `<name>_sup`）无 children worker |

### 2.4 `enabling`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | 正在执行 enable 流程：依赖检查 → start/1 → 注册路由 → 注册 capabilities |
| state data | `#{name, manifest, step, deadline, undo_log}` |
| 允许事件 | 内部 `step_done`、`step_failed`、`timeout`；外部 `cancel` |
| 不变量 | 部分 step 可能已生效，回滚通过 `undo_log` 反向执行 |

### 2.5 `enabled`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | 完全启用：路由已注册、worker 已启动、capabilities 已注册、可接受流量 |
| state data | `#{name, manifest, started_at, plugin_state}` |
| 允许事件 | `disable`、`{upgrade, V}`、`health_check`、`reload_routes` |
| 不变量 | `imboy_router_registry:plugin_routes(Name)` 与 `manifest.routes` 一致；插件 sup pid 存活 |

### 2.6 `disabling`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | 正在执行 disable 流程：反注册路由 → stop/1 → 卸载 worker |
| state data | `#{name, manifest, step, deadline}` |
| 允许事件 | 内部 `step_done`、`step_failed`、`timeout`；外部 `cancel` |
| 不变量 | 路由可能已反注册但 worker 尚未停止，新请求 404，旧连接进入 graceful drain |

### 2.7 `disabled`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | 路由已反注册、worker 已停止；DB 数据保留；manifest 仍在 persistent_term（供 admin 查询） |
| state data | `#{name, manifest, disabled_at}` |
| 允许事件 | `enable`、`{uninstall, Mode}`、`{upgrade, V}`、`health_check` |
| 不变量 | `imboy_router_registry:plugin_routes(Name) == []`；插件 sup 无 children |

### 2.8 `upgrading`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | 正在执行 upgrade 流程：stop（如 enabled）→ migrate up V→V' → start → 注册新路由 |
| state data | `#{name, from_version, to_version, prev_manifest, new_manifest, step, deadline, was_enabled, undo_log}` |
| 允许事件 | 内部 `step_done`、`step_failed`、`timeout`；外部 `cancel` |
| 不变量 | `was_enabled` 决定升级完成后回到 `enabled` 还是 `installed`；失败按 rollback_strategy 处理 |

### 2.9 `uninstalling`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | 正在执行 uninstall 流程：（如有路由 / worker 残留，先 disable）→ migrate down（如 mode=drop_data）→ 清理 persistent_term |
| state data | `#{name, manifest, mode, step, deadline}` |
| 允许事件 | 内部 `step_done`、`step_failed`、`timeout`；外部 **不允许** `cancel`（避免半卸载状态） |
| 不变量 | uninstall 一旦开始就必须推进到 `unknown` 或 `failed`；不可中途回到 `disabled` |

### 2.10 `failed`

| 项 / Item | 内容 / Content |
|----------|---------------|
| 含义 / Meaning | 任意状态因 step_failed / timeout / 异常崩溃而落入此态；等待运维介入 |
| state data | `#{name, prev_state, error, error_detail, occurred_at, recoverable}` |
| 允许事件 | `{retry, prev_action}`（仅 recoverable=true）、`{force_uninstall, Mode}`、`reset` |
| 不变量 | 系统中可能残留半完成副作用（路由 / worker / DB schema），admin 必须显式选择 retry 或 force_uninstall |

---

## 3. 事件 / Events

### 3.1 外部事件 / External events（admin 触发）

| 事件 / Event | 参数 / Args | 语义 / Semantics |
|-------------|------------|-----------------|
| `{install, Path}` | `Path :: file:filename()` | 从 Path（如 `priv/plugins/<name>/`）安装新插件 |
| `enable` | — | 启用 `installed` 或 `disabled` 状态的插件 |
| `disable` | — | 禁用 `enabled` 状态的插件 |
| `{upgrade, NewVersion}` | `NewVersion :: binary()` | 升级到新版本（manifest path 通过约定推断或额外参数） |
| `{uninstall, Mode}` | `Mode :: preserve_data \| drop_data` | 卸载插件，可选保留 / 删除数据 |
| `health_check` | — | 调用插件 `health/0`，强制 100ms 超时 |
| `cancel` | — | 中断当前 *-ing 状态，强制进入 `failed` |
| `{retry, prev_action}` | `prev_action :: install \| enable \| disable \| upgrade \| uninstall` | 从 `failed` 状态重试上一次动作 |
| `{force_uninstall, Mode}` | `Mode :: preserve_data \| drop_data` | 从 `failed` 强制卸载，跳过常规校验 |
| `reset` | — | 从 `failed` 直接清空状态 → `unknown`（仅当 admin 已手动清理副作用） |

### 3.2 内部事件 / Internal events（statem 自身派发）

| 事件 / Event | 参数 / Args | 含义 / Meaning |
|-------------|------------|---------------|
| `{step_done, Step, Result}` | `Step :: atom(), Result :: term()` | 当前 step 完成，由异步 worker 通过 `gen_statem:cast` 回送 |
| `{step_failed, Step, Reason}` | `Step :: atom(), Reason :: term()` | 当前 step 失败 |
| `timeout` | — | gen_statem 内置 state_timeout 触发，落入 `failed` |
| `{health_result, Report}` | `Report :: imboy_plugin:health_report()` | health/0 调用结果回送 |
| `{health_timeout, _}` | — | health 调用 100ms 超时 |

### 3.3 事件来源 / Event sources

```
┌──────────────┐    {install,Path}        ┌────────────────────────┐
│  Admin REST  │ ────────────────────────►│ imboy_plugin_lifecycle │
│  (P4-T4)     │    enable / disable      │   (gen_statem,         │
│              │    {upgrade, V}          │    one per plugin)     │
│              │    {uninstall, Mode}     └────────┬───────────────┘
└──────────────┘                                   │
                                                   │ cast/call
                                                   ▼
                              ┌──────────────────────────────────┐
                              │ Step worker (transient process)  │
                              │ 执行单个 step，完成回送           │
                              │ {step_done|step_failed, ...}     │
                              └──────────────────────────────────┘
```

---

## 4. 状态转换图 / State Transition Diagram

```
                           ┌──────────────────────────────────────────────┐
                           │                                              │
                           ▼                                              │
                       ┌─────────┐                                        │
        ┌──────────────│ unknown │                                        │
        │              └─────────┘                                        │
        │ {install,Path}    ▲                                             │
        │                   │ ok (uninstall 完成 / done)                  │
        ▼                   │                                             │
   ┌────────────┐  ok   ┌────────────┐  enable    ┌──────────┐  ok   ┌─────────┐
   │ installing │──────►│ installed  │───────────►│ enabling │──────►│ enabled │
   └────────────┘       └────────────┘            └──────────┘       └─────────┘
        │ fail               │  ▲                       │ fail            │  │
        │                    │  │                       │                 │  │
        ▼                    │  │ disable               ▼                 │  │
   ┌─────────┐               │  │ ok        ┌──────────────┐   disable    │  │
   │ failed  │◄──────────────┴──┤           │   failed     │ ◄────────────┤  │
   └─────────┘                  │           └──────────────┘              │  │
        ▲                       │                  ▲                      │  │
        │                       │                  │ fail                 │  │
        │  fail                 │                  │                      ▼  │
        │              ┌────────────┐  ok    ┌──────────┐       ┌────────────┐
        ├──────────────│ disabling  │◄───────│  enabled │ ----► │  disabling │
        │              └────────────┘        └──────────┘       └────────────┘
        │                  │                       │  upgrade           │ ok
        │                  ▼                       ▼                    │
        │              ┌──────────┐           ┌────────────┐            │
        │              │ disabled │           │ upgrading  │            │
        │              └──────────┘           └────────────┘            │
        │                  │  ▲                  │   │                  │
        │                  │  │ enable           │   │ ok (was_enabled) ▼
        │                  │  │                  │   ▼                  │
        │                  │  │ ok               │  enabled (new ver)   │
        │                  │  │                  │                      │
        │                  │  └──────────────────┤   fail (atomic)      │
        │  {uninstall,Mode}│                     │   → enabled (旧版)   │
        │                  ▼                     │                      │
        │            ┌──────────────┐  ok        │  fail (manual)       │
        │            │ uninstalling │────────────┘  → failed            │
        │            └──────────────┘                                   │
        │                  │                                            │
        │                  └────────────────────────────────────────────┘
        │                                                  fail
        ▼
   (failed → retry / force_uninstall / reset)
```

简化文字版 / Plain-text summary：

```
unknown ──install──→ installing ──ok──→ installed
installed ──enable──→ enabling ──ok──→ enabled
enabled ──disable──→ disabling ──ok──→ disabled
disabled ──enable──→ enabling ──ok──→ enabled
enabled ──upgrade──→ upgrading ──ok──→ enabled (new ver)
                                  ──fail (atomic)──→ enabled (old ver)
                                  ──fail (manual)──→ failed
disabled ──uninstall──→ uninstalling ──ok──→ unknown
installed ──uninstall──→ uninstalling ──ok──→ unknown
*-ing ──fail / timeout / cancel──→ failed
failed ──retry──→ prev *-ing
failed ──force_uninstall──→ uninstalling
failed ──reset──→ unknown
```

---

## 5. 各状态下 callback 调用 / Callbacks per state

### 5.1 install 流程详细 step 顺序 / install step sequence

| Step # | Step 名 | 实现 / Implementation | 错误处理 / Error handling |
|--------|---------|----------------------|-------------------------|
| 1 | `verify_signature` | `imboy_plugin_signature:verify_file(Path ++ "/plugin.config", Path ++ "/SIGNATURE")` | `{error, signature_mismatch}` → `step_failed`，无副作用，**无需回滚** |
| 2 | `parse_manifest` | `imboy_plugin_toml:load(Path ++ "/plugin.config")` | `{error, Reason}` → `step_failed`，无副作用 |
| 3 | `validate_dependencies` | `imboy_plugin_dependency:validate_constraints([NewManifest \| ExistingManifests])` | `{error, {missing_dep \| version_mismatch \| invalid_constraint, ...}}` → `step_failed`，无副作用 |
| 4 | `migrate_up` | `imboy_plugin_migrate:run(Manifest, up)`（Phase 3.2 待实施） | `{error, sql_error}` → `step_failed`；副作用：可能已部分应用 SQL，按 rollback_strategy 处理（atomic 回滚 = `migrate down`） |
| 5 | `register_manifest` | `persistent_term:put({imboy_plugin_manifest, Name}, Manifest)` | 不会失败；副作用 = persistent_term 写入 |
| 6 | `trigger_loader_rescan`（可选） | `imboy_plugin_loader:scan()` | 失败仅告警，不阻塞 |

完成后转入 `installed`。

### 5.2 enable 流程详细 step 顺序 / enable step sequence

| Step # | Step 名 | 实现 / Implementation | 错误处理 |
|--------|---------|----------------------|---------|
| 1 | `recheck_dependencies` | 校验 `depends_on` 中的插件均已 `enabled`（查询其他 lifecycle statem 状态） | `{error, deps_not_enabled, [Names]}` → `step_failed` |
| 2 | `call_start` | `apply(Manifest.module, start, [StartArgs])`，5 秒超时 | `{error, Reason}` → `step_failed`，无副作用 |
| 3 | `mount_workers` | 通知 `<name>_sup` 启动 children（manifest 声明的 worker spec） | `{error, sup_failed}` → `step_failed`，回滚：`stop/1` |
| 4 | `register_routes` | `imboy_router_registry:register(Name, Manifest.routes)` | `{error, invalid_route_namespace}` → `step_failed`，回滚：`mount_workers` 反向 + `stop/1` |
| 5 | `cowboy_dispatch_reload` | 触发 `cowboy:set_env(imboy_listener, dispatch, NewDispatch)` | 失败回滚 `register_routes` |
| 6 | `register_capabilities` | 写入 capability registry（Phase 4 后续）；无 capability 时跳过 | 失败回滚到 step 5 |

完成后转入 `enabled`。

### 5.3 disable 流程详细 step 顺序 / disable step sequence

| Step # | Step 名 | 实现 | 错误处理 |
|--------|---------|------|---------|
| 1 | `cascade_check` | 查询是否有其他已 enabled 的插件依赖本插件 | `{error, has_dependents, [Names]}` → `step_failed`（除非 force=true） |
| 2 | `unregister_routes` | `imboy_router_registry:unregister(Name)` + cowboy dispatch reload | 不应失败；如失败强制 best_effort |
| 3 | `unregister_capabilities` | 反向 §5.2 step 6 | best_effort |
| 4 | `unmount_workers` | 通知 `<name>_sup` 停止 children（graceful，shutdown=5000ms） | 超时强制 kill |
| 5 | `call_stop` | `apply(Manifest.module, stop, [PluginState])`，5 秒超时 | best_effort，超时仅告警 |

完成后转入 `disabled`。

### 5.4 upgrade 流程详细 step 顺序 / upgrade step sequence

| Step # | Step 名 | 实现 | 错误处理 |
|--------|---------|------|---------|
| 1 | `verify_signature_new` | 验签新版 plugin.config | step_failed，无副作用 |
| 2 | `parse_manifest_new` | 解析新 manifest | step_failed |
| 3 | `validate_dependencies_new` | 校验新版依赖 | step_failed |
| 4 | `record_was_enabled` | 记录 `was_enabled = (current_state == enabled)` | 必成功 |
| 5 | `disable_if_enabled` | 如 `was_enabled`，执行 §5.3 disable 全流程 | step_failed → atomic 回滚（重新 enable 旧版） |
| 6 | `migrate_up` | `imboy_plugin_migrate:run(NewManifest, {upgrade, V, V'})` | step_failed → atomic 回滚（migrate down 到旧版 + re-enable） |
| 7 | `swap_manifest` | `persistent_term:put({imboy_plugin_manifest, Name}, NewManifest)` | 必成功 |
| 8 | `enable_if_was_enabled` | 如 `was_enabled`，执行 §5.2 enable 全流程 | step_failed → atomic 回滚（migrate down + 恢复旧 manifest + re-enable） |

完成后：
- `was_enabled = true` → 转入 `enabled`
- `was_enabled = false` → 转入 `installed`

### 5.5 uninstall 流程详细 step 顺序 / uninstall step sequence

| Step # | Step 名 | 实现 | 错误处理 |
|--------|---------|------|---------|
| 1 | `force_disable_if_enabled` | 如当前 `enabled`，先执行 disable | best_effort |
| 2 | `migrate_down` | 仅当 `Mode == drop_data` 且 `manifest.migrations.preserve_on_uninstall == false`：`imboy_plugin_migrate:run(Manifest, down)` | step_failed → 进入 `failed`（uninstall 不可中途回退） |
| 3 | `unregister_manifest` | `persistent_term:erase({imboy_plugin_manifest, Name})` | 必成功 |
| 4 | `clear_audit_summary` | 写一条 uninstall 完成审计；保留历史日志 | — |

完成后转入 `unknown`，statem 进程**自销毁**（`stop, normal`）。

### 5.6 health_check（任意 enabled / installed / disabled 状态）

```
gen_statem 收到 health_check
  → 启动临时进程 P 调 Manifest.module:health/0
  → P 启动 100ms timer，到点 exit(P, kill)
  → P 完成发回 {health_result, Report} 或 {health_timeout, _}
  → statem 不改 state，仅在 audit_log 记录 + 通过 reply 返回 admin
```

> health/0 调用**不在 statem 主进程内**执行，避免阻塞 state 转换。

---

## 6. 回滚策略 / Rollback strategy

按 `manifest.lifecycle.rollback_strategy` 字段：

### 6.1 atomic（事务式）

```
任一 step 失败：
  1. statem 暂停在当前状态
  2. 反向遍历 undo_log，逐条执行 undo action
  3. undo 全部成功 → 回到入口状态（如 install 失败回到 unknown）
  4. undo 任一失败 → 升级为 failed，标记 partial_rollback
```

undo_log 示例：
```
install undo_log:
  step 4 migrate_up   → undo: migrate_down
  step 5 register_manifest → undo: persistent_term:erase

enable undo_log:
  step 2 call_start   → undo: call_stop
  step 3 mount_workers → undo: unmount_workers
  step 4 register_routes → undo: unregister_routes
  step 5 dispatch_reload → undo: dispatch_reload(without this plugin)
```

### 6.2 best_effort（尽力回滚）

```
任一 step 失败：
  1. 反向遍历 undo_log，每条独立 try/catch
  2. undo 失败仅记录告警 + audit_log，不阻塞
  3. 最终一定回到入口状态，但可能残留少量副作用（如孤立路由项）
  4. statem 进入 failed（标记 best_effort_partial）
```

### 6.3 manual（不自动回滚）

```
任一 step 失败：
  1. statem 直接进入 failed
  2. 不执行任何 undo
  3. admin 必须通过 retry / force_uninstall / reset 恢复
  4. 适合关键业务插件，避免自动 rollback 引入二次故障
```

### 6.4 选择建议 / Recommendation

| 插件类型 | 推荐策略 |
|---------|---------|
| 标准业务插件（channel / moment / location） | atomic |
| 数据敏感型插件（涉及 schema 重构、大表 migration） | manual |
| 实验性 / canary 插件 | best_effort |

---

## 7. 超时处理 / Timeouts

每个 *-ing 状态使用 gen_statem 的 `state_timeout` 设置最大停留时间。超时触发 `timeout` 内部事件，按 rollback_strategy 处理后落入 `failed`。

| 状态 / State | 超时 / Timeout | 备注 / Note |
|-------------|---------------|------------|
| `installing` | 60_000 ms（60s） | 含 SQL migration；如 migration 预计耗时更长，manifest 可声明 `lifecycle.install_timeout_ms` 覆盖 |
| `enabling` | 30_000 ms（30s） | 主要等 `start/1` + worker 启动 |
| `disabling` | 10_000 ms（10s） | 含 worker graceful shutdown（默认 5s）+ stop/1（最长 5s） |
| `upgrading` | 120_000 ms（120s） | disable + migrate + enable 三段累加 |
| `uninstalling` | 30_000 ms（30s） | 含可选 migrate down |
| 单 step 内部超时 | 5_000 ms | 每个 callback 调用（start/1, stop/1, migrate/3）独立超时；超时被视为 step_failed |
| health/0 调用 | 100 ms | 由临时进程 + 强 timer kill 实现，**插件作者不写 try/catch** |

manifest 可选覆盖 / Optional override in manifest:
```erlang
#{
    lifecycle => #{
        rollback_strategy => atomic,
        install_timeout_ms => 300_000,    %% 5min for heavy migrations
        enable_timeout_ms  => 60_000,
        upgrade_timeout_ms => 600_000     %% 10min
    }
}
```

---

## 8. 依赖联动 / Dependency cascade

### 8.1 enable 时的依赖检查 / On enable

```
enable A:
  Deps = manifest.depends_on（如 [{B, "^1.0"}, {C, "^2.0"}]）
  for each Dep in Deps:
    if Dep 当前状态 != enabled:
      返回 {error, deps_not_enabled, [Dep, ...]}
      不进入 enabling
  通过 → 进入 enabling
```

> 不**自动级联 enable** 依赖项，避免隐式启动；admin 必须显式按拓扑顺序操作。Admin REST API 提供 `cascade=true` 开关，开启后由 admin 服务先调用 `topological_sort` 计算顺序再依次 enable。

### 8.2 disable 时的反向级联检查 / On disable

```
disable A:
  Dependents = 所有 enabled 插件中 depends_on 包含 A 的
  if Dependents != []:
    返回 {error, has_dependents, [Names]}
    除非 force=true
  通过 → 进入 disabling
```

### 8.3 upgrade 时的兼容性重检 / On upgrade

```
upgrade A V → V':
  NewDeps = NewManifest.depends_on
  validate_constraints([NewManifest | OtherManifests])
  失败 → 不进入 upgrading

  Dependents = 当前依赖 A 的 enabled 插件
  for each D in Dependents:
    DepConstraint = D.depends_on[A]
    if not check_constraint(parse_semver(V'), DepConstraint):
      返回 {error, breaks_dependents, [{D, DepConstraint}, ...]}
      除非 force=true
```

### 8.4 拓扑顺序由 dependency 模块提供 / Order from dependency module

```
启动序列 enable [A, B, C]（其中 B depends_on A, C depends_on B）:
  {ok, [A, B, C]} = imboy_plugin_dependency:topological_sort(Manifests),
  enable_each_in_order([A, B, C]).

关闭序列 disable [A, B, C]:
  {ok, Sorted} = imboy_plugin_dependency:topological_sort(Manifests),
  disable_each_in_order(lists:reverse(Sorted)).
```

---

## 9. 与组件协作 / Component integration

### 9.1 与 imboy_plugin_signature

- `installing` 状态 step 1 调 `verify_file/2`
- `upgrading` 状态 step 1 调 `verify_file/2` 验新版
- 失败 → `step_failed`，无副作用，**不**进入 atomic 回滚（无可回滚状态）

### 9.2 与 imboy_plugin_toml

- `installing` step 2、`upgrading` step 2 调 `load/1`
- 解析失败 → `step_failed`

### 9.3 与 imboy_plugin_dependency

- `installing` step 3、`enabling` step 1、`upgrading` step 3 + 兼容性重检调 `validate_constraints/1`
- admin REST API 服务调 `topological_sort/1` 计算批量 enable 顺序

### 9.4 与 imboy_plugin_migrate（Phase 3.2 待实施）

- `installing` step 4 调 `run(Manifest, up)`
- `upgrading` step 6 调 `run(NewManifest, {upgrade, V, V'})`
- `uninstalling` step 2（仅 drop_data）调 `run(Manifest, down)`
- atomic 回滚通过 `run(Manifest, down)` 实现

### 9.5 与 imboy_plugin_loader

- `installing` step 6 调 `imboy_plugin_loader:scan()` 重扫并刷新 persistent_term
- `installing` step 5 也可直接 `persistent_term:put`（避免依赖 loader，二选一）
- loader 与 lifecycle 共享 persistent_term 这一 source of truth

### 9.6 与 imboy_plugin_sup / `<name>_sup`

- `enabling` step 3 调 `<name>_sup:start_child/1` 挂载 worker（当真实 worker 实施时）
- `disabling` step 4 调 `<name>_sup:terminate_child/1`
- Phase 1 当前 4 个生产插件无 worker，本 step 自然 N/A

### 9.7 与 imboy_router_registry

- `enabling` step 4 调 `register/2`
- `disabling` step 2 调 `unregister/1`
- `upgrading` 在 disable 子流程中走 unregister，enable 子流程中走 register

### 9.8 与 cowboy

- `register/2` 与 `unregister/1` 后必须触发 `cowboy:set_env(imboy_listener, dispatch, NewDispatch)` 让新路由生效
- 此热更原子，不中断现有连接

### 9.9 与 plugin_audit_log（§11）

- 每次状态转换、每次 step 完成 / 失败、每次 admin 事件接收都写一条 audit log
- 通过 `imboy_plugin_audit:write/1` 异步写入（独立 worker，不阻塞 statem）

---

## 10. Admin REST API 设计 / Admin REST API design

P4-T4 实施前置。所有 endpoint 在 `/v1/adm/plugins/*`，需 admin JWT。

### 10.1 endpoint 一览 / Endpoint summary

| Method | Path | 语义 |
|--------|------|------|
| GET | `/v1/adm/plugins` | 列出全部插件及当前状态 |
| GET | `/v1/adm/plugins/{name}` | 单插件详情 |
| GET | `/v1/adm/plugins/{name}/state` | 单插件当前状态（轻量） |
| GET | `/v1/adm/plugins/{name}/health` | 触发一次 health_check 并返回 |
| POST | `/v1/adm/plugins/{name}/install` | 触发 install 事件 |
| POST | `/v1/adm/plugins/{name}/enable?cascade=<bool>` | 触发 enable |
| POST | `/v1/adm/plugins/{name}/disable?force=<bool>` | 触发 disable |
| POST | `/v1/adm/plugins/{name}/upgrade?to=<version>` | 触发 upgrade |
| DELETE | `/v1/adm/plugins/{name}?mode=<preserve_data\|drop_data>` | 触发 uninstall |
| POST | `/v1/adm/plugins/{name}/cancel` | 取消进行中操作 |
| POST | `/v1/adm/plugins/{name}/retry` | 从 failed 重试上次动作 |
| POST | `/v1/adm/plugins/{name}/force_uninstall?mode=<>` | 从 failed 强制卸载 |
| POST | `/v1/adm/plugins/{name}/reset` | 从 failed 重置为 unknown |
| GET | `/v1/adm/plugins/{name}/audit?limit=<>` | 查询审计日志 |

### 10.2 异步语义 / Async semantics

lifecycle 操作多为耗时（数秒至数分钟），采用 **HTTP 202 Accepted + 后续轮询 / WS 推送** 模式：

```
POST /v1/adm/plugins/channel/enable

Response 202:
{
  "code": 0,
  "msg": "accepted",
  "payload": {
    "operation_id": "op_8472938472938",
    "plugin": "channel",
    "from_state": "installed",
    "target_state": "enabled",
    "started_at": "2026-04-29T10:00:00Z",
    "poll_url": "/v1/adm/plugins/channel/state",
    "audit_url": "/v1/adm/plugins/channel/audit?since=op_8472938472938"
  }
}
```

完成 / 失败时通过 admin WS 推送 `plugin_lifecycle_changed` 事件：
```json
{
  "type": "plugin_lifecycle_changed",
  "plugin": "channel",
  "operation_id": "op_8472938472938",
  "from_state": "enabling",
  "to_state": "enabled",
  "result": "ok",
  "duration_ms": 1234,
  "ended_at": "2026-04-29T10:00:01Z"
}
```

### 10.3 错误响应 / Error responses

| HTTP | code | 含义 |
|------|------|------|
| 400 | `invalid_state_transition` | 当前 state 不允许该操作（如 `enabled` 状态再 enable） |
| 400 | `invalid_args` | 参数缺失或格式错误（如 mode 取值非法） |
| 409 | `deps_not_enabled` | 依赖未满足 |
| 409 | `has_dependents` | 有 enabled 插件依赖本插件，禁止 disable（除非 force） |
| 409 | `breaks_dependents` | upgrade 新版本破坏现有依赖（除非 force） |
| 409 | `operation_in_progress` | 已有 *-ing 操作，需先 cancel |
| 422 | `signature_mismatch` | 签名校验失败 |
| 422 | `manifest_invalid` | manifest 解析或 schema 校验失败 |
| 500 | `internal` | 未分类错误 |

### 10.4 请求 / 响应示例 / Request / response examples

```http
POST /v1/adm/plugins/channel/install
Content-Type: application/json

{
  "path": "priv/plugins/channel"
}

→ 202 Accepted
{
  "code": 0,
  "payload": { "operation_id": "...", "from_state": "unknown", "target_state": "installed" }
}
```

```http
GET /v1/adm/plugins/channel/state

→ 200 OK
{
  "code": 0,
  "payload": {
    "name": "channel",
    "state": "enabled",
    "version": "1.0.0",
    "since": "2026-04-29T10:00:01Z",
    "rollback_strategy": "atomic",
    "deadline": null,
    "last_error": null
  }
}
```

```http
DELETE /v1/adm/plugins/channel?mode=preserve_data

→ 202 Accepted
```

---

## 11. 操作审计日志 / Audit log

P4-T5 实施前置。每次状态转换 / step 进展 / admin 事件均落 `plugin_audit_log` 表。

### 11.1 表 schema / Table schema

```sql
CREATE TABLE plugin_audit_log (
    id              BIGINT PRIMARY KEY,                          -- TSID
    plugin_name     VARCHAR(64)  NOT NULL,
    operation_id    VARCHAR(64),                                  -- admin 触发的操作 id（NULL = 内部事件）
    event           VARCHAR(64)  NOT NULL,                        -- install_started / step_done / state_transition / ...
    from_state      VARCHAR(32),                                  -- 转换前 state
    to_state        VARCHAR(32),                                  -- 转换后 state
    step            VARCHAR(64),                                  -- 当前 step（如 verify_signature / migrate_up）
    operator        VARCHAR(64),                                  -- admin uid 或 'system'
    started_at      BIGINT       NOT NULL,                        -- 毫秒时间戳
    ended_at        BIGINT,                                       -- 毫秒时间戳，NULL = 仍在进行
    duration_ms     INT,                                          -- = ended_at - started_at
    result          VARCHAR(16),                                  -- ok / failed / cancelled / timeout
    error_code      VARCHAR(64),                                  -- 失败时分类码
    error_detail    TEXT,                                         -- 失败时详细信息（脱敏后）
    metadata        JSONB,                                        -- 任意扩展（如 from_version / to_version / mode）
    created_at      TIMESTAMP    NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_plugin_audit_log_plugin_started
    ON plugin_audit_log (plugin_name, started_at DESC);
CREATE INDEX idx_plugin_audit_log_operation
    ON plugin_audit_log (operation_id);
CREATE INDEX idx_plugin_audit_log_event
    ON plugin_audit_log (event, started_at DESC);
```

### 11.2 事件类型 / Event types

| event | 何时触发 |
|-------|---------|
| `admin_request_received` | admin REST 请求落到 statem |
| `state_entered` | 进入新状态 |
| `state_exited` | 离开当前状态 |
| `step_started` | step 开始 |
| `step_done` | step 成功 |
| `step_failed` | step 失败 |
| `rollback_started` | atomic / best_effort 回滚开始 |
| `rollback_step_done` | 回滚单步成功 |
| `rollback_step_failed` | 回滚单步失败 |
| `rollback_finished` | 回滚整体完成 |
| `cancelled` | admin 取消 |
| `timeout_triggered` | state_timeout 触发 |
| `health_invoked` | health_check 调用 |

### 11.3 写入策略 / Write strategy

- 异步写入（独立 worker `imboy_plugin_audit_worker`），失败不影响 statem
- 批量提交（每 100ms 或 50 条）减少 DB 压力
- 严重事件（`step_failed`、`rollback_*`、`cancelled`）实时写入 + 触发 metric

### 11.4 admin 查询 / Admin query

```http
GET /v1/adm/plugins/channel/audit?limit=50&since=2026-04-29T00:00:00Z

→ 200 OK
{
  "code": 0,
  "payload": {
    "items": [
      {
        "id": 81234567890,
        "operation_id": "op_xxx",
        "event": "state_transition",
        "from_state": "enabling",
        "to_state": "enabled",
        "step": null,
        "duration_ms": 1234,
        "result": "ok",
        "started_at": "2026-04-29T10:00:00.000Z",
        "ended_at": "2026-04-29T10:00:01.234Z"
      }
    ],
    "total": 1
  }
}
```

---

## 12. 测试策略 / Testing strategy

### 12.1 测试金字塔 / Test pyramid

| 层 / Layer | 重点 / Focus | 工具 / Tools |
|-----------|-------------|-------------|
| 纯 statem 状态转换 | mock 所有 dep 模块，仅验证状态机本身 | eunit + meck |
| step 单元测试 | 单 step 函数的 happy / failure path | eunit |
| 集成测试 | 端到端 install→enable→disable→uninstall | common_test，依赖临时 PG |
| 故障注入 | 每个 step 模拟失败，验证 rollback | meck（动态返回 `{error, ...}`） |
| 依赖联动 | 多插件拓扑场景（A depends_on B 等） | common_test |
| 回滚策略 | atomic / best_effort / manual 三档分别覆盖 | common_test |

### 12.2 关键测试用例 / Key test cases

1. **happy path**：unknown → installing → installed → enabling → enabled → disabling → disabled → uninstalling → unknown
2. **install 失败回滚（atomic）**：migrate_up 失败 → migrate_down 成功 → 回到 unknown
3. **install 失败回滚（best_effort）**：migrate_up 失败 + migrate_down 也失败 → 进入 failed（标记 partial_rollback）
4. **install 失败（manual）**：migrate_up 失败 → 直接 failed，不执行 down
5. **enable 依赖未启用**：A depends_on B，B 未 enabled → enable A 返回 `deps_not_enabled`
6. **disable 反向依赖**：B 已 enabled 且依赖 A → disable A 返回 `has_dependents`
7. **upgrade 破坏依赖**：B depends_on A `^1.0`，A 升级到 2.0 → 返回 `breaks_dependents`
8. **upgrade atomic 回滚**：migrate_up 新版失败 → migrate down + 恢复旧 manifest + re-enable 旧版
9. **超时**：installing 60s 后强制 timeout → atomic rollback → unknown 或 best_effort → failed
10. **uninstall preserve_data**：mode=preserve_data → 跳过 migrate_down，仅清 persistent_term
11. **uninstall drop_data**：mode=drop_data → migrate_down 全部 SQL → schema 删除
12. **cancel 中断**：installing 中 admin 发 cancel → atomic rollback → unknown
13. **health_check 超时**：plugin health/0 阻塞 200ms → statem 收到 `health_timeout`，返回 `{unhealthy, timeout}`
14. **statem 进程崩溃**：模拟 lifecycle statem exit → `imboy_plugin_lifecycle_sup` 重启，状态从 audit_log 恢复或回到 failed

### 12.3 覆盖率目标 / Coverage targets

- 状态转换图所有边（约 25 条）100% 覆盖
- 每个 step 的成功 + 失败两条路径覆盖
- 三档 rollback_strategy 各覆盖至少一个 *-ing 状态
- 依赖联动：单依赖 / 多依赖 / 循环（应在 install 时被 dependency 模块拒绝）

---

## 13. 实施切片建议 / Implementation slices

P4-T1 按 KISS + TDD slice pattern 拆 4 切片，每切片独立 RED→GREEN→REFACTOR 闭环：

### 切片 1 / Slice 1：纯状态机骨架 / Pure statem skeleton

**范围 / Scope**：
- `src/lib/imboy_plugin_lifecycle.erl` gen_statem，handle_event_function
- 10 个 state（含 `failed`）的 enter / event handler
- 状态转换函数纯 sealed decision（输入 = 当前 state + event + manifest 摘要，输出 = next state + actions list）
- **不**调用任何外部组件（dep 模块均 stubbed）

**TDD**：
- eunit 覆盖 §4 状态转换图所有边
- 验证 invalid event 在每个 state 下返回 `{error, invalid_state_transition}`

**交付**：单 .erl + 测试，零生产副作用，可作为后续切片的 sealed 决策核心。

### 切片 2 / Slice 2：组件接线 / Component wiring

**范围 / Scope**：
- 在切片 1 基础上，每个 step 真实调用 `imboy_plugin_signature` / `imboy_plugin_toml` / `imboy_plugin_dependency` / `imboy_plugin_loader` / `imboy_router_registry`
- step 调用通过 transient worker 执行，`gen_statem:cast` 回送 `step_done / step_failed`
- migrate 部分仍 mock（等 Phase 3.2 实施）

**TDD**：
- meck mock 各 dep 模块，验证调用顺序与参数
- 集成测试：4 个生产插件（channel/moment/location/group_collab）的 install + enable + disable + uninstall 全流程

**交付**：lifecycle.erl + step_worker.erl + 集成测试。

### 切片 3 / Slice 3：超时 + 回滚 / Timeouts + rollback

**范围 / Scope**：
- 每个 *-ing 状态加 `state_timeout`
- 三档 rollback_strategy 实现（含 undo_log 维护）
- timeout / cancel / step_failed 三种触发路径统一进入回滚分支

**TDD**：
- 故障注入：每个 step mock 返回 `{error, ...}`，验证 atomic 回滚链
- 超时测试：`state_timeout` 触发 → 验证 audit_log 与最终状态

### 切片 4 / Slice 4：依赖联动 + audit + admin API / Cascade + audit + admin API

**范围 / Scope**：
- enable / disable / upgrade 时按 §8 联动检查
- `plugin_audit_log` 表 + `imboy_plugin_audit_worker` 异步写入
- 10.1 的 13 个 admin REST endpoint（接入 `imboy-admin-frontend` Vue 由 P5 完成）

**TDD**：
- common_test：A→B→C 拓扑场景全覆盖
- HTTP 集成测试：每个 endpoint 的 happy / 4xx / 5xx 路径

**完成后 P4-T1 / P4-T4 / P4-T5 全部 close。**

---

## 14. 兼容性 / Compatibility

### 14.1 与 contract.md 的关系 / Relationship to contract.md

- 状态枚举 100% 兼容 contract.md §7：`unknown / installing / installed / enabling / enabled / disabling / disabled / upgrading / uninstalling / failed`
- `lifecycle.rollback_strategy` 字段 100% 兼容 contract.md §7（atomic / best_effort / manual）
- 本文档**仅扩展不破坏**：新增 step 顺序、超时、依赖联动、audit log、admin API
- contract_version 仍为 1.0，不需要 bump

### 14.2 与已有组件不冲突 / Non-conflict with existing components

- `imboy_plugin_loader`：loader 仅写 persistent_term，lifecycle 仅读（`installing` step 5 也写，但语义一致：lifecycle 是动态写入源，loader 是启动期一次性写入源；二者通过 `imboy_plugin_audit_log.event` 区分来源）
- `imboy_plugin_sup`：lifecycle 不直接重启 plugin sup；sup 自动 restart 插件 worker，lifecycle 通过 `monitor` 感知崩溃后转入 `failed`
- `imboy_router_registry`：register / unregister 接口已就绪，lifecycle 直接调用
- `imboy_plugin_dependency`：纯函数已就绪，lifecycle 直接调用
- `imboy_plugin_migrate`：纯函数已就绪，副作用层（Phase 3.2）落地后 lifecycle 直接调用
- `imboy_plugin_signature`：核心库已就绪，lifecycle 直接调用

### 14.3 OTP / Erlang 版本

- 最低 OTP 28（与 imboy core 一致）
- 使用 `gen_statem` `handle_event_function` callback mode（OTP 19+ 已支持，OTP 28 稳定）
- `state_timeout`、`postpone`、`internal` 事件均使用 OTP 标准 API

### 14.4 升级路径 / Upgrade path

未来 lifecycle_version 升级遵循 contract.md §9 同样的 semver 规则：
- 增加新 step、扩枚举值（如新增 `paused` 状态）→ minor bump
- 删除状态、改 rollback 语义、改超时默认值 → major bump + ADR

---

## 15. 变更记录 / Changelog

| 日期 / Date | 变更 / Change | lifecycle_version | 作者 / Author |
|------------|---------------|-------------------|---------------|
| 2026-04-29 | 文档创建（Phase 4-T1 实施前置设计） | 1.0 | leeyi + Claude |
