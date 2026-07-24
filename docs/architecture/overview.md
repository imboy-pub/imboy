# Imboy 架构总览

> Last Updated: 2026-03-15  
> Status: 长期架构文档  
> Scope: 当前仓库的分层职责、调用链路与设计边界  
> Source of truth: `src/imboy_router.erl`, `src/api/`, `src/adm/`, `src/logic/`, `src/ds/`, `src/repo/`, `src/lib/`  
> Related docs: `docs/architecture/database-access.md`, `docs/reference/api-format.md`, `docs/README.md`

## 相关 ADR / Related Decisions

- [`2026-03-15-modular-monolith-boundaries.md`](../adr/0005-modular-monolith-boundaries.md): 明确后端继续保持 modular monolith，并仅在高变化扩展点引入轻量插件化。

## 迁移状态（2026-03-28 — 闭环）

> Workspace Modular + Plugin Architecture 迁移已全部完成（Task 0-17）。

- 已建立稳定领域入口的逻辑层模块：`messaging_logic`、`moment_logic`、`channel_logic`、`group_logic`、`group_vote_logic`、`group_schedule_logic`、`group_task_logic`、`auth_logic`、`passport_logic`、`user_logic`、`e2ee_logic`、`report_logic`、`user_collect_logic`。
- 生产中的扩展点保持轻量：`src/lib/imboy_plugin_registry.erl` 作为插件 manifest contract，被 `channel_handler`、`moment_handler`、`group_*_handler`、`report_handler`、后台对应 handler 以及 `imboy_policy` 使用。
- 兼容层已清理：`imboy_plugin_registry:all/0` 与 `get/1` 已移除，调用点已全部收敛到 `manifests/0` 与 `manifest/1`。
- 边界门禁已启用：`script/check_module_boundaries.sh` 在 CI 中防止跨域直接依赖。
- 回归验证：`make app` 编译通过；2709 非DB断言全部通过；修复 `elib_uri:exclusion_param/2` 无 query 字段崩溃。

- 已建立稳定领域入口的逻辑层模块：`messaging_logic`、`moment_logic`、`channel_logic`、`group_logic`、`group_vote_logic`、`group_schedule_logic`、`group_task_logic`、`auth_logic`、`passport_logic`、`user_logic`、`e2ee_logic`、`report_logic`、`user_collect_logic`。
- 生产中的扩展点保持轻量：`src/lib/imboy_plugin_registry.erl` 作为插件 manifest contract，被 `channel_handler`、`moment_handler`、`group_*_handler`、`report_handler`、后台对应 handler 以及 `imboy_policy` 使用。
- 兼容层仍保留且属于迁移期设计：`imboy_plugin_registry:all/0` 与 `get/1` 继续作为 `manifests/0` 与 `manifest/1` 的 deprecated aliases，待调用点完全收敛后再删除。

## 1. 文档目的

本文档用于描述 `Imboy` 当前仓库的真实分层方式，以及各层在代码中的职责边界。

当前项目保留了领域驱动设计和分层架构的思想，但**并不是**严格照搬传统 Java / Spring 式的 `controller -> service -> domain -> repository` 模板。对本仓库而言，更准确的理解方式是：

**Router / Middleware -> Handler -> Logic -> DS -> Repo -> PostgreSQL / 外部依赖**

目标不是把概念讲得更复杂，而是让以下问题有统一答案：

1. HTTP / WebSocket 请求应该在哪一层落地；
2. 业务规则应该放在哪一层；
3. SQL 和持久化应该由谁负责；
4. 哪些公共能力应该沉淀到 `lib/`，而不是散落在业务模块里。

## 2. 当前分层模型

| 层级 | 主要目录 / 模块 | 主要职责 | 不应承担的职责 |
|---|---|---|---|
| 路由与中间件 | `src/imboy_router.erl`、各类 `*_middleware.erl` | 路由分发、鉴权、跨域、请求上下文预处理 | 不写业务规则，不直接做数据落库 |
| Handler | `src/api/`、`src/adm/` | 协议适配、参数读取、响应封装、调用下层 | 不承载复杂业务编排，不直接拼 SQL |
| Logic | `src/logic/` | 业务流程编排、权限与状态流转、跨模块协调 | 不处理 HTTP 细节，不直接暴露协议层对象 |
| DS | `src/ds/` | 领域数据服务、缓存/消息/组合读写、通用数据侧能力 | 不直接处理页面或接口语义 |
| Repo | `src/repo/` | SQL、持久化、查询与数据映射 | 不写协议判断，不承载业务叙事 |
| Lib | `src/lib/` | 通用工具、响应包装、时间/加密/数据库公共能力 | 不写具体业务功能分支 |

## 3. 典型请求流转

以一个典型 HTTP API 为例，请求流转通常如下：

1. `Router` 根据路径把请求分发到对应 `Handler`；
2. `Middleware` 完成登录态、权限、跨域、上下文等通用处理；
3. `Handler` 读取参数、校验基本格式、调用 `Logic`；
4. `Logic` 组织业务流程，并视情况调用 `DS` / `Repo` / `lib`；
5. `DS` 负责组合数据读写、缓存、消息投递或领域侧辅助能力；
6. `Repo` 负责最终 SQL 和数据库访问；
7. 结果回到 `Handler`，再由 `elib_response` 等公共模块统一输出响应。

WebSocket 路径本质也遵循同样思路：连接与协议入口在 `Handler` / `Middleware` 层，业务编排在 `Logic`，数据落点在 `DS` / `Repo`。

## 4. 各层职责说明

### 4.1 Router 与 Middleware

这一层负责“把请求送到正确的位置”，并在业务逻辑开始之前完成通用前置处理，例如：

- 路由匹配；
- 公共鉴权；
- CORS；
- 请求上下文注入；
- 后台与 App 侧不同入口的门禁差异化处理。

这一层的目标是让下层尽量在稳定上下文中运行，而不是承担业务决策。

### 4.2 Handler

`Handler` 是协议适配层，主要解决“怎么接”和“怎么回”的问题：

- 接收 HTTP / WebSocket 请求；
- 解析 path、query、body、header；
- 做基础参数校验和格式转换；
- 调用 `Logic`；
- 把业务结果转换为统一响应包。

`Handler` 可以做轻量分支，但不应沉淀复杂业务规则，也不应直接承担数据库访问。

### 4.3 Logic

`Logic` 是当前项目最核心的业务编排层，主要解决“应该怎么做”的问题：

- 组织主业务流程；
- 协调多个 `DS` / `Repo` / `lib` 模块；
- 落实权限、状态流转、门禁、幂等、补偿等规则；
- 维持接口语义和业务语义的一致性。

如果一个能力属于“消息怎么发”“群成员怎么变更”“功能关闭时应该返回什么”，通常都应优先落在 `Logic`。

### 4.4 DS

`DS` 可以理解为领域数据服务层，主要服务于“数据相关但不等于单表 SQL”的场景，例如：

- 对缓存、消息、配置、辅助数据做统一封装；
- 对多个 `Repo` 结果做组合；
- 提供上层可复用的数据服务能力；
- 屏蔽某些底层读写细节，让 `Logic` 不必知道所有存储细节。

它不是协议层，也不是页面层；它更接近“围绕领域数据的一层服务抽象”。

### 4.5 Repo

`Repo` 是持久化边界，职责应该尽量稳定而清晰：

- 编写 SQL；
- 调用 `elib_pg` / `elib_pg_sql`；
- 返回稳定的数据结构；
- 不掺入协议层、展示层和过强的业务语义。

如果某段逻辑的本质是“查什么、写什么、怎么按条件过滤”，应优先考虑放到 `Repo`。

### 4.6 Lib 与通用基础设施

`src/lib/` 中沉淀的是跨模块复用的公共能力，例如：

- 数据库访问公共封装：`elib_pg`、`elib_pg_sql`；
- 响应封装：`elib_response`；
- 请求辅助：`elib_req`；
- 时间、编码、加密、TSID、集群等基础能力。

公共库应尽量保持可复用、低耦合，不直接绑定某个业务模块的页面或交互语义。

## 5. 设计边界

为避免分层失真，建议长期坚持以下边界：

1. `Handler` 不直接拼 SQL、不直接落库；
2. `Logic` 不直接处理 HTTP 细节和响应包格式；
3. `Repo` 不承担业务主流程编排；
4. `DS` 不承担页面级或协议级判断；
5. 可复用基础能力优先沉淀到 `lib/`，不要复制到多个业务模块；
6. 需要跨层复用的规则，应优先抽象成稳定接口，而不是在多处拷贝实现。

## 6. 历史术语映射

仓库中早期文档曾引用较强的 DDD / 分层术语。为了避免误解，可按下表理解：

| 历史术语 | 当前仓库中的更贴切落点 |
|---|---|
| User Interface 层 | `Router` / `Middleware` / `Handler` |
| application service | `Logic` |
| domain service | 以 `Logic` 为主，部分可复用数据能力落在 `DS` |
| repository | `Repo` |
| infrastructure | `lib/`、数据库驱动、缓存、集群、日志等公共能力 |

这些映射用于帮助理解历史文档，不代表项目必须严格回到旧术语体系。

## 7. 相关文档

- `docs/architecture/database-access.md`
- `docs/reference/api-format.md`
- `docs/reference/error-codes.md`
- `docs/guides/operations/dependencies.md`
