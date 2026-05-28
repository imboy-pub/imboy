<!-- Generated: 2026-05-27 | Files: 330 src | See also: architecture.md, backend.md, data.md, dependencies.md -->

# CODEMAPS Overview / 总览

> 双语 / Bilingual: 中文权威，English mirror.
> 本文件是 imboy 后端（Erlang/OTP）CODEMAPS 的入口索引。
> This file is the entry index for the imboy backend (Erlang/OTP) CODEMAPS package.

---

## 文件索引 | File Index

| 文件 / File | 内容 / Content |
|---|---|
| [architecture.md](./architecture.md) | 分层架构图、消息流水线、启动流程 / Layer diagram, message pipeline, boot flow |
| [backend.md](./backend.md) | 路由分段、Handler→Logic→DS→Repo 完整映射表 / Route segments, full H→L→DS→R map |
| [data.md](./data.md) | PostgreSQL schema、关键表、迁移策略 / DB schema, key tables, migration strategy |
| [dependencies.md](./dependencies.md) | 外部依赖、OTP 应用列表、升级约束 / External deps, OTP apps, upgrade constraints |

---

## 模块依赖图 | Module Dependency Graph

```
                  ┌─────────────────────────────┐
                  │         客户端 / Client       │
                  └──────────────┬──────────────┘
                                 │ HTTP / WebSocket
              ┌──────────────────▼──────────────────┐
              │   Handler 层 / Handler Layer (69)    │
              │  src/api/ (50)   src/adm/ (19)       │
              └──────────────────┬──────────────────┘
                                 │ 业务调用 / call
              ┌──────────────────▼──────────────────┐
              │    Logic 层 / Logic Layer (70)       │
              │  src/logic/                          │
              └──────────────────┬──────────────────┘
                                 │ 数据访问 / data access
              ┌──────────────────▼──────────────────┐
              │    DS 层 / Data Service Layer (77)   │
              │  src/ds/  (缓存封装 + 跨 Repo 编排)   │
              └──────────────────┬──────────────────┘
                                 │ SQL
              ┌──────────────────▼──────────────────┐
              │    Repo 层 / Repository Layer (71)   │
              │  src/repo/                           │
              └──────────────────┬──────────────────┘
                                 │
              ┌──────────────────▼──────────────────┐
              │         PostgreSQL 18+               │
              └─────────────────────────────────────┘

              ◄── 横向 / Cross-cutting ──────────────►
              src/lib/ (43)  elib_pg · imboy_cache · elib_tsid
                             elib_async · elib_retry · elib_cipher
```

**约束 / Constraints:**
- 调用方向严格自上而下；同层禁止横向依赖
- Calls flow strictly top-down; no lateral dependencies within a layer
- DS 是唯一允许跨 Repo 编排的层 / DS is the only layer allowed to orchestrate across Repos
- 所有 DB 访问必须通过 `elib_pg`；禁止绕过 / All DB access MUST go through `elib_pg`

---

## 关键功能路径 | Key Feature Call Chains

### 用户消息 C2C | C2C Message

```
msg_handler (src/api/msg_handler.erl)
  → msg_c2c_logic:send/3  (src/logic/msg_c2c_logic.erl)
    → msg_store_ds:stage/2 (src/ds/msg_store_ds.erl)       # 暂存 / stage
    → msg_c2c_ds:deliver/3 (src/ds/msg_c2c_ds.erl)         # 投递 / deliver
      → websocket_ds:push/2 (src/ds/websocket_ds.erl)      # WS 推送
    → msg_store_worker (src/ds/msg_store_worker.erl)        # 批量归档
      → msg_c2c_repo:insert/1 (src/repo/msg_c2c_repo.erl)  # PostgreSQL
```

### 群组任务 | Group Task

```
group_task_handler (src/api/group_task_handler.erl)
  → group_task_logic:create/4  (src/logic/group_task_logic.erl)
    → group_task_ds:insert/2   (src/ds/group_task_ds.erl)
      → group_task_repo:insert/1 (src/repo/group_task_repo.erl)  # PostgreSQL
  → group_task_logic:list/2
    → group_task_ds:list/2
      → group_task_repo:list/2
  → group_task_logic:close/2
    → group_task_ds:update_status/2
      → group_task_repo:update/2
```

### 用户认证 | Authentication

```
passport_handler (src/api/passport_handler.erl)
  → passport_logic:login/3  (src/logic/passport_logic.erl)
    → user_ds:get_by_account/1 (src/ds/user_ds.erl)
      → user_repo:find_by_account/1 (src/repo/user_repo.erl)
    → auth_ds:issue_token/2 (src/ds/auth_ds.erl)
      → token_repo:insert/1 (src/repo/token_repo.erl)
```

### WebSocket 连接 | WebSocket Connection

```
websocket_handler (src/api/websocket_handler.erl)
  → websocket_logic:init/2 (src/logic/websocket_logic.erl)
    → auth_ds:verify_token/1
    → websocket_ds:register/2  # syn 进程注册 / syn process registry
  → websocket_logic:handle_message/3
    → message_router_logic:route/2 (src/logic/message_router_logic.erl)
      → msg_c2c_logic | msg_c2g_logic
```

---

## 按功能域关键文件 | Key Files by Domain

### 路由与入口 | Routing & Entry

```
src/imboy_router.erl          # Cowboy dispatch，498 条路由 / 498 routes
src/imboy_app.erl             # 启动、运行时守卫 / Boot, runtime guards
src/imboy_sup.erl             # OTP 监督树根 / Supervision tree root
```

### 用户与认证 | User & Auth

```
src/api/passport_handler.erl  # 登录注册 / Login & register
src/api/user_handler.erl      # 用户信息 CRUD
src/logic/passport_logic.erl  # 认证业务逻辑
src/ds/auth_ds.erl            # Token 缓存封装
src/ds/token_ds.erl           # Token 加解密
src/repo/user_repo.erl        # 用户 SQL
src/repo/token_repo.erl       # Token SQL
```

### 消息 | Messaging

```
src/api/msg_handler.erl           # HTTP 消息入口
src/api/websocket_handler.erl     # WS 入口 + 会话管理
src/logic/msg_c2c_logic.erl       # C2C 投递逻辑
src/logic/msg_c2g_logic.erl       # C2G 投递逻辑
src/logic/message_router_logic.erl # 消息路由决策
src/ds/msg_store_worker.erl       # 批量归档 worker
src/ds/websocket_ds.erl           # WS 推送 DS
src/repo/msg_c2c_repo.erl         # C2C SQL
src/repo/msg_archive_repo.erl     # 归档 SQL（conv_seq 严格序）
```

### 群组 | Group

```
src/api/group_handler.erl           # 群基础操作
src/api/group_task_handler.erl      # 群作业 HTTP 入口
src/logic/group_logic.erl           # 群业务逻辑
src/logic/group_task_logic.erl      # 群作业逻辑
src/ds/group_ds.erl                 # 群缓存封装
src/ds/group_task_ds.erl            # 群作业 DS
src/repo/group_repo.erl             # 群 SQL
src/repo/group_task_repo.erl        # 群作业 SQL
```

### 基础设施 | Infrastructure

```
src/lib/elib_pg.erl           # DB facade（强制，禁绕过）
src/lib/elib_tsid.erl         # 分布式 ID 生成
src/lib/elib_cipher.erl       # 加解密工具
src/lib/elib_async.erl        # 异步任务
src/lib/elib_retry.erl        # 重试策略
src/lib/imboy_cache.erl       # depcache 封装
include/error_code.hrl        # 错误码宏
include/imboy_const.hrl       # 系统常量
```

### 管理后台 | Admin (adm)

```
src/adm/adm_user_handler.erl          # 用户管理
src/adm/adm_group_handler.erl         # 群组管理
src/adm/adm_group_task_handler.erl    # 群作业管理（如有）
src/adm/adm_auth_middleware.erl       # adm 鉴权中间件
```

---

## 约定速查 | Convention Quick Reference

| 规范 / Rule | 要点 / Detail |
|---|---|
| UTF-8 中文 | `<<"操作成功"/utf8>>` |
| 错误码 | `?ERR_OK`, `?ERR_USER_NOT_FOUND` etc. — `include/error_code.hrl` |
| ID 生成 | `elib_tsid:generate(TableAtom)` → TSID integer |
| JSON ID | 前端 `safeParseBigIntJson` 自动转 string |
| DB 访问 | 必须通过 `elib_pg`；参数化查询防注入 |
| 响应格式 | `elib_response:success/2,3` · `elib_response:error/3` |
| 分页参数 | `elib_param:page(Body, #{page=>1, page_size=>20})` |
