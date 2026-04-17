<!-- Generated: 2026-04-17 | Files scanned: 330 src + 86 migrations | Token estimate: ~700 -->

# Architecture Codemap / 架构图谱

> 双语 / Bilingual: 中文权威，English mirror.

## Stack / 技术栈

- Erlang/OTP 28+ · Cowboy 2.10 · PostgreSQL 18+ (pg_jieba, postgis, timescaledb, pgcrypto)
- depcache (mem cache) · pooler + epgsql (DB pool) · syn (cluster registry) · lager (logs)

## Layers / 分层 (DDD, single-app, 4-tier)

```
Client ──► Cowboy ──► api/* | adm/*  (Handler 50+19)
                          ▼
                       logic/* (Logic 70)
                          ▼
                        ds/*  (Data Service 77)
                          ▼
                       repo/*  (Repository 71)
                          ▼
                     PostgreSQL
                          ▲
            lib/*  (43 infra: elib_pg, imboy_cache, imboy_syn, elib_async, elib_retry, elib_tsid, elib_cipher)
```

中文 / CN: 调用方向严格自上而下；同层禁止互相依赖（业务模块）；DS 是唯一允许跨 Repo 编排的层。

## Cross-cutting / 横切关注点

| Concern | Module | 说明 |
|---|---|---|
| Routing | `src/imboy_router.erl` (498 routes) | open / auth / api_v1 三段式 |
| App boot | `src/imboy_app.erl` | `validate_runtime_config/0` 生产 fail-fast |
| Supervision | `src/imboy_sup.erl` | OTP 监督树根 |
| Config | `src/ds/config_ds.erl` + `config/sys.config` | env(atom) 静态 / get(binary) 动态-DB |
| Env override | `src/lib/imboy_env.erl` | `IMBOY_*` 环境变量优先 sys.config |
| Cache | `imboy_cache` + `imboy_cache_sync` | depcache + 跨节点失效 |
| Cluster | `imboy_cluster`, `imboy_syn` | syn 进程注册 / 多节点 |

## Boot Flow / 启动流程

`imboy_app:start/2` →
1. `imboy_env:override_from_env/0` (env vars wash sys.config)
2. `validate_runtime_config/0` (prod 强制：jwt_key、pg_password、api_auth_switch=on、TURN→eturnal_secret)
3. `imboy_migrate` (priv/migrations/*.sql)
4. Cowboy listener · pooler · syn · ecron · workers (msg_store_worker)

## Message Pipeline / 消息流水线

```
WS/HTTP → msg_handler → message_router_logic → msg_c2c|c2g_logic
       → msg_store_ds (staging) → msg_store_worker (batch)
       → msg_c2c|c2g (deliver) + msg_archive_repo → msg_store (permanent, conv_seq 严格序)
```

注意：`msg_id`/TSID 仅近似时序；严格顺序以 `conv_seq` 为准。

## Files / 关键文件

- `src/imboy_app.erl:1` — boot + runtime guards
- `src/imboy_router.erl:1` — Cowboy dispatch
- `src/ds/msg_store_worker.erl` — archive trigger
- `src/lib/elib_pg.erl` — DB facade（强制使用，禁绕过）
- `CLAUDE.md` — full project context
