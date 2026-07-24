# PostgreSQL 读副本配置指南
# PostgreSQL Read Replica Configuration Guide

> **版本 / Version**: 1.0.0 | **最后更新 / Last Updated**: 2026-05-27
> **适用范围 / Scope**: 生产环境主从流式复制 + Erlang pooler 读写分离
> **前置文档 / Prerequisites**: [clustering.md](./clustering.md), [sys.config](../../../config/sys.config)

---

## 目录 / Table of Contents

1. [架构概览 / Architecture Overview](#1-架构概览)
2. [PostgreSQL 流复制配置 / Streaming Replication Setup](#2-postgresql-流复制配置)
3. [Erlang pooler 读写分离配置 / pooler Read-Write Split Config](#3-erlang-pooler-读写分离配置)
4. [elib_pg 调用约定 / elib_pg Calling Convention](#4-elib_pg-调用约定)
5. [Helm values 变更示例 / Helm values Changes](#5-helm-values-变更示例)
6. [健康检查与监控 / Health Check & Monitoring](#6-健康检查与监控)
7. [故障转移 / Failover](#7-故障转移)
8. [常见问题 / Troubleshooting](#8-常见问题)

---

## 1. 架构概览
## 1. Architecture Overview

### 1.1 拓扑图 / Topology

```
                    ┌─────────────────────────────────────────────┐
                    │          Erlang/OTP 28+ 应用节点            │
                    │          Erlang/OTP 28+ App Node             │
                    │                                              │
                    │  ┌──────────────┐    ┌────────────────────┐ │
                    │  │  pooler      │    │  pooler            │ │
                    │  │  pool: pgsql │    │  pool: pgsql_ro    │ │
                    │  │  size: 80    │    │  size: 40          │ │
                    │  └──────┬───────┘    └────────┬───────────┘ │
                    │         │  写/事务               │  只读查询   │
                    │         │  Writes/Tx             │  Read-only │
                    └─────────┼───────────────────────┼────────────┘
                              │                       │
                    ┌─────────▼──────────┐   ┌───────▼────────────┐
                    │  PostgreSQL 主库    │   │  PostgreSQL 副本    │
                    │  Primary (R/W)     │──▶│  Replica (R/O)      │
                    │  port: 5432        │   │  port: 5432         │
                    │  pg.primary.svc    │   │  pg.replica.svc     │
                    └────────────────────┘   └────────────────────┘
                              │  Streaming Replication (WAL)
                              └──────────────────────────────▶
```

### 1.2 设计原则 / Design Principles

| 原则 | 说明 |
|------|------|
| **写操作走主库** | `INSERT` / `UPDATE` / `DELETE` / `CALL` / 事务 → `pgsql` pool |
| **读操作走副本** | `SELECT`（无强一致需求）→ `pgsql_ro` pool |
| **强一致读走主库** | 读取刚写入的数据（如登录后取 profile）必须走主库 |
| **副本延迟容忍** | 预期复制延迟 < 100ms；超过 500ms 触发告警 |

| Principle | Description |
|-----------|-------------|
| **Writes to primary** | `INSERT` / `UPDATE` / `DELETE` / `CALL` / transactions → `pgsql` pool |
| **Reads to replica** | `SELECT` (no strong-consistency need) → `pgsql_ro` pool |
| **Strong-consistent reads** | Data read immediately after write (e.g. profile after login) must use primary |
| **Replica lag tolerance** | Expected lag < 100ms; alert threshold 500ms |

---

## 2. PostgreSQL 流复制配置
## 2. Streaming Replication Setup

### 2.1 主库配置 / Primary: postgresql.conf

在主库 `/var/lib/postgresql/data/postgresql.conf` 中添加或修改以下参数：

Add or modify the following in the primary's `postgresql.conf`:

```ini
# ---- 流复制基础 / Streaming Replication Base ----
wal_level = replica              # 必须为 replica 或 logical
max_wal_senders = 5              # 允许的最大复制连接数（建议: 副本数 + 2）
wal_keep_size = 512              # 保留 WAL 段最小体积(MB)，防止副本追日志时 WAL 被清理
max_replication_slots = 5        # 复制槽数量（建议 >= max_wal_senders）

# ---- 性能调优 / Performance ----
synchronous_commit = on          # 主库默认同步提交；如可接受少量数据丢失改为 off
wal_compression = on             # 压缩 WAL，降低网络带宽占用

# ---- 监控 / Monitoring ----
track_commit_timestamp = on      # 记录事务提交时间，便于延迟计算
```

### 2.2 主库访问控制 / Primary: pg_hba.conf

```
# TYPE  DATABASE        USER            ADDRESS                 METHOD
# 允许复制用户从副本 IP 段连接 / Allow replica user from replica subnet
host    replication     replicator      10.0.0.0/24             scram-sha-256
```

> 生产环境请替换 `10.0.0.0/24` 为实际副本 IP 或 CIDR。
> In production, replace `10.0.0.0/24` with the actual replica IP or CIDR.

### 2.3 创建复制用户 / Create Replication User

在主库上执行 / Run on primary:

```sql
CREATE ROLE replicator
    WITH REPLICATION LOGIN
    PASSWORD 'CHANGE_ME_STRONG_PASSWORD';
```

### 2.4 初始化副本 / Initialize Replica

```bash
# 在副本机器上执行 / Run on replica host
pg_basebackup \
    -h pg.primary.example.com \
    -U replicator \
    -D /var/lib/postgresql/data \
    -P --wal-method=stream \
    -R                        # 自动生成 standby.signal + postgresql.auto.conf
```

`-R` 标志会自动创建 `standby.signal` 文件并写入 `primary_conninfo` 到 `postgresql.auto.conf`，
无需手动编辑。

The `-R` flag auto-creates `standby.signal` and writes `primary_conninfo` into
`postgresql.auto.conf`; no manual editing required.

### 2.5 副本配置 / Replica: postgresql.conf

```ini
# ---- 热备模式 / Hot Standby ----
hot_standby = on                 # 允许副本接受只读连接
hot_standby_feedback = on        # 将副本活跃事务反馈主库，避免 vacuum 冲突

# ---- 复制延迟限制 / Lag Limit ----
max_standby_streaming_delay = 30s   # 超过此值暂停应用 WAL（保护长查询）
```

### 2.6 复制槽（可选但推荐）/ Replication Slot (Optional but Recommended)

复制槽确保主库保留副本尚未消费的 WAL，防止副本因网络抖动落后后无法追赶。

Replication slots ensure the primary retains WAL segments until the replica
consumes them, preventing the replica from falling behind irreversibly.

```sql
-- 在主库执行 / Run on primary
SELECT pg_create_physical_replication_slot('imboy_replica_1');
```

在副本 `postgresql.auto.conf` 中指定：

```ini
primary_slot_name = 'imboy_replica_1'
```

> **注意 / Warning**: 若副本长时间离线且槽未被监控，主库磁盘可能因 WAL 积压耗尽。
> 生产环境务必监控 `pg_replication_slots.wal_status`。
>
> If a replica is offline for a long time and the slot is not monitored, the
> primary disk may fill up. Always monitor `pg_replication_slots.wal_status`
> in production.

---

## 3. Erlang pooler 读写分离配置
## 3. pooler Read-Write Split Config

### 3.1 sys.config 新增只读连接池 / Add Read-Only Pool in sys.config

在 `config/sys.config`（生产）和 `config/sys.local.config`（本地调试）的 `imboy` 应用配置中，
在现有 `pg_conf` 条目后增加 `pg_conf_ro`：

Add `pg_conf_ro` after the existing `pg_conf` entry in the `imboy` app section
of `config/sys.config` (production) and `config/sys.local.config` (local dev):

```erlang
%% 现有主库连接池（只写/读写）— 保持不变
%% Existing primary pool (write / read-write) — keep as-is
, {pg_conf
    , #{name => pgsql,
        max_count => 80,
        init_count => 5,
        start_mfa => {
            epgsql
            , connect
            , [
                #{
                    host     => "pg.primary.example.com"   %% IMBOY_PG_HOST
                    , username => "imboy_user"             %% IMBOY_PG_USER
                    , password => "CHANGE_ME"              %% IMBOY_PG_PASSWORD
                    , database => "imboy_pro"              %% IMBOY_PG_DB
                    , port    => 5432
                    , ssl     => true
                    , timeout => 4000
                    , codecs  => [{epgsql_codec_rfc3339_bin, []}]
                }
            ]
        }
    }
}

%% 新增：只读副本连接池
%% New: read-only replica pool
, {pg_conf_ro
    , #{name => pgsql_ro,
        max_count => 40,           %% 副本连接数通常为主库的 50%
        init_count => 5,
        start_mfa => {
            epgsql
            , connect
            , [
                #{
                    host     => "pg.replica.example.com"  %% IMBOY_PG_RO_HOST
                    , username => "imboy_user_ro"         %% IMBOY_PG_RO_USER（建议只读权限账号）
                    , password => "CHANGE_ME_RO"          %% IMBOY_PG_RO_PASSWORD
                    , database => "imboy_pro"             %% IMBOY_PG_RO_DB
                    , port    => 5432
                    , ssl     => true
                    , timeout => 4000
                    , codecs  => [{epgsql_codec_rfc3339_bin, []}]
                }
            ]
        }
    }
}
```

### 3.2 创建只读数据库账号 / Create Read-Only DB Account

```sql
-- 在主库执行；会自动同步到副本
-- Run on primary; will replicate to replica automatically
CREATE ROLE imboy_user_ro
    WITH LOGIN
    PASSWORD 'CHANGE_ME_RO';

-- 授予只读权限
GRANT CONNECT ON DATABASE imboy_pro TO imboy_user_ro;
GRANT USAGE   ON SCHEMA public       TO imboy_user_ro;
GRANT SELECT  ON ALL TABLES IN SCHEMA public TO imboy_user_ro;

-- 对未来新建表自动授权
ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT SELECT ON TABLES TO imboy_user_ro;
```

### 3.3 启动 / 注册只读池 / Register Pool at Startup

如果应用使用 `pooler` 的 `start_mfa` 配置驱动自动注册，确认 `imboy_app.erl` 或池初始化代码
也读取 `pg_conf_ro` 并调用 `pooler:new_pool/1`：

If the app auto-registers pools from `start_mfa` config, verify that
`imboy_app.erl` or pool-init code reads `pg_conf_ro` and calls
`pooler:new_pool/1`:

```erlang
%% imboy_app.erl（示意，具体实现视项目代码）
start_pools() ->
    {ok, PgConf}   = application:get_env(imboy, pg_conf),
    {ok, PgConfRo} = application:get_env(imboy, pg_conf_ro),
    pooler:new_pool(PgConf),
    pooler:new_pool(PgConfRo).
```

---

## 4. elib_pg 调用约定
## 4. elib_pg Calling Convention

### 4.1 路由规则 / Routing Rules

| 操作类型 | 使用接口 | 连接池 |
|---------|---------|-------|
| INSERT / UPDATE / DELETE | `elib_pg:execute/2,3` | `pgsql`（主库）|
| 事务（多语句） | `elib_pg:with_tx/1,2` | `pgsql`（主库）|
| 普通 SELECT（最终一致可接受） | `elib_pg:query_ro/2,3` | `pgsql_ro`（副本）|
| 写后立即读（强一致） | `elib_pg:query/2,3` | `pgsql`（主库）|

| Operation | Interface | Pool |
|-----------|-----------|------|
| INSERT / UPDATE / DELETE | `elib_pg:execute/2,3` | `pgsql` (primary) |
| Transactions (multi-stmt) | `elib_pg:with_tx/1,2` | `pgsql` (primary) |
| Plain SELECT (eventual consistency OK) | `elib_pg:query_ro/2,3` | `pgsql_ro` (replica) |
| Read-after-write (strong consistency) | `elib_pg:query/2,3` | `pgsql` (primary) |

### 4.2 query_ro 实现建议 / Suggested query_ro Implementation

当前 `elib_pg` 尚无 `query_ro` 导出。建议在 `elib_pg.erl` 中新增以下两个函数，
从 `pgsql_ro` 池取连接，逻辑与 `query/2` 完全对称：

`elib_pg` currently does not export `query_ro`. Add the following two functions
that borrow a connection from the `pgsql_ro` pool, mirroring `query/2`:

```erlang
%% @doc 只读查询，从 pgsql_ro 副本池取连接
%% Read-only query using the pgsql_ro replica pool.
-spec query_ro(Sql :: binary() | string(), Params :: list()) ->
    {ok, Columns :: list(), Rows :: list()} | {error, term()}.
query_ro(Sql, Params) ->
    with_conn(pgsql_ro, fun(C) -> query(C, Sql, Params) end).

%% @doc 带显式连接的只读查询
-spec query_ro(Conn :: pid(), Sql :: binary() | string(), Params :: list()) ->
    {ok, Columns :: list(), Rows :: list()} | {error, term()}.
query_ro(Conn, Sql, Params) ->
    query(Conn, Sql, Params).
```

同时在 `-export` 列表中添加 `query_ro/2, query_ro/3`。

Also add `query_ro/2, query_ro/3` to the `-export` list.

### 4.3 Repo 层使用示例 / Repo Layer Usage Examples

```erlang
%% ✅ 写操作 → execute（主库）
%% Write operations → execute (primary)
insert_message(Msg) ->
    Sql = <<"INSERT INTO msg_c2c (id, from_uid, to_uid, body) VALUES ($1, $2, $3, $4)">>,
    elib_pg:execute(Sql, [Msg#msg.id, Msg#msg.from, Msg#msg.to, Msg#msg.body]).

%% ✅ 事务 → with_tx（主库）
%% Transactions → with_tx (primary)
transfer_credits(FromUid, ToUid, Amount) ->
    elib_pg:with_tx(fun(Conn) ->
        elib_pg:execute(Conn,
            <<"UPDATE user_wallet SET balance = balance - $1 WHERE user_id = $2">>,
            [Amount, FromUid]),
        elib_pg:execute(Conn,
            <<"UPDATE user_wallet SET balance = balance + $1 WHERE user_id = $2">>,
            [Amount, ToUid])
    end).

%% ✅ 普通读取 → query_ro（副本，最终一致）
%% Plain reads → query_ro (replica, eventual consistency)
list_friends(Uid, Limit, Offset) ->
    Sql = <<"SELECT friend_id, remark FROM friend WHERE user_id = $1
             ORDER BY created_at DESC LIMIT $2 OFFSET $3">>,
    elib_pg:query_ro(Sql, [Uid, Limit, Offset]).

%% ✅ 写后立即读 → query（主库，强一致）
%% Read-after-write → query (primary, strong consistency)
create_user_and_return(UserMap) ->
    elib_pg:with_tx(fun(Conn) ->
        elib_pg:insert(Conn, <<"user">>, UserMap, <<"id, username, created_at">>)
    end).

%% ❌ 错误示例：写操作误用 query_ro
%% Wrong: write operation using query_ro
bad_update(Uid, Name) ->
    elib_pg:query_ro(
        <<"UPDATE \"user\" SET username = $1 WHERE id = $2">>,
        [Name, Uid]).
%% 副本是只读的，此调用将直接报错。
%% The replica is read-only; this call will return an error.
```

### 4.4 选择主库还是副本的决策树 / Primary vs Replica Decision Tree

```
需要执行此操作？
Is this operation...
        │
        ├── INSERT / UPDATE / DELETE / CALL？
        │   └──▶ elib_pg:execute / with_tx  →  pgsql (主库)
        │
        ├── SELECT，且必须读到刚写入的数据？
        │   (e.g. 注册后立即取用户信息)
        │   └──▶ elib_pg:query            →  pgsql (主库)
        │
        └── SELECT，最终一致可接受？
            (e.g. 列表、搜索、统计)
            └──▶ elib_pg:query_ro         →  pgsql_ro (副本)
```

---

## 5. Helm values 变更示例
## 5. Helm values Changes

### 5.1 values.yaml 新增副本配置段 / Add Replica Section to values.yaml

在现有 `externalDatabase` 块下方追加（`deploy/helm/values.yaml`）：

Append below the existing `externalDatabase` block in `deploy/helm/values.yaml`:

```yaml
# ---------- 外部 PostgreSQL 主库（写）----------
# External PostgreSQL Primary (writes)
externalDatabase:
  host: "pg-primary.example.com"
  port: 5432
  database: imboy_pro

# ---------- 外部 PostgreSQL 副本（只读）----------
# External PostgreSQL Replica (read-only)
externalDatabaseRO:
  host: "pg-replica.example.com"
  port: 5432
  database: imboy_pro
```

### 5.2 Secret 中新增副本凭据 / Add Replica Credentials to Secret

在 `deploy/helm/templates/secret.yaml`（或 `values.prod.yaml`）中追加：

```yaml
secrets:
  # 主库 / Primary
  postgresUser: ""
  postgresPassword: ""
  postgresDb: ""

  # 副本 / Replica (read-only)
  postgresRoUser: ""
  postgresRoPassword: ""
  postgresRoDb: ""
```

### 5.3 Deployment env 注入 / Inject Env in Deployment

在 `deploy/helm/templates/backend-deployment.yaml` 的 `env` 段新增：

Add to the `env` section in the backend Deployment template:

```yaml
# 只读副本连接信息 / Read-only replica connection
- name: IMBOY_PG_RO_HOST
  valueFrom:
    configMapKeyRef:
      name: {{ include "imboy.fullname" . }}-config
      key: PG_RO_HOST
- name: IMBOY_PG_RO_PORT
  value: "5432"
- name: IMBOY_PG_RO_USER
  valueFrom:
    secretKeyRef:
      name: {{ include "imboy.fullname" . }}-secret
      key: POSTGRES_RO_USER
- name: IMBOY_PG_RO_PASSWORD
  valueFrom:
    secretKeyRef:
      name: {{ include "imboy.fullname" . }}-secret
      key: POSTGRES_RO_PASSWORD
- name: IMBOY_PG_RO_DB
  valueFrom:
    secretKeyRef:
      name: {{ include "imboy.fullname" . }}-secret
      key: POSTGRES_RO_DB
```

### 5.4 sys.runtime.config 运行时覆盖 / Runtime Override in sys.runtime.config

应用启动时，`imboy_env` 模块读取 `IMBOY_*` 环境变量覆盖配置。
建议在 `imboy_env.erl` 中增加副本配置的覆盖逻辑（与现有主库覆盖对称）：

At startup, `imboy_env` reads `IMBOY_*` env vars to override config.
Add replica override logic in `imboy_env.erl` mirroring the existing primary:

```erlang
%% 示意：覆盖只读池的 host/user/password/db
%% Illustrative: override read-only pool host/user/password/db
override_pg_ro_conf() ->
    case application:get_env(imboy, pg_conf_ro) of
        {ok, Conf = #{start_mfa := {epgsql, connect, [ConnOpts]}}} ->
            ConnOpts1 = maybe_override(ConnOpts, host,     "IMBOY_PG_RO_HOST"),
            ConnOpts2 = maybe_override(ConnOpts1, username, "IMBOY_PG_RO_USER"),
            ConnOpts3 = maybe_override(ConnOpts2, password, "IMBOY_PG_RO_PASSWORD"),
            ConnOpts4 = maybe_override(ConnOpts3, database, "IMBOY_PG_RO_DB"),
            NewConf   = Conf#{start_mfa => {epgsql, connect, [ConnOpts4]}},
            application:set_env(imboy, pg_conf_ro, NewConf);
        _ -> ok
    end.
```

---

## 6. 健康检查与监控
## 6. Health Check & Monitoring

### 6.1 复制延迟监控 / Replication Lag Monitoring

在主库定期执行以下查询（Prometheus exporter 或 cron 脚本均可）：

Run the following on the primary periodically (Prometheus exporter or cron):

```sql
-- 各副本复制状态 / Per-replica replication status
SELECT
    client_addr,
    state,
    sent_lsn,
    write_lsn,
    flush_lsn,
    replay_lsn,
    -- 字节延迟 / Byte lag
    (sent_lsn - replay_lsn)::bigint        AS replay_lag_bytes,
    -- 时间延迟 / Time lag (requires track_commit_timestamp = on)
    write_lag,
    flush_lag,
    replay_lag
FROM pg_stat_replication;
```

在副本上执行 / Run on replica:

```sql
-- 副本自身延迟 / Replica self-reported lag
SELECT
    now() - pg_last_xact_replay_timestamp() AS replication_lag,
    pg_is_in_recovery()                      AS is_standby,
    pg_last_wal_receive_lsn()                AS receive_lsn,
    pg_last_wal_replay_lsn()                 AS replay_lsn;
```

### 6.2 Erlang 连接池状态 / Erlang Pool Status

```erlang
%% 在 Erlang shell 或 ctl 工具中 / In Erlang shell or ctl tool
pooler:status().
%% 查看两个池的空闲/占用连接数
%% Shows free/in-use connections for both pgsql and pgsql_ro pools
```

### 6.3 告警阈值 / Alert Thresholds

| 指标 / Metric | 警告 / Warning | 严重 / Critical |
|--------------|---------------|----------------|
| 复制字节延迟 Replay lag bytes | > 50 MB | > 200 MB |
| 复制时间延迟 Replay lag time | > 500ms | > 5s |
| 副本连接数 Replica pool usage | > 80% | > 95% |
| 主库连接数 Primary pool usage | > 80% | > 95% |
| 复制槽 WAL 积压 Slot WAL retained | > 1 GB | > 5 GB |

### 6.4 Docker Compose 健康检查 / Docker Compose Health Check

`deploy/docker-compose.prod.yml` 中副本服务建议配置（参考现有主库 healthcheck 风格）：

For the replica service in `deploy/docker-compose.prod.yml` (mirroring the
existing primary healthcheck style):

```yaml
imboy_pg18_replica:
  image: ${PG_IMAGE:-imboy/pg18:3.6.1-2}
  container_name: imboy_pg18_replica
  hostname: pg-replica.docker.imboy.pub
  restart: unless-stopped
  environment:
    TZ: ${TZ:-Asia/Shanghai}
    POSTGRES_USER: ${POSTGRES_USER}
    POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
    POSTGRES_DB: ${POSTGRES_DB}
    # 副本模式 / Standby mode
    POSTGRES_REPLICATION_MODE: slave
    POSTGRES_REPLICATION_USER: replicator
    POSTGRES_REPLICATION_PASSWORD: ${PG_REPLICATION_PASSWORD}
    POSTGRES_MASTER_HOST: imboy_pg18
    POSTGRES_MASTER_PORT_NUMBER: "5432"
  volumes:
    - ${DATA_DIR:-./data}/pg18_replica:/var/lib/postgresql
  expose:
    - "5432"
  depends_on:
    imboy_pg18:
      condition: service_healthy
  healthcheck:
    test: ["CMD-SHELL",
           "pg_isready -U $$POSTGRES_USER -d $$POSTGRES_DB &&
            psql -U $$POSTGRES_USER -d $$POSTGRES_DB -c 'SELECT pg_is_in_recovery()' | grep -q 't'"]
    interval: 10s
    timeout: 5s
    retries: 10
  deploy:
    resources:
      limits:
        memory: ${PG_REPLICA_MEM_LIMIT:-2048M}
  logging:
    driver: "json-file"
    options: { max-size: "10m", max-file: "10" }
```

---

## 7. 故障转移
## 7. Failover

### 7.1 手动故障转移（副本升主）/ Manual Failover (Promote Replica)

**前提 / Prerequisites**: 已确认主库不可恢复或网络完全隔离；避免脑裂（split-brain）。

**Prerequisite**: Confirm that the primary is unrecoverable or fully isolated
to avoid split-brain.

#### 步骤 / Steps

**Step 1 — 停止应用写入 / Stop application writes**

```bash
# 在所有 Erlang 节点上暂停写入（或调整负载均衡只转发读请求）
# Pause writes on all Erlang nodes (or reconfigure LB to route only reads)
_rel/imboy/bin/imboy rpc "application:set_env(imboy, read_only_mode, true)"
```

**Step 2 — 确认副本已追上最新 WAL / Confirm replica has caught up**

```sql
-- 在副本上执行 / Run on replica
SELECT pg_last_wal_receive_lsn() = pg_last_wal_replay_lsn() AS caught_up;
-- 期望结果：t（true）/ Expected: t (true)
```

**Step 3 — 提升副本为主库 / Promote replica to primary**

```bash
# 方法 A（推荐）：pg_ctl promote / Method A (recommended)
pg_ctl promote -D /var/lib/postgresql/data

# 方法 B：pg_promote() SQL 函数（PG 12+）/ Method B: SQL function (PG 12+)
psql -U postgres -c "SELECT pg_promote();"
```

提升成功后，副本将退出 recovery 模式，接受写连接。

After promotion, the replica exits recovery mode and accepts write connections.

**Step 4 — 更新应用连接配置 / Update application connection config**

```bash
# 更新 IMBOY_PG_HOST 指向新主库 IP / Update IMBOY_PG_HOST to new primary IP
# 更新 IMBOY_PG_RO_HOST 指向（如有）新副本 IP，或暂时与主库相同
# 热重载配置 / Hot-reload config
_rel/imboy/bin/imboy rpc "config_ds:local_reload()"
```

**Step 5 — 验证 / Verify**

```bash
# 确认连接池状态 / Confirm pool status
_rel/imboy/bin/imboy rpc "pooler:status()"

# 冒烟测试 / Smoke test
make ctl ARGS="smoke all"
make ctl ARGS="db ping"
```

**Step 6 — 恢复写入 / Resume writes**

```bash
_rel/imboy/bin/imboy rpc "application:unset_env(imboy, read_only_mode)"
```

### 7.2 自动故障转移（Patroni / 其他 HA 方案）/ Automatic Failover

生产环境建议使用 **Patroni + etcd/Consul** 管理自动 leader 选举和 VIP 切换。
部署 Patroni 后，应用层无需手动执行上述步骤；仅需确保：

For production, use **Patroni + etcd/Consul** for automatic leader election
and VIP switching. With Patroni deployed, the app layer needs only to:

1. `IMBOY_PG_HOST` / `IMBOY_PG_RO_HOST` 指向 Patroni 管理的 VIP 或 HAProxy 端点。
   Point to Patroni-managed VIP or HAProxy endpoint.
2. 应用连接池设置合理的重连策略（`epgsql` 在连接断开时自动重连）。
   Set reasonable reconnect policy in the connection pool.

---

## 8. 常见问题
## 8. Troubleshooting

### Q1: 副本连接报错 `ERROR: cannot execute ... in a read-only transaction`

**原因**: 代码误用 `query_ro`（或直接路由到副本）执行了写操作。

**解决**: 检查调用栈，将写操作改为 `elib_pg:execute/2,3` 或 `elib_pg:with_tx/1,2`。

**Cause**: A write was routed to the replica via `query_ro`.

**Fix**: Inspect the call stack and use `elib_pg:execute/2,3` or
`elib_pg:with_tx/1,2` for write operations.

---

### Q2: 复制延迟持续增大 / Replication lag keeps growing

**原因 / Causes**:
- 副本 I/O 不足，无法跟上 WAL 应用速度
- 主库存在大批量写入（bulk load）
- `hot_standby_feedback = off` 导致 vacuum 冲突

**排查步骤 / Diagnosis**:
```sql
-- 主库：检查待发送 WAL / Primary: check unsent WAL
SELECT * FROM pg_stat_replication;

-- 副本：检查应用进程 / Replica: check recovery process
SELECT * FROM pg_stat_recovery_prefetch;  -- PG 14+
```

**缓解 / Mitigation**:
- 检查副本磁盘 IOPS；升级存储
- 批量写入改为分批（每批 1000 行）
- 确认 `hot_standby_feedback = on`

---

### Q3: 副本连接池（pgsql_ro）全部耗尽 / pgsql_ro pool exhausted

**排查 / Diagnosis**:
```erlang
pooler:status().
%% 关注 pgsql_ro 的 in_use_count 与 max_count
```

**解决 / Fix**:
- 增大 `pg_conf_ro` 的 `max_count`（副本机器允许的最大连接数以 `max_connections` 为上限）
- 检查是否有慢查询持连接不释放：在副本执行 `SELECT * FROM pg_stat_activity WHERE state = 'active'`
- 为 `query_ro` 调用加超时保护

---

### Q4: 提升副本后原主库重新上线，脑裂风险 / Old primary comes back after promotion

**处理 / Handling**:
1. 立即隔离旧主库网络访问（防止客户端写入旧主）
2. 将旧主库降级为新主库的副本（`pg_rewind` + `standby.signal`）
3. 验证数据一致性后重新接入副本角色

```bash
# 用 pg_rewind 将旧主回退到新主的时间线
# Use pg_rewind to resync old primary to new primary's timeline
pg_rewind \
    --target-pgdata=/var/lib/postgresql/data \
    --source-server="host=NEW_PRIMARY_HOST user=replicator dbname=postgres"
```

---

**文档维护 / Doc Maintenance**: 变更连接池参数或复制拓扑时同步更新此文档。
**Maintenance**: Update this doc when changing pool parameters or replication topology.
