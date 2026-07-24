# IMBoy 多节点 Erlang 集群配置指南
# IMBoy Multi-node Erlang Cluster Configuration Guide

> **版本 / Version**: 1.0.0 | **最后更新 / Last Updated**: 2026-05-27
> **适用范围 / Scope**: 1.x 中期计划（ROADMAP.md §多节点部署文档）
> **前置文档 / Prerequisites**: [DEPLOYMENT.md](./deployment/DEPLOYMENT.md), [sys.config](../../../config/sys.config)

---

## 目录 / Table of Contents

1. [节点命名约定 / Node Naming](#1-节点命名约定)
2. [epmd 配置与防火墙 / EPMD & Firewall](#2-epmd-配置与防火墙)
3. [DNS SRV 节点发现 / DNS SRV Discovery](#3-dns-srv-节点发现)
4. [连接池分布式配置 / Distributed Pooler Config](#4-连接池分布式配置)
5. [集群健康验证 / Cluster Health Check](#5-集群健康验证)
6. [常见问题 / Troubleshooting](#6-常见问题)

---

## 1. 节点命名约定
## 1. Node Naming Convention

### 1.1 命名规则 / Naming Rules

IMBoy 使用 Erlang/OTP 标准长节点名（long names）。节点名格式：

IMBoy uses Erlang/OTP standard long node names. Node name format:

```
imboy@<hostname-or-ip>
```

| 场景 / Scenario | 节点名示例 / Example Node Name |
|----------------|-------------------------------|
| 单机开发 | `imboy@127.0.0.1` |
| 生产节点 1 | `imboy@node1.imboy.internal` |
| 生产节点 2 | `imboy@node2.imboy.internal` |
| 生产节点 3 | `imboy@node3.imboy.internal` |
| Docker 容器（主机网络） | `imboy@10.0.1.11` |

### 1.2 命名限制 / Naming Constraints

- 节点名中的主机部分必须可被集群内所有节点 **双向 DNS 解析**（或通过 IP 直接通信）
- 生产环境优先使用 **FQDN**（如 `node1.imboy.internal`），避免使用 IP（IP 变更会导致集群断裂）
- 所有节点的 **Erlang cookie** 必须完全一致（见 1.3）

### 1.3 Erlang Cookie 管理 / Cookie Management

```bash
# 生成强 cookie（所有节点使用同一个值）
openssl rand -hex 32 | tr -d '\n' > /etc/imboy/erlang.cookie
chmod 400 /etc/imboy/erlang.cookie
chown imboy:imboy /etc/imboy/erlang.cookie

# 在启动参数中指定 cookie（推荐，避免依赖 ~/.erlang.cookie）
# 在 relx.config 或启动脚本中设置：
# -setcookie XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# 或设置环境变量（imboy_env.erl 读取）
export IMBOY_CTL_COOKIE=<同上cookie值>
```

> **安全提示**：Cookie 等同于集群访问密码。不要写入代码库，使用 `/etc/imboy/erlang.cookie` 或秘密管理器注入。

### 1.4 TSID 节点 ID 配置 / TSID Node ID per Node

多节点时，每个节点必须配置**唯一**的 `tsid_dc_id` + `tsid_node_id` 组合，以确保分布式 ID 全局唯一。

In a multi-node setup, each node must have a **unique** `tsid_dc_id` + `tsid_node_id` combination.

```erlang
%% 节点 1 的 sys.config（或通过 IMBOY_TSID_DC_ID / IMBOY_TSID_NODE_ID 环境变量覆盖）
{tsid_dc_id,   1}  %% 数据中心 1
{tsid_node_id, 1}  %% 节点 1

%% 节点 2 的 sys.config
{tsid_dc_id,   1}  %% 同一数据中心
{tsid_node_id, 2}  %% 节点 2（必须不同）

%% 节点 3 的 sys.config
{tsid_dc_id,   1}
{tsid_node_id, 3}
```

`tsid_dc_bits=3` 时支持最多 8 个数据中心（DC 0–7），每 DC 最多 128 个节点（node_id 0–127）。

---

## 2. epmd 配置与防火墙
## 2. EPMD & Firewall Configuration

### 2.1 epmd 介绍 / What is epmd

epmd（Erlang Port Mapper Daemon）是 Erlang 集群的端口映射服务，类似 DNS。节点启动时向本机 epmd 注册，其他节点通过 epmd 查询目标节点的动态端口。

epmd is the Erlang Port Mapper Daemon — a registry service for Erlang nodes, similar to a local DNS for node ports. Each node registers with its local epmd at startup.

### 2.2 端口规划 / Port Planning

| 端口 / Port | 协议 | 用途 | 方向 |
|------------|------|------|------|
| **4369** | TCP | epmd 默认端口 | 节点间双向 |
| **9100–9200** | TCP | Erlang 分布式通信端口范围（动态）| 节点间双向 |
| **9800** | TCP | HTTP/WebSocket API | 客户端 → 节点 |

> 为限制 Erlang 分布式端口范围（避免防火墙全开放高端口），在 `sys.config` 或启动参数中设置：

```erlang
%% 在 sys.config 的 kernel 段添加：
{kernel, [
    {start_timer, true},
    {logger_level, info},
    %% 限制 Erlang 分布式通信端口范围（集群间防火墙规则只需开放此范围）
    {inet_dist_listen_min, 9100},
    {inet_dist_listen_max, 9200}
]}
```

### 2.3 防火墙规则（iptables / nftables）/ Firewall Rules

```bash
# 仅允许集群节点 IP 访问 epmd 和分布式端口
# 以下示例假设集群节点 IP 为 10.0.1.11, 10.0.1.12, 10.0.1.13

CLUSTER_IPS="10.0.1.11 10.0.1.12 10.0.1.13"

for IP in $CLUSTER_IPS; do
    # epmd
    iptables -A INPUT -s "$IP" -p tcp --dport 4369 -j ACCEPT
    # Erlang 分布式通信
    iptables -A INPUT -s "$IP" -p tcp --dport 9100:9200 -j ACCEPT
done

# 拒绝其他来源访问 epmd 和分布式端口
iptables -A INPUT -p tcp --dport 4369 -j DROP
iptables -A INPUT -p tcp --dport 9100:9200 -j DROP
```

> **安全原则**：epmd 端口（4369）和分布式端口（9100–9200）**绝对不能**对公网开放。Erlang 节点间信任完全基于 cookie，一旦端口暴露，攻击者可以执行任意 Erlang 代码。

### 2.4 Docker 网络隔离 / Docker Network Isolation

使用 `docker-compose.prod.yml` 时，所有服务已通过 `imboy-network`（内部桥接网络）隔离。

When using `docker-compose.prod.yml`, all services are already isolated within `imboy-network` (internal bridge network).

```yaml
# 多节点 Docker 部署时，使用 overlay 网络（需要 Docker Swarm 或手动配置）
# For multi-node Docker deployment, use an overlay network

networks:
  imboy-network:
    driver: overlay          # 替换 bridge 为 overlay
    attachable: true
    driver_opts:
      encrypted: "true"      # 加密节点间流量
```

---

## 3. DNS SRV 节点发现
## 3. DNS SRV Node Discovery

### 3.1 方案概述 / Overview

IMBoy 支持通过 `sys.config` 中的 `cluster_nodes` 静态配置节点列表，也可以通过 DNS SRV 记录动态发现节点。DNS SRV 方案适合节点数量可变的弹性部署场景。

IMBoy supports both static node list (via `cluster_nodes` in `sys.config`) and dynamic DNS SRV-based discovery.

### 3.2 静态节点列表（当前默认）/ Static Node List (Current Default)

```erlang
%% config/sys.config 或环境变量 CLUSTER_NODES
{cluster_nodes, [
    'imboy@node1.imboy.internal',
    'imboy@node2.imboy.internal',
    'imboy@node3.imboy.internal'
]}
```

```bash
# Docker 部署时通过环境变量注入
export CLUSTER_NODES="imboy@10.0.1.11,imboy@10.0.1.12,imboy@10.0.1.13"
```

### 3.3 DNS SRV 记录配置示例 / DNS SRV Record Example

在内部 DNS（如 CoreDNS、Consul DNS、Route53 私有区域）中添加以下 SRV 记录：

Add the following SRV records in your internal DNS (e.g., CoreDNS, Consul DNS, Route53 private zone):

```
; 格式: _service._proto.name TTL class SRV priority weight port target
_erlang._tcp.imboy.internal.  60  IN  SRV  10  100  4369  node1.imboy.internal.
_erlang._tcp.imboy.internal.  60  IN  SRV  10  100  4369  node2.imboy.internal.
_erlang._tcp.imboy.internal.  60  IN  SRV  10  100  4369  node3.imboy.internal.
```

### 3.4 CoreDNS 配置示例 / CoreDNS Configuration Example

```hcl
# CoreDNS Corefile（内网 DNS 服务）
imboy.internal:53 {
    file /etc/coredns/zones/imboy.internal
    log
    errors
}
```

```zone
; /etc/coredns/zones/imboy.internal
$TTL 60
@  IN SOA ns1.imboy.internal. admin.imboy.internal. (
       2026052701 ; serial
       3600       ; refresh
       900        ; retry
       604800     ; expire
       60         ; minimum
   )

@          IN  NS   ns1
ns1        IN  A    10.0.1.1

; 节点 A 记录
node1      IN  A    10.0.1.11
node2      IN  A    10.0.1.12
node3      IN  A    10.0.1.13

; SRV 记录（Erlang epmd）
_erlang._tcp  IN SRV  10 100 4369 node1
_erlang._tcp  IN SRV  10 100 4369 node2
_erlang._tcp  IN SRV  10 100 4369 node3
```

### 3.5 Erlang 应用层读取 SRV 记录 / Reading SRV Records in Erlang

```erlang
%% 在 imboy 启动时查询 SRV 记录并动态加入集群
%% 示例代码（可放入 imboy_app.erl 的 start/2 中）

discover_cluster_nodes() ->
    Domain = "_erlang._tcp.imboy.internal",
    case inet_res:lookup(Domain, in, srv) of
        [] ->
            %% 降级到静态配置
            application:get_env(imboy, cluster_nodes, []);
        SRVRecords ->
            %% [{Priority, Weight, Port, Host}, ...]
            Nodes = [list_to_atom("imboy@" ++ binary_to_list(Host))
                     || {_Prio, _Weight, _Port, Host} <- SRVRecords],
            lager:info("Discovered cluster nodes via DNS SRV: ~p", [Nodes]),
            Nodes
    end.

join_cluster(Nodes) ->
    [net_kernel:connect_node(Node) || Node <- Nodes].
```

---

## 4. 连接池分布式配置
## 4. Distributed Pooler Configuration

### 4.1 当前架构 / Current Architecture

IMBoy 使用 `pooler` 库管理 PostgreSQL 连接池（`epgsql`）。每个节点维护**独立的本地连接池**，`syn` 库负责跨节点的进程注册和消息路由。

IMBoy uses the `pooler` library for PostgreSQL connection pooling (`epgsql`). Each node maintains an **independent local pool**; `syn` handles cross-node process registration and message routing.

```
节点 1 (imboy@node1)          节点 2 (imboy@node2)
┌─────────────────────┐       ┌─────────────────────┐
│  pooler              │       │  pooler              │
│  └─ pgsql pool (80) │       │  └─ pgsql pool (80) │
│                      │◄─────►│                      │
│  syn (进程注册)      │ Erlang │  syn (进程注册)      │
│  WebSocket sessions  │ dist  │  WebSocket sessions  │
└─────────────────────┘       └─────────────────────┘
          │                              │
          └──────────┬───────────────────┘
                     ▼
              PostgreSQL 18
              (共享数据库)
```

### 4.2 多节点 pooler 配置 / Multi-node Pooler Config

每个节点的 `sys.config` 中的 `pg_conf` 配置保持相同（所有节点连接同一 PostgreSQL 实例或同一 PgBouncer）：

Each node's `sys.config` `pg_conf` points to the same PostgreSQL instance (or PgBouncer):

```erlang
{pg_conf, #{
    name       => pgsql,
    max_count  => 80,           %% 每节点最大连接数；3节点集群总计 240 连接
    init_count => 5,            %% 启动时预建连接数
    start_mfa  => {
        epgsql, connect, [#{
            host     => "pg.imboy.internal",   %% PgBouncer 或 PG 主机
            username => "imboy_app",            %% 使用 imboy_app 角色（最小权限）
            password => "",                     %% 由 IMBOY_PG_PASSWORD 注入
            database => "imboy_v1",
            port     => 5432,
            ssl      => true,
            timeout  => 4000,
            codecs   => [{epgsql_codec_rfc3339_bin, []}]
        }]
    }
}}
```

> **连接数规划**：3 节点 × 80 连接/节点 = 240 总连接。PostgreSQL 18 默认 `max_connections=100`，生产部署时须在 `postgresql.conf` 中增大，或使用 PgBouncer 作为连接代理。

### 4.3 PgBouncer 推荐配置（可选）/ PgBouncer Configuration (Optional)

多节点部署时推荐在 PG 前加一层 PgBouncer，减少实际连接数压力：

For multi-node deployments, a PgBouncer layer is recommended to reduce actual PG connection pressure:

```ini
# pgbouncer.ini
[databases]
imboy_v1 = host=pg.imboy.internal port=5432 dbname=imboy_v1

[pgbouncer]
listen_addr   = 0.0.0.0
listen_port   = 6432
auth_type     = scram-sha-256
auth_file     = /etc/pgbouncer/userlist.txt
pool_mode     = transaction       ; 事务模式（适合 OTP 短事务）
max_client_conn = 500
default_pool_size = 50
min_pool_size = 5
reserve_pool_size = 10
server_tls_sslmode = require
```

```bash
# Erlang 节点连接 PgBouncer（而非直接连 PG）：
# 将 sys.config 中 host 改为 pgbouncer 地址，port 改为 6432
IMBOY_PG_HOST=pgbouncer.imboy.internal
IMBOY_PG_PORT=6432
```

### 4.4 syn 跨节点进程注册 / syn Cross-node Process Registry

`syn` 用于 WebSocket 会话的跨节点查找和消息投递。集群组建后 `syn` 自动在节点间同步进程注册表。

`syn` handles cross-node WebSocket session lookup and message delivery. Once the cluster is formed, `syn` automatically synchronizes its registry across nodes.

```erlang
%% 在 Erlang console 中验证跨节点进程同步
%% Verify cross-node process sync in the Erlang console

%% 查看节点 1 上的 WS 会话数
length(syn:members(ws_session_group, 'imboy@node1.imboy.internal')).

%% 查看整个集群的在线 WS 会话总数
TotalSessions = lists:sum(
    [length(syn:members(ws_session_group, N)) || N <- nodes([this, connected])]
).
```

---

## 5. 集群健康验证
## 5. Cluster Health Check

### 5.1 基础节点状态 / Basic Node Status

```bash
# 检查单个节点状态
make ctl ARGS="node status"

# 指定目标节点（CTL_NODE 默认 imboy@127.0.0.1）
make ctl ARGS="node status" CTL_NODE=imboy@node1.imboy.internal

# 检查集群内所有已连接节点
make ctl ARGS="node list" CTL_NODE=imboy@node1.imboy.internal
```

### 5.2 集群拓扑验证 / Cluster Topology Verification

```bash
# 进入节点 1 的 remote_console
_rel/imboy/bin/imboy remote_console

# 查看已连接节点列表（预期包含 node2, node3）
> nodes().
% 期望输出: ['imboy@node2.imboy.internal', 'imboy@node3.imboy.internal']

% 查看节点存活状态
> [net_adm:ping(N) || N <- nodes()].
% 期望输出: [pong, pong]

% 查看 syn 跨节点同步状态
> syn:get_local_member_count(ws_session_group).  % 本节点 WS 会话数
> syn:get_all_members(ws_session_group).         % 全集群 WS 会话列表（大集群慎用）

% 退出
> q().
```

### 5.3 连接池状态（每节点）/ Pool Status per Node

```bash
# 检查节点 1 连接池
IMBOY_CTL_NODE=imboy@node1.imboy.internal make ctl ARGS="db ping"

# 在 remote_console 中详细查看
> pooler:status().
% {pools,[{pgsql,[{size,80},{available,65},{in_use,15}]}]}
```

### 5.4 自动化健康脚本 / Automated Health Script

将以下脚本加入 Cron 或 Prometheus 自定义采集器：

Add the following script to cron or a Prometheus custom collector:

```bash
#!/usr/bin/env bash
# scripts/cluster_health_check.sh
# 用法: bash scripts/cluster_health_check.sh [node1_addr] [node2_addr] ...

NODES=("${@:-imboy@node1.imboy.internal imboy@node2.imboy.internal}")
PASS=0
FAIL=0

for NODE in "${NODES[@]}"; do
    STATUS=$(IMBOY_CTL_NODE="$NODE" IMBOY_CTL_TIMEOUT=5 \
             escript scripts/imboy_ctl node status 2>&1)
    if echo "$STATUS" | grep -q "running"; then
        echo "[OK]   $NODE — running"
        ((PASS++))
    else
        echo "[FAIL] $NODE — $STATUS"
        ((FAIL++))
    fi
done

echo "---"
echo "Cluster: ${PASS} OK / ${FAIL} FAIL / $((PASS+FAIL)) total"
[ "$FAIL" -eq 0 ]  # 退出码 0=全部健康, 1=有节点异常
```

### 5.5 Prometheus 集群指标 / Prometheus Cluster Metrics

`deploy/prometheus/rules/imboy-alerts.yml` 中的 13 条 SLO 告警规则已覆盖集群关键指标。关键告警项：

Key alert rules in `deploy/prometheus/rules/imboy-alerts.yml`:

| 告警名 | 触发条件 | 说明 |
|--------|---------|------|
| `ImboyNodeDown` | 节点 ping 超时 > 30s | 节点不可达 |
| `ImboyHighErrorRate` | 5xx 错误率 > 1% | API 错误飙升 |
| `ImboyPgPoolExhausted` | pool available = 0 | 连接池耗尽 |
| `ImboyHighP99Latency` | p99 延迟 > 1s | 响应变慢 |
| `ImboyErlangVMHighMemory` | 进程内存 > 6GB | 内存泄漏风险 |

---

## 6. 常见问题
## 6. Troubleshooting

### 6.1 网络分区处理 / Network Partition Handling

**症状 / Symptom**: `nodes()` 返回空列表，或部分节点不可达，跨节点消息投递失败

```bash
# 步骤 1：确认网络连通性
ping node2.imboy.internal
telnet node2.imboy.internal 4369   # 测试 epmd 端口
nc -zv node2.imboy.internal 9100   # 测试 Erlang 分布式端口

# 步骤 2：检查 epmd 注册状态
epmd -names    # 列出本机已注册的 Erlang 节点
# 预期输出类似:
# epmd: up and running on port 4369 with data:
# name imboy at port 9150

# 步骤 3：在 console 中手动重连节点
_rel/imboy/bin/imboy remote_console
> net_adm:ping('imboy@node2.imboy.internal').
% pong = 成功重连; pang = 无法到达

% 若 ping 失败，检查 cookie 是否一致
> auth:get_cookie().
% 与目标节点的 cookie 比较

> q().

# 步骤 4：检查防火墙规则
iptables -L INPUT -n | grep -E "4369|9100|9200"
```

**恢复策略**：
- 网络闪断（< 30s）：Erlang 会自动重连，`syn` 会重新同步注册表。
- 网络长时间中断（> 60s）：节点可能进入脑裂（split-brain）。重新加入集群时，`syn` 使用 `coalesce` 策略合并注册表（先到先得）。
- 数据一致性：IMBoy 通过 PostgreSQL（共享单一真相源）保证持久化数据一致，内存中的会话状态（`syn`）重新同步后恢复。

**Recovery strategy**:
- Short network blip (< 30s): Erlang auto-reconnects; `syn` re-syncs its registry.
- Extended partition (> 60s): Nodes may enter split-brain. On rejoin, `syn` uses `coalesce` to merge registries (first-wins).
- Data consistency: IMBoy uses PostgreSQL as single source of truth for persistent data. In-memory session state (`syn`) recovers after re-sync.

### 6.2 节点重连 / Node Reconnection

**症状 / Symptom**: 节点重启后未自动加入集群

```bash
# 方式 1：手动触发重连（不重启）
_rel/imboy/bin/imboy remote_console
> [net_kernel:connect_node(N) || N <- ['imboy@node2.imboy.internal', 'imboy@node3.imboy.internal']].

% 确认已连接
> nodes().

> q().

# 方式 2：重启节点（最简单）
_rel/imboy/bin/imboy restart

# sys.config 中设置 cluster_nodes 后，imboy_app:start/2 会在启动时自动尝试连接
# 确认 cluster_nodes 配置正确：
grep cluster_nodes config/sys.config
```

### 6.3 epmd 端口占用 / epmd Port Conflict

**症状 / Symptom**: 节点启动失败，日志出现 `{error,epmd_error}`

```bash
# 检查 epmd 是否运行
epmd -names
# 若报错，手动启动：
epmd -daemon

# 检查 4369 端口占用
lsof -i :4369
ss -tlnp | grep 4369

# 若被其他进程占用：
kill -9 <PID>
epmd -daemon
```

### 6.4 syn 注册表不同步 / syn Registry Desync

**症状 / Symptom**: 跨节点消息无法投递，节点 1 上找不到节点 2 上的 WS 会话

```bash
_rel/imboy/bin/imboy remote_console

% 检查 syn 是否认为集群已连通
> syn:supervisor_info().

% 检查本节点注册了多少会话
> syn:get_local_member_count(ws_session_group).

% 强制触发 syn 全局重新发现
> syn:leave_group(ws_session_group, all).   % 注意：会导致当前节点所有 WS 会话掉线，谨慎执行

% 若只是查看状态，不需要上面的 leave_group
% 检查特定 UID 的会话在哪个节点
> syn:whereis_name({uid, 1000000051}).

> q().
```

### 6.5 大集群启动顺序 / Large Cluster Startup Order

多节点部署时，推荐按以下顺序启动以避免竞争条件：

For multi-node deployments, start nodes in this order to avoid race conditions:

1. 启动 PostgreSQL（所有节点共用）
2. 启动节点 1（`node1`）— 作为"种子节点"（seed node）
3. 等待节点 1 完成 DB 迁移并进入 `running` 状态
4. 依次启动节点 2、3（它们连接到已稳定的节点 1，`syn` 自动同步）

```bash
# 集群启动脚本示例
# scripts/cluster_start.sh

NODES=("imboy@node1.imboy.internal" "imboy@node2.imboy.internal" "imboy@node3.imboy.internal")

for i in "${!NODES[@]}"; do
    NODE="${NODES[$i]}"
    HOST="${NODE#*@}"

    echo "Starting $NODE..."
    ssh "$HOST" "_rel/imboy/bin/imboy start"

    # 等待节点就绪（最多 60 秒）
    for attempt in $(seq 1 12); do
        sleep 5
        if IMBOY_CTL_NODE="$NODE" escript scripts/imboy_ctl node status 2>/dev/null | grep -q "running"; then
            echo "  ✓ $NODE is running"
            break
        fi
        echo "  ... waiting ($attempt/12)"
    done
done

echo "All nodes started. Verifying cluster..."
IMBOY_CTL_NODE="${NODES[0]}" escript scripts/imboy_ctl node list
```

---

## 附录：集群配置速查 / Appendix: Cluster Config Quick Reference

```erlang
%% sys.config 集群相关配置汇总
%% (每节点的值用注释标出差异项)

{imboy, [
    %% ---- 集群节点列表（所有节点相同，填入全部节点名）----
    {cluster_nodes, [
        'imboy@node1.imboy.internal',
        'imboy@node2.imboy.internal',
        'imboy@node3.imboy.internal'
    ]},

    %% ---- 分布式 ID（每节点唯一）----
    {tsid_dc_id,   1},           %% 每 DC 统一，DC 间不同
    {tsid_node_id, 1},           %% 每节点唯一：node1=1, node2=2, node3=3
    {tsid_dc_bits, 3},           %% 全集群统一，不可变更

    %% ---- 分布式缓存同步（集群必须 true）----
    {dsync_enabled, true},

    %% ---- 连接池（每节点独立，容量 × 节点数 = 总连接数）----
    {pg_conf, #{
        name       => pgsql,
        max_count  => 80,
        init_count => 5,
        ...
    }}
]},

%% ---- Erlang 分布式端口范围（kernel 段）----
{kernel, [
    {inet_dist_listen_min, 9100},
    {inet_dist_listen_max, 9200}
]}
```

```bash
# 环境变量覆盖清单（多节点部署关键变量）
IMBOY_TSID_DC_ID=1            # 数据中心 ID
IMBOY_TSID_NODE_ID=2          # 节点 ID（每节点不同）
CLUSTER_NODES=imboy@node1.imboy.internal,imboy@node2.imboy.internal
IMBOY_CTL_NODE=imboy@node1.imboy.internal
IMBOY_CTL_COOKIE=<erlang_cookie>
IMBOY_CTL_TIMEOUT=10
```
