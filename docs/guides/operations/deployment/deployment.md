# IMBoy 生产部署指南

> - **自动化部署脚本**（推荐）：[deploy-script.md](./deploy-script.md) — 配置一次 `.env.deploy`，一条命令完成全量或增量部署
> - **5 分钟快速上手**：[day1-quickstart.md](./day1-quickstart.md) — 从零搭建最小生产环境
> - **LiveKit 通话运行手册**：[livekit-single-service-runbook.md](./livekit-single-service-runbook.md) — rtc WSS + SFU + embedded TURN 的部署/验证/升级/回滚（小白可逐行执行）
> - **生产架构图**：[production-architecture.md](./production-architecture.md) — 服务分布与端口一览
>
> 本文是完整参考手册，适合定制化配置场景。

## 环境要求

| 组件 | 最低版本 | 推荐版本 |
|------|---------|---------|
| Erlang/OTP | 28+ | 28.0 |
| PostgreSQL | 18+ | 18.x |
| OS | Ubuntu 22.04+ / CentOS 8+ | Ubuntu 24.04 |
| 内存 | 4GB | 8GB+ |
| CPU | 2 核 | 8 核+ |
| 磁盘 | 20GB SSD | 100GB+ SSD |

### PostgreSQL 扩展

```sql
CREATE EXTENSION IF NOT EXISTS pg_jieba;
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS timescaledb;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS pg_trgm;
```

---

## 环境变量配置

### 必需

| 变量 | 说明 | 示例 |
|------|------|------|
| `IMBOYENV` | 运行环境 | `pro` / `dev` / `local` |
| `IMBOY_JWT_KEY` | JWT 签名密钥 (32 字节 binary) | `openssl rand -base64 32` |
| `IMBOY_POSTGRE_AES_KEY` | PostgreSQL 字段级 AES 密钥 (32 字节) | `openssl rand -base64 32` |
| `IMBOY_SOLIDIFIED_KEY` | 客户端 init 加密密钥 (32 字节) | `openssl rand -base64 32` |
| `IMBOY_SOLIDIFIED_KEY_IV` | 客户端 init AES IV (16 字节) | `openssl rand -base64 16` |
| `IMBOY_PASSWORD_SALT` | 历史 MD5 密码盐（投产后不可改） | `openssl rand -hex 16` |
| `IMBOY_LOGIN_RSA_PUB_KEY_FILE` | RSA 公钥 PEM 文件路径 | `/etc/imboy/keys/login_rsa_pub.pem` |
| `IMBOY_LOGIN_RSA_PRIV_KEY_FILE` | RSA 私钥 PEM 文件路径 | `/etc/imboy/keys/login_rsa_priv.pem` |
| `IMBOY_API_AUTH_SWITCH` | API 签名验证开关（生产必须 `on`） | `on` |
| `IMBOY_PG_HOST` | 数据库主机 | `127.0.0.1` |
| `IMBOY_PG_PORT` | 数据库端口 | `5432` |
| `IMBOY_PG_DATABASE` | 数据库名 | `imboy` |
| `IMBOY_PG_USERNAME` | 数据库用户 | `imboy_app` |
| `IMBOY_PG_PASSWORD` | 数据库密码 | (强密码) |
| `IMBOY_ADM_COOKIE_SECRET` | 管理后台 Cookie 签名密钥 | `openssl rand -hex 32` |

### 可选

| 变量 | 说明 | 默认值 |
|------|------|--------|
| `HTTP_PORT` | HTTP 服务端口 | `9800` |
| `IMBOY_PG_MAX_COUNT` | 连接池最大连接数 | `80` |
| `IMBOY_PG_INIT_COUNT` | 连接池初始连接数 | `5` |

### LiveKit 通话（RTC）

通话（1:1 与群通话）统一走 LiveKit Room，后端经 `/api/v1/rtc/room/join` 签发接入 token。Docker Compose 部署（`deploy/docker-compose.community.yml`）已内置 LiveKit 服务（`livekit/livekit-server:v1.13.7`，单节点、SFU + embedded STUN/TURN，无 Redis、无 Egress）：

| 变量 | 说明 | 示例 / 默认 |
|------|------|------|
| `RTC_DOMAIN` | LiveKit 信令域（wss:// 反代 7880，必填） | `rtc.example.com` |
| `TURN_DOMAIN` | LiveKit embedded TURN 域（必填，证书经 80 端口签发） | `turn.example.com` |
| `LIVEKIT_API_KEY` | LiveKit API key（install.sh 自动生成） | 随机字符串 |
| `LIVEKIT_API_SECRET` | LiveKit API secret（≥32 字符，与 key 配对） | `openssl rand -hex 24` |
| `LIVEKIT_TURN_ENABLED` | embedded TURN overlay 开关（`true` 需先退场旧 TURN，禁双活） | `false` |
| `LIVEKIT_TURN_CERT_DIR` | TURN TLS 证书目录（install.sh 自动展开） | `<DATA_DIR>/certbot/conf/live/<TURN_DOMAIN>` |
| `IMBOY_LIVEKIT_WS_URL` | 客户端信令地址覆盖（一般无需设置） | `wss://<RTC_DOMAIN>` |

> 裸机部署需在 `config/sys*.config` 的 `{livekit, ...}` 段配置 `ws_url` / `api_key` / `api_secret`（或用 `IMBOY_LIVEKIT_*` 环境变量覆盖）；任一键为空时通话入会返回受控错误 `livekit_not_configured`（不会崩溃为 500）。
> 端口、容量上限、证书续期与回滚的完整操作见 [livekit-single-service-runbook.md](./livekit-single-service-runbook.md)。
> 旧 eturnal/coturn TURN 链路（含 `/api/v1/user/credential` 端点）已删除，不再是部署依赖。

---

## 部署步骤

### 1. 数据库准备

```bash
# 创建数据库和用户
sudo -u postgres psql <<EOF
CREATE DATABASE imboy;
CREATE ROLE imboy_app LOGIN PASSWORD 'YOUR_STRONG_PASSWORD';
GRANT CONNECT ON DATABASE imboy TO imboy_app;
EOF

# 安装扩展（需 superuser）
sudo -u postgres psql -d imboy <<EOF
CREATE EXTENSION IF NOT EXISTS pg_jieba;
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS timescaledb;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS pg_trgm;
EOF

# 执行迁移：无需手动跑 SQL —— 节点启动时 imboy_app 检测 auto_migrate=true（默认开启）
# 自动调用 imboy_migrate:migrate/0，按 8 位顺序编号执行 priv/migrations/*.up.sql
# （erlang_migrate strict 模式；.down.sql 仅回滚用，不会被自动执行；
# 历史记录在 schema_migrations_history 表，乱序迁移会报错而非被静默跳过）
```

### 2. 编译发布

```bash
# 编译
make compile

# 构建 release
IMBOYENV=pro make rel

# 验证构建产物
ls _rel/imboy/bin/imboy
```

### 3. 启动服务

```bash
# 前台运行（调试用）
_rel/imboy/bin/imboy foreground

# 后台运行
_rel/imboy/bin/imboy start

# 检查状态
_rel/imboy/bin/imboy ping
# 预期输出: pong

# 停止
_rel/imboy/bin/imboy stop
```

### 4. 健康检查

```bash
# HTTP 健康检查
curl -s http://localhost:9800/api/v1/init | jq .

# 健康探针端点（语义见 src/api/healthz_handler.erl）
curl -s http://localhost:9800/livez | jq .    # liveness：BEAM 活着即恒 200，不检查任何依赖
curl -s http://localhost:9800/readyz | jq .   # readiness：聚合 PG + TSID guard，就绪 200 / 依赖不可用 503
curl -s http://localhost:9800/healthz | jq .  # /readyz 的兼容别名（保留既有响应字段并追加 tsid）

# Prometheus 指标
curl -s -H "Accept: text/plain" http://localhost:9800/metrics

# Erlang shell 检查
_rel/imboy/bin/imboy remote_console
> pooler:pool_stats(pgsql).  % 连接池状态
> syn:count(imboy).          % 在线用户数
> observer_cli:start().       % 系统监控
```

> - `/healthz` 公网匿名可达：外网请求 `version` 恒报 `"hidden"`，仅内网（如部署探活走 127.0.0.1）报真实 vsn，避免替匿名者做 CVE 版本匹配。
> - 容器 healthcheck 真源在后端 `Dockerfile`（约 135 行）：`bin/imboy ping` + `bin/imboy eval` 调 `healthz_handler:probe_db()`（运行镜像 debian-slim 无 curl/wget，不走 HTTP 探测）。

---

## 回滚流程

### 快速回滚

```bash
# 停止当前版本
_rel/imboy/bin/imboy stop

# 切换到上一版本目录
cd /opt/imboy/previous

# 启动上一版本
_rel/imboy/bin/imboy start
```

### 数据库回滚

迁移脚本设计为向前兼容，通常不需要回滚。如果需要：

1. 备份当前数据：`bash scripts/backup_pg.sh --full`
2. 恢复到指定时间点（参见 backup-restore.md）

---

## SSL/TLS 配置

```erlang
% config/sys.config
{start_mode, https}
, {cacertfile, "/ssl/chain.csr"}
, {certfile, "/ssl/public.crt"}
, {keyfile, "/ssl/server.key"}
```

---

## 多节点集群

```bash
# 节点 1（用法: start_node.sh <nodename> [cookie] [port] [exclude_apps] [daemon]，需先 make rel）
./scripts/start_node.sh node1 <your-cookie> 9801

# 节点 2
./scripts/start_node.sh node2 <your-cookie> 9802

# 停止节点
./scripts/stop_node.sh node2

# 在 node2 shell 中加入集群（节点 host 默认 127.0.0.1，可用 IMBOY_NODE_HOST 覆盖）
net_adm:ping('node1@127.0.0.1').
```

---

## 运维命令速查

| 命令 | 说明 |
|------|------|
| `_rel/imboy/bin/imboy start` | 后台启动 |
| `_rel/imboy/bin/imboy stop` | 停止 |
| `_rel/imboy/bin/imboy ping` | 健康检查 |
| `_rel/imboy/bin/imboy remote_console` | 远程 shell |
| `lm()` | 热加载所有修改的模块 |
| `config_ds:local_reload()` | 重新加载配置 |
| `observer_cli:start()` | 命令行监控 |
| `pooler:pool_stats(pgsql)` | 连接池状态 |
