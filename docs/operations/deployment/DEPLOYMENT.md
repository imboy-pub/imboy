# IMBoy 生产部署指南

> - **自动化部署脚本**（推荐）：[DEPLOY-SCRIPT.md](./DEPLOY-SCRIPT.md) — 配置一次 `.env.deploy`，一条命令完成全量或增量部署
> - **5 分钟快速上手**：[DAY1-QUICKSTART.md](./DAY1-QUICKSTART.md) — 从零搭建最小生产环境
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

# 执行迁移（按编号顺序）
for f in priv/migrations/0000*.sql; do
    echo "执行: $f"
    sudo -u postgres psql -d imboy -f "$f"
done
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

# Prometheus 指标
curl -s -H "Accept: text/plain" http://localhost:9800/metrics

# Erlang shell 检查
_rel/imboy/bin/imboy remote_console
> pooler:pool_stats(pgsql).  % 连接池状态
> syn:count(imboy).          % 在线用户数
> observer_cli:start().       % 系统监控
```

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
2. 恢复到指定时间点（参见 BACKUP-RESTORE.md）

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
# 节点 1
make start node=node1 port=9801 cookie=imboycookie

# 节点 2
make start node=node2 port=9802 cookie=imboycookie

# 在 node2 shell 中加入集群
net_adm:ping('imboy_node1@hostname').
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
