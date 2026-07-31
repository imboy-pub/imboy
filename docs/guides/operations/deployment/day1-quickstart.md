# Day-1 部署快速上手 / Day-1 Deployment Quickstart

> ⚠️ **先选对路径 / Pick the right path first**
>
> **要用 Docker Compose 部署（绝大多数人）→ 不要看本文**，直接：
> ```bash
> cd deploy && bash install.sh
> ```
> 见 [`deploy/README.md`](../../../../deploy/README.md)。那条路径的密钥、RSA 密钥对与
> TLS 证书全部自动生成，只需人工填两个域名 + 一个邮箱。
>
> **本文是裸机 / release 包部署路径**：变量名带 `IMBOY_` 前缀、密钥路径形如
> `/etc/imboy/keys/`，与 compose 路径的 `.env` 字段名和挂载点**不通用**，照抄会配错。
>
> If you deploy with Docker Compose (most people), use `deploy/install.sh` instead —
> this document covers the bare-metal / release-package path and its variable names
> are **not** interchangeable with the compose `.env`.

> 简体中文 / English — bilingual
> 适用版本 / Applies to: imboy v1.0.0-rc.1+
> 目标 / Goal: 5 分钟内跑通生产部署的最小可用配置 / Stand up a minimal production deployment in 5 minutes.
> 完整参考手册 / Full manual: [deployment.md](./deployment.md) ｜ 自动化脚本 / Script: [deploy-script.md](./deploy-script.md)

---

## 设计原则 / Design Principles

中文：

1. **真相源唯一** — 所有运行时配置只有 3 个来源：
   - `config/sys.config` — 业务参数与端口默认值（Section A/B）
   - `IMBOY_*` 环境变量 — 部署期密钥与连接信息（Section C）
   - `IMBOYENV` 环境变量 — 运行环境标识（local / dev / prod）
2. **生产 fail-fast** — 缺密钥就拒绝启动，不存在「上线后才发现没设」。
3. **Docker 单文件** — 一个 `docker-compose.yml` + profile（dev / pro / turn），不再三件套。

English:

1. **Single source of truth** — runtime config has only 3 inputs:
   - `config/sys.config` — business tunables & port defaults (Section A/B)
   - `IMBOY_*` env vars — deploy-time secrets and connection info (Section C)
   - `IMBOYENV` env var — runtime tag (local / dev / prod)
2. **Fail-fast in prod** — missing secrets refuse to boot; never discover after go-live.
3. **One docker-compose file** — single `docker-compose.yml` with profiles (dev / pro / turn) instead of three separate files.

---

## 1. 准备 .env / Prepare .env

中文：复制 `.env.example` 为 `.env`，填入真实值。

English: Copy `.env.example` to `.env` and fill in real values.

```bash
cp .env.example .env
$EDITOR .env
```

---

## 2. 必填环境变量 / Required environment variables

| 变量 / Variable | 说明 / Purpose | 生成方式 / Generate |
|---|---|---|
| `IMBOYENV` | 运行环境 / Runtime tag | `prod` |
| `IMBOY_JWT_KEY` | JWT 签名密钥 (32B) / JWT signing key | `openssl rand -base64 32` |
| `IMBOY_POSTGRE_AES_KEY` | PG 字段 AES 密钥 (32B) / PG field AES key | `openssl rand -base64 32` |
| `IMBOY_ADM_COOKIE_SECRET` | 管理后台 Cookie 签名密钥 / Admin cookie signing | `openssl rand -hex 32` |
| `IMBOY_SOLIDIFIED_KEY` | 客户端 init 加密密钥 (32B) / Client init AES key | `openssl rand -base64 32` |
| `IMBOY_SOLIDIFIED_KEY_IV` | 客户端 init AES IV (16B) / Client init AES IV | `openssl rand -base64 16` |
| `IMBOY_PASSWORD_SALT` | 旧密码 MD5 盐 (投产后不可改) / Legacy password salt (immutable after go-live) | `openssl rand -hex 16` |
| `IMBOY_LOGIN_RSA_PUB_KEY_FILE` | RSA 公钥 PEM 路径 / RSA pub key PEM path | `/etc/imboy/keys/login_rsa_pub.pem` |
| `IMBOY_LOGIN_RSA_PRIV_KEY_FILE` | RSA 私钥 PEM 路径 / RSA priv key PEM path | `/etc/imboy/keys/login_rsa_priv.pem` |
| `IMBOY_PG_HOST` | 数据库主机 / DB host | `127.0.0.1` |
| `IMBOY_PG_PORT` | 数据库端口 / DB port | `5432` |
| `IMBOY_PG_DATABASE` | 数据库名 / DB name | `imboy_v1` |
| `IMBOY_PG_USERNAME` | 数据库用户 / DB user | `imboy_user` |
| `IMBOY_PG_PASSWORD` | 数据库密码 / DB password | (强密码 / strong) |
| `IMBOY_API_AUTH_SWITCH` | API 签名验证开关 / API sig verify switch | `on` |

> 中文：以上任一缺失，`make rel` 跑出来的 `imboy` 启动会立刻 `error({missing_required_config, ...})`，不会带病上线。
> English: If any of the above is missing, the release built by `make rel` will immediately raise `error({missing_required_config, ...})` on boot — never goes live in a broken state.

---

## 3. 条件必填 / Conditionally required

| 触发条件 / Trigger | 必填变量 / Required when triggered |
|---|---|
| `eturnal_turn_urls` 非空 / non-empty | `IMBOY_ETURNAL_SECRET` |
| `push.enabled = true` (sys.config) | `IMBOY_JPUSH_APP_KEY`, `IMBOY_JPUSH_MASTER_SECRET` |
| `sms.switch = <<"on">>` & `platform = <<"yjsms">>` | `IMBOY_YJSMS_ACCOUNT`, `IMBOY_YJSMS_SECRET` |
| `sms.switch = <<"on">>` & `platform = <<"aliyun">>` | sys.config 中 aliyun.key_id / key_secret 不可为空 |

---

## 4. 生成 RSA 密钥 / Generate RSA keys

```bash
sudo mkdir -p /etc/imboy/keys
sudo openssl genrsa -out /etc/imboy/keys/login_rsa_priv.pem 2048
sudo openssl rsa   -in  /etc/imboy/keys/login_rsa_priv.pem \
                   -pubout -out /etc/imboy/keys/login_rsa_pub.pem
sudo chmod 600 /etc/imboy/keys/login_rsa_priv.pem
sudo chmod 644 /etc/imboy/keys/login_rsa_pub.pem
sudo chown -R imboy:imboy /etc/imboy/keys
```

---

## 5. 启动 PostgreSQL / Start PostgreSQL

中文：使用统一的 `docker-compose.yml`，按 profile 选择 dev 或 pro 实例。

English: Use the unified `docker-compose.yml` and pick `dev` or `pro` profile.

```bash
# 开发 / Development (port 4323)
docker compose --profile dev up -d

# 生产 / Production (port 5181)
docker compose --profile pro up -d

# TURN/STUN（如需）/ Optional WebRTC TURN
docker compose --profile turn up -d

# 同时多实例 / Combined
docker compose --profile dev --profile pro --profile turn up -d
```

> 中文：未设置 `IMBOY_PG_PASSWORD` 时 docker compose 会立刻报错并拒绝启动，不会留下默认密码风险。
> English: With `IMBOY_PG_PASSWORD` unset, `docker compose` errors out immediately — no default-password risk.

---

## 6. 构建并发布 / Build & release

```bash
# 构建 release tarball / Build release tarball
make

# 解压并启动 / Extract and start
tar xf _rel/imboy/imboy-1.0.0-rc.1.tar.gz -C /opt/imboy
cd /opt/imboy && IMBOYENV=prod ./bin/imboy daemon
```

> 中文：版本号通过 `RELX_REL_VSN=x.y.z make rel` 临时覆盖，不需要改 `relx.config`。
> English: Override version with `RELX_REL_VSN=x.y.z make rel` without editing `relx.config`.

---

## 7. 健康检查 / Health check

```bash
# Erlang 节点 ping / Ping Erlang node
/opt/imboy/bin/imboy ping

# HTTP 心跳 / HTTP healthcheck
curl -fsS http://127.0.0.1:9800/api/v1/healthz
```

---

## 蓝绿切换 / Blue-green switch

中文：备机 9801，前置 nginx/七层 LB 改 upstream 后 reload，旧版 9800 静置 5 分钟回收。
English: Standby on 9801; flip nginx/L7 LB upstream and reload, then drain old 9800 for ~5 min.

```nginx
upstream imboy {
    server 127.0.0.1:9800;  # 切换时改成 9801 / swap to 9801 on flip
}
```

---

## 故障排查 / Troubleshooting

| 现象 / Symptom | 原因 / Cause | 处理 / Fix |
|---|---|---|
| `error({missing_required_config, jwt_key})` | `IMBOY_JWT_KEY` 未设置 / unset | 设置环境变量后重启 / set env and restart |
| `insecure_pg_password` | 数据库使用了弱密码（如 `password`、`123456`、空）/ weak DB password | 改强密码并 rotate / rotate to strong password |
| `insecure_config: api_auth_switch` | 生产未开启 API 签名 / API sig disabled in prod | `IMBOY_API_AUTH_SWITCH=on` |
| `eturnal_secret is empty` | 配了 TURN URL 但没 secret / TURN URL set without secret | `IMBOY_ETURNAL_SECRET=<secret>` |
| `solidified_key not configured` (warning) | dev/local 使用稳定默认值 / stable dev default | 生产请显式设置 `IMBOY_SOLIDIFIED_KEY*` / set explicitly in prod |
| `login_rsa_*_key_file not configured` (warning) | dev/local 落盘到 `priv/dev_keys/` / persisted to priv/dev_keys | 生产请配置 `IMBOY_LOGIN_RSA_*_FILE` 指向 release 外路径 / point to path outside release |

---

## 参考 / References

- 完整生产部署指南 / Full production guide: [`deployment.md`](./deployment.md)
- 备份与恢复 / Backup & restore: [`backup-restore.md`](./backup-restore.md)
- 监控 / Monitoring: [`monitoring.md`](./monitoring.md)
- 环境变量映射源码 / Env var mapping source: `src/lib/imboy_env.erl`
- Fail-fast 校验源码 / Fail-fast validation source: `src/imboy_app.erl: validate_runtime_config/0`
