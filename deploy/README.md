# IMBoy 生产部署包 / Production Deployment Package

一键将 IMBoy 部署到一台 Linux 服务器。
One-command deployment of IMBoy to a Linux server.

## 交付清单 / Deliverables

```
deploy/
├── docker-compose.prod.yml      # 7 服务编排：pg18 + backend + admin + nginx + certbot + prometheus + grafana
│                                # 7-service orchestration: pg18 + backend + admin + nginx + certbot + prometheus + grafana
├── .env.example                 # 环境变量模板 / Environment variables template
├── nginx/
│   ├── templates/
│   │   └── imboy.conf.template  # nginx 反向代理（envsubst 渲染）/ nginx reverse proxy (envsubst-rendered)
│   └── init-letsencrypt.sh      # 首次签发 Let's Encrypt 证书 / First-time Let's Encrypt issuance
├── prometheus/
│   ├── prometheus.yml           # 抓取配置（3 job）/ Scrape config (3 jobs)
│   └── rules/                   # 告警规则目录（见 imboy-alerts.yml）/ Alert rules
├── grafana/
│   ├── provisioning/            # 自动装配 datasource + dashboard provider
│   │   ├── datasources/         # Auto-provision Prometheus datasource
│   │   └── dashboards/          # Auto-provision dashboard folder
│   └── dashboards/
│       └── imboy-overview.json  # 9 panel 总览面板 / 9-panel overview dashboard
└── README.md                    # 本文件 / This file
```

## 前置条件

- Linux x86_64（推荐 Ubuntu 22.04 / Debian 12 / Alma 9）
- 内存 ≥ 8 GB，磁盘 ≥ 20 GB（生产建议 ≥ 32 GB 内存 / ≥ 100 GB 盘）
- Docker 24+ 与 `docker compose` 插件
- 已解析到本机的两个域名：`api.example.com`、`admin.example.com`
- 80 / 443 端口可公网访问（certbot 通过 Let's Encrypt HTTP-01 签发）

## 五步部署

### 1. 前置检查

```bash
cd /path/to/imboy/deploy
bash preflight.sh --docker
```

任何 `ERROR` 都必须修复后再继续。

### 2. 准备配置

```bash
cd deploy
cp .env.example .env
$EDITOR .env
```

**必须修改的字段 / Required fields to change:**

| 变量 / Variable | 说明 / Description |
|------|------|
| `API_DOMAIN` | 后端 API + WS 域名 / Backend API + WebSocket domain |
| `ADMIN_DOMAIN` | 管理后台域名 / Admin console domain |
| `POSTGRES_PASSWORD` | 强口令，至少 16 位 / Strong password, 16+ chars |
| `JWT_KEY` | 32 字节随机：`openssl rand -hex 16` |
| `POSTGRE_AES_KEY` | 32 字节随机：`openssl rand -hex 16` |
| `ADM_COOKIE_SECRET` | 32 字节随机 / 32-byte random |
| `GRAFANA_ADMIN_PASSWORD` | Grafana 仪表盘登录密码 / Grafana dashboard password |
| `CERTBOT_EMAIL` | Let's Encrypt 账号邮箱（证书到期提醒）/ Let's Encrypt account email |
| `SENTRY_DSN` | 可选，生产错误监控 / Optional, production error monitoring |

### 3. 创建网络 & 启动

```bash
docker network create imboy-network 2>/dev/null || true
docker compose -f docker-compose.prod.yml up -d
```

**首次部署需一次性签发 TLS 证书**（域名 A 记录须先指向本机，80 端口可公网访问）：

```bash
bash nginx/init-letsencrypt.sh
```

签发成功后，`imboy_certbot` 会在后台自动续期，nginx 定期 reload 加载新证书，无需再手动执行。

### 4. 查看启动状态

```bash
docker compose -f docker-compose.prod.yml ps
docker compose -f docker-compose.prod.yml logs -f imboy_backend
```

等待看到 `imboy started on port 9800` 字样。首次启动需要等待 PG 健康检查 + DB 迁移，约 30-60 秒。

### 5. 首启初始化向导（P0-5）

> 1.0.0-rc.1 起不再需要 erl shell 手工建号。首次访问管理后台会自动跳转 `/setup` 向导。

1. 浏览器打开 `https://admin.example.com`
2. 系统检测到未初始化，自动重定向到 `https://admin.example.com/setup`
3. 填写：
   - **账号**：手机号（`1[3-9]\d{9}`）或邮箱（二选一）
   - **昵称**：1-80 字符
   - **密码**：8-64 位，必须同时包含字母与数字
4. 提交后系统创建 `role_id=1` 超级管理员，密码经 HMAC-SHA512 加盐存储
5. 向导自动跳回 `/login`，使用刚创建的账号密码登录

**安全特性**：
- 向导仅允许成功执行一次，成功后在 `config` 表持久化 `adm.setup.completed_at` 标志
- 双重防线：配置 flag + `adm_user` 表存在性校验，任一命中即视为已初始化
- 两个路由（`/adm/setup/status` / `/adm/setup/init`）加入免鉴权白名单，初始化完成后重复调用会被后端拒绝（`ERR_SETUP_ALREADY_COMPLETED`）

**忘记密码或误操作**：直接在 PG 中 `DELETE FROM config WHERE key='adm.setup.completed_at'` 并清空 `adm_user` 表后即可重新触发向导。生产环境建议改用 `adm_passport_handler` 的密码重置流程。

## 运维

### 升级

```bash
cd deploy
# 1) 拉新镜像
docker compose -f docker-compose.prod.yml pull
# 2) 滚动更新（PG 不动，只重启 backend/admin）
docker compose -f docker-compose.prod.yml up -d imboy_backend imboy_admin
# 3) 查日志确认迁移成功
docker compose -f docker-compose.prod.yml logs -f imboy_backend
```

### 备份

参见 `imboy/docs/operations/deployment/BACKUP-RESTORE.md`。简化版：

```bash
# PG 逻辑备份
docker exec imboy_pg18 pg_dump -U imboy_user -Fc imboy_pro > backup_$(date +%F).dump
```

### 停止 / 启动 / 销毁

```bash
# 停止（保留数据）
docker compose -f docker-compose.prod.yml stop

# 启动
docker compose -f docker-compose.prod.yml start

# 销毁容器（保留数据卷）
docker compose -f docker-compose.prod.yml down

# ⚠️ 销毁一切包括数据（危险）
docker compose -f docker-compose.prod.yml down -v
rm -rf ./data
```

## 数据目录布局

```
deploy/data/
├── pg18/           # PostgreSQL 数据
├── backend_log/    # imboy 后端日志
├── backend_priv/   # 运行时私有文件（证书等）
└── certbot/
    ├── conf/       # Let's Encrypt 证书 & ACME 账号状态（/etc/letsencrypt）
    └── www/        # HTTP-01 challenge webroot（/var/www/certbot）
```

生产建议将 `data/` 放到独立挂载点（SSD），并做快照 + 异地备份。

## 可观测性 / Observability

`docker compose up -d` 完成后，prometheus 和 grafana 随核心服务同时启动。
Prometheus and Grafana start together with the core services after `docker compose up -d`.

### Grafana 访问 / Grafana Access

```
http://<server-ip>:3000
用户名 / username: admin
密码 / password: <GRAFANA_ADMIN_PASSWORD>
```

首次登录后在侧边栏 **Dashboards → IMBoy** 文件夹中可直接看到 "IMBoy Overview" 面板，无需手动导入。
On first login, the "IMBoy Overview" dashboard is available in the **Dashboards → IMBoy** folder — no manual import needed.

### 包含的指标面板 / Included dashboard panels

| 面板 / Panel | 指标 / Metric |
|---|---|
| Backend Up | `up{job="imboy_backend"}` |
| WS 连接数 / WS connections | `imboy_ws_connections_total` |
| HTTP 请求速率 / HTTP req rate | `rate(imboy_http_requests_total[5m])` |
| 消息投递 p50/p95/p99 / Msg delivery latency | `histogram_quantile(0.99, ...)` |
| Erlang VM 内存 / Erlang VM memory | `erlang_vm_memory_bytes_total` |
| PG 事务速率 / PG transaction rate | `rate(pg_stat_database_xact_commit_total[5m])` |

### Prometheus 告警 / Alerting

告警规则位于 `deploy/prometheus/rules/imboy-alerts.yml`，随 Prometheus 启动时自动加载。
Alert rules live in `deploy/prometheus/rules/imboy-alerts.yml`, auto-loaded when Prometheus starts.

**8 条规则覆盖 / 8 rules covering：**

| 规则 / Rule | 触发条件 / Threshold | 级别 / Severity |
|---|---|---|
| `ImBoyBackendDown` | backend 离线 > 1min | critical |
| `ImBoyBackendRestarted` | uptime < 2min | warning |
| `ImBoyMsgDeliveryLatencyHigh` | p99 > 500ms 持续 5min | warning |
| `ImBoyMsgDeliveryLatencyCritical` | p99 > 2s 持续 2min | critical |
| `ImBoyHTTPErrorRateHigh` | 5xx > 1% 持续 5min | warning |
| `ImBoyHTTPErrorRateCritical` | 5xx > 5% 持续 2min | critical |
| `ImBoyErlangMemoryHigh` | VM 内存 > 6GB 持续 5min | warning |
| `ImBoyErlangMemoryCritical` | VM 内存 > 7.5GB 持续 2min | critical |
| `ImBoyErlangProcessCountHigh` | 进程数 > 200000 持续 5min | warning |
| `ImBoyPostgresExporterDown` | exporter 离线 > 2min | critical |
| `ImBoyPostgresHighRollbackRate` | 回滚率 > 5% 持续 5min | warning |
| `ImBoyMsgRateDrop` | 消息速率陡降 > 90% 持续 5min | warning |
| `ImBoyWSConnectionsDrop` | WS 连接数较 10min 前下降 > 50% | warning |

Prometheus 可通过 `http://<server-ip>:9090` 直接访问（建议仅内网暴露）。
Prometheus is accessible at `http://<server-ip>:9090` (recommended: restrict to internal network only).

---

## 故障排查 / Troubleshooting

| 现象 / Symptom | 排查 / Troubleshooting |
|------|------|
| `imboy_backend` 反复重启 / keeps restarting | `docker compose logs imboy_backend` 查看 PG 连接 / 配置 / 迁移冲突 / Check PG connection, config, migration conflicts |
| certbot 证书签发失败 / TLS cert fails | DNS 未指向本机 / 80 端口被占用 / Let's Encrypt 限流 / `CERTBOT_EMAIL` 未填；查 `docker compose logs imboy_certbot` / DNS not pointing here, port 80 blocked, LE rate-limit, missing CERTBOT_EMAIL |
| 管理后台 404 或白屏 / Admin 404 or blank | 检查 `VITE_API_BASE` 是否指向 `https://${API_DOMAIN}` / Check `VITE_API_BASE` |
| WebSocket 连接 403 / WS 403 | `JWT_KEY` 不一致 / nginx WS 升级（Upgrade/Connection）头未透传 / `JWT_KEY` mismatch, check nginx WebSocket upgrade headers |
| PG 启动失败 / PG fails to start | 扩展安装失败：`bash ../script/preflight.sh` / Extension install failed |
| Grafana 无数据 / Grafana no data | 检查 Prometheus target：`http://<ip>:9090/targets` / Check targets at `http://<ip>:9090/targets` |

## 下一步 / Next Steps

- ✅ G3 `scripts/sanity_check.sh` — 部署后 8 项自动验证 / Post-deploy 8-item sanity check _(done)_
- ✅ G5 `prometheus/rules/imboy-alerts.yml` — SLO 告警规则 / SLO alerting rules _(done)_
- ⏳ G1 `.github/workflows/release.yml` — 镜像构建发布自动化（依赖 commercialization-readiness C1）/ Image build-push automation _(pending — backend Dockerfile 已就绪，见 docs/release/RELEASE.md)_
- ⏳ G4 `.github/dependabot.yml` + Trivy SBOM 扫描 / Dependabot + Trivy SBOM scan _(pending)_
- ⏳ P0-8 Sentry DSN 生产注入文档化 / Sentry DSN production injection docs _(pending)_
