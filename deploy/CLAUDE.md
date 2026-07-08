> [imboy.pub 根目录](../CLAUDE.md) > **deploy（生产部署）**

# IMBoy Deploy - AI 上下文文档 / AI Context Document

> **最后更新 / Last updated**: 2026-05-28
> **职责 / Role**: 生产环境部署，包含 Docker Compose、Helm Chart、nginx 反向代理 + certbot TLS、可观测性栈

---

## 目录结构 / Directory Structure

```
deploy/
├── docker-compose.prod.yml      # 7 服务编排：pg18 + backend + admin + nginx + certbot + prometheus + grafana
├── .env.example                 # 环境变量模板（复制为 .env 后填写）
├── preflight.sh                 # 部署前置检查脚本
├── nginx/
│   ├── templates/
│   │   └── imboy.conf.template  # nginx 反向代理配置模板（envsubst 渲染）
│   └── init-letsencrypt.sh      # 首次部署一次性签发 Let's Encrypt 证书
├── prometheus/
│   ├── prometheus.yml           # 抓取配置（3 个 job）
│   └── rules/
│       └── imboy-alerts.yml     # 告警规则
├── grafana/
│   ├── provisioning/
│   │   ├── datasources/
│   │   │   └── prometheus.yml   # 自动装配 Prometheus 数据源
│   │   └── dashboards/
│   │       └── default.yml      # 自动装配 Dashboard Provider
│   └── dashboards/
│       └── imboy-overview.json  # 9-panel 总览面板
├── loki/
│   └── loki.yml                 # 日志聚合配置
├── promtail/
│   └── promtail.yml             # 日志采集 Agent 配置
└── helm/                        # Kubernetes Helm Chart
    ├── Chart.yaml
    ├── values.yaml              # 默认值
    ├── values.prod.yaml         # 生产覆盖值
    └── templates/
        ├── _helpers.tpl
        ├── NOTES.txt
        ├── secret.yaml
        ├── configmap.yaml
        ├── deployment-backend.yaml
        ├── deployment-admin.yaml
        ├── service-backend.yaml
        ├── service-admin.yaml
        ├── ingress.yaml
        └── hpa.yaml             # HorizontalPodAutoscaler
```

---

## 前置条件 / Prerequisites

- Linux x86_64（推荐 Ubuntu 22.04 / Debian 12 / Alma 9）
- 内存 ≥ 8 GB，磁盘 ≥ 20 GB（生产建议 ≥ 32 GB 内存 / ≥ 100 GB 盘）
- Docker 24+ 与 `docker compose` 插件
- 两个已解析到本机的域名：`api.example.com`、`admin.example.com`
- 80 / 443 端口可公网访问（certbot 通过 Let's Encrypt HTTP-01 签发）
- `.env` 中配置 `CERTBOT_EMAIL`（Let's Encrypt 账号邮箱，用于到期提醒）

---

## 常用命令 / Common Commands

### Docker Compose 部署

```bash
cd deploy

# 1. 前置检查（验证 Docker、端口、域名解析）
bash preflight.sh

# 2. 配置环境变量
cp .env.example .env
$EDITOR .env          # 填写 DB 密码、JWT_SECRET、域名等

# 3. 创建网络并启动
docker network create imboy-network
docker compose -f docker-compose.prod.yml up -d

# 3b. 首次部署：一次性签发 Let's Encrypt 证书（域名 A 记录须先指向本机）
bash nginx/init-letsencrypt.sh

# 4. 查看服务状态
docker compose -f docker-compose.prod.yml ps

# 5. 查看日志
docker compose -f docker-compose.prod.yml logs -f imboy_backend
docker compose -f docker-compose.prod.yml logs -f imboy_admin

# 6. 停止服务
docker compose -f docker-compose.prod.yml down
```

### Helm (Kubernetes) 部署

```bash
cd deploy/helm

# 安装
helm install imboy . -f values.prod.yaml --namespace imboy --create-namespace

# 升级
helm upgrade imboy . -f values.prod.yaml --namespace imboy

# 查看状态
helm status imboy --namespace imboy

# 卸载
helm uninstall imboy --namespace imboy
```

### 可观测性访问

| 服务 | 默认端口 | 说明 |
|------|---------|------|
| Prometheus | 9090 | 指标采集，3 个 scrape job |
| Grafana | 3000 | 可视化面板，默认 admin/admin（首次登录需改密） |
| Loki | 3100 | 日志聚合（通过 Grafana 查询） |

---

## 关键文件说明 / Key Files

| 文件 | 说明 |
|------|------|
| `docker-compose.prod.yml` | 生产环境 7 服务编排入口，禁止直接修改数据库密码（改 .env） |
| `.env.example` | 所有必填环境变量的模板，**不要提交含真实密钥的 .env** |
| `preflight.sh` | 部署前自动检查依赖、端口占用、域名解析 |
| `nginx/templates/imboy.conf.template` | nginx 反向代理规则（envsubst 渲染），后端 → `api.*`（含 WS 升级），管理后台 → `admin.*`；入口拦截 `/metrics` 与 `/api/v1/metrics` 返回 403 |
| `nginx/init-letsencrypt.sh` | 首次部署一次性向 Let's Encrypt 申请证书；后续由 `imboy_certbot` 自动续期 |
| `prometheus/rules/imboy-alerts.yml` | 告警规则（CPU/内存/连接数/消息积压等） |
| `grafana/dashboards/imboy-overview.json` | 9-panel 总览：QPS、延迟、WS 连接数、DB 连接池、错误率等 |
| `helm/values.prod.yaml` | 生产 Helm 覆盖值：副本数、资源限制、镜像 tag、Ingress 域名 |

---

## 注意事项 / Notes

- 不修改 `docker-compose.prod.yml` 中的服务名（其他配置文件硬引用了服务名）。
- 生产环境必须修改 `.env` 中的 `JWT_SECRET`、`DB_PASSWORD`、`ADMIN_SECRET`、`CERTBOT_EMAIL`，不得使用默认值。
- Grafana 首次启动后立即修改 `admin` 默认密码。
- Helm HPA (`hpa.yaml`) 默认配置最小 2 副本、最大 10 副本，根据实际负载调整。
