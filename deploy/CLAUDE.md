> [imboy.pub 根目录](../CLAUDE.md) > **deploy（生产部署）**

# deploy — AI 上下文文档 / AI Context Document

> **最后更新 / Last updated**: 2026-09-19（结构树与命令对齐 `deploy/README.md` 口径重排）
> 部署事实以 [deploy/README.md](./README.md) 为唯一权威；本文是 AI/新成员速览。

## 目录结构 / Structure

```
deploy/
├── install.sh                   # 一键部署入口（推荐）：--edition community|business（默认 community）
├── preflight.sh                 # 前置检查（--edition 按版本切换检查口径；.env 不存在时 exit 1）
├── docker-compose.community.yml # 社区版编排（随仓分发）：pg18/garage/backend/admin/nginx/certbot/livekit
│                                #   + 监控 profile（--profile monitoring）：prometheus/alertmanager/loki/promtail/grafana
├── docker-compose.prod.yml      # 商务版编排（不随仓分发，leeyisoft@qq.com 渠道获取后放入本目录）
├── docker-compose.demo.yml      # 最小两服务零配置评估栈
├── docker-compose.healthcheck.yml / observability.yml / alert-dingtalk.yml / uptrace.yml / sales-policy.yml
│                                # 商务版 overlay：按序 -f 叠加（就绪检查/观测组件/钉钉告警/Uptrace/无密钥策略）
├── .env.example / ops.env.example
├── nginx/                       # 反代模板（envsubst）+ init-letsencrypt.sh 首签
├── prometheus/                  # prometheus.yml（4 job）+ rules/imboy-alerts.yml（33 条规则 14 组）
├── alertmanager/                # 告警路由
├── grafana/                     # provisioning + dashboards/imboy-overview.json（9 panel）
├── loki/ · promtail/            # 日志聚合与采集
├── uptrace/                     # 可选 Uptrace overlay（默认关）
├── cron/                        # 定时任务配置
└── helm/                        # Kubernetes Helm Chart（实验性，见文末警示）
```

## 前置条件 / Prerequisites

- Linux x86_64（Debian 13 基准）
- Docker 24+ 与 Compose v2.23.1+（configs 内联需要）
- 核心栈内存 ≥ 4 GB、磁盘 ≥ 10 GB（建议 8 GB / 20 GB）；启用 Uptrace 后至少 8 GB / 20 GB（建议 16 GB / 40 GB）
- 双域名（API 与管理后台）解析到本机，80 / 443 公网可达（certbot HTTP-01）
- `.env` 配置 `CERTBOT_EMAIL`；`install.sh` 会幂等补齐内部密钥并生成 RSA 登录密钥对到 `data/backend_priv/keys/`

## 常用命令 / Common Commands

### 社区版（推荐）

```bash
cd deploy
cp .env.example .env && $EDITOR .env   # 填域名/邮箱/数据库口令
bash preflight.sh --edition community
bash install.sh --edition community
docker compose -f docker-compose.community.yml ps
# 监控栈：部署命令加 --profile monitoring（默认不启动）
```

### 商务版（手工路径）

```bash
# overlay 按序叠加：prod.yml → healthcheck.yml → observability.yml → alert-dingtalk.yml(可选)
docker compose -f docker-compose.prod.yml -f docker-compose.healthcheck.yml up -d
```

### Helm (Kubernetes)

```bash
cd deploy/helm
helm install imboy . -f values.prod.yaml --namespace imboy --create-namespace
helm upgrade imboy . -f values.prod.yaml --namespace imboy
helm uninstall imboy --namespace imboy
```

### 可观测性访问

| 服务 | 默认端口 | 说明 |
|------|---------|------|
| Prometheus | 9090 | 指标采集，4 个 scrape job |
| Grafana | 3000 | 可视化（首次登录立即改默认密码） |
| Loki | 3100 | 日志聚合（经 Grafana 查询） |

## 关键文件说明 / Key Files

| 文件 | 说明 |
|------|------|
| `docker-compose.community.yml` | 社区版编排入口；服务名被其他配置硬引用，禁止改名 |
| `install.sh` | 一键部署：幂等补齐密钥、打印 Release Identity 三元组（`IMBOY_VERSION`/`IMBOY_GIT_SHA`/镜像 digest） |
| `.env.example` | 必填变量模板；**不要提交含真实密钥的 .env** |
| `preflight.sh` | 依赖/端口/域名解析检查 |
| `nginx/templates/imboy.conf.template` | 反代规则（envsubst）：后端 → `api.*`（含 WS 升级），管理后台 → `admin.*`；入口拦截 `/metrics` 返回 403 |
| `prometheus/rules/imboy-alerts.yml` | 告警规则 33 条 14 组（可用性/延迟/HTTP/VM/PG/消息/WS/主机/连接池/备份/TLS/支付） |
| `grafana/dashboards/imboy-overview.json` | 9-panel 总览 |

## 注意事项 / Notes

- 生产必须修改 `.env` 中全部密钥类变量，不得使用默认值；社区版没有手工多步的必要（install.sh 全自动）。
- Grafana 首次启动后立即修改默认密码。
- 版本单一来源：`.env` 的 `IMBOY_VERSION`；升级 = 改版本 → pull → up -d（社区版默认 `auto_migrate=true` 自动迁移）。
- Helm **后端固定单副本**（`backend.replicaCount: 1`），后端 HPA 默认**关闭**。后端是有状态的 Erlang 分布式节点，跨 Pod 的 `syn` 进程注册与消息路由未经生产集群验证，多副本下消息投递不确定；集群水平扩展为路线图项，**不得对外承诺**。admin 是无状态静态前端，多副本安全，其 HPA 不受此限。详见 `helm/README.md` 顶部警示。
