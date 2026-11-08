> [imboy.pub 根目录](../CLAUDE.md) > **deploy（生产部署）**

# deploy — AI 上下文文档 / AI Context Document

> **最后更新 / Last updated**: 2026-11-08（安装配置要求与 `install.sh` / `preflight.sh` 对齐）
> 部署事实以 [deploy/README.md](./README.md) 为唯一权威；本文是 AI/新成员速览。

## 目录结构 / Structure

```
deploy/
├── install.sh                   # 一键部署入口（推荐）：--edition community|business（默认 community）
├── init-cleanup-db.sh            # 社区迁移后清理身份门禁；默认只验，--create 仅建缺失 LOGIN
├── cleanup-db/                  # bootstrap.sql（事务创建/前置检查）和 verify.sql（只读身份/权限验证）
├── preflight.sh                 # 前置检查（--edition 按版本切换检查口径；.env 不存在时 exit 1）
├── docker-compose.community.yml # 社区版编排（随仓分发）：pg18/garage/backend/admin/nginx/certbot/livekit
│                                #   + 监控 profile（--profile monitoring）：prometheus/alertmanager/loki/promtail/grafana
├── docker-compose.prod.yml      # 商务版编排（不随仓分发，leeyisoft@qq.com 渠道获取后放入本目录）
├── docker-compose.demo.yml      # 最小两服务零配置评估栈
├── docker-compose.healthcheck.yml / observability.yml / alert-dingtalk.yml / uptrace.yml / sales-policy.yml
│                                # 商务版 overlay：按序 -f 叠加（就绪检查/观测组件/钉钉告警/Uptrace/无密钥策略）
├── .env.example / ops.env.example
├── nginx/                       # 反代模板（envsubst）+ init-letsencrypt.sh 首签 + prod-vhosts/ 宿主机生产 vhost 快照
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
- 五个互不相同的纯域名（`API_DOMAIN` / `ADMIN_DOMAIN` / `CS_WIDGET_DOMAIN` / `RTC_DOMAIN` / `TURN_DOMAIN`）解析到部署入口，公网 TCP 80 可用于 certbot HTTP-01，443 可用于 HTTPS/WSS；RTC/TURN 媒体端口见部署手册。
- 按[客户配置表](./README.md#customer-input)填写 `CERTBOT_EMAIL`、`IMBOY_BASE_URL=https://<API_DOMAIN>`、独立的 `IMBOY_DIRECT_CLEANUP_PG_USER` 等；社区版显式 `--init-cleanup-db` 可用已有管理权限创建缺失 LOGIN，默认只验证，已有账号密码须与数据库一致。
- `.env` 已填写后，`install.sh` 幂等补齐内部密钥并生成 RSA 登录密钥对到 `<DATA_DIR>/backend_priv/keys/`；TLS 证书须满足 DNS 与 HTTP-01 条件才可签发。

## 常用命令 / Common Commands

### 社区版（推荐）

```bash
cd deploy
cp .env.example .env && chmod 600 .env
$EDITOR .env   # 按客户配置表填写；内部随机密钥由安装器补齐
bash install.sh --edition community --init-cleanup-db  # 全新实例显式初始化清理账号
docker compose -f docker-compose.community.yml ps
# 监控栈：部署命令加 --profile monitoring（默认不启动）
```

独立预检用 `bash preflight.sh --edition community --docker`；须先具备已初始化的内部密钥和 RSA 文件，刚复制模板时缺少这些值的错误是预期行为。`preflight.sh` 默认检查商务版，社区版必须显式指定 `--edition community`。

清理身份门禁在健康之后、sanity 之前运行：精确比对运行镜像的迁移清单，再从 backend 网络命名空间验证正反密码和 SET ROLE/权限。现有 `POSTGRES_USER` 必须具备创建与授权能力，脚本不提权、不改已有账号；商务/外部数据库与失败恢复按[清理账号手册](./standalone/docs/DB_CLEANUP_ACCOUNT.md)。已就绪服务可用 `bash init-cleanup-db.sh` 单独复验，无需宿主机 Python/psql。

### 商务版

```bash
# 先取得专有 docker-compose.prod.yml，并按客户配置表补齐商务版条件字段
# docker-compose-sales-policy.yml 随仓分发，由安装器自动叠加
bash install.sh --edition business
```

手工 Compose 路径及 overlay 顺序见 [部署手册](./README.md)。

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
| `init-cleanup-db.sh` / `cleanup-db/*.sql` | 社区数据库身份门禁；显式创建缺失账号、事务回滚、已有账号只验证；DBA 流程见 `standalone/docs/DB_CLEANUP_ACCOUNT.md` |
| `.env.example` | 必填变量模板；**不要提交含真实密钥的 .env** |
| `preflight.sh` | 配置/依赖/端口检查；`--dns` 仅检查 RTC/TURN 域是否可解析 |
| `nginx/templates/imboy.conf.template` | 反代规则（envsubst）：后端 → `api.*`（含 WS 升级），管理后台 → `admin.*`；入口拦截 `/metrics` 返回 403 |
| `prometheus/rules/imboy-alerts.yml` | 告警规则 33 条 14 组（可用性/延迟/HTTP/VM/PG/消息/WS/主机/连接池/备份/TLS/支付） |
| `grafana/dashboards/imboy-overview.json` | 9-panel 总览 |

## 注意事项 / Notes

- 生产不得使用占位密钥；安装器会补齐内部随机密钥并保留已有有效值。客户域名、外部账号和第三方凭据须自行准备，详见客户配置表。
- `.env` 不存在时，安装器在依赖检查后只复制模板并退出；填写后重跑才生成内部密钥并继续完整部署，没有仅初始化配置的模式。
- PG 镜像须包含初始化脚本移除密码 xtrace 的修复；宿主机补丁不能修复旧镜像。提交后账号验证失败保留 LOGIN/`.env`；不全局撤销 PUBLIC，不把账号门禁当整个数据库 ACL 或真实清理业务验收。
- Grafana 首次启动后立即修改默认密码。
- 版本单一来源：`.env` 的 `IMBOY_VERSION`；升级 = 改版本 → pull → up -d（社区版默认 `auto_migrate=true` 自动迁移）。
- Helm **后端固定单副本**（`backend.replicaCount: 1`），后端 HPA 默认**关闭**。后端是有状态的 Erlang 分布式节点，跨 Pod 的 `syn` 进程注册与消息路由未经生产集群验证，多副本下消息投递不确定；集群水平扩展为路线图项，**不得对外承诺**。admin 是无状态静态前端，多副本安全，其 HPA 不受此限。详见 `helm/README.md` 顶部警示。
