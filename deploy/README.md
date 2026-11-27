# 安装 IMBoy / Install IMBoy

## 第一次安装 / First install

在 Linux 单机上安装社区版。先准备 Docker 24+、Compose 2.23.1+、Bash、jq、curl、OpenSSL，以及可拉取发布镜像的网络。准备五个不同的域名，都指向部署入口，并开放 TCP 80 和 443。80 用于签发证书，443 用于 HTTPS 和 WebSocket。

For a single Linux host, prepare Docker 24+, Compose 2.23.1+, Bash, jq, curl, OpenSSL, and registry access. Point five distinct hostnames to the host and open TCP ports 80 and 443 for certificate issuance, HTTPS, and WebSocket.

<a id="customer-input"></a>

| 填什么 / Input | 用途 / Purpose |
|---|---|
| `API_DOMAIN`、`ADMIN_DOMAIN`、`CS_WIDGET_DOMAIN` | 后端、管理后台、客服入口 / Backend, Admin, support |
| `RTC_DOMAIN`、`TURN_DOMAIN` | 音视频信令、中继 / Call signaling and relay |
| `CERTBOT_EMAIL` | 你确认的证书联系人 / Your confirmed certificate contact |

把商务交付的 `docker-compose.prod.yml` 放到本目录，用 `.env.example` 准备现场配置，在仓库根目录运行：

Place the delivered `docker-compose.prod.yml` in this directory, prepare site settings from `.env.example`, and run from the repository root:

```bash
bash deploy/install.sh --edition business
```

## 展开目录时看哪里 / What each directory does

日常安装只从上面的入口开始。商务安装器读取本机交付的 Compose，`deploy/` 提供部署脚本和配置。目录按用途速查如下。

Start with the install command above. The business installer uses the site-specific Compose file. The folders below supply the scripts and settings it uses.

| 目录 / Directory | 用途 / Purpose |
|---|---|
| `cleanup-db/` | 安装器建立或验证附件清理专用数据库账号 / Checks and setup for the attachment cleanup role |
| `nginx/` | 域名路由、HTTPS 证书和续期 / Domain routing, HTTPS certificates and renewal |
| `haproxy/` | 已有宿主机部署让 HTTPS 与 TURN 共用 443 时使用的模板 / Shared port 443 template for existing host deployments |
| `prometheus/`、`grafana/` | 采集指标、配置告警规则和展示仪表盘 / Metrics, alert rules, and dashboards |
| `uptrace/` | 可选链路追踪，让运维按一次请求查看处理路径 / Optional request tracing |
| `cron/` | 备份、证书检查等定时任务定义 / Schedules for backup and certificate checks |
| `helm/` | [Kubernetes 部署 / Kubernetes deployment](../docs/guides/operations/deployment/helm.md) |
| `native-shell/` | [原生 systemd 交付 / Native systemd delivery](../docs/guides/operations/deployment/native-shell/README.md) |
| `widget/` | 客服静态资源的独立测试镜像和演练工具 / Widget test image and rehearsal tools |
| `secrets/` | 现场密钥文件的位置 / Site secret files |

`preflight.sh` 检查安装条件。`ops.env.example` 配置定时任务。其余初始化和权限脚本由安装器调用。TSID 节点登记和旧宿主机 SNI 切换属于专项运维。本机的 `data/`、`.env` 和商务私有 `docker-compose.prod.yml` 是现场配置或数据。

Use `preflight.sh` for prerequisite checks and `ops.env.example` for scheduled jobs. The installer handles initialization and runtime permissions. TSID node registration and legacy host SNI cutover are specialist tasks. Local `data/`, `.env`, and private business `docker-compose.prod.yml` hold site state or configuration.

<a id="operations-lifecycle"></a>

## 按任务找手册 / Find a guide by task

- [配置、可选服务、排障和旧部署兼容 / Configuration, optional services, troubleshooting, and legacy installs](../docs/guides/operations/deployment/compose-operations.md)
- [备份和恢复 / Backup and restore](../docs/guides/operations/deployment/backup-restore.md)
- [本地开发 / Local development](../docs/guides/operations/deployment/local-development.md)

<a id="rtc-turn-regression"></a>

[音视频和 TURN 验证 / RTC and TURN checks](../docs/guides/operations/deployment/compose-operations.md#rtc-turn-regression)
