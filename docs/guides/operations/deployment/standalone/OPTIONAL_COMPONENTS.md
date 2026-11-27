# 可选组件与交付边界 / Optional components

默认安装只运行社区核心服务。下面均为客户服务器上的操作说明，制包与离线检查不会执行这些操作；实际结果单独记录。

## 根 Compose / Root Compose

在包内 `deploy/.env` 设置 `COMPOSE_PROFILES=monitoring` 启用监控，或设置 `LIVEKIT_TURN_ENABLED=true` 启用 TURN。共用 443 时再设置 `LIVEKIT_SNI_ENABLED=true`。从包根运行 `bash install.sh`。全新实例可使用 `--init-cleanup-db`。若是已有实例变更 TURN 模式，请按[升级与恢复](UPGRADE_AND_RECOVERY.md)走停机切换。

Set `COMPOSE_PROFILES=monitoring` in the package's `deploy/.env` to enable monitoring, or set `LIVEKIT_TURN_ENABLED=true` to enable TURN. Add `LIVEKIT_SNI_ENABLED=true` to share TCP 443. Run `bash install.sh` from the package root. For a new instance, add `--init-cleanup-db`. When changing TURN mode on an existing deployment, follow the maintenance cutover procedure in [Upgrade and recovery](UPGRADE_AND_RECOVERY.md).

以下命令使用[验收命令](ACCEPTANCE_COMMANDS.md)定义的 `dc`。 / Define `dc` using the site acceptance guide.

## Monitoring profile

启用 monitoring 后，Prometheus、Grafana、Pushgateway、Node Exporter 和 PostgreSQL Exporter 一并启动。在 Prometheus 的 targets 页核对采集状态。
The monitoring profile starts Prometheus, Grafana, Pushgateway, Node Exporter, and PostgreSQL Exporter. Verify their status on the Prometheus targets page.

在已完成基础安装的 `deploy/` 内：

```bash
dc --profile monitoring config -q
dc --profile monitoring pull
dc --profile monitoring up -d
dc --profile monitoring ps
```

先按[验收命令](ACCEPTANCE_COMMANDS.md)定义与现场一致的 `dc`。根 Compose 安装可在包内 `deploy/.env` 设置 `COMPOSE_PROFILES=monitoring` 后重跑安装入口。启动前清除父 shell 中继承的 `COMPOSE_PROFILES`。

Grafana 默认为 `127.0.0.1:3000`，经 SSH 隧道访问；请使用 `.env` 里的既有口令，不把该口令写进交接记录。运行规则目录只装 `imboy-alerts.yml`，不应复制上游 `imboy-alerts-test.yml`，后者是 promtool 单测文件。

## LiveKit embedded TURN

首次保持 `LIVEKIT_TURN_ENABLED=false`，仍须准备五域 DNS、HTTP-01 并取得 TURN 证书。只有已确认旧 TURN 服务退出、UDP 3478 / TCP 5349 / UDP 50201–50500 可用时才开启。

本包标准路径使用本 Compose 的 Certbot 证书。TURN overlay 将 `LIVEKIT_TURN_CERT_DIR` 只读挂到 `/etc/letsencrypt/live/<TURN_DOMAIN>`，并把 `./data/certbot/conf/archive/<TURN_DOMAIN>` 只读挂到对应的 `/etc/letsencrypt/archive/<TURN_DOMAIN>`。这样 `fullchain.pem`、`privkey.pem` 的 `../../archive` 相对链接在容器中仍可解析。

`.env` 的 `LIVEKIT_TURN_CERT_DIR` 同时用于宿主机预检和 live 目录挂载；标准路径留空由安装器填写，或填 `./data/certbot/conf/live/<实际TURN_DOMAIN>`。overlay 保留外部平铺证书目录的兼容性，但外部证书的分发、所有权与续期需按实际方案另验，不能只挂一个指向未挂载 archive 的符号链接目录。旧编排的这个 overlay 使用 TCP 5349。共用 443 通过上述根 Compose 入口启用。

第一次启用：编辑 `.env` 设 `LIVEKIT_TURN_ENABLED=true`，确认上述证书路径和端口后从包根执行 `bash install.sh`。保留所有旧密钥、RSA、数据和正式证书。

启用后从 `deploy/` 检查：

```bash
dc config -q
dc ps "$livekit_service"
dc logs --tail=100 "$livekit_service"
```

还要在客户网络验证 5349 的正式证书、实际 ICE relay 候选及双端媒体。TURN_DOMAIN 的 443 不属于此模式的验收端口。原 preflight 会把已经运行的 TURN 占用 3478/5349 记为错误；不要为重跑安装器随意杀进程或删除实例。先核对监听者，再按维护窗口制定变更步骤。

### 续期后重新加载

Certbot 定期续期、Nginx 定期 reload，不代表 LiveKit 已加载新证书。需要在变更窗口完成证书更新后的 LiveKit 重启并复查 5349。下面重启会中断媒体会话，须先协调停机：

```bash
dc restart "$livekit_service"
```

未建立续期运维流程时，不可勾选“TURN 自动续期闭环已完成”。本包未安装 cron、systemd 或外部调度任务。

## 其他形态

本包根入口要求 `UPTRACE_ENABLED=false`、`IMBOY_EB_DIRECT_ENABLED=false`。Uptrace 文件保留以闭合原分支文件引用并复用既有回归；虽然文件齐备，其额外数据库、存储、凭据和服务仍需专项配置与验收，真实运行不属于本包标准交付。不要绕过包根保护入口直接启用。

`docker-compose.prod.yml` 不在开源仓库和本包中。商务部署、支付收款、授权文件、企业 proof、Helm、宿主机蓝绿部署、L4 SNI 443 切换和源码构建不属于本包已具备并验证的交付能力。

即使上游脚本帮助文字提到上述路径，独立包仍以根 README 和本目录手册为准。本包没有用空白占位 Compose 伪装具备商务交付能力。
