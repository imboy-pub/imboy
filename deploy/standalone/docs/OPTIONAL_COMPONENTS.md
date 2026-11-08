# 可选组件与交付边界 / Optional components

默认安装只运行社区核心服务。下面均为客户服务器上的操作说明，制包与离线检查不会执行这些操作；实际结果单独记录。

## Monitoring profile

本包带齐社区 Compose 直接引用的 Prometheus、Alertmanager、Loki、Promtail、Grafana 文件。原始配置保持不变；Prometheus 还列出基础 monitoring 栈未提供的 pushgateway、postgres_exporter、node_exporter 目标。这些目标在未专项补齐时会显示 DOWN，不能把它们当成已具备的能力。Alertmanager 默认空 receiver，不会自动给客户发送通知。

在已完成基础安装的 `deploy/` 内：

```bash
docker compose --env-file .env -f docker-compose.community.yml --profile monitoring config -q
docker compose --env-file .env -f docker-compose.community.yml --profile monitoring pull
docker compose --env-file .env -f docker-compose.community.yml --profile monitoring up -d
docker compose --env-file .env -f docker-compose.community.yml --profile monitoring ps
```

TURN 已启用时，以上每条命令的基础 `-f` 后都须追加 `-f docker-compose.livekit-turn.yml`。不要在 `.env` 或父 shell 中设置 `COMPOSE_PROFILES`；本包入口会阻断隐式启用配置。

Grafana 默认为 `127.0.0.1:3000`，经 SSH 隧道访问；请使用 `.env` 里的既有口令，不把该口令写进交接记录。运行规则目录只装 `imboy-alerts.yml`，不应复制上游 `imboy-alerts-test.yml`，后者是 promtool 单测文件。Promtail 需要宿主机 Docker 日志目录及 socket。

## LiveKit embedded TURN

首次保持 `LIVEKIT_TURN_ENABLED=false`，仍须准备五域 DNS、HTTP-01 并取得 TURN 证书。只有已确认旧 TURN 服务退出、UDP 3478 / TCP 5349 / UDP 50201–50500 可用时才开启。

本包标准路径使用本 Compose 的 Certbot 证书。TURN overlay 将 `LIVEKIT_TURN_CERT_DIR` 只读挂到 `/etc/letsencrypt/live/<TURN_DOMAIN>`，并把 `./data/certbot/conf/archive/<TURN_DOMAIN>` 只读挂到对应的 `/etc/letsencrypt/archive/<TURN_DOMAIN>`。这样 `fullchain.pem`、`privkey.pem` 的 `../../archive` 相对链接在容器中仍可解析。

`.env` 的 `LIVEKIT_TURN_CERT_DIR` 同时用于宿主机预检和 live 目录挂载；标准路径留空由安装器填写，或填 `./data/certbot/conf/live/<实际TURN_DOMAIN>`。overlay 保留外部平铺证书目录的兼容性，但外部证书的分发、所有权与续期需按实际方案另验，不能只挂一个指向未挂载 archive 的符号链接目录。TURN/TLS 共用 443 或 L4 SNI 分流不属于本包标准方案。

第一次启用：编辑 `.env` 设 `LIVEKIT_TURN_ENABLED=true`，确认上述证书路径和端口后从包根执行 `bash install.sh`。保留所有旧密钥、RSA、数据和正式证书。

启用后从 `deploy/` 检查：

```bash
docker compose --env-file .env -f docker-compose.community.yml \
  -f docker-compose.livekit-turn.yml config -q
docker compose --env-file .env -f docker-compose.community.yml \
  -f docker-compose.livekit-turn.yml ps imboy_livekit
docker compose --env-file .env -f docker-compose.community.yml \
  -f docker-compose.livekit-turn.yml logs --tail=100 imboy_livekit
```

还要在客户网络验证 5349 的正式证书、实际 ICE relay 候选及双端媒体。TURN_DOMAIN 的 443 不属于此模式的验收端口。原 preflight 会把已经运行的 TURN 占用 3478/5349 记为错误；不要为重跑安装器随意杀进程或删除实例。先核对监听者，再按维护窗口制定变更步骤。

### 续期后重新加载

Certbot 定期续期、Nginx 定期 reload，不代表 LiveKit 已加载新证书。需要在变更窗口完成证书更新后的 LiveKit 重启并复查 5349。下面重启会中断媒体会话，须先协调停机：

```bash
docker compose --env-file .env -f docker-compose.community.yml \
  -f docker-compose.livekit-turn.yml restart imboy_livekit
```

未建立续期运维流程时，不可勾选“TURN 自动续期闭环已完成”。本包未安装 cron、systemd 或外部调度任务。

## 其他形态

本包根入口要求 `UPTRACE_ENABLED=false`、`IMBOY_EB_DIRECT_ENABLED=false`。Uptrace 文件保留以闭合原分支文件引用并复用既有回归；虽然文件齐备，其额外数据库、存储、凭据和服务仍需专项配置与验收，真实运行不属于本包标准交付。不要绕过包根保护入口直接启用。

`docker-compose.prod.yml` 不在开源仓库和本包中。商务部署、支付收款、授权文件、企业 proof、Helm、宿主机蓝绿部署、L4 SNI 443 切换和源码构建不属于本包已具备并验证的交付能力。

即使上游脚本帮助文字提到上述路径，独立包仍以根 README 和本目录手册为准。本包没有用空白占位 Compose 伪装具备商务交付能力。
