# IMBoy 生产部署包 / Production Deployment Package

一键将 IMBoy 部署到一台 Linux 服务器。
One-command deployment of IMBoy to a Linux server.

> 文档分工：本页是 **Docker Compose 客户安装入口**，使用 `deploy/.env.example`；[day1-quickstart.md](../docs/guides/operations/deployment/day1-quickstart.md) 与 [deployment.md](../docs/guides/operations/deployment/deployment.md) 是裸机/release 或定制运行时参考，不能混用其配置模板和启动步骤。备份恢复见 [backup-restore.md](../docs/guides/operations/deployment/backup-restore.md)。

## 原生 Shell 固定版本交付包 / Pinned native Shell kit

[原生 Shell 安装材料](native-shell/README.md) 提供单机 systemd 的安装、升级、
备份恢复、配置模板与陌生人验收步骤，安装工具不依赖 Python 或 Docker。
该工具包固定于 `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af` /
`1.0.0-alpha.83` / 迁移头 `243`，初始材料版为 `PAYLOAD_READY=no`。
发布方须先供应原生产物并完成真实演练；当前 main / alpha.84 的新增安装合同
需要单独适配，不能只修改版本号或模板哈希后使用。

The native Shell kit is a separate, source-pinned installation toolkit for
alpha.83. It contains no runtime binaries or site secrets. Publisher assembly
and real-host acceptance are required; it does not certify current main or
alpha.84. Follow its own templates and manuals for that frozen deployment scope.

## 独立社区版 ZIP / Standalone community ZIP

交付客户时不要只压缩 `deploy/`：原安装器还会检查并调用
`../scripts/sanity_check.sh`。维护者在仓库根目录使用以下入口，从固定 Git
提交生成完整白名单包；客户只需解压该 ZIP，无需完整源码或宿主机 Python。

```bash
# 维护者制包；仅导出已提交文件，不读取工作区 .env、密钥或数据。
# --output 必填；若文件已存在则拒绝覆盖。此命令不部署、不发布。
bash scripts/package_community.sh --ref HEAD --output /tmp/imboy-community.zip
```

打包机需要 Git、Bash、GNU 文件工具、zip/unzip；客户依赖及步骤见
[独立交付手册](standalone/README.md)。包内保留相邻的 `deploy/` 与 `scripts/`，
补齐 Compose、Nginx、证书、自检、备份脚本及监控配置依赖，并附来源、哈希、权限
清单和空白现场验收表。生成的包不含镜像、实际 `.env` 或商务版专有 Compose。
完整性与离线回归通过不代表 Docker、TLS、迁移、登录、WebSocket 或附件验收通过。

The packager exports a committed allowlist, including the sibling `scripts/`
dependencies. The ZIP is an installation kit, not an image bundle or a verified
server deployment. Use its root `install.sh` and manuals for the supported
single-host community path; the repository installer remains available below.

## 社区镜像发布合同 / Community image release contract

`VERSION` 与 `.env.example` 记录交付版本，例如 `1.0.0-alpha.84`；对应 Git
标签为 `v1.0.0-alpha.84`，四类最终镜像标签为 `1.0.0-alpha.84`。只去掉 Git
标签开头的 `v`，完整保留 alpha/beta/rc 后缀。不能将预发布版本自动转成稳定版，
不能用 `latest` 填补缺失版本。当前发布入口明确拒绝 `+build` 元数据，避免在
Docker 标签中做有歧义的转换。

The delivery version is not proof that images exist. Git tags retain `v`, image
tags omit it, and both preserve the complete prerelease identity. The release
workflow validates the Git tag against `VERSION` and the environment template
before any candidate build. A missing version fails Compose configuration.

| Component | Source | Default image |
|---|---|---|
| Backend | Tagged backend commit / 后端固定提交 | `ghcr.io/imboy-pub/imboy-backend:<VERSION>` |
| Admin | Approved frontend commit / 批准的前端提交 | `ghcr.io/imboy-pub/imboy-admin:<VERSION>` |
| Widget | Same frontend commit; `build:widget`, `verify:widget`, `Dockerfile.widget` | `ghcr.io/imboy-pub/imboy-widget:<VERSION>` |
| PostgreSQL 18 | Backend commit, `docker/pg18_Dockerfile` | `ghcr.io/imboy-pub/imboy-pg18:<VERSION>` |

发布者须将 `ADMIN_REPO_REF` 明确设置为批准的 **40 位前端 commit SHA**，不能
使用浮动 `main`；Admin 与 Widget 共享该 SHA。前端自身 `package.json` 版本
独立记录，不要求与后端源码版本相等。最终 `community-images.json` 附件记录两仓
SHA、前端包版本、四镜像 digest、目标平台和 workflow 链接。

The current release target is `linux/amd64`. ARM64 requires its own build and
runtime evidence; an OCI attestation's `unknown/unknown` entry is not a CPU
architecture. Mutable base images and upstream build inputs mean rebuilding the
same source need not reproduce the same digest. Promotion reuses tested digests.

### 部署前实际核验 / Verify before deployment

在仓库根或独立 ZIP 根运行。该工具只读 Registry，不登录或推送；`--pull`
使用空的临时 Docker 凭据配置，检验陌生人的匿名拉取路径。无需宿主机 Python。

```bash
bash scripts/verify_community_images.sh \
  --version 1.0.0-alpha.84 --platform linux/amd64 --pull \
  --output "./image-evidence-$(date -u +%Y%m%dT%H%M%SZ)-$$"
```

`--output` 必须为新目录；证据保留实际状态、摘要、架构和 pull 结果。去掉
`--pull` 只验证 manifest/config 元数据，不能声称实际拉取成功。已有 Docker
缓存可能复用 layer，空凭据配置不等于空镜像缓存；陌生人演练仍应使用干净主机。
显式覆盖镜像时改用 `--backend`、`--admin`、`--widget`、`--pg` 四个完整引用。

Only a `MANIFEST_UNKNOWN` response from a demonstrably readable repository is
reported as missing. Authentication, visibility and network failures remain
unconfirmed. A successful writer-authenticated CI build does not prove public
access. Default community delivery requires all four images to pass anonymous
pull; changing package visibility remains an owner-approved operation.

### 发布门禁与失败恢复 / Gates and recovery

既有 Golden Install/Upgrade 保留，且必须消费同一组四镜像 digest；另加 Widget
资产配对/构建/运行检查和四镜像匿名拉取。`RELEASE_FINALIZE_ENABLED` 开关保留。
当前 Golden 的现场前置、五域配置和清理数据库账号均须实际满足，不能为了发布
跳过失败。源码或离线交付 CI 成功不等于这些门禁已通过。

Golden Install/Upgrade 在首次拉取前共用现场准备：填齐五域及 API origin，
准备独立清理 LOGIN 的用户名与随机凭据，并经安装器 `--init-cleanup-db`
受控创建缺失账号、验证实际认证与权限；已有有效凭据保留，不重置已有账号。
Golden 可通过 `IMBOY_DIRECT_CLEANUP_PG_USER` / `IMBOY_DIRECT_CLEANUP_PG_PASSWORD`
提供清理凭据，通过 `IMBOY_CTL_NODE` / `IMBOY_CTL_COOKIE` 提供节点身份。
清理密码至少 32 位、cookie 为 32–255 位，均只接受字母数字及 `_`、`-`；
不合法的已提供值和占位的清理密码会被拒绝，不静默换值。此限制仅用于 Golden 的临时
配置写入，生产安装器原有账号规则不变。
`host` 模式须显式提供 `--api-domain`、`--admin-domain`、`--cs-widget-domain`、
`--rtc-domain`、`--turn-domain` 和 `--certbot-email`。`ci` 模式使用五个假域名
和完整自签 fixture（开启 Uptrace 时包括其域名），仍执行本地 HTTP-01 路由
及 Nginx 配置检查；Certbot 的 CI 网络被隔离，不能视作公网 TLS/RTC 验证。

Each Golden cleanroom creates a random release cookie, stored only in a mode
`0600` runtime `vm.args` under its private working data. A separate runtime
overlay selects that file through relx `VMARGS_PATH`; the server, health probes,
installer and smoke CLI use the same node identity and cookie. Installation
keeps `IMBOYENV=pro`; only the later smoke overlay enables `dev`. Upgrade
refreshes VM flags from the target source while retaining the run's cookie.
Never upload `.env`, runtime VM arguments, private keys or the cleanroom data.

社区安装器可重复传入 `--compose-file <overlay>`，在社区基准和已选 overlay 之后追加，并将
相同组合贯穿 preflight、拉取、起栈、TLS、清理账号验证和 sanity。文件须存在、
可读且路径不含空白；这不会跳过任何生产检查。商务版拒绝此参数，继续沿用
受控交付的原有 Compose/策略组合。Golden 的旧基线若不支持这个
输入及 `--init-cleanup-db`，须明确阻断，不能修改基线源码后声称原版本升级
已通过。无严格稳定标签时仍按 Release 原策略记录 `SKIPPED_FIRST_RELEASE`。

首次写最终标签前，workflow 保存四镜像及来源 receipt。最终标签已存在且摘要
一致则可重用；摘要不同、无法判断权限或网络错误时停止。写后再次比对摘要和
匿名拉取证据，再以 `--verify-tag` 在原 Git 标签创建 Release；预发布标记为
prerelease，不创建另一个稳定 Git 标签。

Four registry writes are not atomic and Buildx provides no compare-and-swap
here. The release workflow serializes a version; external publishers must not
write the same version concurrently. On partial promotion, do not announce a
release or rebuild and overwrite it. Resume with the saved original four
digests, verify all final tags again, then complete the approved release.

客户回滚须使用上一份已验收的完整四镜像 receipt 和匹配配置；不要移动公共版本
标签。涉及数据库迁移时，遵循 [升级恢复手册](standalone/docs/UPGRADE_AND_RECOVERY.md)
的备份与恢复步骤，不能把换回 pg18 镜像当成数据库回滚。

## 交付清单 / Deliverables

> 双轨分发 / Dual-track distribution：
> - **社区版 `docker-compose.community.yml` 随仓库分发**（garage 默认启用、监控栈
>   走 `--profile monitoring`），`bash install.sh --edition community` 一键部署。
> - **商务版 `docker-compose.prod.yml` 不随开源仓分发**，需通过商务交付渠道获取
>   （leeyisoft@qq.com）后放入本目录。`install.sh --edition business` 会在缺失时
>   明确提示并给出索取方式。
> - 仅作评估可用 `docker-compose.demo.yml`（最小两服务，零配置，无需任何密钥）。

```
deploy/
├── install.sh                   # 一键部署入口（推荐）/ One-command installer (recommended)
│                                # --edition community|business（默认 community）
├── preflight.sh                 # 前置检查（--edition 按版本切换检查口径）/ Pre-flight check
├── docker-compose.community.yml # 社区版编排，随仓分发 / Community stack, shipped in-repo
│                                # 9 核心服务：pg18 + garage + backend + admin + widget
│                                # + edge + nginx + certbot + livekit；监控 5 服务走 --profile monitoring
├── docker-compose.uptrace.yml   # 可选 Uptrace overlay（默认关闭）
├── uptrace/                     # Uptrace / OTel Collector / nginx 配置
├── docker-compose.demo.yml      # 最小两服务演示栈（零配置评估）/ Minimal 2-service demo stack
├── docker-compose.prod.yml      # ⚠️ 商务版走单独交付渠道，不在开源仓内 / Business edition, NOT in the open-source repo
│                                # 7 服务编排：pg18 + backend + admin + nginx + certbot + prometheus + grafana
├── docker-compose-sales-policy.yml # 销售版策略覆盖（无密钥）/ sales policy overlay
├── .env.example                 # 环境变量模板 / Environment variables template
├── nginx/
│   ├── templates/
│   │   ├── imboy.conf.template    # nginx 反向代理（envsubst 渲染）/ nginx reverse proxy (envsubst-rendered)
│   │   └── cs-widget.conf.template # 客服 Widget 第三域 vhost（CSD-DEP-01）/ CS widget gateway vhost
│   ├── rtc.conf.template        # 社区独立 RTC 域 HTTPS/WSS → LiveKit / Community RTC origin
│   ├── prod-vhosts/             # 宿主机生产 vhost 快照（蓝绿单机形态）/ host-level prod vhost snapshots（见其 README）
│   └── init-letsencrypt.sh      # 首次签发 Let's Encrypt 证书 / First-time Let's Encrypt issuance
├── prometheus/
│   ├── prometheus.yml           # 抓取配置（4 job）/ Scrape config (4 jobs)
│   └── rules/                   # 告警规则目录（见 imboy-alerts.yml）/ Alert rules
├── grafana/
│   ├── provisioning/            # 自动装配 datasource + dashboard provider
│   │   ├── datasources/         # Auto-provision Prometheus datasource
│   │   └── dashboards/          # Auto-provision dashboard folder
│   └── dashboards/
│       └── imboy-overview.json  # 9 panel 总览面板 / 9-panel overview dashboard
├── widget/                      # CS Widget 独立静态镜像（DEP-01）
│   ├── Dockerfile               # nginx:1.27-alpine 非 root 承载 build:widget 产物
│   ├── nginx.conf               # 缓存策略 + SSE 直通（见下「CS Widget」节）
│   └── dryrun.sh                # install/restart/rollback 合成环境演练（DEP-01 A06）
├── docker-compose.widget.yml    # widget 服务 overlay（默认宿主端口 18900）
├── docker-compose.secrets.example.yml # 后端 secret 以 _FILE 加 0600 文件注入示例
├── secrets/                     # 部署机本机 secret 文件（git 忽略，仅路径入 compose）
└── README.md                    # 本文件 / This file
```

## CS Widget 静态服务（DEP-01）

客服 Widget（宿主 loader + iframe 聊天应用）以**独立镜像**交付，不混入 Admin
SPA 镜像。产物由 admin 仓 `bun run build:widget` 生成（`dist-widget/`，含
`loader.js` / `widget/index.html` / `assets/`），构建时拷入本目录 `widget/dist/`
（git 忽略，dist 本体永不入仓）：

```bash
# 1. 生成产物（admin 仓）
cd /path/to/imboyadmin && bun run build:widget
# 2. 构建镜像（tag 固定、禁 latest：升级=推新 tag，回滚=切回旧 tag）
cp -R dist-widget/. /path/to/imboy/deploy/widget/dist/
docker build -t imboy-widget:v1 /path/to/imboy/deploy/widget/
# 3. 起服务（overlay 可并入社区栈；独立演练见 widget/dryrun.sh）
docker compose -f docker-compose.widget.yml up -d   # 宿主端口 18900
```

缓存策略（`widget/nginx.conf`）：宿主 HTML 与 `widget/` 应用 HTML `no-cache`，
`/loader.js` 与 `/assets/` 短缓存（300s）+ `must-revalidate` + ETag——产物文件名
当前不带内容 hash，**不得**加 `immutable`（升级后旧资产会按同名永生）；产物改带
hash 后方可升级为长缓存。Widget 访客事件流（SSE）经 nginx 时禁缓冲/禁缓存、
1h 读超时（本镜像与主反代模板均已内置对应 location）。

后端 secret（widget subject key 与企业消息 keyring）经 `docker-compose.secrets.example.yml`
以 `_FILE` + 0600 只读挂载注入：明文只存在于部署机 `secrets/` 目录文件里，
不出现在 compose、`.env` 或日志中；任一文件缺失或权限过宽，后端启动即拒绝
（fail-closed，读方 `src/lib/imboy_env_overrides.erl`）。

## 前置条件

- Linux x86_64，文档基准环境 **Debian 13 (Trixie)**（其他发行版同样适用）
- 核心栈内存 ≥ 4 GB、磁盘 ≥ 10 GB（建议 8 GB / 20 GB）；启用 Uptrace 后
  至少 8 GB / 20 GB，建议 16 GB / 40 GB
- Docker 24+ 与 `docker compose` v2 插件（社区版 compose 使用 configs 内联定义，
  需要 **Compose v2.23.1+**；Debian 13 经 get.docker.com 安装的 Docker 均满足。
  未安装 Docker 时 `install.sh` 会确认后引导安装）
- 宿主机需要 Bash、`jq` 1.6+、`curl`、OpenSSL 及常规 Unix 工具；完整清单见下文。
  安装、初始化和部署后自检不需要宿主机 Python、pip 或 Python WebSocket 包。
- 已解析到本机的五个独立域名：`api.example.com`、`admin.example.com`、
  `cs.example.com`（客服托管 Widget）、`rtc.example.com`、`turn.example.com`；启用 Uptrace
  时再准备独立的 `uptrace.example.com`
- 五域的公网 A 记录指向部署入口；若发布 AAAA，IPv6 也必须指向可达的同一入口。
  五域的 TCP 80 都须允许公网 ACME HTTP-01 请求到达 `/.well-known/acme-challenge/`，
  HTTPS/WSS 使用 TCP 443。即使 `LIVEKIT_TURN_ENABLED=false`，TURN 域仍必填并参与签证。
- RTC 媒体还需 TCP 7881、UDP 50000–50200；启用 embedded TURN 后需 UDP 3478、
  TCP 5349、UDP 50201–50500。默认 TURN/TLS 使用 5349，443 共用是下文独立切换路径。

<a id="customer-input"></a>

### 客户填写与自动生成 / Customer input and generated values

以 [`install.sh`](install.sh)、[`preflight.sh`](preflight.sh) 和 [`.env.example`](.env.example)
为准：五域及邮箱是两版本的共同要求，**不是全部人工配置**。请在 `deploy/.env` 中核对：

| 客户字段 / Customer field | 要求 / Requirement |
|---|---|
| `API_DOMAIN` | API / WebSocket 纯域名，如 `api.example.com` |
| `ADMIN_DOMAIN` | 管理后台纯域名，如 `admin.example.com` |
| `CS_WIDGET_DOMAIN` | 托管 Widget 纯域名，如 `cs.example.com` |
| `RTC_DOMAIN` | LiveKit 信令纯域名，如 `rtc.example.com` |
| `TURN_DOMAIN` | TURN 纯域名，如 `turn.example.com`；关闭 TURN 时也不能省略 |
| `CERTBOT_EMAIL` | 客户实际维护的证书账号邮箱 |
| `IMBOY_BASE_URL` | 改为实际 `https://<API_DOMAIN>`，无路径、无尾斜杠；独立 preflight 和商务安装直接检查此值 |
| `IMBOY_DIRECT_CLEANUP_PG_USER` | 必填的独立清理数据库 LOGIN，不能与 `POSTGRES_USER` 相同；仅加入 `v5_worker`。社区版可显式 `--init-cleanup-db` 创建缺失账号，其他路径由 DBA 开通 |
| `IMBOY_DIRECT_CLEANUP_PG_PASSWORD` | 已开通账号须填其匹配的强密码（至少 32 字符，且不同于 `POSTGRES_PASSWORD`）；新账号留空时安装器会生成，受控初始化使用同一密码 |

所有示例域名、邮箱都要替换；五域两两不同，不带协议、端口或路径。`POSTGRES_USER`、
`POSTGRES_DB`、`DATA_DIR` 等可保留适用的模板默认值，版本和镜像须与实际交付包一致。
预检只核对配置，不证明账号已存在、密码可登录或 `v5_worker` 权限已就绪。社区版在
后端健康之后另做真实数据库门禁：默认只验证；显式 `--init-cleanup-db` 才创建缺失
LOGIN，已有账号不改密、不补权。商务版、外部数据库或管理权限不足时由 DBA 按
[清理数据库账号手册](https://github.com/imboy-pub/imboy/blob/main/deploy/standalone/docs/DB_CLEANUP_ACCOUNT.md) 初始化并验证。
独立交付包的离线副本位于包根 `docs/DB_CLEANUP_ACCOUNT.md`。

安装器在已有 `.env` 后自动补齐空值或 `CHANGE_ME` 内部密钥，保留已有非占位值：
数据库密码、JWT/AES、管理 Cookie、edge token、密码盐、握手密钥/IV、Grafana、
LiveKit、Garage，以及启用 Uptrace 时的六项内部密钥；登录 RSA 文件生成到
`${DATA_DIR}/backend_priv/keys/`，已有私钥保留。DNS、第三方账号/凭据、License 和
数据库管理权限须由客户或交付方准备；脚本不会自动提权。正式 TLS 由 ACME 在网络条件满足后签发。

| 安装路径 / Install path | 配置差异 / Configuration differences |
|---|---|
| `--edition community`（默认） | 使用随仓 `docker-compose.community.yml`；容器固定 `IMBOY_EDITION=community`、外部支付网关关闭，无需支付商户凭据，修改 `.env` 不能打开该网关 |
| `--edition business` | 需另行取得 `docker-compose.prod.yml`，叠加随仓 `docker-compose-sales-policy.yml`；默认 `IMBOY_SALES_RELEASE=true` 时要求网关开启、`IMBOY_PAYMENT_MODE=live`，至少一个网关的完整凭据 |

`--edition business` 是部署文件选择；授权级别 `IMBOY_EDITION=professional|enterprise`
及对应 `IMBOY_LICENSE_FILE` 另按商务交付配置。两版本启用短信/SMTP 后均须补齐相应
凭据；启用 Uptrace 另填 `UPTRACE_DOMAIN`、`UPTRACE_ADMIN_EMAIL` 并准备其 DNS/证书。
支付开启时，安装器只为留空的微信/支付宝回调地址派生 API 域 HTTPS URL，不生成商户凭据。

## 推荐：一键部署

```bash
cd /path/to/imboy/deploy
cp .env.example .env
$EDITOR .env
bash install.sh --edition community --init-cleanup-db
```

上例用于全新社区实例，显式授权创建缺失的清理 LOGIN；已有账号仍保持原样。
客户先按[配置表](#customer-input)准备五域、邮箱、API origin、独立清理数据库账号配置，
以及实际启用的第三方配置，再运行 `install.sh`。脚本会幂等补齐数据库口令、JWT、AES、LiveKit、Garage、Uptrace 等内部
随机密钥和 RSA 登录密钥，并自动派生支付宝/微信回调 URL。之后依次完成：前置检查
→ 拉齐缺失镜像 → 起服务 → 签发 TLS → 等待健康 → 清理数据库身份门禁 → 部署后自检 →
打印 Release Identity 三元组（`IMBOY_VERSION` / `IMBOY_GIT_SHA` /
`IMBOY_IMAGE_DIGEST`）与访问地址。证书签发失败会直接中止并列出常见原因，
**不会在 TLS 没起来的情况下谎报"部署完成"**。

社区单机包提供核心 IM、Garage 和 LiveKit，外部支付网关固定关闭；需要支付宝、微信
或 Stripe 实际收款时使用商务单机包 `--edition business`，并在同一份 `.env` 中填写
所启用网关的凭据。短信、SMTP 和可选 Uptrace 两个版本都支持。

交付前必须确保 `IMBOY_VERSION` 对应的 backend/admin/pg18 镜像已发布且客户可拉取。
安装器会在启动任何容器前统一检查；私有 GHCR 交付由客户先在部署机执行其获授权的
`docker login ghcr.io`，安装器不会猜测、保存或输出客户的仓库账号。

若尚未创建 `.env`，安装器通过工具/Docker 前置检查后只复制权限为 `600` 的模板并退出；
填写后重跑才生成内部密钥/RSA 并继续部署。没有 `--init-only` 或 `--dry-run` 参数，
不要把安装命令当成仅生成配置的检查命令。首次保持 `LIVEKIT_TURN_ENABLED=false` 完成
签证；确认 TURN 证书就绪、旧 TURN 已退场且 3478/5349 无占用后，再置 `true` 重跑。

### 清理数据库身份 / Cleanup database identity

社区门禁使用 [`init-cleanup-db.sh`](init-cleanup-db.sh)：按运行中的 backend 镜像精确
比对全部迁移 history/head/dirty（须包含 179/267/270/271/274），再验证清理身份。
只有 `--init-cleanup-db` 才使用现有 `POSTGRES_USER` 的管理权限，在单个事务内创建
缺失 LOGIN、用 `psql \password` 设置 SCRAM 密码、授 `v5_worker` 和目标库 CONNECT。
需要已有的 CREATEROLE、`v5_worker` ADMIN OPTION 和 CONNECT 授权权；不足时停止，
不自动提升权限。工作账号保持非超级用户、NOINHERIT、无管理属性。

验证客户端来自运行中 PG 的本地镜像，共用 backend 网络命名空间，先要求错误密码
明确被拒绝，再用正确密码核验独立身份、成员关系、所有权及 SET ROLE/函数授权。
不以 socket/loopback 的 trust 登录作证，不调用清理队列函数，不更改已有账号。
服务与迁移已就绪时，从 `deploy/` 执行 `bash init-cleanup-db.sh` 可单独复验；
显式创建用 `bash init-cleanup-db.sh --create`，overlay 通过实际 `COMPOSE_FILES` 传入。
这两条命令不会启动服务或代跑迁移。完整 DBA 步骤、失败重跑和安全退役见
[清理数据库账号手册](https://github.com/imboy-pub/imboy/blob/main/deploy/standalone/docs/DB_CLEANUP_ACCOUNT.md)。

全新部署的 PG 镜像必须包含 `docker/pg-initdb-imboy.sh` / `docker/pg-update-imboy.sh`
移除密码 xtrace 的修复。更新宿主机脚本不会修复旧镜像；交付方须重建或选择包含修复
的镜像并核对 digest。迁移 190 合法提供的 SELECT 权限会保留；本门禁不全局撤销
PUBLIC，也不等于整个数据库 ACL 或真实附件垃圾回收已经验收。

### 单公网 IP 共用 TURN/TLS 443

已有宿主机 Nginx/OpenResty 占用 `443/TCP`，又需要 LiveKit embedded TURN 支持严格
只放行 443 的网络时，使用独立的 L4 SNI 切换器：

```bash
sudo install -m 600 livekit-l4-sni.env.example /etc/imboy/livekit-l4-sni.env
sudo editor /etc/imboy/livekit-l4-sni.env
sudo bash install-livekit-l4-sni.sh --check
sudo bash install-livekit-l4-sni.sh --apply
```

该脚本不会扩大 `install.sh` 的社区整栈职责，也不会卸载 eturnal/coturn；任一步失败
自动恢复备份。拓扑、端口、预期输出、续期、重启检查和一键回滚见
[LiveKit TURN/TLS 443 一键切换手册](../docs/guides/operations/deployment/livekit-turn-443-l4-sni.md)。

### 服务关系与域名

```text
客户端 ──HTTPS/WSS──> nginx :80/:443
                         ├── API_DOMAIN/             -> Erlang/OTP 29 backend :9800
                         ├── API_DOMAIN/livekit/     -> LiveKit :7880
                         ├── API_DOMAIN/s3/          -> Garage S3 :3900
                         ├── ADMIN_DOMAIN/           -> React admin :80
                         ├── CS_WIDGET_DOMAIN/       -> Widget 静态 :8080
                         │     /v1/loader.js /assets/ /widget/（loader/资产/frame 壳）
                         ├── CS_WIDGET_DOMAIN/w/*    -> backend :9800（动态 frame HTML）
                         ├── CS_WIDGET_DOMAIN/api/v1/cs/widget/* -> backend :9800
                         └── UPTRACE_DOMAIN/ (可选)  -> Uptrace :80

backend -> PostgreSQL 18（业务数据）
backend -> Garage（附件）；backend 签发 LiveKit token，媒体直连 TCP 7881/UDP 50000-50200
backend -> 支付/短信/SMTP 第三方 HTTPS/TLS 服务（仅配置并启用后）
OTel Collector -> 抓取 backend /metrics -> Uptrace
Uptrace -> PostgreSQL 18 的独立 uptrace 库 + 独立 ClickHouse + 独立 Redis
```

客服托管 Widget 的接入与运维细节（snippet、smoke、证书续期、卸载边界）见
[customer-service-widget.md](../docs/guides/operations/deployment/customer-service-widget.md)。

Uptrace 默认关闭。启用时在 `.env` 设置 `UPTRACE_ENABLED=true`，填写
`UPTRACE_DOMAIN` 和 `UPTRACE_ADMIN_EMAIL`；其余 Uptrace 密钥由安装器生成。该 overlay
只采集 Prometheus 指标，不采集业务日志，且故障不会成为 IMBoy 核心服务的依赖。

可选参数（`bash install.sh --help` 查看全部）：

- `--init-cleanup-db`：仅社区版，迁移后以已有数据库管理权限创建缺失的清理 LOGIN；
  默认只验证，已有账号不改密或补权。商务/外部数据库由 DBA 另行执行账号手册。
- `--admin-phone` / `--admin-password`：装完后在 backend 容器内经
  `imboy_ctl adm create` 创建超管，无需浏览器走 `/setup` 向导（无浏览器纯脚本部署）
- `--yes, -y`：跳过所有确认（含 Docker 缺失时的安装确认，适合自动化）
- `--edition business`：改用商务版 `docker-compose.prod.yml` + sales-policy
  overlay（缺文件时提示商务索取方式）

### API 与客服上传地址

`IMBOY_BASE_URL` 使用 `https://<API_DOMAIN>`，供 API、edge 回退及其他业务使用。社区 Compose 为旧 Widget/Seat 同源上传单独设置 `IMBOY_WIDGET_BASE_URL=https://<CS_WIDGET_DOMAIN>`；缺少该地址时旧上传不下发 URL。direct-v1 上传使用独立的存储授权。Helm 的 `widgetHost` 和 `widgetBaseUrl` 默认为空，启用旧同源上传时须配置同一实际 Widget 网关。

社区安装器以 Compose 实际解析的域名执行 preflight，并临时派生 API origin；商务安装
与独立 preflight 读取 `.env`。因此仍须把 `.env` 的 `IMBOY_BASE_URL` 配成实际 API origin。

### Shell 配置检查 / Configuration checks

以下命令不启动服务、不申请证书。完整 preflight 需要已有内部密钥/RSA 的 `.env`；
刚复制模板时出现密钥占位符或 RSA 缺失错误是预期，首次安装按上面的安装器流程执行。
`preflight.sh` 单独运行默认是 **business**，必须显式选择版本：

社区版：

```bash
cd /path/to/imboy/deploy
# 社区版：配置、宿主机/Docker 与 RTC/TURN DNS 探测
bash preflight.sh --edition community --docker --dns &&
  docker compose --env-file .env -f docker-compose.community.yml config -q
```

商务版（与上面二选一，先取得对应的 `prod.yml`）：

```bash
cd /path/to/imboy/deploy
bash preflight.sh --edition business --docker --dns &&
  docker compose --env-file .env -f docker-compose.prod.yml \
    -f docker-compose-sales-policy.yml config -q
```

启用 Uptrace/TURN 时，`config -q` 也须按安装器顺序追加对应 `-f` overlay；不要输出完整
`compose config`（会展开密钥）。`config -q` 仅验证 Compose 展开；preflight 不连接数据库
验证清理账号。已运行实例还可能报告端口占用，须核对监听者；TURN 开启时的 3478/5349
占用会直接报错，不能据此要求删除正在工作的实例。

`--dns` 仅探测 RTC/TURN 是否可解析，不能证明其公网 IP 正确，也不覆盖另外三域。
在 Debian/Ubuntu 上可另检查五域 A 解析（字段沿用模板的无引号 `KEY=value` 格式）：

```bash
(
  set -eu
  for key in API_DOMAIN ADMIN_DOMAIN CS_WIDGET_DOMAIN RTC_DOMAIN TURN_DOMAIN; do
    domain=$(awk -F= -v key="$key" '$1 == key {print $2; exit}' .env)
    test -n "$domain"
    printf '%s: %s\n' "$key" "$domain"
    getent ahostsv4 "$domain"
  done
)
```

人工比对返回地址与客户公网入口，并从外网检查 AAAA、HTTP-01 路径和正式证书。
缺解析工具的 WARN 不能记为通过；这些配置检查均不代表安装或业务验收成功。

### Shell 交付依赖与验收边界 / Shell delivery dependencies and acceptance

正式客户入口仍为 `bash deploy/install.sh --edition community`（商务版改为 `--edition business`）。宿主机编排只用
Bash、Docker Compose 和 Unix 工具；Compose 环境按域名白名单读取，JSON 校验由 `jq`
执行，不执行 Compose 输出，不把展开后的密钥 JSON 写入文件或输出到日志。

The customer entry point remains `deploy/install.sh`. Host-side installation, initialization
and sanity checks require no Python interpreter or Python packages. This does not describe
the implementation languages inside third-party images (for example, Certbot), and does not
remove development or CI dependencies elsewhere in the repository.

| 宿主机依赖 / Host dependency | 用途与范围 / Purpose and scope |
|---|---|
| Bash | 执行安装器、preflight、TLS 初始化与 sanity；不能用 `sh install.sh` 代替 |
| Docker 24+、Compose v2.23.1+ | 配置展开、镜像拉取与检查、服务编排；需有 daemon 访问权限 |
| `jq` 1.6+ | 校验 Compose JSON 中的 origin、密钥、独立清理账号与镜像；缺失立即停止 |
| OpenSSL、`curl`、系统 CA 证书 | 生成内部密钥/RSA/临时证书、HTTPS 与健康探测 |
| `awk`、`grep`、`sed` 及 coreutils | 包括 `cat`、`chmod`、`cp`、`cut`、`date`、`df`、`dirname`、`head`、`mkdir`、`mktemp`、`mv`、`rm`、`seq`、`sleep`、`tail`、`touch`、`tr`；常规 Linux 基础工具 |
| `ss` 或 `lsof`；`getent` 或 `host`/`dig`/`nslookup` | preflight 端口与 DNS 探测；交付机应提供这些工具，不能把缺工具的 WARN 当作该项已验证 |
| Git（可选） | 输出源代码 SHA；散件交付应随包提供版本身份记录 |

Debian/Ubuntu 可先安装宿主机工具（Docker/Compose 按上方版本要求另行准备）：

```bash
sudo apt-get update
sudo apt-get install -y bash jq curl openssl ca-certificates coreutils grep sed gawk iproute2 libc-bin
```

`psql`、`pg_isready` 和 Erlang `escript` 由已有 PostgreSQL/backend 容器提供，客户宿主机
不需要安装 PostgreSQL 客户端或 Erlang。数据库首次初始化与迁移继续使用原有镜像流程；
超管可通过 `/setup` 或安装器的 `--admin-phone`/`--admin-password` 初始化。CLI 方式仍须
提供与后端节点一致的 `IMBOY_CTL_COOKIE`，失败不能当作初始化成功。

| 检查类别 / Check category | 必须执行的内容 / Required evidence |
|---|---|
| 安装器强制门禁 | preflight；社区版实际 Compose 的 API/Widget origin、密钥强度与互异、backend/edge/PG 一致性；两版本的镜像可用性及 edge 文件检查、TLS、`/healthz`；社区版迁移清单与清理账号的真实正反认证/权限检查；`scripts/sanity_check.sh`。缺少必需脚本/SQL 即停止，不能省略交付材料后继续安装 |
| 初始化与数据保护 | 首次超管初始化/登录；重复安装保留已有非占位密钥、RSA 和正式证书；升级前执行备份与适用迁移验收。重跑安装器不代表可以删除数据目录或重新生成存储凭据 |
| 客户业务验收 | 用客户验收账号在实际 App/管理后台验证注册或既定开户方式、登录、双端消息发送和接收、断线重连、附件上传和下载；按启用功能补做 Widget、RTC/TURN、短信/SMTP 等验收。不得用端口通或 WS 握手成功替代消息收发结果 |
| 开发 / Golden / CI | `scripts/smoke_8step.sh`、`golden_install.sh` 及相关测试仍保留其既有 Python/测试依赖；8 步链使用开发验证码、测试账号和直接 RPC，Golden 准备步骤会切换 `IMBOYENV=dev`，不能拿到正式客户实例上当通用验收脚本 |

`sanity_check.sh` 的成功只表示其既有基础设施检查通过。它不会验证真实用户 WS 往返、
附件内容或外部证书信任链；其内部 nginx 探测使用 `--no-check-certificate`，部分管理页
或日志问题仍记为 WARN。交付时须处理或记录 WARN，并从客户网络验证正式域名证书和
上述业务流程，保存结果后再判定业务验收完成。正式安装不传 `--skip-tls`。

社区 Compose 默认下发 `wss://<RTC_DOMAIN>`，由单独挂载的
`nginx/rtc.conf.template` 终结 TLS，再经 Compose 内网反代到 `imboy_livekit:7880`。
宿主 OpenResty vhost 使用的 `127.0.0.1:7880` 不能用于 Nginx 容器内部。
API 域原有 `/livekit/` 路由保留；可选 L4/SNI stream 模板不挂入社区 HTTP 模板目录。
Nginx 的 shell 启动包装必须重新调用 `/docker-entrypoint.sh nginx`，否则官方入口
会跳过模板渲染，离线 `envsubst` 成功也不能证明容器实际加载了 vhost。

首签为各域准备临时证书后，先逐域确认本机 HTTP-01 webroot 可读，再执行 Certbot；
正式证书通过 `nginx -t` 后才 reload。临时证书保留自签标记，不能视为正式 TLS 就绪。
若签发失败且尚无 Certbot archive/renewal 状态，会恢复临时证书供重跑；已有正式证书
和部分签发的 archive/renewal 不覆盖，后者需人工修复。这个本机 IPv4 探针不证明公网
DNS、AAAA、HTTP-01 可达性或证书信任链正确。交付仍须在客户网络验 WSS、双端音视频
及 TURN 回退，WSS `101 Switching Protocols` 仅表示信令连接升级成功。

Community RTC uses its dedicated TLS vhost and the Compose service `imboy_livekit:7880`.
The shell wrapper re-enters the official Nginx entrypoint to render the explicitly mounted
HTTP templates; optional host stream templates remain excluded. TLS bootstrap waits for
every local HTTP-01 route before issuance and validates Nginx configuration before reload.
Neither the local probe nor a successful WebSocket upgrade proves public TLS, two-client
audio/video or TURN fallback acceptance.

Production acceptance includes infrastructure gates, administrator initialization and real
client business checks. A successful sanity check is not evidence of a WebSocket message
round trip or an attachment round trip. Keep the development smoke chain in an isolated
test environment; do not enable development authentication shortcuts on a customer instance.

**离线回归 / Offline regression**（源码仓库执行；不启动真实服务，不申请证书）：

独立交付包使用其 `scripts/run_offline_checks.sh`；以下专项回归由源码仓库和 CI 执行。

```bash
for script in deploy/install.sh deploy/init-cleanup-db.sh deploy/preflight.sh deploy/nginx/init-letsencrypt.sh \
  scripts/sanity_check.sh scripts/test/install_one_click_test.sh; do
  bash -n "$script" || exit 1
done
bash scripts/test/install_one_click_test.sh
bash scripts/test/community_delivery_config_test.sh
bash scripts/test/init_letsencrypt_test.sh
bash deploy/preflight.sh --self-test
```

安装回归使用真实 Bash/jq/OpenSSL 和本地 Docker/网络测试桩，验证解析、失败关闭、
调用顺序和幂等保护；它不能证明真实 Docker、镜像发布、ACME、数据库迁移或客户网络
已可用。在没有 Python 的干净测试机/隔离根目录中执行同样命令，可复核宿主机无 Python
依赖。真实部署验收另按上表执行；开发与 CI 专用测试不属于此无 Python 承诺。

**容器 RTC 回归 / Container RTC regression**（在源码仓库执行）：

```bash
bash scripts/test/community_rtc_wss_shell_test.sh
bash scripts/test/community_rtc_wss_test.sh --docker
```

需要 Linux Docker/Compose、Bash、jq、curl、OpenSSL 和可拉取仓库锁定的 Nginx/LiveKit
镜像。测试使用隔离容器网络、临时测试证书和合成短期凭据，读取实际社区 Compose 的
镜像、启动命令、模板挂载和替换白名单，验证 `nginx -t`、严格 TLS 校验以及真实
LiveKit `/rtc` 的 WSS 握手，并抽查已有 API 路由。缺少 Docker/Compose 时以
`SKIP` 和退出码 `77` 结束，不能记为 PASS。该测试不申请公网证书，不执行真实用户
登录或双端媒体验收，也不启用 TURN overlay。

This isolated regression uses the pinned images and real startup command. It requires
Docker/Compose; exit 77 means not run. It checks Nginx syntax and TLS/WebSocket signaling,
not customer ACME issuance, product authorization, media connectivity or TURN.

`community_rtc_wss_shell_test.sh` 只用 Docker 桩验证无换行 CID 的读取、失败诊断和
容器/网络清理，不启动容器，不计作 TLS/WSS 通过。
The Shell regression uses a Docker stub for CID/cleanup control flow only; it does not verify TLS/WSS.

清理身份的 SQL、真实密码认证、角色授权、事务回滚、重复执行和服务端日志检查须从
源码仓库在隔离 PostgreSQL 18 执行 `bash scripts/test/cleanup_db_postgres_test.sh`。
该开发专项不在独立交付包内；使用合成迁移台账和函数，不证明全量迁移或真实删除。
其 Docker 前置以脚本为准，不能把生产实例作为故障注入目标。
现场仍须验证实际镜像、网络/HBA、权限及附件生命周期；未执行项不记为通过。

---

## 手工五步排查（仅已有完整配置的商务版）

> 首次安装使用 `install.sh --edition community|business`。本节仅供已完成内部密钥/RSA
> 初始化的**商务版**（`docker-compose.prod.yml` + sales-policy overlay）逐步排查；
> 不能把刚复制的模板直接用于此流程，也不能以这些命令替代安装器的全部门禁和 sanity。

### 1. 检查已有配置

```bash
cd /path/to/imboy/deploy
test -f .env
$EDITOR .env
```

逐项核对上方[客户填写与自动生成](#customer-input)：五域、邮箱、API origin、独立清理
LOGIN/匹配密码及商务条件字段。内部密钥应已消除占位符，RSA 位于
`${DATA_DIR}/backend_priv/keys/`。不要覆盖现有 `.env`、RSA 私钥或密码盐。

**未启用外部支付网关时**（`IMBOY_PAYMENT_GATEWAY_ENABLED=false`）：充值、网关回调、
提现端点会返回「功能未启用」，站内钱包账务（余额/流水/红包/转账）不受影响。
需要对外收款时置为 `true`，此时 `IMBOY_PAYMENT_MODE` 必须是 `live` 且至少一个网关
凭据完整，否则节点 fail-fast。销售版默认 `IMBOY_SALES_RELEASE=true`，前置检查会
强制要求以上条件。商务包仅作钱包演示时须显式设 `IMBOY_SALES_RELEASE=false`；社区版
由其 Compose 固定关闭外部网关，且社区 preflight 跳过销售支付强校验。

**动态插件生命周期写操作默认关闭**（`IMBOY_PLUGIN_LIFECYCLE_ENABLED=false`）：
`/api/adm/plugin/*` 的 `install`/`enable`/`disable`/`upgrade`/`uninstall`/`reset`/
`force_uninstall` 七个写端点会返回「功能未启用」。该子系统冻结（`@status FROZEN`），
`install` 的路径参数无白名单（审计 #43）、签名校验 100% 放行（#44），admin 可达即等于
代码加载面。**内置功能开关**（channel/moment/location/group_collab）是纯 manifest、
只读可见，不受此开关影响。需要时置为 `true`（需自行承担动态加载的安全风险）。

### 2. 前置检查

```bash
bash preflight.sh --edition business --docker --dns
docker compose --env-file .env -f docker-compose.prod.yml \
  -f docker-compose-sales-policy.yml config -q
```

任何 `ERROR` 都必须修复后再继续。

### 3. 创建网络 & 启动

```bash
docker network create imboy-network 2>/dev/null || true
docker compose \
  -f docker-compose.prod.yml \
  -f docker-compose-sales-policy.yml \
  up -d
```

销售版必须同时合并 `docker-compose-sales-policy.yml`，以显式开启严格 E2EE、频道和付费频道入口；该覆盖文件不含密钥。

**证书未完成时补做签发**（全部五域的 DNS/HTTP-01 条件均须满足；使用与启动相同的 Compose 组合）：

```bash
COMPOSE_FILES="docker-compose.prod.yml docker-compose-sales-policy.yml" \
  bash nginx/init-letsencrypt.sh
```

启用 Uptrace/TURN 时，启动、检查和签证命令均须追加相同 overlay；优先重跑安装器保持组合一致。

签发成功后，`imboy_certbot` 会在后台自动续期，nginx 定期 reload 加载新证书，无需再手动执行。

### 4. 查看启动状态

```bash
docker compose -f docker-compose.prod.yml -f docker-compose-sales-policy.yml ps
docker compose -f docker-compose.prod.yml -f docker-compose-sales-policy.yml logs -f imboy_backend
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

## OA 免登开通（默认 OA 应用引导）

> 标准部署下 OA 免登（工作台 `getAuthCode`）随部署自动开通，无需手工 SQL 登记
> （ADR：`docs/architecture/2026-10-12-organization-positioning-amendment.md` M0）。

**两步开通**：

1. **部署配置注入回调地址**（启动期自动引导，幂等）：

   在部署机的 `config/sys.pro.config`（本机本地配置，gitignored，各环境独立维护）的 `{imboy, [...]}` 配置列表加一项（**无需改动启动环境与部署脚本**）：

   \`\`\`erlang
   %% {imboy, [ ...既有项..., 以下为 OA 免登引导
   {oa_redirect_uri, "https://oa.example.com/sso/cb"}
   %% 可选：{oa_application_key, <<"default-oa">>}
   %% 可选：{oa_name, <<"办公">>}
   \`\`\`

   业务归属组织自动取实例首个 active 组织，不提供配置项（OA 免登与
   organization 无关，授权边界是部署实例）。

   重启后启动期自动登记默认 OA 应用（app + redirect allowlist + `sso:exchange`
   Grant）。未配置该项 = 不登记，App 工作台静默隐藏 OA 入口（不报错）。
   兼容通道：OS 环境变量 `IMBOY_OA_REDIRECT_URI` 等价项仍可用（app env 优先）。

2. **签发 OA 侧凭证**（OA 后端要调 exchange 时执行，credential 明文不进日志）：

   \`\`\`bash
   PG_HOST=... PG_PORT=... PG_USER=... PG_PASSWORD=... PG_DATABASE=... \
   scripts/seed_default_oa_application.escript
   \`\`\`

   脚本自动从 `config/sys.pro.config` 读取 `{oa_redirect_uri, ...}`（与节点同源；
   也可用 `IMBOY_OA_REDIRECT_URI` env 覆盖）。幂等：应用已存在则只补缺口；
   新签发 credential 的明文**仅打印一次**，通过安全渠道转交 OA 团队。

**验证**：App 重新进入工作台 → 打开 OA 联调页（`https://www.imboy.pub/test_oa.html`）
→ 「检测免登能力」应返回 supported；「获取一次性 code」应成功取码。

## 运维

### 升级

版本历史、每版升级说明与回滚指引见仓库根 [RELEASES.md](../RELEASES.md)。
升级前先备份（`scripts/backup_pg.sh` 等，见 backup-restore 手册）。

企业保留期清理使用 DBA 配置的 `imboy_enterprise_purge_worker` 角色；若该角色在迁移 264 之后才创建，由 DBA 对应用数据库执行 `scripts/grant_enterprise_direct_purge.sql`。缺少角色或授权时清理保持失败关闭。

**社区版**（`docker-compose.community.yml`）——迁移默认自动执行（`auto_migrate=true`，
新镜像启动时自动跑数据库迁移）：

```bash
cd deploy
# 1) 改 .env 的 IMBOY_VERSION 为目标版本（单一版本来源）
# 2) 拉新镜像并滚动更新（PG 不动，只重启 backend/admin）
docker compose -f docker-compose.community.yml pull
docker compose -f docker-compose.community.yml up -d imboy_backend imboy_admin
# 3) 查日志确认迁移成功（看到 started on port 9800 即就绪）
docker compose -f docker-compose.community.yml logs -f imboy_backend
```

**商务版**（prod.yml + sales-policy overlay）：

```bash
cd deploy
# 1) 拉新镜像
docker compose -f docker-compose.prod.yml -f docker-compose-sales-policy.yml pull
# 2) 滚动更新（PG 不动，只重启 backend/admin）
docker compose -f docker-compose.prod.yml -f docker-compose-sales-policy.yml up -d imboy_backend imboy_admin
# 3) 查日志确认迁移成功
docker compose -f docker-compose.prod.yml -f docker-compose-sales-policy.yml logs -f imboy_backend
```

蓝绿升级用 `bash scripts/imboy-deploy.sh api`（私有实现不接受直接调用；迁移时序与普通重启不同，
详见 [scripts/README.md](../scripts/README.md)）。

**Schema 迁移的回滚禁区（binary × DB 组合）**：数据库执行过破坏性迁移后，
**不得**把应用回滚到仍依赖被删结构的旧版本 binary。当前已知边界是
migration 210（`internal_hmac_only`）：删除 `enterprise_application.allowed_scopes`
与 `enterprise_oa_login_ticket.issuer_authentication` 两列，并把
`enterprise_application_credential.secret_digest` 改为可空。migration 210 之后的
DB schema **不能**搭配仍读取这些列的旧镜像运行（Internal API 请求会以
`undefined_column` 失败）。

down（回退到 209）在存在以下任一数据时**拒绝执行**（迁移内建守卫，勿绕过）：

- `enterprise_application` 存在任何行（存量应用）；
- `enterprise_oa_login_ticket` 存在任何行（存量登录票据）；
- 存在 `secret_digest IS NULL` 的凭证（210 之后新签发的 HMAC 凭证）。

因此回滚顺序固定为：① 回滚到仍兼容当前 schema 的 HMAC-only 版本；
② 无法回滚时 forward-fix 当前版本。**已有备份和维护窗口不等于可以执行 down**：
被删列的内容无法从现行数据重建，恢复旧 schema 必须另有经过验证的数据与凭证
恢复方案；通过清空业务表、删除凭证或修改历史迁移守卫来强行 down 是不可接受的。

回滚兼容性预检（**辅助手段**——零命中不代表兼容，还须确认目标版本对应的
迁移状态与认证协议）：

```bash
git grep -nE 'allowed_scopes|issuer_authentication' <目标commit> -- src test
```

命中需区分：`src/` 生产依赖（阻断回滚）；`priv/migrations/` 历史迁移与回滚
SQL（允许保留）；`test/` 明确固定在历史 schema 的 fixture 与"旧机制已移除"
负向断言（允许保留）。

### 备份

参见 `imboy/docs/guides/operations/deployment/backup-restore.md`。简化版：

```bash
# PG 逻辑备份
docker exec imboy_pg18 pg_dump -U imboy_user -Fc imboy_pro > backup_$(date +%F).dump
```

### 停止 / 启动 / 销毁

```bash
# 停止（保留数据）
docker compose -f docker-compose.prod.yml -f docker-compose-sales-policy.yml stop

# 启动
docker compose -f docker-compose.prod.yml -f docker-compose-sales-policy.yml start

# 销毁容器（保留数据卷）
docker compose -f docker-compose.prod.yml -f docker-compose-sales-policy.yml down

# ⚠️ 销毁一切包括数据（危险）
docker compose -f docker-compose.prod.yml -f docker-compose-sales-policy.yml down -v
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

启动方式按版本不同 / Startup differs by edition：

- **商务版**（prod.yml）：`docker compose up -d` 完成后，prometheus 和 grafana
  随核心服务同时启动。Prometheus and Grafana start together with the core services.
- **社区版**（community.yml）：监控栈（prometheus / alertmanager / loki / promtail
  / grafana）默认**不启动**，需要时加 `--profile monitoring` 启用：
  `docker compose -f docker-compose.community.yml --profile monitoring up -d`。

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

**63 条规则、21 个分组覆盖 / 63 rules in 21 groups：**（以 `imboy-alerts.yml` 为真源，2026-11-06 复核；下表为其中 14 组的阈值摘要，非全集——e2ee / license / online_users / red_packet / l4_sni / tsid / cache_sync 组未列出，全集以规则文件为准）

| 分组 / Group | 规则 / Rule | 触发条件（摘要）/ Threshold | 级别 |
|---|---|---|---|
| availability | `ImBoyBackendDown` | backend 离线 > 1min | critical |
| availability | `ImBoyBackendRestarted` | 后端刚重启（uptime < 2min） | warning |
| latency | `ImBoyMsgDeliveryLatencyHigh` | p99 > 500ms 持续 5min | warning |
| latency | `ImBoyMsgDeliveryLatencyCritical` | p99 > 2s 持续 2min | critical |
| http | `ImBoyHTTPErrorRateHigh` | 5xx > 1% 持续 5min | warning |
| http | `ImBoyHTTPErrorRateCritical` | 5xx > 5% 持续 2min | critical |
| erlang_vm | `ImBoyErlangMemoryHigh` | VM 内存 > 6GB 持续 5min | warning |
| erlang_vm | `ImBoyErlangMemoryCritical` | VM 内存 > 7.5GB 持续 2min | critical |
| erlang_processes | `ImBoyErlangProcessCountHigh` | 进程数 > 200000 持续 5min | warning |
| postgres | `ImBoyPostgresExporterDown` | exporter 离线 > 2min | critical |
| postgres | `ImBoyPostgresHighRollbackRate` | 回滚率 > 5% 持续 5min | warning |
| message | `ImBoyMsgRateDrop` | 消息速率陡降 > 90% 持续 5min | warning |
| websocket | `ImBoyWSConnectionsDrop` | WS 连接数较 10min 前下降 > 50% | warning |
| host_resources | `IMBoyHighMemoryUsage` | 主机内存使用率过高 持续 5min | warning |
| host_resources | `IMBoyHighMemoryUsageCritical` | 主机内存严重不足 持续 2min | critical |
| host_resources | `IMBoyHighDiskUsage` | 磁盘使用率过高 持续 5min | warning |
| host_resources | `IMBoyHighDiskUsageCritical` | 磁盘严重不足 持续 2min | critical |
| node_down | `IMBoyNodeDown` | Erlang 节点宕机 持续 5min | critical |
| pg_pool | `IMBoyDatabasePoolExhausted` | PG 连接池使用率过高 持续 5min | warning |
| pg_pool | `IMBoyDatabasePoolExhaustedCritical` | PG 连接池即将耗尽 持续 2min | critical |
| backup | `IMBoyBackupNotRunning` | 备份超过 24 小时未运行 | critical |
| backup | `IMBoyBackupJobFailed` | 备份作业上报失败 | warning |
| backup | `IMBoyRestoreDrillFailed` | 备份恢复演练失败 | critical |
| backup | `IMBoyRestoreDrillStalled` | 恢复演练 48 小时未成功（持续 30min 确认） | warning |
| tls | `IMBoyTLSCertExpiringSoon` | TLS 证书 14 天内到期 | warning |
| tls | `IMBoyTLSCertExpired` | TLS 证书已过期 | critical |
| tls | `IMBoyTLSCheckStale` | TLS 到期检查指标缺失（持续 6h） | warning |
| payment | `IMBoyPaymentCallbackErrorRateHigh` | 支付回调错误率 > 10% 持续 10min | critical |
| payment | `IMBoyPaymentSignFailureSpike` | 支付回调验签失败激增 持续 5min | critical |
| payment | `IMBoyPaymentReconcileMismatch` | 支付对账发现不一致 | critical |
| payment | `IMBoyPaymentReconcileRepairFailed` | 支付对账补单失败 | critical |
| payment | `IMBoyPaymentReconcileUnrepairable` | 支付流水找不到对应业务订单 | warning |
| payment | `IMBoyPaymentReconcileStalled` | 支付对账 job 已停摆 持续 10min | warning |

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
| PG 启动失败 / PG fails to start | 扩展安装失败：`bash preflight.sh` / Extension install failed |
| `cleanup-db` 门禁失败 / Cleanup identity gate fails | 保留原 `.env`、账号和数据；按阶段核对迁移、现有管理权限、账号及 HBA，见[账号手册](https://github.com/imboy-pub/imboy/blob/main/deploy/standalone/docs/DB_CLEANUP_ACCOUNT.md)（独立包 `docs/DB_CLEANUP_ACCOUNT.md`）。未提交事务回滚；提交后验证失败不自动删角色 / Preserve credentials and data; investigate migration, authority and authentication failures |
| Grafana 无数据 / Grafana no data | 检查 Prometheus target：`http://<ip>:9090/targets` / Check targets at `http://<ip>:9090/targets` |

## 下一步 / Next Steps

- ✅ G3 `scripts/sanity_check.sh` — 部署后 8 项自动验证 / Post-deploy 8-item sanity check _(done)_
- ✅ G5 `prometheus/rules/imboy-alerts.yml` — SLO 告警规则 / SLO alerting rules _(done)_
- ✅ G1 `.github/workflows/release.yml` — 镜像构建发布自动化 / Image build-push automation _(done — Phase 1：tag `v*` → 构建 candidate 镜像推 GHCR `<version>-rc` digest 并输出 Release Identity 三元组；Golden Install / Golden Upgrade 门禁与正式 semver tag / GitHub Release 属 Phase 2（P2-R1）接入)_
- ⏳ G4 `.github/dependabot.yml` + Trivy SBOM 扫描 / Dependabot + Trivy SBOM scan _(pending)_
- ⏳ P0-8 Sentry DSN 生产注入文档化 / Sentry DSN production injection docs _(pending)_
