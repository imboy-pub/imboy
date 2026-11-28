# Compose 配置参考与运维 / Compose configuration and operations

一键将 IMBoy 部署到一台 Linux 服务器。
One-command deployment of IMBoy to a Linux server.

> 本地开发与评估统一从仓库根目录运行 `bash scripts/dev_setup.sh`，见
> [本地开发指南](local-development.md)。本页保留生产安装、证书、升级与恢复流程。
>
> 文档分工：本页是 **Docker Compose 客户安装入口**，使用 `deploy/.env.example`；[day1-quickstart.md](day1-quickstart.md) 与 [deployment.md](deployment.md) 是裸机/release 或定制运行时参考，不能混用其配置模板和启动步骤。备份恢复见 [backup-restore.md](backup-restore.md)。

## 根 Compose 服务组合 / Root Compose profiles

首个 profile 选环境：`dev`、`prod` 或 `test`。安装器始终使用 `--profile prod`，再按 `deploy/.env` 叠加服务。`COMPOSE_PROFILES` 可附加 `monitoring` 和 `enterprise-direct`；RTC、TURN、SNI 与 Uptrace 由专属开关选择。`pro` 是仅启动 `prod_imboy_pg18` 的兼容入口，不是生产整栈入口。

Choose the environment first: `dev`, `prod`, or `test`. The installer always invokes `--profile prod` and adds services selected in `deploy/.env`. Use `COMPOSE_PROFILES` for `monitoring` and `enterprise-direct`; use the dedicated flags for RTC, TURN, SNI and Uptrace. The legacy `pro` profile starts only `prod_imboy_pg18`, not the full production stack.

| Profile | 启用条件 / Selection | 对外 HTTPS 和 TURN / Published HTTPS and TURN |
|---|---|---|
| `prod` | 默认 / Default | Nginx TCP 443 |
| `rtc` | 配合 `prod` 启用 LiveKit / Add to `prod` for LiveKit | TCP 7881 and UDP 50000–50200 |
| `prod + prod-turn` | `LIVEKIT_TURN_ENABLED=true` | Nginx TCP 443, LiveKit UDP 3478 and TLS TCP 5349 |
| `prod + prod-sni` | TURN 已启用且 `LIVEKIT_SNI_ENABLED=true` / TURN enabled with shared TLS | HAProxy TCP 443 for HTTPS and TURN TLS, LiveKit UDP 3478 |
| `monitoring` | 可选 / Optional | Prometheus、Grafana 和三类采集器 / Prometheus, Grafana, and three collectors |
| `enterprise-direct` | 可选 / Optional | 保留独立企业验证身份和配置 / Separate enterprise verification identity and configuration |

直接预览带共享 443 的服务时，须停用默认网关，且不可同时加 `rtc` 或 `prod-turn`；它们与 `prod-sni` 重复绑定端口。安装器会自动处理，正式安装仍从统一入口运行。单独使用旧 `prod-turn` 或 `prod-sni` 的历史命令仍兼容。

For direct preview of a shared-443 stack, suppress the default gateway and do not add `rtc` or `prod-turn` to `prod-sni`; they would bind overlapping ports. The installer sets this automatically. Use the unified installer for deployment. Standalone historical `prod-turn` and `prod-sni` commands remain compatible.

```bash
IMBOY_PROD_DEFAULT_GATEWAY_PROFILE=prod-default-gateway docker compose \
  --env-file deploy/.env --profile prod --profile prod-sni \
  --profile monitoring --profile enterprise-direct config --services
```

安装后从仓根运行 `bash scripts/sanity_check.sh`。旧社区部署按原文件组合显式传入 `--compose-file`。

After installation, run `bash scripts/sanity_check.sh` from the checkout root. For legacy deployments, pass the original file list with `--compose-file`.

## 隔离测试 / Isolated test stack

从仓库根目录将 `.env.example` 复制为 `.env.test`，填写独立测试凭据和所需镜像。测试项目使用自己的网络和数据卷。API、S3 和附件 Edge 默认只监听回环地址的 19800、13900 和 13991 端口。

From the repository root, copy `.env.example` to `.env.test` and set dedicated test credentials and image references. The test project uses its own network and volumes. The API, S3 and attachment Edge bind to loopback ports 19800, 13900 and 13991 by default.

```bash
docker compose -p imboy-test --env-file .env.test --profile test up -d
docker compose -p imboy-test --env-file .env.test --profile test --profile test-rtc --profile test-monitoring up -d
docker compose -p imboy-test --env-file .env.test --profile test --profile test-uptrace up -d
```

`test-rtc` 启用本机 LiveKit，`test-monitoring` 启用抓取测试 API 的 Prometheus。生产环境对应的可选组合为 `prod + rtc` 和 `prod + monitoring`。

`test-rtc` enables loopback LiveKit. `test-monitoring` enables Prometheus for the test API. The corresponding production combinations are `prod + rtc` and `prod + monitoring`.

`test-uptrace` 只连测试数据库，ClickHouse 持久化到测试项目的卷，Redis 仅在追踪的内部网络使用。将部署模板中的 Uptrace 凭据项填入 `.env.test`，使用独立测试值。控制台默认监听 `http://127.0.0.1:14318`。若要验证应用主动上报，在同一文件设置 `IMBOY_UPTRACE_DSN`（示例格式 `http://<测试项目令牌>@uptrace/1`）和 `IMBOY_UPTRACE_OTLP_ENDPOINT=http://uptrace:80`。

`test-uptrace` connects to the test database, keeps ClickHouse data in a project volume, and confines Redis to the tracing network. Set dedicated test values for the Uptrace credentials from the deployment template in `.env.test`. The dashboard defaults to `http://127.0.0.1:14318`. To verify application traces, set the project DSN and `IMBOY_UPTRACE_OTLP_ENDPOINT=http://uptrace:80` in the same file.


<a id="operations-lifecycle"></a>

## 运维操作入口 / Operations lifecycle

| 环节 | 权威入口 |
|---|---|
| 安装与健康检查 | 本页[客户配置](#customer-input)、`install.sh`；记录 Release Identity，验证业务接口 |
| 备份与恢复 | [备份恢复](backup-restore.md)（PG、Garage、恢复后的数据及业务验证） |
| 升级与回滚 | [RELEASES.md](../../../../RELEASES.md) 与本页运维章节；裸机/定制 release 见[部署脚本](deploy-script.md)；[正式版迁移草案](../upgrade-runbook.md)仅在适用版本生效 |
| 监控 | [监控指南](monitoring.md)，按需启用 |

升级前验证备份可恢复；记录版本、镜像摘要、演练耗时及真实消息/附件验收。社区首次安装可显式使用 `--init-cleanup-db` 创建缺失清理账号，商务/外部数据库由 DBA 开通。宿主机与 Compose 配置不可混用。

## 原生 Shell 固定版本交付包 / Pinned native Shell kit

[原生 Shell 安装材料](native-shell/) 提供单机 systemd 的安装、升级、
备份恢复、配置模板与陌生人验收步骤，安装工具不依赖 Python 或 Docker。
该工具包固定于 `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af` /
`1.0.0-alpha.83` / 迁移头 `243`，初始材料版为 `PAYLOAD_READY=no`。
发布方须先供应原生产物并完成真实演练；当前 main / alpha.84 的新增安装合同
需要单独适配，不能只修改版本号或模板哈希后使用。

The native Shell kit is a separate, source-pinned installation toolkit for
alpha.83. It contains no runtime binaries or site secrets. Publisher assembly
and real-host acceptance are required; it does not certify current main or
alpha.84. Follow its own templates and manuals for that frozen deployment scope.

## CS Widget 静态服务

标准社区部署已包含独立 `imboy_widget` 服务，使用
`ghcr.io/imboy-pub/imboy-widget:${IMBOY_VERSION}`；源码与发布镜像定义在
imboyadmin 仓的 `Dockerfile.widget`。Widget 的域名、缓存、SSE、安装白名单及
升级回滚统一见[Widget 运行手册](customer-service-widget.md)。
本地联调使用仓库根目录 `bash scripts/dev_setup.sh --with-widget`。

`deploy/widget/` 保留独立镜像及合成升级演练，不再提供另一套 Compose 入口：

```bash
bash deploy/widget/dryrun.sh --dist /path/to/imboyadmin/dist-widget --skip-sse
```

从仓库根目录执行。演练只创建自己的 `csww-widget-nginx` 容器，绑定回环端口
`18900`，完成安装、重启、静态资源一致性与回滚检查后移除本次容器；同名容器已存在时拒绝运行。
需要 SSE 验证时按脚本 `--help` 提供参数。这个旧式镜像采用 `loader.js` / `assets/`
短缓存与 `must-revalidate`；标准发布镜像的缓存合同以 Widget 运行手册为准。

Widget 密钥可保存在部署机的私有目录，以权限 `0600`、运行用户可读的文件和只读挂载注入。若现场使用私有覆盖配置，在根 Compose 的 `prod_imboy_backend` 服务设置 `IMBOY_CS_WIDGET_SUBJECT_KEY_FILE`、`IMBOY_CS_WIDGET_IDENTITY_KEYS_FILE`，值为对应的容器路径。每个密钥只配置一种来源。

Widget keys may be supplied as read-only mounts from a private host directory. Set the two `_FILE` variables on `prod_imboy_backend`, use mode `0600` and allow the runtime user to read the files.

Uptrace 使用根 Compose 的 `uptrace` profile。社区源码安装时在 `deploy/.env` 设置 `UPTRACE_ENABLED=true`，填写 Uptrace 域名和管理员邮箱后再执行安装。专用 Redis 和 ClickHouse 只在追踪内部网络，Uptrace 通过生产网络连接 PostgreSQL 与 Nginx。

Set `UPTRACE_ENABLED=true` and configure the trace domain and administrator email to enable the root Compose `uptrace` profile through the community source installer. Redis and ClickHouse stay on the private tracing network.

商务策略从根 Compose 的扩展区渲染，包含严格 E2EE 和频道配置。手工维护前，从 `deploy/` 在当前 Bash 终端准备临时文件：

```bash
business_policy="$(mktemp)"
bash render-compose-extension.sh imboy-business-policy > "$business_policy"
trap 'rm -f -- "$business_policy"' EXIT
```

## 前置条件

- Linux x86_64，文档基准环境 **Debian 13 (Trixie)**（其他发行版同样适用）
- **PostgreSQL ≥ 18（QR 扫码登录硬性要求，LEE-5）**：QR 登录凭据的原子领取
  依赖 PG18 的 `UPDATE ... RETURNING OLD` 语法；自带数据库（Helm/自建）部署
  必须满足 `server_version_num >= 180000`，否则后端在首次使用时明确
  拒绝 QR 登录（HTTP 200 + 信封业务码 503 `ERR_SERVICE_UNAVAILABLE`，
  fail closed，绝不降级缓存——监控告警挂业务码而非 5xx）。社区 compose 栈默认
  使用带扩展的 PG18 镜像（`ghcr.io/imboy-pub/imboy-pg18`），并要求库内预装
  timescaledb/postgis/pg_jieba/pgcrypto/pg_trgm 扩展。PG 大版本原地升级
  （如 17→18）后须滚动重启后端节点，版本门禁结果按节点缓存、不随库升级自动刷新。
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

以 [`install.sh`](../../../../deploy/install.sh)、[`preflight.sh`](../../../../deploy/preflight.sh) 和 [`.env.example`](../../../../deploy/.env.example)
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
[清理数据库账号手册](https://github.com/imboy-pub/imboy/blob/main/docs/guides/operations/deployment/standalone/DB_CLEANUP_ACCOUNT.md) 初始化并验证。
独立交付包的离线副本位于包根 `docs/DB_CLEANUP_ACCOUNT.md`。

安装器在已有 `.env` 后自动补齐空值或 `CHANGE_ME` 内部密钥，保留已有非占位值：
数据库密码、JWT/AES、管理 Cookie、edge token、密码盐、握手密钥/IV、Grafana、
LiveKit、Garage，以及启用 Uptrace 时的六项内部密钥；登录 RSA 文件生成到
`${DATA_DIR}/backend_priv/keys/`，已有私钥保留。DNS、第三方账号/凭据和
数据库管理权限须由客户或交付方准备；脚本不会自动提权。正式 TLS 由 ACME 在网络条件满足后签发。

| 安装命令 / Install command | 配置 / Configuration |
|---|---|
| `bash deploy/install.sh` | 使用根 Compose / Use root Compose |
| `bash deploy/install.sh --site-compose` | 使用商务渠道交付的 `deploy/docker-compose.prod.yml`（不在开源仓）/ Use the commercially delivered site Compose file |

根 Compose 默认只签发 API、Admin 和 Widget 证书。设置 `LIVEKIT_ENABLED=true` 加启信令域，设置 `LIVEKIT_TURN_ENABLED=true` 同时启用 TURN 域。启用 Uptrace 时还需配置其域名和管理员资料。

功能策略使用 `IMBOY_PRODUCT_PROFILE=standard` 或 `audited`。`audited` 预设包含归档消息检索、导出与完整审计。已有现场配置的旧值继续按原策略解释。

Use `IMBOY_PRODUCT_PROFILE=standard` or `audited`. The audited preset includes archived-message search, export and full audit. Existing installations keep the behavior of their previous values.

Root Compose requests API, Admin and Widget certificates by default. Set `LIVEKIT_ENABLED=true` for the signaling domain and `LIVEKIT_TURN_ENABLED=true` to also enable the TURN domain. Uptrace uses its own domain and administrator settings.

### 清理数据库身份 / Cleanup database identity

社区门禁使用 [`init-cleanup-db.sh`](../../../../deploy/init-cleanup-db.sh)：按运行中的 backend 镜像精确
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
[清理数据库账号手册](https://github.com/imboy-pub/imboy/blob/main/docs/guides/operations/deployment/standalone/DB_CLEANUP_ACCOUNT.md)。

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
[LiveKit TURN/TLS 443 一键切换手册](livekit-turn-443-l4-sni.md)。

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
[customer-service-widget.md](customer-service-widget.md)。

Uptrace 默认关闭。启用时在 `.env` 设置 `UPTRACE_ENABLED=true`，填写
`UPTRACE_DOMAIN` 和 `UPTRACE_ADMIN_EMAIL`；其余 Uptrace 密钥由安装器生成。该 overlay
只采集 Prometheus 指标，不采集业务日志，且故障不会成为 IMBoy 核心服务的依赖。

可选参数（`bash install.sh --help` 查看全部）：

- `--init-cleanup-db`：仅社区版，迁移后以已有数据库管理权限创建缺失的清理 LOGIN；
  默认只验证，已有账号不改密或补权。商务/外部数据库由 DBA 另行执行账号手册。
- `--admin-phone` / `--admin-password`：装完后在 backend 容器内经
  `imboy_ctl adm create` 创建超管，无需浏览器走 `/setup` 向导（无浏览器纯脚本部署）
- `--yes, -y`：跳过所有确认（含 Docker 缺失时的安装确认，适合自动化）
- `--site-compose`：改用现场 `docker-compose.prod.yml` + sales-policy
  overlay（缺文件时提示商务索取方式）

### API 与客服上传地址

`IMBOY_BASE_URL` 使用 `https://<API_DOMAIN>`，供 API、edge 回退及其他业务使用。社区 Compose 为旧 Widget/Seat 同源上传单独设置 `IMBOY_WIDGET_BASE_URL=https://<CS_WIDGET_DOMAIN>`；缺少该地址时旧上传不下发 URL。direct-v1 上传使用独立的存储授权。Helm 的 `widgetHost` 和 `widgetBaseUrl` 默认为空，启用旧同源上传时须配置同一实际 Widget 网关。

社区安装器以 Compose 实际解析的域名执行 preflight，并临时派生 API origin；商务安装
与独立 preflight 读取 `.env`。因此仍须把 `.env` 的 `IMBOY_BASE_URL` 配成实际 API origin。

### Shell 配置检查 / Configuration checks

以下命令不启动服务、不申请证书。完整 preflight 需要已有内部密钥/RSA 的 `.env`；
刚复制模板时出现密钥占位符或 RSA 缺失错误是预期，首次安装按上面的安装器流程执行。
预检使用现场配置。根 Compose 禁用支付时使用 `--without-payment`：

社区版：

```bash
cd /path/to/imboy/deploy
# 社区版：配置、宿主机/Docker 与 RTC/TURN DNS 探测
bash preflight.sh --without-payment --docker --dns &&
  docker compose --env-file .env --profile prod -f ../docker-compose.yml config -q
```

商务版（与上面二选一，先取得对应的 `prod.yml`）：

```bash
cd /path/to/imboy/deploy
bash preflight.sh --docker --dns &&
  docker compose --env-file .env -f docker-compose.prod.yml \
    -f "$business_policy" config -q
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

正式部署入口为 `bash deploy/install.sh`，现场 Compose 使用 `--site-compose`。宿主机编排只用
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
| 开发 / CI | `scripts/smoke_8step.sh` 及相关测试仍保留其既有 Python/测试依赖；8 步链使用开发验证码、测试账号和直接 RPC，Golden 准备步骤会切换 `IMBOYENV=dev`，不能拿到正式客户实例上当通用验收脚本 |

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
bash scripts/test/init_letsencrypt_test.sh
bash deploy/preflight.sh --self-test
```

安装回归使用真实 Bash/jq/OpenSSL 和本地 Docker/网络测试桩，验证解析、失败关闭、
调用顺序和幂等保护；它不能证明真实 Docker、镜像发布、ACME、数据库迁移或客户网络
已可用。在没有 Python 的干净测试机/隔离根目录中执行同样命令，可复核宿主机无 Python
依赖。真实部署验收另按上表执行；开发与 CI 专用测试不属于此无 Python 承诺。

<a id="rtc-turn-regression"></a>

**RTC/TURN 回归归属 / RTC/TURN regression ownership**（在源码仓库执行）：

以下套件均由 Backend CI 的 `Standalone Community Delivery` job 阻塞执行；
preflight/Compose 和真实 WSS 各有独立步骤，便于区分配置失败与容器信令失败。

| 套件 | 验证范围 | 执行环境与边界 |
| --- | --- | --- |
| `lk_preflight_rtc_turn_test.sh` | 五域配置中的 RTC/TURN 正负例、DNS 失败关闭、TURN 开关/证书/端口门；真实 Compose 渲染的唯一固定 LiveKit、禁用组件、同域 live/archive 只读挂载、TURN 参数与端口；模板占位符 | DNS/端口用桩；Compose 只渲染，不启动容器，不证明 TURN relay 或媒体可用 |
| ~~`community_delivery_config_test.sh`~~（已退役） | 原静态部署配置/Nginx 模板/续期文件兼容检查已并入 `install_one_click_test.sh`（安装器离线控制流，含 Nginx/证书 fixture）与 `lk_preflight_rtc_turn_test.sh`（Compose 渲染与模板占位符） | 文件系统模拟，不替代真实 Compose 渲染或容器挂载 |
| `init_letsencrypt_test.sh` | 首签前 HTTP-01 就绪、临时证书、失败恢复、`nginx -t` 与 reload 顺序 | Docker/OpenSSL 桩，不向公共 CA 申请证书 |
| `community_rtc_wss_shell_test.sh` | WSS 测试运行器的 CID、Compose 转义、失败诊断与清理 | Docker 桩，不计为真实 TLS/WSS 通过 |
| `community_rtc_wss_test.sh --docker` | 真实 Nginx/LiveKit 的启动、五域 HTTP-01 路由、严格 TLS、认证 WSS 握手和既有代理路由 | 需要 Docker daemon 与固定镜像；不启用 TURN overlay，不替代双端媒体验收 |

```bash
bash scripts/test/lk_preflight_rtc_turn_test.sh
```

预检套件的 Compose 阶段需要 Docker CLI 和 Compose（仓库最低要求 v2.23.1，
支持 `configs.content`），无需 daemon 或镜像下载。preflight 仍检查真实宿主资源，
至少需要 4 GiB 内存和 10 GiB 可用磁盘。测试只使用合成凭据，版本取根目录 `VERSION`，
并隔离宿主配置覆盖。缺少 Compose 时先完成 preflight/模板检查，再报告
`COMPOSE=NOT_RUN`、退出 77；已有断言失败则仍退出 1。渲染失败或空输出退出 1，
不得把空模型上的否定匹配记为通过。CI 直接保留非零结果，不豁免或吞掉退出码。

All suites above are blocking steps in the existing community delivery job. The preflight
suite retains its configuration and TURN model checks; WSS tests do not replace them.
Compose rendering needs the CLI/plugin only, while preflight still checks host resources.
Exit 77 means the environment is unavailable, never a pass; failed or empty rendering exits 1.

**容器 RTC 回归 / Container RTC regression**：

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

> 首次安装使用 `install.sh` 或 `install.sh --site-compose`。本节仅供已完成内部密钥/RSA
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
bash preflight.sh --docker --dns
docker compose --env-file .env -f docker-compose.prod.yml \
  -f "$business_policy" config -q
```

任何 `ERROR` 都必须修复后再继续。

### 3. 创建网络 & 启动

```bash
(
  set -euo pipefail
  IMBOY_START_ARGS=(--env-file .env -f docker-compose.prod.yml -f "$business_policy")
  docker compose "${IMBOY_START_ARGS[@]}" pull
  bash prepare-backend-runtime.sh "${IMBOY_START_ARGS[@]}"
  docker network create imboy-network 2>/dev/null || true
  docker compose "${IMBOY_START_ARGS[@]}" up -d
)
```

拉取、权限准备与启动共用 `IMBOY_START_ARGS`；权限准备失败时子 shell 立即退出，
不会执行 `up`。启用其他 overlay 时，在此数组中追加与预检相同的参数。
Pull, permission preparation and startup use the same `IMBOY_START_ARGS`. The
subshell stops before `up` if preparation fails; add the same extra overlays as
preflight to this array.



**证书未完成时补做签发**（全部五域的 DNS/HTTP-01 条件均须满足；使用与启动相同的 Compose 组合）：

```bash
COMPOSE_FILES="docker-compose.prod.yml $business_policy" \
  bash nginx/init-letsencrypt.sh
```

启用 Uptrace/TURN 时，启动、检查和签证命令均须追加相同 overlay；优先重跑安装器保持组合一致。

签发成功后，`imboy_certbot` 会在后台自动续期，nginx 定期 reload 加载新证书，无需再手动执行。

### 4. 查看启动状态

```bash
docker compose -f docker-compose.prod.yml -f "$business_policy" ps
docker compose -f docker-compose.prod.yml -f "$business_policy" logs -f imboy_backend
```

首次启动需要等待 PG 健康检查 + DB 迁移，约 30-60 秒；就绪判定以 `/healthz` 探测（`curl -fsS http://127.0.0.1:9800/healthz` 返回 ok）为准，后端不承诺输出固定的启动成功日志文案（`install.sh` 同样以 `/healthz` 判定）。

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

### 后端容器运行身份 / Backend container identity

后端 API 镜像默认以 `10001:10001` 运行。ERTS、`bin` 下的启动脚本和 BEAM 目录
保持 root 所有；`HOME=/home/imboy`、日志、relx 生成配置目录和应用 `priv` 目录可写。
当前 `priv` 的可写范围也包含 `priv/workers/*.escript`，用于保留开发密钥、许可证与试用状态流程；
生产密钥仍从只读 `/opt/imboy/priv_runtime` 挂载读取。API 进程丢弃全部 Linux capabilities
并启用 `no-new-privileges`；临时目录、HOME 和 relx 生成目录使用受限 tmpfs。
不需要写入应用目录的 edge 与 enterprise proof 服务采用只读根文件系统。

安装器与 Golden 升级在拉取镜像之后、启动服务之前调用
`prepare-backend-runtime.sh`。它读取实际 Compose 的镜像和两个 bind 目录，
用无网络、只读根文件系统的临时初始化容器，给 UID 10001 设置最小 POSIX ACL：
私钥目录只读、日志可读写。宿主所有权和密钥字节不变，既有 `root:600` 私钥及
root 日志无需改成全员可读写。安装器重复执行 `chmod 600` 后会重新准备 ACL；
`stat` 显示的组位是 ACL mask，实际授权应以 `getfacl` 为准。

手工 Compose 升级也必须在 `pull` 后、`up` 前，用相同 `-f`/`--env-file`
参数执行该脚本。新镜像拒绝 Compose 的 root/自定义 UID 覆盖；旧 root 镜像
回滚时保持原行为。脚本拒绝重叠目录、符号链接、非 bind 挂载和不可用的 ACL。
商务版、自定义 Secret/PEM 挂载或其他 UID 的部署须先审核其实际路径和权限；
两个标准 bind 之外的文件必须另外保证 UID 10001 可读，不能把预检成功当成
所有外部挂载已验收。安装包与新镜像须一起升级。

The backend API image runs as UID/GID `10001:10001`. ERTS, launch scripts under `bin`,
and BEAM directories remain root-owned. Home, logs, relx-generated configuration
and the app `priv` directory are writable, including `priv/workers/*.escript`, preserving
the existing development-key, license and trial-state workflows. Production keys are read
from the read-only `/opt/imboy/priv_runtime` mount. The API drops all Linux capabilities,
enables `no-new-privileges`, and uses bounded tmpfs for temporary paths. The edge and
enterprise-proof services, which do not need writable application assets, use read-only root filesystems.
The installer prepares the two
resolved bind mounts with narrowly scoped POSIX ACLs before starting services.
This preserves host ownership, private-key contents and the read-only secret mount.
Run the same helper between `pull` and `up` for manual upgrades. Unsupported ACLs,
symlinks, overlapping mounts or custom runtime-user overrides stop the upgrade;
review custom/business mounts separately. Upgrade the installation kit with the image.

`Backend Container Runtime` 在 PR 中构建实际镜像，并验证非 root relx/ERTS 启动、
旧私钥/日志权限、只读挂载、HOME/cookie/临时目录和重复准备。其 `console_clean`
不启动业务应用或数据库；完整安装/升级及业务验收仍由发布 Golden 门完成。
The real-image PR smoke uses `console_clean`; it verifies runtime permissions and
release startup, while full application/database acceptance remains in the release Golden gates.

### 升级

版本历史、每版升级说明与回滚指引见仓库根 [RELEASES.md](../../../../RELEASES.md)。
升级前先备份（`scripts/backup_pg.sh` 等，见 backup-restore 手册）。

企业保留期清理使用 DBA 配置的 `imboy_enterprise_purge_worker` 角色；若该角色在迁移 264 之后才创建，由 DBA 对应用数据库执行 `scripts/grant_enterprise_direct_purge.sql`。缺少角色或授权时清理保持失败关闭。

**社区版**（`../docker-compose.yml`）——迁移默认自动执行（`auto_migrate=true`，
新镜像启动时自动跑数据库迁移）：

```bash
cd deploy
# 1) 改 .env 的 IMBOY_VERSION 为目标版本（单一版本来源）
# 2) 拉新镜像并滚动更新（PG 不动，只重启 backend/admin）
docker compose --env-file .env --profile prod -f ../docker-compose.yml pull
bash prepare-backend-runtime.sh --env-file .env --profile prod -f ../docker-compose.yml
docker compose --env-file .env --profile prod -f ../docker-compose.yml up -d prod_imboy_backend prod_imboy_admin
# 3) 确认迁移成功（curl http://127.0.0.1:9800/healthz 返回 ok 即就绪，不依赖固定启动日志文案）
docker compose --env-file .env --profile prod -f ../docker-compose.yml logs -f prod_imboy_backend
```

**商务版**（prod.yml + sales-policy overlay）：

```bash
cd deploy
# 1) 拉新镜像
docker compose -f docker-compose.prod.yml -f "$business_policy" pull
bash prepare-backend-runtime.sh -f docker-compose.prod.yml -f "$business_policy"
# 2) 滚动更新（PG 不动，只重启 backend/admin）
docker compose -f docker-compose.prod.yml -f "$business_policy" up -d imboy_backend imboy_admin
# 3) 查日志确认迁移成功
docker compose -f docker-compose.prod.yml -f "$business_policy" logs -f imboy_backend
```

蓝绿升级用 `bash scripts/imboy-deploy.sh api`（私有实现不接受直接调用；迁移时序与普通重启不同，
详见 [scripts/README.md](../../../../scripts/README.md)）。

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
docker compose -f docker-compose.prod.yml -f "$business_policy" stop

# 启动
docker compose -f docker-compose.prod.yml -f "$business_policy" start

# 销毁容器（保留数据卷）
docker compose -f docker-compose.prod.yml -f "$business_policy" down

# ⚠️ 销毁一切包括数据（危险）
docker compose -f docker-compose.prod.yml -f "$business_policy" down -v
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

## TURN 证书续期 / TURN certificate renewal

Certbot 更新内置证书，Nginx 周期性重新加载。LiveKit v1.13.7 在创建 TURN TLS 监听时读取证书。续期后在维护窗口重启当前 Compose 项目的 LiveKit 服务，再从外部核对 TURN 证书指纹及通话。普通 TURN 使用 TCP 5349，共用 443 模式使用 TCP 443。先确认服务属于当前实例，重启期间媒体连接会中断。外部证书目录按其交付者的续期流程更新。

Certbot renews bundled certificates and Nginx reloads them periodically. LiveKit v1.13.7 loads its certificate when it creates the TURN TLS listener. After renewal, restart the LiveKit service in the active Compose project during a maintenance window. Verify the TURN certificate fingerprint and a call from outside the host. Use TCP 5349 for direct TURN and TCP 443 for shared-port mode. Confirm the service belongs to the intended deployment. Restarting interrupts media connections. Follow the certificate provider's renewal procedure for external directories.

[LiveKit v1.13.7 TURN listener](https://github.com/livekit/livekit/blob/v1.13.7/pkg/service/turn.go#L208)

## 可观测性 / Observability

生产环境启用 `monitoring` profile 后，按[监控指南](monitoring.md)通过 SSH 隧道检查采集目标和仪表盘。规则阈值以 [Prometheus 配置](../../../../deploy/prometheus/rules/imboy-alerts.yml) 为准。

Enable the `monitoring` profile and follow the [monitoring guide](monitoring.md) to check scrape targets and dashboards over an SSH tunnel. Alert thresholds are defined in the [Prometheus rules](../../../../deploy/prometheus/rules/imboy-alerts.yml).

## 故障排查 / Troubleshooting

| 现象 / Symptom | 排查 / Troubleshooting |
|------|------|
| `imboy_backend` 反复重启 / keeps restarting | `docker compose logs imboy_backend` 查看 PG 连接 / 配置 / 迁移冲突 / Check PG connection, config, migration conflicts |
| certbot 证书签发失败 / TLS cert fails | DNS 未指向本机 / 80 端口被占用 / Let's Encrypt 限流 / `CERTBOT_EMAIL` 未填；查 `docker compose logs imboy_certbot` / DNS not pointing here, port 80 blocked, LE rate-limit, missing CERTBOT_EMAIL |
| 管理后台 404 或白屏 / Admin 404 or blank | 检查 `VITE_API_BASE` 是否指向 `https://${API_DOMAIN}` / Check `VITE_API_BASE` |
| WebSocket 连接 403 / WS 403 | `JWT_KEY` 不一致 / nginx WS 升级（Upgrade/Connection）头未透传 / `JWT_KEY` mismatch, check nginx WebSocket upgrade headers |
| PG 启动失败 / PG fails to start | 扩展安装失败：`bash preflight.sh` / Extension install failed |
| `cleanup-db` 门禁失败 / Cleanup identity gate fails | 保留原 `.env`、账号和数据；按阶段核对迁移、现有管理权限、账号及 HBA，见[账号手册](https://github.com/imboy-pub/imboy/blob/main/docs/guides/operations/deployment/standalone/DB_CLEANUP_ACCOUNT.md)（独立包 `docs/DB_CLEANUP_ACCOUNT.md`）。未提交事务回滚；提交后验证失败不自动删角色 / Preserve credentials and data; investigate migration, authority and authentication failures |
| Grafana 无数据 / Grafana no data | 检查 Prometheus target：`http://127.0.0.1:9090/targets` / Check targets at `http://127.0.0.1:9090/targets` |
