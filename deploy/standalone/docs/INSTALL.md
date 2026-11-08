# 社区版独立安装手册 / Installation

本手册适用于本 ZIP 的单机社区版安装。客户只需要交付包、现场配置和可拉取的发布镜像，不需要完整源码仓库，也不需要在宿主机安装 Python、Erlang、Node.js 或 PostgreSQL 客户端。

源码基线以包根 `SOURCE_COMMIT` 为准，源码与模板版本分别见 `VERSION` 和 `deploy/.env.example`。模板中的版本号不证明相应镜像已经发布、客户有权限拉取或镜像已通过业务验收。

包内复用所选提交的现有安装器，文件来源见 `review/SOURCE_FILES.tsv`，完整依赖见 [DEPENDENCIES.md](DEPENDENCIES.md)。**离线检查与真实服务器验收分开记录；Docker、TLS、数据库和客户端业务验收须在现场执行。**

## 1. 本包的适用范围

| 项目 | 本包约定 |
|---|---|
| 部署形态 | 一台 Linux x86_64 服务器；Debian 13 为上游文档基准 |
| 编排 | Docker 24+、Docker Compose v2.23.1+ |
| 资源 | 上游最低要求 4 GB 内存、10 GB 可用磁盘；建议至少 8 GB / 20 GB，并另计业务数据、备份和日志增长 |
| 安装入口 | 包根目录 `bash install.sh`，调用现有 `deploy/install.sh --edition community` |
| 数据路径 | 保持 `DATA_DIR=./data`；实际位置为安装目录内的 `deploy/data/` |
| 后端宿主端口 | 保持 `BACKEND_PORT=9800`，绑定 `127.0.0.1` |
| 默认关闭 | Uptrace、enterprise-direct、embedded TURN；监控 profile 默认关闭 |
| 社区版支付 | Compose 固定关闭外部支付网关；不需要填支付商户凭据 |
| 分发内容 | Shell 脚本、配置、模板、说明和检查材料；不含容器镜像、客户端安装包、客户密钥或商业授权 |
| 不在本包内 | 商务版 `docker-compose.prod.yml`、源码构建链、Helm、多节点、L4 SNI 共用 443 切换 |

如需独立数据盘，可把数据盘挂载到固定安装目录的 `deploy/data/`，仍保留 `DATA_DIR=./data`。不要在已有数据时改成另一个空目录，也不要直接把 `DATA_DIR` 改成绝对路径来绕过本包约定。

## 2. 安装前要拿到哪些现场信息

| 信息 | 谁准备 | 要求 |
|---|---|---|
| API、Admin、Widget、RTC、TURN 五个域名 | 客户 / DNS 管理员 | 五个互不相同的纯域名，不含协议、端口、路径或尾斜杠 |
| 公网入口 IP 与 DNS | 客户 / 网络管理员 | 五域 A 记录指向正确入口；若配置 AAAA，IPv6 路径也要可达 |
| `CERTBOT_EMAIL` | 客户 | 实际维护的证书账号邮箱 |
| `IMBOY_BASE_URL` | 部署人员 | 精确填写 `https://<API_DOMAIN>`，无尾斜杠 |
| 发布镜像身份和拉取权限 | 交付方 | backend、admin、widget、pg18 四类镜像对应本次交付；记录实际 digest |
| 独立清理数据库 LOGIN | DBA / 部署人员 | 填写 `IMBOY_DIRECT_CLEANUP_PG_USER`；不能复用 `POSTGRES_USER` |
| 清理 LOGIN 的密码 | DBA / 部署人员 | 已有账号填匹配密码；新账号按数据库清理账号说明开通 |
| 验收客户端和两个测试账号 | 交付方 / 客户 | 客户端能配置本实例地址；使用正式开户、登录流程 |
| SMTP / 短信 | 客户 | 只在实际使用时填写完整配置；未配置时不要把验证码业务记为已验收 |
| 维护窗口、备份位置、联系人 | 客户 | 安装失败、升级回滚和账号开通有明确负责人 |

内部 JWT、AES、管理 Cookie、edge token、密码盐、LiveKit、Garage 等随机密钥及登录 RSA 由原安装器在客户机器上生成。不要在交付 ZIP 中预置真实值。已有实例必须沿用原 `.env`、RSA 和存储凭据，不能用新模板替换。

独立清理 LOGIN 的数据库开通不是安装器自动完成的工作。首次数据库迁移后，按 [DB_CLEANUP_ACCOUNT.md](DB_CLEANUP_ACCOUNT.md) 完成最小授权和 TCP 密码登录验证，再判定这部分交付完成。

## 3. 网络和端口

| 端口 | 方向 / 范围 | 用途 |
|---|---|---|
| TCP 80 | 公网至入口；五域均需可达 | ACME HTTP-01；路径 `/.well-known/acme-challenge/` |
| TCP 443 | 公网至入口 | API、Admin、Widget、RTC 的 HTTPS / WSS |
| TCP 7881 | RTC 客户端至服务器 | LiveKit ICE/TCP 回退 |
| UDP 50000–50200 | RTC 客户端至服务器 | LiveKit 媒体 |
| UDP 3478、TCP 5349、UDP 50201–50500 | 仅启用 embedded TURN 后 | TURN / TURN TLS / relay |
| TCP 5432、9800、7880、可选 3000 | 默认仅宿主机 loopback | PostgreSQL、backend、LiveKit 信令诊断、Grafana |
| TCP 3900 | Compose 内网 | Garage S3；客户端经 API 域 `/s3/` 访问 |

出站需要访问镜像仓库、DNS、ACME 服务，以及实际启用的短信 / SMTP 服务。包是独立文件分发包，不是断网安装介质。TURN 域即使未启用 TURN 也必须填写并签证；**TURN TLS 使用 5349，不把 `turn.<域名>:443` 当成本包验收目标。**

## 4. 解压与离线检查

先核对交付方提供的 ZIP SHA-256，再完整解压。ZIP 含一层 `IMBoy_Community_Standalone_<VERSION>_<提交前12位>/`；把这一层作为固定安装目录，例如 `/opt/imboy-community`。不要把它的父目录当成包根，也不要覆盖已有实例目录。保留根目录、`deploy/`、`scripts/` 之间的相对位置；不要只复制 `deploy/`。下面命令均假定固定目录已准备好，并且当前操作者有读写权限。

```bash
cd /opt/imboy-community
bash scripts/check_delivery.sh
```

这一步检查交付文件的字节、目录和权限，不启动服务。安装控制流与规则回归放在下一节的工具安装之后执行，避免在干净机器上把缺少 jq 等依赖误当成安装器失败。

包校验要求脚本权限为 0755、静态文件为 0644。在新的空目录中使用 Linux `unzip`，并仅对解压子 shell 设置 `umask 022`；客户凭据和数据仍按安装器的受限权限创建。若解压工具丢权限，重新解压并复查，不要批量 chmod 既有实例、更不要 `chmod -R 777`。完整性失败时使用原 ZIP 重新解压，不要自行重算校验清单来掩盖差异。

## 5. 准备宿主机工具

Debian / Ubuntu 可在获准的服务器上执行：

```bash
sudo apt-get update
sudo apt-get install -y bash jq curl openssl ca-certificates coreutils grep sed \
  gawk iproute2 libc-bin unzip tar util-linux findutils nano
docker version
docker compose version
docker info
```

Docker / Compose 应先按客户认可的安装渠道准备，版本须满足第 1 节。`util-linux` 提供包入口使用的 `flock`。没有 Docker 时，原安装器会询问是否运行 Docker 官方安装脚本；`--yes` 会同意这个操作，因此首次陌生人演练建议使用交互安装，不传 `--yes`。

工具就绪后，运行统一离线检查入口：

```bash
cd /opt/imboy-community
bash scripts/run_offline_checks.sh
```

它复用原有安装控制流回归与 preflight 自测，并检查本包入口保护。该过程不启动真实服务、不拉镜像、不签证书；Docker / 网络行为使用本地测试桩。通过后仍需执行现场验收。

## 6. 首次填写配置

仅对**没有旧配置、没有旧数据**的新实例执行以下复制步骤：

```bash
cd /opt/imboy-community
(
  set -eu
  if [ -e deploy/.env ] || [ -L deploy/.env ]; then
    printf '%s\n' '已有配置：停止复制模板，请按升级/恢复手册处理。' >&2
    exit 1
  fi
  if [ -e deploy/data ] || [ -L deploy/data ]; then
    test -d deploy/data
    test ! -L deploy/data
    first_entry=$(find deploy/data -mindepth 1 -print -quit)
    if [ -n "$first_entry" ]; then
      printf '%s\n' 'data 目录非空：停止复制模板，先核实已有数据。' >&2
      exit 1
    fi
  fi
  umask 077
  cp deploy/.env.example deploy/.env
  chmod 600 deploy/.env
)
nano deploy/.env
```

允许预先挂载的空 `deploy/data/` 目录；非空目录、符号链接或无法读取的目录会停止上述复制步骤。不要为了通过检查而删除已有内容。

按第 2 节填写五域、邮箱、API origin、独立清理 LOGIN 等客户字段。保持以下标准值：

```dotenv
DATA_DIR=./data
BACKEND_PORT=9800
UPTRACE_ENABLED=false
IMBOY_EB_DIRECT_ENABLED=false
LIVEKIT_TURN_ENABLED=false
IMBOY_API_AUTH_SWITCH=on
IMBOY_ATTACHMENT_REGISTRY_FROM_GARAGE=true
```

字段按模板使用单行 `KEY=value`。不要把命令、Shell 表达式或从其他实例复制的环境变量块放进 `.env`。安装器与部分自检脚本会读取这份受信任配置。不要预先在终端 `export` 另一套域名或密码；继承环境可能覆盖 Compose 的 `.env`。

第一次保持 `LIVEKIT_TURN_ENABLED=false`，先完成证书签发。Uptrace 和 enterprise-direct 超出本包标准路径，包入口会拒绝这类配置。监控可在核心验收完成后单独启用。

已有 `.env` 后，独立运行完整 `preflight` 可能报告内部密钥占位或 RSA 缺失，这是首次初始化之前的预期状态。**原安装器没有 `--init-only`、`--dry-run`、`--force` 或跳过预检选项。** 不要为了生成密钥而在未获准服务器上运行安装命令。

## 7. DNS 与镜像访问核对

从 `deploy/` 检查五域解析，不输出密钥：

```bash
cd /opt/imboy-community/deploy
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

人工比对地址与实际公网入口；发布 AAAA 时另从外网核对 IPv6。域名能解析不代表 IP 正确、80 可达或 HTTP-01 成功。安装器的 `--dns` 预检只覆盖 RTC / TURN 是否能解析。

若 IMBoy 发布镜像需要 GHCR 登录，由客户在部署机使用其获授权账号执行交互式 `docker login ghcr.io`。不要把 token 放进命令行、安装日志或验收表。公开镜像不需要额外登录。镜像访问失败时停止，先核实版本标签、权限和网络。

## 8. 执行一键安装

在获准的目标服务器上，从包根目录执行：

```bash
cd /opt/imboy-community
(
  set -euo pipefail
  umask 077
  mkdir -p records
  chmod 700 records
  bash install.sh 2>&1 | tee "records/install-$(date +%Y%m%d-%H%M%S).log"
)
```

包入口先检查文件完整性、受支持配置、重复安装数据保护与并发锁，再交给原安装器。原流程为：补齐内部密钥 / RSA → preflight → 校验 Compose 实际配置 → 拉齐镜像 → 检查 edge 镜像文件 → 启动服务 → 签发 TLS → 等待 `/healthz` → 强制运行 `scripts/sanity_check.sh`。

任何 ERROR 或非零退出都必须先解决。失败发生在服务启动之后时，已有容器与数据可能已存在；不要删除目录或重置密钥来“重新开始”。重跑继续使用原配置。故障记录与处理见 [TROUBLESHOOTING.md](TROUBLESHOOTING.md)。

原安装器在无 Git 的解压目录输出 `IMBOY_GIT_SHA=unknown` 属于预期；源码身份以包内 `SOURCE_COMMIT` 为准，运行身份还要记录实际镜像 digest。不能把源提交号直接当作已拉取镜像的构建提交。

## 9. 安装后的必做工作

1. 按 [DB_CLEANUP_ACCOUNT.md](DB_CLEANUP_ACCOUNT.md) 开通或核对独立清理 LOGIN，只授予所需的 `v5_worker` 成员资格；验证实际 TCP 密码登录。
2. 按 [ACCEPTANCE_COMMANDS.md](ACCEPTANCE_COMMANDS.md) 检查 9 个核心服务、Nginx 配置、RSA 挂载、证书信任、数据库迁移和镜像身份。
3. 在 `https://<ADMIN_DOMAIN>` 使用 `/setup` 首次初始化向导创建管理员并实际登录。已有管理员时验证不会再次开放初始化。建议由浏览器输入密码，避免在 Shell 命令历史中留下明文。
4. 使用两个真实验收客户端完成登录、双向消息、断线恢复、附件往返；有对应交付承诺时再做 Widget、RTC / TURN、SMTP / 短信验收。
5. 复制 [ACCEPTANCE_TEMPLATE.md](ACCEPTANCE_TEMPLATE.md) 填写结果、命令退出码、证据位置、未验证项和负责人。处理或明确记录全部 WARN。

`sanity_check.sh` 不覆盖所有业务，也不验证外部证书信任。其成功不能替代本节工作。

## 10. 可选监控与 TURN

### 监控

核心安装验收后，如本次交付需要监控：

```bash
cd /opt/imboy-community/deploy
docker compose --env-file .env -f docker-compose.community.yml --profile monitoring config -q
docker compose --env-file .env -f docker-compose.community.yml --profile monitoring up -d
docker compose --env-file .env -f docker-compose.community.yml --profile monitoring ps
```

监控文件随包提供。默认告警接收器不代表通知渠道已配置；须分别验证采集、面板和真实告警通知。Grafana 默认只监听 `127.0.0.1:3000`，可按客户运维方式使用 SSH 隧道；不要用暴露公网端口代替访问配置。

### embedded TURN

先确认五域证书已签发，TURN 证书及其链接目标可被 LiveKit 容器读取，旧 TURN 已退场且端口未被占用，再设置 `LIVEKIT_TURN_ENABLED=true` 并按包内 TURN 说明实施。安装器会叠加 `docker-compose.livekit-turn.yml`；后续运维命令也必须使用同一 Compose 文件组合。

已运行 TURN 后再执行原 preflight，3478 / 5349 可能被自身占用而报错；这不表示应删除正在运行的服务。先辨认占用者，再按维护窗口处理。TURN 文件齐全、证书存在和 TLS 握手成功，都不能代替真实客户端中继回退验收。

## 依据

本手册复用包内 `deploy/README.md`、`deploy/install.sh`、`deploy/preflight.sh`、`deploy/.env.example` 与 `scripts/sanity_check.sh`。仓库入口见 [部署手册](https://github.com/imboy-pub/imboy/blob/main/deploy/README.md)，审阅时切换至 `SOURCE_COMMIT` 对应提交。仓库部署手册也介绍商务版、源码构建及其他形态；本 ZIP 以根目录交付范围及本手册为客户操作路径。
