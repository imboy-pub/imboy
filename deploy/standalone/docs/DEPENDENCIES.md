# 独立文件依赖 / File dependencies

本表区分静态文件、运行时生成内容和现场条件。制包器以 `deploy/standalone/files.tsv` 为白名单，从指定 Git 提交导出；包内 `review/SOURCE_FILES.tsv` 记录来源。文件完整不等于真实安装或业务通过。

| 路径/组件 | 文件依赖 | 来源及范围 |
|---|---|---|
| 包根入口 | `install.sh`、`scripts/check_delivery.sh`、`scripts/install_guard.sh`、清单文件 | 保护检查通过后调用原社区安装器 |
| 原安装链 | `deploy/install.sh` → `.env.example`、社区 Compose、`preflight.sh`、`nginx/init-letsencrypt.sh`、`../scripts/sanity_check.sh` | 保持 `deploy/` 与 `scripts/` 同级 |
| 清理身份门禁 | `deploy/init-cleanup-db.sh` → `deploy/cleanup-db/{bootstrap,verify}.sql`、运行中 backend/PG 镜像 | 迁移后真实认证与权限检查；显式 `--init-cleanup-db` 才创建缺失 LOGIN，复用已有管理权限 |
| Nginx | `deploy/nginx/templates/{imboy,cs-widget}.conf.template`、`deploy/nginx/rtc.conf.template` | 社区 Compose 显式挂载这三个 HTTP 模板，不加载宿主机 stream 模板 |
| 证书与数据 | `deploy/data/certbot/{conf,www}`、`backend_priv/keys`、`backend_log`、`pg18`、`garage`、`nginx_log` | 安装时生成或由客户提供；ZIP 不携带真实内容 |
| PostgreSQL / backend | 迁移 SQL、sys.config、ERTS、escript、psql、pg_isready、edge worker | 由发布镜像提供；实际镜像内容另验，不要求客户源码编译 |
| Garage | Compose 内联配置、本地数据目录、业务桶及既有 access key | Compose 需 v2.23.1+；默认只初始化一个业务桶，公开资源桶另验 |
| LiveKit | Compose 内联 `LIVEKIT_CONFIG`、五域证书/域名条件 | 默认单节点；产品权限按实际发布范围及镜像验证 |
| TURN 可选 | `docker-compose.livekit-turn.yml`、`LIVEKIT_TURN_CERT_DIR`、同域 Certbot archive 目录 | live 与 archive 只读挂载保留证书链接；默认关闭，端口和续期重启另验 |
| monitoring 可选 | Prometheus 配置/规则、Alertmanager、Loki、Promtail、Grafana provisioning 与 dashboards | 全部本地配置随包；Docker 命名卷、宿主日志目录/socket 为运行条件 |
| Uptrace 保留分支 | `docker-compose.uptrace.yml`、`uptrace/{uptrace.yml,otel-collector.yml,nginx.conf.template}` | 保留文件依赖并复用原回归；标准入口关闭 |
| 离线回归 | 原安装回归、preflight 自测、交付入口测试及所引用安装文件 | 临时目录/mock；不包含实际数据或已执行业务结论 |
| 既有 PG 备份 | `scripts/backup_pg.sh` → `scripts/lib/metrics_push.sh`、`backup_offsite.sh` | Shell 依赖齐全；异地/加密功能另需 rclone、age/gpg 与实际配置 |

## 工具依赖 / Tools

客户服务器需要 Bash 4+、Docker 24+、Compose v2.23.1+、jq 1.6+、OpenSSL、curl、系统 CA、coreutils、awk/grep/sed、util-linux/flock、ss 与 DNS 查询工具。ZIP 校验和解压使用 sha256sum、stat、unzip；备份使用 tar。普通客户安装不依赖宿主机 Python、pip、Erlang、Node 或数据库客户端。

制包维护者另需 Git、ZIP 与 GNU 文件工具，在源码仓库运行 `scripts/package_community.sh`。这些不是客户服务器安装条件。额外 YAML/模板静态分析不能替代真实 `docker compose config` 或 `nginx -t`。

## 仍需现场确认 / Site requirements

- 镜像存在、客户可拉取，digest 与批准发布身份一致；源码 SHA 不是运行镜像证据。
- 五域 DNS、80/443 与媒体/TURN 网络；证书签发、外部信任、续期和服务加载。
- 完整迁移、独立清理 LOGIN 的认证/权限、公开资源桶创建与授权。
- 管理员和用户登录、双端消息/重连、私有附件、公开资源以及约定范围内的 RTC/TURN。
- monitoring 原配置还列出基础栈未提供的 exporter 目标；默认空 Alertmanager receiver 不提供通知能力。

现场步骤见 [ACCEPTANCE_COMMANDS.md](ACCEPTANCE_COMMANDS.md)。`review/SOURCE_MIGRATIONS.tsv` 只描述所选源码提交的 up 文件及 Git blob SHA，实际迁移必须读取运行镜像的文件并与数据库比较。
