# IMBoy 社区版独立安装包 / Standalone community installer

本包用于 Linux x86_64 单机社区版安装，包含完整的 Shell 安装入口、必要配置和操作手册。客户无需完整源码仓库，也无需宿主机 Python、pip、Erlang 或 Node。镜像、系统软件、DNS 与 ACME 仍需现场网络条件；本包不包含容器镜像，也不代表已完成真实服务器验收。

本页在源码仓库位于 `deploy/standalone/README.md`，制包后成为根 `README.md`。**下文客户命令均在 ZIP 解压后的包根运行。** 仓库维护者使用 `scripts/package_community.sh --ref <提交或标签> --output <ZIP 路径>` 制包，文件白名单见仓库的 `deploy/standalone/files.tsv`。

## 来源与范围 / Provenance and scope

- `SOURCE_COMMIT`：制包所选 Git ref 解析后的完整提交号。
- `VERSION`、`deploy/.env.example`：源码版本与环境模板；版本号不证明镜像已经发布或客户可拉取。
- `review/SOURCE_FILES.tsv`、`review/SOURCE_MIGRATIONS.tsv`：文件来源与迁移文件清单；运行镜像身份和实际迁移仍需现场核对。
- `MANIFEST.sha256`、`FILE_MODES.tsv`：不可变交付文件的字节与权限清单。

**完整保留包根的 `deploy/` 和 `scripts/`。** 原安装器启动前检查、安装后调用 `../scripts/sanity_check.sh`，因此不能单独分发 `deploy/`。

标准路径要求 `DATA_DIR=./data`、`BACKEND_PORT=9800`、`UPTRACE_ENABLED=false`、`IMBOY_EB_DIRECT_ENABLED=false`；首次保持 `LIVEKIT_TURN_ENABLED=false`。商务版专有 Compose、源码构建链、多节点和 L4 SNI 共用 443 方案不在本包范围。

## 安装顺序 / Installation

先阅读 [安装手册](docs/INSTALL.md)，准备五个不同的实际域名、证书邮箱、发布镜像及拉取权限、独立清理数据库账号和验收客户端。

```bash
# 在完整解压后的包根检查文件；不启动 Docker。
bash scripts/check_delivery.sh

# 按 docs/INSTALL.md 准备工具、首次配置和 DNS。
# 已有实例保留原 .env、RSA、证书和数据，按升级/恢复手册处理。
bash install.sh --check

# 仅在已获准的目标服务器上执行实际安装。
bash install.sh
```

包根入口检查完整性、已有数据与密钥保护、配置范围和安装锁，再调用原 `deploy/install.sh --edition community`。密钥生成、Compose、TLS、管理员初始化与部署后 sanity 继续由现有安装流程负责。`--check` 只做离线保护检查，不生成密钥，也不替代原 preflight 或真实 Compose 校验。

安装后还须完成 [清理数据库 LOGIN](docs/DB_CLEANUP_ACCOUNT.md)、[Garage 桶与现场业务验收](docs/ACCEPTANCE_COMMANDS.md)，并把实际结果填写到 [验收记录](docs/ACCEPTANCE_TEMPLATE.md) 的副本中。基础健康通过不能代替登录、WebSocket 消息、附件和公开资源验收。

## 包内材料 / Contents

| 路径 | 用途 |
|---|---|
| `install.sh` | 社区版交付入口，带配置/旧数据检查和独占锁 |
| `deploy/install.sh`、`preflight.sh`、`nginx/init-letsencrypt.sh` | 原有安装、预检与证书流程 |
| `deploy/docker-compose.community.yml`、`.env.example` | 社区编排与无真实密钥的配置模板 |
| `deploy/nginx/templates/`、`deploy/nginx/rtc.conf.template` | API/Admin、Widget、RTC 三个 HTTP 模板，由社区 Compose 逐个挂载 |
| monitoring 配置目录、可选 TURN / Uptrace 文件 | Compose 本地文件依赖；启用边界见可选组件说明 |
| `scripts/sanity_check.sh`、备份脚本及其 `lib/` | 必需自检与已有备份工具 |
| `scripts/check_delivery.sh`、`run_offline_checks.sh`、`scripts/test/` | 完整性检查、原安装回归、preflight 自测、入口保护测试 |
| `docs/`、`records/` | 安装、升级恢复、排障、交接、验收命令与空白表格 |
| `review/`、根清单与 `LICENSE` | 可核对的来源、迁移、哈希、权限和许可证 |

完整依赖见 [文件依赖](docs/DEPENDENCIES.md)。HTTP 模板目录不包含宿主机 stream 模板；Prometheus 运行规则目录不包含 promtool 测试定义。制包器只导出所选提交的白名单文件，不携带工作目录中的 `.env`、数据、证书、私钥或临时日志。

## 验证与后续运维 / Verification and operations

```bash
# 宿主工具就绪后，在包根复跑；Docker/网络使用测试桩。
bash scripts/run_offline_checks.sh
```

离线检查的证据范围与记录方法见 [验证说明](docs/VALIDATION_RESULTS.md)。每个新包和现场实例均从 `NOT_RUN` 开始记录；缺前置条件记 `BLOCKED`。真实 Docker、镜像、TLS、数据库、登录、消息与附件结果只能来自对应现场执行。

- [安装手册](docs/INSTALL.md)：工具、五域、配置、安装与后续必做步骤。
- [升级与恢复](docs/UPGRADE_AND_RECOVERY.md)：保留原目录与凭据，固定 PG 镜像，备份后升级；镜像回退与数据库恢复分别判断。
- [故障排查](docs/TROUBLESHOOTING.md)：按阶段诊断，保留已有数据和密钥。
- [陌生人交付](docs/STRANGER_HANDOFF.md)：只给本包与现场信息，记录人工干预及材料缺口。
- [可选组件](docs/OPTIONAL_COMPONENTS.md)：monitoring、TURN、续期及明确排除的形态。

包内 `deploy/README.md` 保留仓库部署手册作为参考，其中还介绍源码开发、商务版及其他部署方式。独立包按本页和 `docs/` 操作；不要将参考文档提及的未打包脚本当作已交付材料。
