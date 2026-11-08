# IMBoy 后端原生 Shell 一键初始化交付包 v1

日期：2026-11-05（北京时间）。源码基线：`imboy-pub/imboy@cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`；后端版本：`1.0.0-alpha.83`。

> **仓库集成的版本边界（2026-11-08）**：本目录仍冻结在上述 SHA、`1.0.0-alpha.83`、迁移头 `243`。本次核对的 main 快照 `8957f0ed6cf7b3251259dbdf777fc1ae43837574` 已是 alpha.84／迁移 `275`，其配置尚未适配本安装器。合入仓库不代表支持安装当前 main，也不代表已经验证 alpha.83 → alpha.84 升级；详见 [验证范围与版本边界](docs/10_VALIDATION.md)。

**所有安装、构建、初始化、验收与运维入口均为 Bash，不依赖 Python 或 Docker。** 本套交付包含可执行脚本、完整手册、环境变量模板、部署配置示例和验收记录模板。请先查看自己收到目录内的 `metadata/release.env`：

| `PAYLOAD_READY` | 目录状态 | 下一步 |
| --- | --- | --- |
| `no` | 安装工具与材料版，没有完整原生二进制 | 发布方构建并组装，客户不要直接安装 |
| `yes` | 已组装原生安装候选 | 核对候选身份及真实演练证据，再执行现场安装 |

2026-11-05 本工具包初次编写时交付的是 `no` 状态材料版，没有已编译的 IMBoy、PostgreSQL 或 Garage 运行产物。发布方完成构建和组装后生成新目录；`yes` 只代表组装完成，不代表业务验收已通过。

## 从哪里开始

| 你是谁 | 先读什么 |
| --- | --- |
| 发布负责人 | `tools/README_PUBLISHER.md`、`docs/07_RELEASE_ASSEMBLY.md`；先构建并组装原生运行产物 |
| 陌生执行者 | `docs/01_INSTALL.md`；拿到已组装候选，只填现场参数与证书，不用读取源码或编译 |
| 网络/环境负责人 | `docs/02_NETWORK_TLS.md`、`templates/site-handover.tsv` |
| 验收/运维负责人 | `docs/04_OPERATIONS.md`、`docs/05_ACCEPTANCE.md`、`templates/acceptance.tsv` |

## 客户的一键入口

下面命令在**发布方已组装并演练的候选目录**执行。域名和证书必须是真实现场值；模板中的 `example.com` 会被拒绝。

```bash
cp templates/site.env.example site.env
chmod 600 site.env
# 编辑 site.env，并按 TLS_DIR 准备 api/s3/files 三组 PEM。

# 机器依赖已经准备好时，先做只读检查，再安装：
sudo bash ./install.sh --check
sudo bash ./install.sh --apply

# 如需同时从现场批准的软件源安装固定主机依赖，可以改用这一条：
sudo bash ./install.sh --apply --prepare-host
```

安装会生成并保留本站密钥，初始化数据库与 Garage，启动后端和 TLS 入口，随后隐藏读取首次管理员口令并进行基础检查。内部秘密不打印在命令行或通用报告中。没有预置管理员口令，也不内置任何客户密钥。

这里的“一键”指已准备候选、域名、证书和权限后的自动初始化，不代表能够猜出客户域名、生成客户认可的 TLS 证书、跳过发行构建或替代业务验收。

## 常用命令

| 操作 | 命令 |
| --- | --- |
| 中断后继续同一安装 | `sudo bash ./install.sh --apply --resume` |
| 已存在安装只读基础验证 | `sudo bash ./verify.sh` |
| 验证 S3 HTTPS 签名读写并清理测试对象 | `sudo bash ./verify.sh --s3-write` |
| 单独补做首次管理员初始化 | `sudo bash ./admin-init.sh` |
| 停机一致性备份 | `sudo bash ./backup.sh` |
| 升级预检查 | `sudo bash ./upgrade.sh --package /srv/new-candidate --check` |
| 升级并创建升级前备份 | `sudo bash ./upgrade.sh --package /srv/new-candidate --apply` |
| 从备份恢复到新隔离主机 | 按 `docs/04_OPERATIONS.md` 使用 `restore.sh`；必须提供可信备份 SHA-256 和原候选 |
| 轮换现场已签发的证书 | `sudo bash ./renew-tls.sh --apply` |

完整性检查覆盖候选目录中所有文件，**仅 `site.env` 允许现场修改**。运行日志、验收输出、备份和新证书均保存在目录外；不要把额外文件塞进已冻结候选或重写其校验表。

## 固定部署范围

单机 systemd，包含自带 ERTS 的 IMBoy release、专用 PostgreSQL 18 及所需扩展、Garage 2.4.1 和 Nginx。首个候选目标为 Ubuntu 24.04 x86_64；工具也允许为 Debian 13 x86_64 组装独立候选，两者都需要各自真实演练，不可混用二进制。

三个域名分别承载 API/WebSocket、签名 S3 API、公开附件网站：

- `https://api.example.com`，WebSocket 为 `wss://api.example.com/api/v1/ws`。
- `IMBOY_GARAGE_PUBLIC_ENDPOINT=https://s3.example.com`，不增加 `/s3` 前缀。
- `https://files.example.com`，只提供独立公开桶读取。

本包不安装 Flutter、管理后台静态站、客服 Widget、LiveKit/TURN 或高可用集群。实际产品范围冻结为 `phase1_delivery`，运行时开关和未验收能力见 `docs/00_DELIVERY_STATUS.md`。

## 当前证据与未完成事项

验证范围、三套可重复合成测试命令与 CI 边界见 [docs/10_VALIDATION.md](docs/10_VALIDATION.md)；固定源码合同见 [docs/09_SOURCE_EVIDENCE.md](docs/09_SOURCE_EVIDENCE.md)。2026-11-05 的历史材料记录三套合成测试合计 62 项通过；本次及后续修改的执行结果以对应 PR 的 CI 检查与结果摘要为准，不在仓库保存运行日志。

**本工具包初次编写时尚未执行**：真实原生编译、干净机安装、实际数据库迁移、真实管理员/客户端登录、消息附件、完整恢复、升级及故障演练。本次仓库集成没有补做这些实装门槛，也未完成 alpha.84 适配。验收表不把这些项目预填为 PASS。发布方必须另行提供对应候选的真实运行产物、客户端配置配对和演练结果；现场必须补齐主机、DNS/TLS、维护与备份信息。

安装或升级失败时保留状态、配置、数据和日志；不要删除数据库、覆盖密钥、修改迁移标记或盲目切回旧二进制。依据具体故障继续或使用原始完整备份在新隔离主机恢复。

## English scope, status and verification

The Chinese documentation is authoritative. This directory provides Bash-only tooling and delivery instructions for a single-host, native systemd deployment of the IMBoy backend, PostgreSQL 18 with required extensions, Garage 2.4.1 and Nginx. Installation does not require Python or Docker. Client applications, the admin web UI and RTC infrastructure are outside this package.

The supported source contract is frozen at `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`, `1.0.0-alpha.83`, migration head `243`. The main snapshot inspected for this integration, `8957f0ed6cf7b3251259dbdf777fc1ae43837574` (alpha.84, migration head `275`), is **not adapted or supported by this installer**. Configuration digest checks remain mandatory; editing a hash or version field does not provide compatibility.

The initial material package has `PAYLOAD_READY=no`. A publisher must supply and assemble native binaries before a candidate can be installed; `yes` confirms assembly only. The historical material recorded 62 passing synthetic checks (29 installer, 21 payload, 12 host-package checks). Current results belong to the PR and the dedicated `.github/workflows/native-shell-delivery.yml` CI workflow. These checks neither build OTP/PostgreSQL nor deploy services. Real installation, migrations, business UAT, restore and upgrade acceptance remain required. See [validation scope](docs/10_VALIDATION.md) and the [publisher guide](tools/README_PUBLISHER.md).
