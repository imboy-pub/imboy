# IMBoy 后端原生 Shell 完整安装与交付手册

版本：v1；初始材料日期：2026-11-05；仓库集成文档更新：2026-11-08。

本手册汇总本目录的 README、全部 00–10 章和发布工具说明。它与同目录 Shell 脚本及可编辑模板配套；手册自身不是二进制安装产物。分章文档有修改时应同步重生成本手册。

所有安装、构建、初始化、验证与运维入口均为 Shell，不需要 Python 或 Docker。实际目录是否包含运行产物，以 `metadata/release.env` 的 `PAYLOAD_READY` 为准：`no` 是材料版，`yes` 是已组装候选，仍需真实部署与业务验收。

本目录冻结于 `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`／`1.0.0-alpha.83`／迁移 `243`。本次核对的 main 快照 `8957f0ed6cf7b3251259dbdf777fc1ae43837574`／alpha.84／迁移 `275` 尚未适配；配置摘要门禁继续生效。新的执行结果以对应 PR/CI 为准，历史 62 项合成通过不代表真实安装或版本升级通过。

## 阅读顺序

| 读者 | 建议顺序 |
| --- | --- |
| 首次部署执行者 | 交付总览 → 交付状态 → 环境网络 → 从零安装 → 验收 → 常见故障 |
| 发布负责人 | 发布方组装 → 构建工具说明 → 固定源码合同 → 验证范围与版本边界 |
| 运维与验收人 | 配置目录合同 → 备份恢复升级 → 陌生人交付演练 → 验收/签收 TSV |

客户操作命令相对于解压后的候选目录；验证章节明确标识的开发命令从仓库根目录执行。运行状态、日志、备份与新证书保存在候选目录之外。示例域名必须替换，秘密由目标机生成或经现场安全渠道提供。

---

<!-- Source: README.md -->

## IMBoy 后端原生 Shell 一键初始化交付包 v1

日期：2026-11-05（北京时间）。源码基线：`imboy-pub/imboy@cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`；后端版本：`1.0.0-alpha.83`。

> **仓库集成的版本边界（2026-11-08）**：本目录仍冻结在上述 SHA、`1.0.0-alpha.83`、迁移头 `243`。本次核对的 main 快照 `8957f0ed6cf7b3251259dbdf777fc1ae43837574` 已是 alpha.84／迁移 `275`，其配置尚未适配本安装器。合入仓库不代表支持安装当前 main，也不代表已经验证 alpha.83 → alpha.84 升级；详见 [验证范围与版本边界](docs/10_VALIDATION.md)。

**所有安装、构建、初始化、验收与运维入口均为 Bash，不依赖 Python 或 Docker。** 本套交付包含可执行脚本、完整手册、环境变量模板、部署配置示例和验收记录模板。请先查看自己收到目录内的 `metadata/release.env`：

| `PAYLOAD_READY` | 目录状态 | 下一步 |
| --- | --- | --- |
| `no` | 安装工具与材料版，没有完整原生二进制 | 发布方构建并组装，客户不要直接安装 |
| `yes` | 已组装原生安装候选 | 核对候选身份及真实演练证据，再执行现场安装 |

2026-11-05 本工具包初次编写时交付的是 `no` 状态材料版，没有已编译的 IMBoy、PostgreSQL 或 Garage 运行产物。发布方完成构建和组装后生成新目录；`yes` 只代表组装完成，不代表业务验收已通过。

### 从哪里开始

| 你是谁 | 先读什么 |
| --- | --- |
| 发布负责人 | `tools/README_PUBLISHER.md`、`docs/07_RELEASE_ASSEMBLY.md`；先构建并组装原生运行产物 |
| 陌生执行者 | `docs/01_INSTALL.md`；拿到已组装候选，只填现场参数与证书，不用读取源码或编译 |
| 网络/环境负责人 | `docs/02_NETWORK_TLS.md`、`templates/site-handover.tsv` |
| 验收/运维负责人 | `docs/04_OPERATIONS.md`、`docs/05_ACCEPTANCE.md`、`templates/acceptance.tsv` |

### 客户的一键入口

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

### 常用命令

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

### 固定部署范围

单机 systemd，包含自带 ERTS 的 IMBoy release、专用 PostgreSQL 18 及所需扩展、Garage 2.4.1 和 Nginx。首个候选目标为 Ubuntu 24.04 x86_64；工具也允许为 Debian 13 x86_64 组装独立候选，两者都需要各自真实演练，不可混用二进制。

三个域名分别承载 API/WebSocket、签名 S3 API、公开附件网站：

- `https://api.example.com`，WebSocket 为 `wss://api.example.com/api/v1/ws`。
- `IMBOY_GARAGE_PUBLIC_ENDPOINT=https://s3.example.com`，不增加 `/s3` 前缀。
- `https://files.example.com`，只提供独立公开桶读取。

本包不安装 Flutter、管理后台静态站、客服 Widget、LiveKit/TURN 或高可用集群。实际产品范围冻结为 `phase1_delivery`，运行时开关和未验收能力见 `docs/00_DELIVERY_STATUS.md`。

### 当前证据与未完成事项

验证范围、三套可重复合成测试命令与 CI 边界见 [docs/10_VALIDATION.md](docs/10_VALIDATION.md)；固定源码合同见 [docs/09_SOURCE_EVIDENCE.md](docs/09_SOURCE_EVIDENCE.md)。2026-11-05 的历史材料记录三套合成测试合计 62 项通过；本次及后续修改的执行结果以对应 PR 的 CI 检查与结果摘要为准，不在仓库保存运行日志。

**本工具包初次编写时尚未执行**：真实原生编译、干净机安装、实际数据库迁移、真实管理员/客户端登录、消息附件、完整恢复、升级及故障演练。本次仓库集成没有补做这些实装门槛，也未完成 alpha.84 适配。验收表不把这些项目预填为 PASS。发布方必须另行提供对应候选的真实运行产物、客户端配置配对和演练结果；现场必须补齐主机、DNS/TLS、维护与备份信息。

安装或升级失败时保留状态、配置、数据和日志；不要删除数据库、覆盖密钥、修改迁移标记或盲目切回旧二进制。依据具体故障继续或使用原始完整备份在新隔离主机恢复。

### English scope, status and verification

The Chinese documentation is authoritative. This directory provides Bash-only tooling and delivery instructions for a single-host, native systemd deployment of the IMBoy backend, PostgreSQL 18 with required extensions, Garage 2.4.1 and Nginx. Installation does not require Python or Docker. Client applications, the admin web UI and RTC infrastructure are outside this package.

The supported source contract is frozen at `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`, `1.0.0-alpha.83`, migration head `243`. The main snapshot inspected for this integration, `8957f0ed6cf7b3251259dbdf777fc1ae43837574` (alpha.84, migration head `275`), is **not adapted or supported by this installer**. Configuration digest checks remain mandatory; editing a hash or version field does not provide compatibility.

The initial material package has `PAYLOAD_READY=no`. A publisher must supply and assemble native binaries before a candidate can be installed; `yes` confirms assembly only. The historical material recorded 62 passing synthetic checks (29 installer, 21 payload, 12 host-package checks). Current results belong to the PR and the dedicated `.github/workflows/native-shell-delivery.yml` CI workflow. These checks neither build OTP/PostgreSQL nor deploy services. Real installation, migrations, business UAT, restore and upgrade acceptance remain required. See [validation scope](docs/10_VALIDATION.md) and the [publisher guide](tools/README_PUBLISHER.md).

---

<!-- Source: docs/00_DELIVERY_STATUS.md -->

## 交付状态、使用边界与责任

版本：原生 Shell 交付包 v1。编写日期：2026-11-05（北京时间）。

核对的后端源码：`imboy-pub/imboy@cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`，源码版本 `1.0.0-alpha.83`。以下结论只对应这份固定源码与本包脚本；更换源码或运行时必须重新组装、验证。

本次仓库集成核对日期为 2026-11-08。**版本范围仍是 alpha.83／迁移头 `243`，不是对 main 的自动适配。** main 快照 `8957f0ed6cf7b3251259dbdf777fc1ae43837574` 的 alpha.84／迁移头 `275` 以及新的 `sys.config` 摘要均超出本目录冻结合同。必须先进行发布方适配、重新组装和真实演练，才能声明支持该版本或升级路径；摘要对照和验证范围见 [10_VALIDATION.md](docs/10_VALIDATION.md)。

### 1. 这次交付了什么

这套材料提供纯 Bash 的原生部署流程：准备配置、验证发行物、按需安装已冻结的主机依赖、生成现场密钥、初始化 PostgreSQL 与 Garage、生成 systemd/Nginx 配置、启动后端、执行基础检查，以及备份、升级和恢复。脚本不依赖 Python，也不调用 Docker。

**本工具包于 2026-11-05 初次生成的材料版尚未包含已编译的 IMBoy、PostgreSQL 或 Garage 二进制。** 组装器会把本手册原样带入后续候选，因此拿到的实际目录应以 `metadata/release.env` 中的 `PAYLOAD_READY` 判定状态：`no` 是材料版，须由发布方按 `07_RELEASE_ASSEMBLY.md` 供应并组装真实 `payload/`；`yes` 是已组装候选，仍须查看对应实装与业务 UAT 证据。不能只凭本文日期或 `yes` 宣称生产就绪。

无论哪一种目录，通用发行物都不应带入客户证书私钥、口令或生产数据。安装器会把缺失的运行产物作为阻塞，不会把空模板运行成“安装成功”。

“源码已核对”“Shell 检查通过”“模拟流程通过”“干净机实装通过”“业务验收通过”是不同证据。2026-11-05 材料制作时尚未取得目标服务器的干净机安装、真实登录/消息/附件、完整恢复或升级演练结果，因此通用验收表初始为 `NOT_RUN`。后续发布方应填写实际候选的结果，不沿用初始状态冒充最新结论。

### 2. 支持目标与范围

| 项目 | 本包约定 |
| --- | --- |
| 安装方式 | 单机原生 systemd；应用/数据库/对象存储二进制随包供应；操作入口使用 Bash |
| 首个候选平台 | Ubuntu 24.04、Linux x86_64；发行物必须在相同 ABI 平台构建并演练 |
| Debian / ARM | 需要制作独立发行物；当前包不把它们视为已经认证的等价目标 |
| IMBoy | 自包含 relx release，内含 ERTS；客户无需额外安装或编译 Erlang |
| 数据库 | 专用 PostgreSQL 18 集群，包含当前迁移必需的扩展 |
| 附件 | Garage 2.4.1 单节点；私有桶与公开桶分开；单节点不提供磁盘/主机容灾 |
| 入口 | Nginx TLS；API、S3 签名接口、公开文件使用三个独立域名 |
| 固定产品 profile | `phase1_delivery`，以同一发行物中的 feature manifest 为准 |
| 本包不安装 | Flutter、管理后台静态站、客服 Widget 静态站、LiveKit/TURN、监控整栈 |
| 本包不自动接管 | 已有生产数据库、已有 Garage 数据、共享 Nginx、已有 IMBoy 安装或跨主机集群 |

当前 profile 清单选择 core、channel、customer_service、enterprise_business、group_schedule、group_task、group_vote、location、moment；运行时交付锁关闭 E2EE、RTC、Agent/Bot 等未交付能力。部分后端模块仍可常驻，因此不能把“功能关闭”写成“相关代码完全不在发行物中”。支付和动态插件由运行配置明确关闭；LLM/LiveKit 等外部配置保持为空。客服 AI 属于客服领域，不能仅因未配置 LLM 便宣称所有客服 AI 路径都关闭，应按未开通、未验收处理。扩展启用范围必须重新核对配置与 UAT。[产品清单](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/config/product-feature-manifest.json)、[profile 运行时预设](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_profile_preset.erl)。

同机数据库和对象存储是部署简化方案。可用性、容量、RPO/RTO 和备份留存期必须由现场确定。不能把“一键装上”理解为“高可用”“备份已验证”或“所有 IMBoy 产品能力均已开通”。

### 3. 可直接交付、发布方待办与现场待办

| 分类 | 内容 | 当前处理 |
| --- | --- | --- |
| 可直接交付 | Shell 安装/运维入口、配置模板、网络要求、安装/升级/恢复手册、验收表、排错指南 | 已作为本包内容提供；验证范围见 [10_VALIDATION.md](docs/10_VALIDATION.md)，新执行结果以对应 PR/CI 为准 |
| 可直接交付 | 固定源码版本与部署合同说明、文件完整性清单 | 供发布与验收核对；哈希不能替代发行方身份认证 |
| 发布方必须完成 | 同一固定 SHA 的 Linux 自包含 IMBoy release、PG18 必需扩展、Garage 二进制、目标机运行库 | 通过 `tools/prepare-payload.sh` 组装；不能让客户现场找代码或临时补编译 |
| 发布方必须完成 | 二进制来源、版本、平台、SHA-256、依赖许可清单；干净机首装、恢复、升级验证 | 属于发行工程工作，填写客户域名无法解决 |
| 现场必须补齐 | 主机、域名、DNS、TLS、磁盘、端口、维护窗口、备份目的地、操作人与验收人 | 在 `templates/site-handover.tsv` 填写，不填秘密 |
| 现场按范围补齐 | 管理员身份、客户端候选包、SMTP/短信/推送/IdP 测试身份及回调配置 | 秘密通过现场独立渠道提供，不写入通用交付包 |
| 安装时自动生成 | 数据库口令、应用签名材料、Erlang cookie、Garage 认证材料、登录 RSA 等 | 只落在目标机受限文件中；续跑必须复用，不能重新生成替换 |

### 4. 角色与阅读顺序

| 角色 | 首先阅读 | 完成标准 |
| --- | --- | --- |
| 发布负责人 | 本文、`07_RELEASE_ASSEMBLY.md` | 交付有真实 payload、版本清单与验证证据的候选包 |
| 环境负责人 | `02_NETWORK_TLS.md`、`templates/site-handover.tsv` | 目标机、三域名、证书、网络、磁盘和备份渠道准备完成 |
| 陌生执行者 | `01_INSTALL.md` | 只按文档完成安装、基础验证并记录每一次阻塞 |
| 运维负责人 | `03_CONFIGURATION.md`、`04_OPERATIONS.md`、`06_TROUBLESHOOTING.md` | 能独立定位问题、备份、恢复和按允许路径升级 |
| 配置审阅人 | `08_DEPLOYMENT_EXAMPLES.md` | 对照无秘密的 Nginx、systemd、Garage、PG 示例理解生成结果 |
| 验收负责人 | `05_ACCEPTANCE.md`、`templates/acceptance.tsv` | 每项必验项目都有真实结果和可追溯证据 |

### 5. 不把未知内容交给客户猜

如发行包缺二进制、系统依赖、扩展、可信来源或迁移说明，执行者登记 `BLOCKED` 并交回发布负责人。不要在客户服务器上临时克隆默认分支、换成通用 PostgreSQL、去掉启动检查或删除数据重装。

源码证据：[release 自包含设置](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/relx.config)、[运行时环境覆盖](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env.erl)、[原生 CI 依赖准备](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-bootstrap.sh)。CI 配方是构建参考，不是本交付包实装认证。

---

<!-- Source: docs/01_INSTALL.md -->

## 从零安装手册

适用对象：第一次接触 IMBoy、能使用 SSH 和基本 Linux 命令的执行者。目标是按材料部署，无需阅读后端业务代码。执行前先读 `00_DELIVERY_STATUS.md`，确认拿到的是**发布方已组装 payload 的候选目录**。

### 1. 接收材料

必须同时收到：完整安装目录、发行物版本与哈希、非秘密现场参数、三域名证书交接说明、范围表、验收表、发布联系人。仅收到模板材料 ZIP 时，先由发布方执行组装流程。

在无生产数据的目标机解压后，进入安装目录。下面命令均在这个目录运行。不要复制单个 `install.sh` 到其他位置执行；它依赖同目录下的 `lib/`、`templates/`、`metadata/` 与 `payload/`。

Linux 建议接收 `.tar.gz` 格式。假设发布方把候选放在 `/srv/deliveries/imboy-candidate.tar.gz`，可用 `tar -xzf /srv/deliveries/imboy-candidate.tar.gz -C /srv/deliveries` 解压，再进入解压后含 `install.sh` 的目录。文件名和候选目录以发布方实际交付为准。ZIP 也提供相同内容，需要本机具备 ZIP 解压工具。

```bash
bash ./install.sh --help
sha256sum --check CHECKSUMS.sha256
```

校验失败时先核对传输是否完整、候选版本是否一致，不要自行重新生成清单掩盖差异。完整性清单验证文件没有意外变化；发行方身份与可信渠道需要另行确认。

### 2. 填写现场参数

按 `02_NETWORK_TLS.md` 完成环境检查，复制并编辑模板：

```bash
cp templates/site.env.example site.env
chmod 600 site.env
```

`site.env` 只存非秘密值；不要放数据库口令、JWT、RSA 私钥、S3 secret 或 IdP client secret。模板中的占位符必须替换。域名填写主机名，不能带 `https://`、路径或尾斜杠；由配置生成器组合 URL。

| 参数 | 示例 | 用途 |
| --- | --- | --- |
| `API_DOMAIN` | `api.example.com` | HTTPS 与 `wss://api.example.com/api/v1/ws` |
| `S3_DOMAIN` | `s3.example.com` | Garage S3 API、公网预签名上传/下载 |
| `FILES_DOMAIN` | `files.example.com` | 独立公开桶的网站读取 |
| `TLS_DIR` | `/srv/imboy-tls` | 现场已准备的三域名证书目录 |

`TLS_DIR` 下必须有 `api/fullchain.pem`、`api/privkey.pem`、`s3/fullchain.pem`、`s3/privkey.pem`、`files/fullchain.pem`、`files/privkey.pem`。证书至少剩余 7 天有效期，且覆盖各自域名。建议交付时预留 30 天以上余量。相同的 SAN 证书可以覆盖三域，但三个目录仍按本包合同准备。私钥从现场安全渠道取得，不随通用包分发。

### 3. 只读预检查

```bash
sudo bash ./install.sh --check
```

预检查核对发行物、版本/操作系统/架构、冻结的主机软件包版本、系统命令和动态库、systemd、现场参数与证书。它不访问真实 DNS/TLS 链路、不启动服务。已有安装目录、专用账号、私有端口等接管保护在 `--apply` 写入之前再次执行；`--check` 通过不保证主机在正式安装时仍为空。预检查不等同于运行验收。

有阻塞就先修正对应项目再检查，无需反复盲试 `--apply`。如果只是主机缺少候选清单已冻结的系统依赖，可使用下一节的 `--prepare-host` 安装方式。记录故障编号、命令、退出码、时间和脱敏输出，按 `06_TROUBLESHOOTING.md` 处理。

### 4. 一键初始化安装

环境负责人确认这是一台专用的新主机、数据目录没有已有业务后，选择一种方式。系统依赖已经齐备时运行：

```bash
sudo bash ./install.sh --apply
```

如果需要同时安装候选包冻结的系统工具、Nginx 和运行库，目标机能访问现场批准的发行版软件源，并且本次操作包含安装这些软件的授权，则使用：

```bash
sudo bash ./install.sh --apply --prepare-host
```

`--prepare-host` 只与 `--apply` 一起使用。候选包、现场参数和已有安装/续跑身份检查通过后，入口调用 `tools/prepare-host.sh --apply`，再进行其余主机检查与初始化。依赖齐备时不用此选项。它会实际安装系统软件，不是只读检查；精确版本不可取得时应处理依赖阻塞，不临时换版本。后续初始化失败不会自动卸载已安装的软件。离线目标应预装匹配依赖，不使用需要联网取包的方式。

安装负责以下动作：

1. 核对候选包与目标环境，建立本安装的状态和目录。
2. 安装随包提供的运行时和后端 release，准备专用服务账号。
3. 在本机生成内部秘密与 RSA 材料，并设置受限权限。
4. 初始化专用 PostgreSQL 集群、业务身份和必需扩展。
5. 初始化 Garage 单节点布局、私有桶、公开桶、访问密钥及桶权限。
6. 生成应用运行配置、systemd 单元和 Nginx 三域名 TLS 配置。
7. 迁移期间保持应用公网入口关闭，临时提升专用 PG 集群中的 `imboy` 角色；成功后撤销授权，失败处理也停止入口并执行撤销。异常断电或数据库不可用时，恢复操作必须核对实际角色状态。成功检查数据库/Garage/后端 readiness 后才开放 TLS 入口并执行基础验证。

以程序返回码与验证报告判断结果。某一阶段“完成”不代表后续阶段完成。数据库迁移、对象存储权限和应用配置错误都必须保留为失败。

秘密只在现场生成或通过现场渠道提供。程序退出、SSH 断开或重启后，不要删除 `/etc/imboy` 或重新生成密钥；这些文件与数据库、对象存储共同构成安装身份。

### 5. 中断后续跑

保留原候选目录、原 `site.env`、状态和全部数据，核对错误原因后运行：

```bash
sudo bash ./install.sh --apply --resume
```

续跑应复用已有安装身份和秘密。候选版本、现场配置或证书发生变化时，先记录并确认脚本是否允许续跑，不要用新目录冒充同一次安装。无法确认某一步是否执行完成时，先检查实际服务和数据；不能仅依据日志最后一行猜测。

`install.sh --resume` 只用于这次首次安装。实例一旦进入升级或恢复阶段，入口会拒绝用旧安装包继续，以免把旧二进制切回已经迁移的数据。升级失败按 `04_OPERATIONS.md` 保留现场并处理，不能用首次安装的续跑命令代替。

### 6. 管理员与客户端接入

安装第七步会调用本机管理员初始化入口，需要交互终端。管理员初始化仍是独立验收项目；非交互运行或中断后，可单独执行：

```bash
sudo bash ./admin-init.sh
```

按提示输入管理员身份与口令，口令隐藏输入。本包入口要求 12–64 个可打印 ASCII 字符、无空格、至少包含字母和数字；允许标点，并需二次确认。邮箱或手机号必须符合后端校验。不要把口令附加到命令行、Shell 历史或工单。已有管理员时入口保留原账号和口令，不执行重置。没有预置通用管理员密码。

后端实际提供 `/api/adm/setup/status` 与 `/api/adm/setup/init`，本包使用本机入口并限制公网访问初始化路径。初始化管理员不代表管理后台网页已经安装；管理后台 UI 不属于本包。

客户端候选包由项目方提供，需配置同一 API 地址、WebSocket 路径和产品范围。真实登录若依赖短信/SMTP/IdP，先完成供应商配置；不得关闭认证以换取“验收通过”。

源码证据：[实际路由](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_router.erl)、[管理员初始化与校验](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/logic/adm_setup_logic.erl)。

### 7. 验证与交接

```bash
sudo bash ./verify.sh
sudo bash ./verify.sh --s3-write
curl --fail --silent --show-error https://api.example.com/healthz
```

`--s3-write` 通过本机 Nginx 的 HTTPS S3 域名入口发送真实 SigV4 请求，写入、读取、比对并删除唯一的临时测试对象；默认验证不会写对象。该检查将域名解析到回环，只证明本机 TLS/代理/S3 往返，不能代替外网 DNS、客户网络或真实 APP 预签名/确认业务。最后一条命令的域名换成现场值，并从真实用户接入网络再执行一次。不要添加 `-k` 绕过 TLS。内网 CA 应在测试客户端及本机 curl 的信任库中正确安装。

基础检查通过后，按 `05_ACCEPTANCE.md` 完成真实登录、双用户消息、私有/公开附件、重启、备份恢复、升级与允许回退。健康端点只检查后端、数据库连通性与 TSID readiness，不能替代附件签名、短信、推送或业务验收。

交接时保留候选版本、配置摘要、脱敏结果与实际命令，交接备份位置、密钥保管人、证书续期责任人和维护窗口。不要将 `/etc/imboy`、完整日志或数据库备份上传到公共问题跟踪系统。

---

<!-- Source: docs/02_NETWORK_TLS.md -->

## 环境、网络、域名与证书

### 1. 主机要求

首个演练目标为专用 Ubuntu 24.04 x86_64 主机和同平台构建的二进制。准备起点为 4 vCPU、16 GiB 内存、至少 40 GiB 可用 SSD；这些数值是本包小规模演练预算，不是承诺的用户数、吞吐量或生产容量。磁盘还需容纳附件增长、数据库/WAL、日志、备份与恢复演练的临时空间。

目标环境应具备 systemd、Bash 4.4+、GNU coreutils、tar、gzip、支持 `--aws-sigv4` 的 curl、OpenSSL、jq、awk、sed、grep、find、util-linux、iproute2、file、Nginx，以及候选二进制需要的动态库。发布方冻结同平台候选主机软件包的精确版本；环境方可预装，也可在有软件安装授权且批准源可达时使用 `install.sh --apply --prepare-host`。最终依赖以无缓存主机实测为准，不从其他主机随意复制 `.so`。

应用 release 自带 ERTS。客户不需要 Erlang 开发环境、Git、编译器或 Python。纯 Shell 指交付流程与辅助工具的实现；业务仍由 Erlang/OTP、PostgreSQL、Garage 和 Nginx 二进制运行。

主机必须完成时间同步。安装器记录 UTC 时间，后端运行配置中的 `TZ` 默认为 `Asia/Shanghai`，会影响沿用上游的本地时区定时任务；日志与演练记录均注明时区，不能把这一设置擅自当作 UTC。预签名 URL 与 TLS 依赖合理时钟；大量偏移会使请求过期或尚未生效。

### 2. 三域名与流量关系

| 外部域名 | 后端去向 | 必须保留的语义 |
| --- | --- | --- |
| `api.example.com` | 本机 IMBoy HTTP 9800 | `/api/v1/ws` 的 Upgrade/Connection；真实客户端 IP；HTTPS 来源 |
| `s3.example.com` | 本机 Garage S3 API 3900 | 原 Host、路径、查询串和签名头；不增添 `/s3` 前缀 |
| `files.example.com` | 本机 Garage Website 3902 | Host 映射到公开桶；仅提供公开桶读取 |

`IMBOY_GARAGE_PUBLIC_ENDPOINT` 的示例值固定为 **`https://s3.example.com`**，不再使用 `https://api.example.com/s3`。`FILES_DOMAIN` 对应 `garage.public_base_url`；两者用途不同，不能互换。私有附件不通过公开网站域名访问。

三个域名的 A 记录指向入口 IPv4。当前生成的 Nginx 模板只配置 IPv4 监听，默认不发布 AAAA；IPv6 要由发布方补充监听、路由与防火墙并单独演练，不能只新增 DNS。DNS TTL、变更负责人、内外网解析差异、代理/CDN 都记录在现场表中。

本包不处理多台后端、数据库高可用、Garage 多节点或既有共享反向代理。这些拓扑需要另行验证节点标识、路由、对象一致性和故障切换。

### 3. 入站端口

| 端口/协议 | 来源 | 用途 | 默认约定 |
| --- | --- | --- | --- |
| 443/TCP | 许可的客户端网络 | HTTPS、WSS、S3 签名请求、公开文件 | 对外开放 |
| 80/TCP | 许可的客户端网络 | HTTPS 跳转；是否用于证书挑战由现场确定 | 按 Nginx 配置使用 |
| SSH 管理端口/TCP | 管理网络/VPN/跳板机 | 安装与运维 | 由现场控制，不由脚本修改 |
| 9800/TCP | 127.0.0.1 | IMBoy 内部 HTTP | 不对公网开放 |
| 5432/TCP | 127.0.0.1 | 本机后端访问 PostgreSQL | 不对公网开放 |
| 3900/TCP | 127.0.0.1 | Garage S3 API | 仅经 Nginx 对外 |
| 3901/TCP | 127.0.0.1 | Garage 单节点 RPC | 不对公网开放 |
| 3902/TCP | 127.0.0.1 | Garage 公开桶 Website | 仅经 Nginx 对外 |
| 3903/TCP | 127.0.0.1 | Garage 管理 | 不对公网开放 |
| 4369 / 9100 TCP | 127.0.0.1 | EPMD / Erlang 本机运行控制 | 不对公网开放 |

本包不需要 TURN、LiveKit 媒体端口、Kubernetes 端口等未纳入合同的入口。安装器不修改现场防火墙；环境负责人配置并留存验证证据。

### 4. 出站与离线范围

应用与 PG/Garage 通过本机地址通讯。是否访问短信、SMTP、推送、IdP、更新仓库取决于启用范围；域名解析、授时和证书续期服务应可用。

**二进制随包供应不等于整台操作系统完全离线安装。** `--prepare-host` 需要访问现场已批准、能提供清单中精确版本的发行版源。完全离线时，由环境方先准备匹配的本地源或安装好系统依赖，再不带该选项安装并复验。不能把构建机缓存当作客户依赖已齐，也不能把“源里有更新版本”当作冻结版本可用。

发布方构建阶段的源码下载与工具联网不得转嫁给客户执行者；在版本冻结表记录构建来源即可。

### 5. TLS 文件核对

`TLS_DIR` 下有 `api/`、`s3/`、`files/` 三个目录，每个包含 `fullchain.pem` 与 `privkey.pem`。证书覆盖对应域名，完整中间链与私钥匹配。证书最少剩余 7 天有效，交付建议 30 天以上。CA 根证书通过可信渠道安装；应用私钥不作为 CA 材料分发。

以下只读命令不输出私钥正文。路径和域名替换为现场值：

```bash
openssl x509 -in fullchain.pem -noout -subject -issuer -dates
openssl x509 -in fullchain.pem -noout -checkhost api.example.com
openssl x509 -in fullchain.pem -noout -checkend 2592000
openssl x509 -in fullchain.pem -pubkey -noout | openssl pkey -pubin -outform DER | sha256sum
openssl pkey -in privkey.pem -pubout -outform DER | sha256sum
```

`-checkhost` 要查看输出，明确出现 `does match certificate` 才算域名匹配；部分 OpenSSL 版本即使不匹配也可能返回 0，不能只看退出码。安装器也显式检查匹配输出。最后两行公钥摘要应一致。`-checkend 2592000` 检查未来 30 天是否过期。本包不接受需要口令解密的 TLS 私钥，使用受限文件权限保护现场无口令 PEM；其他私钥托管方式需另做对应部署方案。

本包不调用 Certbot，不自动签发证书。证书由企业 CA、外部运维流程或经批准的 Shell ACME 工具提供。续期责任人应按现场计划更新证书、执行 `nginx -t`、受控 reload 并从外网验证；失败时保留已知可用证书。证书轮换不需要重建数据库或重新生成应用 RSA。

### 6. S3 与浏览器检查

预签名请求包含精确 Host、路径、查询串和有效期。代理不能重写签名路径、移除查询参数、改写 Host 或把上传重定向到其他域名。CDN/WAF/企业网关若限制 PUT、DELETE、OPTIONS、分块上传或大请求体，应按实际上传边界逐项验证。

浏览器附件验收需要真实前端 origin 与 CORS 配置。只测 `curl` 不能证明浏览器跨域上传成功。本包仅接受 `https://真实域名` 形式的默认 443 origin，不允许显式端口、路径或通配符；自定义端口需由发布方调整并重新验收。没有交付 Web 前端时可以按范围记录 `NOT_APPLICABLE`，不能记为已通过。

TLS 与网络至少从服务器、管理网络、真实用户接入网络分别验证。S3 使用临时测试对象；报告不保留可访问业务附件的完整预签名 URL。

---

<!-- Source: docs/03_CONFIGURATION.md -->

## 配置、环境变量与目录合同

### 1. 三类配置分开管理

| 类型 | 文件 | 谁填写 / 生成 | 能否进入通用交付包 |
| --- | --- | --- | --- |
| 现场非秘密参数 | 包目录的 `site.env` | 环境负责人从 `templates/site.env.example` 填写 | 模板可以；真实客户拓扑应按现场信息分级处理 |
| 固定发行信息 | `metadata/release.env` 与 `CHECKSUMS.sha256` | 发布方组装工具生成 | 可以；不得由客户手改以跳过兼容检查 |
| 运行秘密与应用配置 | `/etc/imboy/` 下生成文件 | 安装器在现场生成；外部服务秘密由现场注入 | 不可以；属于安装身份与恢复材料 |

`site.env` 是受限制的 `KEY=value` 数据文件，不是任意 Shell 程序。按模板填写，不加 `export`、命令替换、Shell 函数、表达式或额外未支持的键。安装器解析数据，不应执行客户文件中的代码。也不要手工 `source site.env` 作为排障捷径。

### 2. 现场参数表

| 参数 | 含义 | 约束 / 选择依据 |
| --- | --- | --- |
| `SITE_NAME` | 本次部署标识 | 使用模板允许的简单标识；不含个人秘密 |
| `API_DOMAIN` | 后端 HTTPS/WSS 域名 | 真实 FQDN，证书覆盖；不附协议、端口、路径 |
| `S3_DOMAIN` | S3 公网签名入口 | 独立 FQDN；生成 `https://s3.example.com` 这类 URL |
| `FILES_DOMAIN` | 公开桶读取域名 | 与 S3 签名入口分开；明确公开内容范围 |
| `TLS_DIR` | 三域名 PEM 来源目录 | 绝对路径；其下为 `api/`、`s3/`、`files/` |
| `GARAGE_CAPACITY` | Garage 布局声明的可用存储容量 | 不大于实际可用于附件的空间；预留数据库、日志和备份余量 |
| `CORS_ORIGINS` | 允许的 Web 来源 | 逗号分隔的 `https://真实域名`，当前仅支持默认 443、无显式端口/路径/通配符；无 Web 范围可留空 |

首次支持面固定端口与路径。已有端口占用、共享盘布局或外部数据库不能靠添加未声明变量解决，应作为不同部署拓扑由发布方提供相应候选。

### 3. 后端实际读取的变量

以下为固定源码确认的映射，供发布方和运维定位问题。用户通常只填 `site.env`；运行文件由安装器生成。完整示例见 `templates/backend.env.example`，不要把示例中的秘密占位符直接用于运行。

| 运行变量 / 配置项 | 本包目标 | 说明 |
| --- | --- | --- |
| `IMBOYENV` | `pro` | 启用生产环境启动校验 |
| `HTTP_PORT` | `9800` | 实际 HTTP 监听端口变量；`IMBOY_HTTP_PORT` 不是当前监听器读到的变量 |
| `sys.config` 的 `listen_ip` | `{127,0,0,1}` | 当前没有对应环境变量覆盖；由配置生成器写入 |
| `IMBOY_BASE_URL` | `https://API_DOMAIN` | 本包后端公开基址；客服特殊 URL 合同另行核对 |
| `IMBOY_WS_URL` | `wss://API_DOMAIN/api/v1/ws` | 固定源码真实 WebSocket 路由 |
| `IMBOY_PG_HOST` / `PORT` | `127.0.0.1` / `5432` | 本机专用数据库 |
| `IMBOY_PG_USER` / `DB` / `PASSWORD` | 本包专用业务身份与现场密码 | 源码还接受 USERNAME / DATABASE 别名，交付统一一种写法 |
| `IMBOY_AUTO_MIGRATE` | 按安装/升级流程设置 | 迁移失败或 dirty 状态导致应用启动失败，不应忽略 |
| `IMBOY_PRODUCT_PROFILE` | `phase1_delivery` | 必须与发行物编译的清单一致 |
| `IMBOY_PRODUCT_EXPERIENCE` | 见生成配置 | 不能单凭切换体验值承诺完整工作台/组织业务交付 |
| `IMBOY_GARAGE_ENDPOINT` | `http://127.0.0.1:3900` | 服务端访问对象存储的内部地址 |
| `IMBOY_GARAGE_PUBLIC_ENDPOINT` | `https://S3_DOMAIN` | 客户端预签名地址；不加 `/s3` |
| `IMBOY_GARAGE_BUCKET` | 私有桶 | 桶名由生成配置固定 |
| `IMBOY_GARAGE_PUBLIC_BUCKET` | 独立公开桶 | 只有明确公开内容使用 |
| `IMBOY_GARAGE_ACCESS_KEY` / `SECRET_KEY` | 现场生成 | 不提供给客户端或写入验收报告 |
| `garage.region` | `garage` | 当前在 `sys.config` 中设置，不能假设存在任意 IMBOY_* 覆盖 |
| `garage.public_base_url` | `https://FILES_DOMAIN` | 当前在 `sys.config` 中设置 |
| `IMBOY_ATTACHMENT_REGISTRY_FROM_GARAGE` | `true` | 按 Garage 参数生成存储注册表；缺失有效注册表会触发严格启动检查 |
| 登录 RSA 文件变量 | `/etc/imboy/keys/` 下文件 | 权限必须允许服务账号读取，禁止在 release 中夹带旧私钥 |

数据加密、JWT、密码盐、管理员 Cookie、TSID 等参数也属于运行合同。秘密一经使用不得通过“重新初始化”更换，否则可能导致历史密文、会话或签名失效。需要轮换时应使用独立的、经验证的轮换流程。

直接证据：[环境覆盖](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env.erl)、[额外环境覆盖](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env_overrides.erl)、[监听与启动迁移](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl)、[真实路由](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_router.erl)。

### 4. 安装后的固定目录

| 路径 | 内容 | 备份 / 修改规则 |
| --- | --- | --- |
| `/opt/imboy/releases/<RELEASE_ID>/backend` | 每个后端发行物 | 不在线改代码；保留旧发行物与哈希 |
| `/opt/imboy/current` | 当前发行物指针 | 由安装/升级入口维护，不能靠手改链接做数据库回滚 |
| `/opt/imboy/runtime/postgresql` | PG18 二进制和扩展 | 应与恢复时的数据库版本一致 |
| `/opt/imboy/runtime/garage` | Garage 二进制 | 保留版本记录；不能随意替换大版本 |
| `/etc/imboy/site.env` | 已接受的现场参数 | 修改需走受控流程，确保生成配置同步 |
| `/etc/imboy/backend.env`、`sys.config`、`vm.args` | 应用运行配置 | 含敏感值，限制读取；备份中一并保护 |
| `/etc/imboy/secrets.env`、`keys/` | 安装身份与秘密 | 必须备份，禁止公共分发 |
| `/etc/imboy/tls/` | 服务使用的 TLS 材料 | 由证书负责人管理轮换、权限、到期验证 |
| `/var/lib/imboy/postgres` | 数据库集群 | 不手工清空；不能把在线普通文件复制当作一致快照 |
| `/var/lib/imboy/garage/` | Garage 元数据与对象数据 | 必须作为同一组恢复；不能只备份其中一边 |
| `/var/lib/imboy/backend` | 后端本地持久状态 | 与数据库/密钥联合备份；不可当缓存随便删除 |
| `/var/backups/imboy` | 本机备份归档 | 非异机备份；制定离机复制、加密、保留及恢复抽查 |
| `/var/lib/imboy-install` | 安装/恢复身份、状态与互斥锁 | 不通过删除状态绕过续跑或并发保护 |

路径、权限和所属账号以安装器最终生成结果为准。`root` 能读取服务器秘密属于运维信任边界；不要通过给所有用户开放目录权限来解决启动问题。

### 5. 配置变更原则

先记录候选版本、现有配置摘要、变化原因与受影响业务，再在演练环境验证。URL、域名、证书、CORS、桶、数据库连接、加密材料不是可以随意互换的字符串。修改后重新进行对应验证，至少检查生成配置语法、服务 readiness 和真实附件操作。

`site.env` 修改不意味着已运行的服务自动接收新值；实际生效需重新生成对应配置并受控重启/reload。当前安装器面向首次部署与同一状态续跑，不将任意配置变更视为已支持的升级方式。

渲染器冻结了已核对上游配置的摘要。安装/升级时，release 的无秘密配置模板、元数据 `CONFIG_SHA256` 与渲染器允许的摘要必须一致。上游配置变化时，由发布方适配渲染器并重新验收；不能只换版本号或手改哈希让门禁通过。

### 6. 企业 IdP 与其他外部集成

如合同要求企业免登/SSO，至少记录 IdP 类型、issuer/discovery 地址、客户端标识、精确回调 URL、租户/组织映射、授权 scopes、退出策略、测试账户和停用用户行为。client secret、证书私钥与用户 Token 通过独立安全渠道提供。

本包没有替客户自动创建 IdP 应用，也没有声称已完成特定 IdP 接入。SMTP/短信/推送同理：缺少供应商配置时应将相关登录/通知验收标为 `BLOCKED`，不能用健康检查通过代替。实际启用这些集成前核对对应代码与配置合同，不在 `site.env` 中随意增加变量。

---

<!-- Source: docs/04_OPERATIONS.md -->

## 备份、恢复与升级

### 1. 运维对象是一套完整状态

可恢复的 IMBoy 实例至少包括：后端发行物与运行时版本、数据库、Garage 元数据与对象文件、应用本地状态、应用密钥/RSA、现场配置和 TLS 材料。只保存数据库 dump、只保存 Garage 数据目录、只保留一份 `site.env` 都不构成完整恢复包。

单节点 Garage 和本机备份不能抵御整机或整盘损毁。现场需确定异机备份目的地、加密方式、权限、留存周期、RPO/RTO 和定期恢复频率；在 `templates/site-handover.tsv` 登记负责人。

### 2. 生成一致的备份

本包的备份流程需要维护窗口，按顺序停止 Nginx、后端、Garage 和 PostgreSQL，确认四个服务均已停止，再做完整冷快照。预签名上传、消息发送和下载在此期间不可用，应提前通知并让客户端按产品设计重试。确认不存在另一进程或外部任务写入同一数据目录。

```bash
sudo bash ./backup.sh
```

脚本冷拷贝整个数据库集群、Garage 元数据/对象、配置、秘密和后端本地状态，生成 `/var/backups/imboy/` 下的 `.tar.gz` 与独立 `.sha256`，具体路径以命令返回为准。本包不是 `pg_dump` 逻辑恢复流程，也不提供任意时间点恢复。备份目录至少需当前配置与数据总大小的两倍再加 1 GiB 余量。脚本结束时恢复备份前运行的服务并等待相应 readiness，恢复失败返回非零；随后仍应执行 `verify.sh` 和业务抽查，不能仅凭归档存在判断成功。

归档权限应为仅 root 可读写。**归档包含真实秘密与业务数据，默认不是加密文件。** 应按现场策略加密后离机传输，目的端限制访问；未经脱敏不得作为普通验收附件上传。不要将加密口令放进命令行、包内说明或同一归档。

每次备份记录：开始/结束时间、停止写入窗口、源 RELEASE_ID/SHA、数据库与 Garage 版本、归档字节数和哈希、命令退出码、服务恢复结果、离机复制位置与下一次恢复抽查日期。档案哈希只证明字节完整，不证明内容可恢复。

PostgreSQL 官方文件系统备份要求普通文件复制时先关闭数据库，并备份完整集群；本包为保持数据库与对象存储同一窗口而一并停止四个服务。[官方文件系统备份说明](https://www.postgresql.org/docs/18/backup-file.html)。外部 tablespace、符号链接数据目录或另挂 WAL 不在本包冷快照合同内，检查会拒绝此类布局。

### 3. 恢复只在新的隔离目标上进行

当前 `restore.sh` 面向全新隔离目标，拒绝覆盖已安装实例。它不是生产库原地覆盖按钮。恢复须在**产生该快照的同一个原始完整候选目录**下执行，版本号相同但候选哈希不同也会拒绝。准备好可信的归档与独立校验文件后，示例如下：

```bash
read -r imboy_backup_sha imboy_backup_name < /srv/restore/imboy-backup.tar.gz.sha256
sudo bash ./restore.sh \
  --from /srv/restore/imboy-backup.tar.gz \
  --sha256 "$imboy_backup_sha" --isolated-target --check
sudo bash ./restore.sh \
  --from /srv/restore/imboy-backup.tar.gz \
  --sha256 "$imboy_backup_sha" --isolated-target --apply
```

恢复前准备同一版本的已组装候选包、匹配的系统/架构及冻结主机依赖、完整归档及其哈希。需要安装系统依赖时，在原候选目录执行 `sudo bash ./tools/prepare-host.sh --apply`；恢复入口不会自动修改系统包。目标网络必须与生产流量隔离；在验证前不要让生产 DNS 指向恢复机，也不要让测试客户端向生产环境发送消息。恢复归档可能含生产秘密，因此恢复主机按生产等级保护。

恢复步骤：

1. 核对源实例身份、备份时间、归档哈希、数据库/Garage 版本与应用 SHA。
2. 准备空主机和匹配的发行物；确认没有需要保留的现有 IMBoy 数据。
3. 在原始候选目录中按上述归档参数执行恢复；需要续跑时使用同一归档/hash 并加 `--apply --resume`。不通过删目录绕过“已有安装”保护。
4. 检查数据库、Garage 元数据和对象、应用密钥及本地状态都已恢复，不缺任何一组。
5. 按 `05_ACCEPTANCE.md` 验证数据库记录、消息、私有/公开附件、管理员/用户登录和后端重启；恢复前后检查同一批测试对象的 SHA-256。
6. 由验收人确认 RPO/RTO 和全部必验项目后，再决定是否切换正式流量。切流不是恢复脚本的隐式步骤。

如果恢复机不能使用原域名，应通过受控解析或专门恢复配置进行验证；不能只改一个 URL 就宣称所有签名和回调已适配。原域名的预签名 URL 不会因为换了目标主机自动变成新域名 URL。

### 4. 升级前必须有明确路径

本包的后端升级是维护窗口内的发行切换，不承诺无停机。脚本要求 PostgreSQL 和 Garage payload 字节与已安装运行时完全一致；即使只是数据库小版本变化也需独立的运行时升级方案，不能夹在后端切换里自动执行。

升级入口要求现有实例已完成安装、恢复或上一次技术升级；未完成首次管理员初始化、安装失败或未解决的升级失败会被拒绝。预检查后实际状态发生变化也会要求重新检查。开始切换前记录 `UPGRADING`，首次安装的 `--resume` 不允许处理这一阶段。

升级也检查目标候选的主机软件包版本锁。差异必须先按发布方已验证的维护方案处理；升级入口不会临时更换系统库，也不会因为机器上已有同名命令就忽略版本不符。

升级前至少确认：

| 项目 | 必须有的证据 |
| --- | --- |
| 新候选来源 | 固定 SHA、RELEASE_ID、完整哈希、相同平台与许可来源 |
| 版本兼容 | 当前版本 → 目标版本的明确允许路径；运行时和扩展兼容矩阵 |
| 数据迁移 | 待执行迁移清单、涉及表和耗时评估；已在同版本副本验证 |
| 备份恢复 | 当前完整备份可在隔离目标恢复；源版本二进制可获得 |
| 验收范围 | 升级前后相同账户、消息、附件与业务不变量检查 |
| 失败处理 | 人员、维护窗口、最大可用时长、停止/前修/恢复决策点 |

先在隔离副本演练相同路径，再在正式维护窗口运行。命令中的目录应是已组装的新候选目录：

```bash
sudo bash ./upgrade.sh --package /path/to/new-candidate --check
sudo bash ./upgrade.sh --package /path/to/new-candidate --apply
```

入口先校验新候选，再转交新候选自己的升级脚本，由目标候选维护渲染器和 systemd 单元。升级停止写入并创建完整备份，安装新 release，再执行迁移与启动检查；上游配置变化必须先通过渲染器摘要与适配检查。迁移时专用 PG 的 `imboy` 角色临时为 SUPERUSER，应用公网入口保持关闭，成功或失败路径均撤销这项临时授权。ready 后仍需完成真实业务验证，再恢复对用户的服务承诺，不只看 `systemctl is-active`。

### 5. 升级失败不能盲切回旧二进制

本包不在迁移失败后自动回切旧应用。数据库可能已执行部分或全部新迁移，旧二进制未必能读取新结构。保留失败现场和备份，不手工将迁移标记改成成功，也不随意执行全部 `down.sql`。

根据发布负责人事先确认的方案，选择修复后继续，或使用升级前完整备份与旧版本发行物恢复到新的隔离目标。恢复后验证完整状态再考虑切流。仅修改 `/opt/imboy/current` 不能恢复数据库、对象存储或秘密。

如果新版本已接受真实写入，恢复到升级前备份会丢失这段写入，必须明确其数据损失窗口与业务决定；这不能由脚本默认为可接受。

固定源码在 `imboy_app:start` 中要求迁移成功，dirty 或迁移失败会阻止后续启动。[启动流程](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl)、[迁移执行器](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_migrate.erl)。

### 6. 日常检查与定期任务

关注证书到期、磁盘/ inode、数据库/WAL 增长、Garage 容量、系统时间、后端 readiness、失败登录/上传与备份任务结果。采用现场监控平台时记录告警接收人；本包不预置长期监控或定时任务，也不将它们视为已启用。

业务高峰、配置变更、证书轮换、备份恢复和升级后各执行一次对应范围的验收。例行检查不要读取或公开输出秘密文件，日志共享前先按 `06_TROUBLESHOOTING.md` 脱敏。

---

<!-- Source: docs/05_ACCEPTANCE.md -->

## 部署验收与陌生人交付演练

### 1. 验收目标

验收需要证明：一位没接触过 IMBoy 代码库、具备基础 Linux 运维能力的人，仅凭最终交付材料，可以安装、初始化、完成核心业务、重启、备份恢复、按允许路径升级，并能根据材料定位常见故障。

同一候选的二进制、配置、产品 profile 和迁移清单必须固定。中途替换脚本或版本后，受影响的项目重新执行；旧包结果不能自动算作新包通过。

### 2. 结果状态

| 状态 | 什么时候使用 | 必须记录 |
| --- | --- | --- |
| `PASS` | 已实际执行，预期断言全部满足 | 命令/动作、时间、候选标识、结果与脱敏证据 |
| `FAIL` | 已执行，实际结果不满足预期 | 失败断言、退出码/响应、现场状态、问题编号 |
| `BLOCKED` | 不能执行或无法可靠判断，因为输入、环境或上游缺失 | 缺什么、责任人、恢复条件；不能当作跳过后通过 |
| `NOT_RUN` | 尚未执行 | 计划执行人和时间；本包运行验收的初始状态 |
| `NOT_APPLICABLE` | 合同/产品范围明确不要求此项目 | 具体范围依据与验收人；必验核心项目不能随意取消 |

脚本进程返回 0 不自动使业务项目变成 `PASS`。未执行要说明是环境缺失、范围外、上游失败还是尚未安排，不能统一写成“跳过”。

### 3. 演练组织方式

选择未参与 IMBoy 开发和本包编写的执行者。只提供最终候选目录、安装手册、已填非秘密现场表、现场证书/身份材料、客户端候选包和工单渠道；不要把开发者聊天记录作为隐含安装步骤。

使用全新专用主机，网络和权限接近实际交付环境。安装前单独记录“环境准备耗时”；从执行 `install.sh --apply` 开始记录“安装耗时”，从开始 UAT 到签收记录“验收耗时”。备份、恢复和升级各单独计时，不将下载依赖、等待证书等时间藏起来。

开发者解释、登录主机、修改配置或口头补充步骤，都算一次介入。记录原因、用时、是否说明手册缺失和应修改哪个文件。执行者可按手册排查，但不能由开发者接管后继续声称“陌生人独立完成”。

同一问题三次尝试仍未解决，停止重复安装，保留状态与证据。发布方修订材料后，从适当的干净环境或已声明续跑点重新验证。

### 4. 基础自动检查的边界

`verify.sh` 的实际检查列表以脚本报告为准，用于核对服务、版本/readiness、12 个数据库扩展、迁移 HEAD、运行角色已降权、管理员存在、三域 TLS/本机代理、DNS 可解析以及公网初始化路径被拦。`--s3-write` 额外走本机 Nginx HTTPS S3 域名入口，执行真实 SigV4 临时对象往返；它不代替外网解析与真实 APP 预签名/确认流程。

`/livez` 表示进程能响应；`/readyz` 与 `/healthz` 聚合数据库连通性和 TSID readiness。200 不表示对象存储上传、移动推送、SSO 或业务权限已验证。[健康检查源码](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/api/healthz_handler.erl)。

### 5. 必验的端到端主线

| 步骤 | 操作 | 通过条件 |
| --- | --- | --- |
| 首装 | 全新主机从最终候选执行安装 | 没有临时读源码或拼接遗漏配置；脚本失败有明确定位；版本一致 |
| 管理员 | 通过本机入口创建首个管理员，验证重复初始化行为 | 首次成功；重复调用不重置/覆盖已有管理员；无通用默认口令 |
| 连接与登录 | 两个测试用户从实际客户端网络登录 | HTTPS/WSS 正常；账户身份正确；WebSocket 路径为 `/api/v1/ws` |
| 双向消息 | A→B、B→A 各发文字，B 断开后 A 再发，B 恢复 | 消息内容/顺序符合需求；无意外重复；离线消息可达 |
| 私有附件 | A 上传私有文件，许可用户下载 | 上传确认成功；字节与源文件 SHA-256 相同；未授权者不能读取 |
| 公开附件 | 上传明确允许公开的测试资源，从公开域读取 | 地址属于 FILES_DOMAIN；字节一致；私有桶未开放 |
| 错误与边界 | 过期链接、错误权限、超限制大小、上传中断后重试 | 按合同失败；无静默损坏；重试不产生错误可见对象 |
| 重启 | 受控重启服务器，再执行主流程 | systemd 自动恢复；原数据/密钥保留；登录、消息、附件可用 |
| 备份恢复 | 带种子数据实例备份，在空隔离主机恢复 | 数据库、文件和秘密匹配；业务通过；记录实际 RPO/RTO |
| 升级 | 同版本副本按允许路径升级，再复验同一测试集 | 迁移成功；原消息/附件保留；主要行为满足目标合同 |
| 失败恢复 | 在演练环境走发布方认可的失败处理路径 | 可用旧发行物和完整备份恢复；不靠删数据或任意 down 迁移 |

使用专用测试用户、测试群和临时文件。不要把真实客户消息、手机号、Token、完整预签名 URL 记录进对外报告。文件哈希可用于比对，但原文件仍按数据分类保护。

### 6. 恢复验收必须比“成功解压”更强

先创建测试数据：两名用户、双向消息、离线消息、一份私有文件、一份公开文件与关联记录；登记 ID 的脱敏形式、计数和源文件 SHA-256。备份后恢复到另一台空主机。

恢复机必须满足：源候选匹配；关键记录/计数一致；文件下载后哈希一致；权限正确；管理员和测试用户可合法进入；后端重启后仍正常。schema 存在、归档可读或 PG 启动，都不够成为恢复结论。

如果因恢复网络/域名隔离无法执行业务，记录 `BLOCKED` 并说明条件，不改成通过。不要将恢复机的生产秘密提交进验收表。

### 7. 按合同增加的项目

| 业务范围 | 需增加的真实验证 | 缺少材料时 |
| --- | --- | --- |
| 企业 IdP / SSO | 首次进入、精确回调、组织切换、退出、会话到期、停用撤权 | `BLOCKED`，记录 IdP/回调/测试身份缺口 |
| 客服 | 首次开通、无人在线/排队、接单、双向附件、转交、结束、历史查找、坐席撤权 | 需要管理前端/Widget 及配置；后端启动不代表业务链通过 |
| 组织/工作台 | 组织身份、成员/权限边界、跨组织隔离、授权变化 | 需要匹配客户端与初始化业务数据 |
| 浏览器上传 | 实际 origin 的 OPTIONS、上传、确认、下载 | curl 不能代替浏览器 CORS |
| 推送/短信/SMTP | 受控测试号码/地址、失败/重试、供应商回执 | 缺供应商则相关项阻塞，不影响已有其他证据 |

未包含的 RTC/Agent/Bot/E2EE 能力不得填 `PASS`。需要这些能力时，先调整正式范围和 profile，再制作部署与验收材料。

### 8. 签收条件

填写 `templates/acceptance.tsv`、`templates/rehearsal-log.tsv`、`templates/handover-signoff.tsv`。必验项目全部 `PASS`，适用的条件项目全部 `PASS`，剩余 `NOT_APPLICABLE` 有范围依据和签字，才能签收该范围。

记录候选 SHA/RELEASE_ID、包哈希、平台/架构、三域名、profile、数据库/对象存储版本、各阶段耗时、开发者介入次数、开放问题、备份和证书负责人。签收表不填写密码、私钥、Cookie、Token 或数据库 dump。

---

<!-- Source: docs/06_TROUBLESHOOTING.md -->

## 常见故障排查

先保存时间、候选标识、失败阶段、原命令和退出码，再检查对应服务。只共享脱敏片段。不要发送 `secrets.env`、`backend.env`、`sys.config`、`vm.args`、完整 journal、数据库 dump、Token 或完整预签名 URL；这些文件或日志可能包含秘密和客户数据。

### 1. 诊断顺序

先核对包和参数，再核对系统/架构/权限，随后检查数据库、Garage、后端、Nginx，最后从真实客户端网络验证 DNS/TLS/业务。按依赖顺序定位，避免把数据库错误当作 Nginx 故障反复重启入口。

可以只读检查：

```bash
cat /etc/os-release
uname -m
timedatectl status
df -h
df -i
ss -lnt
sudo bash ./verify.sh
```

本包服务名为 `imboy-postgresql`、`imboy-garage`、`imboy-backend` 和 `nginx`。`systemctl status`、`journalctl` 输出先在本机审阅脱敏，不直接整段贴到工单。`systemctl cat`、完整环境和生成配置也可能暴露秘密，不作为默认支持材料。

明确标记的退出码：2 表示输入错误；3 表示缺依赖/发行物/配置或完整性问题；4 表示并发、已有状态或操作冲突；5 表示服务/验证失败；6 表示还需交互完成首次管理员。外部命令也可能返回其他非零值，始终结合 `ERROR [标签]` 与失败阶段判断，不能只按数字猜原因。

### 2. 故障索引

| 编号 / 现象 | 优先检查 | 处理 / 停止条件 |
| --- | --- | --- |
| F01 缺 payload / 发行物不完整 | 是否仅收到材料模板；backend/PG/Garage 是否真实存在 | 由发布方组装完整候选；不让客户临时构建或改清单 |
| F02 哈希不匹配 | 候选是否混用，传输是否截断，文件是否编辑 | 重新取得可信候选；不重写 hash 让检查变绿 |
| F03 系统/架构不符，Exec format error | release 与 OS_ID/OS_VERSION_ID/ARCH 是否一致 | 获取匹配发行物；不能在 ARM 上用 amd64 产物 |
| F04 动态库缺失或 NIF 加载失败 | ERTS/NIF/PG 扩展实际依赖 | 由发布方补系统包/重新构建；不要软链到不同 ABI |
| F05 端口或固定路径已存在 | 是否已有 IMBoy/PG/Garage/Nginx 或其他业务 | 首装拒接管是预期；转独立迁移方案，不删除现有目录 |
| F06 占位符或非法参数 | site.env 格式；域名是否带协议/路径 | 按模板修正；不使用 eval/source 强行加载 |
| F07 证书缺失、过期、域名不符 | 三组 PEM、SAN、有效期 | 取得正确证书；不使用 -k 或明文公网服务绕过 |
| F08 私钥与证书不匹配 | 比较证书与私钥的公钥摘要 | 使用同一证书对并核对权限；不输出私钥 |
| F09 服务账号不能读密钥 | 用户/组、父目录权限、文件模式 | 仅给指定账号必要权限；禁止 chmod 777 |
| F10 PG 启动失败 | 版本、数据目录权限、磁盘、端口、preload 扩展 | 保留日志；缺扩展补发行物；不 initdb 覆盖现有数据 |
| F11 缺扩展 / create_hypertable 未定义 | 是否包含 TimescaleDB 等必需扩展，版本是否匹配 | 通用 PG18 不足；补完整运行时 |
| F12 后端连接 PG 失败 | IMBOY_PG_*、认证规则、socket/TCP、PG readiness | 核对同一身份；不把认证改成公网 trust |
| F13 dirty migration / 迁移失败 | 迁移编号、已有结构、目标版本 | 停止升级保留现场；在副本分析，不改迁移表冒充成功 |
| F14 严格配置校验失败 | 秘密、RSA、存储注册表、profile | 按合同修正；不改开发模式绕过 |
| F15 Garage 无布局/容量不足 | node id、布局版本、容量与余量 | 确认实际状态后续跑，不盲 apply 多版本 |
| F16 Garage 认证/桶权限错误 | endpoint、密钥、私有/公开桶、授权 | 核对同一套秘密；不公开私有桶作为修复 |
| F17 API 502 / 503 | 后端是否运行；127.0.0.1:9800；db/tsid | 502 查上游，503 查 readiness；不只重启 Nginx |
| F18 WebSocket 404 / 不能升级 | 是否 /api/v1/ws；Upgrade 头、超时 | 修正客户端/代理；旧 /api/ws 或 /ws 不是当前合同 |
| F19 S3 SignatureDoesNotMatch | s3 独立域、Host/路径/query、region | 保留签名语义；不改回 api 域的 /s3 |
| F20 链接过期 / 时钟错误 | 客户端与服务端时间、URL 有效期 | 修正授时后重取 URL；不无限延长有效期 |
| F21 curl 成功而浏览器失败 | origin、OPTIONS、允许头、CORS | 用实际来源浏览器验收，不把网络通当跨域通过 |
| F22 公开文件 404 / 私有文件被公开 | FILES_DOMAIN Host 映射、Website、对象所属桶 | 区分公开/私有；不能对所有桶启用 website |
| F23 大文件失败/断流 | Nginx/网关大小和超时、磁盘、产品限制 | 对齐批准边界，核对重试对象状态，不仅提高限额 |
| F24 管理员初始化失败 | 是否已有管理员、账号格式、口令规则、DB | 已完成不能重置；使用本机入口并记录业务错误 |
| F25 备份失败/不完整 | 空间、停止写入结果、冷拷贝/归档退出码、服务恢复结果 | 保留失败记录，确认服务恢复；失败归档不能签收 |
| F26 恢复被拒绝 | 目标已有数据、版本不符、归档 hash 失败 | 使用空隔离目标/正确产物；不删除保护状态 |
| F27 升级失败 / undefined_column | binary × schema 兼容与迁移结果 | 按前修/完整恢复方案处理；不只切 current 链接 |
| F28 续跑被拒绝 | 候选、site.env、状态、路径、秘密有无变化；是否已进入升级/恢复 | 首装仅续跑同一身份；升级/恢复使用对应处理流程，不删状态冒充首装 |
| F29 外网失败，本机成功 | DNS/AAAA、ACL、代理/WAF、证书链、客户端 CA | 从实际来源逐段验证；本机 curl 不能替代外网路径 |
| F30 内存/磁盘耗尽 | 数据库/WAL、Garage、日志和备份增长 | 保全数据后扩容/调整留存；不删 DB/WAL/对象腾空间 |
| F31 主机依赖精确版本不可取得 | 冻结包版本、发行版、已批准源是否匹配 | 由发布方/环境方恢复相同版本来源，不把清单改成 latest 或换其他发行版源 |
| F32 RENDERER 配置摘要不符 | release 模板、CONFIG_SHA256、渲染器基线是否同一合同 | 发布方适配并重新验收，不手改哈希绕过 |

### 3. 健康端点解释

`/livez` 活着不代表依赖就绪。`/healthz` / `/readyz` 的 503 应结合 `db` 和 `tsid` 判断。当前就绪检查不读每张业务表，也不验证 S3/SSO/短信；200 之后业务仍需逐项测试。

内部探针与代理看到的来源不同，版本展示字段不用于证明外部访问权限。验收结合固定发行信息和实际运行产物，不能只依赖公网一段版本文本。[健康检查实现](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/api/healthz_handler.erl)。

### 4. 最小支持材料

提供故障编号、SHA/RELEASE_ID、OS/架构、时间与时区、阶段、脱敏命令、退出码、期望与实际、服务状态、已尝试动作及次数。网络另附“来源网络 → 域名/端口 → 结果”；证书附 hostname/issuer/有效期；附件附类型、大小和脱敏哈希比对。

不提供数据库/S3/管理员口令、Cookie/JWT、RSA 私钥、密钥文件、客户消息和可访问私有资源的链接。深入诊断使用现场授权的受控支持渠道，不把完整备份当作普通邮件附件。

---

<!-- Source: docs/07_RELEASE_ASSEMBLY.md -->

## 发布方组装、版本冻结与证据

本文供 IMBoy 发布负责人使用。客户执行者只应收到组装后的候选目录，不需要源码、Git、编译器、Erlang 开发环境或构建网站账号。

### 1. 当前交付材料和可安装候选的区别

2026-11-05 初次生成的工具包材料版尚无原生二进制。本文会原样进入后续组装候选，**实际目录以 `metadata/release.env` 的 `PAYLOAD_READY` 为准**：`no` 表示仍是材料版；`yes` 表示已组装，但不表示 UAT 通过。发布方取得真实构建产物后运行 `prepare-payload.sh`，由工具核对平台、SHA、版本、文件集合、扩展与哈希，生成新目录。不能手改 `PAYLOAD_READY` 绕过组装。

2026-11-05 初始材料整理时未发现可直接作为本包 payload 的公开原生 release asset。仓库原有容器发行工作流不能自动证明原生 tar 发行物存在。[发行工作流](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/.github/workflows/release.yml)。

源码固定为 `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`，VERSION 与 relx release 均为 `1.0.0-alpha.83`。`RELX_REL_VSN` 不能被当作已验证的版本覆盖机制；核对生成文件中的实际版本。[relx 配置](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/relx.config)。

本目录同时冻结迁移头 `243` 和原始配置摘要。2026-11-08 仓库集成核对的 main 快照 `8957f0ed6cf7b3251259dbdf777fc1ae43837574` 为 alpha.84／迁移 `275`，其配置摘要已变化；**本次没有完成该快照的渲染器适配、发行物构建或升级认证**。不要把下面命令的 `--sha` 改成 main 后直接交付，也不要修改摘要常量以绕过拒绝。版本与摘要的完整对照、三套合成测试和 PR 检查范围见 [10_VALIDATION.md](docs/10_VALIDATION.md)。

### 2. 构建主机与输入

在独立的 Linux 构建机操作，系统发行版、发行版版本与架构须同目标机。工具接受 Ubuntu 24.04 x86_64 或 Debian 13 x86_64，但每个平台各自需要干净机认证，不能跨平台复用二进制。用户的 macOS 开发机可存源码和材料，不能把其本地产物直接当作 Linux release。

准备只读可访问的完整后端 Git 仓库，包含固定 SHA。构建脚本使用 `git archive`，不带入本地未提交配置，也不修改源仓。新输出目录必须不存在，避免覆盖已有构建或运行状态；失败时保留目录与日志，由发布方检查。重新构建选择新的目录。

发布工具依赖 Bash、GNU 工具、Git、make、C/C++ 编译工具、jq 等。`jq` 是二进制 JSON 工具，不需要 Python。构建网络、代理、可信 CA、下载来源与系统依赖由发布方准备。客户主机运行依赖也要冻结精确软件包版本，由环境方预装或 `tools/prepare-host.sh` 从批准的发行版源安装，不让客户猜动态库清单。

### 3. 纯 Shell 构建参考流程

下列路径是发布机示例。不要在已经安装 IMBoy 的主机上执行源码依赖构建，因为最终 PG prefix 必须保持为空，不能覆盖运行中的数据库软件。

先检查脚本帮助，按其中清单通过批准的软件源准备构建依赖：

```bash
bash tools/build-native-dependencies.sh --help
bash tools/build-backend.sh --help
bash tools/prepare-payload.sh --help
```

依赖构建示例：

```bash
bash tools/build-native-dependencies.sh \
  --repo /srv/src/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --work /srv/imboy-build/deps-cb95fa72 \
  --otp-prefix /srv/imboy-build/otp29 \
  --pg-prefix /opt/imboy/runtime/postgresql \
  --jobs 2
```

该流程依据固定源码的原生 CI pins，提供 OTP 29.1.2、PG 18.6、PostGIS 3.6.4、TimescaleDB 2.23.1、pgvector 0.8.7、固定 pg_jieba 与 Garage 2.4.1 的构建/供应配方；其中档案 SHA-256、Git commit、子模块和词典均按 pin 核对。具体锁定值以固定源码和输出元数据为准。[原生 pins](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-pins.json)。

脚本不自动提权、不执行系统包安装、不启动服务。源码构建可能耗时较长，且本次未在目标构建机实际跑完整链；不可把该配方的存在当作产物已经供应。

使用新构建的 OTP 生成后端：

```bash
PATH=/srv/imboy-build/otp29/bin:$PATH \
bash tools/build-backend.sh \
  --repo /srv/src/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --out /srv/imboy-build/backend-cb95fa72 \
  --jobs 2
```

脚本在独立目录中的 `work/imboy` 构建，保持构建目录 basename 为 `imboy`。输出 `backend/` 包含自包含 release、同源迁移、CLI、产品清单、构建收据和文件哈希；应用不在构建阶段启动。脚本拒绝通过 PATH 调用 Python 或 Docker，不靠跳过上游质量门来伪装成功。遇到上游构建新增此类依赖，应明确失败并由发布方处理，不能偷偷安装 Python。

### 4. PostgreSQL 不能只用通用空发行包

本安装合同检查这些扩展：`pgcrypto`、`pg_jieba`、`timescaledb`、`vector`、`postgis`、`pg_trgm`、`btree_gin`、`btree_gist`、`citext`、`unaccent`、`intarray`、`pg_stat_statements`。必须同时提供扩展控制文件、SQL、共享库与词典；仅存在 `.control` 文件不足以证明 SQL 能执行。

PG prefix 按 `/opt/imboy/runtime/postgresql` 生成完整 `bin/lib/share`，不要直接拼接来自系统不同目录/不同版本的文件。本包的升级入口要求 PG/Garage payload 字节保持一致；它们的升级需另做恢复和兼容演练。

### 5. 组装客户候选

依赖和后端构建完成后运行：

```bash
bash tools/prepare-payload.sh \
  --repo /srv/src/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --release-dir /srv/imboy-build/backend-cb95fa72/backend \
  --pg-prefix /opt/imboy/runtime/postgresql \
  --garage-bin /srv/imboy-build/deps-cb95fa72/garage \
  --release-id native-20261105-cb95fa72 \
  --out /srv/imboy-deliveries/native-20261105-cb95fa72
```

输出是全新目录，包含安装/运维入口、文档模板、三个 payload、发行信息和 `CHECKSUMS.sha256`。工具核对 build receipt、源码 VERSION、profile、迁移集合、无秘密配置模板、Garage 固定 hash、PG 必需扩展与词典，并拒绝可疑私钥/数据/日志与不支持的链接。

这些检查不能替代发布者对构建来源、二进制夹带内容和依赖许可的负责。保留依赖来源与许可文本，不把一个哈希文件当成全面审计或签名。

`site.env` 是客户后续填写的数据文件，不纳入不可变文件清单；程序会绑定实际接受的 site.env 以保护续跑一致性。不要在构建阶段写入客户秘密，不将构建日志、测试账户、真实配置和生产 vhost 夹带进通用包。

### 6. 版本冻结清单

| 需要冻结 | 应记录的内容 |
| --- | --- |
| 后端 | repo、完整 SHA、VERSION、RELEASE_ID、构建日期、产品 profile/manifest hash |
| 平台 | OS_ID、OS_VERSION_ID、ARCH、内核/运行库验证环境 |
| 主机软件包 | `metadata/host-runtime-packages.tsv` 中的包名/架构/精确版本、可取得该版本的批准源、无缓存安装证据 |
| OTP/ERTS | 构建版本、release 确认自包含、ERTS/NIF 动态库核对 |
| PostgreSQL | 精确版本、12 个扩展与词典版本/hash、编译 prefix |
| Garage | 版本、官方来源、二进制 SHA-256、单节点布局合同 |
| 数据迁移 | 同源完整迁移集合、HEAD、失败/dirty 处理、允许的升级源版本 |
| 配置渲染 | 模板摘要与渲染器基线一致；模板变化后的适配和运行验收 |
| 应用对接 | API/WSS/S3/公开文件合同；配对客户端版本与首屏配置协议 |
| 验证 | 适用平台的干净机首装、重启、真实业务、备份恢复、升级/失败恢复证据 |

当前材料不会声称已经签发发布者签名。发行方可通过自己的可信交付渠道核对最终 ZIP/tar 哈希；是否另签名按其现有发布流程执行。

### 7. 初始材料版待办与候选发布核对

下表 P01–P07 是初始材料制作时尚未闭合的发布任务，也是后续候选的核对项目。发布方完成后应为各项附上该候选的证据与关闭结论；它不表示任何组装后候选永远没有二进制或永远未测试。S 类由具体现场按实际情况补齐。

| 编号 | 类型 | 完成标准 |
| --- | --- | --- |
| P01 原生 payload 供应 | 发布方工程 | 三个真实产物组装成功、完整性通过，不是空模板 |
| P02 原生构建认证 | 发布方工程 | 在选定平台完成构建、扩展 SQL 检查、动态库检查；保留原始结果 |
| P03 空机安装与续跑 | 发布方工程 | 独立执行者在干净机首装成功，中断恢复不重置数据/秘密 |
| P04 完整业务验证 | 发布方+验收人 | 登录、消息、私有/公开附件、重启、权限等必验项全部通过 |
| P05 恢复与升级验证 | 发布方+运维 | 同版本完整恢复、目标升级路径与失败处理都有数据断言 |
| P06 客户端协议配对 | 发布方+客户端负责人 | 新生成的配置加密/签名材料与配对 Flutter/管理端初始配置合同一致；不能沿用不匹配的硬编码值 |
| P07 依赖与供应链记录 | 发布方工程 | 固定来源/版本/hash、完整许可文本与允许交付范围；无秘密/生产数据 |
| S01 主机和网络 | 现场输入 | 专用主机、时间同步、三域 DNS/端口、足够空间与权限 |
| S02 证书和域名 | 现场输入 | 三组匹配且有效的 PEM、可信链、续期责任人 |
| S03 运维目标 | 现场输入 | 维护窗口、备份目的地与留存、RPO/RTO、操作与验收负责人 |
| S04 外部集成 | 按范围的现场输入 | IdP/SMTP/短信/推送材料、测试身份与对应 UAT |

P 类不能用“等客户填密钥”概括，必须由发布方完成。S 类也不应由安装脚本猜测。

### 8. 与此前材料的明确变化

本包使用原生 systemd 与 Shell 准备/安装入口；不复用此前 Python 组装工具，也不走 Docker Compose。核心服务的三域名与此前包含管理后台、Widget、RTC/TURN 的整栈五域拓扑不同；增加其他产品组件时需补对应材料。

S3 公开签名入口统一为独立 `https://s3.example.com`。真实 WebSocket 路径使用 `/api/v1/ws`，监听变量使用 `HTTP_PORT`。这些修正来自本次固定源码核对，不能将旧示例直接搬回新配置。

---

<!-- Source: docs/08_DEPLOYMENT_EXAMPLES.md -->

## 部署配置示例与生成结果

本节供发布审阅、网络准备和故障定位。客户按 `01_INSTALL.md` 运行安装器即可，不必手工拼装本节内容。示例域名属于占位符，含 `GENERATED_ON_TARGET` 的项不是可运行秘密。

### 1. 配置示例清单

| 文件 | 内容 | 如何使用 |
| --- | --- | --- |
| `templates/site.env.example` | 七个现场非秘密参数 | 复制到包根目录 `site.env` 后填写 |
| `templates/backend.env.example` | 后端实际接收的运行环境变量 | 对照定位；不直接覆盖已初始化的运行文件 |
| `templates/nginx.conf.example` | 三域名 HTTP/TLS/WSS/S3/公开文件代理 | 从本包生成函数导出，无真实证书私钥 |
| `templates/systemd/imboy-postgresql.service` | 原生 PG18 单元 | 同安装器逻辑，仅供审阅 |
| `templates/systemd/imboy-garage.service` | Garage 单节点单元 | 同安装器逻辑，仅供审阅 |
| `templates/systemd/imboy-backend.service` | 自包含 Erlang release 前台单元 | 环境指向 `/etc/imboy` |
| `templates/garage.toml.example` | Garage 目录、端口、region 与 Website 合同 | 秘密由现场生成，不从示例复制 |
| `templates/postgresql.conf.example` | 专用 PG18 配置片段 | 生成的 `imboy.conf` 参考，不是完整集群配置 |

这些示例未在真实服务器独立启动测试。安装器生成具体值，Nginx 语法检查通过后再加载；文件可读不等于目标机运行通过。

### 2. 三入口映射

| 客户 URL | 内部服务 | 关键配置 |
| --- | --- | --- |
| `https://api.example.com` | `127.0.0.1:9800` | API 保留 Host；WSS Upgrade 支持 `/api/v1/ws` |
| `https://s3.example.com` | `127.0.0.1:3900` | `proxy_pass` 不追加 URI；Host、查询串、签名路径不变 |
| `https://files.example.com` | `127.0.0.1:3902` | Host 改为 `imboy-public.garage.localhost`；只允许 GET/HEAD |

S3 公网 endpoint 为 `https://s3.example.com`，内部 endpoint 为 `http://127.0.0.1:3900`。公开文件 base URL 为 `https://files.example.com`，由 Website 读取独立公开桶。三者不能合并成带 `/s3` 的代理路径。

Nginx 示例的 API/S3 请求体上限为 `500m`。这只是代理配置，不是已承诺的产品上传大小；后端、客户端、网关上限仍需一起核对，超限与中断重试须真实验收。

默认访问日志不记录查询串，但其他日志仍可能含敏感字段，分享前必须审阅脱敏。公网 `/metrics` 与 `/api/adm/setup/` 在代理层拒绝，管理员初始化走本机入口。

代理语义参考：[Nginx proxy_set_header / proxy_pass](https://nginx.org/en/docs/http/ngx_http_proxy_module.html)。

### 3. systemd 与运行目录

后端由 `imboy` 用户执行 `/opt/imboy/current/bin/imboy foreground`，systemd 读取 `/etc/imboy/backend.env`。本版本 relx 运行配置入口为：

```ini
Environment=RELX_CONFIG_PATH=/etc/imboy/sys.config
Environment=VMARGS_PATH=/etc/imboy/vm.args
Environment=RELX_REPLACE_OS_VARS=
```

`RELX_REPLACE_OS_VARS` 必须为空值；该版本按非空判断，字符串 `false` 也会启用替换。持久写入放在 `/var/lib/imboy/backend`，release 由 root 管理。启动模板证据见 `09_SOURCE_EVIDENCE.md`。

PG 与 Garage 使用各自服务账号和目录。服务依赖表达启动顺序，实际 readiness 由安装器继续检查；`After=` 不是数据库已可接受业务查询的证明。

本包采用单 Erlang 节点。EPMD/分布端口只在本机使用；不能复制同一 TSID 身份启动另一个同时承载写入的实例。多节点需另做节点登记、共享状态与故障验证。

### 4. 数据库身份与迁移窗口

集群使用本机专用 socket 和受限 TCP。安装/升级只在本包专用 PG 集群内临时将 `imboy` 角色提升为 SUPERUSER，应用公网入口在此窗口保持关闭。成功核对后收回权限；失败路径停止入口/后端并撤销临时授权。运行验证检查该角色是非 superuser。异常断电等情况下恢复操作仍须核对实际角色状态，不对公网开放数据库。

迁移和扩展正确性需要真实 PG18 执行证明。静态 SQL、库文件存在或源码搜索不能代替迁移成功记录。

---

<!-- Source: docs/09_SOURCE_EVIDENCE.md -->

## 固定源码与部署合同证据

历史读取日期：2026-11-05（北京时间）。初始材料读取时 `main` 为 `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`，后端 `VERSION=1.0.0-alpha.83`。以下链接均固定到该提交，不引用会移动的默认分支作为发行身份。

### 1. 核对结果与安装器决定

| 主题 | 核对到的事实 | 本包如何处理 | 直接来源 |
| --- | --- | --- | --- |
| 自包含发布 | relx `dev_mode=false`、`include_erts=true`、扩展启动脚本开启 | 客户取得含 ERTS 的原生 release，目标机不另编译 Erlang | [relx.config](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/relx.config) |
| 配置准备 | Makefile 将选定配置复制为 `config/sys.runtime.config` | 发布方只从固定源码无秘密模板构建；运行时配置在 `/etc/imboy` 单独生成 | [Makefile](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/Makefile) |
| HTTP 端口 | 启动时读取 `HTTP_PORT` | 使用 `HTTP_PORT=9800`；不使用无读取方的 `IMBOY_HTTP_PORT` | [imboy_app.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl) |
| WebSocket | 实际路由 `/api/v1/ws` | 公网地址固定为 `wss://API_DOMAIN/api/v1/ws` | [imboy_router.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_router.erl) |
| 监听地址 | 由 `listen_ip` 配置决定 | 后端只监听回环，外部访问经 Nginx | [imboy_app.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl) |
| 严格秘密校验 | pro 环境需要 JWT、PG AES、管理员 Cookie 等正确格式且关键值独立 | 现场分别生成，保持原值续跑/升级；丢失后拒绝重新生成替代数据密钥 | [imboy_env.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env.erl)、[sys.config.example](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/config/sys.config.example) |
| S3 与公开读 | Garage 内网 S3 API、公网 S3 endpoint、公开桶 Website 是不同角色 | 签名 API 用 `https://s3.example.com`；公开 Website 用独立 `https://files.example.com`；保留签名 Host、URI、查询串 | [Garage 原生脚本](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/garage-install.sh)、[环境覆盖](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env.erl) |
| Garage 固定根 | 严格启动要求附件 registry；可从 Garage 配置派生 | 设置 `IMBOY_ATTACHMENT_REGISTRY_FROM_GARAGE=true`，首次写入后不擅改根的桶和端点 | [启动校验](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl) |
| PostgreSQL 依赖 | 原生 CI 的同版本迁移准备包含12项扩展 | 随包提供完整 PG prefix，检查扩展/动态库/词典，不能替换成裸 PostgreSQL | [native-eunit-pins.json](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-pins.json)、[native-eunit-run.sh](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-run.sh) |
| 自动迁移 | strict 迁移失败/dirty 会抛异常，在 HTTP 监听前阻止启动 | 停写期间执行；不伪造成功标记；失败保留现场 | [imboy_migrate.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_migrate.erl)、[应用启动](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl) |
| 健康端点 | `/healthz` 检查 PG 与 TSID readiness，不检查 S3 或真实业务 | 同时检查版本、DB、TSID；对象读写和业务 UAT 分开记录 | [healthz_handler.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/api/healthz_handler.erl) |
| 首次管理员 | 本机 setup 接口使用 RSA-OAEP，传入客户端 MD5 后的口令；事务内防重复 | 隐藏读入口令→MD5文本→OAEP/SHA256；仅回环调用，公网禁止 setup 路由 | [adm_setup_handler.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/adm/adm_setup_handler.erl)、[adm_setup_logic.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/logic/adm_setup_logic.erl)、[elib_cipher.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/elib_cipher.erl) |
| 产品范围 | 当前 `phase1_delivery` 清单及运行时交付锁限定功能 | 不安装 RTC/AI 额外服务，不把关闭的入口当成交付能力，不把常驻模块误称为完全裁剪 | [product-feature-manifest.json](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/config/product-feature-manifest.json)、[imboy_profile_preset.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_profile_preset.erl) |

12项扩展：`pgcrypto`、`pg_jieba`、`timescaledb`、`vector`、`postgis`、`pg_trgm`、`btree_gin`、`btree_gist`、`citext`、`unaccent`、`intarray`、`pg_stat_statements`。

### 2. 原生启动脚本的额外合同

IMBoy 固定依赖是 relx 4.10.0。本包核对了该版本的[扩展启动模板](https://github.com/erlware/relx/blob/v4.10.0/priv/templates/extended_bin)：VM 参数文件由 `VMARGS_PATH` 指定；`RELX_REPLACE_OS_VARS` 使用非空判断，所以写 `false` 仍会启用替换。本包将其设为空并使用独立配置；不使用无效的 `RELX_VM_ARGS` 或 `RELX_READ_ONLY`。

`start_erl.data` 可以没有结尾换行。本包配置读取处理了这种 EOF，不把已读到的有效版本误判为失败。配置渲染器绑定原始模板 SHA-256：

```text
cf111ef0af8d713fdd6c80e4fcfbfcaf01fd3161b048eb31dc1a1bb5a2da4616
```

新 release 的上游配置发生变化时，发布方必须适配和重新验证渲染器，不能仅换版本号就继续安装。升级先验证目标候选，再执行目标候选自己的升级入口、渲染器和 systemd 配置。

### 3. 初始材料与本次集成的验证边界

2026-11-05 初始材料整理时访问 [GitHub Releases API](https://api.github.com/repos/imboy-pub/imboy/releases?per_page=3) 返回空数组，因此没有把一个猜测的二进制下载地址写成已供应资产。公开构建文件与原生 CI 配方只用于确认构建合同；真实编译产物、完整安装与业务结果仍需发布方生成。

主仓固定 SHA 不会自动冻结所有外部依赖；部分依赖仍指向移动分支。组装器记录实际依赖提交并校验最终产物，不能据此宣称构建天然逐字节可复现。

本包备份采用停止数据库后的完整文件系统快照，并同时停止后端、入口和对象存储以保持同一写入窗口。PostgreSQL 对普通文件级备份的完整集群与停机要求见[官方文档](https://www.postgresql.org/docs/18/backup-file.html)。这一设计仍需要真实恢复演练证明可用，哈希正确不等于业务已恢复。

2026-11-05 的初始交付只形成材料包。2026-11-08 的仓库集成将材料纳入版本管理和独立合成测试工作流，没有因此完成真实构建、部署或业务验收，也没有完成 main 的 alpha.84／迁移 275 适配。权威验证范围与可重复命令见 [10_VALIDATION.md](docs/10_VALIDATION.md)；当前执行状态由对应 PR/CI 报告。

---

<!-- Source: docs/10_VALIDATION.md -->

## 验证范围、冻结版本与可重复检查

本章是 `deploy/native-shell` 的权威验证范围说明，更新于 2026-11-08。它定义各层验证能够支持的结论，不存放某次运行日志，也不替代候选发行物的实际验收记录。新执行结果以对应 PR 的检查状态、CI 输出和结果摘要为准。

### 1. 本目录冻结的源码合同

本次工作把既有 Shell 交付材料纳入仓库，并增加可重复合成检查。**没有把安装器适配到当前 main，也没有完成 alpha.83 → alpha.84 的升级认证。**

| 合同 | 本目录冻结基线 | 本次集成核对的 main 快照 |
| --- | --- | --- |
| 源码 SHA | `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af` | `8957f0ed6cf7b3251259dbdf777fc1ae43837574` |
| 后端版本 | `1.0.0-alpha.83` | `1.0.0-alpha.84` |
| 迁移头 | `243` | `275` |
| 原始 `sys.config` SHA-256 | `cf111ef0af8d713fdd6c80e4fcfbfcaf01fd3161b048eb31dc1a1bb5a2da4616` | `14e448376e6512ca5f25bcdd4825eb54bd323d2d753d2033825c63b272851232` |
| 本安装器状态 | 按此合同组装；仍需实际构建和部署验收 | 尚未适配，不在支持范围内 |

本地身份见 `metadata/release.env`，配置合同见 `lib/render.sh` 的 `IMBOY_NATIVE_CONFIG_SHA256`。源码依据为冻结版的 [VERSION](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/VERSION)、[原始配置](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/config/sys.config.example)，以及集成时 main 快照的 [VERSION](https://github.com/imboy-pub/imboy/blob/8957f0ed6cf7b3251259dbdf777fc1ae43837574/VERSION)、[原始配置](https://github.com/imboy-pub/imboy/blob/8957f0ed6cf7b3251259dbdf777fc1ae43837574/config/sys.config.example)。这里引用的是确定的提交，不追随以后移动的 main。

配置摘要不匹配时，封包和渲染必须继续拒绝。不能通过手改 `PAYLOAD_READY`、迁移头、源码 SHA 或摘要常量来代替适配。支持新版本需要重新核对配置字段、运行时依赖、迁移与功能范围，完成相应渲染器及安装合同修改，再构建新候选并完成真实首装、恢复和升级演练。本 PR 没有完成这项工作。

### 2. 历史合成验证与当前结果

2026-11-05 初始材料的历史记录为三套测试合计 **62 项通过**。这些数字描述原材料的那次执行，不是对今后每个提交自动继承的通过结论，也不是 62 项真实部署或业务验收。

| 测试入口 | 历史数量与结果 | 合成验证范围 |
| --- | --- | --- |
| `tests/run.sh` | 29 / 29 通过 | Bash 语法与帮助、缺 payload 拒绝、环境数据解析、独立秘密及续跑、配置摘要、文件集合、TLS 域名校验、RSA 本地往返、服务模板、合成备份与恢复拒绝 |
| `tools/test-payload-tools.sh` | 21 / 21 通过 | 合成 Git/构建收据、固定源码与配置合同、完整性、权限、扩展存在性、PG prefix、Garage hash、候选封包及失败条件 |
| `tools/test-host-tools.sh` | 12 / 12 通过 | 精确系统包锁、只读检查、版本不可用/更高版本/锁外依赖/删除计划拒绝、锁篡改与重复项、动态库归属和随包 PG 库路径 |

历史运行环境为 Ubuntu 24.04.3 x86_64，使用 Bash 5.2.21、OpenSSL 3.0.13、curl 8.5.0 与 jq 1.7。没有以该记录宣称 Debian 或 ARM 已认证。原材料中的执行日志不纳入仓库；当前 PR 的重跑及其后提交应由各自 PR/CI 报告结果，失败或未运行均不能沿用历史 PASS。

测试在随机临时目录生成合成配置、短期测试密钥、假发行物和服务状态桩，结束后清理。发行工具测试会调用本机只读 `dpkg-query` / `apt-cache`，并为缺少的 Nginx 包提供明确测试桩；主机准备测试的 APT 变更操作由桩代替。测试不会访问生产数据、执行真实 APT 更新/安装、启动 PostgreSQL/Garage/Nginx/IMBoy，或连接客户环境。

### 3. 在开发机重复运行

使用 Ubuntu 24.04 x86_64 环境，准备 Bash、GNU 常用工具、Git、OpenSSL、curl、jq、file/binutils 及只读 `dpkg-query` / `apt-cache`。三套脚本本身不安装系统软件，不需要 root，不需要已编译的后端 payload。CI 的环境准备和测试命令以仓库根目录的 `.github/workflows/native-shell-delivery.yml` 为准。

从仓库根目录执行：

```bash
cd deploy/native-shell
bash tests/run.sh
bash tools/test-payload-tools.sh
bash tools/test-host-tools.sh
```

每条命令都必须返回 0，并核对各自测试输出。任何命令非零时，先解决具体失败，再重新运行受影响的检查。需要归档输出时写入仓库外目录或由 CI 保存；不要把临时日志、生成密钥、合成数据或运行报告加入本目录。

`tests/run.sh` 包含 `bash -n` 语法项，但它不是 ShellCheck，也不等同于执行所有安装路径。三套脚本使用的桩验证预期顺序、输入拒绝和数据处理，不能代替真实系统服务行为。

### 4. 独立 PR 工作流

仓库根目录的 `.github/workflows/native-shell-delivery.yml` 在 Ubuntu 24.04 runner 上运行这三套合成测试。面向 `main` 或 `dev` 的 PR、以及对这两个分支的推送，在修改 `deploy/native-shell/**` 或该工作流文件时触发；工作流也支持手动触发。它独立于既有质量工作流。

CI 先为隔离 runner 安装测试所需的系统工具，再核对本目录的 `CHECKSUMS.sha256` 完整性，最后运行三套测试；这与测试内使用桩核对现场 APT 安装计划是不同层次。工作流自动覆盖本章列出的 29 + 21 + 12 项合同检查，不执行 OTP/PG 编译，不生成可交付二进制，也不部署、迁移或初始化实际服务。具体提交是否通过，以对应检查运行的状态和输出为准；本章中的历史数量不取代该结果。

### 5. 仍需真实候选补齐的发布门槛

| 门槛 | 所需真实对象与结果 | 当前文档能够作出的结论 |
| --- | --- | --- |
| 原生构建与组装 | 固定 SHA 后端 release、PG18 及扩展、Garage、目标运行库和许可清单 | 存在构建/组装流程；尚不能声称真实产物已经验收 |
| 空机首装与续跑 | 对应平台的干净主机、真实 payload、现场 DNS/TLS，独立执行者完成全流程 | 合成检查不证明实装成功 |
| 数据库迁移与权限 | 真实 SQL 迁移到候选目标、失败行为、临时权限撤销 | 迁移门禁与运行约定已文档化；真实执行仍需验证 |
| 客户端协议与业务 UAT | 配对客户端，管理员/用户登录、WebSocket 消息、权限及附件闭环 | `/healthz` 与本地 RSA 往返不能代替客户端闭环 |
| S3 和公网入口 | 实际客户端网络、DNS/TLS、预签名上传与确认、私有/公开读取 | `verify.sh --s3-write` 只补充本机经 HTTPS Nginx 的真实 S3 签名读写；仍不等于完整业务 UAT |
| 停机备份与恢复 | 真实数据库和对象数据、隔离恢复主机、恢复后的数据断言与 RTO | 合成归档和服务桩不证明业务可恢复 |
| 升级与故障处理 | 确定的旧/新候选、真实迁移和故障恢复路径 | 本 PR 不认证 alpha.83 → alpha.84，也不认证反向迁移 |
| 陌生人交付演练 | 未接触代码库的执行者，仅凭交付材料完成操作、计时并记录开发者介入 | 按 [05_ACCEPTANCE.md](docs/05_ACCEPTANCE.md) 和验收模板实际填写 |

上述项目在本次仓库集成中没有新增实装通过结论。`PAYLOAD_READY=no` 表示材料版；发布方成功组装后为 `yes`，仍须逐项填写 `PASS`、`FAIL`、`BLOCKED`、`NOT_RUN` 或适用的 `NOT_APPLICABLE`，并在验收系统或仓库外保存对应结果。完整客户步骤继续使用 [01_INSTALL.md](docs/01_INSTALL.md)、[04_OPERATIONS.md](docs/04_OPERATIONS.md) 和 [05_ACCEPTANCE.md](docs/05_ACCEPTANCE.md)。

---

<!-- Source: tools/README_PUBLISHER.md -->

## 发布方：制作原生二进制交付物

客户拿到的是组装后的完整候选目录，不需要源码、Git、编译器或独立安装 Erlang。下面操作只由发布方在独立构建机执行。工具包初始材料版 `PAYLOAD_READY=no`；收到目录实际状态以 `metadata/release.env` 为准。`no` 表示尚未含有二进制，不能改一个字段就冒充完整发行物。

本组工具采用 Bash、Git、GNU 工具、curl、jq 和项目本身的 Erlang 构建链，不调用 Python 或容器。固定源码的 `make rel` 消费已有产品特性生成结果，不重新执行 Python 生成器。如果后续版本引入必需的 Python 构建步骤，工具会失败，不会跳过步骤。

### 平台与固定输入

支持封装平台为 Ubuntu 24.04 x86_64 或 Debian 13 x86_64。每份候选只声明实际构建机的组合，目标机必须相同；这些组合仍需分别做真实安装验收。固定源码 SHA 为 `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`，后端版本 `1.0.0-alpha.83`，编译档 `phase1_delivery`，迁移头 `243`。

2026-11-08 集成到仓库后，这些冻结值仍不变。本次核对的 main 快照 `8957f0ed6cf7b3251259dbdf777fc1ae43837574` 已是 alpha.84／迁移 `275`，新 `sys.config` 摘要为 `14e448376e6512ca5f25bcdd4825eb54bd323d2d753d2033825c63b272851232`。本 PR 未适配它；保持配置摘要拒绝，不能仅更改 `--sha`、元数据或摘要常量宣称支持。完整边界见 [验证范围](docs/10_VALIDATION.md)。

源仓即使有未提交改动，工具也仅通过 `git archive` 导出指定 commit，不修改原仓工作树、索引、分支或远端。构建输出必须使用新目录，失败后保留，不自动覆盖重装。所有输出路径要求为不含空白和Shell特殊字符的绝对路径。

### 1. 构建依赖

若已有可信的同平台 OTP29、完整 PG18 prefix 及 Garage2.4.1，可复用。PG 在编译时的 `--prefix` **必须是 `/opt/imboy/runtime/postgresql`**，与安装器的目标运行路径一致；组装器会硬核验。仅把发行版分散的 `/usr/bin`、`/usr/share` 文件复制到一起不符合此合同。

缺少依赖产物时，先查看构建依赖清单，再运行源码构建配方：

```bash
bash tools/build-native-dependencies.sh --help

bash tools/build-native-dependencies.sh \
  --repo /srv/source/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --work /srv/build/native-deps-20261105 \
  --otp-prefix /srv/build/otp-29.1.2 \
  --pg-prefix /opt/imboy/runtime/postgresql \
  --jobs 4
```

调用者须有创建输出目录的权限。只在没有既有 IMBoy 安装的专用构建机使用上述 `/opt/imboy` 路径。脚本不会提权或安装 apt 包；`--help` 列出所需包，由发布方通过可信发行版仓库准备。请求保留本机代理和 TLS 验证设置。

配方依据当前源码的 `scripts/ci/native-eunit-pins.json`，固定 OTP29.1.2、PG18.6、PostGIS3.6.4、TimescaleDB2.23.1、pgvector0.8.7、pg_jieba commit/子模块、词典及 Garage2.4.1 哈希。下载校验失败即停止。只取后端必要依赖，不构建 Agent、Textguard 或 RTC 服务。原生 CI 调用的 Python 获取器没有进入本包。

PG必需扩展为：`pgcrypto`、`pg_jieba`、`timescaledb`、`vector`、`postgis`、`pg_trgm`、`btree_gin`、`btree_gist`、`citext`、`unaccent`、`intarray`、`pg_stat_statements`，并需默认分词词典和源码中的 `docker/pg_jieba_userdict.txt.big`。配方只构建文件，不启动数据库；扩展SQL和业务迁移由客户安装流程执行、验收。

### 2. 构建后端

```bash
export PATH=/srv/build/otp-29.1.2/bin:$PATH

bash tools/build-backend.sh \
  --repo /srv/source/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --out /srv/build/backend-20261105 \
  --jobs 4
```

输出为 `/srv/build/backend-20261105/backend`。工具在 `work/imboy` 内构建，目录名与 relx 的应用发现行为保持一致。它核对 `VERSION` 与 `relx.config`，不依赖有歧义的 `RELX_REL_VSN` 参数；要求 `dev_mode=false`、`include_erts=true`。

产物包括随包ERTS、业务及依赖BEAM、完整迁移、CLI与构建收据。工具拒绝运行数据、日志、私钥和外部符号链接；要求 sys.config/vm.args 仍为无秘密模板；检查构建机动态库并运行 `erlexec -version`。这些步骤不代表应用启动或业务验收通过。

固定主仓SHA不能冻结 `include/deps.mk` 中仍引用 `master` 的第三方依赖。工具记录实际依赖commit并用最终文件哈希冻结本次候选；重新构建可能产生不同字节，需视为新候选重新验收，不声称可逐字节复现。

### 3. 组装客户候选

```bash
bash tools/prepare-payload.sh \
  --repo /srv/source/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --release-dir /srv/build/backend-20261105/backend \
  --pg-prefix /opt/imboy/runtime/postgresql \
  --garage-bin /srv/build/native-deps-20261105/garage \
  --release-id alpha.83-cb95fa72-native \
  --out /srv/deliveries/imboy-native-candidate-20261105
```

组装器核对：后端收据与完整哈希、SHA/版本/平台、配置/CLI/产品manifest/迁移集合与源码字节、PG版本/编译前缀/扩展文件/词典、Garage固定哈希。原始 `sys.config` 哈希还必须等于当前工具包 `lib/render.sh` 声明的 `IMBOY_NATIVE_CONFIG_SHA256`；不一致须先适配并验证渲染器，不能仅改元数据放行。通过后生成新的候选目录、`metadata/release.env`、`metadata/host-runtime-packages.tsv`、`CHECKSUMS.sha256`，并设置 `PAYLOAD_READY=yes`。新候选中的无秘密文件统一为目录755、普通文件644、可执行文件755，使服务账户可以读取。

这个状态只表示产物已组装。`metadata/validation-status.tsv` 将干净机安装、业务UAT、备份恢复、升级回退列为 `NOT_RUN`，不得直接改成PASS。现场 `site.env` 不进入哈希；真实密钥在目标机初始化时生成，不提前打包。

### 4. 主机运行依赖与客户入口

ERTS、PG扩展和Garage仍可能依赖发行版动态库。封包时逐一检查实际ELF的 `ldd` 输出，排除已在payload内供应的库，其余用 `dpkg-query` 映射系统包，并用 `apt-cache depends --installed` 收集已安装的 Depends/PreDepends 闭包，记录准确包名、版本、架构。未知、无法映射或缺失的动态库会阻止封包；不会猜测库名对应的软件包。

发布机还须已安装以下运行工具：`bash curl jq openssl ca-certificates tar coreutils util-linux passwd iproute2 file nginx imagemagick fonts-dejavu-core`。这些工具及其依赖同样写入版本锁。锁文件为三列TSV，表头为 `package`、`version`、`architecture`。它随候选整体校验；不能手工删包或改版本绕过失败。

```bash
# 目标机只读检查：无网络、无APT索引刷新。
bash tools/prepare-host.sh --check

# 从现场已经配置、批准的APT源准备精确依赖。
sudo bash tools/prepare-host.sh --apply

# 客户首次安装也可合并使用主安装入口。
sudo bash install.sh --apply --prepare-host
```

`--check` 是默认模式；版本缺失/较旧时返回3，其他阻断返回非零。`--apply` 仅在有待安装包时执行 `apt-get update`，核对待安装精确版本可用，再模拟并检查安装计划，最后安装锁内包。已全部符合时不刷新索引。工具不修改源、不整机升级、不删除包；目标已有更高版本会在刷新索引前拒绝，绝不自动降级。依赖求解如要求锁外包也拒绝，须由发布方重做闭包和候选。它不启动IMBoy、PostgreSQL或Garage；正常OS软件包维护动作可能启动或配置Nginx。已有配置使用dpkg的保留策略。

即使现场预装了依赖，主安装器、升级和恢复仍会执行精确锁检查。新主机尚未安装到 `/opt/imboy/runtime/postgresql` 时，最终动态库检查临时使用候选内 `payload/postgresql/lib`，以核对随包PG库。

这不是全离线依赖包，需要可达的已批准APT源提供锁定版本。源中版本已撤下或现场离线时，由发布方另外供应同平台、同版本的批准镜像/离线依赖集合并重新验收；工具不会自动改源、换版本或下载未锁定库。不要把构建机glibc覆盖到目标系统目录。最终运行路径、扩展加载、首装和业务闭环必须在干净机验证。

### 验证范围

权威范围说明见 [docs/10_VALIDATION.md](docs/10_VALIDATION.md)。在本交付目录运行三套合成测试：

```bash
bash tests/run.sh
bash tools/test-payload-tools.sh
bash tools/test-host-tools.sh
```

2026-11-05 初始材料记录合计 62 项通过（29 + 21 + 12）。本次及后续执行结果以对应 PR/CI 为准；专用工作流 `.github/workflows/native-shell-delivery.yml` 在 Ubuntu 24.04 上自动执行三套测试，不构建 OTP/PG，不部署服务。运行输出保留在 CI 或仓库外的执行记录中，不提交运行日志。

这些测试使用随机临时目录、合成配置、假的 Git 仓库及 PG/Garage/APT 执行桩。其中封包测试会使用本机只读 `dpkg-query` / `apt-cache` 查询，并为缺少的 Nginx 包提供测试桩。它们核对合同与拒绝行为，不证明真实 APT 安装、原生编译、SQL 迁移或客户部署成功。真实候选仍须完成构建、空机首装、业务、恢复和升级验收。

### English status summary

The Chinese guide is authoritative. The publisher tools remain frozen at `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`, `1.0.0-alpha.83`, migration head `243`. The main snapshot `8957f0ed6cf7b3251259dbdf777fc1ae43837574` (alpha.84, migration `275`) has a different configuration digest and is not adapted by this PR. Keep the renderer digest gate; changing metadata cannot establish compatibility.

This is Bash-only native packaging tooling. `PAYLOAD_READY=yes` is written only after assembling actual binaries and does not certify deployment or business readiness. The historical 62 checks are synthetic; fresh results are reported by the PR and the dedicated native-shell CI workflow. No OTP/PostgreSQL build, service deployment, real migration, business UAT, restore or upgrade acceptance is implied. Follow the full validation scope in [10_VALIDATION.md](docs/10_VALIDATION.md).

### 源码证据

- [relx.config](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/relx.config)：ERTS、版本、非开发发布。
- [Makefile](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/Makefile)：配置复制、构建依赖、已有特性生成结果。
- [Dockerfile](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/Dockerfile)：relx目录命名、erlang_pay归档pin、CLI与动态库依据；本工具没有调用容器。
- [原生输入pins](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-pins.json)、[原生bootstrap](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-bootstrap.sh)、[原生迁移](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-run.sh)：配方及12项扩展依据，不能当成本包的实测结果。
