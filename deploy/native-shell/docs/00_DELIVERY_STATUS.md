# 交付状态、使用边界与责任

版本：原生 Shell 交付包 v1。编写日期：2026-11-05（北京时间）。

核对的后端源码：`imboy-pub/imboy@cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`，源码版本 `1.0.0-alpha.83`。以下结论只对应这份固定源码与本包脚本；更换源码或运行时必须重新组装、验证。

本次仓库集成核对日期为 2026-11-08。**版本范围仍是 alpha.83／迁移头 `243`，不是对 main 的自动适配。** main 快照 `8957f0ed6cf7b3251259dbdf777fc1ae43837574` 的 alpha.84／迁移头 `275` 以及新的 `sys.config` 摘要均超出本目录冻结合同。必须先进行发布方适配、重新组装和真实演练，才能声明支持该版本或升级路径；摘要对照和验证范围见 [10_VALIDATION.md](10_VALIDATION.md)。

## 1. 这次交付了什么

这套材料提供纯 Bash 的原生部署流程：准备配置、验证发行物、按需安装已冻结的主机依赖、生成现场密钥、初始化 PostgreSQL 与 Garage、生成 systemd/Nginx 配置、启动后端、执行基础检查，以及备份、升级和恢复。脚本不依赖 Python，也不调用 Docker。

**本工具包于 2026-11-05 初次生成的材料版尚未包含已编译的 IMBoy、PostgreSQL 或 Garage 二进制。** 组装器会把本手册原样带入后续候选，因此拿到的实际目录应以 `metadata/release.env` 中的 `PAYLOAD_READY` 判定状态：`no` 是材料版，须由发布方按 `07_RELEASE_ASSEMBLY.md` 供应并组装真实 `payload/`；`yes` 是已组装候选，仍须查看对应实装与业务 UAT 证据。不能只凭本文日期或 `yes` 宣称生产就绪。

无论哪一种目录，通用发行物都不应带入客户证书私钥、口令或生产数据。安装器会把缺失的运行产物作为阻塞，不会把空模板运行成“安装成功”。

“源码已核对”“Shell 检查通过”“模拟流程通过”“干净机实装通过”“业务验收通过”是不同证据。2026-11-05 材料制作时尚未取得目标服务器的干净机安装、真实登录/消息/附件、完整恢复或升级演练结果，因此通用验收表初始为 `NOT_RUN`。后续发布方应填写实际候选的结果，不沿用初始状态冒充最新结论。

## 2. 支持目标与范围

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

## 3. 可直接交付、发布方待办与现场待办

| 分类 | 内容 | 当前处理 |
| --- | --- | --- |
| 可直接交付 | Shell 安装/运维入口、配置模板、网络要求、安装/升级/恢复手册、验收表、排错指南 | 已作为本包内容提供；验证范围见 [10_VALIDATION.md](10_VALIDATION.md)，新执行结果以对应 PR/CI 为准 |
| 可直接交付 | 固定源码版本与部署合同说明、文件完整性清单 | 供发布与验收核对；哈希不能替代发行方身份认证 |
| 发布方必须完成 | 同一固定 SHA 的 Linux 自包含 IMBoy release、PG18 必需扩展、Garage 二进制、目标机运行库 | 通过 `tools/prepare-payload.sh` 组装；不能让客户现场找代码或临时补编译 |
| 发布方必须完成 | 二进制来源、版本、平台、SHA-256、依赖许可清单；干净机首装、恢复、升级验证 | 属于发行工程工作，填写客户域名无法解决 |
| 现场必须补齐 | 主机、域名、DNS、TLS、磁盘、端口、维护窗口、备份目的地、操作人与验收人 | 在 `templates/site-handover.tsv` 填写，不填秘密 |
| 现场按范围补齐 | 管理员身份、客户端候选包、SMTP/短信/推送/IdP 测试身份及回调配置 | 秘密通过现场独立渠道提供，不写入通用交付包 |
| 安装时自动生成 | 数据库口令、应用签名材料、Erlang cookie、Garage 认证材料、登录 RSA 等 | 只落在目标机受限文件中；续跑必须复用，不能重新生成替换 |

## 4. 角色与阅读顺序

| 角色 | 首先阅读 | 完成标准 |
| --- | --- | --- |
| 发布负责人 | 本文、`07_RELEASE_ASSEMBLY.md` | 交付有真实 payload、版本清单与验证证据的候选包 |
| 环境负责人 | `02_NETWORK_TLS.md`、`templates/site-handover.tsv` | 目标机、三域名、证书、网络、磁盘和备份渠道准备完成 |
| 陌生执行者 | `01_INSTALL.md` | 只按文档完成安装、基础验证并记录每一次阻塞 |
| 运维负责人 | `03_CONFIGURATION.md`、`04_OPERATIONS.md`、`06_TROUBLESHOOTING.md` | 能独立定位问题、备份、恢复和按允许路径升级 |
| 配置审阅人 | `08_DEPLOYMENT_EXAMPLES.md` | 对照无秘密的 Nginx、systemd、Garage、PG 示例理解生成结果 |
| 验收负责人 | `05_ACCEPTANCE.md`、`templates/acceptance.tsv` | 每项必验项目都有真实结果和可追溯证据 |

## 5. 不把未知内容交给客户猜

如发行包缺二进制、系统依赖、扩展、可信来源或迁移说明，执行者登记 `BLOCKED` 并交回发布负责人。不要在客户服务器上临时克隆默认分支、换成通用 PostgreSQL、去掉启动检查或删除数据重装。

源码证据：[release 自包含设置](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/relx.config)、[运行时环境覆盖](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env.erl)、[原生 CI 依赖准备](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-bootstrap.sh)。CI 配方是构建参考，不是本交付包实装认证。
