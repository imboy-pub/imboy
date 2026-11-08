# 发布方组装、版本冻结与证据

本文供 IMBoy 发布负责人使用。客户执行者只应收到组装后的候选目录，不需要源码、Git、编译器、Erlang 开发环境或构建网站账号。

## 1. 当前交付材料和可安装候选的区别

2026-11-05 初次生成的工具包材料版尚无原生二进制。本文会原样进入后续组装候选，**实际目录以 `metadata/release.env` 的 `PAYLOAD_READY` 为准**：`no` 表示仍是材料版；`yes` 表示已组装，但不表示 UAT 通过。发布方取得真实构建产物后运行 `prepare-payload.sh`，由工具核对平台、SHA、版本、文件集合、扩展与哈希，生成新目录。不能手改 `PAYLOAD_READY` 绕过组装。

2026-11-05 初始材料整理时未发现可直接作为本包 payload 的公开原生 release asset。仓库原有容器发行工作流不能自动证明原生 tar 发行物存在。[发行工作流](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/.github/workflows/release.yml)。

源码固定为 `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`，VERSION 与 relx release 均为 `1.0.0-alpha.83`。`RELX_REL_VSN` 不能被当作已验证的版本覆盖机制；核对生成文件中的实际版本。[relx 配置](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/relx.config)。

本目录同时冻结迁移头 `243` 和原始配置摘要。2026-11-08 仓库集成核对的 main 快照 `8957f0ed6cf7b3251259dbdf777fc1ae43837574` 为 alpha.84／迁移 `275`，其配置摘要已变化；**本次没有完成该快照的渲染器适配、发行物构建或升级认证**。不要把下面命令的 `--sha` 改成 main 后直接交付，也不要修改摘要常量以绕过拒绝。版本与摘要的完整对照、三套合成测试和 PR 检查范围见 [10_VALIDATION.md](10_VALIDATION.md)。

## 2. 构建主机与输入

在独立的 Linux 构建机操作，系统发行版、发行版版本与架构须同目标机。工具接受 Ubuntu 24.04 x86_64 或 Debian 13 x86_64，但每个平台各自需要干净机认证，不能跨平台复用二进制。用户的 macOS 开发机可存源码和材料，不能把其本地产物直接当作 Linux release。

准备只读可访问的完整后端 Git 仓库，包含固定 SHA。构建脚本使用 `git archive`，不带入本地未提交配置，也不修改源仓。新输出目录必须不存在，避免覆盖已有构建或运行状态；失败时保留目录与日志，由发布方检查。重新构建选择新的目录。

发布工具依赖 Bash、GNU 工具、Git、make、C/C++ 编译工具、jq 等。`jq` 是二进制 JSON 工具，不需要 Python。构建网络、代理、可信 CA、下载来源与系统依赖由发布方准备。客户主机运行依赖也要冻结精确软件包版本，由环境方预装或 `tools/prepare-host.sh` 从批准的发行版源安装，不让客户猜动态库清单。

## 3. 纯 Shell 构建参考流程

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

## 4. PostgreSQL 不能只用通用空发行包

本安装合同检查这些扩展：`pgcrypto`、`pg_jieba`、`timescaledb`、`vector`、`postgis`、`pg_trgm`、`btree_gin`、`btree_gist`、`citext`、`unaccent`、`intarray`、`pg_stat_statements`。必须同时提供扩展控制文件、SQL、共享库与词典；仅存在 `.control` 文件不足以证明 SQL 能执行。

PG prefix 按 `/opt/imboy/runtime/postgresql` 生成完整 `bin/lib/share`，不要直接拼接来自系统不同目录/不同版本的文件。本包的升级入口要求 PG/Garage payload 字节保持一致；它们的升级需另做恢复和兼容演练。

## 5. 组装客户候选

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

## 6. 版本冻结清单

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

## 7. 初始材料版待办与候选发布核对

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

## 8. 与此前材料的明确变化

本包使用原生 systemd 与 Shell 准备/安装入口；不复用此前 Python 组装工具，也不走 Docker Compose。核心服务的三域名与此前包含管理后台、Widget、RTC/TURN 的整栈五域拓扑不同；增加其他产品组件时需补对应材料。

S3 公开签名入口统一为独立 `https://s3.example.com`。真实 WebSocket 路径使用 `/api/v1/ws`，监听变量使用 `HTTP_PORT`。这些修正来自本次固定源码核对，不能将旧示例直接搬回新配置。
