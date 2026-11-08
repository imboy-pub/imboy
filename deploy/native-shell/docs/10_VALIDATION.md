# 验证范围、冻结版本与可重复检查

本章是 `deploy/native-shell` 的权威验证范围说明，更新于 2026-11-08。它定义各层验证能够支持的结论，不存放某次运行日志，也不替代候选发行物的实际验收记录。新执行结果以对应 PR 的检查状态、CI 输出和结果摘要为准。

## 1. 本目录冻结的源码合同

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

## 2. 历史合成验证与当前结果

2026-11-05 初始材料的历史记录为三套测试合计 **62 项通过**。这些数字描述原材料的那次执行，不是对今后每个提交自动继承的通过结论，也不是 62 项真实部署或业务验收。

| 测试入口 | 历史数量与结果 | 合成验证范围 |
| --- | --- | --- |
| `tests/run.sh` | 29 / 29 通过 | Bash 语法与帮助、缺 payload 拒绝、环境数据解析、独立秘密及续跑、配置摘要、文件集合、TLS 域名校验、RSA 本地往返、服务模板、合成备份与恢复拒绝 |
| `tools/test-payload-tools.sh` | 21 / 21 通过 | 合成 Git/构建收据、固定源码与配置合同、完整性、权限、扩展存在性、PG prefix、Garage hash、候选封包及失败条件 |
| `tools/test-host-tools.sh` | 12 / 12 通过 | 精确系统包锁、只读检查、版本不可用/更高版本/锁外依赖/删除计划拒绝、锁篡改与重复项、动态库归属和随包 PG 库路径 |

历史运行环境为 Ubuntu 24.04.3 x86_64，使用 Bash 5.2.21、OpenSSL 3.0.13、curl 8.5.0 与 jq 1.7。没有以该记录宣称 Debian 或 ARM 已认证。原材料中的执行日志不纳入仓库；当前 PR 的重跑及其后提交应由各自 PR/CI 报告结果，失败或未运行均不能沿用历史 PASS。

测试在随机临时目录生成合成配置、短期测试密钥、假发行物和服务状态桩，结束后清理。发行工具测试会调用本机只读 `dpkg-query` / `apt-cache`，并为缺少的 Nginx 包提供明确测试桩；主机准备测试的 APT 变更操作由桩代替。测试不会访问生产数据、执行真实 APT 更新/安装、启动 PostgreSQL/Garage/Nginx/IMBoy，或连接客户环境。

## 3. 在开发机重复运行

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

## 4. 独立 PR 工作流

仓库根目录的 `.github/workflows/native-shell-delivery.yml` 在 Ubuntu 24.04 runner 上运行这三套合成测试。面向 `main` 或 `dev` 的 PR、以及对这两个分支的推送，在修改 `deploy/native-shell/**` 或该工作流文件时触发；工作流也支持手动触发。它独立于既有质量工作流。

CI 先为隔离 runner 安装测试所需的系统工具，再核对本目录的 `CHECKSUMS.sha256` 完整性，最后运行三套测试；这与测试内使用桩核对现场 APT 安装计划是不同层次。工作流自动覆盖本章列出的 29 + 21 + 12 项合同检查，不执行 OTP/PG 编译，不生成可交付二进制，也不部署、迁移或初始化实际服务。具体提交是否通过，以对应检查运行的状态和输出为准；本章中的历史数量不取代该结果。

## 5. 仍需真实候选补齐的发布门槛

| 门槛 | 所需真实对象与结果 | 当前文档能够作出的结论 |
| --- | --- | --- |
| 原生构建与组装 | 固定 SHA 后端 release、PG18 及扩展、Garage、目标运行库和许可清单 | 存在构建/组装流程；尚不能声称真实产物已经验收 |
| 空机首装与续跑 | 对应平台的干净主机、真实 payload、现场 DNS/TLS，独立执行者完成全流程 | 合成检查不证明实装成功 |
| 数据库迁移与权限 | 真实 SQL 迁移到候选目标、失败行为、临时权限撤销 | 迁移门禁与运行约定已文档化；真实执行仍需验证 |
| 客户端协议与业务 UAT | 配对客户端，管理员/用户登录、WebSocket 消息、权限及附件闭环 | `/healthz` 与本地 RSA 往返不能代替客户端闭环 |
| S3 和公网入口 | 实际客户端网络、DNS/TLS、预签名上传与确认、私有/公开读取 | `verify.sh --s3-write` 只补充本机经 HTTPS Nginx 的真实 S3 签名读写；仍不等于完整业务 UAT |
| 停机备份与恢复 | 真实数据库和对象数据、隔离恢复主机、恢复后的数据断言与 RTO | 合成归档和服务桩不证明业务可恢复 |
| 升级与故障处理 | 确定的旧/新候选、真实迁移和故障恢复路径 | 本 PR 不认证 alpha.83 → alpha.84，也不认证反向迁移 |
| 陌生人交付演练 | 未接触代码库的执行者，仅凭交付材料完成操作、计时并记录开发者介入 | 按 [05_ACCEPTANCE.md](05_ACCEPTANCE.md) 和验收模板实际填写 |

上述项目在本次仓库集成中没有新增实装通过结论。`PAYLOAD_READY=no` 表示材料版；发布方成功组装后为 `yes`，仍须逐项填写 `PASS`、`FAIL`、`BLOCKED`、`NOT_RUN` 或适用的 `NOT_APPLICABLE`，并在验收系统或仓库外保存对应结果。完整客户步骤继续使用 [01_INSTALL.md](01_INSTALL.md)、[04_OPERATIONS.md](04_OPERATIONS.md) 和 [05_ACCEPTANCE.md](05_ACCEPTANCE.md)。
