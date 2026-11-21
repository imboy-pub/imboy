# alpha.84 + PostgreSQL 18.7 原生发行适配门禁

状态：**规划与验收合同；不是已适配的安装器，也不是可交付候选。**

基线核对日期：2026-11-21  
后端基线提交：`37f2ef3af32a01f5156ce9baf3ba72b579dfe20e`（包含原生 PG18.7 pin 修复 PR #75）  
后端版本：`1.0.0-alpha.84`  
目标迁移头：`275`  
PostgreSQL：`18.7`  
原生 CI pin：`scripts/ci/native-eunit-pins.json`

## 1. 当前发行身份

仓库内现有材料包 `metadata/release.env` 仍是历史材料版：

| 字段 | 现有材料包 | 新版目标 |
|---|---|---|
| RELEASE_ID | `alpha.83-cb95fa72-native` | 新候选 ID，构建时生成 |
| SOURCE_SHA | `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af` | `37f2ef3af32a01f5156ce9baf3ba72b579dfe20e` |
| BACKEND_VERSION | `1.0.0-alpha.83` | `1.0.0-alpha.84` |
| PG_VERSION | `18.6` | `18.7` |
| MIGRATION_HEAD | `243` | `275` |
| PAYLOAD_READY | `no` | 仅由真实组装器生成 |

**不得把现有 `metadata/release.env` 手工改成新版。** 它描述旧材料包，不是新发行配置模板。新版应由发布方从固定源码 SHA 构建并输出到全新目录，旧包及其哈希保持不变。

PG18.7 官方源码 pin 位于 `scripts/ci/native-eunit-pins.json`：

- 文件：`postgresql-18.7.tar.bz2`
- URL：`https://ftp.postgresql.org/pub/source/v18.7/postgresql-18.7.tar.bz2`
- SHA-256：`441dfae4280cae3e0425e5f876f80559fb0731c140f1aae22b3e5a54d0411bfd`

## 2. 发行阻断项

现有原生安装器仍绑定 alpha.83 的配置模板和摘要。当前 `lib/render.sh` 中的 `IMBOY_NATIVE_CONFIG_SHA256` 是旧配置摘要，`imboy_native_base_config` 也是旧版完整配置模板。

新版配置摘要必须由固定候选源码现场计算：

```bash
git show 37f2ef3af32a01f5156ce9baf3ba72b579dfe20e:config/sys.config.example \
  | sha256sum
```

不要将 Git blob SHA 当作文件 SHA-256，也不要根据旧摘要推算新摘要。把实际结果写入适配补丁，并由测试验证它与候选 release 的原始 `sys.config` 完全一致。

在配置渲染器完成适配前，`prepare-payload.sh` 的摘要拒绝行为必须继续保持。不得通过放宽摘要比较、跳过配置锚点、修改收据或直接改 `PAYLOAD_READY` 来绕过。

## 3. 实施顺序与验收门

### G0 — 冻结新版基线

- [ ] 固定源码 SHA 为本文件记录的不可变 commit；若 main 前进，必须重新审查并显式更新候选 SHA。
- [ ] 在该 SHA 上读取并核验 `VERSION`、迁移目录最大版本、产品 manifest、`sys.config.example`、`vm.args`、原生 pins。
- [ ] 记录配置文件 SHA-256（文件内容摘要，不是 Git blob SHA）。
- [ ] 核对 PG18.7 官方源码 SHA、OTP、扩展、Garage、词典和子模块 pin。
- [ ] 不读取或修改客户现场秘密。

通过条件：所有版本身份来自同一个不可变源码 SHA，且迁移头与预期一致。

### G1 — 适配配置渲染器

- [ ] 将新版 `config/sys.config.example` 作为新合同真源；更新 renderer 的源 SHA 注释和配置摘要。
- [ ] 逐项复核旧 renderer 的 awk 锚点：新增、删除、改名、重复或语义变化均须显式处理。
- [ ] 核对 CORS、API/WS 域名、Garage endpoint/bucket、附件 registry、数据库凭据、日志、Mnesia、时区、RTC、支付和安全开关。
- [ ] 新配置中不属于本原生交付范围的服务/密钥继续保持安全关闭或空值；不得凭旧模板静默丢弃新配置项。
- [ ] 为每个必需锚点增加“恰好一次”检查；未知关键配置变化必须失败关闭。
- [ ] 保证 renderer 只写目标输出目录，生成文件权限仍为 0600。

通过条件：用固定候选 release 的真实原始配置运行渲染测试；渲染结果经 Erlang term 解析/配置合同检查，关键配置逐项断言通过。仅比较摘要或 Bash 语法不算通过。

### G2 — 适配发行元数据与封包合同

- [ ] 保持旧 `metadata/release.env` 不变。
- [ ] `prepare-payload.sh` 从指定 SHA 读取版本、迁移头和 PG pin；拒绝调用方提供的互相矛盾身份。
- [ ] 继续核对 build receipt、`pg_config --version`、编译 prefix、12 项扩展、扩展 SQL/动态库、jieba 词典、Garage hash。
- [ ] 新候选使用全新输出目录，生成 `release.env`、依赖锁、收据摘要和 `CHECKSUMS.sha256`。
- [ ] `validation-status.tsv` 的实装验收项初始必须为 `NOT_RUN`。
- [ ] 增加 alpha.83 旧合同与 alpha.84 新合同的正反向回归，证明旧材料包没有被意外重写。

通过条件：合成测试证明版本读取、身份一致性、拒绝路径和旧包不变；真实封包产物的文件清单及哈希可复核。

### G3 — 原生依赖与后端构建

在隔离、同目标发行版/架构的构建机执行。不得在生产数据库主机上构建。

```bash
bash tools/build-native-dependencies.sh --help
bash tools/build-backend.sh --help
bash tools/prepare-payload.sh --help
```

- [ ] 使用固定 SHA 构建 OTP 29.1.2、PostgreSQL 18.7 和全部必需扩展。
- [ ] 校验官方归档 SHA、Git commit、子模块、词典、Garage 二进制哈希。
- [ ] 确认 PG 的编译 prefix 是 `/opt/imboy/runtime/postgresql`，不得事后重定位分散的系统包文件。
- [ ] 构建固定 SHA 对应的 IMBoy release，核验 ERTS、CLI、配置、产品 manifest 和完整迁移集合。
- [ ] 记录第三方依赖实际 commit；动态依赖必须纳入运行时包锁。
- [ ] 不将“依赖构建成功”描述为数据库启动、SQL 迁移或业务验收通过。

通过条件：构建收据、二进制版本、源 pin、扩展和依赖锁相互一致。

### G4 — 真实部署与业务验收

在独立干净测试主机进行；Ubuntu 24.04 x86_64 与 Debian 13 x86_64 是不同候选，必须分别构建、分别验收。

- [ ] 首装、安装中断续跑、服务账户与目录权限。
- [ ] PostgreSQL 18.7 启动、12 项扩展加载、词典和数据库权限。
- [ ] 从零执行完整迁移至 275；核对迁移记录、索引、约束和关键表。
- [ ] 管理员初始化、客户端登录、WebSocket、单聊/群聊、历史消息。
- [ ] Garage S3 签名上传、确认、私有读取和公开附件读取。
- [ ] 观察服务重启、日志、健康检查和主机重启恢复。
- [ ] 由实际执行者填写 `templates/acceptance.tsv`，保存脱敏证据。

通过条件：所有必需业务场景有可复现证据；合成 fixture 不得代替真实数据库和客户端闭环。

### G5 — 备份、恢复、升级和回滚

- [ ] 执行停机一致性备份，核验归档 SHA。
- [ ] 在隔离新主机恢复数据库、对象数据、配置和候选身份。
- [ ] 对恢复后的账号、消息、附件、对象元数据做数据断言。
- [ ] 用真实旧版本副本执行升级演练；确认迁移前备份、写入冻结和失败后的写入边界。
- [ ] 在恢复副本上演练失败回滚；不得对生产数据做破坏性试验。
- [ ] 记录 RTO、RPO、操作步骤和所有人工介入。

通过条件：恢复后业务数据可验证；升级失败不会出现新旧服务同时写入或无安全回滚点。

## 4. 状态和发布纪律

| 状态 | 含义 | 允许的声明 |
|---|---|---|
| `NOT_RUN` | 尚未执行 | 不得声明通过 |
| `BLOCKED` | 有明确前置条件阻塞 | 不得交付 |
| `FAIL` | 已执行但失败 | 不得交付 |
| `PASS` | 有对应候选、环境、命令和证据 | 仅声明该项通过 |

版本状态必须分开：

1. **源码已合并**：代码进入 main。
2. **构建候选已组装**：真实二进制及收据存在，`PAYLOAD_READY=yes`。
3. **部署验收通过**：G4 完成。
4. **正式可交付**：G5 完成并经过发布负责人签署。

PR 合并、CI 全绿、PG18.7 官方容器镜像发布、`PAYLOAD_READY=yes`，均不能单独替代正式发行验收。

## 5. 生产环境边界

本适配 PR 不授权、不执行生产 PG 升级、生产服务重启、数据迁移、切流或清理旧数据目录。生产 18.7 升级继续遵守既定在途计划；原生候选的构建与隔离测试可以并行，但不得把测试环境结果当成生产升级许可。

## 6. 当前结论

本文件定义新版发行的基线和验收合同。它本身**不表示 alpha.84 原生安装器已适配、不表示 PG18.7 已在原生包中构建，也不表示任何真实安装、迁移、恢复或业务测试已经通过**。

实际适配必须继续修改并测试 `lib/render.sh`、`tools/prepare-payload.sh`、对应合成测试和版本文档；在这些实现及真实候选验收完成之前，旧材料包继续保持旧身份，新版不得发布。
