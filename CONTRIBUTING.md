# 贡献指南 / Contributing to IMBoy

感谢你对 IMBoy 的兴趣！本文档定义参与本项目的标准流程。

> IMBoy 是一个 workspace，包含三个独立可发布的子项目。贡献时请先确定你的改动属于哪一个：
>
> - **后端** `imboy/` — Erlang/OTP + Cowboy + PostgreSQL
> - **客户端** `imboyapp/` — Flutter（Android / iOS）
> - **管理后台** `imboy-admin-frontend/` — React + Vite + Bun

---

## 贡献者协议 / Contributor Agreement

向本项目提交代码（Pull Request、Patch 或任何形式的代码贡献），即表示你已阅读并同意以下协议：
By submitting code to this project (Pull Request, patch, or any form of contribution), you confirm that you have read and agree to the following:

### DCO（开发者起源证书）/ Developer Certificate of Origin

本项目采用 [DCO（Developer Certificate of Origin）](https://developercertificate.org/) 作为贡献者协议，不使用 CLA（Contributor License Agreement）。
This project uses the [DCO (Developer Certificate of Origin)](https://developercertificate.org/) as its contributor agreement. No CLA (Contributor License Agreement) is required.

**每个 commit 必须附带 DCO sign-off，否则 PR 将被 CI 自动拒绝。**
**Every commit must include a DCO sign-off, otherwise the PR will be automatically rejected by CI.**

```bash
# 提交时自动添加 Signed-off-by / Add sign-off automatically when committing
git commit -s -m "feat(backend): your message"

# 忘记签名时补签最后一次提交 / Sign the last commit retroactively
git commit -s --amend --no-edit

# 批量补签 PR 中所有 commit（N 为 commit 数量）/ Bulk sign all commits in the PR
git rebase HEAD~N --signoff
```

sign-off 会在 commit message 末尾自动追加一行：
The sign-off appends the following line to the commit message:

```
Signed-off-by: Your Name <your@email.example>
```

这表示你确认：你有权提交此代码，并同意以 [MulanPSL-2.0](./LICENSE) 授权给本项目。
This confirms: you have the right to submit this code, and you agree to license it to this project under [MulanPSL-2.0](./LICENSE).

> **CI 强制执行**：`.github/workflows/backend-ci.yml` 中的 `dco-check` job 会检查 PR 中的每一个 commit。
> **CI enforcement**: The `dco-check` job in `.github/workflows/backend-ci.yml` checks every commit in the PR.

---

## 行为准则

请先阅读 [CODE_OF_CONDUCT.md](./CODE_OF_CONDUCT.md)。参与本项目即表示同意遵守其中条款。

---

## 开始之前 / Before You Start

### 我应该在哪讨论？

| 情况 | 去哪里 |
|------|--------|
| 发现了 bug | GitHub Issue，选 **Bug Report** 模板 |
| 想做一个新功能 | **先开 Issue 讨论**，选 **Feature Request** 模板 |
| 使用问题 / 部署失败 | GitHub Issue，选 **Question** 模板 |
| 安全漏洞 | **不要开公开 Issue**。见 [SECURITY.md](./SECURITY.md) |
| 文档改进 | 直接 PR 即可 |

### 不接受的贡献

- 纯粹的代码风格调整（缩进、换行、引号）—— 除非配合有实质性改动
- 一次性把多个无关改动塞到一个 PR
- 引入未经讨论的大型依赖
- 破坏现有 API 兼容性而没有 ADR 讨论

---

## 开发环境

### 前置依赖

本地依赖检查由 `bash scripts/dev_setup.sh` 首步承担（docker / erlang / make）。

- Erlang/OTP **28+**
- PostgreSQL **18+**（含扩展 `pg_jieba`、`postgis`、`timescaledb`、`pgcrypto`、`pg_trgm`）
- Flutter **3.24+**（客户端）
- Bun **1.0+**（管理后台）

### 快速开始

```bash
# 后端
cd imboy && make compile && make eunit

# 客户端
cd imboyapp && flutter pub get && flutter test

# 管理后台
cd imboy-admin-frontend && bun install && bun run test
```

详细开发说明见各子项目 `README.md` 与 `CLAUDE.md`。

---

## 分支模型 / Branching

采用简化的 **GitHub Flow**：

```
main                    <- 受保护，只接受 PR 合入
  ↑
  feat/your-feature     <- 功能开发
  fix/bug-description   <- bug 修复
  docs/xxx              <- 文档
  refactor/xxx          <- 重构
  chore/xxx             <- 杂项（依赖升级等）
```

**发版分支**：`release/1.0.0` 在发版冻结期间创建，只接受 backport。

---

## 远程仓与推送规范（双 remote 双推）

三仓均配置多个远程仓。**gitee（origin）为主托管，github 为 GitHub Actions 门禁所在**——两处都必须推送，缺一不可（原因见下）。本节适用于有直推权限的 maintainer / 内部开发者；Fork 贡献者走 GitHub PR 即可（PR 事件同样触发门禁）。

### 现状（2026-08-22 实查 `git remote -v`）

| 仓 | `origin`（主托管） | `github`（Actions 门禁） | 其他 remote |
|----|--------------------|--------------------------|-------------|
| imboy | `git@gitee.com:imboy-pub/imboy.git` | `git@github.com:imboy-pub/imboy.git` | `gitee` → `git@gitee.com:leeyi/imboy.git`（个人 remote）；`gitcode` → `git@gitcode.com:imboy/imboy.git` |
| imboyapp | `https://gitee.com/imboy-pub/imboy-flutter.git` | `git@github.com:imboy-pub/imboy-flutter.git` | `gitcode` → `git@gitcode.com:imboy/imboy-flutter.git` |
| imboyadmin | `git@gitee.com:imboy-pub/imboy-admin-frontend.git` | `git@github.com:imboy-pub/imboy-admin-frontend.git` | `gitcode` → `git@gitcode.com:imboy/imboy-admin-frontend.git` |

> gitcode 与个人名下 remote 不在强制推送规范内，按需手动推。

### 为什么必须双推

**GitHub Actions 只在 github.com 的事件（push / pull_request / tag）上触发**，gitee 不运行 GitHub Actions。当前三仓在 github 侧的门禁与流水线：

| 仓 | github 上的 workflow |
|----|----------------------|
| imboy | `backend-ci.yml`、`contract-gate.yml`（API/枚举/EntityId 契约门）、`quality.yml`、`sonar.yml`、`docs.yml`、`codemap.yml`、`sbom-diff.yml`、`release.yml`（tag 触发发布链） |
| imboyapp | `contract.yml`（错误码契约门）、`ci.yml`、`core-automation.yml`、`integration_test.yml`、`quality.yml`、`sonar.yml` |
| imboyadmin | `admin-e2e.yml`、`ci.yml`、`quality.yml`、`sonar.yml` |

**只推 gitee 不推 github = 门禁空转**：契约门（`contract-gate.yml` / `contract.yml`）不会执行，非法漂移无人拦截——等于门禁不存在；tag 漏推 github 则 `release.yml` 发布链不触发。

### 标准化命令（供你本人执行；remote 配置属个人资产，本文档只给命令不代执行）

**方案 A（推荐）：origin 配双 pushurl，一条 `git push` 双达**

```bash
# 以 imboy 仓为例（其余两仓按下表 URL 替换）：
git remote set-url --add --push origin git@gitee.com:imboy-pub/imboy.git
git remote set-url --add --push origin git@github.com:imboy-pub/imboy.git

# 验证（应看到 origin 有两条 push 记录）：
git remote -v
```

> 注意：第一条 `set-url --add --push` 的语义是**替换**（origin 无显式 pushurl 时默认继承 fetch URL，首次设置即覆盖该继承），第二条才是**追加**。两条都执行后 `git push` 一次推两处。
>
> URL 对照：imboyapp 的 gitee 侧为 `https://gitee.com/imboy-pub/imboy-flutter.git`；imboyadmin 的 gitee 侧为 `git@gitee.com:imboy-pub/imboy-admin-frontend.git`；github 侧三仓均为 `git@github.com:imboy-pub/<仓名>.git`。

- 优点：漏推 github 从机制上不可能——这正是要根治的风险。
- 缺点：一处失败时 git 整体非零退出但另一处可能已成功，需看输出分辨；临时只想推一处时需显式 `git push git@github.com:... <branch>`。

**方案 B：保持显式双 remote（三仓 `github` remote 已配好，零配置改动）**

```bash
git push origin && git push github        # 分支
git push origin vX.Y.Z && git push github vX.Y.Z   # tag（release.yml 只认 github 侧 tag）
```

- 优点：零配置改动、失败定位清晰。
- 缺点：全靠人记，漏推 github 无任何告警——恰是本规范要消除的风险。

**推荐**：日常用方案 A 兜底防漏推；一次性补推 / 单侧推送用方案 B 语法。

---

## 提交规范 / Commit Message

采用 [Conventional Commits 1.0](https://www.conventionalcommits.org/zh-hans/v1.0.0/)：

```
<type>(<scope>): <subject>

<body>

<footer>
```

### type

| type | 含义 |
|------|------|
| `feat` | 新功能 |
| `fix` | bug 修复 |
| `docs` | 文档 |
| `refactor` | 重构（无功能变化） |
| `perf` | 性能优化 |
| `test` | 测试 |
| `chore` | 构建、依赖、杂项 |
| `ci` | CI/CD 配置 |
| `revert` | 回滚 |

### scope（可选）

用于标注影响的子项目或模块：

```
feat(backend/msg): 支持批量撤回
fix(app/chat): 修复会话滚动错位
docs(deploy): 补充 Sentry DSN 注入说明
chore(admin/deps): 升级 React 到 19.1
```

### 示例

```
feat(backend/e2ee): 支持社交恢复流程

在 e2ee_logic 中新增 social_recovery_init/2 与 social_recovery_complete/3，
允许用户通过预先指定的 3 位联系人恢复加密密钥。

Closes #123
Refs ADR 2026-04-01-social-recovery
```

### 提交签名（DCO）/ Commit Sign-off (DCO)

每个 commit 必须附带 `Signed-off-by` 行，详见顶部"[贡献者协议](#贡献者协议--contributor-agreement)"章节。
Every commit must include a `Signed-off-by` line. See the "[Contributor Agreement](#贡献者协议--contributor-agreement)" section at the top.

```bash
git commit -s -m "feat(backend): ..."
```

---

## Pull Request 流程

### 1. Fork → 本地开发

```bash
git clone https://github.com/YOUR_USERNAME/imboy.git
cd imboy
git checkout -b feat/your-feature
```

### 2. 开发前自检

- [ ] 我知道改动会影响的子项目
- [ ] 我已阅读对应子项目的 `CLAUDE.md`
- [ ] 大改动已在 Issue 中讨论并获得 maintainer 同意

### 3. 开发中

- 遵循现有代码风格（后端：`efmt`；前端：`prettier` + `eslint`；Flutter：`dart format`）
- 为新功能添加测试，目标覆盖率 **≥ 80%**
- 保持 commit 粒度清晰，不要一个 commit 塞十件事

### 4. 提交前检查

| 子项目 | 命令 |
|--------|------|
| 后端 | `make compile && make eunit && make ct && make dialyze && bash script/check_module_boundaries.sh` |
| 客户端 | `flutter analyze && flutter test` |
| 管理后台 | `bun run lint && bun run test && bun run build` |

### 5. 创建 PR

- 使用仓库 `.github/PULL_REQUEST_TEMPLATE.md` 模板
- 标题用 Conventional Commits 格式
- 描述中必须包含：
  - **Why**：为什么做这个改动
  - **What**：改了什么
  - **How to verify**：审阅者如何验证
  - **Breaking changes**（如有）
- 关联相关 Issue：`Closes #123`

### 6. 审阅 / Review

- 所有 PR 需要至少 **1 位 maintainer 同意**
- CI 必须全绿
- 合并方式：**Squash and merge**（保持主干历史清爽）

---

## 契约变更 SOP（跨仓契约，必读）

三仓之间共享的契约——错误码、API 端点、DB 枚举值域、EntityId/TSID 类型——实行**单一真源（Source of Truth）治理**，配套 CI 硬门禁。任何跨仓契约面改动（改错误码、加路由、扩枚举值域）前必读本节。

### 铁律：真源永远在后端

| 契约面 | 真源（imboy 仓） | 客户端镜像 |
|--------|------------------|------------|
| 错误码 | `include/error_code.hrl` | imboyapp `lib/config/error_code.dart`（生成物，入仓） |
| API 端点 | `src/imboy_router.erl`（可执行路由实况） | `.contract/api_contract.json` 产物 + `api/openapi.yaml`（手工文档） |
| DB 枚举值域 | `priv/migrations/*.up.sql` 的 `CHECK` 约束 | imboyadmin `src/types/billing.ts` 类型注释 / imboyapp 钱包注释 |
| EntityId/TSID | canonical：JSON 传输层 integer（TSID 64-bit） | admin 网络层 `safeParseBigIntJson` + `EntityId` 类型 |

**任何"以 Flutter 为准还是以 Admin 为准"的争论一律非法**——客户端只是真源的镜像，不存在反向"先改客户端再让后端追认"。手改客户端生成物绕过门禁同样非法。

### 合法变更流程（两步跨仓提交）

> 三仓是三个独立 git 仓，"同一 PR 内同时改后端真源 + Flutter 生成物"结构性不可行
> （PR 只存在于单仓）。因此合法变更固定拆成**两步跨仓提交**，顺序不可颠倒。

**第 1 步 —— imboy 仓（真源 + 契约产物，同一 PR）**

```bash
cd imboy
# 1. 修改真源：include/error_code.hrl / src/imboy_router.erl / priv/migrations（新枚举约束需在 scripts/contract_gate.py 的 ENUM_SOURCES 登记）
# 2. 一行重生成全部契约物：
make contract-regen
#    ① 重新导出 .contract/api_contract.json（确定性输出：内容不变则文件不变）
#    ② 若并排存在 ../imboyapp，顺带重生成 imboyapp/lib/config/error_code.dart
#    （并排仓是本地布局假设；CI 与独立 clone 缺仓时该步 WARN 跳过，到 imboyapp 仓内自行生成）
# 3. 同一 PR 提交：真源改动 + .contract/api_contract.json
# 4. 本地预检（可选 ADMIN_DIR= / FLUTTER_DIR= 覆盖客户端仓路径）：
make contract-check
```

**第 2 步 —— imboyapp 仓（生成物，单独提交）**

```bash
cd imboyapp
dart run scripts/generate_error_code.dart        # 若第 1 步未在并排布局下顺带生成
git diff lib/config/error_code.dart              # 生成物 diff 可见可 review
# 提交 lib/config/error_code.dart（commit message 建议 chore(contract): regen error_code from hrl@<后端短 SHA>）
```

**为什么顺序不可颠倒**：CI（见下表）checkout 的是**远端 main/dev 快照**。imboyapp 的契约门禁会双仓 checkout 后端真源做逐字比对——若 Flutter 生成物先合入而后端真源 PR 尚未推送/合并，比对必然失败（红门禁）；若后端真源被回滚，Flutter 生成物就成了无源之水。固定顺序 = **后端真源先合并 → Flutter 生成物再提交**。

### 非法漂移 = 只改一端（这些 CI 会红）

| 漂移场景 | 红的 CI | 检查内容 |
|----------|---------|----------|
| 后端真源改了，`.contract/api_contract.json` 未同步提交 | imboy `.github/workflows/contract-gate.yml` | 重导出 vs 落仓产物 diff |
| admin / flutter 枚举与后端注册表不一致（多值、缺关键值） | imboy `.github/workflows/contract-gate.yml` | 双端枚举 diff（`scripts/contract_gate.py check`） |
| EntityId 规则违规（ID 字段裸 `number` 等） | imboy `.github/workflows/contract-gate.yml` | EntityId/TSID 规则子集 |
| 后端 `.hrl` 改了，imboyapp 生成物未重生成 | imboyapp `.github/workflows/contract.yml` | 双仓 checkout 后 `dart run scripts/generate_error_code.dart --check` 逐字断言 |
| 手改 imboyapp 生成物（绕过生成器） | imboyapp `.github/workflows/contract.yml` | 同上（生成物与真源失配即红） |

**本地预检（push 前跑，别等 CI）**：

```bash
# imboy 仓：自检 + admin/flutter 枚举 diff + EntityId 规则（任一漂移非零退出）
make contract-check

# imboyapp 仓：生成物 vs .hrl 逐字校验
# 默认读跨仓相对路径 ../imboy/include/error_code.hrl；
# 独立 clone（无并排 imboy）时用 --source= 或环境变量 IMBOY_ERROR_CODE_HRL 指定
dart run scripts/generate_error_code.dart --check
```

### 错误码变更 SOP（与上同一逻辑）

```text
改 imboy/include/error_code.hrl
  → make contract-regen（imboy 仓内重生成 api_contract.json + 顺带 imboyapp error_code.dart）
  → imboy 仓提交真源 + api_contract.json（第 1 步 PR）
  → imboyapp 仓提交 lib/config/error_code.dart（第 2 步）
  → 双仓 contract 门禁绿
```

新增错误码必须走 `?ERR_XXX` 宏定义（不用裸数字），并注意 `contract_gate.py` 会按错误码段位（每 100 一段）做摘要入契约物。

### EntityId / TSID canonical 约定

- **canonical**：ID 在 JSON 传输层一律 **integer**（TSID 64-bit）；`server_ts` 等时间戳为 13 位毫秒 number（< 2^53，JSON 安全）。
- **admin 侧**：网络层经 `safeParseBigIntJson` 转 string，TS 类型一律 `EntityId`；ID 字段（`id`、`user_id`、`wallet_id`、`plan_id`、`tenant_id`、`subscription_id`、`group_id`、`channel_id` 等）**禁止裸 `number`**。
- **校验**：以上规则由 `scripts/contract_gate.py` 的 EntityId 检查项承担（`make contract-check` 带 admin 目录时自动执行），违规逐条列出并非零退出。

### 当前已知漂移欠账（drift debt）

> 已全部清零（2026-08-23）：`contract_gate.py` 所有枚举 binding 均为 `exact` 模式，
> 无 `drift_debt` 标记。历史欠账的处理记录：

| 原欠账 | 处理结果（2026-08-23） |
|------|------------------------|
| `payment_tx_status=5`（退款中） | ✅ admin 注释/`TX_STATUS_LABELS`/`TX_STATUS_VARIANTS`/筛选下拉已补 5，binding 转回 exact |
| `wallet_tx_type=4` 语义缺失 | ✅ 代码考古：后端无 `tx_type => 4` 写入路径，定为「保留（未使用）」并双端登记，binding 转 exact |
| `wallet_tx_type=2` 语义错误（隐匿欠账） | ✅ 修正：双端曾写「2=充值退款」，代码实写证明 2=频道订单支付（消费扣减、负数出账，`payment_wallet_gateway.erl:61`）、3=订单退款（`:111`）；admin/flutter/docs 三处已同步纠正 |
| `wallet_status=2` 语义缺失 | ✅ 代码考古：仅 status=1 在用（freeze/unfreeze 守卫），0=冻结（语义预留）、2=保留（未使用），admin 注释补齐，binding 转 exact |
| `openapi.yaml` 与 router 漂移（573 vs 494，95/16 端点差异） | ⏳ 仍为 informational（不阻塞，`make contract-check` 打印）；后续增量补齐或建立豁免清单 |

---

## 代码规范速查

### 后端（Erlang）

见 `imboy/doc/standards/` 目录：

- UTF-8 字符串必须带 `/utf8` 后缀
- 错误码使用 `?ERR_XXX` 宏，不用裸数字
- 所有数据库操作必须走 `elib_pg` 模块
- TSID 是唯一 ID 标准，不再用 HashID / BIGSERIAL
- 层级边界严格：`Handler → Logic → DS → Repo`，禁止跨层直连

### 客户端（Flutter）

- 状态管理统一 Riverpod（不再接受 GetX 新代码）
- 遵循 `imboyapp/DESIGN.md` 的 iOS 原生感风格
- 测试命名：`describe behavior, not implementation`

### 管理后台（React）

- 使用 `DataTablePagination` 组件，不允许自定义分页
- 默认 `size = 10`，搜索/筛选/切页时 `page` 必须重置为 1
- 遵循 `imboy-admin-frontend/CLAUDE.md` 约定

---

## 许可协议 / License

本项目采用 [木兰宽松许可证第 2 版（MulanPSL-2.0）](./LICENSE)。

**贡献即表示你同意：**
- 你有权提交该代码
- 你的贡献将以 MulanPSL-2.0 授权给项目及其所有用户
- 你以 [DCO](https://developercertificate.org/) 形式确认以上两点

---

## 发版流程（仅 maintainer）

1. 更新根 `VERSION` 文件
2. 更新根 `CHANGELOG.md`（`[Unreleased]` → 新版本）
3. 三端同步版本：`imboy/Makefile` / `imboyapp/pubspec.yaml` / `imboy-admin-frontend/package.json`
4. 打 tag `vX.Y.Z` 并推送
5. CI 自动构建产物：`aab`、`ipa`、`docker image`、`source tarball`
6. 创建 GitHub Release 并附带产物

---

## 问题？

- 一般问题：GitHub Issue（Question 模板）
- 安全问题：`security@imboy.pub`
- 其他联系：见根 `README.md` 或官网

感谢你让 IMBoy 变得更好！
