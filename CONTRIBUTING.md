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

> **CI 强制执行**：`.github/workflows/ci.yml` 中的 `dco` job 会检查 PR 中的每一个 commit。
> **CI enforcement**: The `dco` job in `.github/workflows/ci.yml` checks every commit in the PR.

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

```bash
bash script/preflight.sh --native    # 检查本地依赖
```

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
