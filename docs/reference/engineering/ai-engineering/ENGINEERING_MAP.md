# 工程知识地图（Engineering Map）

> 版本 1.0 · 2026-11-18 · 隶属 [ai-engineering](./README.md)
> 原则：**每项只留指针+一句话**，事实活在源头（代码/CI/专项文档）。区分 ✅事实 / 📐约定 / ❓待确认。适用基线：imboy `259f5f480` / imboyapp `5ae7b5f2b` / imboyadmin `5a8925a`。

---

## Product Map（产品）

- ✅ imboy = 自托管 IM 平台：C2C/C2G、WS 长连、E2EE、Garage S3 直传 → `imboy/README.md`、`imboy/docs/`
- ✅ 版本权威源：后端 `imboy/CHANGELOG.md`；App `pubspec.yaml`（1.0.0-alpha.19）+ `docs/changelog.md`（asset 打包，禁移动）；Admin `package.json`（1.0.0-alpha.18）
- ❓ 商业化材料在工作区 `imboy-commercial-hub/`（敏感，不入任何 git）

## Architecture Map（架构）

- ✅ 后端四层 Handler→Logic→DS→Repo 单向 → `imboy/AGENTS.md`；边界校验脚本 `imboy/scripts/check_module_boundaries*`
- ✅ Flutter MVVM+Repository、Riverpod 100%、go_router → `imboyapp/CLAUDE.md`（唯一真源，AGENTS.md 是指针）
- ✅ Admin React 19.3+Vite+Radix+Zustand+TanStack → `imboyadmin/AGENTS.md`
- 📐 全栈禁 Redis；TSID 64-bit 整数传输、前端 `EntityId`/`safeParseBigIntJson`
- ❓ imboy-textguard（Rust）候选未验收；hird 实验项目独立演进

## API Map

- ✅ OpenAPI 按三受众校验（contract-gate.yml）→ `imboy/docs/api-contracts/`（含 `three-platform-alignment.md`）
- ✅ error_code 跨端逐字门：`imboy/include/error_code.hrl` ⇄ `imboyapp/lib/config/error_code.dart`（`ci-source-lock.json` SHA 锁）
- ✅ 路由：`imboy/src/imboy_router.erl`（新增端点四步流程见根级 AGENTS.md §AI 使用指引 4）
- ✅ WS 真路径 `/api/v1/ws`

## Protocol / Data Map

- ✅ SQLite 迁移唯一真源：`imboyapp/lib/service/migrations/manifest_all.dart`（schema v40）。40→39 降级边**只存在于 v40 新二进制**（`downgrade_edges_v40.dart`）；已安装的旧版本二进制不含该代码，故回退安装旧包前必须先用 v40 二进制把库降到 39（或快照恢复）
- ✅ PG 迁移：`imboy/priv/migrations/`（编号锁 `.Codex/coordination/migration-reservations/`）；erlang_migrate 独立库
- ✅ 跨端消息同步/协议对齐归 AUTOTEST v2.1 车道（BLOCKED 中，状态在 `_task_packages/IMBOY_AUTOTEST_V2.1/runtime/STATE.json` rev150）
- 📐 平行迁移 PR 冲突多取 main 侧；envied 生成物走 `ci_gen_env.sh`

## Security Map

- ✅ 三仓 secrets-scan/security-scan 工作流；gitleaks ratchet（imboyapp 22 / imboyadmin 40）
- ✅ E2EE 审计证据 → `.Codex/e2ee_audit/`；TSID 加固 → `.Codex/evidence/tsid-correctness-hardening/`
- 📐 PII 严禁入工作区/仓库；附件 URL 必经 `AssetsService.viewUrl` 授权
- ✅ strict 分支保护配方（dependabot 自动合并前置）→ `imboyadmin/.github/workflows/dependabot-auto-merge.yml` 头注

## Test Map

- ✅ imboy：1259 eunit + 19 CT + 32 quality 模块（`tools/imboy_quality/`）+ dialyzer 基线 ratchet（184 条指纹）+ nightly 慢门零豁免
- ✅ imboyapp：`test/auto_test/execution_contract.json` 机器合同；`run_test_suite.sh --profile pr|full`；integration_test 8 profiles 手动；golden 零跟踪 PNG 约定（`5ae7b5f2b`）
- ✅ imboyadmin：`bun run test`（--isolate 必须）；Playwright 主配置 + 7 专属配置；admin-e2e CI 仅 setup-flow
- ✅ 真机 E2E 手册与坑库：`.task-state/`、`_task_packages/IMBOY_AUTOTEST_V2.1/`（W7/RCT 车道沉淀）
- ❓ 覆盖率数字以最近一次 quality 收据为准（仓外维护），本文不复制以免漂移

## Delivery Map（交付流水）

- ✅ Issue→Task→Branch→PR→CI→Review→Merge 的既成流程：任务包（本目录 [TASK_CONTRACT.md](./TASK_CONTRACT.md)）→ 专用分支 worktree → PR 模板自检 → 三仓快门 CI → 属主合并（自动合并仅 dependabot 白名单，见 [AUTO_MERGE_POLICY.md](./AUTO_MERGE_POLICY.md)）
- ✅ 发布：imboy `release.yml`（tag v* 触发，golden-install/upgrade）；三仓 mirror.yml 镜像 gitee/gitcode（github 唯一 CI 源）
- ✅ 部署 → `imboy/deploy/`（Compose+Helm+Caddy+可观测栈；先读其 README/CLAUDE.md）
- 📐 一次提交=完整可回滚逻辑单元；禁盲目 `git add ./-A`；DCO 必须（signoff 邮箱=作者邮箱）

## Debt Map（技术债与在途）

- ✅ 工程总览与债务索引 → `imboy/docs/reference/engineering/engineering-overview.md`（10 个专项 md）
- ✅ 收口计划存档 → `imboy/docs/plans/`（37 条目 = 30 md + 7 sha256；**仅 2 份入 git**，其余为主工作树未跟踪文件——对买家/CI 不可见，最新三端 CI 收口 v2 2026-10-14）
- ❓ 在途：AUTOTEST v2.1（BLOCKED）、geo_regions PR#4/#5（待合）、PR#76 悬空 worktree（待属主）、OTP 收尾（待 push 授权）→ 明细见 [ASSESSMENT.md](./ASSESSMENT.md) §6

## 仓库外结构锚定（瞬态目录，2026-11-18 实测）

| 目录 | 职责 | 生命周期 |
|---|---|---|
| `_task_packages/` | 15 任务包（跨会话接力真源） | 任务收口后归档（如 closeout-archive-*） |
| `_task_worktrees/` | 任务 worktree（现存：imboy-ai-eng-v1 本任务、imboyapp-pr76-merge 悬空、**slimdown-v1=三仓 linked worktree 属在途另一车道勿动**、IMBOY_APP_UIUX_V10 与 IMBOY_WEB_MVP 2 个空壳待属主确认删除） | 任务结束即删（属主拍板前不清理） |
| `.task-state/` | 已收口任务控制目录/证据（garage、otp-opt） | 长期只读 |
| `.Codex/` | AI 工程资料：coordination（锁）、handoffs（交接 tar+sha256）、plans、runs、evidence | 长期 |
| 根级 `AGENTS.md`/`CLAUDE.md` | 工作区上下文（umbrella，非 git） | 属主维护，agent 勿擅改 |
