# IMBoy AI 工程体系现状审计（AI Engineering Assessment）

> 版本 1.0 · 2026-11-18 · 审计基线：imboy `259f5f480` / imboyapp `5ae7b5f2b` / imboyadmin `5a8925a`
> 审计方式：现场只读核查（git 状态、目录清点、CI workflow 逐个阅读、任务包结构比对），非文档推断。
> 上级索引：[README.md](./) · 实施与恢复契约：[TASK_CONTRACT.md](./TASK_CONTRACT.md)

---

## 1. 三仓库真实状态

| 仓库 | HEAD | 工作区 | worktree | 主线在途 |
|---|---|---|---|---|
| imboy (Erlang) | main `2b215c6f3`（2026-11-18 晚前进 1 笔 Makefile 修复） | 5 个未暂存修改（docker 系 4 文件 + postgres-read-replica.md）+ 未跟踪 `.plans/`（属主工作，勿动） | 主 + `/private/tmp/imboy-uiux-final-…-20261115`（detached）+ `_task_worktrees/slimdown-v1/imboy`（在途车道） | web-mvp 后端车道已并入 main；QR 登录 LEE-5、UTC 修复、OTP 运行时优化已落 |
| imboyapp (Flutter) | main `5ae7b5f2b` | 干净 | 主 + `_task_worktrees/imboyapp-pr76-merge`（detached `98dcde66c`，13 个生成文件 M，7 commits 领先 origin/main，**PR#76 closed 悬空待属主拍板**） | v1.0.0-alpha.19；WebMVP V3.0 已收口并入 main |
| imboyadmin (React) | main `5a8925a` | 干净 | 主 + `/private/tmp/imboy-uiux-final-admin-20261114`（detached `eb4e24c`） | dependabot 6 PR 全合并终态 |

## 2. 已经真实运行的机制（SKIP_ALREADY_PASS 候选区）

### 2.1 质量与测试基建（三仓均真实现役）

| 能力 | imboy | imboyapp | imboyadmin |
|---|---|---|---|
| 单元测试 | 1259 个 `_tests.erl`（test/ 下）+ 19 个 CT 套件 | 3013 个测试文件（1459 `.dart`） | 263 个 `*.test.{ts,tsx}` |
| 质量编排 | `tools/imboy_quality/` 32 模块（+test_fixtures/）经 `include/quality.mk` 接管 `make eunit`（local 默认走 quality_full 路由） | `imboyapp/scripts/auto_test.py` + `test/auto_test/execution_contract.json` 机器合同；`run_test_suite.sh --profile pr\|full` | `bun run test`（--isolate 防跨文件泄漏） |
| 静态门 | dialyzer 递减基线（184 条指纹 ratchet，文件 188 行含头注释，`check_dialyzer_baseline.sh`）、xref-strict、elvis 5.0.3 | flutter_lints ^6.0 + strict 三开关；analyze ratchet 830/703；gitleaks 22 | eslint --max-warnings=0；ratchet lint 37/typecheck 31/knip 35/gitleaks 40 |
| CI workflows | 15 个（backend-ci 快门 <10min 全阻塞、nightly 慢门零 continue-on-error、quality、contract-gate、release、security、sbom、mirror、dependabot-auto-merge） | 8 个（ci、contract、quality、integration_test 手动 8 profiles、uiux-full-offline、security、mirror、dependabot-auto-merge） | 7 个（ci、quality、admin-e2e、security、mirror、dependabot-auto-merge、widget-pairing） |
| 本地钩子 | lefthook（erlfmt/gitleaks/conventional） | lefthook 三级（pre-commit fmt/analyze/gitleaks/error-code-sync；pre-push ratchet+契约，`IMBOY_SKIP_PRE_PUSH=1` 逃生门） | lefthook（pre-commit eslint+gitleaks；pre-push lint+test） |
| DCO | `dco-check` job（shell 自研实现 + 测试） | 同左 | 同左 |
| 自动合并 | dependabot 白名单（patch/minor/major 分级） | 同左 | 同左（**strict 分支保护配方固化在 workflow 注释**：merge API 服务端强制、GITHUB_TOKEN 读不了 branchProtectionRule） |
| 跨端契约 | `contract-gate.yml`（OpenAPI 三受众）；`docs/api-contracts/three-platform-alignment.md` | `contract.yml`：error_code.dart 与后端 `include/error_code.hrl` 逐字一致 + `ci-source-lock.json` SHA 锁双仓 checkout | `widget-pairing.yml` 与 imboy 对称互验 |

### 2.2 任务执行与状态管理（真实运行，但结构不统一）

- **任务包体系** `_task_packages/`（16 项 = 15 目录 + 散置 Eunit_plan.md）：从早期 `TASKS.md + state.json + evidence/`（Timezone/Garage 系）演化到 `runtime/STATE.json` 模式（AUTOTEST v2.1 最成熟：schema v3、rev150、receipts 100 条、history 133 条、`.state-writer.lock` 单写权、SHA256SUMS、快照链）。
- **控制目录** `.task-state/`：imboy-garage-direct-v1（state.json rev39、resume.md 45KB、evidence/ 63 项）、otp-opt-20261112-evidence（review-ledger.md 29 轮审查台账、final-gate-28.log、patches A1–F4）。
- **协调机制** `.Codex/coordination/`：heavy-backend.lock、migration-reservations（迁移号锁）、three-track 会话目录（多 agent 分轨 + 请求-追踪文件）。
- **工程交付边界**：三仓 AGENTS/CLAUDE.md 统一段落——仅授权时 commit、一次提交=完整可回滚逻辑单元、禁盲目 `git add ./-A`、多 agent 由主集成者控制共享分支提交、不绕 hooks/CI；imboy 另有"子 agent 不跑全量 EUnit，最终集成候选由主集成者统一跑"的验证节奏。

### 2.3 已完成收口的历史主线（避免重复建设）

- IMBOY_ERLANG_QUALITY 质量工程：已 closeout-archive-20261105，车道经 `209004c0e` 归档收编进 main；代码标记 `IMBOY_ERLANG_QUALITY` 存于 39 文件（其中全串 `IMBOY_ERLANG_QUALITY P1-03` 7 文件）。
- WebMVP V3.0：rev963 收口，双远端已推（imboy `259f5f480` / imboyapp `5ae7b5f2b` 即其产物）。
- 三端 CI 收口 v2（2026-10-14 plan）与 dependabot triage 终态。

## 3. 只有规划/已漂移、尚未落地的机制

| 项 | 现状 | 证据 |
|---|---|---|
| 统一任务契约 | **缺失**。16 项（15 目录 + 1 散文件）4 种结构模式并存；`execution-log.md` 在所有历史包中不存在 | `_task_packages/` 结构比对（2026-11-18） |
| 独立验证政策成文 | **缺失**。语义散落于 AUTOTEST STATE.json receipts、otp-opt review-ledger、TEST_PLAN.md §5 | 全域无 verification-policy 文件 |
| 工程知识地图 | **部分**。`docs/reference/engineering/` 10 个专项索引存在，但自述"实施计划与执行证据在仓库外维护"——仓库外结构（_task_packages/.task-state/.Codex）无锚定文档 | engineering-overview.md 头注 |
| Agent 角色与并发模型成文 | **缺失**。仅有"主集成者"表述与 .Codex/coordination 事实实践 | 三仓 AGENTS.md |
| 自动合并政策成文 | **部分**。dependabot-auto-merge.yml 注释即事实政策，但无独立文档、无风险分级扩展路径 | 三仓 workflow |
| quality-loop.md 计划引用 | **漂移**。该计划已被收编删除，但 `quality.yml:3`、`pull_request_template.md:3`、`codemap.yml:3,109`、`disabled-workflows/sonar.yml:5` 仍引用 | `.claude/plans/` 现仅 3 个无关计划 |
| IMBOY_ERLANG_QUALITY 运行时指针 | **失效**。`RUNTIME_LOCATION.json` 指向 `imboy/.git/zcode-tasks/…`（已不存在），实际状态在 closeout-archive | 现场核查 |
| AUTOTEST STATE.json 的 runs/ 引用 | **悬空**。handoff 引用 `runtime/runs/at13/…` 但目录不存在（该包 BLOCKED 中，**不修**，仅登记） | 现场核查 |

## 4. 测试能力与缺口

**强项**：快门 CI 全阻塞化（imboy backend-ci）；ratchet 棘轮框架三仓统一；跨端 error_code 契约逐字门；dialyzer 基线只减不增；nightly 慢门零豁免。

**缺口**（按影响排序）：
1. **跨端动态契约**：error_code 是静态逐字门，但 API 行为/WS 协议/DB 迁移的三端一致性主要靠人工车道（AUTOTEST v2.1 正是为此，当前 BLOCKED）。
2. **验证证据标准化**：各任务包证据格式不一（receipts/ledger/log 三种），无法机器汇总 Verified Delivery Rate。
3. **Flutter E2E 依赖真机**：integration_test 8 profiles 手动触发；NC5 硬绑 Android；CI 无真机池。
4. **imboyadmin E2E 范围**：admin-e2e.yml 仅 setup-flow（API mock），其余 spec 需真实后端在本地跑。

## 5. Agent 执行能力与缺口

**能力**：worktree 隔离实践成熟（含 detached 车道）；迁移号锁（migration-reservations）；单写权锁（.state-writer.lock）；DCO 纪律；交接 tar 包（handoffs/ + sha256）。

**缺口**：
1. 无统一角色定义（Coordinator/Worker/Verification 边界靠惯例）；
2. 无"验证独立性"硬规则（Verification 不得复用 Worker 工作目录，尚未成文）；
3. 并发上限未成文（实践默认 2-3，曾发生 installation API 配额被多循环并发烧干事故）；
4. 断点恢复协议各包各自为政（RESUME_PROTOCOL.md 三种版本）。

## 6. 在途任务与依赖（2026-11-18 快照）

| 任务 | 状态 | 依赖/阻塞 | 本体系动作 |
|---|---|---|---|
| IMBOY_AUTOTEST_V2.1 | rev150 BLOCKED_PREDECESSOR_REQUIRED_EVIDENCE（CALL-08 复测） | 前任证据义务 | 不干预，登记 |
| geo_regions Phase 2 | PR#4/#5 绿待合 | 属主合并决策 | 不干预 |
| imboyapp PR#76 | closed 悬空，worktree 存活 | 属主拍板（重建或放弃） | 不清理，登记 |
| OTP 优化收尾 | 车道载体待属主核实（2026-11-18 晚本轮 ref 下本地无 OTP 分支；证据在 `.task-state/otp-opt-20261112-evidence/`） | 属主核实 + push 授权 | 不干预 |
| web-mvp 运行面 | beam 96275@52392 存活（PG fixture 严禁重启） | — | 环境冲突登记 |

## 7. 重复建设风险清单

- ❌ 不要新建 Agent 框架/调度器：quality_full 路由 + auto_test.py + CI 已是编排层。
- ❌ 不要新建状态系统：AUTOTEST STATE schema v3 是既成标准，统一契约应**收敛到它**而非另起。
- ❌ 不要新建跨端契约门：contract.yml/widget-pairing 已运行，扩展走同模式。
- ❌ 不要把任务状态迁入 git 仓（_task_packages 瞬态约定是既定架构决策）。

## 8. 优先级建议（任务包实施计划的输入）

- **P0**：统一任务契约模板落地；RUNTIME_LOCATION.json 失效指针修复；quality-loop 引用漂移修复；验证证据最小 schema。
- **P1**：验证独立性规则成文；工程知识地图锚定仓库外结构；园丁机制（缺陷→规则闭环，复用 ratchet 模式）。
- **P2**：CI 失败自动归因、任务自动调度、指标看板（全部待属主授权后另立任务）。
