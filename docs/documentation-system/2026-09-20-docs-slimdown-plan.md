# 历史计划与证据瘦身执行计划 | 2026-09-20

> 状态：EXECUTED（2026-09-20；RUN_ID docs-slimdown-20260920T031556Z；终态见第 8 节）
> 目标：尽可能从当前工作树删除历史计划、过程报告与执行证据；需要追溯时只从 Git 历史恢复。
> 执行范围：`/Users/leeyi/project/imboy.pub/imboy` 单仓；工作区根不是 Git 仓库。
> 并行上限：最多 8 个活跃 Agent，包含协调器 A0；最多 7 个非协调器同时活跃；已完成席位可复用，不限制累计 Agent 数。

## 1. 最终裁定

1. 删除 `docs/plans/**` 与 `docs/planning/**` 的全部 Git 跟踪文件，不为历史可发现性另建 archive 副本。
2. 删除旧的 Moya OpenAPI evidence 快照，不迁入 `docs/reference/`。该文件使用旧 `/api/v1/teaching/...` 路径，而当前代码是 `/api/v1/moya/...`，不能升级为现役契约。
3. 清理当前跟踪树对被删路径的语义引用。稳定事实改指代码、正式 ADR、architecture、reference、compliance、security audit 或 CHANGELOG；无现役价值的历史注释直接删除。
4. 对 ignored/untracked 的 `docs/plans/**`、`docs/planning/**`，只删除内容可由当前仓库可达 Git blob 精确恢复的文件；没有可达 blob 的文件保留并报告，不猜测、不覆盖。
5. 不重写 Git 历史，不修改 `.gitignore`，不 push，不发布，不部署，不操作远端分支。
6. 每个可写分片独立提交；A0 串行集成并生成最终提交。不得混入用户或其他会话的无关改动。

## 2. 当前基线

评审基线 HEAD：`d0a7ea6e6de5b729c74fbb14befc83e83d00a170`。执行时必须重新采样，以执行时 Base 为准；禁止 reset 到本哈希。

| 指标 | 当前事实 | 终态目标 |
|---|---:|---:|
| `git ls-files docs` | 608；本计划提交后为 609 | 403 |
| `git ls-files docs/plans` | 142 | 0 |
| `git ls-files docs/planning` | 64 | 0 |
| 本次跟踪删除总数 | 206 | 206 个全部从当前树删除 |
| 本次跟踪删除字节 | 1,669,587 bytes | 全部移出 HEAD |
| `docs/plans` 磁盘文件 | 355；其中 213 ignored | 跟踪文件全删；ignored 按可恢复性裁定 |
| `docs/planning` 磁盘文件 | 69；其中 5 ignored | 跟踪文件全删；ignored 按可恢复性裁定 |
| 当前树相关引用 | 78 处、65 个文件 | 未裁定悬空引用为 0 |

上述数字只绑定评审基线。A0 必须在执行开始和最终集成后重新生成同口径数据，不得继承旧统计冒充验收。

## 3. 范围边界

### 3.1 本次允许修改

- 删除全部 Git 跟踪的 `docs/plans/**`、`docs/planning/**`。
- 修改当前 `imboy` 仓内对上述目录的引用和说明。
- 修改 `docs-site/.vitepress/config.ts`，启用 VitePress 原生死链失败门。
- 更新本文档的最终验收表和执行结果。
- A0 删除经 Git blob 可恢复性证明通过的 ignored 本地文件，并在运行证据中记录 blob OID 与恢复命令。

### 3.2 明确不做

- 不删除无法从可达 Git blob 恢复的 ignored/untracked 文件。
- 不处理 `imboyapp/test/auto_test/reports/**`。它们当前全部未跟踪，删除后不能从本仓 Git 历史恢复。
- 不移动或删除 imboyapp 根级 I18N 审核材料。母语审核仍是开放流程，不属于历史计划清理。
- 不做全仓 `.DS_Store` 清理；不进入 `ios/**`、`macos/**`、`plugin/r_upgrade/**` 等保留区。
- 不补齐 Moya OpenAPI 覆盖。正式 `api/openapi.yaml` 的缺口另行立项，本次只删除过时 evidence 引用。
- 不修改业务行为、数据库结构、API 路由、错误码、认证、部署配置或生产数据。
- 不清理其他 worktree、其他运行目录、其他 Agent 的 lease、进程、端口或数据库。
- 不 push、不发 PR、不发布、不部署。

## 4. 真源与恢复规则

### 4.1 跟踪文件恢复

跟踪文件删除后按删除前 Base 恢复：

```bash
git restore --source=<BASE_SHA> -- <path>
```

### 4.2 ignored/untracked 文件恢复资格

只有文件内容哈希对应的 blob 存在于 `git rev-list --objects --all` 可达对象集合时，才允许删除。每个删除项必须记录：

- 原路径；
- SHA-256；
- Git blob OID；
- 一个历史路径；
- 恢复命令 `git cat-file blob <OID> > <path>`。

只在 `.reports/docs-slimdown/<RUN_ID>/recoverable-local-files.tsv` 记录路径和哈希，不把文件内容写进日志。没有可达 blob、检查失败或路径不明确时，一律保留并记为 `PRESERVED_NO_GIT_RECOVERY`。

## 5. Agent 所有权

| Agent | 所有权 | 主要任务 | 禁止事项 |
|---|---|---|---|
| A0 | 协调器；本文档；集成分支；运行证据 | 基线、lease、调度、串行集成、ignored 可恢复文件清理、验证、最终提交 | 不替 worker 越权修改；不 push |
| A1 | `docs/**`，排除 `docs/plans/**`、`docs/planning/**`、本文档 | 清理稳定文档中的历史路径；把仍需保留的事实内联或改指正式真源 | 不创建新 archive；不改代码 |
| A2 | `config/**`、`include/**`、`src/**` | 清理配置、头文件、生产源码中的历史引用；保留有价值的就地技术注释 | 不改运行逻辑 |
| A3 | `priv/migrations/**` | 清理迁移文件中的计划/evidence 注释；保留 DDL 语义注释 | 不改 SQL 语句、迁移顺序或文件名 |
| A4 | `scripts/**`、`test/**` | 清理脚本和测试中的历史路径；保留可执行命令、Case ID 与断言语义 | 不改变脚本/测试行为 |
| A5 | 仅删除 `docs/plans/**`、`docs/planning/**` 的跟踪文件 | 固化删除清单并执行 206 个跟踪文件删除 | 不碰 ignored 文件；不改其他路径 |
| A6 | `docs-site/**` | 移除 `ignoreDeadLinks: true`，验证同步与 build 能把本地死链变成失败 | 不新增依赖；不改文档正文 |
| A7 | 全仓只读 | 独立复核删除清单、引用处置、越界改动、恢复证明和最终门禁 | 不写文件、不提交 |

任何路径只能有一个写 Owner。发现所有权重叠时停止相关分片并交 A0 裁定，不得同时编辑。

## 6. 执行波次

### Wave 0：A0 基线与保护

1. 确认 Git 根、HEAD、分支、status、worktree、活动协调器和现有 lease。
2. 若存在占用同仓写权限的活动协调器，产出 `BLOCKED_ACTIVE_COORDINATOR`，不接管、不停止对方。
3. 创建唯一 `RUN_ID` 和 `.reports/docs-slimdown/<RUN_ID>/`。
4. 保存 Base SHA、status、206 个跟踪删除路径、78 条引用、ignored 文件清单及命令退出码。
5. 冻结 A1-A6 独占路径；A7 只读。

验收 `DS-00`：基线记录含 cwd、Base SHA、branch、status、worktree、lease、命令、退出码和清单哈希。

### Wave 1：六个写分片并行，A7 只读预审

- A1-A6 可同时工作，各自在隔离 worktree/分支中提交。
- A5 只删除 Git 跟踪文件；禁止操作主工作树里的 ignored 文件。
- A1-A4 只改引用和注释，不借机重构业务代码。
- A6 复用已安装的 VitePress，不新增 link checker 或其他依赖。
- A7 对 Base 做只读审计，形成“必须清理引用”和“允许保留的通用政策引用”清单。

验收：

- `DS-01`：删除清单恰好等于 Base 上 `docs/plans` 与 `docs/planning` 的跟踪并集。
- `DS-02`：旧 Moya `/api/v1/teaching/...` evidence YAML 已在删除清单中，未迁入任何 reference/archive 路径。
- `DS-03`：A1-A4 的改动不改变业务、SQL、脚本或测试执行语义。
- `DS-04`：A6 未新增依赖，VitePress 不再忽略本地死链。

### Wave 2：A0 串行集成

建议顺序：A5 删除 -> A1 文档 -> A2 源码/配置 -> A3 迁移 -> A4 脚本/测试 -> A6 docs-site。

每次集成前检查 Base、所有权和 diff；有冲突时由原 Owner 基于最新集成头重做最小补丁。禁止用 ours/theirs 覆盖未知改动。

集成后运行引用门：

```bash
git grep -n -E 'docs/(plans|planning)/|\]\([^)]*(plans|planning)/' -- \
  ':!docs/documentation-system/2026-09-20-docs-slimdown-plan.md'
```

允许保留的只有明确描述 ignore 政策或运行时 glob 排除的通用目录引用；不得保留具体已删文件、evidence run、章节号或把本地计划称为现役权威的引用。A0 必须把允许项逐行写入证据，不得用整文件排除隐藏结果。

验收 `DS-05`：所有具体路径引用归零，剩余通用目录引用全部在 allowlist 中并经 A7 复核。

### Wave 3：A0 清理可从 Git 恢复的 ignored 文件

仅处理主工作树中的：

- `docs/plans/**`
- `docs/planning/**`

A0 对每个 ignored/untracked 文件执行内容哈希和可达 blob 核验。通过者写入恢复清单后删除；不通过者保留。不得使用目录级 `rm -rf`、通配删除或仅凭同名路径判断可恢复。

验收 `DS-06`：每个本地删除项都有可达 blob OID 和可执行恢复命令；无证明文件零删除。

### Wave 4：验证、独立复核与最终提交

A0 在同一集成头执行：

```bash
git ls-files docs/plans | wc -l
git ls-files docs/planning | wc -l
git ls-files docs | wc -l
git diff --check
cd docs-site && bun run build
```

A7 独立检查：

- 删除集合是否超出两个目标目录；
- 是否误删无法恢复的 ignored 文件；
- 是否残留具体历史路径；
- 是否把旧 OpenAPI evidence 搬成新真源；
- 是否存在依赖、业务、SQL、测试行为或保留区改动；
- 每条验收是否绑定当前候选 SHA 和真实退出码。

验收：

- `DS-07`：`docs/plans` 跟踪数 = 0。
- `DS-08`：`docs/planning` 跟踪数 = 0。
- `DS-09`：若执行 Base 仅比评审基线多本文档，则 `git ls-files docs` = 403；否则使用 `BASE_DOCS - BASE_DELETE_COUNT` 精确对账。
- `DS-10`：VitePress build PASS，且未启用 `ignoreDeadLinks` 绕过。
- `DS-11`：`git diff --check` PASS；A7 给出 `PASS` 或列出阻断项。
- `DS-12`：任务改动按独立功能本地提交，作者/提交者为 `leeyi <leeyisoft@qq.com>`；没有 push、发布、部署。

## 7. 提交边界

建议最终保持两个本地提交，避免把治理规则和大规模删除混在一起：

1. `docs: define historical documentation slimdown execution plan`
2. `docs: remove historical plans and evidence`

第二个提交应包含 206 个历史文件删除、引用修正和 docs-site 死链门。若执行 Base 漂移导致删除数量变化，以运行时清单为准，并在本文档最终结果中解释差异。

禁止 blanket stage。A0 必须按任务路径显式暂存，并在提交前检查 `git diff --cached --name-status`。

## 8. 最终报告

A0 在本文档末尾补写以下字段后再提交执行结果：

| 字段 | 结果 |
|---|---|
| Base SHA / Final candidate SHA | Base `680973b0`（=计划提交，与评审基线 d0a7ea6e 仅差本计划文件）；执行候选为本次执行提交（见"本地提交"行） |
| 跟踪删除文件数 / 字节数 | 206 个 / 1,669,587 bytes（delete-list.txt 与 Base 两目录跟踪并集 diff 为空；非 D 状态 0） |
| `docs/plans` / `docs/planning` 最终跟踪数 | 0 / 0；`git ls-files docs` = 403（609 − 206 精确对账） |
| 引用扫描结果 / allowlist | 门 grep 剩余 2 处，全部在 allowlist：① scripts/check_module_boundaries.sh:29 运行时 rg 排除 glob（行为保留）；② docs/api-contracts/three-platform-alignment.md:67 imboyadmin 跨仓引用（目标在兄弟仓被跟踪）。另有 4 行历史语境化目录名描述未被门正则命中，登记于 DS-05-allowlist.md 供 A7 复核 |
| ignored 可恢复删除数 / 保留数 | 218 项核验：7 删除（内容 blob 100% 可达，含 SHA-256/blob OID/历史路径/恢复命令，抽验恢复一致）/ 211 保留为 PRESERVED_NO_GIT_RECOVERY / 0 missing |
| docs-site build / diff-check | `bun run build` PASS（0 死链，12.48s）；ignoreDeadLinks 已移除；附带修复 A6 死链门暴露的 70 处存量死链（与 plans/planning 无关，28 文件改指 GitHub 真源 URL/现存页/退化纯文字）；`git diff --check` 与 `--cached --check` 均 PASS |
| A7 独立结论 | 预审完成（74 MUST_CLEAN / 2 GENERIC_KEEP / 2 UNCERTAIN 已裁定）；终审基于执行提交 SHA 进行，结论见后续回填 |
| 本地提交 | `docs: remove historical plans and evidence`（298 文件：206 删除 + 92 修改；作者 leeyi <leeyisoft@qq.com>；A5/A1/A2/A3/A4/A6 六分片补丁 + A1b 死链修复串行集成） |
| 外向操作 | NONE（未 push、未发 PR、未发布、未部署、未动远端与 .gitignore） |

执行补充：集成方式为六分片 worktree 补丁（`git diff --binary Base..branch | git apply --index`，零冲突）+ A1b 增量；主树编译 `make compile` PASS；Wave 1 同时发现 Base 的 .gitignore L121-122 已含两目录忽略规则（历史遗留，本次未改）。

最终状态只允许：

- `PASS`：DS-00 至 DS-12 全部通过；
- `PARTIAL`：安全完成部分删除，但仍有明确未闭合引用或验证；
- `BLOCKED_ACTIVE_COORDINATOR`：同仓已有活动协调器；
- `BLOCKED_BASE_DRIFT`：Base 漂移使清单或所有权无法安全复用；
- `FAIL`：越界删除、无法恢复的本地文件被删、死链门失败或验证不通过。

## 9. ZCODE 启动提示词

以下提示词可直接交给 ZCODE。它是完整协调器合同，不依赖当前对话上下文。

```text
你是本任务唯一协调器 A0。执行：
/Users/leeyi/project/imboy.pub/imboy/docs/documentation-system/2026-09-20-docs-slimdown-plan.md

目标：最大幅度删除 imboy 当前树中的历史计划、过程报告和 evidence；已跟踪材料只从 Git 历史恢复。不得为了可发现性创建 archive 副本。必须清理稳定树中的悬空引用，并保证无法从可达 Git blob 恢复的本地 ignored 文件不被删除。

并发硬约束：
- MAX_ACTIVE_AGENTS=8，包含 A0。
- MAX_NON_COORDINATOR_ACTIVE=7。
- CUMULATIVE_AGENT_LIMIT=unlimited；已完成席位可复用。
- 只有 A0 可以调度/替换 Agent、集成提交、操作主工作树 ignored 文件和生成最终提交。
- 同一路径同一时间只能有一个写 Owner。

开始前必须完整阅读计划。以执行时当前仓库为唯一事实来源，先执行 DS-00：确认 cwd、Git 根、HEAD、branch、status、worktree、活动协调器、lease；重新统计 docs、docs/plans、docs/planning、引用和 ignored 文件。不得 reset、clean、stash、覆盖或吸收用户及其他会话改动。若存在占用 imboy 写权限的活动协调器，只输出 BLOCKED_ACTIVE_COORDINATOR 证据并停止写操作，不接管、不终止对方。

创建唯一 RUN_ID，证据写入 .reports/docs-slimdown/<RUN_ID>/。证据必须绑定 Base SHA、候选 SHA、命令、退出码、清单哈希和 Agent handoff；不得把文件内容、密钥、PII、环境变量或联系方式写入日志。

Base 冻结后最多同时派 A1-A7：
- A1：独占 docs/**，排除 docs/plans/**、docs/planning/**、计划文件。清理稳定文档引用；仍需保留的事实内联或改指正式真源。
- A2：独占 config/**、include/**、src/**。只清理历史引用和注释，不改变运行逻辑。
- A3：独占 priv/migrations/**。只清理历史计划注释，不修改 SQL、迁移顺序或文件名。
- A4：独占 scripts/**、test/**。只清理历史引用，不改变脚本或测试行为。
- A5：只删除 Base 上 Git 跟踪的 docs/plans/** 和 docs/planning/**；不得碰 ignored 文件或其他路径。
- A6：独占 docs-site/**。移除 ignoreDeadLinks: true，复用已有 VitePress 验证死链；不得新增依赖。
- A7：全仓只读审查，不写文件、不提交；预审引用清单，集成后做独立终审。

A1-A6 使用隔离 worktree/分支并各自提交；A0 按 A5 -> A1 -> A2 -> A3 -> A4 -> A6 串行集成。冲突交原 Owner 基于最新集成头重做最小补丁，禁止 ours/theirs 覆盖未知改动。每个 handoff 必须包含 AGENT、STATUS、BASE_SHA、OWNED_PATHS、FILES_CHANGED、COMMANDS_RUN、EXIT_CODES、ACCEPTANCE_IDS、EVIDENCE_PATH、RISKS、CONFLICTS、NEXT_ACTION。

删除规则：
1. 删除全部 Git 跟踪的 docs/plans/**、docs/planning/**，不归档。
2. 旧 Moya OpenAPI evidence 使用 /api/v1/teaching/...，当前路由为 /api/v1/moya/...；直接删除，禁止迁入 docs/reference 或 archive。
3. 稳定事实改指代码、ADR、architecture、reference、compliance、security audit 或 CHANGELOG；无价值历史注释直接删。
4. ignored/untracked 文件只允许 A0 在串行集成后处理。必须证明内容 blob 位于 git rev-list --objects --all 的可达集合，先写 recoverable-local-files.tsv，记录路径、SHA-256、blob OID、历史路径和 git cat-file 恢复命令，再逐文件删除。无法证明则保留为 PRESERVED_NO_GIT_RECOVERY。禁止 rm -rf、目录级删除和通配删除。

范围外：imboyapp、imboyadmin、moya、hird；test/auto_test/reports；I18N 审核材料；全仓 .DS_Store；ios、macos、plugin/r_upgrade；业务行为、SQL、API、认证、部署、生产数据；其他 worktree/run/lease/process/port/database；push、PR、发布、部署。

必须完成 DS-00 至 DS-12。最低验证：
- git ls-files docs/plans | wc -l 为 0；
- git ls-files docs/planning | wc -l 为 0；
- docs 总数按 BASE_DOCS - BASE_DELETE_COUNT 精确对账；
- 全仓具体 docs/plans 或 docs/planning 文件/evidence 引用为 0；允许的通用 ignore/glob 引用逐行列入 allowlist，不得整文件排除；
- docs-site 未启用 ignoreDeadLinks，bun run build 成功；
- git diff --check 成功；
- A7 独立审查 PASS；
- ignored 本地删除项 100% 有可达 blob 与恢复命令；
- git diff --cached --name-status 仅含任务路径。

验证通过后，用 leeyi <leeyisoft@qq.com> 做本地提交；不得 push。任务执行提交建议为 docs: remove historical plans and evidence。若发现用户无关改动，保留并排除，不要求工作树绝对干净。最终回填计划第 8 节并报告 PASS、PARTIAL、BLOCKED_ACTIVE_COORDINATOR、BLOCKED_BASE_DRIFT 或 FAIL；READY_FOR_DISPATCH、命令 exit 0、docs build 单独通过都不得冒充整体 PASS。

现在从 Wave 0 / DS-00 开始，持续执行到终态；常规判断采用当前条件下最安全的本地最优解，不因非阻断问题中途询问用户。
```
