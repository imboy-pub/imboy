# 存量文档整理方案（Cleanup Plan）

> **类型**：指南 · **读者**：文档负责人 · **版本**：v1.0 · **最后更新**：2026-07-24
> **原则**：只移动，不删除；每批移动必须同步更新全部引用；`git mv` 保留历史。

## 诊断：乱的四种形态

| 形态 | 证据 | 处方 |
|------|------|------|
| 模糊筐目录 | `analysis/` 40 篇（契约/审计/商业分析混装）、`engineering/` 9 篇、`dev/` 3 篇 | 按四象限拆分到 reference/guides/explanation |
| 职能重叠目录 | `planning/`(4) + `plans/`(2) + `roadmap/`(7) 三个"计划"目录 | roadmap 保留；planning/plans 归 archive |
| 单文件目录泛滥 | audit/ payment/ migrations/ libraries/ design/ 等 10 个目录只有 1 个文件 | 按主题并入邻近目录 |
| 过程材料混居 | `review/` 17 篇评审材料与正式文档平级 | 归 archive/review/ |

## 分批执行清单

### 批次 A — imboyapp 根目录散落文件 ✅ 已完成（2026-07-24）

| 文件 | 目标 | 原因 |
|------|------|------|
| `imboyapp/TEST.md` | `imboyapp/docs/qa/TEST.md` | 测试笔记归 qa/ |
| `imboyapp/TEST_1.md` | `imboyapp/docs/qa/TEST_1.md` | 同上 |
| `imboyapp/QA_PRO_CLICK_TEST_REPORT.md` | `imboyapp/docs/qa/QA_PRO_CLICK_TEST_REPORT.md` | 同上 |

验证：全仓无引用（grep 确认）。`DESIGN.md` 保留根目录（与 imboyadmin 约定一致）。

### 批次 B — analysis/ 参考类迁 reference/ ✅ 已完成（2026-07-24）

| 文件 | 目标 | 四象限归属 |
|------|------|-----------|
| `reference/rest-api.md` | `reference/rest-api.md` | 参考（基础契约） |
| `reference/rest-api-v1-catalog.md` | `reference/rest-api-v1-catalog.md` | 参考 |
| `reference/ws-protocol-contract.md` | `reference/ws-protocol-contract.md` | 参考 |
| `reference/websocket-api-2.md` | `reference/websocket-api-2.md` | 参考 |
| `reference/tsid-field-convention.md` | `reference/tsid-field-convention.md` | 参考 |
| `reference/tsid-field-matrix.md` | `reference/tsid-field-matrix.md` | 参考 |
| `reference/contracts/channel_api_contract_v1.md` | `reference/contracts/channel_api_contract_v1.md` | 参考 |
| `reference/contracts/moment_api_contract_v1.md` | `reference/contracts/moment_api_contract_v1.md` | 参考 |
| `reference/contracts/e2ee_server_persisted_shard_contract_v1.md` | `reference/contracts/e2ee_server_persisted_shard_contract_v1.md` | 参考 |
| `guides/sentry-dsn-integration-guide.md` | `guides/sentry-dsn-integration-guide.md` | 指南 |

只移动不改名，引用更新为机械的路径前缀替换。全部引用点已同步更新。

### 批次 C — 过程材料归 archive/ ✅ 已完成（2026-07-24）

| 源 | 目标 | 说明 |
|----|------|------|
| `review/` 17 篇 | `archive/review/` | 评审过程材料 |
| `planning/` 4 篇 | `archive/planning/` | 需先更新 3 处交叉引用（adr/0003 等） |
| `plans/` 2 篇 | `archive/plans/` | 已完成的历史计划 |
| `analysis/` 剩余 ~20 篇审计/计划类 | `archive/analysis/` | `*-audit-*`、`*-plan`、`code-review-*`、`execution-checklist` 等 |

注意：analysis 中 4 篇商业分析（`ai-era-monetization-*`、`business-value-*`、`im-market-*`、`monetization-path-*`）**不归 archive**——它们是有效业务文档，建议移 `business/`（新建）或留待确认。

### 批次 D — 单文件目录并编 ✅ 已完成（2026-07-24）

| 目录 | 文件 | 建议去向 |
|------|------|---------|
| `audit/` | 1 篇 | 按主题归 security/ 或 archive/ |
| `payment/` | 1 篇 | guides/（支付集成指南）或 explanation/ |
| `migrations/` | 1 篇 | guides/migrations/ |
| `libraries/` | 1 篇 | reference/ |
| `design/` | 1 篇 | explanation/ |
| `dev/` | 3 篇 | cheatsheet→reference/；changelog→engineering/；backend-readme 与根 README 去重 |

### 批次 E — 大专题目录拆分 ✅ 部分完成（2026-07-24）

- `e2ee/` 36 篇（含 v2/ 子目录）：拆为 `guides/e2ee/`（操作）+ `reference/e2ee/`（协议契约）+ `explanation/e2ee-design.md`（设计理由）
- `testing/` 16 篇：整体移 `guides/testing/`（基本都是任务型指南）
- `operations/` 17 篇：整体移 `guides/operations/`
- `engineering/` 9 篇：notes 类移 `reference/engineering/`，overview 移 explanation/

## 执行纪律

1. 每批执行前先 `grep -rl <文件名>` 找全部引用点，移动后逐一更新，再跑死链检查。
2. 每批一个独立 commit，信息格式：`docs: batch X — move <源> to <目标>`。
3. 移动后在源目录留 `.moved` 说明文件（一批一个），30 天后清理。
4. archive/ 内保留原目录结构，永不删除。

### 批次 F — release/plugin/CODEMAPS 归位 ✅ 已完成（2026-07-24，团队协作）

- release/ 4 活跃指南 → guides/release/；2 快照 → archive/release/；3 商业 → business/
- plugin/ 3 规范 → reference/plugin/
- CODEMAPS/ → archive/CODEMAPS/（实测失真 +46%，已加基线警告）
- roadmap/ 8 篇保留原位（TASKS.md 是 loop 状态真源，不可动）
- api-sandbox/ 保留（工具与相对路径强耦合）

### 批次 G — architecture/standards/templates/benchmark 归位 ✅ 已完成（2026-07-24，团队协作）

- architecture/adr/ 2 篇 → docs/adr/0005、0006（统一编号风格，索引登记原名）
- architecture/ 5 篇完成历程 → archive/architecture/；2 篇设计 → explanation/
- 保留 architecture/ 仅 4 篇活跃（overview/module_map/database-access/module-layer-cheatsheet）
- standards/ 3 篇 → reference/；e2ee-key-rotation → guides/e2ee/（待并入）；migration_naming → reference/engineering/
- templates/PRIVACY_POLICY → legal/（加法务守门提示）
- benchmark.md → guides/operations/

### 跨仓批次（imboyapp + imboyadmin）✅ 已完成（2026-07-24）

- imboyapp：feature-status → reference/；2 审计 → qa/audits/；1 完成计划 → archive/plans/；FAQ/changelog/privacy-policy 保留根级（Flutter asset 打包，已加警示）
- imboyadmin：4 契约 → api-contracts/ + 草案状态标注

### 剩余尾巴（不影响结构稳定）

- architecture/ 4 篇活跃文档未来可迁 explanation/ 或保留专题（owner 决定）
- guides/e2ee/ 36 篇专题待 owner 细分（当前保持结构）
- docs/ 一级目录最终：15 目录 + 2 根文件（CONVENTIONS.md、README.md）
