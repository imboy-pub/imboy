# IMBoy E2EE 标准驱动体系（Standards-Driven）

> **版本**：2026.1 ｜ **建立**：2026-08-01 ｜ **维护者**：安全负责人 + 各层 owner
> **定位**：IMBoy E2EE 的长期维护骨架。所有"顶级"声明、任务立项、验收证据、对外口径都必须挂到本体系的某一层，禁止游离。

---

## 体系结构

```text
Research（研究层，季度复核）
├── research/industry-survey.md       行业调查：六玩家协议栈/可验证性/审计/PQ 现状
├── research/public-audit-cases.md    公开审计案例：交付物格式/证伪清单
├── research/rfc-index.md             规范索引：RFC/标准/白皮书锚定表
└── research/benchmark.md             对标矩阵：六玩家 × 能力维度（含 IMBoy 列）
        │
        ▼
Top-tier Standard（标准层，版本化）
└── top-tier-standard-2026.md         顶级标准：TT-编号需求 + 级别 + 验证方法
        │
        ▼
Gap Matrix（差距层，随任务推进实时更新）
└── gap-matrix.md                     TT 需求 → 现状 → 证据(file:line) → 关闭任务
        │
        ▼
Hardening Plan（计划层，一次性执行序列）
└── 工作区 .claude/PRPs/plans/e2ee-top-tier-hardening.plan.md
    （任务↔行业依据映射表 = 本目录 task-industry-reference.md）
        │
        ▼
Evidence Matrix（证据层，随验收实时更新）
└── evidence-matrix.md                对外声明 → 验证工件 → 复现命令 → 状态
        │
        ▼
Audit-ready Package（交付层）
└── audit-ready-package.md            审计就绪包索引（六件套+SOW+自审计报告）
```

架构决策记录：**`../v2/31-top-tier-definition-2026-revision.md`**（为什么"MLS 必须"调整为"满足安全属性即可"，待五方签字）。

---

## 各层职责与更新规则

| 层 | 回答的问题 | 更新触发 | 更新方式 |
|---|---|---|---|
| Research | 行业现在做到哪了？ | 季度复核；重大行业事件（新协议/新审计/新攻击） | 修订对应文件，标新日期 |
| Standard | 我们的"顶级"定义是什么？ | Research 层变化；PQ/MLS 等路线图项转为默认要求 | **升版本号**（2026.1→2026.2），加修订史，旧版归档不删 |
| Gap Matrix | 我们离标准差在哪？ | 每任务完成/新差距发现 | 改行状态+证据链接，**不重排结构** |
| Hardening Plan | 按什么顺序关差距？ | 一次性工件；新差距出现时**追加任务，不改写已完成部分** | 任务状态回写 v2/22 号文件 |
| Evidence Matrix | 每个声明的证据在哪、怎么复验？ | 每验收动作完成；每次发布前 | 改状态/日期/commit 锚 |
| Audit-ready Package | 第三方审计拿什么进场？ | 上述各层稳定后组装 | 索引指针更新（内容物=各层文件本身） |

**核心纪律**：行业标准演进时，改 Research → 升 Standard 版本 → 刷 Gap Matrix。
**不重写 Hardening Plan**——只按新 Gap 追加任务。这是本体系存在的理由。

## 与其他体系的关系

- **v2 ADR 系列（`../v2/00-30`）**：协议层架构决策，冻结流程不变。本体系是其上的**验收标准层**；冲突时 ADR 管"协议怎么设计"，本体系管"什么叫足够好"。
- **v2/22 执行状态文件**：任务状态唯一事实来源。本体系的 Gap/Evidence 矩阵是**标准视角**的索引，不取代 22 号文件的任务视角。
- **07-31 三仓审计"对外宣称红线"**：其七项口径问题的长期解法=Evidence Matrix 的"不可宣称"清单，售前材料以此为准。
