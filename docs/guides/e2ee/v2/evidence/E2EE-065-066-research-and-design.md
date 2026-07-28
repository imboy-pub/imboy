# E2EE-065/066 Key Transparency：调研与设计（不实施）

- **Slice**：`22-...state.md` §1.1 队列**第 6 项**（队列末项）
- **会话**：`20260729-0500-claude-code`
- **仓库**：`imboy`（文档）
- **状态**：本刀完成；**E2EE-065/066 仍为 `PENDING`**（队列规定只出调研与设计）
- **交付物**：[`28-e2ee-065-066-key-transparency-research.md`](../28-e2ee-065-066-key-transparency-research.md)

---

## 1. 做了什么

产出 Key Transparency 的调研与九刀切片计划。**未实施任何生产代码。**

与上一刀（E2EE-061 设计）同样的方法：**先把现状读实，再对齐 playbook**，
而不是从 ADR 抄写一份通用 KT 方案。

---

## 2. 本轮最重要的实证发现

### 2.1 ⚠️ 身份键是**就地覆盖**，被替换后连痕迹都不留

`src/repo/olm_identity_repo.erl:46` 的 `upsert_identity/6`：

```sql
ON CONFLICT (user_id, device_id) DO UPDATE
  SET ed25519_key = EXCLUDED.ed25519_key, ...
```

这正是 KT 存在理由的教科书场景：**被攻陷的服务端替换某账号的 identity key 后，
数据库里没有任何可供事后审计的记录**——旧值已被 `DO UPDATE` 抹掉。

客户端 TOFU（`_enforceTofu`）能在**已固定过指纹的对端**上发现变化，但：
- 对**首次建会话**的对端无能为力；
- TOFU 证据只存在于每个客户端本地，无法跨设备 / 跨用户交叉验证。

**认识论状态：已实证**（SQL 逐行）。

### 2.2 `trust_audit` 看似可复用，实则不是 KT 需要的那条流

`trust_audit` 确实是 append-only，也确实带 `target_ed25519` 身份键快照。
但它记录的是**「谁信任谁」**（关系），KT 需要的是
**「某账号发布了哪些设备身份键」**（目录）。

关键差别：`trust_audit` 只在**有人做出信任决策时**才产生一行——
**从未被任何人信任过的设备**（正是攻击者最想插入的那种）根本不会出现在这条流里。

且该表标注 **「冻结项：本表结构变更须走 supersedes 流程」**，不能直接扩展。

若不做这层核实、只看到「已有 append-only 表且带身份键快照」，
很容易得出「复用 trust_audit 即可」的错误设计。**认识论状态：已实证。**

### 2.3 正面资产：双语言对齐的 canonical 编码已经存在

`trust_event_canonical.dart:112` 与 `e2ee_trust_logic:canonical_payload/1`
是一套**已在生产中运行、双语言逐字对齐**的 `key=value\n` + ASCII 字典序编码，
且带 fail-closed 守卫（值内含 `\n`/`\r` 即拒——防编码非单射导致的签名伪造）。

playbook 要求 KT profile 具备「canonical event bytes + 跨实现 golden vector」，
本项目**已有可复制的先例**，不必从零发明。
设计据此建议**复用该编码而非引入第三套**（项目已有 CanonicalCbor 与它两套），
理由是第三套 = 第三份 golden vector + 第三处跨实现漂移面。

### 2.4 一个设计内部冲突被提前发现

playbook 要求「并发 append 1000 events 得到**唯一连续**位置」。
而与 `trust_audit` 同范式的 `bigserial` 在事务回滚后**不回收序号**，会产生空洞。

两者冲突。**认识论状态：`bigserial` 空洞行为为通用知识，本项目未实证**——
已列为 Slice 1 的全部内容，要求在真 PG 上验证后再定 Slice 3 的形状，
不得凭 PostgreSQL 通例推断。

---

## 3. 三个阻塞点（均已实证，均需人工）

| # | 阻塞 | 依据 |
|---|---|---|
| 1 | **PFv3 携带 tree-head digest = 改协议规范** | playbook E2EE-034 第 2 步要求把 digest 放进 PFv3；而 PFv3 字段集由 ADR 15 §3.3 固定、接收侧硬比对。**loop 明令不得改协议**。替代路径（放 payload 内）牺牲「受 header 认证」性质，取舍需人工定 |
| 2 | **ADR 16 的 transparency log 部分仍 Proposed** | ADR 16 头部第 3 行：Accepted 是范围收敛豁免，transparency log 与 cross-signing 仍待五方签字。阻塞**实施**，不阻塞本调研 |
| 3 | **上游 E2EE-064 已 BLOCKED** | 同一道签字（上一轮已记录） |

**净效果：GA-C2C 的三个硬门禁——061（附件）可实施但需人工拍板三项取舍；
065/066（透明度）连实施都被签字卡住。**

---

## 4. RED 记录

**不适用。** 本刀交付物是文档，无可复现的行为缺陷。

与上一刀同样处置：用**事实核实**替代 RED，设计文档 §5 与本文件 §6 的认识论状态表
逐条标注「已实证 / 未实证 / 设计推理」，标「已实证」的均可由文中给出的
文件名与行号复核。

---

## 5. 验收命令与结果

本刀不改生产代码，按「改哪侧跑哪侧」原则，两侧验收命令均不适用。
已核实两仓无生产代码漂移：

```
$ cd imboy    && git status --porcelain   # 仅本刀新增的两个 md + 22 状态文件
$ cd imboyapp && git status --porcelain   # 空
```

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 项目中无任何 KT / Merkle 基建 | **已实证**（目录 grep 零命中） |
| 身份键就地覆盖、无历史 | **已实证**（`olm_identity_repo.erl:46`） |
| `trust_audit` 记录的是关系而非目录 | **已实证**（migration 44 列定义与注释） |
| `trust_audit` 是冻结表 | **已实证**（migration 注释逐字） |
| 已存在双语言对齐的 canonical 编码可复用 | **已实证**（Dart + Erlang 两侧） |
| PFv3 加字段即改协议规范 | **已实证**（ADR 15 §3.3 + 接收侧硬比对） |
| ADR 16 transparency log 仍 Proposed | **已实证**（ADR 16 头部第 3 行） |
| `bigserial` 空洞与「唯一连续位置」冲突 | **未实证** —— Slice 1 |
| 本设计能让 DT-05/06/07 成立 | **设计推理，未实证** |

---

## 7. 残留风险

1. **队列内可自动推进项至此穷尽。** 第 1/2 项 DONE、第 3 项 PARTIAL（残留需 UX /
   运维 / 真机）、第 4 项 BLOCKED、第 5/6 项设计阶段已完成而实施需人工。
   **loop 无法在不越界的情况下继续推进 E2EE 主线。**
2. Slice 1 的问题未答（§2.4），且它同时是 065 与 061 两条线的起点模式
   （都以「一条纯实证零改动的刀」开局）。
3. 「本设计能让 DT-05/06/07 成立」是**设计推理**，需按九刀逐刀验收。
4. §3 的三个阻塞点全部需要人工：协议变更决定、ADR 16 五方签字。
5. E2EE-062 既有残留不变（见其各 evidence §5）。

---

## 8. 未做

- **未实施任何生产代码**（队列第 6 项明确规定）。
- 未改 ADR / 协议规范；未代签任何 ADR；未动 E2EE-012/023/024/025/029 状态标记。
- 未新增迁移、依赖、配置项；未删除或 skip 任何测试。
- 不 push、不部署、不访问生产、不通知第三方。
