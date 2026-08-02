# 28 — E2EE-065/066 Key Transparency：调研与设计

> **状态**（2026-08-02 更正）：**部分已实施，KT 整体未部署**。
>
> 原状态头写「调研与设计草案（**不改任何生产代码**）」——**该表述已失真**：
> 后端 Merkle 库 `src/lib/e2ee_kt_merkle.erl` 已落地并有 `test/lib/e2ee_kt_merkle_tests.erl`；
> 客户端 `trust_event_canonical.dart` 已被 2 个生产文件引用。
> **未接线部分**：`e2ee_kt_merkle` 除自身与测试外无调用方；`trust_event_client.dart`
> 生产零引用。
>
> ⚠️ **后果**：KT 未部署，服务端分叉视图（split-view / non-inclusion）**不可检测**。
> 见 `standard/known-issues-ledger.md` IMB-2026-007、威胁模型 T11。
> **上位约束**：ADR 14 T8、ADR 16 §5/§6（transparency log，**仍为 Proposed**）、
> `20-implementation-and-acceptance-plan.md` G3「独立 monitor 已运行并演练 split view」
> **对应 playbook**：E2EE-033（日志与 proof API）、E2EE-034（客户端、gossip、monitor）
> **执笔**：Claude Code loop，会话 `20260729-0500-claude-code`，2026-07-29

---

## 1. 现状调研（已实证）

### 1.1 KT 基建：零

`src/repo/` 与 `src/logic/` 下**没有任何** merkle / transparency / KT 相关模块
（`ls | grep -i "merkle\|transparen\|kt_"` 零命中）。这一项是从零开始。

### 1.2 ⚠️ 核心缺口：身份键是**就地覆盖**，连痕迹都不留

`olm_identity_repo:upsert_identity/6`（`src/repo/olm_identity_repo.erl:46`）：

```sql
INSERT INTO olm_identity (id, user_id, device_id, ed25519_key, curve25519_key, signature)
VALUES (...)
ON CONFLICT (user_id, device_id) DO UPDATE
  SET ed25519_key = EXCLUDED.ed25519_key,
      curve25519_key = EXCLUDED.curve25519_key,
      signature = EXCLUDED.signature,
      updated_at = CURRENT_TIMESTAMP
```

**身份键被就地覆盖，历史不保留。**

这正是 KT 存在的理由的教科书场景：被攻陷的服务端替换某账号的 identity key 后，
数据库里**没有任何可供事后审计的记录**——旧值已被 `DO UPDATE` 抹掉。
客户端侧的 TOFU（`_enforceTofu`）能在**已固定过指纹的对端**上发现变化，
但对**首次建会话**的对端无能为力，且 TOFU 证据只存在于每个客户端本地，
无法跨设备、跨用户交叉验证。

**认识论状态：已实证**（SQL 逐行）。

### 1.3 `trust_audit` 是 append-only，但**不是** KT 需要的那条流

`priv/migrations/00000044_device_trust.up.sql` 的 `trust_audit`
（append-only，`bigserial` 主键，ADR 06 §8.2.2）记录的是
**「谁信任谁、何时、何方法」**——`actor_uid` 对 `target_uid/target_device_id`
的信任决策。

KT 的 leaf 需要的是**「某账号发布了哪些设备身份键」**这条目录流
（playbook E2EE-033：leaf 为 canonical **account/device/revocation** event hash）。
两者主语不同：前者是**关系**，后者是**目录**。

`trust_audit` 有 `target_ed25519`（决策时的对端身份键快照），看似能反推目录，
但它只在**有人做出信任决策时**才产生一行——从未被任何人信任过的设备
（正是攻击者最想插入的那种）**根本不会出现在这条流里**。

⚠️ 且该表标注 **「冻结项：本表结构变更须走 supersedes 流程」**，
不能直接扩展它承载 KT。

**认识论状态：已实证**（migration 注释与列定义逐行）。

### 1.4 已有两套 canonical 编码，跨语言范式成立

| 方案 | 位置 | 用途 |
|---|---|---|
| Canonical CBOR | `imboyapp/lib/service/e2ee/protected_frame_v3.dart` `CanonicalCbor` | PFv3 protected_header |
| `key=value\n` + ASCII 字典序 | Dart `trust_event_canonical.dart:112`；Erlang `e2ee_trust_logic:canonical_payload/1` | trust event 签名输入 |

第二套已经是**双语言对齐**的成品（Dart 注释逐字写着「顺序须与后端
`canonical_payload/1` 完全一致」），且带 fail-closed 守卫
（值内含 `\n`/`\r` 即拒——防编码非单射导致的签名伪造）。

**这是本项最重要的正面资产**：KT profile 需要的「canonical event bytes +
跨实现 golden vector」在本项目已有可复制的先例，不必从零发明。

**认识论状态：已实证**（两侧源码）。

---

## 2. 设计要点

### 2.1 KT 日志的 leaf 必须来自**新增的**身份发布事件流

不复用 `trust_audit`（§1.3），不改 `olm_identity`（会破坏现有读路径）。
新增 append-only 表，在 `upsert_identity` 成功后**同事务**追加一行：

```
identity_log (
  seq        bigserial PRIMARY KEY,   -- 连续位置，KT leaf index
  user_id, device_id,
  event_type,                          -- publish | rotate | revoke
  ed25519_key, curve25519_key, signature,   -- 发布时的快照
  canonical_hash bytea NOT NULL,       -- leaf hash，domain-separated
  created_at timestamptz NOT NULL
)
```

~~`seq` 用 `bigserial` 与 `trust_audit` 同范式。~~

⛔ **Slice 1 已实证：`bigserial` 不能直接充当 KT leaf index。**
（2026-07-29 真 PG 探针，见 `evidence/E2EE-065-slice1-bigserial-probe.md`，
探针模块 `test/integration/kt_seq_contiguity_probe_tests.erl`）

| 探针问题 | 实证结果 |
|---|---|
| 顺序提交时 seq 连续？ | **是**（对照组，相邻差恒为 1） |
| 回滚后序号回收？ | **否** —— 留下**永久空洞**，那一行永不出现 |
| 分配顺序 = 提交可见顺序？ | **否** —— 先取号者后提交时，按 `seq` 扫描会看到空洞，**且该空洞稍后追溯填上** |

第三条对 Merkle 日志是**致命**的，而不只是"不好看"：
读者在 t1 扫到 `[.., SeqB]`（缺 SeqA）算出 root R1；
A 提交后在 t2 扫到 `[.., SeqA, SeqB]` 算出 root R2 ≠ R1。
**同一 tree size 先后算出不同 root**——这与 §2.4 要检出的 split view **形状完全一致**，
即日志会自己制造出无法与真实攻击区分的告警。consistency proof 亦直接失效。

**据此定案（安全那一侧）**：leaf index **必须与 `bigserial` 解耦**。
append 走「先提交行、再由单一串行化 sequencer 分配 leaf index」两阶段，
sequencer 只处理**已提交可见**的行。这也顺带满足 playbook 的「唯一连续位置」。
具体机制留给 Slice 3，但**不得**再把 `bigserial` 直接当 leaf index。

### 2.2 Transparency profile 必须先冻结（playbook 第 1 步）

冻结项：hash 算法、leaf/node **domain separation**、空树值、canonical event bytes、
tree-head 签名输入、proof wire 格式、signing-key 轮换。

**建议复用 §1.4 的第二套编码**（`key=value\n` + ASCII 字典序）作为 canonical event
bytes，而不是引入第三套：
- 已双语言对齐并在生产中运行；
- 已有 fail-closed 的非单射守卫；
- 引入第三套编码 = 第三份 golden vector + 第三处跨实现漂移面。

domain separation 用前缀字节区分 leaf 与 node（RFC 6962 范式），
在 profile 里写死，不留 `TBD`。

### 2.3 signing key 不落 DB

playbook 硬性要求：签名私钥不在 DB、repo、日志、API。
本项目已有先例——E2EE 私钥从不入库（见 `evidence/E2EE-backend-audit`）。
tree-head signing key 走同一原则：环境变量 / 外部 KMS，**写权限与 log DB 写权限分离**。

### 2.4 split view 检出

同 tree size 不同 root 必须被识别，且**不能最后写覆盖**。
落点：`(tree_size, root_hash)` 唯一约束 + 冲突即告警，而不是 upsert。
——注意这正是 §1.2 那个缺陷的同构形态；不要在 KT 里重犯 `DO UPDATE`。

---

## 3. 三个必须人工 / 走 ADR 的阻塞点

这三条**不属于**「两种合理实现选安全那个」可自行裁决的范围。

### 3.1 ⛔ PFv3 携带 tree-head digest = 改协议规范

playbook E2EE-034 第 2 步要求「PFv3 内携带最近 tree-head digest 做联系人 gossip」。

PFv3 的 `protected_header` 字段集由 ADR 15 §3.3 固定，接收侧
`_validateContextBinding` 逐字段硬比对。**增加字段就是改协议规范**，
loop 明令不得做。必须走 ADR supersedes 流程 + 人工签字。

替代路径（**仍需人工确认**）：digest 走 `payload` 内而非 protected_header，
牺牲「digest 受 header 认证」的性质，换取不动协议。取舍由人工定。

### 3.2 ⛔ ADR 16 的 transparency log 部分仍为 Proposed

`16-supersedes-03-04-06-device-trust.md` 头部第 3 行：其 Accepted 是
**范围收敛豁免**，`transparency log` 与 `cross-signing` **仍为 Proposed 待五方签字**。
E2EE-065 的**实施**受此阻塞（本调研文档不受阻塞——被卡的是实施，不是调研）。

### 3.3 ⛔ 依赖链上游 E2EE-064 已 BLOCKED

`22-...state.md` §5.3：E2EE-065 依赖 E2EE-064/033，而 E2EE-064 已于
2026-07-29 判 BLOCKED（同一道签字）。

**净效果：GA-C2C 的三个硬门禁（附件 061、透明度 065/066）——
061 可实施但需人工拍板三项取舍，065/066 连实施都被签字卡住。**

---

## 4. 切片计划（实施需先解 §3 的阻塞）

| # | Slice | 仓库 | 内容 | 验收对象 |
|---|---|---|---|---|
| 1 | ~~**`bigserial` 并发空洞实证**~~ | imboy | ✅ **DONE**（2026-07-29）。结论：不能直接当 leaf index，须两阶段解耦，见 §2.1 | 真 PG 探针 3/3 绿，表测完即 DROP |
| 2 | **transparency profile 冻结** | 文档 | hash / domain separation / 空树值 / canonical bytes / tree-head 签名输入 / proof wire / 轮换。**无 `TBD`** | 安全 reviewer 接受（人工） |
| 3 | **identity_log append-only 表 + 同事务写入** | imboy | 迁移 + repo；`upsert_identity` 成功后同事务追加 | 真 PG；**正向可用性**：现有 identity 读路径不受影响 |
| 4 | ~~**Merkle 树与 proof（纯函数）**~~ | imboy | ✅ **DONE**（2026-07-30）。`src/lib/e2ee_kt_merkle.erl`，24 例已入 e2ee-verify 门禁。生成侧直译 RFC 6962 递归定义、验证侧走迭代算法，**穷举 n≤16 全部 (m,n)/(index,size) 交叉核验**（另一次性扫到 n≤64，2080+2080 组合 0 失败）。⚠️ 该方法当场抓到真 bug：`verify_consistency` 左兄弟判据漏 `orelse Node =:= Last`，**只在非平衡树上失败**（m=5,n=6 等 8 组）| profile §8 golden vector 全部钉死（§10 残留 1 关闭）；见 `evidence/E2EE-065-slice4-merkle-and-proofs.md` |
| 5 | **signed tree head + proof API** | imboy | 端点 + signing key 外置 + 写权限分离 | 篡改/删除/重排历史 leaf 后新旧 proof 不能同时通过 |
| 6 | **split view 检出** | imboy | `(tree_size, root)` 唯一约束 + 告警，**不得 upsert** | 同 size 异 root 被识别 |
| 7 | **客户端 proof verifier** | imboyapp | 保存每账号最高 tree size/root，验证 inclusion/consistency | rollback size / 错误 proof / 同 size 异 root / 过期 key 均阻断新设备信任 |
| 8 | **gossip**（⛔ 阻塞于 §3.1） | 两仓 | tree-head digest 传播 | 两客户端 split view 下一次 gossip 检出 |
| 9 | **独立 monitor** | 运维 | 独立网络比对 signed tree heads，只处理公开 hash | 人工演练告警；monitor 停止时客户端仍 fail-closed |

~~**建议起点：Slice 1**~~ —— **已完成**。
下一个可在解签字前推进的是 **Slice 2（profile 冻结，纯文档）** 与
**Slice 4（Merkle 纯函数）**；
Slice 3 及之后触及生产写路径，须先解 §3.2/§3.3。

---

## 5. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 项目中无任何 KT / Merkle 基建 | **已实证**（目录 grep 零命中） |
| 身份键 `ON CONFLICT DO UPDATE` 就地覆盖、无历史 | **已实证**（`olm_identity_repo.erl:46`） |
| `trust_audit` 是 append-only 但记录的是「谁信任谁」而非目录 | **已实证**（migration 44 列定义与注释） |
| `trust_audit` 是冻结表，结构变更须走 supersedes | **已实证**（migration 注释逐字） |
| 已存在双语言对齐的 canonical 编码可复用 | **已实证**（Dart `trust_event_canonical.dart` + Erlang `e2ee_trust_logic:canonical_payload/1`） |
| PFv3 加字段即改协议规范 | **已实证**（ADR 15 §3.3 字段集固定 + 接收侧硬比对） |
| ADR 16 transparency log 仍 Proposed | **已实证**（ADR 16 头部第 3 行逐字） |
| `bigserial` 回滚留永久空洞 | **已实证**（Slice 1 真 PG 探针） |
| **分配顺序 ≠ 提交可见顺序，空洞会追溯填上** | **已实证**（同上）——对 Merkle 日志致命，见 §2.1 |
| leaf index 必须与 `bigserial` 解耦 | **已定案**（由上述实证直接推出） |
| 本设计能让 DT-05/06/07 成立 | **设计推理，未实证** |

---

## 6. 未做

- **未实施任何生产代码**（队列第 6 项明确规定）。
- 未改 ADR / 协议规范；未代签任何 ADR；未新增迁移、依赖、配置项。
- 未改动或删除任何既有测试。
- 不 push、不部署、不访问生产、不通知第三方。
