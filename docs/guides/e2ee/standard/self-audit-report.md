# 自审计报告（Self-Audit Report）

> 审计就绪包**附录 A**（P5-7）。按 [`top-tier-standard-2026.md`](./top-tier-standard-2026.md) 32 条逐条核验。
> 核验日期：2026-08-02 ｜ **签字：见 §5（未签）**

## 0. 这份报告的证据效力（先说清楚）

**这是自审计，审计者就是实现者。** 它能提供的是「我们自己知道哪里不达标」的完整披露，
**不能**替代第三方审计，也不构成合规证明。

它的唯一价值在于：如果第三方审计后来发现的问题，本报告里一条都没提，
那说明我们的自查能力有问题；如果发现的问题本报告大都已列，说明披露是诚实的。
**请把本报告当作可被证伪的声明，而不是结论。**

## 1. 范围与版本锚

双仓 commit、依赖哈希、测试计数见
[`evidence-manifest.generated.md`](./evidence-manifest.generated.md)（机器生成，含工作树脏否）。
协议生效状态见 [`protocol-whitepaper.md`](./protocol-whitepaper.md) §1。

---

## 2. 标准符合性总表

32 条 = **26 条 MUST + 6 条 SHOULD**。逐条状态取自
[`gap-matrix.md`](./gap-matrix.md)（该表按条款组织，本节按合规等级重排）。

### 2.1 结论摘要

| MUST（26 条） | 数量 |
|---|---|
| ✅ 达成**且有证据** | **6** |
| ⚠️ 声称达成但**未取证** | **2** |
| 🟡 部分达成 | **16** |
| ❌ 未达成 | **2** |

| SHOULD（6 条） | 数量 |
|---|---|
| 🟡 部分 | 5 |
| ❌ 未达 | 1 |

> **26 条 MUST 里只有 6 条达成且有证据。** 这是本报告最重要的一个数字，
> 不加修饰地放在最前面。项目当前处于「实现大体完成、验证严重滞后」的状态。

### 2.2 MUST 未达成（2 条）

| 条款 | 问题 | 台账 |
|---|---|---|
| TT-B1 | 交叉签名地基**生产零接线**；`safety_number.dart` 生产零调用 | IMB-2026-006 |
| TT-C2 | **零跨进程双端测试**——今天只有单进程双 Account round-trip | IMB-2026-022 |

### 2.3 MUST 声称达成但未取证（2 条）

| 条款 | 说明 |
|---|---|
| TT-A6 | gap-matrix 标「✅待证」——**待证不是达成**。本报告不把它计入达成 |
| TT-B2 | 同上 |

> 单列这一类，是因为把「待证」混入「达成」是自审计最容易犯的自欺。

### 2.4 MUST 达成且有证据（6 条）

| 条款 | 证据 |
|---|---|
| TT-D1 | [`protocol-whitepaper.md`](./protocol-whitepaper.md)（含此前不存在的 supersedes 生效解析表） |
| TT-D2 | [`../v2/08-threat-model.md`](../v2/08-threat-model.md)（T1–T11，含 T10 附件 / T11 分叉视图） |
| TT-D3 | [`crypto-inventory.md`](./crypto-inventory.md) + [`../../../legal/third-party-licenses.md`](../../../legal/third-party-licenses.md)（143 条，机器可查） |
| TT-D4 | [`../key-lifecycle.md`](../key-lifecycle.md) |
| TT-D6 | [`known-issues-ledger.md`](./known-issues-ledger.md)（IMB-2026-001..029） |
| TT-E4 | 设备吊销级联清 Olm 材料（`olm_identity_repo:delete_by_device/2`），测试含顺序断言与删除维度断言 |

> ⚠️ 这 6 条**全部是 D 类（文档）与 E4**。换句话说：本轮达标的是**可审计性**，
> 不是密码学能力本身。这个分布本身就是结论——文档层已经补齐，验证层还没开始。

### 2.5 MUST 部分达成（16 条）

A1 A2 A3 A4 · B3 B4 B6 · C1 C3 C5 C6 C7 · D7 · E1 E3 E5

主要共性阻塞：**真机从未验证**（A1、C 类多条）、**KT 未部署**（B3、B6、E3、E5）、
**测试基础设施缺跨进程能力**（C1、C3、C5）。逐条现状见 gap-matrix。

### 2.6 SHOULD（6 条）

| 条款 | 状态 |
|---|---|
| TT-A5 | 🟡 依赖库已审计；本地 pickle 层未核 |
| TT-B5 | 🟡 策略文档已建（`device-verification-policy.md`），**选型待拍板**；实施依赖 Safety Number 接线（IMB-2026-006） |
| TT-C4 | ❌ 无进程/网络操纵能力 |
| TT-D5 | 🟡 就绪包**八件全部到位**；本报告 §5 **签字栏留空**故计部分 |
| TT-E2 | 🟡 PBKDF2-310k + 端点限流；无 HSM/OPAQUE；XFF 限流根基被推翻 |
| TT-E6 | 🟡 SECURITY.md 待审 |

---

## 3. 攻击面复核

对照 [`../v2/08-threat-model.md`](../v2/08-threat-model.md) T1–T11 逐条给防御与证据。

| 威胁 | 判定 | 关键证据 / 缺口 |
|---|---|---|
| T1 Curious Operator | ✅ | 服务端零密码学；olm 三表无私钥列 |
| T2 Compromised Server | ⚠️ 部分 | Ed25519 身份签名、signed capabilities 在；**Safety Number 生产零调用**（006） |
| T3 Compromised DBA | ✅ | 私钥永不落 DB，`private_key_encrypted` 列已 DROP 且有守护测试 |
| T4 Network MITM | ✅ | AEAD + 身份签名 + PFv3 canonical |
| T5 Compromised Device | ⚠️ 部分 | PFS/PCS 由 Olm 提供；设备攻陷后实时解密是 E2EE 根本限制 |
| T6 Stolen Backup | ✅ | PBKDF2-310k + KDF 可迁移；无 HSM 属已接受弱点（010） |
| T7 Malicious Client | ✅ | OTK 原子 claim、重放/乱序拒绝、room key 域一致性校验 |
| T8 Social Engineer | ⚠️ 部分 | 依赖 Safety Number，而它**零接线**（006） |
| T9 Rollback | ⚠️ 部分 | 版本单调 + session 单调；TOFU 首见窗口无解 |
| **T10 Object-Store** | ❌ | 加密实现全在、13 个测试在盘，**开关未翻**→附件明文存储（005） |
| **T11 Key-Server Equivocation** | ❌ | KT 未部署（007）+ Safety Number 零调用（006）→**今天无任何检测手段** |

> T10/T11 两条 ❌ 与 T2/T8 的"部分"，根因收敛到同两件事：
> **附件开关没翻、验证 UI 没接线**。

---

## 4. 发现清单

不在此重复列举。完整披露见 [`known-issues-ledger.md`](./known-issues-ledger.md)：
`IMB-2026-001..029`，三态 `Acknowledged` / `Open` / `Blocked`，各带理由与负责人。

分布：分发阻断 4 条（Critical）· 名义防御与运行时不符 3 条（High）·
有意接受的取舍 7 条 · 已知未决 8 条 · 验证与流程缺口 7 条。

**本报告不新增 finding 编号**——自审计的发现就是台账本身，两处记录会立刻分叉。

---

## 5. 签字

> 🔒 **本报告未签字。**
>
> 签署是对"以上披露完整且属实"承担责任的动作，须由安全负责人本人做出，
> 不由自动化流程代签。

| 项 | 值 |
|---|---|
| 安全负责人 | `[ 待签 ]` |
| 签署日期 | `[ 待签 ]` |
| 签署时报告对应的双仓 commit | `[ 签署时从 evidence-manifest 抄入，避免签一份已漂移的报告 ]` |

签字前建议先确认三件事：
1. §2.1 那个「26 条 MUST 只有 6 条达成且有证据」的数字，你认不认这个口径；
2. §2.3 把「✅待证」排除在达成之外，你认不认；
3. 台账 §1 的四条分发阻断项，在签字之日是否仍然全部未解。
