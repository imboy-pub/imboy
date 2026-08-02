# 协议规范白皮书（Protocol Whitepaper）

> 审计就绪包**第 3 件工件**（P5-3）。协议层分析的入口。
> 最后核对：2026-08-02

## 0. 这份白皮书**不做**什么

**它不复制任何 wire 格式定义。**

规范字节留在 ADR 里作唯一真源。理由不是省事——本仓刚刚发生过一次反例：
`e2ee-key-rotation-policy.md` 把当时的协议抄了一份，协议演进后副本没跟上，
于是它把 RSA 描述成主协议、列出早已删除的 Shamir 社交恢复，
在审计场合**比没有文档更糟**（已加⛔被取代横幅）。

复制一份冻结的 wire 格式，只是在预约下一次同样的事故。

**它做的是三件 ADR 各自做不到的事**：
1. **生效状态解析**——31 份 ADR 之间有 supersedes 链，单读一份会读到已被取代的条款（§1）；
2. **协议分段导航**——按协议关注点而非文件序号索引（§2）；
3. **规范与实现的落差**——哪些"已冻结"的规范其实没上线，哪些 ADR 的状态头已经失真（§3）。

---

## 1. 生效状态解析（**先读这一节**）

⚠️ **直接读 ADR 03/04/05/06/07/08/13 会读到已被取代的条款。**

| 若你要读 | 实际生效的是 | 取代范围 |
|---|---|---|
| ADR 08 威胁模型 | ADR 08 **+ ADR 14** | 14 取代 08 的威胁边界、可追溯矩阵与发布判定 |
| ADR 05 metadata 格式 | **ADR 15**（新写入） | 15 取代 05 的**新写入** metadata 格式；旧数据读取仍按 05 |
| ADR 13 的 Olm→RSA 接收 fallback | **ADR 15** | 15 禁止未认证降级，取代 13 的 fallback 与单包退出路径 |
| ADR 15 §3.1 / §7.2 `epoch_or_counter` | **ADR 26** | 26 把该语义收敛为**仅 MLS** |
| ADR 03 设备身份写入授权 | **ADR 16** | |
| ADR 04 capability 信任来源 | **ADR 16** | |
| ADR 06「仅预留 CrossSigningService」 | **ADR 16** | 16 使 cross-signing 与 transparency log 全文生效 |
| ADR 07 的 4S / 备份格式 / PBKDF2 默认方案 | **ADR 17** | |
| ADR 07/13 合规接收方信任与 fallback 语义 | **ADR 18** | |
| ADR 02/05 未覆盖的 Megolm room key 分发密码学 | **ADR 13** | 13 是补空白，非取代 |
| RSA 的地位 | **ADR 24** | Olm-only cutover，RSA 降为**仅解密** |

### 唯一未签的取代提案

**ADR 19（supersedes 09 R3「本轮不实现 MLS」）状态仍是 `Proposed`。**
故关于 MLS 的**当前生效决定仍是 ADR 09：不实现**。ADR 02/04/11 里的 MLS 占位
按原样有效，不要按 ADR 19 来审。

### 签署状态

ADR 14–18、29、31 于 2026-08-02 由 leeyi 单人决策全签（solo）。
审计方若认为单人签署不足以构成变更控制，请作为流程 finding 提出。

---

## 2. 协议分段导航

按协议关注点索引，每段给出**权威定义所在**。数值参数见
[`crypto-inventory.md`](./crypto-inventory.md)，密钥流转见 [`../key-lifecycle.md`](../key-lifecycle.md)。

| 协议段 | 权威 ADR | 备注 |
|---|---|---|
| 总纲与 supersedes 流程 | [`../v2/01-overview.md`](../v2/01-overview.md) §5 | 变更流程本身在此 |
| 协议注册表 / 套件标识 | [`../v2/02-protocol.md`](../v2/02-protocol.md) | 接口签名冻结 |
| wire v1 / v2 | ADR 02 + [`../v2/05-metadata-version.md`](../v2/05-metadata-version.md) | v1 = RSA 套件，今日**仅解密** |
| **wire v3 / PFv3 canonical** | [`../v2/15-supersedes-05-13-authenticated-envelope.md`](../v2/15-supersedes-05-13-authenticated-envelope.md) §3 | canonical CBOR + 禁止未认证降级；`epoch_or_counter` 语义见 ADR 26 |
| **room key 分发（双包）** | [`../v2/13-room-key-over-olm.md`](../v2/13-room-key-over-olm.md) §3（Normative） | Megolm room key 经 Olm 一对一信道逐设备下发 |
| 设备身份与写入授权 | ADR 03 → **ADR 16** | |
| capability 协商 | ADR 04 → **ADR 16**（信任来源） | |
| **device trust 字段集** | [`../v2/16-supersedes-03-04-06-device-trust.md`](../v2/16-supersedes-03-04-06-device-trust.md) §3.1 / §5 | device-bound session 完整体 + cross-signing |
| 存储与 pickle | [`../v2/07-storage.md`](../v2/07-storage.md) → **ADR 17**（备份格式） | |
| **备份容器 / 恢复保险库** | [`../v2/17-supersedes-07-recovery-vault-v2.md`](../v2/17-supersedes-07-recovery-vault-v2.md) | |
| 合规接收方边界 | [`../v2/18-supersedes-07-compliance-boundary.md`](../v2/18-supersedes-07-compliance-boundary.md) | |
| **附件 descriptor 与分块 AEAD** | [`../v2/27-e2ee-061-attachment-encryption-design.md`](../v2/27-e2ee-061-attachment-encryption-design.md) | ⚠️ 状态头失真，见 §3 |
| **KT profile v1** | [`../v2/29-e2ee-065-transparency-profile-v1.md`](../v2/29-e2ee-065-transparency-profile-v1.md) | 冻结项一览 §1；树结构 §2；STH §5；proof wire §6 |
| 向后兼容矩阵 | [`../v2/11-backward-compatibility-matrix.md`](../v2/11-backward-compatibility-matrix.md) | 含 §6 不兼容场景诚实声明 |
| 已否决方案 | [`../v2/09-rejected-decisions.md`](../v2/09-rejected-decisions.md) | 读这份可省掉重复提问 |
| 威胁模型 | [`../v2/08-threat-model.md`](../v2/08-threat-model.md)（+ ADR 14） | T1–T11 |

---

## 3. 规范与实现的落差

**这一节是本白皮书对审计方最有用的部分。** 规范冻结 ≠ 已上线。

### 3.1 ADR 状态头曾经失真（2026-08-02 已修正）

本次核对发现 ADR 27/28 的状态头与实现严重不符，照原状态头读会得出
「附件加密根本没做」的**反事实**结论。**两处已当场改正**，此处保留记录：

| ADR | 原状态头 | 实际（已写入更正后的状态头） |
|---|---|---|
| **27**（附件加密） | 「设计草案（**不实施**）……**本文件不改动任何生产代码**」 | Slice 1–8 已接入生产：`attachment_encryptor` / `seal_policy` / `chunk_codec` / `binding` / `temp_hygiene` 各有 2–5 个 `lib/` 生产引用，13 个附件测试文件。仅 **Slice 9 运行时开关**未翻 |
| **28**（KT 调研） | 「调研与设计草案（**不改任何生产代码**）」 | 后端 `src/lib/e2ee_kt_merkle.erl` 已落地并有测试；客户端 `trust_event_canonical.dart` 有 2 个生产引用。未接线：`e2ee_kt_merkle` 无调用方、`trust_event_client.dart` 生产零引用 |

**教训**：以「不改动任何生产代码」措辞冻结的设计文档，一旦后续实施就必然失真，
而没人会回头改状态头。审计前应把这类措辞逐份复核一遍。

### 3.2 冻结但未上线

| 规范 | 状态 | 台账 |
|---|---|---|
| 附件加密（ADR 27） | 实现完成，**开关未翻开** → 附件今天明文存于对象存储 | IMB-2026-005 |
| KT profile v1（ADR 29） | 规范冻结、golden vectors 已核验，**未部署** | IMB-2026-007 |
| Cross-signing（ADR 16 §5） | 算法在，**生产零接线** | IMB-2026-006 |

完整披露见[已知问题台账](./known-issues-ledger.md)，尤其 §2「名义防御与运行时不符」。

---

## 4. Golden 向量位置

⚠️ **无独立向量文件**，向量以内联形式散在 ADR 与测试中。这对第三方复核不友好，已如实记入证据清单。

| 向量 | 位置 |
|---|---|
| KT 树 / 事件 / STH | ADR 29 §8.1（基础）、§8.2（事件与树） |
| fallback key canonical | 两端各自钉死同一条（含长度 82），见 E2EE-062 evidence |
| PFv3 canonical CBOR | `imboyapp/test/service/e2ee/protected_frame_v3_test.dart` |

---

## 5. 修订史

| 日期 | 变更 |
|---|---|
| 2026-07-19 | ADR 13 冻结签字 |
| 2026-07-28 | ADR 26 签字（`epoch_or_counter` 收敛为仅 MLS） |
| 2026-08-02 | ADR 14–18、29、31 由 leeyi solo 全签；ADR 19 维持 `Proposed` |
| 2026-08-02 | 本白皮书建立；威胁模型补 T10/T11；密码学清单、密钥生命周期、已知问题台账、SOW、证据清单先后建立 |

> 更细的变更走 git log。ADR 自身的变更流程见 `01-overview` §5 supersedes 流程——
> **本白皮书不是变更控制点**，它只解析当前状态；改协议仍走 ADR。
