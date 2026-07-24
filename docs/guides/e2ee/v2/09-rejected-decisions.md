# ADR 09 — Rejected Decisions

> **状态**：Architecture Freeze
> **用途**：记录评估过但未采用的方案，避免半年后重复讨论同一建议。每条 Rejected Decision 带 **revisit 触发条件**——满足条件时应重新评估，避免本文件变成教条。
> **维护原则**：提出新建议时，先查本文；若已被 reject 且 revisit 条件未触发，引用本 ADR 关闭讨论。若 revisit 条件已触发，新建 superseding ADR。

---

## 1. 为什么需要这份文档

ADR 的常见失败模式：**没有记录 rejected alternatives**。结果是：
- 半年后有人提"为什么不试试 Signal Protocol / libolm / Argon2id / MLS？"；
- 团队重新讨论一遍，得出相同结论；
- 重复消耗 review 周期，且新成员无法理解决策背景。

本文为每个被否决的方案记录：**是什么 / 为什么否决 / 什么情况下重新评估**。

---

## 2. Rejected Alternatives 清单

### R1 — Signal Protocol（libsignal_protocol_dart）

**方案**：用 Signal 官方的 `libsignal_protocol_dart`（纯 Dart Signal 协议实现）替代 Olm/Megolm。

**为什么评估**：Signal Protocol 是 E2EE IM 的事实标准，X3DH + Double Ratchet 最成熟的实现。

**为什么否决**：
- IMBoy 已锁定 vodozemac 0.5.0（同时提供 Olm + Megolm，群聊已用），引入第二套密码学库增加维护成本；
- vodozemac 的 Olm 实现与 Signal Protocol 的 Double Ratchet 在协议层等价（X3DH + DR），安全属性一致；
- 群聊场景 Megolm 是 Matrix 生态成熟方案，Signal 的 Sender Keys 在 IM 场景经验更少；
- `libsignal_protocol_dart` 维护活跃度低于 vodozemac（Matrix 基金会主导）。

**revisit 触发条件**：
- vodozemac 停止维护或被 Matrix 弃用；
- 或 Signal Protocol 出现 vodozemac 不具备的关键安全特性（如后量子签名）。

---

### R2 — libolm（@matrix-org/olm WASM，Web 端）

**方案**：Web SDK 用 `@matrix-org/olm`（libolm 的 WASM 绑定）。

**为什么评估**：生态成熟、文档全、Matrix 自己用了多年。

**为什么否决**：
- Matrix 官方已将 libolm 标记为维护状态，正在迁往 vodozemac；
- 引入一个正在被弃用的依赖是技术债；
- vodozemac-js 是同一生态的继任者，迁移路径清晰。

**revisit 触发条件**：
- vodozemac-js 在浏览器环境的成熟度或包体未达可用门槛（实际验证后若不达标，可临时用 libolm 过渡，但须标注「临时」）。

---

### R3 — MLS（Message Layer Security，本轮实现）

**方案**：本轮直接实现 MLS 替代 Olm/Megolm。

**为什么评估**：MLS 是 IETF RFC 9420，行业长期方向（Signal/Google/Cisco/IETF 都在推）。

**为什么否决**：
- MLS 实现复杂度远高于 Olm（group state machine、tree math、welcome 流程）；
- 本轮目标是「v1 E2EE 完备化 + 协议可插拔架构」，不是「一次性切 MLS」；
- Protocol Registry（ADR 02）已为 MLS 预留接口位，未来切换业务层零改动；
- 工程量评估：完整 MLS 实现至少 +1 个月。

**revisit 触发条件**（任一满足）：
- Olm/Megolm 出现无法修复的协议级漏洞；
- 或 MLS 的 Dart/JS 实现成熟（有生产级 `mls-rs` 或同等 FFI 绑定可用）；
- 或业务需求出现 MLS 特有能力（如大型群的高效成员变更，MLS 的 tree 结构优于 Megolm 的全量 rotate）。

**当前状态**：Registry 占位 `ProtocolSuite.mls = ('mls', 0, 'reserved')`，不创建实现文件。

---

### R4 — Server-side Key Escrow（服务端密钥托管）

**方案**：保留 compliance 模式的服务端私钥托管（线 A 改造前的状态）。

**为什么评估**：简化合规审计（管理员可在服务端直接解密），部分企业客户期望。

**为什么否决**：
- 破坏纯端到端语义，与"零信任"宣传矛盾；
- 运营方/管理员可解密所有消息，威胁 T1（Curious Operator）无法防御；
- 线 A 已改造为「合规私钥仅审计方本地持有」，审计能力保留但服务端零密码学。

**revisit 触发条件**：
- 出现明确的合规法规**强制要求**服务端持有解密能力（非"方便"而是"违法不持有"）；
- 此情况下应作为独立的 compliance-escrow-mode（与 strict_e2ee 并列），而非默认行为。

---

### R5 — Argon2id（本轮替换 PBKDF2）

**方案**：备份加密直接用 Argon2id 替代 PBKDF2-HMAC-SHA256。

**为什么评估**：Argon2id 是密码哈希竞赛优胜者，抗 GPU/ASIC 暴力破解优于 PBKDF2。

**为什么否决**：
- vodozemac 的 Dart 端、Erlang 的 `crypto` 模块、WebCrypto 对 Argon2id 的原生支持不一致（尤其 WebCrypto 不内置）；
- 当前 PBKDF2 310k 迭代在通用硬件上仍达安全水位；
- 备份格式已含 `kdf_version` 字段，未来可平滑迁移。

**revisit 触发条件**：
- PBKDF2 的安全迭代次数需 > 1,000,000 次（硬件加速使 310k 不再充分，性能代价超 Argon2id）；
- 或 WebCrypto / Dart / Erlang 三端出现统一的 Argon2id 标准接口；
- 此情况下走 ADR 07 §7 的 KDF 可迁移路径升级。

---

### R6 — Cross-signing（本轮实现）

**方案**：本轮实现完整的 Cross-signing（Master Key + Self Signing Key + User Signing Key 三层）。

**为什么评估**：Matrix 的 cross-signing 是多设备信任的最佳实践，verified device 自动 cross-sign 新设备。

**为什么否决**：
- 三层密钥的生命周期管理复杂（生成、保护、恢复、吊销）；
- 本轮做到 Trust State + Safety Number + 扫码验证已覆盖 T2/T8 的核心防御；
- CrossSigningService 接口位在 ADR 06 预留，未来实现时业务层零改动。

**revisit 触发条件**：
- 用户反馈多设备验证流程太繁琐（手动逐设备扫码）；
- 或业务出现「自动信任同用户新设备」的强需求；
- 此情况下按 ADR 06 §6 的接口位落地。

---

### R7 — 前量子/后量子签名（PQC）

**方案**：引入后量子签名算法（如 Dilithium/Kyber）应对量子计算威胁。

**为什么评估**：量子计算发展使 RSA/ECC 长期可能不安全（"harvest now, decrypt later"）。

**为什么否决**：
- 后量子算法标准化仍在演进（NIST PQC 标准化近期才落地）；
- 各平台原生支持缺失；
- IM 场景的"前向保密"已大幅降低量子威胁的实际影响（捕获的密文依赖会话密钥，而非长期身份）。

**revisit 触发条件**：
- NIST PQC 标准在 IM/E2EE 场景有成熟实现与部署先例；
- 或出现实用化量子计算突破（capture-now-decrypt-later 成为现实威胁）；
- 此情况下应在 MLS 迁移时一并考虑（MLS 协议层支持 PQC 混合模式）。

---

### R8 — 全量迁移到 v2 metadata（强制）

**方案**：冻结后强制所有客户端立即只写 v2 metadata（`meta_version` + `ProtocolSuite` 三元组），停止双写。

**为什么评估**：双写期增加消息体积、客户端逻辑复杂。

**为什么否决**：
- 生产环境有大量存量客户端（旧版本），强制 v2 会导致它们无法解密新消息；
- 双写期是向后兼容的标准做法（ADR 05 定义）。

**revisit 触发条件**：
- 客户端升级率达 99%+（通过遥测确认）；
- 或双写期的消息体积成为实际性能瓶颈；
- 此情况下按 ADR 05 的退出条件结束双写。

---

## 3. Rejected Decisions 的维护流程

1. **新建议提出时**：先查本文，若已被 reject：
   - 检查 revisit 触发条件是否已满足；
   - 未满足 → 引用本 ADR 关闭讨论，避免重复 review；
   - 已满足 → 新建 superseding ADR（如 `13-supersedes-09-r3-mls.md`）。
2. **新增 rejected decision**：评估新方案后若否决，追加到本文 §2，必填「为什么否决」+「revisit 触发条件」。
3. **定期 review**：每年一次扫一遍本文件，检查是否有 revisit 条件已触发但未处理。

---

## 4. 与其他 ADR 的关系

- **01-overview**：本 ADR 是 freeze gate 的输入（00-freeze-gate §1 完整性核查）；
- **02-protocol**：R1（Signal）、R3（MLS）直接关联协议选型；
- **05-metadata**：R8（强制 v2）关联双写期；
- **06-device-trust**：R6（Cross-signing）关联信任模型；
- **07-storage**：R4（Escrow）、R5（Argon2id）关联存储与 KDF；
- **08-threat-model**：每条 rejected decision 的否决理由应可追溯到威胁或工程约束。
