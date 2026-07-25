# E2EE v2 Architecture — Overview

> **状态**：Architecture Freeze（B.-1 阶段产出）
> **冻结日期**：2026-07-18
> **目标周期**：未来 5–10 年可演进
> **关联**：本目录为 v2 架构冻结文档；v1 实现见 `docs/archive/analysis/e2ee-*.md`（11 份历史文档）

---

## 1. 为什么需要 v2

v1（当前实现）是「一次性工程升级」视角：
- OlmSessionService 被写成了实现而非接口；
- 单聊套件字符串硬编码（`"OLM.V1"`）；
- 无 Threat Model、无 Capability Negotiation、无 Device Identity 抽象；
- 服务端 Olm prekey 端点存在事务一致性风险（5 个独立端点）。

v2 的定位是**下一代 E2EE 架构**，目标：
1. 协议可插拔（Olm/Megolm/MLS 共存，业务层零改动）；
2. 设备身份与信任是一等公民（Trust / Verify / Safety Number）；
3. 元数据版本化，未来加字段不破坏存量；
4. 攻击者模型显式化，每个防御点可追溯到威胁。

**核心判断**：Olm 不是架构，只是第一个协议插件。架构是 Protocol Registry + Device Identity + Capability Negotiation。

---

## 2. 文档结构（8 份 ADR）

| # | 文档 | 决策范围 | 冻结后是否可独立演进 |
|---|---|---|---|
| 01 | **overview**（本文） | 总纲、与其他文档关系、阶段路线 | — |
| 02 | **protocol** | Protocol Registry、`E2eeSessionProtocol` 接口、套件注册机制 | 否（接口冻结） |
| 03 | **device-identity** | Device 数据模型（identity key / signing key / capabilities / trust） | 否（表结构冻结） |
| 04 | **capability-negotiation** | 双端如何协商共同支持的协议套件 | 否（协商算法冻结） |
| 05 | **metadata-version** | E2EE 元数据结构、版本演进策略 | 是（向后兼容约束下） |
| 06 | **device-trust** | Trust State / Verification / Safety Number / Cross-signing 接口 | Cross-signing 实现可后补 |
| 07 | **storage** | 客户端私钥/pickle 落盘、服务端零密码学约束、Web StorageProvider 抽象 | 是 |
| 08 | **threat-model** | 攻击者分类、每个防御点对应的威胁、不防御的明确声明 | 是（威胁随业务演进） |

---

## 3. 阶段路线（修订版，5+ 周）

```
B.-1  Architecture Freeze（8 份 ADR）                ← 当前阶段
  ↓
B.0   Foundation 编码（Protocol Registry / Device Identity / Capability）
  ↓
B.1   OlmProtocol 实现（作为 E2eeSessionProtocol 第一个插件）
  ↓
B.2   Flutter 三套件路由（RSA/Megolm/Olm 共存）
  ↓
B.3   后端统一 Device API + Batch Claim + OTK 生命周期 + Device Trust 表
  ↓
B.5   集成测试矩阵 + 灰度发布 + 回归
  ↓
B.4   Web SDK（vodozemac-js + StorageProvider）
```

**不在 v2 范围**（明确排除，留接口位）：
- ❌ MLS 实现（仅 Registry 注册占位）
- ❌ Cross-signing 实现（仅 `CrossSigningService` 接口位）
- ❌ 频道 / 朋友圈 E2EE（非 IM 消息范畴）

---

## 4. 与现有文档的关系

| 现有文档 | v2 中的角色 |
|---|---|
| `docs/compliance/e2ee-policy.md` | 线 A 已更新，v2 继承其零信任契约 |
| （原 `docs/archive/analysis/e2ee-zero-trust-redesign-plan.md`，内容已并入本 v2 规范，2026-07-25 删除） | transfer/social 改造先例，v2 遵循同模式 |
| （原 `docs/archive/analysis/e2ee-backend-audit-final.md`，审计结论已并入 v2 规范，2026-07-25 删除） | 审计基线，v2 的 Threat Model 与之对齐 |
| （原 `docs/archive/analysis/e2ee-key-backup-implementation-plan.md`，4S 方案已并入本 v2 规范，2026-07-25 删除） | 4S 备份方案，v2 Storage ADR 引用 |
| `docs/guides/e2ee/e2ee-key-rotation-policy.md` | 轮换策略，v2 Device Identity ADR 引用 |
| `docs/archive/analysis/e2ee-cross-device-recovery-assessment-2026-06.md` | 跨设备恢复评估，v2 Device Trust ADR 引用 |

**冲突处理原则**：v2 ADR 与现有文档冲突时，**以 v2 为准**；现有文档需在 v2 freeze 后同步标注「已被 v2 supersedes」。

---

## 5. 冻结后的变更流程

ADR freeze 后**不是不可变**，但变更必须走流程：
1. 新建 `NN-supersedes-XX.md`（如 `09-supersedes-03.md`），声明替代哪份 ADR 及原因；
2. 在被替代的 ADR 顶部加 `> ⚠️ Superseded by 09-xxx` 标注；
3. 重大架构变更（接口/表结构/威胁模型）需人工 review 签字。

**不可单方面变更的冻结项**（任何 PR 不得绕过）：
- `E2eeSessionProtocol` 接口签名（02）；
- `user_device` 表的 identity 相关列（03）；
- Capability Negotiation 算法（04）；
- e2ee 元数据的 `e2ee_ver` 字段语义（05）。

---

## 6. 验收标准

本架构在以下条件满足时视为「落地」：
1. `grep "OLM.V1\|MEGOLM.V1\|RSA-OAEP-256+AES-256-GCM"` 在业务层零硬编码（全部走 `ProtocolSuite.parse()`）；
2. 新增 MLS 协议插件时，**业务层无需修改协议路由逻辑**（仅 Registry 注册新实现，`E2EEService.decryptE2EEMessage` 的分发逻辑不变）；
3. Threat Model 中列出的每个「需防御」威胁，都有对应的代码守护测试；
4. 设备信任状态变更（verified/unverified/revoked）有完整的审计日志。
