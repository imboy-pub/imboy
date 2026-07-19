# E2EE v2 Architecture — Freeze Gate

> **用途**：B.-1 阶段收尾的**架构治理闸门**。含 ADR 完整性核查、Cross-ADR 一致性、冻结项审批、范围控制、向后兼容性验证。全部 ✅ 才算 Architecture Freeze 完成，方可进入 B.0 编码。
> **状态**：进行中
> **定位**：本文件不是普通 checklist，而是 Architecture Governance 工具——它定义进入编码阶段的**准入条件**与**签字流程**。

---

## 1. ADR 文档完整性

| # | 文档 | 状态 | 核心决策已落定 |
|---|---|---|---|
| 00 | **freeze-gate**（本文） | ✅ | 架构治理闸门、签字流程 |
| 01 | overview | ✅ | 阶段路线、与现有文档关系、冻结流程 |
| 02 | protocol | ✅ | E2eeSessionProtocol 接口、Registry、ProtocolSuite、MLS 占位 |
| 03 | device-identity | ✅ | user_device 扩展、olm_identity 表、capabilities 字段、扩展签名 |
| 04 | capability-negotiation | ✅ | 协商算法（取交集）、fallback 顺序、Signed Capabilities |
| 05 | metadata-version | ✅ | meta_version 与 e2ee_ver 解耦、双写期、字段冻结清单 |
| 06 | device-trust | ✅ | Trust State、Safety Number、CrossSigningService 接口位（不实现） |
| 07 | storage | ✅ | 客户端分级、StorageProvider 抽象、服务端零密码学、KDF 可迁移 |
| 08 | threat-model | ✅ | Protected Assets、T1-T9、可追溯矩阵（含 Test 列） |
| 09 | rejected-decisions | ✅ | R1-R8 带 revisit 触发条件 |
| 10 | adr-dependency-graph | ✅ | 依赖矩阵、变更传播路径 |
| 11 | backward-compatibility-matrix | ✅ | 客户端版本×协议套件兼容矩阵 |
| 12 | reference-implementation | ✅ | Flutter 为 normative，Web 为 derivative |

**文档总计**：13 份，全部 ✅。

---

## 2. Cross-ADR 一致性（freeze 前 patch 已完成）

| 核查项 | 状态 | 说明 |
|---|---|---|
| 协议名拼写统一（olm/megolm/mls/rsa-oaep） | ✅ 已 patch | 02 的 `'rsa'` 已统一为 `'rsa-oaep'`，与 04/e2ee_protocol.dart 一致 |
| ProtocolSuite 三元组定义唯一来源 | ✅ | 仅 02 §3 定义，其余引用 |
| 威胁 ID 引用全部在 08 T1-T9 范围内 | ✅ | 05 用 T7/T9、06 用 T2/T4/T7/T8、07 用 T1/T3/T5/T6/T9，零捏造 |
| T9 Rollback 在相关 ADR 补引 | ✅ 已 patch | 02 ProtocolSuite.version 注释补 T9；03 §4.3 防御映射补 T9 行 |
| meta_version 概念跨 02/05 一致 | ✅ | 05 定义 `meta_version`（容器版本）与 `e2ee_ver`（协议族）解耦，02 的 ProtocolSuite 与之双写 |
| StorageProvider 接口在 07 定义、12 引用 | ✅ | 07 §4 TypeScript 接口，12 §4 衍生约束 |
| Signed Capabilities 概念跨 04/08 一致 | ✅ | 04 §7 定义，08 §4 可追溯矩阵绑定 `capability_signature_forgery_fails_test` |
| CrossSigningService 接口位在 06 定义、09 reject 一致 | ✅ | 06 §6 接口位（不实现），09 R6 记录否决理由 + revisit 条件 |
| CrossSigningService 接口位在 06 定义、02 不引用为实现 | ⏳ 待核 | 06 完成后核 |
| meta_version 字段名跨 02/05 一致 | ⏳ 待核 | 05 完成后核 |
| StorageProvider 接口在 07 定义、Web SDK（B.4）复用 | ⏳ 待核 | 07 完成后核 |

---

## 3. 冻结项明确性（不可单方面变更）

每份 ADR 必须显式标注哪些是「冻结项」（变更需走 01 §5 supersedes 流程）：

| ADR | 冻结项 | 是否显式标注 |
|---|---|---|
| 02 | `E2eeSessionProtocol` 接口签名、ProtocolSuite 三元组、legacy 解析矩阵 | ✅（02 §10 标注） |
| 03 | user_device 表的 identity 相关列、olm_identity 表结构 | ⏳ 待核 |
| 04 | 协商算法（纯函数）、fallback 顺序 olm>megolm>rsa-oaep | ✅（04 明确冻结） |
| 05 | meta_version 字段、双写期规则、字段冻结清单 | ⏳ 待 05 完成 |
| 06 | Trust State 三态、Safety Number 算法、CrossSigningService 接口签名 | ⏳ 待 06 完成 |
| 07 | 服务端零密码学约束、StorageProvider 接口、客户端数据分级 | ⏳ 待 07 完成 |
| 08 | Protected Assets 表、T1-T9 定义、可追溯矩阵 | ✅ |

---

## 4. 可追溯性（每个决策可追溯到威胁 + 测试）

| 核查项 | 状态 |
|---|---|
| 每个协议套件决策追溯到威胁 | ✅（02 §10 引用 08） |
| 每个 device identity 字段追溯到威胁 | ⏳ 待 03 复核 |
| 协商算法追溯到 T2（降级攻击） | ✅（04 §7） |
| 每个防御点绑定守护测试 | ✅（08 §4 含 Test 列） |
| 明确「不防御」清单 | ✅（08 §3） |

---

## 5. 范围边界（防止 scope creep）

| 本轮做 | 本轮不做（留接口位） |
|---|---|
| ✅ Protocol Registry + Olm/Megolm/RSA 三插件 | ❌ MLS 实现（仅 Registry 占位） |
| ✅ Device Identity 表扩展 + capabilities | ❌ Cross-signing 实现（仅 CrossSigningService 接口） |
| ✅ Capability Negotiation + Signed Capabilities | ❌ verified device 自动 cross-sign 新设备 |
| ✅ Safety Number + 扫码验证 + Trust State | ❌ 量子安全签名 |
| ✅ StorageProvider 抽象 + Web 实现 | ❌ 频道/朋友圈 E2EE |
| ✅ 元数据 meta_version 双写期 | ❌ 强制全量迁移到 v2（双写期兜底） |

---

## 6. 验收命令（freeze 后可执行的自动检查）

```bash
# 1. 服务端零密码学守护
grep -rn "elib_cipher.*decrypt.*e2ee\|jsone:decode.*e2ee.*payload.*decrypt" imboy/src/ | grep -v test
# 期望：零命中

# 2. 业务层无硬编码套件字符串
grep -rn "\"OLM.V1\"\|\"MEGOLM.V1\"\|\"RSA-OAEP-256+AES-256-GCM\"" imboyapp/lib/ | grep -v "test\|kOlmSuite\|kMegolmSuite\|ProtocolSuite\|legacy"
# 期望：零命中（全部走 ProtocolSuite.parse）

# 3. 协议插件新增时业务层无需修改路由逻辑（B.0 完成后验证）
# 新增 MlsProtocol 后，E2EEService.decryptE2EEMessage 的分发逻辑不变；
# 允许 formatter / import / 注释等非逻辑改动，但 if/else 路由分支不变

# 4. 私钥永不落 DB（migration 守护）
psql -c "\d compliance_key" | grep private_key_encrypted
# 期望：column does not exist
```

---

## 7. Freeze 签字

当 §1-§6 全部 ✅ 时：
- [ ] 8 份 ADR 全部完成
- [ ] Cross-ADR 一致性 patch 完成
- [ ] 冻结项全部显式标注
- [ ] 可追溯性闭环
- [ ] 范围边界明确
- [ ] 验收命令可执行

**签字后**：方可进入 B.0 Foundation 编码。签字前任何 B.0+ 编码视为越界（已实施的线 A 除外，因其独立于 v2 架构）。
