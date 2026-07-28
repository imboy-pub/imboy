# ADR 26 — Supersedes ADR 15 §3.1 / §7.2：`epoch_or_counter` 语义收敛为仅 MLS

> **状态**：Accepted —— 人工签字 2026-07-28（提案 25 §7 第 1、2 项）
> **取代**：[`15-supersedes-05-13-authenticated-envelope.md`](./15-supersedes-05-13-authenticated-envelope.md) §3.1 中 `epoch_or_counter` 行的约束与语义、§7 第 2 条
> **不改动**：ADR 15 其余全部条款；`protected_header` 的字段名、CBOR 类型与字段顺序**保持冻结**（wire 格式不变）
> **决策依据**：[`25-proposal-replay-counter-semantics.md`](./25-proposal-replay-counter-semantics.md) 选项 C
> **实证**：[`evidence/E2EE-025-production-wiring-finding.md`](./evidence/E2EE-025-production-wiring-finding.md)

---

## 1. 背景

ADR 15 §3.1 把 `epoch_or_counter` 定义为「uint，单调且有界」，§7 第 2 条要求
「`epoch_or_counter` 维护滑动窗口」。这两条在 Olm/Megolm 场景下经实证不可落地：

| # | 事实 | 后果 |
|---|---|---|
| P1 | 发送侧从无 counter 递增逻辑，恒填 0 | 「单调」约束在生产上从未被满足 |
| P2 | 接收侧实现为**严格单调**（`sequence <= lastSeq → 拒绝`），而 counter 恒 0 | `0 <= 0` 成立 → **首条合法消息即被判 replay** |
| P3 | 严格单调 ≠ 滑动窗口 | 离线批量投递与 WS 重连乱序是 IMBoy 常态，严格单调必然误杀真实消息 |

同时，重放防护在生产上**已由另外两道机制承担且已接线生效**：

1. `message_id` dedupe（ADR 15 §7 第 1 条）：`message_id` 位于**受认证的**
   `protected_header` 内，篡改它会破坏 `header_hash` 与 inner/outer 比对，
   因此攻击者无法用「换个 id 重放同一密文」绕过；
2. Olm Double Ratchet 自身：message key 用后即毁，重放同一密文必然解密失败。

在此之上再叠一套应用层计数器属重复机制，其**可用性风险高于其安全收益**。

---

## 2. 决策

### 2.1 `session_ref`（澄清，非变更）

维持 ADR 15 §3.1 原约束不变：`text, 1..256 字节`，即**必填非空**，
填**实际协议会话标识**（Olm session id / Megolm session id / MLS group ref）。

此前生产写空串属**实现违规**，不是协议问题。修复见 §4。

### 2.2 `epoch_or_counter`（变更）

ADR 15 §3.1 该行的约束与语义替换为：

| 键 | CBOR 类型 | 约束 | 语义 |
|---|---|---|---|
| `epoch_or_counter` | uint | MLS：单调且有界；Olm/Megolm：恒 `0` | **仅 MLS 使用**，承载 epoch。Olm/Megolm 不使用该字段，接收侧不得对其做序列检查 |

字段本身**仍是冻结字段**，仍必须出现在 canonical CBOR 中（保持 wire 兼容与
`header_hash` 稳定），只是 Olm/Megolm 下取值恒 0 且不参与任何判定。

### 2.3 §7 第 2 条（变更）

原文：

> 2. `epoch_or_counter` 维护滑动窗口；窗口大小由协议配置并设置硬上限，超限执行 resync，不无限缓存。

替换为：

> 2. `epoch_or_counter` 的滑动窗口**仅适用于 MLS**：窗口大小由协议配置并设置硬上限，
>    超限执行 resync，不无限缓存。Olm/Megolm 不使用该字段做重放判定——其重放防护
>    由本节第 1 条的 `message_id` dedupe 与协议自身 ratchet 语义承担。

### 2.4 MLS 落地时的强制约束

引入 MLS（E2EE-04x）时**不得**复用现有 `CryptoStore.checkAndUpdateSequence`：
该实现是**严格单调**，而本节要求的是 IPsec 式**滑动窗口**
（high-water mark + 定长 bitmap + 越界 resync），见提案 25 §3 选项 B 的窗口设计。
直接复用会把 P3 的误杀问题原样带进 MLS。

---

## 3. 被否决的替代方案

| 方案 | 否决理由 |
|---|---|
| 选项 A：`epoch_or_counter` = Olm ratchet message index | 依赖 vodozemac Dart 绑定暴露 message index（未核实可得）；三种协议语义挤同一字段；且 P3 仍不解 |
| 选项 B：应用层 per-session 计数器 + 真滑动窗口 | 与 `message_id` dedupe 功能重叠；新增两处必须纳入事务的状态；窗口参数选错即误杀真实消息 |
| 「只修 `session_ref` 不动 counter」 | **等于选中 P2**：counter 恒 0 + 严格单调 → 首条消息即被拒 → C2C 全线不可读。已实证 |

---

## 4. 实现影响

| 位置 | 变更 |
|---|---|
| `imboyapp/lib/service/e2ee/protected_frame_v3.dart` | `buildProtectedHeader` 对空 `sessionRef` fail-closed（守卫置于构造处，任何调用点漏传立即报错） |
| `imboyapp/lib/service/olm_session_service.dart` | 新增 `ensureSessionId/2`：锁内 load-or-establish 并**立即持久化**，供发送侧在加密前取得真实会话标识 |
| `imboyapp/lib/page/chat/chat/services/chat_network_service.dart` | 传入 `ensureSessionId` 的返回值，不再写空串 |
| `imboyapp/lib/service/e2ee_service.dart` | 接收侧移除 Olm/Megolm 的序列检查；MLS 分支显式未实现 |
| `imboyapp/lib/service/e2ee/crypto_store.dart` | `checkAndUpdateSequence` 存储故障抛 `CryptoStoreUnavailableException`，不再伪装成重放 |

---

## 5. 验收

提案 25 §6 的 RC-01 / RC-02 / RC-04 / RC-05 为本 ADR 的验收门；
其中 **RC-02（首条合法消息必须被接受）与 RC-04（离线批量 + 乱序全部可读）
是可用性回归门**，任何后续改动上线前必须先证明其为绿。

证据见 `evidence/E2EE-025.md`（补充）与
`evidence/E2EE-025-production-wiring-finding.md`。

---

## 6. 未决

- 提案 25 §7 第 3、4 项（§5 两项实现 bug 是否同批修复、`22` 中 E2EE-025 的
  `PASS` 判定如何回退）仍待人工签字；本 ADR 不代签。
- ADR 15 本体文件未就地改写——本文件以 supersede 方式生效，与 ADR 14–19 的
  既有 supersede 惯例一致。若人工要求就地改写 ADR 15，需另行签署。
