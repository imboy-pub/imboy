# 提案 25 — `session_ref` / `epoch_or_counter` 语义定案

> **状态**：Proposed —— 需人工决策后方可实施。本文件只做评审与选项分析，
> 不改变任何已冻结的协议规范（执行规则 13；20-plan §S0.1 允许"评审、Spike"）。
> **草拟**：Claude Code 代笔，2026-07-27
> **触发**：`evidence/E2EE-027-followup.md` §3 —— 注入真实事务存储后暴露，
> PFv3 序列检查在生产 C2C 路径上不生效。
> **关联**：ADR 15 §3.1（header 冻结字段）、§7.1（message_id 幂等）、§7.2（滑动窗口）

---

## 1. 事实基线（已核实，非推断）

### 1.1 发送侧

`imboyapp/lib/page/chat/chat/services/chat_network_service.dart` 调 `encryptV3`：

```dart
sessionRef: '',   // 注释称 "OlmProtocol 内部填充 session_id"
// epochOrCounter 未传 → 默认 0
```

`OlmProtocol.encrypt` 填的是 `protocol_metadata.session_id`，**不会**回填 protected_header 的
`session_ref`。counter 从无递增逻辑。

**这违反 ADR 15 §3.1 的冻结字段约束**：`session_ref` 定义为 `text, 1..256 字节`（即非空），
`epoch_or_counter` 定义为 `uint, 单调且有界`。写空串与恒 0 都不合规。

### 1.2 接收侧

`imboyapp/lib/service/e2ee_service.dart:506-516`：

```dart
final sessionRef = outerHeader['session_ref']?.toString() ?? '';
final seq = outerHeader['epoch_or_counter'] as int? ?? 0;
if (sessionRef.isNotEmpty) {
  final store = await OlmSessionService.to.cryptoStore;
  if (store != null) {
    final success = await store.checkAndUpdateSequence(sessionRef, seq);
    if (!success) return _decryptFailedPayload(payload, reason: 'replay_detected');
  }
}
```

`CryptoStore.checkAndUpdateSequence` 的判定是 `sequence <= lastSeq → 拒绝`，即**严格单调递增**。

三个问题：

| # | 问题 | 后果 |
|---|---|---|
| P1 | `session_ref` 恒空 → 整段被跳过 | 该层防护在生产上从不执行（fail-open） |
| P2 | 若只修 P1 而 counter 仍恒 0 → `0 <= 0` | **首条合法消息即被判 replay**，C2C 全线不可用 |
| P3 | 严格单调 ≠ ADR 15 §7.2 的**滑动窗口** | 离线批量投递与 WS 重连乱序是 IMBoy 的常态，严格单调必然误杀 |

### 1.3 严重性界定

**不是重放门户大开。** 另两道防线在生产上有效：

1. `message_id` dedupe：`e2ee_service.dart:538` 把 `outerHeader['message_id']` 透传给
   `OlmProtocol.decrypt` → `decryptC2CMessage(messageId:)` → `crypto_inbox_dedupe` 原子去重。
   且 `message_id` 位于**受认证的** protected_header 内，攻击者改它会破坏 header_hash/inner 比对，
   无法用"换个 id 重放同一密文"绕过。
2. Olm Double Ratchet 自身：message key 用后即毁，重放同一密文解密失败。

故本项定性为**纵深防御少了一层**，而非可利用漏洞。这决定了它不该被当作 P0 抢修，
而应作为协议语义定案一次做对。

---

## 2. 决策点

`epoch_or_counter` 在 **Olm/Megolm** 场景下到底承担什么职责？（MLS 场景无争议：它就是 epoch。）

---

## 3. 选项

### 选项 A — 由协议自身 message index 承载

`session_ref` = Olm session id；`epoch_or_counter` = 该密文的 ratchet message index。

- ✅ 天然单调、零新增状态。
- ❌ 依赖 vodozemac Dart 绑定暴露 message index —— **未核实是否可得**，需先做 Spike。
- ❌ Olm index / Megolm index / MLS epoch 三种语义塞进同一字段，跨协议不同构。
- ❌ 仍需接收侧实现滑动窗口（P3 不解）。

### 选项 B — 应用层 per-session 单调计数器 + 真正的滑动窗口

- 发送侧：复用 `crypto_session_sequence` 作**出站**计数器，按 `session_ref` 域原子自增后写入 header。
- 接收侧：把 `checkAndUpdateSequence` 从"严格单调"改为 **IPsec 式抗重放窗口**
  （high-water mark + 定长 bitmap，如 W=2048）：窗口内未见过 → 接受并置位；
  窗口内已见过 → 拒绝；低于窗口下沿 → 拒绝；远超上沿 → 触发 resync（ADR 15 §7.2 明文要求）。

- ✅ **不需要修订 ADR 15**——这就是 §3.1 + §7.2 的字面实现。
- ✅ 协议无关，Olm/Megolm/MLS 统一。
- ❌ 与 `message_id` dedupe 功能重叠：后者已由受认证字段提供同等保证（见 §1.3）。
- ❌ 新增出站计数器 + 接收窗口 bitmap 两处状态，且都必须与 ratchet 同事务提交，
  否则重蹈 E2EE-030 的状态分叉覆辙。
- ❌ 窗口参数选错即误杀真实消息（可用性风险高于其安全收益）。

### 选项 C — 显式收敛职责，Olm/Megolm 不使用该字段（**推荐**）

- `session_ref`：**必填非空**，填协议会话标识（Olm session id / Megolm session id / MLS group ref）。
  修复 §1.1 的字段约束违规，并保留可观测性与 MLS 落点。
- `epoch_or_counter`：**仅 MLS 使用**（epoch）。Olm/Megolm 恒填 0，接收侧**不对其做序列检查**。
- 重放防护职责明确归属：`message_id` dedupe（ADR 15 §7.1）+ 协议自身 ratchet 语义。
- ADR 15 §7.2 的滑动窗口条款收敛为**仅适用于 MLS**，在 E2EE-04x 引入 MLS 时按选项 B 的窗口设计落地。

- ✅ 最小改动，消除 P2 误杀风险。
- ✅ 不引入与既有机制重复的第二套计数器（DRY / YAGNI）。
- ✅ 职责边界清晰：谁防重放、防到什么程度，可写进安全说明。
- ❌ **需要修订 ADR 15**（§3.1 `epoch_or_counter` 说明 + §7.2 适用范围），走 supersede 流程签字。
- ❌ 纵深防御确实少一层——但该层的独立安全收益接近 0（见 §1.3）。

---

## 4. 建议

**采用选项 C。**

理由：`message_id` 位于受认证 header 内，dedupe 已是密码学绑定的幂等保证；Olm 自身
message key 用后即毁。在此之上再叠一套应用层计数器，属重复机制，其**可用性风险
（离线批量+乱序误杀）高于其安全收益**。ADR 15 §7.2 的滑动窗口对 MLS 应用消息是必需的，
对 Olm 不是。

若决策者不愿动 ADR 15（考虑到 14–19 尚未签字，再叠修订会加长阻塞链），
则退而采用**选项 B**——它无需修订 ADR，但必须完整实现滑动窗口而非严格单调，
且两处新状态必须纳入事务提交。**不接受"只修 session_ref 不动 counter"**，那等于选 P2。

---

## 5. 无论选哪个都必须一并修的实现 bug

1. **`session_ref: ''`** —— 违反 ADR 15 §3.1 冻结字段约束，属实现 bug 而非协议问题。
   修复点：`chat_network_service.dart` 的 `encryptV3` 调用需拿到 Olm session id。
   注意时序：session id 由 `OlmProtocol.encrypt` 内部产生，而 `session_ref` 是构造
   protected_header 的**入参**——存在与 E2EE-027 outbox 同源的先后依赖，需一并设计。
2. **`CryptoStore.checkAndUpdateSequence` 的 `catch (_) { return false; }`** ——
   把 DB 故障报成"序列校验失败"，上层显示 `replay_detected`。
   方向上是 fail-closed（可接受），但错误分类失真，应区分 `replay` 与 `store_error`
   （参照 `evidence/E2EE-027-followup.md` §4b 已落地的错误分类修复范式）。

---

## 6. 验收测试（选定方案后落地）

| ID | 用例 | 通过条件 |
|---|---|---|
| RC-01 | 生产发送路径产出的 header | `session_ref` 非空且等于实际协议会话标识 |
| RC-02 | 首条合法消息 | 必须被接受（回归 P2） |
| RC-03 | 同一密文重放 100 次 | 业务只提交一次，ratchet 不重复推进（现由 dedupe 保证，须有守护测试） |
| RC-04 | 离线批量 + 乱序投递 50 条 | 全部可读，0 误判 replay |
| RC-05 | DB 故障期间收消息 | 分类为 `crypto_store_unavailable`，不得报 `replay_detected` |
| RC-06 | 选 B 时：窗口下沿外的旧序号 | 拒绝；远超上沿 → 触发 resync 而非无限缓存 |

RC-02 / RC-04 是**可用性回归门**：任何方案上线前必须先证明它们绿，
否则会把一个纵深防御缺口换成一次全线消息不可读事故。

---

## 7. 需要签字的内容

- [x] 选定 A / B / C → **选定 C**（人工决策，2026-07-28，会话 `20260728-1141-claude-code` 记录）
- [x] 若选 C：批准 ADR 15 §3.1 + §7.2 的 supersede 修订 → **已批准**（同上；选项 C 的签字项已明示含此条）
- [x] 确认 §5 两项实现 bug 与所选方案同批修复 —— **已签**（2026-08-02 leeyi solo 决策；随 P1-1 批次修复）
- [x] 确认 `22` 中 E2EE-025 的 `PASS` 判定回退为 `PARTIAL` —— **已签**（2026-08-02 leeyi solo 决策）；
      `22` §3 状态机已于同日补 `PASS -> PARTIAL` 合法转换（人工裁定通道）

### 7.1 落地前置（选定 C 之后仍未完成）

选项 C 只是**语义定案**，尚未产生任何代码或规范变更。E2EE-025 开工前还需要：

1. 撰写 ADR 15 的 supersede 修订稿（§3.1 `epoch_or_counter` 说明 + §7.2 适用范围
   收敛为仅 MLS），走与 ADR 14–19 相同的人工签署流程；
2. 明确上面两个未签字项；
3. §6 的 RC-01..06 中，RC-02（首条合法消息必须被接受）与 RC-04（离线批量+乱序
   50 条全部可读）是**可用性回归门**，必须先绿再改生产路由。
