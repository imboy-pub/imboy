# E2EE-025 —— 生产 wiring 实证：C2C Olm v3 消息在接收侧被整条拒绝

- **会话**：`20260728-1141-claude-code`
- **性质**：**已确认的 P0 生产缺陷**（实证复现，非静态推断）
- **状态**：缺陷已锁定并留有失败守护测试；**修复未实施**（等待人工拍板，见 §5）
- **仓库**：`imboyapp`（HEAD `955e27a6`，未提交）

---

## 1. 结论先行

提案 25 §1.3 把 `session_ref` 恒空定性为「纵深防御少了一层，而非可利用漏洞」。
**该定性不成立——实际后果严重得多。**

生产 C2C PFv3 路径产出的消息，在接收侧被 `_validateContextBinding` 判为
`context_mismatch_session_id`，**整条消息不可读**。不是少一层防御，
是这条链路根本不通。

---

## 2. 实证

新增 `imboyapp/test/service/e2ee/production_session_ref_wiring_test.dart`，
完全复刻生产调用姿势（`sessionRef: ''`）+ 与真实 `OlmProtocol` 逐字段一致的
metadata 形状（对照 `lib/service/e2ee/olm_protocol.dart:65-78`）。

```
$ flutter test test/service/e2ee/production_session_ref_wiring_test.dart

RC-01 header.session_ref 必须非空且等于 protocol_metadata.session_id
  Expected: non-empty
    Actual: ''

RC-02 生产路径产出的首条合法消息必须能被接收侧接受
  Expected: not <true>
    Actual: <true>
  失败原因: {..., _e2ee_failed: true, _e2ee_reason: context_mismatch_session_id, ...}
```

解码出的 `protected_header` 里可直接看到 `ksession_ref` 后跟 CBOR 空串 `` `0x60` ``，
而同一信封的 `protocol_metadata.session_id` 是非空的真实会话 id。

---

## 3. 因果链（三处都已逐行核实）

| # | 位置 | 事实 |
|---|---|---|
| 1 | `lib/page/chat/chat/services/chat_network_service.dart:634` | `sessionRef: '', // OlmProtocol 内部填充 session_id` —— 注释所述行为**不存在** |
| 2 | `lib/service/e2ee/olm_protocol.dart:77` | `'session_id': res.sessionId` 写进 **protocol_metadata**，从不回填 protected_header |
| 3 | `lib/service/e2ee_service.dart:692-702` | `_validateContextBinding` §7 硬比对 `protocol_metadata.session_id == outerHeader.session_ref`，不等即返回 `'session_id'` |

`useOlmForC2C = true`（`chat_network_service.dart`）已默认开启，因此这条链路
在启用 Olm 的部署上是**默认路径**。

---

## 4. 为什么既有测试全绿却没抓到

`test/service/e2ee/protected_frame_v3_roundtrip_test.dart:95` 手工写
`sessionRef: 'test-session'`，而同文件的假协议 `_IdentityProtocol` 返回
`{'session_id': 'test-session'}` —— 测试**把生产不会对齐的两个值人为对齐了**。

这与项目既有教训同类：mock 掉协议边界的测试只验证「调用姿势」，
验不出真实实现之间的契约错配。

---

## 5. 修复方案与待拍板的取舍

选项 C 要求 `session_ref` 填**真实协议会话标识**（Olm session id），
所以 `_validateContextBinding` §7 的比对是正确的，缺陷完全在发送侧。

**根本困难（循环依赖）**：
`E2eeOutboundRouter.encryptV3` 必须先 `buildProtectedHeader`（需要 sessionRef）
→ 组 inner_frame → 才能 `protocol.encrypt`；
而 Olm session id 只在 `OlmSessionService.encryptC2CMessage` **内部**、
`_loadSession ?? _establishOutboundSession` 之后才产生
（`olm_session_service.dart:414-436`）。

`E2eeSessionProtocol` 接口被 **ADR 02 §10 冻结**，不能加方法取 session id。

### 方案 A（唯一不动冻结接口的路径）

在 `OlmSessionService` 新增 `ensureSessionId(peerUid, peerDeviceId)`：
在**同一把 per-device 锁**内 load-or-establish 并返回 `session.sessionId`；
`chat_network_service` 先调它拿到 id 再传给 `encryptV3`。

- ✅ 不动 ADR 02 冻结接口，不动 PFv3 编码。
- ⚠️ **两阶段调用存在竞态窗口**：`ensureSessionId` 与随后的 `encrypt` 之间，
  会话理论上可能被替换（如并发触发重新协商），导致 header 里的 `session_ref`
  与实际加密所用 session 不一致。
  后果是**接收侧 fail-closed 拒绝该条消息**（不是安全漏洞，是可用性抖动）。
- ⚠️ 新增一次 `_establishOutboundSession` 的触发点：首次对某设备发消息时，
  claim prekey 会在 `ensureSessionId` 阶段发生，改变了现有的网络时序。

### 方案 B（改比对而非改发送侧）

放弃「session_ref 必须等于协议 session id」，改为发送方可独立计算的稳定标识
（如 `peerUid:peerDeviceId`），并相应放宽 `_validateContextBinding` §7。

- ✅ 无竞态、无新 API、无新网络时序。
- ❌ **偏离已签字的选项 C 定义**（C 明确写「填协议会话标识」），
  需要再次修订 ADR 15 §3.1 的 `session_ref` 语义并重新签字。
- ❌ 削弱该字段的绑定强度（不再把密文绑定到具体 ratchet 会话）。

**本会话未选择、未实施任何一方案**——两者都改变已签字的语义或引入新的失败模式，
属架构决策，按执行规则须人工拍板。

---

## 6. 对既有状态标记的影响（供人工裁定）

- `22` 中 E2EE-025 标为 `PASS`（⚠️ 待人工复核）。本发现证明其验收
  **只在 CryptoStore 层完成，从未穿过生产路由**，且生产路由实际是断的。
- `22` §3 的状态机不含 `PASS -> PARTIAL` 转换，本会话未擅改该行。
- 同时应复核 **E2EE-012**（`PASS`，evidence/E2EE-012.md，验收对象正是
  「Protected Context 纵向闭环」与 `_validateContextBinding`）与
  **E2EE-024**（`PASS`，mutation matrix）：它们同样可能建立在
  「测试内手工对齐 sessionRef」之上而非生产 wiring 上。

---

## 7. 本次改动

| 文件 | 性质 |
|---|---|
| `imboyapp/test/service/e2ee/production_session_ref_wiring_test.dart` | **新增**：2 项失败守护测试（RC-01 / RC-02），准确反映生产真实状态 |

未改任何生产代码。未 commit、未 push。

**基线变化**：`flutter test test/service/e2ee/` 由
`304 passed / 0 failed` 变为 `304 passed / 2 failed`。
这 2 项失败是**真实缺陷的暴露**，按执行规则不得 skip 或删除；
修复落地后应转绿。
