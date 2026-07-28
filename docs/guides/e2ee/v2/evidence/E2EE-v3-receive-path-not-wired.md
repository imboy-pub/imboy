# ⚠️ 纠正 + 新发现：PFv3 接收侧在生产 WS 路径上完全未接线

- **会话**：`20260728-1141-claude-code`
- **性质**：**对本会话此前结论的纠正**，同时是一个更根本的缺口
- **状态**：仅诊断，未修复（接线属架构级改动，需人工决策）

---

## 1. 先纠正我此前写错的因果

本会话此前在 `E2EE-025-production-wiring-finding.md`、
`E2EE-012-024-review.md`、`22` 的会话日志、以及**已提交的 commit message**
（imboyapp `df92b232`）中反复写道：

> 「每条生产 C2C Olm v3 消息都被判 `context_mismatch_session_id` /
> `context_mismatch_id`，整条不可读。」

**这个因果是错的。** 那些 `context_mismatch_*` 是我在测试里
直接调用 `E2EEService.decryptIncomingPayload` 观察到的——
而**生产 WS 接收路径根本不调用这个方法**。

结论方向没变（生产 v3 链路确实是断的），但**断点位置与机制判断错了**。
真实断点在更前面，见 §2。

---

## 2. 事实

### 2.1 `decryptIncomingPayload` 在 WS 接收路径上没有调用方

```
$ rg -rn "decryptIncomingPayload" lib/ --type dart -l
lib/service/e2ee_service.dart                      ← 定义处自身
lib/page/mine/user_collect/user_collect_provider.dart  ← 收藏列表，非 WS 路径
```

即 `_decryptV3Payload`、`ProtectedFrameV3.verifyOuterEnvelope`、
`_validateContextBinding`（**E2EE-012 / 023 / 024 / 025 的全部验收对象**）
对 WS 消息而言是**死代码**。

### 2.2 生产 WS 接收路径走的是另一条

`lib/service/message.dart::_handleE2EEMessage`
（`lib/modules/messaging/infrastructure/message_model_mapper.dart:40` 同）：

```dart
// 1. 获取密文字符串
final ciphertext = data['payload']?.toString();
if (ciphertext == null || ciphertext.isEmpty) {
  return { ..., '_e2ee_failed': true, '_e2ee_reason': 'empty_payload' };
}
...
final plaintext = await E2EEService.decryptE2EEMessage(
  ciphertext: ciphertext,
  e2ee: e2ee,
);
```

而 `decryptE2EEMessage` 只做一件事：

```dart
return E2eeProtocolRegistry.resolve(metadata)
    .decrypt(ciphertext: ciphertext, metadata: metadata);
```

**没有** header_hash 校验、**没有** canonical CBOR 严格解析、
**没有** context binding、**没有** inner/outer header 比对。

### 2.3 v3 消息在第一步就失败

v3 发送侧（`E2eeOutboundRouter.encryptV3` 结尾）：

```dart
// v3: ciphertext 在 envelope 内，外层 E2eeCiphertext.ciphertext 为空
return E2eeCiphertext('', envelope);
```

`chat_network_service` 把它放进外层消息的 `'payload'` 字段 → **空串**。
接收侧第一行取 `data['payload']` → 空 → 直接返回
`_e2ee_reason: 'empty_payload'`。

**每条 v3 消息在接收侧第一步即失败**，`context_mismatch_*` 永远不会被触发。

此外 v3 的真实密文位于 `e2ee.devices[<did>].ciphertext`，
接收侧完全没有解析 `devices` fan-out 结构的逻辑。

---

## 3. 这对既有判定意味着什么

| 任务 | 原判定 | 修正后的复核结论 |
|---|---|---|
| E2EE-012 | `PASS` | **不成立**，且比此前所述更严重：验收对象在生产 WS 路径上未接线 |
| E2EE-023 | `PASS` | **存疑**（同一模块，同样未接线）——本会话未单独复核 |
| E2EE-024 | `PASS` | **不成立**，同 012 |
| E2EE-025 | `PASS`（待复核） | **不成立**；本会话所修的 `session_ref` 语义正确且必要，但在接线之前不产生任何运行时效果 |
| E2EE-029 | `PASS` | **存疑**：C2C per-device Olm fan-out 的发送侧已实现，但接收侧无 `devices` 解析逻辑 |

**共同根因**：这一批验收全部只在 `decryptIncomingPayload` 这条
**测试专用**入口上完成，从未穿过生产 WS 接收路径
（`message.dart::_handleE2EEMessage`）。

---

## 4. 本会话三处修复的定位（不撤回，但需要重新表述）

| 修复 | 是否仍必要 | 说明 |
|---|---|---|
| `session_ref` 填真实 Olm session id | ✅ 必要 | ADR 15 §3.1 冻结字段约束；接线后立即生效 |
| `message_id` / `message_type` 不再自造 | ✅ 必要 | 同上；且 `message_id` 是 dedupe 的键，直接关系 ADR 26 的前提 |
| counter 语义收敛（ADR 26） | ✅ 必要 | 协议语义定案，与接线无关 |

三者都是**接线的前置条件**——不修，接线当天就会全线不可读。
但**它们都不足以让生产 v3 可用**，因为接收侧压根没走那条路。

---

## 5. 待人工决策

接线 WS 接收路径到 v3 是架构级改动，涉及：

1. `_handleE2EEMessage` 需要按 `e2ee.meta_version` 分流：
   v3 走 `decryptIncomingPayload`（含完整 PFv3 校验），v1/v2 保持现状；
2. 需要解析 `fan_out: per_device` 的 `devices` 结构，
   按本机 `deviceId` 取出对应信封（发送侧 E2EE-029 已实现，接收侧缺）；
3. 失败分类需要与既有 `_e2ee_reason` 体系合并；
4. 灰度与回滚策略：接线后旧客户端/旧消息的兼容边界。

**本会话不自行实施**，按执行规则交人工裁定。

---

## 6. 方法论教训（建议写入验收门）

本次连续三轮，每一轮都是「静态阅读得出的结论被下一轮实证推翻」：

1. 提案 25 静态判断：「纵深防御少一层，非可利用漏洞」
   → 实证：`context_mismatch_session_id`，链路不通；
2. 我静态判断：「每条消息被 context binding 拒收」
   → 实证：接收侧根本不执行那段代码，真实断点是 `empty_payload`。

**唯一可靠的验收方式是从生产入口进、从生产出口出**。
建议在 `20-plan` 的验收要求中固化一条：

> E2EE 接收侧的任何验收，必须以 `message.dart::_handleE2EEMessage`
> （生产 WS 入口）为起点，不得以 `E2EEService.decryptIncomingPayload`
> 等内部方法为起点。

---

## 7. 接线（已实施）

采用「把解密从 `_receiveMessage` 副作用链中解耦」的方向，最小落地：

### 7.1 新增可测边界

`E2EEService.decryptInboundV3(data)` —— **纯函数**，无 DB / 事件 / provider 副作用。
返回 `null` 表示「非 v3，调用方继续走既有 v1/v2 路径」。

它与此前被测的 `decryptIncomingPayload` 有本质区别：
**它是生产路径实际调用的入口**，而不是一条生产不走的旁路。

### 7.2 两处放行 + 分流

| 位置 | 改动 |
|---|---|
| `message.dart::_receiveMessage` | `if (payloadRaw.isEmpty) return;` 改为**仅对非 v3 生效**——此前正是这行让每条 v3 消息被静默丢弃 |
| `message.dart::_handleE2EEMessage` | 第 0 步调用 `decryptInboundV3`；命中即返回（成功返回明文，失败返回带 `_e2ee_reason` 的占位），未命中则继续既有 v1/v2 路径 |

v1/v2 路径**未改动**，其错误分类（含 `No key found for device` 的精确判定）
原样保留，避免误伤既有行为。

### 7.3 验收

```
$ flutter test test/service/e2ee/v3_receive_path_e2e_test.dart
  All tests passed!   (4)
    - v3 fan-out 消息必须被识别并解出明文
    - 非 v3 信封必须返回 null，交回 v1/v2 路径（不得撞 invalid_keys）
    - 无 e2ee 的明文消息必须返回 null
    - v3 信封损坏必须返回失败分类（no_device_envelope），不得静默丢弃

$ flutter test test/service/e2ee/     → 321 passed 0 failed 0 skipped（此前 317）
$ flutter test test/service/          → 1201 passed
$ dart analyze lib                    → 基线（1 条既有 info）
```

### 7.4 仍未闭合

1. **「`_handleE2EEMessage` 确实委托给 `decryptInboundV3`」目前靠代码审查保证，
   无自动化断言。** 真正的端到端门仍待建。
2. 端到端 harness 尝试失败的记录见 §7.5，其暴露的架构问题未解决。
3. **真机双端未验证**——本会话所有 E2EE 修复（session_ref / message_id /
   message_type / counter 语义 / 本次接线）都只在单测层证明。

### 7.5 端到端 harness 尝试失败的记录（保留以免重复踩）

试过从 `MessageService.processMessage` 端到端验证，失败：

- ✅ `processMessage` 能在测试宿主跑起来；
- ✅ 协议重复注册（`already registered: olm`）可解——先
  `E2eeBootstrap.ensureRegistered()` 置位再 `resetForTest()` 换入假协议；
- ❌ `contact.account_type` 缺列：内嵌基线 DDL 是 v16，当前 schema 是 v24，
  而 `MigrationService.migrate` 在 in-memory 库上未生效（疑与快照/文件路径
  依赖有关，未深查）；
- ⚠️ `_providerContainer` 未初始化（已被生产代码 catch）。

**暴露的架构问题**：`_receiveMessage` 把 E2EE 解密与 contact 仓储、会话
provider、通知等**与 E2EE 无关**的依赖耦合在一条链上，导致「从生产入口
验证解密」代价极高。这正是此前所有验收都退到内部方法上测的根本原因——
那条路好测，但生产不走。本次接线把解密抽成纯函数是朝正确方向走了一步，
但副作用链本身未解耦。

需要修正的既有文档（已在 §1 声明，原文件保留以存证）：
`evidence/E2EE-025-production-wiring-finding.md`、
`evidence/E2EE-012-024-review.md`、`22` 相关会话日志、
imboyapp commit `df92b232` 的 message。
