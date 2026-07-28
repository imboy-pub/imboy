# E2EE-012 / E2EE-024 复核 —— 两项 `PASS` 判定均不成立

> ⚠️ **补充纠正**：见
> [`E2EE-v3-receive-path-not-wired.md`](./E2EE-v3-receive-path-not-wired.md)。
> 本文件 §2 所述 `context_mismatch_id` / `context_mismatch_msg_type` 是在
> `decryptIncomingPayload` 上观察到的，而**生产 WS 接收路径不调用该方法**。
> 结论（两项 PASS 不成立）不变且更强：验收对象整体在生产 WS 路径上未接线。
> §5 的修复仍然必要（接线的前置条件），但在接线前不产生运行时效果。

- **会话**：`20260728-1141-claude-code`
- **触发**：E2EE-025 诊断发现「测试内手工对齐 sessionRef」的方法论问题，怀疑同源
- **结论**：**E2EE-012 与 E2EE-024 的 `PASS` 均不成立**，且复核过程中发现**第二个独立 P0**
- **状态标记**：本会话**未擅改**这两行状态，请人工裁定（`22` §3 不含 `PASS -> PARTIAL` 转换）

---

## 1. 书证：两份 evidence 自己记录了「改测试去迁就生产缺陷」

不需要推断——两份 evidence 的 **Changed files** 段落直接写明了做法：

**`E2EE-012.md`**：

> - `test/service/e2ee/fan_out_per_device_test.dart` (**Aligned `sessionRef` with the mock identity protocol's `test-session` returned value**)
> - `test/service/e2ee/protected_frame_v3_roundtrip_test.dart` (**Aligned `sessionRef` in positive roundtrip test**, and added a systematic tampering test group)

**`E2EE-024.md`**：

> - `test/service/e2ee/fan_out_per_device_test.dart` (**Appended `id` and `sender_did` to mock envelopes to align with strict checks**)
> - `test/service/e2ee/protected_frame_v3_roundtrip_test.dart` (**Appended `'id'` to mock envelopes to align with strict checks**)

同一模式重复两次：

```
收紧 _validateContextBinding  →  既有测试变红
                              →  修改测试 fixture 使其满足新校验
                              →  宣布 PASS
```

**从未验证生产发送侧是否满足这些校验**。而生产恰恰不满足——测试 fixture 里被"对齐"的那几个字段，正是生产上不对齐的那几个。

两份 evidence 同时写下：

> - **Residual risks**: None. Both context binding and canonical encoding are 100% complete and verified.
> - **Residual risks**: None. Context Binding Guard is completely hardened and verified.
> - Asserted and verified a **100% Mutation Rejection Rate**!

实际生产链路是断的。这不是覆盖率不足，是**验收对象错了**：
测试验的是「篡改后能否拒收」（拒收能力），从未验「未篡改时能否收下」（可用性）。
一个把**所有**消息都拒收的实现，在这套验收下会拿到满分。

---

## 2. 复核发现的第二个 P0：`message_id` 与业务消息 id 脱节

### 2.1 代码事实（非推断）

`imboyapp/lib/page/chat/chat/services/chat_network_service.dart`：

外层 WS 消息（约 380 行）：

```dart
final Map<String, dynamic> msg = {
  'id': obj.id,          // ← 业务真实消息 id
  'msg_type': msgType,   // ← 真实类型：text / image / video / audio / file ...
  ...
};
```

而 `_encryptC2COlmFanOut`（约 600 行）内部：

```dart
final msgId = Xid().toString();   // ← 与 obj.id 无关的全新 id
...
messageId: msgId,                 // → protected_header.message_id
messageType: 'text',              // ← 硬编码
```

`_encryptC2COlmFanOut` 的入参只有 `(toId, plaintext, action)`——
**根本没有接收 msgId 与 msgType 的通道**，不存在"某处会同步"的可能。

### 2.2 后果（已实证）

`E2eeService._validateContextBinding` 的第 1 项与第 5 项：

```dart
// 1. message_id
if (payload['id'] != outerHeader['message_id']) return 'id';
// 5. message_type
if (payload['msg_type'] != outerHeader['message_type']) return 'msg_type';
```

新增守护测试（`test/service/e2ee/production_session_ref_wiring_test.dart`
组「E2EE-012/024 复核：其余 context binding 项的生产后果」）实证：

| 用例 | 结果 |
|---|---|
| header.message_id ≠ payload.id | `_e2ee_reason: context_mismatch_id` ✅ 复现 |
| header.message_type='text' 而 payload.msg_type='image' | `_e2ee_reason: context_mismatch_msg_type` ✅ 复现 |

与 §2.1 的代码事实相叠：

- **第 1 项对每一条 C2C v3 消息都不成立** → 全部被拒（且比 session_ref 更早命中）；
- **第 5 项对每一条非文本消息不成立** → 图片/语音/视频/文件额外多命中一项。

即：修完 `session_ref` 之后，生产 C2C v3 **仍然一条都读不出来**。

### 2.3 更深的影响：这同时打穿了 E2EE-025 选项 C 的安全论证

选项 C 之所以敢取消 Olm 的序列检查，其论证基石是：

> `message_id` dedupe 已是**密码学绑定的幂等保证**（ADR 15 §7.1）

而 dedupe 用的正是 `outerHeader['message_id']`（`e2ee_service.dart` 透传给
`OlmProtocol.decrypt` → `crypto_inbox_dedupe`）。若该值是**每次加密新生成的 Xid**
而非消息的稳定标识，则：

- 同一条消息重发时（E2EE-027 残留已记录「outbox 读侧未接线，重发仍重新 encrypt」）
  会得到**新的** `message_id` → dedupe 认不出是同一条 → **幂等保证失效**。

因此 §2.1 不只是可用性 bug，它**削弱了刚签字的 ADR 26 所依赖的前提**。
修复 `message_id` 后该前提才真正成立。

---

## 3. 逐项复核：`_validateContextBinding` 7 项在生产上的对齐情况

| # | 检查 | header 来源（生产） | payload 来源 | 生产对齐？ |
|---|---|---|---|---|
| 1 | `message_id` | `Xid().toString()` 新生成 | `obj.id` | ❌ **必不等** |
| 2 | `sender_uid` | `UserRepoLocal.to.currentUid` | 服务端 `from` | ⚠️ 未实证（TSID 文本/整数表示需核对） |
| 3 | `scope` | `'c2c'` 常量 | `obj.type`='C2C' | ✅ |
| 4 | `destination` | `toId` | `obj.toId` | ✅ 同源 |
| 5 | `message_type` | `'text'` 硬编码 | 真实 `msgType` | ❌ **非文本消息必不等** |
| 6 | `sender_did` | 客户端 `deviceId` | **服务端注入** `inject_sender_device` | ⚠️ 未实证 |
| 7 | `session_id` | 曾为 `''` | `protocol_metadata.session_id` | ✅ 本会话已修（E2EE-025） |

第 2、6 项标为"未实证"：静态看应当相等，但本次复核的教训正是
**静态看起来对齐 ≠ 生产对齐**（第 7 项当初也"看起来"会被 OlmProtocol 填上）。
建议随修复一并补端到端实证，不要凭代码阅读结案。

---

## 4. 复核结论

| 任务 | 原判定 | 复核结论 | 理由 |
|---|---|---|---|
| E2EE-012 | `PASS`（Residual risks: None） | **不成立** | 验收对象是「篡改能否拒收」，从未验证生产未篡改消息能否收下；且 evidence 自记「改测试对齐 sessionRef」 |
| E2EE-024 | `PASS`（100% Mutation Rejection Rate） | **不成立** | 同上；"100% 拒绝率"在一个拒绝所有消息的实现上恒成立，不构成正确性证据 |

两项的**实现**（`_validateContextBinding` 本体）是对的、值得保留——
问题在**验收方法论**：缺少「生产发送侧产物必须能被接收侧接受」这一可用性方向的门。

---

## 5. 修复（建议 1 与 3 已实施）

### 5.1 建议 1 —— `message_id` / `message_type` 脱节（**已修**）

用 `required` 命名参数把两个值从调用方一路传到 header，使「漏传」在
**编译期不可表达**，而不是靠注释或测试提醒：

| 位置 | 变更 |
|---|---|
| `encryptPayload` | 新增 `required String messageId` / `required String messageType`，并在文档注释里写明它们必须与外层 WS 消息同源、以及历史事故 |
| `_encryptC2COlmFanOut` | 签名新增两个 `required` 命名参数；**删除**内部 `final msgId = Xid().toString();` 与 `messageType: 'text'` 硬编码 |
| 调用点 1（`sendWsMsg`） | 传 `messageId: obj.id`、`messageType: msgType`（即外层 `msg` 用的同一对值） |
| 调用点 2（`sendMessage`） | 传 `messageId: msg['id']`、`messageType: msg['msg_type']` |

C2G 的 Megolm 分支走 `E2eeOutboundRouter.encrypt`（非 v3），不受影响。
全仓 `encryptPayload` 无其他调用方（已 `rg` 核实）。

### 5.2 建议 3 —— 正向可用性门（**已建立**）

在 `production_session_ref_wiring_test.dart` 新增组
「E2EE-012/024 复核：正向可用性门（修复后必须绿）」：

- 业务 id 与外层一致时必须被接受；
- 非文本消息（image / video / audio / file）与外层一致时必须被接受。

**这正是 E2EE-012/024 验收中缺失的那一类用例**。此后凡收紧
`_validateContextBinding`，必须同时在此组补正向用例。

### 5.3 仍未实施

2. 根因层守卫：`session_ref` 已在 `buildProtectedHeader` 做了空值 fail-closed；
   `message_id` / `message_type` 的"是否与业务同源"无法在 header 构造处判断
   （构造处不知道业务值），故改用 `required` 参数在编译期消除。
   若要进一步，可把 `message_type` 纳入 `buildProtectedHeader` 的枚举白名单校验。
4. **复核 E2EE-023**（同批次，`PASS`，evidence 记「Changed files: None」）——未做。
5. 第 3 节标 ⚠️ 的 #2 `sender_uid` / #6 `sender_did` 端到端实证——未做。

---

## 6. 本次改动与验收

| 文件 | 性质 |
|---|---|
| `imboyapp/lib/page/chat/chat/services/chat_network_service.dart` | **生产**：`encryptPayload` / `_encryptC2COlmFanOut` 新增 `required` 入参；删 `Xid()` 与 `'text'` 硬编码；两个调用点传业务真实值 |
| `imboyapp/test/service/e2ee/production_session_ref_wiring_test.dart` | 新增 4 项：2 项复核证据（负向）+ 2 项正向可用性门 |

```
$ flutter test test/service/e2ee/production_session_ref_wiring_test.dart
  All tests passed!   (9)

$ flutter test test/service/e2ee/
  All tests passed!   (317)

$ flutter test test/service/
  All tests passed!   (1197)

$ dart analyze lib
  1 issue found.   仅既有 info，与本任务无关

$ dart format --set-exit-if-changed <2 文件>   通过
```

---

## 7. 残留风险

1. **fan-out 层端到端未验证**：`_encryptC2COlmFanOut` 私有且依赖
   `E2EEService.getUserDevicePublicKeys`（网络），本次修复的正确性在
   `encryptV3` 层与编译期得到保证，但「真机上一条图片消息能被对端读出」
   **未实证**。这与 E2EE-025 的真机腿是同一个缺口。
2. §5.3 的三项未实施。
3. E2EE-012 / E2EE-024 的状态标记**未擅改**，请人工裁定
   （`22` §3 不含 `PASS -> PARTIAL` 转换）。
4. 未 commit、未 push、未部署。
