# E2EE-027 补课 — 出站 outbox fail-closed

- **性质**：修复已确认漏洞（不改协议、不改 ADR、不新增任务编号），依据 20-plan §S0.1
  「未获人工签字前只允许评审、Spike 和修复已确认漏洞」。
- **Session ID**: 20260727-1819-claude-code（同会话续做，用户明确指定）
- **Date**: 2026-07-27
- **Repository**: imboyapp
- **Before HEAD**: 955e27a6 / **After HEAD**: 955e27a6（未提交）
- **触发**：E2EE-030 期间复核发现，记录在 `evidence/E2EE-030.md` §7.2。

## 1. 缺陷

`E2eeOutboundRouter.encryptV3` 在加密后写 immutable outbox，但：

```dart
try {
  final db = await SqliteService.to.db;
  if (db != null) { ...insertOutbox... }
} catch (_) {
  // outbox 写入失败不影响主流程
}
```

两处 fail-open：
1. `db == null`（SQLCipher 不可用）→ 静默跳过 outbox，密文照常返回给调用方发送；
2. 写入抛错 → `catch (_)` 静默吞掉，同样照常返回。

后果：崩溃恢复所依赖的 outbox 条目可能根本不存在，而 UI 已显示"已发送"——消息既未投递也无重发依据。
违反 ADR 14 §8（CryptoStore 提交失败 → 消息保留"未发送"）与 ADR 20 §S2.3（先原子提交 outbox，**再**发送）。

## 2. 修复

- 删除 `catch (_)`：写入错误一律向上传播。
- `db == null` → 抛新增的 `E2eeOutboxCommitException`，不返回可发送信封。
- 顺序保持：encrypt → 提交 outbox → 才把信封交给调用方。

失败测试先行（`test/service/e2ee/outbox_fail_closed_test.dart`，4 用例）：
修复前 2 failed（两处 fail-open 均未抛）/ 2 passed，修复后 4 passed。

| 用例 | 断言 |
|---|---|
| CryptoStore 不可用时不得返回可发送信封 | `throwsA(isA<E2eeOutboxCommitException>())` |
| outbox 写入报错不得被静默吞掉 | 抛出，且不留半条记录 |
| 成功路径 | 返回前 outbox 已 pending，payload 与信封 `jsonEncode` 逐字节一致 |
| outbox 内容不得包含消息明文 | payload 不含明文串 |

### 影响面处理

fail-closed 后，走生产加密路径但未注入事务存储的 3 个既有测试会失败。**未放宽策略、未 skip、未删除测试**，
而是给它们注入真实 SQLite（ffi in-memory），并改为**逐用例独立 DB**——`crypto_session_sequence`
是跨消息单调状态，共享 DB 会让用例结果依赖执行顺序：

- `test/service/e2ee/mutation_matrix_test.dart`
- `test/service/e2ee/protected_frame_v3_roundtrip_test.dart`
- `test/service/e2ee/fan_out_per_device_test.dart`

## 3. 注入 DB 后新暴露的生产缺陷（未修，需人工决策）

**E2EE-025 的 PFv3 序列检查这一层在生产 C2C 路径上失效。** 此前测试不注入 DB，
`checkAndUpdateSequence` 从不执行，故该缺陷被完全掩盖。

> **严重性界定（复核后收敛）**：重放防护并非全空。`message_id` dedupe 那一层
> **在生产上已接线且生效**：`e2ee_service.dart:538-542` 把 `outerHeader['message_id']`
> 透传给 `OlmProtocol.decrypt` → `decryptC2CMessage(messageId:)` → `crypto_inbox_dedupe`
> 原子去重；叠加 Olm 自身 message key 用后即毁。故本项是**纵深防御少了一层**，
> 而非重放门户大开。

发送侧 `lib/page/chat/chat/services/chat_network_service.dart` 调 `encryptV3` 时：

```dart
sessionRef: '',              // 注释称"OlmProtocol 内部填充 session_id"
// epochOrCounter 未传 → 默认 0
```

`OlmProtocol` 填的是 `protocol_metadata.session_id`，**不会**回填 protected_header 的 `session_ref`；
counter 也从无递增逻辑。接收侧 `lib/service/e2ee_service.dart:506-516`：

```dart
final sessionRef = outerHeader['session_ref']?.toString() ?? '';
final seq = outerHeader['epoch_or_counter'] as int? ?? 0;
if (sessionRef.isNotEmpty) { ...checkAndUpdateSequence(sessionRef, seq)... }
```

两种结果都错：
- `session_ref` 恒为空 → **整段防重放被跳过**（当前生产实况，fail-open）；
- 若把 `session_ref` 填上而 counter 仍恒 0 → `0 <= lastSeq(0)` → **首条合法消息被判 replay**（fail-closed 误杀）。

修复需要先定 counter 语义（按 `(session_ref)` 域单调递增由谁维护、与 Olm 自身 message index 的关系、
ADR 15 §7 滑动窗口如何落地），属协议层决策，不在"修复已确认漏洞"授权范围内。

**建议**：重核 `22` 中 E2EE-025 的 `PASS` 判定（其验收同样只在 CryptoStore 层完成，未穿过生产路由）。
本次未擅自改动该状态标记。

## 4. E2EE-027 其余残留（未修）

1. **outbox 读侧未接线**：`pendingOutbox` / `confirmOutbox` / `getOutboxEntry` 在 `lib/` 下
   **零生产调用者**。重发仍由 `message_retry.dart` 走业务重发并**重新 encrypt**，
   即验收项「重发 100 次 ciphertext byte-for-byte 相同且 ratchet version 只推进一次」在生产上不成立。
   接线需改动出站确认状态机（`message_retry.dart` 是唯一状态机），属独立 Slice。
2. **严格原子性未达成**：ratchet 提交发生在 `protocol.encrypt` 内部（`OlmSessionService`），
   outbox 提交发生在其返回之后，二者不在同一 SQLite 事务。要真正合并需要给
   `E2eeSessionProtocol.encrypt` 传入 outbox 构造回调——而该接口是 ADR 02 §10 **冻结项**，
   未经 ADR 签字不得改（执行规则 13）。
   *安全影响评估*：剩余窗口是「ratchet 已提交、outbox 未提交」。此时消息不会被发送（本次修复保证），
   下条消息在 index k+1 加密，**不产生 key reuse**，只损失该条消息（用户可见为发送失败）。
   即残留为可用性缺口，非机密性/完整性缺口。
3. `OlmSessionService.encryptC2CMessage` 的 `outboxId`/`outboxPayload` 参数在生产上从不被传，
   `CryptoStore.persistSessionWithOutbox` 因此是死路径（仅测试覆盖）。保留未删，待 §4.1 接线时复用。

## 4b. 追加修复：v3 接收侧错误分类（ADR 15 §5）

复核 §3 时发现 `e2ee_service.dart` 的 v3 解密段用 `catch (_)` 把所有异常压成
`decrypt_error`，丢失两类语义不同的信号：

- `DuplicateMessageException` → 现归类 `duplicate_message`。ADR 15 §7.1 要求重复密文
  幂等返回；压成解密失败会让上层向用户报错，而 ratchet 其实并未重复推进。
- `OlmStateCommitException`（E2EE-030）→ 现归类 `crypto_store_unavailable`。这是**可重试**
  的本地存储故障，密文本身无问题；压成 `decrypt_error` 会被上层当作永久失败。

其余异常仍归 `decrypt_error`（ADR 15 §5：不上送 oracle 细节）。

失败测试先行：`test/service/e2ee/decrypt_error_taxonomy_test.dart`（4 用例），
修复前 2 failed / 2 passed，修复后 4 passed。含"错误分类不得泄漏秘密细节"守护用例。

> 附带发现（未修）：`CryptoStore.checkAndUpdateSequence` 用 `catch (_) { return false; }`
> 把 DB 故障也报成"序列校验失败"，上层显示为 `replay_detected`。方向上是 fail-closed，
> 但分类错误——DB 故障不是重放。与 §3 的 counter 语义一并处理为宜。

## 5. 修改文件

- `imboyapp/lib/service/e2ee/e2ee_outbound_router.dart` — 新增 `E2eeOutboxCommitException`；outbox 提交改 fail-closed。
- `imboyapp/lib/service/e2ee/crypto_store.dart` — 修正 `insertOutbox` 文档（读侧未接线的事实）。
- `imboyapp/lib/service/e2ee_service.dart` — v3 接收侧错误分类（§4b）。
- `imboyapp/test/service/e2ee/outbox_fail_closed_test.dart` — 新增（4 用例）。
- `imboyapp/test/service/e2ee/decrypt_error_taxonomy_test.dart` — 新增（4 用例）。
- `imboyapp/test/service/e2ee/{mutation_matrix,protected_frame_v3_roundtrip,fan_out_per_device}_test.dart` — 注入真实事务存储 + 逐用例 DB 隔离。

## 6. 验收命令与结果

| 命令 | 结果 |
|---|---|
| `flutter test test/service/e2ee/outbox_fail_closed_test.dart` | 4 passed, 0 failed, 0 skipped（修复前 2 failed） |
| `flutter test test/service/e2ee/decrypt_error_taxonomy_test.dart` | 4 passed, 0 failed, 0 skipped（修复前 2 failed） |
| `flutter test test/service/e2ee/` | 304 passed, 0 failed, 0 skipped |
| `flutter test test/integration/room_key_olm_roundtrip_test.dart test/service/group_session_service_test.dart test/service/olm_suite_routing_test.dart test/service/e2ee_service_test.dart` | 42 passed, 0 failed, 0 skipped |
| `dart analyze lib` | 1 info（`component/ui/ios_settings_ui.dart`，既有基线，与本改动无关） |
| `git diff --check` | clean |

无明文、私钥、content key、token、PII 进入日志或异常文本。
