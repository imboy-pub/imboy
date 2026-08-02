# E2EE-027 outbox 读侧接线与残留关闭（2026-08-02）

> **性质**：残留关闭实证（P1-5）
> **前置**：`E2EE-027-followup.md` §4 残留三项
> **执行**：Claude Code，imboyapp HEAD（提交见 git log）

## 1. 残留 #1（读侧未接线）→ 已关闭，并纠正一处事实错误

**纠正**：§4.1 称"重发仍重新 encrypt"——**与当前生产代码不符**。
`E2eeOutboundRouter.encryptV3` 把同一 envelope 既写 `crypto_outbox`（`e2ee_outbound_router.dart:180`）
又返回给发送路径落进消息行 `e2ee` 字段；`MessageRetry._retryMessage`（message_retry.dart:460-471）
从消息行取 `e2ee`/`payload` 原样发出，**不经任何加密调用**。

**接线**：`confirmOutbox` 已接入 ACK 单一汇聚点
（`message_retry.dart` 的 `RemoveFromRetryQueueRequestedEvent` 监听 → `_confirmOutboxQuietly`，
失败只记日志不阻断 ACK）。

**实证**（`test/service/e2ee/outbox_read_side_wiring_test.dart`，3/3 绿，真 SQLite + 真事件总线）：
1. 重发事件的 `e2ee` 与落库信封**逐字节一致**（byte-for-byte，harness 无加密服务，重新加密必爆炸）；
2. ACK 移除事件驱动 `crypto_outbox` pending → sent；
3. outbox 表缺失时移除事件照样生效（卫生动作不阻断 ACK）。

**验收项核对**：「重发 100 次 ciphertext byte-for-byte 相同且 ratchet 只推进一次」
经用例 1 在重发路径上成立（重发不触碰 ratchet，无加密调用）。

## 2. 残留 #2（ratchet+outbox 非同一事务）→ 维持 Acknowledged

`E2eeSessionProtocol.encrypt` 是 ADR 02 §10 **冻结项**（本次签字未涉及 ADR 02），
合并事务需改该接口，仍冻结。安全影响按 §4.2 既有评估：**可用性缺口**
（崩溃窗口内丢该条消息，无 key reuse），非机密性/完整性缺口。登记台账。

## 3. 残留 #3（persistSessionWithOutbox 死路径）→ 维持（待接线复用，随读侧演进再议）

## 4. 验收命令与结果

| 命令 | 结果 |
|---|---|
| `flutter test test/service/e2ee/outbox_read_side_wiring_test.dart` | 3/3 绿 |
| `flutter test test/service/e2ee/` | 591/591 绿（588+3） |
| `flutter test test/service/message_retry_state_test.dart` | 9/9 绿 |
| `dart analyze lib` | 1 既有 info（与改动无关） |

修改：`lib/service/message_retry.dart`（confirmOutbox 接线+导入）、
`lib/service/e2ee/crypto_store.dart`（注释纠偏）、新增 `test/service/e2ee/outbox_read_side_wiring_test.dart`。
