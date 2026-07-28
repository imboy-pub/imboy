# E2EE-062 残留 3：重发明文闸门的 C2G 分支实证

- **Slice**：E2EE-062 第九刀（`E2EE-062-retry-guard-wiring-proof.md`）的**残留 3**
- **会话**：`20260729-0900-claude-code`
- **仓库**：`imboyapp`
- **状态**：残留 3 完成；**E2EE-062 整体仍为 `PARTIAL`**（其余残留见 §5）
- **本刀不改任何生产代码**（`git diff --stat lib/` 无输出），只补验收

---

## 1. 做了什么

`MessageRetry._isPlaintextRetryBlocked` 的判据是**两条**：

```dart
final groupMegolm = chatType == 'C2G'
    && await GroupSessionService.to.isGroupE2EE(msg.toId.toString());
encryptionRequired = groupMegolm
    || E2EEService.shouldEncryptOutgoingPayload(chatType);
```

第九刀用真 SQLite + 真事件总线实证了接线，但**只覆盖 C2C**——
`chatType == 'C2G'` 那一支被明确记为「**文件级阅读结论，未实证**」
（该 evidence §5.3 / §6）。本刀补上。

### 1.1 为什么这一支不是冗余

群级 E2EE（P0-B B4）是**独立于全局策略**的强制开关。存在这样的部署：
全局 policy 判 plaintext（`shouldEncryptOutgoingPayload` 返回 false），
但某个群开了群级 E2EE。

此时若判据里只有全局策略那一项，**该群的明文行会被照常重发 —— 明文出网**。
`groupMegolm ||` 这一项就是挡它的，而它此前从未被任何测试触及。

---

## 2. RED 记录 —— 空验证

生产代码在第八刀已改完，直接跑必然全绿；**改前改后都绿的测试没有价值**。
因此 RED 用「临时摘除判据里的 `groupMegolm` 项」取得：

```dart
// 临时：encryptionRequired = E2EEService.shouldEncryptOutgoingPayload(chatType);
```

```
00:00 +2 -1: Some tests failed.
Failing tests:
  … C2G 群级 E2EE 分支 群已开 E2EE + 全局 plaintext + 明文行 → 不得出网
```

**唯独核心用例变红**，对照组与正向可用性仍绿 ——
这正是空验证要的结果：说明该用例确实在测 `groupMegolm` 这一项，
而不是被别的判据顺带盖住。

验证后已恢复：`git diff --stat lib/` **无输出**，生产代码与第八刀提交状态
逐字节一致。

### 2.1 对照组与正向可用性

- **对照组**：`群未开 E2EE + 全局 plaintext → 明文行必须照常重投`。
  它红就说明 harness 没驱动起重投，此时任何「没出网」的绿都毫无意义。
  摘除项前后**都绿** → harness 无缺陷。
- **正向可用性**：`群已开 E2EE + 已加密行 → 必须照常重投`。
  一个「群开了 E2EE 就一律不发」的实现在「不泄漏明文」指标上恒满分，
  被这条否掉。

### 2.2 全局策略固定为 plaintext 是刻意的

`setUp` 里把 `EncryptionModeService` 固定为 `plaintext + initialized`。
若全局策略也要求加密，两条判据会同时为真，**就分不清是哪一条在起作用**——
测试会在摘除 `groupMegolm` 后依然绿，空验证失效。

---

## 3. harness 噪音修正

首次运行时 `contact` 表查询报
`no such column: account_type`（被生产代码的
`⚠️ [RETRY] UI 刷新失败（不影响重试计数）` 捕获，不影响断言）。

虽不影响结果，仍补齐了 DDL 的 `account_type` 列：**噪音会掩盖真错误**，
一个持续刷红字的 harness 会让下一个人看不见真正的失败。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/retry_plaintext_guard_c2g_test.dart
  All 3 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (367 passed；上一刀 364，本刀 +3)

$ flutter test test/service/
  All tests passed!   (1247 passed；上一刀 1244)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）

$ git diff --stat lib/
  （无输出：本刀不改生产代码）
```

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **被拦下的消息会被扫描器反复捡起** —— `_scanAndRetryFailedMessages` 的状态集
   不变，被拦的行下一轮仍会入队再被拦。**不出网、不耗流量**，仅日志重复。
2. **滞留后 UX 无提示** —— 拦下后消息停在库里，用户看到「发送中/失败」，
   没有「安全策略未就绪 / 密钥暂不可用」的说明。
   PolicyGate 注释里记的「显式安全策略未就绪 UX 门」仍未做。
3. **耗尽告警 / 运维指标缺失**（服务端侧）。
4. **幂等 / 补传链路端到端未实证** —— 服务端半边（真 PG）与客户端半边（单测）
   各自实证，拼接仍只有文件级论证。
5. 单租户/全局两层限流未做；租约无独立 TTL；fallback prekey 未在服务端验签；
   60/min 未压测校准；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方。
6. 真机双端未验证。

**本刀关闭的是第九刀残留 3；残留 1/2 与上述其余项不变。**

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| C2G + 群级 E2EE 开启时明文行不出网 | **已实证**（真 SQLite + 真事件总线 + secure storage channel） |
| 群未开 E2EE 时明文行照常重投（对照组） | **已实证** |
| 群已开 E2EE + 已加密行照常重投（正向可用性） | **已实证** |
| `groupMegolm` 项确实在起作用（非被其他判据盖住） | **已实证**（空验证：摘除后唯独核心用例变红） |
| 本刀未改生产代码 | **已实证**（`git diff --stat lib/` 无输出） |
| 「耗尽/限流绝不降级明文」不变量 | **C2C 与 C2G 均已实证**；其余残留见 §5 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未新增依赖。
- **未改动任何生产代码**（本刀是纯验收）。
- 未改动 `_scanAndRetryFailedMessages` 的重试状态集（见 §5.1）。
