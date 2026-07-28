# E2EE-062 残留 1：闸门必须排在 CAS 之前

- **Slice**：E2EE-062 第八/九刀的**残留 1**
- **会话**：`20260729-1000-claude-code`
- **仓库**：`imboyapp`
- **状态**：残留 1 的**有害部分已关闭**；良性部分仍在（§5.1）。
  **E2EE-062 整体仍为 `PARTIAL`**

---

## 1. ⚠️ 先更正我此前的记载

第八/九刀的 evidence 与会话日志把残留 1 写成：

> 被拦下的消息会被扫描器反复捡起 …… **不出网、不耗流量**，仅日志重复。

**该记载不完整。** 逐行核实 `_retryMessage` 的实际顺序后发现，闸门排在
**CAS 之后**：

```
403 行  CAS：把 status 翻成 sending
429 行  info.retryCount++
434 行  repo.find
445 行  ← 闸门在这里才拦，然后出队 return
```

于是被拦下的行在**每个扫描周期**都会：

1. **被写库**，`error` → `sending`；
2. `retryCount++`，但随后即出队 → **计数被丢弃**。

后果比「日志重复」严重：

- **用户永远看到「发送中」而不是「失败」** —— 状态在 error/sending 间反复翻转；
- **DB 写入无上限**；
- **永远到不了放弃上限**（`RetryPolicy.maxRetryAttempts`），因为计数每轮都被丢。

「不出网」这一条仍然成立；「不耗流量」也成立；但「仅日志重复」是错的。
**认识论状态：以上均为已实证**（RED 用例直接观测到 DB 状态被翻成 `sending`）。

---

## 2. 做了什么

把**读取消息 + 闸门**整体移到 CAS **之前**：

```
find → 闸门 → CAS → 构造报文 → 发送
```

原则很简单：**拦下意味着这一轮什么都不该发生，包括不该动库。**

CAS 之后原有的第二次 `find` 已删除——`msg` 在闸门前已读到，
而 CAS 只改 `status`，下方 `messageData` 不含 `status`，故复用安全，
**且不增加任何一次额外读**。

| 接缝 | 改动 |
|---|---|
| `lib/service/message_retry.dart` | `_retryMessage`：find + 闸门前移到 CAS 之前；删除 CAS 后的重复 find |

无新依赖、无协议变更、无 schema 变更、无新增函数。

### 2.1 前移带来的读取新鲜度变化（已评估）

闸门读到的 `msg` 现在比过去**略旧**（早于 CAS 而非晚于）。两个方向：

- 消息在 find 与 CAS 之间**刚被加密** → 本轮误拦 → **fail-closed 方向**，
  且下一轮扫描即放行，无损；
- 消息在此窗口内**被解密**（`e2ee` 被清空）→ 会漏放。
  但代码库中**没有任何路径清空 `e2ee`**，且旧顺序存在同类竞态。

因此该变化**只在安全一侧**，不构成新风险。

---

## 3. RED 记录

在既有 `test/service/e2ee/retry_plaintext_guard_integration_test.dart` 上新增 2 例
（复用其真 SQLite + 真事件总线 harness，未另起 harness）。

```
00:00 +4 -1: Some tests failed.
Failing tests:
  … 被拦下时不得改动 DB 状态（闸门必须排在 CAS 之前）
```

**1 红为行为失败**：被拦下的行状态实得 `sending`，期望 `error`。

**对照组**：`允许重投的行，状态必须被 CAS 翻成 sending` —— 改前改后**都绿**。
它红就说明 CAS 根本没生效，此时「被拦下时状态不变」的绿也说明不了任何事
（一个 CAS 完全失灵的实现会让两条用例同时"通过"）。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/retry_plaintext_guard_integration_test.dart
  All 6 tests passed.        # 上一刀 4，本刀 +2

$ flutter test test/service/e2ee/
  All tests passed!   (369 passed；上一刀 367)

$ flutter test test/service/
  All tests passed!   (1249 passed；上一刀 1247)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）
```

既有 `message_retry_state_test.dart`（含「重投达上限 → error 出队（放弃）」
「CAS 把状态翻回 sending」等断言）在本次调序后**全绿**——
说明前移闸门没有削弱重试状态机的任何既有行为。

---

## 5. 残留风险（E2EE-062 仍未完成）

### 5.1 残留 1 只关闭了有害部分

被拦下的行**仍会被扫描器每轮重新捡起**（status 保持 `error`，仍在重试状态集内），
每轮仍有一次 `find` + 一次闸门判定 + 一行日志。

**但现在确实只是这些了**：不写库、不翻状态、不出网。
彻底消除需要改 `_scanAndRetryFailedMessages` 的状态集或引入「已拦下」标记，
那会扩大爆炸半径且触及重试状态机的核心不变量，**本刀未做**。

### 5.2 其余残留（不变）

1. **滞留后 UX 无提示** —— 现在用户能正确看到「失败」（本刀的副产品），
   但仍没有「安全策略未就绪 / 密钥暂不可用」的具体说明。
   PolicyGate 注释里记的「显式安全策略未就绪 UX 门」仍未做；
2. 耗尽告警 / 运维指标缺失（服务端侧）；
3. 幂等 / 补传链路端到端未实证；
4. 单租户/全局两层限流未做；租约无独立 TTL；fallback prekey 未在服务端验签；
   60/min 未压测校准；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方；
5. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 闸门原本排在 CAS 之后 | **已实证**（源码行号 + RED 用例观测到状态被翻） |
| 被拦下的行会被写库并翻成 `sending` | **已实证**（RED） |
| 前移后被拦下的行状态保持 `error` | **已实证** |
| 允许重投的行仍被 CAS 翻成 `sending`（对照组） | **已实证** |
| 既有重试状态机行为未被削弱 | **已实证**（`message_retry_state_test.dart` 全绿） |
| 读取新鲜度变化只在安全一侧 | **文件级阅读结论**（全仓无清空 `e2ee` 的路径） |
| 「被拦下的行永远到不了放弃上限」 | **已实证**（计数每轮被丢弃，由代码顺序直接可见） |
| 残留 1 已彻底关闭 | **否** —— 见 §5.1 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未新增依赖。
- **未改动 `_scanAndRetryFailedMessages` 的重试状态集**（见 §5.1）。
