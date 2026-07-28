# E2EE-062 第九刀：重发路径明文闸门的**接线实证**

- **Slice**：`22-...state.md` §1.1 队列第 3 项的第九刀
  （第八刀 `E2EE-062-retry-plaintext-guard.md` 的残留 ①）
- **会话**：`20260729-0300-claude-code`
- **仓库**：`imboyapp`
- **状态**：本刀完成；**E2EE-062 整体仍为 `PARTIAL`**（残留见 §5）
- **本刀不改生产代码**（`git diff --stat lib/` 为空），只补验收

---

## 1. 做了什么

第八刀实现了 `shouldBlockPlaintextRetry` 并把它接进
`MessageRetry._retryMessage`，但当时**只有纯函数被实证**：
「MessageRetry 真的会调它、且在发送之前」是**文件级阅读结论**，
被明确记为该刀最大的验收缺口（`E2EE-062-retry-plaintext-guard.md` §5.1）。

本刀补上这条实证。断言对象是**有没有 `WebSocketMessageSendRequestEvent` 出网**，
不是内部函数的返回值。

| 接缝 | 改动 |
|---|---|
| `test/service/e2ee/retry_plaintext_guard_integration_test.dart`（新） | 真 SQLite（ffi 内存库）+ 真事件总线，驱动完整 `retryFailedMessages()` |

harness 复用 `message_retry_state_test.dart` 的既有范式（同一份最小 DDL、
`SqliteService.setDbForTest`、`RetryPolicy` 拨到期），未新增任何基建依赖。

---

## 2. RED 记录 —— 本刀的 RED 是**空验证**

生产代码在第八刀已改完，直接跑必然全绿。**一条改前改后都绿的测试没有价值**，
因此本刀的 RED 用「临时把闸门还原成载体（恒 `return false`）」取得：

```
00:00 +2 -2: Some tests failed.
```

**2 红正是两条「不得出网」用例**：

| 用例 | 失败形态 |
|---|---|
| `部署要求加密 + 明文行 → 不得出网` | 出网了 |
| `策略未就绪（未知）+ 明文行 → 不得出网` | 出网了 |

**2 绿是对照组与正向可用性**（见 §2.2），闸门在与不在都必须绿。

### 2.1 ⚠️ RED 输出直接证实了第八刀标注为「未实证」的那条

失败输出里逐字带出了发出去的帧：

```
WebSocketMessageSendRequestEvent({"id":"gd0000000000000pt04","type":"C2C",
 "from":1001,"to":2002,"msg_type":"text","action":"",
 "e2ee":null,"payload":{"msg_type":"text","text":"hi"}, ...})
```

`e2ee` 为 `null`、`payload` 是**明文**，且是在
`EncryptionModeService` 判定为「要求加密 / 策略未就绪」的前提下发出的。

第八刀 evidence §6 把「明文经重发路径出网」标为
**「文件级推理，未实证（未抓包）」**；本刀在真 SQLite + 真事件总线上把它
升级为 **已实证**。（帧中的 `"hi"` 是测试 fixture 文本，非真实用户数据。）

### 2.2 对照组与正向可用性

- **对照组**：`部署本就明文 → 明文行必须照常重投`。
  它红就说明 harness 根本没驱动起重投，此时任何「没出网」的绿都毫无意义。
  该用例在闸门在与不在时**都绿** → harness 无缺陷。
- **正向可用性**：`部署要求加密 + 已加密行 → 必须照常重投`。
  一个「一律不发」的实现在「不泄漏明文」指标上恒得满分，被这条否掉。

### 2.3 恢复核实

空验证后已恢复：`git diff --stat lib/` **无输出**——生产代码与第八刀提交状态
逐字节一致，临时改动没有残留。

---

## 3. 生产调用方核实

测试不触碰任何内部函数，只做三件事：写库 → `retry.retryFailedMessages()` →
观察事件总线。链路由生产代码自行走完：

```
retryFailedMessages → _retryFailedMessagesLocked → _retryMessage
  → _isPlaintextRetryBlocked → shouldBlockPlaintextRetry
  → （放行时）构造 messageData → AppEventBus.fire(WebSocketMessageSendRequestEvent)
```

GREEN 运行的日志里可见生产代码打出的拦截信号：

```
重试发送消息: gd0000000000000pt04, 第1次重试
🚫 [RETRY] 未加密消息不得重发，已拦下: gd0000000000000pt04
```

即闸门确实在**报文构造之前**生效，不是事后过滤。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/retry_plaintext_guard_integration_test.dart
  All 4 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (364 passed；上一刀 360，本刀 +4)

$ flutter test test/service/
  All tests passed!   (1244 passed；上一刀 1240)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）

$ git diff --stat lib/
  （无输出：本刀不改生产代码）
```

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **被拦下的消息会被扫描器反复捡起** —— `_scanAndRetryFailedMessages` 的状态集
   不变，被拦的行下一轮仍会入队再被拦。**不出网、不耗流量**，仅日志重复。
   未动扫描器以免扩大爆炸半径。
2. **滞留后 UX 无提示** —— 拦下后消息停在库里，用户看到「发送中/失败」，
   没有「安全策略未就绪 / 密钥暂不可用」的明确说明。
   PolicyGate 注释里记的「显式安全策略未就绪 UX 门」仍未做。
3. **群级 E2EE 分支未实证** —— 本文件只覆盖 C2C。
   `_isPlaintextRetryBlocked` 中 `chatType == 'C2G' && isGroupE2EE(...)`
   这一支需要 `GroupSessionService` 就位，未纳入。
   **认识论状态：文件级阅读结论，未实证。**
4. **耗尽告警 / 运维指标缺失**（服务端侧）。
5. **幂等 / 补传链路端到端未实证** —— 服务端半边（真 PG）与客户端半边（单测）
   各自实证，拼接仍只有文件级论证。
6. 单租户/全局两层限流未做；租约无独立 TTL；fallback prekey 未在服务端验签；
   60/min 未压测校准；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方。
7. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 闸门已接入 `_retryMessage` 且在报文构造之前 | **已实证**（真 SQLite + 真事件总线 + 生产日志） |
| 部署要求加密时明文行不出网 | **已实证** |
| 策略未就绪时明文行不出网（未知即拦） | **已实证** |
| 已加密行照常重投（正向可用性） | **已实证** |
| 部署本就明文时明文行照常重投（对照组） | **已实证** |
| **明文确实会经重发路径出网**（无闸门时） | **已实证**（空验证 RED 的帧输出，见 §2.1；第八刀此项为"未实证"） |
| 本刀未改生产代码 | **已实证**（`git diff --stat lib/` 无输出） |
| C2G / 群级 E2EE 分支同样生效 | **文件级阅读结论，未实证**（见 §5.3） |
| 「耗尽/限流绝不降级明文」不变量整体成立 | **C2C 已成立；C2G 未实证** |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未新增依赖。
- **未改动任何生产代码**（本刀是纯验收）。
