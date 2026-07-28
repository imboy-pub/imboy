# E2EE-062 第八刀：耗尽 / 限流绝不触发明文降级

- **Slice**：`22-...state.md` §1.1 队列第 3 项的第八刀（残留 ①，安全性最高的一项）
- **会话**：`20260729-0200-claude-code`
- **仓库**：`imboyapp`
- **状态**：本刀完成；**E2EE-062 整体仍为 `PARTIAL`**（残留见 §5）

---

## 1. 做了什么

### 1.1 本轮实证发现的缺陷

发送路径在加密失败时**确实是 fail-closed 的**：
`ChatNetworkService.sendWsMsg` / `sendMessage` 的 `catch` 分支 → toast →
`return false`，不发送。到此为止一切正确。

**但消息行早已落库**：payload 是明文、`e2ee` 为空，随后被置为
`IMBoyMessageStatus.error`。

`MessageRetry._scanAndRetryFailedMessages` 的重试状态集是
`{sending, pendingRetry, error}`（`message_retry.dart`），会把这条行捡起来；
`_retryMessage` 直接从库里读 `payload` / `e2ee` 拼报文发 WS，
**完全不经过 `encryptPayload`，也不经过 PolicyGate**。

完整链路：

```
OTK 池被抽干 / 触发 429
  → _establishOutboundSession 抛错
  → encryptPayload 抛错
  → sendWsMsg 置 error 并拒发        ← 发送侧 fail-closed 成立
  → MessageRetry 扫到 error 行
  → 按库中原样重发                    ← 明文出网
```

**发送侧的 fail-closed 被重发路径整条旁路掉了。**
这正是残留 ① 要守护的不变量「耗尽 / 限流绝不触发 RSA / Megolm / 明文」的反面。

### 1.2 这不是新发现，是**已知未闭合**

`lib/service/e2ee/policy_gate.dart:55-62` 的注释逐字写着：

> 发送路径捕获本异常后给用户可见 toast，且**不**把消息标记为 error
> （error 会暴露手动重试入口，而 **MessageRetry 重发库里的原始报文、不再经本门
> = 绕过 fail-closed**），消息保持 sending 由重开会话经本门重发。

即：代码库早已知道该旁路，采取的对策是「策略门路径不标 error」。
**该对策不成立**——`sending` 本身就在重试状态集里，且**加密失败路径（本刀关注的
那条）明确会标 error**（`chat_network_service.dart` 的 catch 分支）。
绕法两头都不挡。

**认识论状态：链路各环节均为源码实证（重试状态集、`messageData` 构造不含加密、
catch 分支标 error）；未在真实网络上抓包观测明文出网。**

| 接缝 | 改动 |
|---|---|
| `lib/service/e2ee/retry_plaintext_guard.dart`（新） | 纯函数 `shouldBlockPlaintextRetry` |
| `lib/service/message_retry.dart` | `_isPlaintextRetryBlocked/1`；`_retryMessage` 在构造报文**之前**拦截 |
| `test/service/message_retry_state_test.dart` | 显式声明 E2EE 前置条件（见 §2.2，**断言一字未改**） |

无新依赖、无协议变更、无 schema 变更。

### 1.3 取舍一：只做**拒发**，不在重发路径上补加密

重发状态机不持有加密所需的上下文（对端设备表、Olm session、PFv3 的
`messageId`/`messageType` 同源约束——见 `encryptPayload` 文档注释记录的
E2EE-012/024 历史事故）。在那里补加密等于把发送路径复制一份，是**更大的面**，
且极易再犯 context binding 不同源的老错。

拒发后消息留在库里，由正常发送路径（经门）重新发出。
安全方向：宁可消息发不出去，不可明文出网。

### 1.4 取舍二：策略取不到时按「需要加密」处理

`E2EEService.shouldEncryptOutgoingPayload` 在策略未初始化时抛
`E2eeSecurityException`（PolicyGate 的既定 fail-closed 语义）。
本闸门捕获后一律按 **`encryptionRequired = true`** 处理——未知即拦，不得 fail-open。
这与发送路径对同一异常的处置方向一致。

### 1.5 取舍三：判据与发送路径同源

`encryptionRequired` 用与 `encryptPayload` **同一组判据**算出：
群级 E2EE 强制（P0-B B4，独立于全局策略）**或** `shouldEncryptOutgoingPayload`。
只用后者会漏掉「全局策略不要求、但该群开了群级 E2EE」的情形。

---

## 2. RED 记录

新增 `test/service/e2ee/retry_plaintext_guard_test.dart`（5 例）。
先落**保留今天语义**的载体（`shouldBlockPlaintextRetry` 恒返回 `false`，
即「一律放行」＝今天的行为），使 RED 是行为失败。

```
00:00 +3 -2: Some tests failed.
```

**2 红均为行为失败**：

| 用例 | 失败形态 |
|---|---|
| `该加密却无 e2ee 元数据 → 拦下` | 得 `false` —— 明文行照常重发 |
| `e2ee 为空 map 同样视为明文 → 拦下` | 得 `false` |

**3 绿全部是正向可用性 / 对照组**，改前改后都必须绿：
`已加密的行必须照常重发`、`本就不需要加密的行必须照常重发`、
`不需加密且已加密 —— 两个维度互不干扰`。
对照组全绿 → harness 本身没坏。

### 2.1 「只验拒收」反模式的规避

一个「一律拦下」的实现在「不泄漏明文」这个指标上**恒得满分**，却会让所有重发
失效、消息永久卡住。三条正向可用性用例专为否掉它而写。

### 2.2 既有测试被打红 —— 是真信号，不是噪音

接线后 `message_retry_state_test.dart` 的 4 个用例转红。
**没有据此放宽闸门**，而是先查清原因：

该文件从未初始化 `EncryptionModeService`，于是
`PolicyGate.requireReadyForSend('C2C')` 抛异常 → 按 §1.4 判为「需要加密」→
其明文 fixture 全被拦下。

这暴露了一个此前**隐式**的前提：这些用例的被测对象是**重试状态机**，其成立
前提是「这些消息本来就允许以明文重发」。在重试路径从不查询策略的年代，该前提
无论部署是不是 E2EE 都自动成立；闸门补上查询后，它必须被显式声明，否则这些用例
实际测的是「策略未就绪 → 一律拦下」，与其标题不符。

处置：在 `setUp` 中加

```dart
EncryptionModeService.debugSet(
  mode: EncryptionMode.plaintext,
  initialized: true,
);
```

**断言一字未改，未删除、未 skip 任何用例**，理由已写入该文件的 setUp 注释。
`debugSet` 是既有的 `@visibleForTesting` 注入点（`encryption_mode.dart:226`），
非本刀新增。

---

## 3. 生产调用方核实

```
lib/service/message_retry.dart  _retryMessage → _isPlaintextRetryBlocked → shouldBlockPlaintextRetry
                                （在 messageData 构造与 WebSocketMessageSendRequestEvent 之前）
```

`_retryMessage` 是重发路径的唯一发送点（`retryFailedMessages` →
`_retryFailedMessagesLocked` → `_retryMessage`），由定时 tick、网络恢复事件、
手动重试三条入口驱动。闸门在**构造报文之前**拦截，不是事后过滤。

⚠️ 未做「MessageRetry 端到端拦截」的集成测试：需要 SQLite + 事件总线 + 策略状态
三者同时就位。**`_isPlaintextRetryBlocked` 本身的行为为文件级阅读结论，未实证**；
其唯一下探的纯函数已实证。这是本刀最大的验收缺口，已列入 §5。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/retry_plaintext_guard_test.dart
  All 5 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (360 passed；上一刀 355，本刀 +5)

$ flutter test test/service/
  All tests passed!   (1240 passed；上一刀 1235)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）
```

既有 `message_retry_state_test.dart`（4 例）与 `message_retry_queue_test.dart`
在补上显式前置条件后**全绿**。

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **闸门的接线未实证** —— `_isPlaintextRetryBlocked` 是文件级阅读结论（§3）。
   纯函数已实证，但「MessageRetry 真的会调它、且在发送之前」缺集成测试。
2. **被拦下的消息会被扫描器反复捡起** —— `_scanAndRetryFailedMessages` 的状态集
   不变，被拦的行下一轮仍会入队再被拦。**不出网、不耗流量**，但日志会重复。
   本刀未动扫描器状态集以免扩大爆炸半径。
3. **消息滞留后的 UX 未处理** —— 拦下后消息停在库里，用户看到的是「发送中/失败」，
   没有「安全策略未就绪 / 密钥暂不可用，稍后自动重试」的明确提示。
   PolicyGate 注释里记的「显式安全策略未就绪 UX 门」仍未做。
4. **耗尽告警 / 运维指标缺失**（服务端侧）。
5. **端到端未实证** —— 幂等链路与补传链路各半边分别实证，拼接只有文件级论证。
6. 单租户/全局两层限流未做；租约无独立 TTL；fallback prekey 未在服务端验签；
   60/min 未压测校准；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方。
7. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 该加密却无 e2ee 的行会被判定为「必须拦下」 | **已实证** |
| 已加密的行不受影响（正向可用性） | **已实证** |
| 不需加密的行不受影响（正向可用性） | **已实证** |
| 策略取不到时按「需要加密」处理 | **已实证**（既有 retry 测试在未初始化时全被拦，反证该分支生效） |
| 既有重试状态机行为未被削弱 | **已实证**（补显式前提后 4+8 例全绿，断言未改） |
| MessageRetry 从不经过 encryptPayload / PolicyGate | **已实证**（源码：`messageData` 直接取库中 payload/e2ee） |
| 重试状态集含 error 与 sending | **已实证**（源码：`statusesToRetry`） |
| 「明文经重发路径出网」在真实网络上发生 | **文件级推理，未实证**（未抓包） |
| 闸门已接入 `_retryMessage` 且在发送前 | **文件级阅读结论，未实证**（缺集成测试） |
| 「耗尽/限流绝不降级明文」不变量整体成立 | **不成立** —— 见 §5.1 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未新增依赖。
- 未改动 `_scanAndRetryFailedMessages` 的重试状态集（见 §5.2）。
