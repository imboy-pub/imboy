# E2EE-061 Slice 4 —— ⛔ BLOCKED：AAD 绑定 `header_hash` 在当前发送链路上不可实现

> **会话**：20260730-0300-claude-code ｜ **仓库**：imboyapp（只读核实）、imboy（文档）
> **状态**：**未写任何代码**。设计 §2.1 的 AAD 构成须先由人工修订。

---

## 1. 结论先说

设计 §2.1 写着：

> 每块 AAD **至少**绑定 `header_hash + attachment_id + chunk_index + chunk_count`
> —— `header_hash` 来自 PFv3 protected header，**这是 ATT-01 的直接依据**。

**这一条在当前发送链路上不可实现**，两条独立的原因，均已实证：

| # | 原因 | 严重度 |
|---|---|---|
| A | 附件上传发生在消息组装**之前**，上传时 header 尚未存在 | 可通过重排缓解 |
| B | **同一条消息对每个收件设备有各自不同的 protected_header**，因而有 N 个 `header_hash`；而附件对象**只有一份** | **决定性，无法通过重排解决** |

因此 **Slice 4（上传接线）不能按现设计动工**。已停在此处，未写一行接线代码。

---

## 2. 实证

### 2.1 A：上传先于消息组装

`lib/page/chat/chat/attachment_handler.dart:271`

```dart
final meta = await AttachmentApi.uploadImageEntityViaPresign(entity, ...);
await handleImageUploadPresign(meta, entity);   // ← 消息在这之后才建
```

上传返回 `object_key` 等 meta 后才去构造并发送消息。上传时
`message_id` / `created_at_ms` / `session_ref` / `epoch_or_counter` 全都尚未确定。

### 2.2 B：header 是**每收件设备一份**（决定性）

`lib/page/chat/chat/services/chat_network_service.dart:636` 起：

```dart
for (final entry in didToPem.entries) {        // ← 逐个收件设备
  final peerDid = entry.key;
  ...
  // E2EE-025 的既有注释：session_ref 必须是真实的 Olm 会话标识
  final sessionRef = await OlmSessionService.to.ensureSessionId(toId, peerDid);
  final encrypted = await E2eeOutboundRouter.encryptV3(
      ..., sessionRef: sessionRef, ...);        // ← 每设备各建一次信封
  devices[peerDid] = envelope;
}
```

`E2eeOutboundRouter.encryptV3` 内部 `FrameContext.sessionRef = sessionRef`
→ `buildProtectedHeader` → `header_hash`。
**`sessionRef` 逐设备不同（`ensureSessionId(toId, peerDid)`）⇒ `header_hash` 逐设备不同。**

> ⚠️ 我在核实途中一度看 `e2ee_outbound_router.dart` 里「只有一次
> `encodeOuterEnvelope` 调用」，据此得出「header 是每消息一份」的**错误**中间结论。
> 是往上追到**调用方**才看到那个 `for` 循环——**判断一个值是否唯一，必须看调用方，
> 不能只看被调函数内部**。这与本项目多次记录的失效模式同类。

### 2.3 由此得出的矛盾

一条带附件的 C2C 消息发往对端 3 台设备 ⇒ 3 个不同的 `header_hash`，
但 Garage 上**只有一个附件对象**。密文块的 AAD 只能绑其中一个，
另外两台设备**必然打不开**——即「加密后附件对多数收件设备不可读」。

`epoch_or_counter` 还有第二重问题：它在加密时才确定，且 `MessageRetry` 重发路径
会重新加密（counter 前进）⇒ 即便只有一台设备，重发一次已上传的附件也会失配。

---

## 3. 为什么不自行改掉

把 AAD 里的 `header_hash` 换成「上传前即可确定、且全设备一致」的值
（例如 `message_id + conversation_id + sender_uid`）在技术上可行，
且 ATT-01「附件搬到另一条消息打不开」由 `message_id` 同样成立。

**但这是改动密码学绑定的构成，属设计修订**，不在「两种合理实现选安全那个」
可自行裁决的范围：

1. 它**削弱**了设计原文声称的绑定强度（原文绑整个 header，替代方案只绑三个字段）；
2. 绑定内容是接收侧拒收判据的一部分，属安全语义；
3. 设计 §2.1 明文把 `header_hash` 称作「ATT-01 的**直接依据**」，
   替换它就是替换验收用例的依据。

故按裁决规则记 BLOCKED，交人工修订，**不代改**。

---

## 4. 给拍板者的三个候选（不含推荐，未做取舍）

| 方案 | 做法 | 代价 |
|---|---|---|
| **甲** | AAD 绑 `message_id + conversation_id + sender_uid + attachment_id + chunk_index + chunk_count`（全部在上传前确定、全设备一致） | 绑定强度弱于原设计；`message_id` 须在上传前生成并贯穿到发送 |
| **乙** | 保留 `header_hash`，改为**每设备一份密文附件对象** | 存储与流量 ×N；N 台设备就传 N 份，100MB 视频对 3 设备 = 300MB |
| **丙** | 保留 `header_hash`，把附件上传移到加密之后、且**不允许重新加密** | 与 E2EE-027 outbox（先落密文再发送）耦合；重发路径要改；上传失败时 ratchet 已前进 |

三者都会牵动 Slice 6（下载侧校验）与 ATT-01 的验收表述。

---

## 5. 本刀对既有交付的影响

**Slice 2 / 3 不受影响，无需返工**：

- `AttachmentChunkCodec` 的 AAD 参数只要求「32 字节的绑定值」，
  它叫 `headerHash` 只是命名；换成别的 32 字节摘要，编解码器一行不用改；
- `AttachmentDescriptor` 完全不涉及该绑定；
- `AttachmentEncryptor` 同样只是把该值透传给 codec。

若人工选定方案甲，改动落点是**调用方怎么算这 32 字节**，以及命名与注释。

**Slice 5（后端 `cipher` 列）不受影响**：它与 AAD 构成无关。

---

## 6. 验收

本刀**未改任何代码**，两侧验收命令不适用。已核实两仓无生产代码漂移
（`git status --porcelain` 仅并发会话的两个文件）。

---

## 7. 认识论状态

| 结论 | 状态 |
|---|---|
| 上传先于消息组装 | **已实证**（`attachment_handler.dart:271`） |
| `sessionRef` 逐收件设备不同 | **已实证**（`chat_network_service.dart:636` 的 `for` + `ensureSessionId(toId, peerDid)`） |
| ⇒ `header_hash` 逐设备不同、附件对象只有一份 | **已实证**（由上两条直接推出） |
| `epoch_or_counter` 重发时前进导致失配 | **推理**（基于 `MessageRetry` 重发会重新加密这一既有记载），**未构造重发实测** |
| 「header 是每消息一份」 | **已被自己推翻** —— 只看被调函数内部得出的错误中间结论（§2.2） |
| 方案甲的绑定强度足以支撑 ATT-01 | **未验证**，属待拍板内容 |

---

## 8. 未做

- 未写任何接线代码；未改 AAD 构成；未改 ADR；未改任何任务状态标记。
- 不 push、不部署、不访问生产、不通知第三方。
