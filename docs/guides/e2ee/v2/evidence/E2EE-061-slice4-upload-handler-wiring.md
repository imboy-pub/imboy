# E2EE-061 Slice 4（接线刀）——上传路径接线：message_id 前置 / 闸门调用 / descriptor 进加密 payload

- 日期：2026-07-30
- 仓库：imboyapp（代码）、imboy（本文档）
- 状态：**PARTIAL**。代码接线完成、判定与绑定输入已实证；
  **推出开关默认关闭**，且**未真机验证** —— 按用户 2026-07-30 决定
  （「代码先写完，真机验证留作单独一轮」），061 后续各刀在真机验证前
  **一律不得标 PASS**。

---

## 1. 本刀做了什么

三步（对齐 Slice 4b 记录的 Next task）：

| # | 内容 | 落点 |
|---|---|---|
| ① | `message_id` 提前到**上传之前**生成并贯穿 | `attachment_handler.dart` 全部 **6 条**上传路径 |
| ② | 上传前调用 `AttachmentSealPolicy.decide(...)` 决定是否传 `seal` | `ChatAttachmentHandler.buildSealRequest`（唯一入口） |
| ③ | descriptor 放进消息 **metadata**（→ 并入 payload → 随 PFv3 加密） | `_withDescriptor(...)`，同上 6 条路径 |

顺带处理了残留 3（`metadata.file_hash256` 的明文暴露）与两条由本刀**新造出来的**
泄漏面（见 §3）。

### ① 覆盖的上传路径（**一次覆盖全部**，不是只改图片那条）

| 路径 | 入口 | attachment_id |
|---|---|---|
| 任意文件 | `uploadFile` | `file` |
| 相机图片 | `uploadCameraAsset` → `handleImageUploadPresign` | `image` |
| 相册图片 | `uploadSelectedAsset` → `handleImageUploadPresign` | `image` |
| FilePicker 图片（华为 ROM fallback） | `_uploadImagePlatformFile` | `image` |
| 相机/相册视频 | `uploadVideoViaPresign`（**仅本体**） | `video` |
| 语音 | `handleVoiceSelection` | `voice` |
| 位置快照 | `handleLocationSelection` | `location_thumb` |

`handleImageUploadPresign` / `handleVideoUpload` / `handleSelectedVideoUpload`
新增的是**可选命名参数**（`messageId` / `seal`），旧调用形状原样保留
（既有 `test/page/chat/chat/attachment_handler_test.dart` 一行未改仍 212 绿）。

非上传的构造点（表情 / 名片 / 红包 / 转账 / 收藏转发）**刻意未动**：
它们不产生附件对象，提前生成 id 对它们没有任何意义。

---

## 2. 三个不得不做的判断（都不是"顺手"）

### 2.1 ⚠️⚠️ 绑定值的 `conversation_id` **不能用 `conversationUk3`**

方案甲要求三个值「全设备一致」。`conversationUk3` **不满足**：

```
ConversationUk3Generator._generateGroupUk3  →  'C2G_<currentUserId>_<groupId>'
```

群会话的 uk3 **含本机 uid，逐用户不同**。拿它算绑定值，
**除发送者外没有任何人能算出同一个 AAD** —— 群附件对所有收件人直接不可读，
而且这种失败是"密文完好、就是打不开"，最难排查。

本刀改用上传 scope 的 `scope_ref`（同一个 `_uploadScope`，不新造概念）：

| 会话 | conversation_id | 两端一致？ |
|---|---|---|
| C2C | `c2c:<min>:<max>`（BigInt 归一化） | ✅ |
| C2G | `<group_id>` | ✅ |
| C2S / 未知 | `''`（scopeRef 为 null） | 闸门判 `missingBinding` → 不封装 |

> 这一条是**开工时才发现**的：Slice 4b 的闸门只要求「conversationId 非空」，
> 非空的错值它拦不住。**「非空」不等于「两端一致」。**

### 2.2 ⚠️ 判据必须抄 `sendWsMsg` 实际用的那个，不是 `encryptPayload` 内部那个更宽的

`encryptPayload` 内部的 needEncrypt 是
`groupMegolm || shouldEncryptOutgoingPayload(chatType)`。
**但 `sendWsMsg` 在 `shouldEncryptOutgoingPayload` 为假时根本不会调用它**
（`chat_network_service.dart` 直接走 `finalPayload = payloadWithTs` 明文分支）。

我一度打算把 `groupMegolm` 并进封装判据（"这样群 E2EE 的附件也能加密"）。
**那会是一个真实的密钥泄漏**：群级 Megolm 开着、全局策略是明文时，
封装会被批准，而 payload 实际以明文出网 —— content key 明文送达服务端。

⇒ 封装判据固定为 `E2EEService.shouldEncryptOutgoingPayload(type)`，
与 `sendWsMsg` 同源。**代价**：C2G-Megolm-但全局明文的会话不封装附件
（fail-closed，退回今天行为，不是泄漏）。

> 又一次「必须看调用方」：只读 `encryptPayload` 会得出相反的结论。

### 2.3 消息体里的 `file_hash256`：封装后必须换成**密文**哈希

原先 meta 一律回明文 SHA-256。封装后服务端存的是密文哈希，
而 `user_collect.attach_file_hash256` ↔ `attachment.file_hash256` 是**按值 JOIN**
（Slice 5 已核实），孤儿清理也依赖同一张表。
消息体继续带明文哈希 ⇒ 收藏引用计数对不上，
**最坏情况：对象被判定无人引用而被清理掉**。

⇒ `AttachmentSealRequest` 新增回填字段 `uploadedFileHash256`
（= 实际上报给服务端的那个值），meta 用它。
明文哈希只留在 descriptor 的 `plain_sha256` 里（拍板 ①）。
`size` 仍是**明文**大小 —— 它进的是加密 payload，给 UI 显示用。

---

## 3. 本刀新造出来的两个面，同刀补上了闸门

descriptor 带 **content key**。它一旦存在，就有两条以前不存在的出网路径：

| 路径 | 失效场景 | 补的闸门 |
|---|---|---|
| `sendWsMsg` 明文分支 | 上传时策略要求加密（封装了），几秒后发送时策略已翻明文 → 钥匙明文出网 | `AttachmentSealPolicy.carriesContentKey(payload)` → 拒发 |
| `MessageRetry` 重发 | 该行在库里躺几小时后被重发，`encryptionRequired` 此时为 false → 旧判据放行 | `shouldBlockPlaintextRetry(carriesContentKey: ...)`，**不看** `encryptionRequired` 就拦 |

`carriesContentKey` 入参刻意是 `Object?`：库里读出的 payload 常是
`Map<dynamic, dynamic>`，要求 `Map<String, dynamic>` 会让调用点各写一次强转，
而**强转失败静默当成「不带钥匙」正是这道闸门最怕的失效方式**。

---

## 4. ⚠️⚠️ 推出开关默认关闭 —— 为什么

`kAttachmentSealRolloutEnabled = false`（`attachment_handler.dart`）。

**Slice 6（下载侧解密 + 完整性门）尚未接线。** 今天的读取链路
（`cachedImageProvider` / `IMBoyCacheManager.getSingleFile`）把对象字节直接交给
渲染器，`lib/` 内**没有任何一处调用 `AttachmentEncryptor.open`**。
此时开启封装的后果不是"更安全"，而是 **E2EE 会话里所有新附件对谁都打不开**，
包括发送者自己。

按裁决规则「两种合理实现二选一 → 选安全那个（fail-closed）」，
默认取关闭：代码全部写完并验收，行为逐字节维持今天的明文直传。

- **不是数据丢失**：descriptor 随加密 payload 落库，Slice 6 上线后旧密文仍可解；
- **翻开条件**：Slice 6 合入 + 真机验证通过；
- **翻开动作**：改 `attachment_handler.dart` 那一行 `false` → `true`。
  测试已按 `sealRollout: true` 覆盖开启后的判定路径。

> 若人工判断"宁可先开着"，这是一行的事 —— 但请连同 Slice 6 的时间点一起决定。

---

## 5. 空验证（逐条精确变红，恢复后全绿）

| # | 摘掉的防线 | 变红用例 | 是否精确 |
|---|---|---|---|
| A | `if (!rolloutEnabled) return null;` | **1 红**：「推出开关关闭 → 不封装」 | ✅ 唯独 |
| B | `if (decision is! SealApproved) return null;`（忽略闸门） | **5 红**：payload 不加密 / message_id 空 / conversation_id 空 / sender_uid 空 / 非聊天面 | ✅ 恰是闸门全部判据 |
| C | `sealConversationId` 改用 `conversationUk3` | **3 红**：C2G 两端一致 / C2C 两端一致 / 非聊天面 | ✅ 恰是 §2.1 那条 |
| D | meta 回退成明文哈希 | **1 红**：「封装后消息体带密文哈希，与 confirm 同值」 | ✅ 唯独 |
| E | 去掉 `carriesContentKey && isPlaintextRow` 分支 | **1 红**：「策略说不需加密也必须拦下」 | ✅ 唯独 |

恢复后 23/23 绿。

**正向可用性用例**（防「恒 null / 恒拦下也满分」）：
- 「全部就绪时确实封装，且绑定值就是 `AttachmentBinding.compute` 那个值」；
- 「message_id 不同 → 绑定值不同」（ATT-01 的锚点确实穿过来了）；
- 「带 content key 但已加密的行照常重发」；
- 「未封装时 meta 仍是明文哈希」（旧行为零破坏）。

**⚠️ 一条防线没有用例覆盖**：`sendWsMsg` 明文分支里的 `carriesContentKey` 拒发。
该函数依赖 `MessageRepo`/`AppLoading`/`PolicyGate` 等单例，进不了当前单测。
其**判据**（`carriesContentKey`）已被穷举验收，**分支本身**是
**文件级阅读结论，未实证**。不编造用例、不删除该防线。

---

## 6. 验收

| 门 | 基线 | 本刀 |
|---|---|---|
| `flutter test test/service/e2ee/` | 521 | **539 passed**（+18） |
| `flutter test test/service/` | 1401 | **1419 passed**（+18） |
| `flutter test test/store` | 397 | **397 passed**（未变） |
| `flutter test test/page/chat/chat`（不在门内，本刀改到） | — | **212 passed** |
| `dart analyze lib` | 1 条既有 info | **1 issue**（`ios_settings_ui.dart`，与 E2EE 无关） |

imboy 侧本刀只改文档，`make e2ee-verify` 不适用（基线 385 未动）。

⚠️ `test/page/chat/chat/` **不在任何绿灯门内**——与上一刀发现 `test/store/`
红了三周同类。本刀手动跑了它，但**没有把它加进门**（该决定属测试基建，
不在本刀范围；记为残留）。

---

## 7. 残留风险

1. ⚠️⚠️ **推出开关默认关闭** —— 生产附件路径**依旧明文直传**，
   ATT-01..05 仍不成立。开关翻开前，本刀的效果是「代码就位」而非「附件已加密」；
2. ⚠️ **未真机验证**（按用户决定留作单独一轮）。故本刀标 **PARTIAL**，不标 PASS；
3. ⚠️ **接线点本身没有自动化覆盖** —— 「handler 真的把 `seal` 传进了
   `AttachmentApi`」依赖文件 IO 与静态方法，只有真机腿能证。
   已验收的是判定、绑定输入、meta 哈希语义；
4. ⚠️ `sendWsMsg` 的明文拒发分支未实证（见 §5 末）；
5. **缩略图仍是明文**（视频缩略图、位置快照走独立对象；本刀只封装视频本体）。
   设计 §3.3：缩略图不加密 = 预览即泄漏，**ATT-04 在缩略图上不成立**（Slice 7）；
6. **waveform / duration / width / height** 等元数据随 payload 走：
   加密会话下受保护，非加密会话下明文（与今天一致，未恶化）；
7. **MIME 泄漏旁路按拍板保留**（已知缺口，不得对外宣称附件元数据完全不可见）；
8. **绑定强度弱于原设计**（方案甲，已接受）；
9. C2G-Megolm-但全局策略明文的会话不封装附件（§2.2，fail-closed 取舍）；
10. 整文件入内存，100MB 上限下低端机未实测（Slice 4a 起的老残留）；
11. **未与后端做真实的密文 confirm 往返**（Slice 5 的真 PG 测试是后端侧单测）。

## 8. 认识论状态

| 结论 | 状态 |
|---|---|
| 闸门判定矩阵、绑定值输入、conversation_id 两端一致性 | **已实证**（15 例 + 5 条空验证） |
| meta 的 `file_hash256` 与 confirm 同值 | **已实证**（走注入 seam，不触网） |
| 重发闸门的 content key 维度 | **已实证**（3 例 + 空验证） |
| `sendWsMsg` 明文拒发分支 | **文件级阅读结论，未实证** |
| 「`groupMegolm` 并进判据会造成泄漏」 | **文件级阅读结论**（读 `sendWsMsg` 调用点得出），未构造运行时复现 |
| 「收藏引用计数按值 JOIN，哈希不一致会断」 | **沿用 Slice 5 的推理结论**，仍未构造收藏场景实测 |
| Slice 6 缺席 ⇒ 开启封装会让附件不可读 | **已实证**（`grep` 全 `lib/` 零处调用 `AttachmentEncryptor.open`） |
