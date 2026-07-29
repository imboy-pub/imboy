# E2EE-061 Slice 7 —— 缩略图加密（设计 §3.3）

- 日期：2026-07-30
- 仓库：imboyapp（代码）、imboy（本文档）
- 状态：**PARTIAL**。封装/开封已实证；**推出开关仍关**、未真机验证 ⇒ 不标 PASS。

---

## 1. 为什么这一刀不能省

设计 §3.3：缩略图是**独立对象**。只加密视频本体而让缩略图明文 =
**预览即泄漏** —— 拿到缩略图对象就看得到画面内容，**ATT-04 在缩略图上直接失败**。

比"两个都明文"更坏的地方在于：那种状态**看起来像已加密**。
用户以为视频受保护，实际服务端与任何拿到对象的人都能看到那一帧。

Slice 4 当时刻意只封装了视频本体并把这条记成残留；本刀补上。

## 2. 改动

| 文件 | 内容 |
|---|---|
| `lib/service/e2ee/attachment_encryptor.dart` | `seal` 新增可选 `thumb` 参数（已封装好的缩略图 descriptor 挂进主 descriptor） |
| `lib/store/api/attachment_api.dart` | `AttachmentSealRequest.thumbDescriptor`；`uploadVideoViaPresign` 新增 `thumbSeal` + 同生同灭闸门 |
| `lib/service/e2ee/attachment_seal_policy.dart` | `sealTogether(main, thumb)` —— 闸门判据的单一入口 |
| `lib/page/chat/chat/attachment_handler.dart` | **两个**视频调用点各传 `thumbSeal: _sealFor(messageId, 'video_thumb')` |
| `test/service/e2ee/attachment_thumb_seal_test.dart`（新，7 例） | 验收 |

### 2.1 顺序：先封缩略图，再封本体

`uploadVideoViaPresign` 里缩略图先上传（拿到自己的 `object_key` 与 descriptor），
然后 `videoSeal.thumbDescriptor = thumbSeal.descriptor`，视频封装时挂进去。
反过来做不到 —— descriptor 必须带 `object_key`，而 `object_key` 来自 presign。

### 2.2 同生同灭闸门

```dart
if (!AttachmentSealPolicy.sealTogether(videoSeal, thumbSeal)) {
  videoSeal = null; thumbSeal = null;   // 两个都不封装
}
```

只传其一时**两个都退回明文**，而不是交付「本体加密、预览裸奔」的假象。
判据放在 `AttachmentSealPolicy`（与封装判定同一个模块），不在调用点各写一份 `if`。

### 2.3 接收侧零改动

`AttachmentOpenRegistry.registerFromMessage` 在 Slice 6 就已经把
`descriptor.thumb` 按**它自己的 object_key** 一并登记 —— 本刀不需要动读取侧，
两个对象各自可开。这条现在有了用例。

### 2.4 密钥独立由构造校验强制

`AttachmentDescriptor` 的构造校验（Slice 3）已经强制
「thumb 必须有独立 content_key / base_nonce、不得再嵌套 thumb」。
本刀不重复判断，只加了一条用例把它钉在**封装入口**上。

---

## 3. 空验证（逐条精确变红，恢复后 7/7 绿）

| # | 摘掉的防线 | 变红用例 |
|---|---|---|
| L | `seal` 丢弃 `thumb` 参数 | **5 红**：thumb 存在 / 往返 / 独立密钥拒绝 / 两个对象可开 / 搬走被拒 |
| M | 登记表不登记 `descriptor.thumb` | **2 红**：两个对象可开 / 缩略图搬走被拒 |
| N | `sealTogether` 恒 `true` | **1 红**：只有其一 → 不一致 |

正向可用性：「登记一条视频消息 → 视频与缩略图**各自**可开封、明文逐字节相同」
——防「一律拒绝也满分」。

---

## 4. 验收

| 门 | 上轮 | 本刀 |
|---|---|---|
| `flutter test test/service/e2ee/` | 571 | **578 passed**（+7） |
| `flutter test test/service/` | 1451 | **1458 passed**（+7） |
| `flutter test test/store` | 397 | **397 passed** |
| `flutter test test/page/chat/chat` | 212 | **212 passed** |
| `dart analyze lib` | 1 条既有 info | **1 issue**（同基线） |

imboy 侧仅文档，`make e2ee-verify` 不适用（385 未动）。

---

## 5. 残留风险

1. ⚠️ **推出开关仍关**（`kAttachmentSealRolloutEnabled = false`）⇒
   生产附件路径依旧明文直传。至此**发送封装 / 接收开封 / 读取链路 / 缩略图**
   四块都已就位，**剩下的就是真机验证 + 人工点头翻开关**；
2. ⚠️ **未真机验证** ⇒ PARTIAL；
3. ⚠️ **`uploadVideoFileViaPresign`（File 版，频道发布用）未接线** ——
   它服务的是频道内容，不是 E2EE 会话，本刀刻意未动；
   若将来频道要 E2EE，这条要一起补；
4. **位置快照**在 Slice 4 已作为主对象封装（`location_thumb`），
   它没有二级缩略图，不受本刀影响；
5. **图片消息的 `thumbhash`** 是内联占位串不是独立对象，随 payload 加密，
   不在本刀范围；
6. ⚠️ **音频 waveform** 仍随 payload 走（加密会话下受保护、非加密会话下明文），
   设计 §3.3 把它与 OCR/EXIF/推送摘要一并点名 —— **本刀未处理**；
7. 缩略图与本体**分两次上传**：其中一次失败会留下孤儿对象，
   与今天的行为一致（孤儿清理依赖 `attachment` 表），未恶化但也未改善。

## 6. 认识论状态

| 结论 | 状态 |
|---|---|
| thumb descriptor 随主 descriptor 送达、密钥独立、往返不丢 | **已实证**（7 例 + 3 条空验证） |
| 两个对象在接收侧各自可开 | **已实证** |
| 同生同灭闸门 | **已实证**（纯函数） |
| `uploadVideoViaPresign` 里闸门与顺序的真实编排 | **文件级阅读结论，未实证**（依赖 `AssetEntity`/`VideoCompress`，进不了单测） |
| 孤儿对象行为未恶化 | **推理，未实证** |
