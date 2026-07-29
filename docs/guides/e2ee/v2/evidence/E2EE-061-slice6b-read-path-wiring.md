# E2EE-061 Slice 6（下半）——接进读取链路：登记表 + 下载漏斗开封

- 日期：2026-07-30
- 仓库：imboyapp（代码）、imboy（本文档）
- 状态：**PARTIAL**。登记与开封逻辑已实证；**漏斗内的调用顺序只有真机能证**，
  且推出开关仍关 ⇒ 不标 PASS。

---

## 1. 为什么是登记表，不是一路传参

下载漏斗 `IMBoyCacheManager.getSingleFile(url)` **只收一个 URL**。它上面压着：

- `IMBoyCachedImageProvider`——其**身份与相等性就是 `url`**（`operator ==` / `hashCode`），
  加一个 descriptor 参数就得同时改它的缓存键语义；
- 9 个调用点（chat_provider / chat_audio_handler / message.dart /
  message_audio_builder_mobile / video_viewer / markdown / channel_message_item /
  image provider / 自调用），沿途还有各 message builder 的构造参数。

而其中**多数调用点永远不会是加密附件**（头像、频道封面、markdown 图）。
为了少数几条路径改九处传参与一串 widget 参数，是典型的把面铺大。

取而代之：消息在**唯一**的转换入口
`MessageModel.toTypeMessage()`（`message_model_mapper.dart`）被解密后，
顺手登记 `object_key → {descriptor, bindingHash}`；漏斗按 `object_key` 查。
**一个登记点 + 一个查询点**，替代九处传参。

登记点的位置是关键：它排在 v3/v1 解密之后、消息构造之前，
`payloadData` 此刻**已是明文**，descriptor 就在里面。

## 2. 漏斗里的顺序（不能反）

```
下载 rawDownloaded
  → materialize(object_key, bytes)   ← 开封在这里
  → validateImageData 魔数校验
  → _crossCache.set(cacheKey, 明文)
```

- **必须先开封再校验**：密文过不了图片魔数校验，会被当成"损坏"反复重下 3 次；
- **缓存里必须是明文**：命中缓存那条路径不再开封（也无从判断该不该开封）。
  ⇒ 明文落盘，与今天所有附件一致；生命周期归 Slice 8；
- **开封失败不重试、不落缓存**：重下同一个对象不会变好；
  失败时 best-effort 删掉 `downloadAndSave` 顺手写下的那份密文缓存。
  这条排在 404 判定**之前**，避免开封异常文本里的数字被
  `_isNotFoundError` 的字符串匹配误判。

## 3. ⚠️ 已知代价：登记表是内存表

冷启动即空。若某个密文对象在其消息**尚未**经 `toTypeMessage()` 时就被下载，
查不到 spec，字节会被**当明文交给渲染器** —— 结果是**坏图，不是泄漏**
（密文本身不可读）。实际链路里消息转换恒在渲染之前，但预取/分享等路径不保证。

持久化登记归 Slice 8 一并处理（与临时明文生命周期同一刀）。
本刀在测试里为这条**显式留了一个用例**（「登记表未命中 → 原样返回」），
把它钉成**已知行为**而不是无人知晓的洞。

**投毒**（另一条消息声称同一个 `object_key` 并给出自己的 descriptor）
只会让开封失败（AAD/tag 对不上）→ 拒绝渲染，**不泄露内容**。

## 4. 空验证（编译可通过的变异，逐条精确变红）

| # | 摘掉的防线 | 变红用例 |
|---|---|---|
| I | `materialize` 从不开封（等于没接线） | **4 红**：两条正向 + 两条 fail-closed 抛出 + 重复登记 |
| J | 去掉容量上限 | **1 红**：容量用例 |
| K | 登记键写错（不用 `descriptor.object_key`） | **6 红**：所有依赖命中的用例 |

⚠️ 第一次做 I / K 时用了 `|| true` 与字符串插值变异，**文件编译不过**，
测试在 `loading` 阶段就红了 —— 那种"红"没有任何信息量。
已改成编译可通过的变异重做。**编译失败的空验证不算空验证。**

恢复后 10/10 绿。

## 5. 验收

| 门 | 上轮 | 本刀 |
|---|---|---|
| `flutter test test/service/e2ee/` | 561 | **571 passed**（+10） |
| `flutter test test/service/` | 1441 | **1451 passed**（+10） |
| `flutter test test/store` | 397 | **397 passed** |
| `flutter test test/page/chat/chat` | 212 | **212 passed** |
| `flutter test test/modules` | — | **4 passed**（`toTypeMessage` 登记点未破坏既有转换用例） |
| `dart analyze lib` | 1 条既有 info | **1 issue**（同基线） |

imboy 侧仅文档，`make e2ee-verify` 不适用（385 未动）。

## 6. 残留风险

1. ⚠️ **漏斗内的调用顺序未实证** —— `getSingleFile` 依赖网络、`CrossCache`、
   `path_provider`，进不了单测。已验收的是登记/开封逻辑；
   「先开封再校验、失败不重试不落缓存」是**文件级阅读结论**，须真机验证；
2. ⚠️ **登记表是内存表**，冷启动/预取路径未命中 ⇒ 坏图（§3，已用例钉死）；
3. ⚠️ **未真机验证** ⇒ PARTIAL；
4. **推出开关仍关**（`kAttachmentSealRolloutEnabled = false`）⇒
   生产附件路径依旧明文直传。翻开前还需：真机验证 + Slice 7（缩略图）取舍；
5. **明文落盘**：缓存里是明文，与今天一致；临时明文生命周期归 Slice 8；
6. 视频播放器 / 音频播放器走的是**文件路径**而非字节，本刀未验证它们在
   开封后拿到的临时文件扩展名是否仍正确（`extSource` 用的是 object_key，
   与内容无关，理论上不受影响——**未实证**）；
7. 缩略图仍明文（Slice 7）；`open` 一次性拼出整个明文，低端机未实测。

## 7. 认识论状态

| 结论 | 状态 |
|---|---|
| 登记 → 开封 → 明文；未登记 → 原样放行 | **已实证**（10 例 + 3 条空验证） |
| 绑定值不匹配 / 密文被篡改 → 抛，不返回半截明文 | **已实证** |
| 容量上限生效，长会话不无界增长 | **已实证** |
| 漏斗里「先开封再校验、失败不重试」 | **文件级阅读结论，未实证** |
| 登记表未命中 ⇒ 坏图而非泄漏 | **已实证**（用例显式断言原样返回） |
| 播放器拿到的临时文件扩展名不受影响 | **推理，未实证** |
