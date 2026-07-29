# E2EE-061 Slice 6（上半）——接收侧开封：绑定值重算 + ATT-01/02/03 端到端

- 日期：2026-07-30
- 仓库：imboyapp（代码）、imboy（本文档）
- 状态：**PARTIAL**。开封内核与两端一致性已端到端实证；
  **尚未接进真实读取链路**（`IMBoyCacheManager` / 各 message builder），
  且**未真机验证** ⇒ 不标 PASS。

---

## 1. 为什么这是 Slice 6 的第一刀

Slice 4 把封装接进了发送路径，但**接收侧一行都没有**：
`grep` 实证 `lib/` 内零处调用 `AttachmentEncryptor.open`。
这也是 Slice 4 的推出开关必须默认关闭的原因。

本刀补上接收侧的**内核与推导**，并第一次把两侧对接起来跑通。
把「拿字节」（缓存管理器 / 各 builder）留到下半刀，是因为那部分依赖
网络与 `path_provider`，进不了单测；而**绑定值推导只要错一处，
密文完好也打不开**，必须先用可穷举的方式钉死。

---

## 2. 交付物

| 文件 | 内容 |
|---|---|
| `lib/service/e2ee/attachment_conversation_ref.dart`（新） | `conversation_id` 的**单一真值源**，发送侧与接收侧共用 |
| `lib/service/e2ee/attachment_opener.dart`（新） | `descriptorFrom` / `bindingFor` / `openForMessage` |
| `lib/page/chat/chat/attachment_handler.dart` | `_c2cConvKey` 下沉到共用模块（行为不变） |
| `test/service/e2ee/attachment_open_e2e_test.dart`（新，22 例） | 端到端 seal↔open + ATT-01/02/03 + 兼容性 |

### 2.1 `conversation_id` 只留一份实现

发送侧原来算 `c2c:<min>:<max>` 的 `_c2cConvKey` 是 `attachment_handler` 的私有方法。
接收侧要重算同一个值 —— **抄一份就是埋一颗雷**：两份实现哪怕只在
「非数字时回退字符串序」这种边角上分歧，就会变成
「上传 scope 与绑定值对不上」的隐性错配，且只在特定 uid 组合下出现。

⇒ 实现下沉到 `AttachmentConversationRef.c2cKey`，`deriveUploadScope` 调它。
既有 11 例 `attachment_upload_scope_test.dart` 一行未改仍全绿 ⇒ 行为未变。

| 会话 | conversation_id | 两端一致 |
|---|---|---|
| C2C | `c2c:<min>:<max>`（BigInt 归一化） | ✅ 与收发方向无关 |
| C2G | `<group_id>` | ✅ 每个群成员都算得出 |
| C2S / 未知 | `null` → 抛异常 | 不拿空串兜底 |

### 2.2 两条 fail-closed 取舍

| 情形 | 处置 | 理由 |
|---|---|---|
| payload 里**没有** descriptor | 返回 `null`，调用方照旧直读 | 设计 §4：历史明文附件必须仍可读 |
| descriptor **存在但坏掉** | **抛异常**，绝不退回明文直读 | 静默降级 = 给攻击者一条把加密关掉的开关：把 descriptor 改成"看起来不像 descriptor"，就能让客户端当明文渲染 |

`bindingFor` 在推不出会话标识时抛异常而**不**用空串：
空串会让同一发送者的所有附件共用一个绑定值，**ATT-01 直接失效**
（与 E2EE-025 `sessionRef: ''` 那次事故同类）。

---

## 3. 端到端验收：两端一致性是**跑出来的**，不是推出来的

测试的发送侧走 `ChatAttachmentHandler.buildSealRequest` + 真实的
`sealConversationId`；接收侧只拿**消息字段**（`id` / `type` / `from` / `to`）
重算，**不碰发送侧的任何变量**。正向用例通过 ⇒ 两端确实算出同一个绑定值。

- **C2C**：收件人重算即可开封，明文逐字节相同；
- **⚠️ C2G**：测试里显式断言「第三个群成员算出的 `conversationUk3` 与发送者
  完全不同」，然后仍能开封 —— 这正是 Slice 4 那条
  「不能用 uk3」的**运行时证据**，不再只是读代码得出的结论；
- 多块 + 末块不满同样完整还原。

### ATT 矩阵

| 项 | 用例 | 结果 |
|---|---|---|
| **ATT-01** | 搬到另一条消息 / 另一个会话 / 冒充另一个发送者 | 3 例全部拒绝 |
| **ATT-02** | 翻一个 bit / 两块互换 / 截断一块 / 追加一块 | 4 例全部拒绝 |
| **ATT-03** | 改 `plain_size` / `chunk_count` / `attachment_id` / `content_key` | 4 例全部拒绝 |
| ATT-03 边界 | 改 `mime` | ⚠️ **本层拦不住**（见下） |

⚠️ **如实记录一条能力边界**：`mime` / `name` **不进块 AAD**，
附件分块 AEAD 拦不住它们被改。它们的完整性由 **PFv3 对整个 payload 的认证**
保证（descriptor 住在加密 payload 内）。用例断言的是「改 mime 也拿不到
别的内容」，即它不构成解密侧的口子 —— 但**不得**把 ATT-03 整条说成
"由附件加密保证"。

---

## 4. 空验证（逐条精确变红，恢复后全绿）

| # | 摘掉的防线 | 变红用例 | 备注 |
|---|---|---|---|
| F | `bindingFor` 改用固定常量（不含 message_id/会话/发送者） | **5 红**：3 条正向可用性 + mime 对照组 + `Map<dynamic,dynamic>` 解析 | ⚠️ 见下 |
| G | descriptor 坏掉时静默返回 null | **3 红**：改 `plain_size` / 改 `chunk_count` / 显式 fail-closed 用例 | 精确 |
| H | `AttachmentConversationRef` 忽略 `chatType`（一律走 c2c 键） | **2 红**：C2G 端到端 + conv_ref 单测 | 精确 |

⚠️⚠️ **F 是本刀最有价值的一条**：把绑定值换成固定常量后，
**ATT-01 的三条负向用例全部照旧通过**（该拒的还是拒了），
唯独**正向用例**塌方。这正是「只验篡改能否拒收无效——全拒的实现恒满分」
的现场复现：如果本刀只写了 ATT-01/02/03 而没写正向可用性用例，
一个绑定值算错、谁的附件都打不开的实现会拿到满分。

---

## 5. 验收

| 门 | 上轮 | 本刀 |
|---|---|---|
| `flutter test test/service/e2ee/` | 539 | **561 passed**（+22） |
| `flutter test test/service/` | 1419 | **1441 passed**（+22） |
| `flutter test test/store` | 397 | **397 passed** |
| `flutter test test/page/chat/chat` | 212 | **212 passed** |
| `flutter test test/page/chat/attachment_upload_scope_test.dart` | — | **11 passed**（证明 `_c2cConvKey` 下沉行为未变） |
| `dart analyze lib` | 1 条既有 info | **1 issue**（同基线） |

imboy 侧仅文档，`make e2ee-verify` 不适用（385 未动）。

⚠️ **顺带发现（未修）**：`test/page/chat/`（父目录，**不在任何门内**）有
**3 条预存失败** —— `widget/extra_item_test.dart` 找不到「媒体」「群投票」
等 i18n 文案。相关文件（`lib/page/chat/widget/extra_item.dart` 与该测试）
**本线两次提交均未触碰**（`git show --stat` 实证），失败形态是 widget
finder 找不到 i18n 文本，与附件无关。另有两个文件在并行跑时报 `loading [E]`，
单独跑全绿（疑似并行加载抖动）。
**本刀未修、也未把该目录加进门** —— 属测试基建，另开一刀。
这是继 `test/store/` 之后**第二个**门外目录藏红的实例。

---

## 6. 残留风险

1. ⚠️ **尚未接进真实读取链路** —— `IMBoyCacheManager.getSingleFile` 只收一个
   URL，拿不到 descriptor 与消息字段；各 message builder（image/audio/video/
   file/location）都要传下来。**这是下半刀**，也是翻开推出开关的最后一环；
2. ⚠️ **未真机验证**（真机腿在停放区）⇒ 标 PARTIAL；
3. **临时明文的落盘与生命周期未处理**（Slice 8）：接线后缓存里会是明文，
   与今天所有附件都明文落盘一致，但设计 §2.2 要求「失败时删除临时明文」，
   须在下半刀一并考虑；
4. `mime` / `name` 不受块 AAD 保护（§3 能力边界，已记录）；
5. **缩略图仍明文**（Slice 7）；
6. 整文件入内存：`open` 一次性拼出整个明文，100MB 上限下低端机未实测；
7. 与后端无交互（本刀纯客户端）。

## 7. 认识论状态

| 结论 | 状态 |
|---|---|
| 发送侧封装 ↔ 接收侧开封两端一致（含 C2G 第三成员） | **已实证**（端到端 22 例） |
| ATT-01 / ATT-02 / ATT-03（除 mime/name）成立于本层 | **已实证** |
| `mime` / `name` 不受块 AAD 保护、靠 PFv3 payload 认证 | **已实证**（对照组用例）+ 文件级阅读结论（PFv3 那半） |
| `_c2cConvKey` 下沉未改变 `scope_ref` 行为 | **已实证**（既有 11 例未改仍绿） |
| `test/page/chat/` 三条红是预存且与本线无关 | **证据强但非旧树对照**：相关文件两次提交均未触碰、失败是 i18n finder 未命中 |
| 接线后真实读取链路可用 | **未实证**（下半刀 + 真机） |
