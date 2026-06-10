# 消息引用回复字段映射分析 / Quote Reply Field Mapping Analysis

> 生成时间 / Generated: 2026-05-27  
> 分析范围 / Scope: 后端 Erlang + Flutter 客户端  

---

## 1. 后端字段定义 / Backend Field Definition

### ✅ 字段存储方式

后端（Erlang/OTP）**不在服务端单独存储引用字段**，引用内容完整嵌入在消息的 `payload` 中随消息体一起持久化和转发。

**消息路由路径**：`C2C` 类型消息经 `message_router_logic.erl` 路由后，由 `message_ds:send_next/4` 投递到目标用户在线设备，并持久化到数据库。

### ✅ 字段名称（payload 内嵌套）

后端消息 `payload` 中引用相关字段：

| 字段名 | 类型 | 说明 |
|--------|------|------|
| `quote_msg` | `Map` (JSON Object) | 被引用消息的完整 `flutter_chat_core Message` JSON 序列化对象 |
| `quote_msg_author_name` | `String` | 被引用消息作者的显示名称 |
| `quote_text` | `String` | 引用气泡上方展示的新消息正文 |

**消息顶层字段**：`msg_type` = `"quote"`（v2.0 协议规范）

⚠️ 后端 `message_ds.erl` 中无针对引用字段的额外验证逻辑，仅通过 `validate_message_by_type` 检查 `msg_type` 非空，引用字段内容完全由客户端写入并原样转发。

---

## 2. Flutter 客户端字段解析 / Flutter Field Parsing

### ✅ 消息构建（发送侧）

**发送入口**（双路径，逻辑一致）：

**路径 A** — `chat_page.dart:_sendQuoteMessage()`：
```dart
metadata: {
  'msg_type': 'quote',
  'peer_id': widget.peerId,
  'quote_msg': quoteMessage?.toJson(),      // 完整消息 JSON
  'quote_msg_author_name': quoteMsgAuthorName,
  'quote_text': text,                        // 新消息正文
}
```

**路径 B** — `message_handling_service.dart:createQuoteMessage()`：
```dart
final metadata = {
  'msg_type': 'quote',
  'peer_id': peerId,
  'quote_msg': quoteMessage.toJson(),
  'quote_msg_author_name': ...,
  'quote_text': text,
};
```

两路径字段完全对齐 ✅。

### ✅ 消息渲染（接收侧）

**渲染组件**：`message_quote_builder.dart` → `QuoteMessageBuilder`

```dart
Map<String, dynamic> quoteMsgMap =
    message.metadata?['quote_msg'] as Map<String, dynamic>? ?? {};
String text = message.metadata?['quote_text'] as String? ?? '';
```

渲染流程：
1. 从 `metadata['quote_msg']` 反序列化为 `flutter_chat_core Message` 对象
2. 上方展示 `quote_text`（新消息正文）
3. 下方气泡展示被引用消息作者名 + 时间 + 内容预览
4. 点击气泡触发 `onQuoteTap(quoteMsg.id)` 跳转定位

### ✅ `message_model.dart:toTypeMessage()` 中的 quote 处理

```dart
} else if (effectiveMsgType == MessageType.quote) {
  message = CustomMessage(
    authorId: author.id,
    id: safeId,
    createdAt: createdDt,
    metadata: {...metadata, ...payloadData},  // payloadData 包含 quote_msg/quote_text
  );
}
```

`payloadData` 会将 `quote_msg`、`quote_text`、`quote_msg_author_name` 合并到 `metadata`，渲染侧可直接读取。

---

## 3. 引用内容截断逻辑 / Content Truncation Logic

### ✅ 文本消息截断

`message_quote_builder.dart:_buildQuoteMessageContent()` 中文本内容截断：

```dart
if (quoteMsg is TextMessage)
  return Text(
    quoteMsg.text,
    style: style,
    maxLines: 2,           // ✅ 最多 2 行
    overflow: TextOverflow.ellipsis,  // ✅ 超出省略
  );
```

### ⚠️ 非文本消息截断

非文本类型通过图标+标签替代，无截断问题，但覆盖范围有限：

| 类型 | 处理方式 |
|------|---------|
| `ImageMessage` | 图标 + "[图片]" |
| `FileMessage` | 图标 + 文件名（maxLines: 1）|
| `voice` | 图标 + "[语音消息]" |
| `video` | 图标 + "[视频消息]" |
| `location` | 图标 + 位置标题（maxLines: 1）|
| `visitCard` | 图标 + "[名片]" |
| `revoked` | 图标 + "[消息已撤回]" |

⚠️ **待改进**：`CustomMessage` 中 `revoked` 类型的 `quote_text` 字段在会话列表副标题显示为 `'[引用]'`（`message_model.dart:conversationSubtitleFromModel` 中 `MessageType.quote` case 仅读取 `payload['quote_text']`），当被引用消息被撤回后会话列表副标题不会更新反映撤回状态。

---

## 4. 引用消息被删除/撤回后的展示 / Handling Revoked/Deleted Quoted Messages

### ⚠️ 部分处理

**已实现**：`QuoteMessageBuilder` 包含错误兜底 `_buildQuoteErrorWidget()`：

```dart
if (quoteMsgMap.isEmpty)
  return _buildQuoteErrorWidget(context, userIsAuthor, isDark);

late Message quoteMsg;
try {
  quoteMsg = Message.fromJson(quoteMsgMap);
} catch (e) {
  return _buildQuoteErrorWidget(context, userIsAuthor, isDark);
}
```

错误 UI 展示：红色叹号 + `t.common.quoteMessageNotAvailable`（"引用内容不可用"）。

**⚠️ 缺失**：当被引用消息被撤回时（状态变为 `peerRevoked`/`myRevoked`），`quote_msg` 对象仍保存在发送时快照中，**不会自动更新为已撤回状态**。渲染时仍会按原始内容展示，而非显示"[消息已撤回]"。

**❌ 缺失**：服务端无主动推送机制通知"被引用消息已撤回"，客户端也无本地 `quote_msg` 内容追溯更新逻辑。

---

## 5. 字段映射总结 / Field Mapping Summary

```
发送方 Flutter                后端(纯转发)            接收方 Flutter
─────────────────────         ────────────            ──────────────────────
msg_type: "quote"        →    原样透传(JSON)    →     effectiveMsgType == "quote"
quote_msg: {...}              (不解析/不存储)          metadata['quote_msg']
quote_text: "..."                                     metadata['quote_text']
quote_msg_author_name: "..."                          metadata['quote_msg_author_name']
```

---

## 6. 问题清单 / Issue List

| # | 状态 | 描述 |
|---|------|------|
| 1 | ✅ 已实现 | 引用消息字段命名前后端一致（`quote_msg`/`quote_text`/`quote_msg_author_name`） |
| 2 | ✅ 已实现 | 引用文本内容 maxLines:2 截断 + ellipsis |
| 3 | ✅ 已实现 | 引用消息解析失败时的错误 UI 兜底 |
| 4 | ✅ 已实现 | 非文本类型（图片/语音/视频/文件/位置）的图标替代展示 |
| 5 | ✅ 已实现 | `onQuoteTap` 点击跳转到被引用消息 |
| 6 | ⚠️ 待改进 | 会话列表中引用消息副标题固定显示 `quote_text`，被引用方撤回后不更新 |
| 7 | ⚠️ 待改进 | 被引用消息撤回后，已有引用气泡仍展示原始内容（无实时更新） |
| 8 | ❌ 缺失 | 后端无 `quote_msg_id` 字段引用原始消息 ID，只做快照嵌入，无法服务端追溯原消息状态 |
| 9 | ❌ 缺失 | 服务端无"被引用消息撤回通知"推送（S2C）给引用方，双端均无同步刷新机制 |

---

## 7. 改进建议 / Recommendations

**短期**：
- 在 `QuoteMessageBuilder` 中检查 `quote_msg` 内的 `status` 字段，若已撤回则渲染 "[消息已撤回]" 而非原始内容。

**长期**：
- 后端 payload 中增加 `quote_msg_id` 字段（仅存消息 ID），在渲染时按需查询最新状态，避免快照与实时状态不一致。
- 在 C2C_REVOKE S2C 通知中增加 `quoted_by` 字段，触发引用方客户端刷新对应引用气泡。
