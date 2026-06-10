# 黑名单同步 & 频道未读计数对接分析报告
# Blacklist Sync & Channel Unread Count Integration Analysis

> 分析日期 / Analysis Date: 2026-05-27
> 项目路径 / Project Path: `/Users/leeyi/project/imboy.pub/`
> 关联 Issue / Related Issues: #12 好友黑名单同步, #13 频道未读计数

---

## 目录 / Table of Contents

1. [#12 好友黑名单同步分析](#12-好友黑名单同步分析)
2. [#13 频道未读计数分析](#13-频道未读计数分析)
3. [综合问题优先级清单](#综合问题优先级清单)
4. [修复建议](#修复建议)

---

## #12 好友黑名单同步分析

### 涉及文件 / Files Analyzed

| 文件 | 说明 |
|------|------|
| `imboyapp/lib/store/api/denylist_api.dart` | Flutter 远程 API 客户端 |
| `imboyapp/lib/store/repository/user_denylist_repo_sqlite.dart` | Flutter 本地 SQLite 仓库 |
| `imboyapp/lib/page/mine/denylist/denylist_provider.dart` | 黑名单页 Riverpod Notifier |
| `imboyapp/lib/page/mine/denylist/denylist_page.dart` | 黑名单列表页 UI |
| `imboyapp/lib/page/contact/contact_setting/contact_setting_page.dart` | 联系人设置页（包含黑名单开关） |
| `imboyapp/lib/service/message_s2c.dart` | S2C 消息处理（含 `in_denylist` 事件） |
| `imboy/src/api/user_denylist_handler.erl` | Erlang 后端 Handler |
| `imboy/src/imboy_router.erl` | 路由定义 |

---

### 后端接口 / Backend Endpoints

后端路由定义了以下接口（同时存在旧版和 v1 前缀版本）：

| 路由 | Handler Action | 说明 |
|------|---------------|------|
| `POST /friend/denylist/add` | `add` | 添加黑名单 |
| `POST /friend/denylist/remove` | `remove` | 移除黑名单 |
| `GET /friend/denylist/page` | `page` | 分页查询黑名单 |
| `POST /v1/friend/denylist/add` | `add` | 同上（v1 版） |
| `POST /v1/friend/denylist/remove` | `remove` | 同上（v1 版） |
| `GET /v1/friend/denylist/page` | `page` | 同上（v1 版） |

Handler 响应格式：
- `add`：返回 `{user_id, denied_user_id, created_at}`
- `remove`：返回空成功响应
- `page`：返回分页列表（由 `user_denylist_logic:page/3` 生成）

---

### Flutter 端实现现状 / Flutter Implementation Status

#### ✅ 已实现功能 / Implemented

1. **API 调用层完整（`denylist_api.dart`）**
   - `page()` — 分页拉取服务端黑名单列表
   - `add()` — 调用 `/friend/denylist/add`
   - `remove()` — 调用 `/friend/denylist/remove`

2. **本地 SQLite 仓库完整（`user_denylist_repo_sqlite.dart`）**
   - `page()` / `search()` — 本地分页/关键字搜索
   - `insert()` / `update()` / `delete()` / `deleteForUid()`
   - `findByDeniedUid()` — 精确查找
   - `inDenylist()` — 检查是否在黑名单
   - `save()` — upsert（先查后插/更新）
   - `count()` — 总计数

3. **黑名单页面（`denylist_page.dart` + `denylist_provider.dart`）**
   - `loadData()` — 冷启动优先读本地 SQLite，本地为空时拉服务端
   - `addDenylist()` — API add → 本地 insert → 隐藏联系人 → 隐藏会话
   - `removeDenylist()` — API remove → 本地 delete → 恢复联系人 → 恢复会话
   - `inDenylist()` — 静态方法供其他模块调用
   - 支持 A-Z 拼音排序（azlistview）
   - 支持滑动删除（Dismissible）

4. **S2C 推送处理**
   - `message_s2c.dart` 处理 `in_denylist` 事件（发送方被对方拉黑时收到通知）

5. **聊天过滤规则（`event_filter_rules.dart`）**
   - 发送消息前检查接收方是否在黑名单中

---

#### ❌ 未实现 / Not Implemented

1. **联系人设置页（`contact_setting_page.dart`）黑名单开关仅写本地 SQLite，未调用服务端 API**
   - `_handleDenylistToggle(true)` 只调用 `denylistRepo.insert(model)`
   - `_handleDenylistToggle(false)` 只调用 `denylistRepo.delete(peerId)`
   - **后端感知不到此操作**，服务端黑名单表和本地 SQLite 将产生永久不一致

2. **无定期后台同步（Periodic Sync）**
   - 全局代码中无 `Timer.periodic` 或定时任务驱动黑名单同步
   - 仅在打开黑名单列表页（`initState` → `loadData`）时触发一次服务端拉取
   - **多设备登录时**：设备 A 添加/移除黑名单，设备 B 无法感知，重启前一直不同步

3. **无 S2C 推送驱动的黑名单变更同步**
   - 后端没有向其他设备推送 `denylist_add` / `denylist_remove` 的 S2C 事件
   - `in_denylist` 仅是"发送失败回执"，不是主动同步信号

4. **`contact_setting_provider.dart` 的 `toggleDenylist` 也不调用 API**
   - `denylistRepo.delete(peerId)` 直接操作本地，无服务端通知

---

### 数据流分析 / Data Flow Analysis

```
黑名单列表页（denylist_page）:
  loadData()
    ├─ 本地 SQLite 非空 → 直接展示（不同步服务端）[隐患：数据可能过期]
    └─ 本地 SQLite 为空 → GET /friend/denylist/page → 落库 SQLite

黑名单页添加（denylist_provider.addDenylist）:
  ✅ POST /friend/denylist/add → insert SQLite → hide contact → hide conversation

黑名单页移除（denylist_provider.removeDenylist）:
  ✅ POST /friend/denylist/remove → delete SQLite → show contact → show conversation

联系人设置页开关（contact_setting_page._handleDenylistToggle）:
  ❌ 添加：仅 insert SQLite（跳过 API 调用）
  ❌ 移除：仅 delete SQLite（跳过 API 调用）
```

---

## #13 频道未读计数分析

### 涉及文件 / Files Analyzed

| 文件 | 说明 |
|------|------|
| `imboyapp/lib/page/channel/channel_provider.dart` | 频道列表/详情/未读 Provider |
| `imboyapp/lib/service/channel_service.dart` | 频道业务服务层 |
| `imboyapp/lib/store/api/channel_api.dart` | 频道远程 API 客户端 |
| `imboyapp/lib/store/repository/channel_repo_sqlite.dart` | 频道本地仓库 |
| `imboyapp/lib/service/message_s2c.dart` | S2C 消息处理（频道推送） |
| `imboy/src/imboy_router.erl` | 后端频道路由 |

---

### 后端频道接口 / Backend Channel Endpoints

| 路由 | Action | 说明 |
|------|--------|------|
| `GET /v1/channels/subscribed` | `subscribed` | 获取订阅频道列表 |
| `GET /v1/channels/unread/summary` | `unread_summary` | **获取所有频道未读汇总** |
| `POST /v1/channel/:id/read` | `mark_read` | 标记频道已读 |
| `GET /v1/channel/:id/messages` | `messages` | 获取频道消息 |
| `POST /v1/channel/:id/message` | `publish_message` | 发布消息 |
| `POST /v1/channel/:id/subscribe` | `subscribe` | 订阅频道 |
| `POST /v1/channel/:id/unsubscribe` | `unsubscribe` | 取消订阅 |

---

### Flutter 端实现现状 / Flutter Implementation Status

#### ✅ 已实现功能 / Implemented

1. **`_ChannelUnreadCountCache`（进程内单例缓存）**
   - 内存中维护频道总未读数（`_value`）
   - 监听 `ChannelUnreadCountUpdatedEvent` / `ChannelNewMessageEvent` 驱动刷新
   - 监听 `WebSocketStatusChangedEvent`：WS 重连时触发服务端权威对账
   - 监听 `ChannelStateChangedEvent`：订阅/退订/删除时同步总未读

2. **`channelUnreadCount` Provider（Riverpod）**
   - 同步读 `_ChannelUnreadCountCache.value`，异步通过 stream 通知失效
   - Tab 徽标可直接 `ref.watch(channelUnreadCountProvider)` 实时更新

3. **`ChannelService.syncUnreadSummary()`**
   - 调用 `GET /v1/channels/unread/summary` 获取服务端权威未读集合
   - 与本地订阅表逐条对账，差异时更新本地并广播事件
   - 触发时机：
     - 冷启动（`_ChannelUnreadCountCache.start()`）
     - WS 重连（`ws_connected` 触发）
     - 频道列表加载（`channel_list_load` 触发）

4. **`ChannelService.markAsRead()`**
   - 三步同步：调用 `POST /v1/channel/:id/read` → 本地 DB 清零 → 广播 `ChannelUnreadCountUpdatedEvent(unreadCount: 0)`

5. **`ChannelDetailNotifier.markAsRead()`**
   - 进入频道详情时调用 `ChannelService.to.markAsRead(channelId, messageId)`

6. **实时推送处理（`message_s2c.dart`）**
   - 处理 `channel_message`、`channel_subscribed`、`channel_unsubscribed`、`channel_updated`、`channel_deleted` 等 S2C 事件
   - 新消息到来时触发 `_ChannelUnreadCountCache._syncFromDb()`

7. **载荷守卫（Payload Guard）**
   - `channels` 字段缺失或类型错误时不清零本地未读，防止因网络异常误清数据

---

#### ❌ 未实现 / Not Implemented

1. **进入频道详情后的标记已读时机不确定**
   - `ChannelDetailNotifier.markAsRead()` 需外部显式调用
   - `channel_detail_page.dart` 中调用时机（是否在页面 `initState` 或消息可见时调用）需进一步确认
   - 若用户进入频道但未显式触发，未读徽标不会清零

2. **本地计数可能滞后于推送**
   - 新消息 S2C 到达时，`_ChannelUnreadCountCache` 通过 `_syncFromDb()` 读本地 DB
   - 若 `ChannelMessageRepo.saveMessage()` 和 `ChannelRepo.updateUnreadCount()` 之间存在竞态，总未读可能短暂不准

3. **`loadData` 策略：列表页仅取前 50 条**
   - `getSubscribedChannelsPage(limit: 50)`，超过 50 个订阅频道时需翻页
   - 初始化时 `syncUnreadSummary` 仍能覆盖所有频道，未读数本身不受限制

4. **离线期间的未读积压**
   - 离线期间收到的频道消息由 `message_offline` 服务处理
   - 频道消息离线补齐路径是否完整（是否调用 `updateUnreadCount`）未在当前分析范围内确认

---

### 未读计数数据流 / Unread Count Data Flow

```
冷启动:
  _ChannelUnreadCountCache.start()
    └─ syncUnreadSummary(trigger: 'cache_start')
         └─ GET /v1/channels/unread/summary
              └─ 对账本地订阅表 → 差异广播 ChannelUnreadCountUpdatedEvent
                   └─ _syncFromDb() → 更新 _value → invalidate channelUnreadCountProvider

WS 重连:
  WebSocketStatusChangedEvent(status: 'connected')
    └─ syncUnreadSummary(trigger: 'ws_connected')  [同上]

收到新频道消息（S2C channel_message）:
  _handleChannelMessage()
    └─ 落库 → 广播 ChannelNewMessageEvent
         └─ _ChannelUnreadCountCache._syncFromDb()

用户进入频道详情:
  ChannelDetailNotifier.markAsRead(channelId, messageId)
    └─ POST /v1/channel/:id/read
         └─ _repo.markAsRead() → 广播 ChannelUnreadCountUpdatedEvent(unreadCount: 0)
              └─ _syncFromDb() → _value 更新 → UI 徽标清零
```

---

## 综合问题优先级清单

### P0（阻断性 Bug）

| # | 模块 | 问题 | 影响 |
|---|------|------|------|
| P0-1 | 黑名单 | `contact_setting_page._handleDenylistToggle()` 仅写本地 SQLite，未调用 `/friend/denylist/add` 或 `/friend/denylist/remove` API | 服务端黑名单表与客户端永久不一致；重装 App / 换设备后黑名单失效；后端不会拒收被本地标记黑名单者发来的消息 |

### P1（高优先级缺陷）

| # | 模块 | 问题 | 影响 |
|---|------|------|------|
| P1-1 | 黑名单 | 无多设备黑名单变更实时同步（无 S2C 推送 + 无定期拉取） | 多设备用户在设备 A 拉黑某人，设备 B 不感知，直到手动进入黑名单页刷新 |
| P1-2 | 黑名单 | `denylist_provider.page()` 本地 SQLite 非空时永远不走服务端 | 本地数据过期也不刷新（例如服务端已删除的记录本地仍保留）；`onRefresh=false` 分支跳过 API |
| P1-3 | 频道未读 | `markAsRead` 触发时机不明确 | 可能出现用户已阅读频道消息但徽标不清零的情况 |

### P2（中优先级缺陷）

| # | 模块 | 问题 | 影响 |
|---|------|------|------|
| P2-1 | 黑名单 | `contact_setting_provider.toggleDenylist()` 在移除时也只操作本地 SQLite | 与 P0-1 联动，remove 路径同样绕过 API |
| P2-2 | 频道未读 | 离线消息补齐后未读计数更新路径未验证 | 离线收到的频道消息数量可能与徽标显示不一致 |
| P2-3 | 频道未读 | `loadSubscribedChannels` 每次 `channel_subscribed` 事件都全量重载列表 | 订阅频道较多时产生不必要的网络请求 |

---

## 修复建议

### 修复 P0-1 & P2-1：联系人设置页补全 API 调用

**文件**：`imboyapp/lib/page/contact/contact_setting/contact_setting_page.dart`

```dart
// ❌ 当前实现（仅写本地）
await denylistRepo.insert(model);

// ✅ 修复：复用 DenylistNotifier 中已有的 API+本地双写逻辑
// 方案 A：直接调用 denylist_provider 中已有的 addDenylist / removeDenylist
final notifier = ref.read(denylistProvider.notifier);
if (val) {
  await notifier.addDenylist(model);   // 内部已包含 API + SQLite + hide contact/conversation
} else {
  await notifier.removeDenylist(peerId);  // 内部已包含 API + SQLite + show contact/conversation
}
```

`DenylistNotifier.addDenylist()` 和 `removeDenylist()` 已正确实现 API+本地双写，联系人设置页直接复用即可，无需重复实现。

---

### 修复 P1-1：添加多设备黑名单同步

**方案**：后端新增 `denylist_add` / `denylist_remove` S2C 事件推送给当前用户的其他在线设备。

后端（`user_denylist_logic.erl`）：
```erlang
% add/remove 成功后，向 CurrentUid 的其他设备广播 S2C
websocket_logic:push_s2c(CurrentUid, #{
  action => <<"denylist_add">>,
  payload => #{denied_user_id => DeniedUserId2, created_at => CreatedAt}
}).
```

Flutter 端（`message_s2c.dart`）：
```dart
case 'denylist_add':
  await _handleDenylistAdd(payloadMap);
  break;
case 'denylist_remove':
  await _handleDenylistRemove(payloadMap);
  break;
```

---

### 修复 P1-2：黑名单列表加入强制刷新逻辑

**文件**：`imboyapp/lib/page/mine/denylist/denylist_provider.dart`

```dart
// 当前问题：本地非空时跳过服务端
if (onRefresh == false) {
  list = await repo.page(limit: size, offset: offset);
}
if (list.isNotEmpty) {
  return list;  // ← 本地有数据就直接返回，不对账
}

// 建议修复：page=1 时额外触发一次后台对账，更新本地数据
static Future<List<DenylistModel>> page({
  int page = 1,
  int size = 10,
  bool onRefresh = false,
}) async {
  final repo = UserDenylistRepo();
  List<DenylistModel> localList = [];
  if (!onRefresh && page == 1) {
    localList = await repo.page(limit: size, offset: 0);
  }
  if (localList.isNotEmpty && !onRefresh) {
    // 返回本地缓存，同时后台对账（fire and forget）
    unawaited(_syncFromServer(repo, page: page, size: size));
    return localList;
  }
  // 本地为空或强制刷新，走服务端
  return await _syncFromServer(repo, page: page, size: size);
}
```

---

### 修复 P1-3：明确 markAsRead 触发时机

**文件**：`imboyapp/lib/page/channel/channel_detail_page.dart`

建议在以下时机调用 `ChannelDetailNotifier.markAsRead()`：
1. 进入频道详情页（`initState` 或 `loadChannel` 完成后，消息列表不为空时）
2. 用户滚动到最新消息时（可用 `ScrollController` 监听）

```dart
// 推荐：进入页面时标记已读
@override
void initState() {
  super.initState();
  WidgetsBinding.instance.addPostFrameCallback((_) async {
    await ref.read(channelDetailProvider.notifier).loadChannel(widget.channelId);
    // 加载完成后，取最新一条消息标记已读
    final messages = ref.read(channelDetailProvider).messages;
    if (messages.isNotEmpty) {
      await ref.read(channelDetailProvider.notifier)
          .markAsRead(messages.first.id.toString());
    }
  });
}
```

---

## 总结 / Summary

| 功能 | 状态 | 核心问题 |
|------|------|---------|
| 黑名单 API 调用（黑名单页） | ✅ 正常 | - |
| 黑名单本地 SQLite | ✅ 完整 | - |
| **联系人设置页黑名单开关** | ❌ **P0 缺陷** | 仅写本地，绕过服务端 API |
| 多设备黑名单同步 | ❌ 缺失 | 无 S2C 推送，无定期拉取 |
| 频道未读计数（推送驱动） | ✅ 正常 | - |
| 频道未读对账（冷启动/重连） | ✅ 正常 | - |
| 频道标记已读（三步同步） | ✅ 正常 | 触发时机需确认 |
| 频道离线消息未读积压 | ⚠️ 待验证 | - |

**最高优先级**：立即修复 `contact_setting_page._handleDenylistToggle()`，将其改为调用已有的 `DenylistNotifier.addDenylist() / removeDenylist()`，代价极低（复用已有逻辑），但不修复将导致服务端黑名单数据长期被绕过。
