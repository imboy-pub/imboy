# ImBoy 频道功能设计方案

> 版本: 1.0
> 日期: 2026-02-15
> 状态: 待确认

---

## 一、功能概述

### 1.1 什么是频道？

频道（Channel）是 ImBoy 中类似 **Telegram Channel** 的单向关注型消息订阅机制：

- **单向订阅**：用户订阅频道后可接收消息，但无法在频道内发言
- **一对多广播**：频道管理员发布消息，所有订阅者可见
- **公开/私有**：支持公开频道（可搜索发现）和私有频道（仅邀请可加入）
- **多类型消息**：支持文本、图片、视频、文件、链接等多种消息类型

### 1.2 核心价值

| 用户类型 | 价值 |
|---------|------|
| 普通用户 | 订阅感兴趣的内容源，获取最新资讯 |
| 内容创作者 | 建立自己的粉丝群体，一对多内容分发 |
| 企业/组织 | 官方公告、产品更新、活动通知 |

---

## 二、后端 API 分析

### 2.1 已实现的后端 API

| API | 方法 | 路径 | 说明 |
|-----|------|------|------|
| 创建频道 | POST | `/api/channel/create` | 创建新频道 |
| 获取频道 | GET | `/api/channel/show?channel_id=xxx` | 获取频道详情 |
| 通过自定义ID获取 | GET | `/api/channel/by_custom_id?custom_id=xxx` | 通过自定义ID获取 |
| 更新频道 | POST | `/api/channel/update` | 更新频道信息 |
| 删除频道 | POST | `/api/channel/delete` | 删除频道 |
| 订阅频道 | POST | `/api/channel/subscribe` | 订阅频道 |
| 取消订阅 | POST | `/api/channel/unsubscribe` | 取消订阅 |
| 我订阅的频道 | GET | `/api/channels/subscribed` | 获取订阅列表 |
| 我管理的频道 | GET | `/api/channels/managed` | 获取管理列表 |
| 发布消息 | POST | `/api/channel/publish_message` | 管理员发布消息 |
| 消息列表 | GET | `/api/channel/messages` | 获取频道消息 |
| 标记已读 | POST | `/api/channel/mark_read` | 标记消息已读 |
| 搜索频道 | GET | `/api/channel/search` | 按关键词搜索 |
| 发现频道 | GET | `/api/channel/discover` | 推荐频道列表 |
| 添加管理员 | POST | `/api/channel/add_admin` | 添加管理员 |
| 移除管理员 | POST | `/api/channel/remove_admin` | 移除管理员 |
| 频道统计 | GET | `/api/channel/stats` | 获取统计数据 |
| 记录阅读 | POST | `/api/channel/record_view` | 记录消息阅读 |
| 添加反应 | POST | `/api/channel/add_reaction` | 添加消息反应 |
| 移除反应 | POST | `/api/channel/remove_reaction` | 移除消息反应 |
| 每日统计 | GET | `/api/channel/stats_daily` | 获取每日统计 |

### 2.2 数据模型

#### Channel (频道)
```json
{
  "id": "string",
  "name": "string",
  "description": "string",
  "avatar": "string",
  "type": 0,
  "custom_id": "string",
  "creator_uid": "string",
  "subscriber_count": 0,
  "is_verified": false,
  "tags": ["tag1", "tag2"],
  "status": 1,
  "created_at": "2026-01-01T00:00:00Z",
  "updated_at": "2026-01-01T00:00:00Z"
}
```

#### ChannelMessage (频道消息)
```json
{
  "id": "string",
  "channel_id": "string",
  "author_id": "string",
  "author_name": "string",
  "author_avatar": "string",
  "content": "string",
  "msg_type": "text",
  "payload": {},
  "is_pinned": false,
  "view_count": 0,
  "reaction_summary": {"like": 10, "heart": 5},
  "created_at": "2026-01-01T00:00:00Z"
}
```

---

## 三、前端页面设计

### 3.1 页面架构

```
频道模块
├── 频道入口（底部导航/会话列表）
│   └── ChannelListPage (频道列表页)
│
├── 频道发现
│   └── ChannelDiscoverPage (发现/搜索页)
│
├── 频道详情
│   └── ChannelDetailPage (频道消息页)
│
├── 频道创建
│   └── ChannelCreatePage (创建频道页)
│
└── 频道设置
    └── ChannelSettingPage (设置页 - 待实现)
```

### 3.2 页面详情

#### 3.2.1 ChannelListPage (频道列表页)

**入口**：底部导航 Tab 或会话列表顶部

**功能**：
- 顶部 Tab 切换：`我订阅的` | `我管理的`
- 列表展示频道卡片（头像、名称、订阅数、最新消息预览）
- 右上角按钮：搜索、创建
- 下拉刷新、上拉加载更多

**UI 设计**：
```
+-------------------------------------+
|  频道                          +  |
+-------------------------------------+
|  [我订阅的]  [我管理的]              |
+-------------------------------------+
|  +-----------------------------+   |
|  | [头像] ImBoy 官方       [>]  |   |
|  |    ImBoy 最新版本发布...      |   |
|  |    1.2万订阅  ·  科技 · 产品   |   |
|  +-----------------------------+   |
+-------------------------------------+
```

#### 3.2.2 ChannelDiscoverPage (发现/搜索页)

**入口**：频道列表页右上角搜索按钮

**功能**：
- 搜索框：支持按频道名称、标签搜索
- 推荐频道列表（未搜索时显示）
- 搜索结果列表
- 订阅/取消订阅按钮

#### 3.2.3 ChannelDetailPage (频道详情页)

**入口**：点击频道列表项

**功能**：
- 顶部：频道名称、更多菜单（取消订阅、分享）
- 统计栏：订阅数、消息数、阅读量、反应数
- 消息列表：支持多种消息类型展示
- 消息操作：点赞/反应、分享、复制
- 管理员功能：发布消息、置顶、删除
- 下拉刷新、滚动加载更多

#### 3.2.4 ChannelCreatePage (创建频道页)

**入口**：频道列表页右上角创建按钮

**功能**：
- 频道名称（必填）
- 频道描述
- 频道头像（上传）
- 频道类型（公开/私有）
- 自定义 ID（可选）
- 标签（可选）

---

## 四、前端实现状态

### 4.1 已完成

| 组件/页面 | 文件 | 状态 |
|----------|------|------|
| ChannelModel | `lib/store/model/channel_model.dart` | DONE |
| ChannelMessageModel | `lib/store/model/channel_message_model.dart` | DONE |
| ChannelStatsModel | `lib/store/model/channel_stats_model.dart` | DONE |
| ChannelApi | `lib/store/api/channel_api.dart` | DONE |
| ChannelProvider | `lib/page/channel/channel_provider.dart` | DONE |
| ChannelListPage | `lib/page/channel/channel_list_page.dart` | DONE |
| ChannelDiscoverPage | `lib/page/channel/channel_discover_page.dart` | DONE |
| ChannelDetailPage | `lib/page/channel/channel_detail_page.dart` | DONE |
| ChannelCreatePage | `lib/page/channel/channel_create_page.dart` | DONE |
| 路由配置 | `lib/config/router/app_router.dart` | DONE |

### 4.2 待完善

| 功能 | 优先级 | 说明 |
|------|--------|------|
| 频道设置页 | P1 | 编辑频道信息、管理管理员 |
| 消息发布UI | P1 | 管理员发布消息的输入框 |
| 本地缓存 | P1 | SQLite 存储频道消息 |
| WebSocket 推送 | P1 | 实时接收频道新消息 |
| 未读计数 | P2 | 频道未读消息角标 |
| 消息置顶 | P2 | 管理员置顶/取消置顶 |
| 消息删除 | P2 | 管理员删除消息 |
| 分享频道 | P2 | 生成分享链接/二维码 |
| 付费频道 | P3 | 订阅付费频道 |

---

## 五、国际化文案

### 5.1 需要添加的翻译键

```yaml
# zh-CN.i18n.yaml
channel:
  title: "频道"
  discover: "发现频道"
  create: "创建频道"
  search: "搜索"
  searchHint: "搜索频道..."
  searchTip: "输入关键词搜索频道"
  subscribed: "我订阅的"
  managed: "我管理的"
  subscribers: "订阅者"
  messages: "消息"
  views: "阅读"
  reactions: "互动"
  subscribe: "订阅"
  unsubscribe: "取消订阅"
  view: "查看"
  loading: "加载中..."
  noSubscribedChannels: "还没有订阅任何频道"
  noManagedChannels: "还没有管理任何频道"
  noRecommendedChannels: "暂无推荐频道"
  noResults: "没有找到相关频道"
  noMessages: "暂无消息"
  subscribeSuccess: "订阅成功"
  subscribeFailed: "订阅失败"
  unsubscribeConfirm: "取消订阅？"
  unsubscribeConfirmDesc: "取消后将不再接收该频道的消息"
  share: "分享"
  shareNotImplemented: "分享功能开发中"
  pinned: "置顶"
  selectReaction: "选择表情"
  react: "互动"
  today: "今天"
  yesterday: "昨天"
  daysAgo: "天前"
  # 创建频道
  createTitle: "创建频道"
  nameHint: "请输入频道名称"
  nameRequired: "频道名称不能为空"
  descHint: "介绍一下你的频道..."
  typePublic: "公开频道"
  typePublicDesc: "所有人可见，可被搜索"
  typePrivate: "私有频道"
  typePrivateDesc: "仅邀请可见"
  customIdHint: "@your_channel_id"
  tagsHint: "添加标签"
  createSuccess: "创建成功"
  createFailed: "创建失败"
```

---

## 六、技术方案

### 6.1 状态管理

使用 Riverpod 管理状态：
- `ChannelListNotifier` - 频道列表状态
- `ChannelDetailNotifier` - 频道详情状态
- `CreateChannelNotifier` - 创建频道状态
- `channelUnreadCountProvider` - 未读计数

### 6.2 数据缓存

使用 SQLite 本地缓存：
- `channel` 表 - 频道基础信息
- `channel_message` 表 - 频道消息
- `channel_subscription` 表 - 订阅关系

### 6.3 实时消息

通过 WebSocket 接收频道新消息：
- 订阅频道后自动加入消息推送
- 收到新消息更新本地缓存
- 更新未读计数

---

## 七、待确认事项

请确认以下问题后，再开始实施：

1. **频道入口位置**：频道入口放在底部导航 Tab 还是会话列表？
2. **消息发布方式**：频道详情页底部是否需要显示消息输入框（管理员可见）？
3. **私有频道**：私有频道是否需要在第一版实现？
4. **付费频道**：付费频道是否计划支持？时间安排？
5. **其他功能优先级**：上述"待完善"列表中的功能优先级是否需要调整？

---

**请确认设计方案后，我将开始实施。**
