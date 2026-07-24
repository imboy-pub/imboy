# AI 助手冷启动 · Flutter UI 设计蓝图 / AI Companion Cold-Start · Flutter UI Blueprint

> 目标读者：真机实现工程师 / Target: engineers implementing on device
> 范围 / Scope：`imboyapp`（Flutter 移动端）M3「透明 AI 助手冷启动」两个界面
> 对应计划 / Plan ref：M3 · APP-1（助手广场页）+ APP-2（BotBadge 徽章）
> 最后更新 / Last updated：2026-07-23

---

## 0. 设计前提 / Design Premises（务必先读）

| 约束 | 结论（已核实） |
|------|--------------|
| 透明 AI 路线 | 明确标注 AI 身份，**非伪装真人**。广场页顶部必须有透明声明。 |
| E2EE 红线 | AI 助手 **绝不进入端到端加密会话**。声明文案要把这一点变成卖点。 |
| `account_type` | 0=真人（不显徽章）/ 1=AI 助手（「AI」徽章）/ 2=官方机器人（「官方」徽章）。 |
| 头像授权 | 所有头像走 `Avatar` 组件（内部 `dynamicAvatar → cachedImageProvider → AssetsService.viewUrl`，TTL 3600s）。**禁止** `Image.network`。 |
| Token 纪律 | 颜色/间距/字号/圆角一律走 `AppColors`/`AppSpacing`/`AppRadius`/`FontSizeType`。硬编码触发 review 阻断。 |
| 双主题 | 亮/暗都要过。项目对「彩色 pill 标签」的既有惯例是 `color.withValues(alpha: 0.1) 底 + 饱和色前景`（见 `recently_registered_user_page.dart` 日期徽章、tips 卡片）。徽章沿用此惯例即天然适配双主题。 |

### 现有可复用资产（已核实存在）

| 资产 | 路径 | 复用点 |
|------|------|--------|
| `Avatar` | `lib/component/ui/avatar.dart` | 头像（内置授权） |
| `BadgeWidget` | `lib/component/ui/badge_widget.dart` | 角标叠加（会话列表小圆点/AI 角标叠加用） |
| `AsyncStateView` | `lib/component/ui/async_state_view.dart` | loading/empty/error 三态统一 |
| `NoDataView` | `lib/component/ui/nodata_view.dart` | 空/错误态视图（`AsyncStateView` 内部已用） |
| `searchBar(...)` | `lib/component/search.dart` | 顶部搜索栏（M3 SearchBar 封装） |
| `IosPageTemplate` / `ImBoyListTile` | `lib/component/ui/ios_settings_ui.dart` | iOS 风格页壳 + 列表项 |
| `ButtonWidget` | `lib/component/ui/button.dart` | 「发消息」按钮 |
| 分页页模板 | `lib/page/contact/recently_registered_user/` | **直接照抄结构**：NotifierProvider + page/size/kwd state + Sliver 列表 |
| C2C 会话路由 | `context.push('/chat/$peerId?type=C2C&title=..&avatar=..&sign=..')` | 「发消息」跳转（见 `contact_page.dart:124`、`people_info_page.dart:324`） |

### 需新增（不存在，须补）

| 项 | 说明 |
|----|------|
| `PeopleModel.accountType` (int) | 现 `PeopleModel` **无** `account_type` 字段，须新增并在 `fromJson` 解析 `account_type`（默认 0）。 |
| `AgentApi` | 现无 `/api/v1/agent/list` 客户端封装，须新增（照 `user_api.dart` 的 `ftsRecentlyUser` 写）。 |
| `BotBadge` widget | 本文档 APP-2。 |
| `assistant_plaza_page` + provider | 本文档 APP-1。 |
| 徽章语义色 token（建议） | 见 §4「需新增 token」——可先用现有色落地，再抽语义别名。 |

---

## 1. 助手广场页（assistant_plaza_page）· 布局线框 / Wireframe

### 1.1 亮色主态（有数据）

```
┌─────────────────────────────────────────────────┐
│  ‹  AI 助手广场                              [⌕]  │  ← IosPageTemplate 导航栏
├─────────────────────────────────────────────────┤
│  ╭───────────────────────────────────────────╮  │
│  │ 🛡  以下都是 AI 助手，会明确标注身份；      │  │  ← 透明声明卡（E2EE 卖点）
│  │    加密聊天里，只有真人。                    │  │     info 蓝 tint 底
│  ╰───────────────────────────────────────────╯  │
│                                                   │
│  ╭─ 搜索 ───────────────────────────────────╮   │  ← searchBar()
│  │ ⌕  搜索助手                                │   │
│  ╰──────────────────────────────────────────╯   │
│                                                   │
│  ┌─────────────────────────────────────────────┐ │  ← 助手卡片 ×N
│  │ ╭────╮   翻译助手  [AI]                      │ │
│  │ │ 头 │   实时中英互译，聊天里直接喊我        │ │
│  │ │ 像 │                        ╭──────────╮  │ │
│  │ ╰────╯                        │  发消息   │  │ │  ← ButtonWidget（brand）
│  │                               ╰──────────╯  │ │
│  └─────────────────────────────────────────────┘ │
│  ┌─────────────────────────────────────────────┐ │
│  │ ╭────╮   官方小秘书  [官方]                  │ │
│  │ │ 头 │   版本公告、使用帮助找我              │ │
│  │ ╰────╯                        ╭──────────╮  │ │
│  │                               │  发消息   │  │ │
│  │                               ╰──────────╯  │ │
│  └─────────────────────────────────────────────┘ │
│                    ⟳ 加载更多…                     │  ← 上拉触底 footer
└─────────────────────────────────────────────────┘
```

### 1.2 三态（loading / empty / error）

```
   loading                 empty                    error
┌───────────────┐   ┌───────────────────┐   ┌───────────────────┐
│               │   │       ◌ (person_2) │   │   ⚠ (exclam...)   │
│      ◌        │   │    暂无 AI 助手      │   │  加载失败，点击重试 │
│  (spinner)    │   │   稍后再来看看       │   │     ╭────────╮    │
│               │   │                     │   │     │  重试   │    │
└───────────────┘   └───────────────────┘   │     ╰────────╯    │
                                             └───────────────────┘
  CupertinoActivity   NoDataView(无重试)       NoDataView(onRetry)
  Indicator           icon: person_2           icon: exclamationmark_circle
  ↑ 全部由 AsyncStateView 按 isLoading > error > isEmpty 优先级自动切换
```

### 1.3 卡片内部结构（放大）

```
┌───────────────────────────────────────────────────────┐
│  ← AppSpacing.cardPadding (16) →                        │
│ ╭──────╮  ┌ nickname (body/17, semiBold) ┐  [BotBadge] │  ← 名字行：名 + 徽章基线对齐
│ │      │  └──────────────────────────────┘             │
│ │ 头像 │   description (footnote/13, secondary, 最多2行)│  ← Avatar 56×56
│ │56×56 │   ┌────────────────────────────────────────┐  │
│ ╰──────╯   │                          ╭───────────╮ │  │
│            │                          │   发消息   │ │  │  ← 右下：ButtonWidget
│  ↑ horizontalMedium(12) 间隔          ╰───────────╯ │  │     brand 填充, radius small(8)
│            └────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────┘
```

---

## 2. BotBadge 徽章 · 三态线框 / Three-State Spec

```
account_type=0 (真人)      account_type=1 (AI)         account_type=2 (官方)
   （不渲染，                ╭─────────╮                 ╭───────────╮
    返回                     │ ✦  AI   │                 │ ✔  官方   │
    SizedBox.shrink）        ╰─────────╯                 ╰───────────╯
                            teal tint 底 + teal 前景      brand tint 底 + brand 前景
                            icon: sparkles(✦)             icon: checkmark_seal_fill(✔)
```

### 挂载点示意 / Mount Points

```
① 会话列表 tile                    ② 聊天页标题
┌──────────────────────────┐       ┌─────────────────────────────┐
│ ╭──╮ 翻译助手 [AI]   12:30│       │  ‹   翻译助手 [AI]        ⋮  │
│ │头│ 你好，需要翻译吗？    │       └─────────────────────────────┘
│ ╰──╯                  ·  │           标题右侧, horizontalTiny(4) 间隔
└──────────────────────────┘

③ 用户资料页                        ④ 广场卡片（本页 §1.3）
┌──────────────────────────┐       名字行内联，与昵称基线对齐
│      ╭────╮               │
│      │头像│               │       通用规则：徽章始终在昵称**之后**、
│      ╰────╯               │       同一行、baseline / center 对齐，
│   翻译助手 [AI]           │       左侧 AppSpacing.horizontalTiny(4)。
│   ID: 100xxx             │       名字过长时昵称先 ellipsis，徽章不压缩。
└──────────────────────────┘
```

---

## 3. 组件树 / Widget 分解

### 3.1 助手广场页（APP-1）

```
AssistantPlazaPage  (ConsumerStatefulWidget)          [新建]
 └ IosPageTemplate                                    [复用 ios_settings_ui.dart]
    ├ 导航栏 title = t.agent.plazaTitle, trailing 搜索图标（可选）
    └ slivers:
       ├ SliverToBoxAdapter → _TransparencyBanner     [新建·私有 widget/内联]
       ├ SliverToBoxAdapter → searchBar(...)          [复用 component/search.dart]
       └ AsyncStateView(                              [复用 async_state_view.dart]
            isLoading / isEmpty / error / onRetry,
            child: SliverList/ListView of AssistantCard)
              └ AssistantCard(model)  ×N               [新建]
                 ├ Avatar(imgUri: model.avatar, 56×56) [复用·内置授权]
                 ├ Row[ Text(nickname) + BotBadge(accountType) ]
                 ├ Text(description, maxLines:2)
                 └ ButtonWidget(「发消息」→ _openChat)  [复用 ui/button.dart]

AssistantPlazaProvider  (NotifierProvider)            [新建·照抄 recently_registered_user_provider]
 └ AssistantPlazaState { page, size(=10), kwd, list<PeopleModel>, isLoading, isLoadingMore, hasMore, error }
    ├ initData()            首屏加载
    ├ loadMore()           上拉分页
    ├ refresh()            下拉刷新（page=1）
    └ updateKwd(kwd)       搜索（debounce 后 page=1 重查）

AgentApi                                              [新建·照抄 user_api.ftsRecentlyUser]
 └ agentList({page, size, keyword}) → GET /api/v1/agent/list
```

> **状态管理**：用 `NotifierProvider`（项目首选，见 page/CLAUDE.md）。**不要**把服务端列表复制进额外 client store；`state.list` 即唯一真源，`copyWith` 不可变更新。

### 3.2 BotBadge（APP-2）

```
BotBadge  (StatelessWidget)                           [新建 lib/component/ui/bot_badge.dart]
  参数: final int accountType;  final bool compact;（compact=仅图标，标题栏用）
  逻辑:
    switch(accountType) {
      0 => const SizedBox.shrink(),        // 真人不渲染
      1 => _pill(AI  label, teal token, CupertinoIcons.sparkles),
      2 => _pill(官方 label, brand token, CupertinoIcons.checkmark_seal_fill),
      _ => const SizedBox.shrink(),        // 未知兜底不渲染
    }
  _pill = Semantics(label:...) → Container(pill) [ Icon + SizedBox(w:tiny) + Text ]
```

建议文件落位：

| 文件 | 路径 |
|------|------|
| 页面 | `lib/page/discover/assistant_plaza/assistant_plaza_page.dart` |
| Provider | `lib/page/discover/assistant_plaza/assistant_plaza_provider.dart` |
| 卡片 | `lib/page/discover/assistant_plaza/widget/assistant_card.dart` |
| 徽章 | `lib/component/ui/bot_badge.dart`（通用组件层，多处挂载） |
| API | `lib/store/api/agent_api.dart` |
| Model 改动 | `lib/store/model/people_model.dart`（+ `accountType`） |

---

## 4. Design Token 映射表 / Token Mapping

> 所有 token 定义位置：`lib/theme/default/app_colors.dart`、`app_spacing.dart`、`app_radius.dart`、`font_types.dart`。
> 字号一律走 `context.textStyle(FontSizeType.x)`（享受 `FontSizeOption` 缩放），**不要**用 `.size` 常量。

### 4.1 助手广场页

| 视觉元素 | 属性 | Token（真实存在） | 值 |
|---------|------|------------------|----|
| 页面壳 | 容器 | `IosPageTemplate`（内部已 token 化） | — |
| 声明卡 底色 | color | `AppColors.info.withValues(alpha:0.1)`（亮）/ 复用 `getIosBlue(b).withValues(alpha:0.1)` 惯例 | `#006C9A`@10% |
| 声明卡 圆角 | radius | `AppRadius.borderRadiusMedium` | 12 |
| 声明卡 内边距 | padding | `AppSpacing.allRegular` | 16 |
| 声明卡 图标 | color | `AppColors.getIosBlue(brightness)` | `#007AFF`/`#0A84FF` |
| 声明卡 文案 | style | `context.textStyle(FontSizeType.footnote, color: 次要色)` | 13 |
| 搜索栏 | widget | `searchBar(...)`（内部 `AppRadius.borderRadiusSmall`=8） | — |
| 卡片外边距 | margin | `AppSpacing.symmetric(h:regular, v:small)`（≈ `listItemMargin` 变体） | h16 v8 |
| 卡片内边距 | padding | `AppSpacing.cardPadding` | 16 |
| 卡片圆角 | radius | `AppRadius.card` (=medium) | 12 |
| 卡片底色 | color | `AppColors.getSurfaceColor(brightness)` | `#FFFFFF`/`#1C1C1E` |
| 卡片分隔/边框 | color | `AppColors.getDividerColor(brightness)` | `#C6C6C8`/`#38383A` |
| 头像 | 尺寸 | `Avatar(width:56, height:56)`（对齐 recently_registered_user） | 56 |
| 头像↔文字 间隔 | gap | `AppSpacing.horizontalMedium` | 12 |
| 昵称 | style | `context.textStyle(FontSizeType.body, fontWeight: w600)` | 17 / semiBold |
| 昵称↔徽章 间隔 | gap | `AppSpacing.horizontalTiny` | 4 |
| description | style | `context.textStyle(FontSizeType.footnote, color: 次要色)` maxLines:2 | 13 |
| 次要文字色 | color | `AppColors.getTextColor(brightness, isSecondary:true)` | `#3C3C43`/`#EBEBF5` |
| 行间距 | gap | `AppSpacing.verticalSmall` | 8 |
| 「发消息」按钮 | 填充 | `AppColors.primary` | `#2474E5` |
| 「发消息」文字 | color | `AppColors.onPrimary` | `#FFFFFF` |
| 「发消息」圆角 | radius | `AppRadius.button` (=small) | 8 |
| 「发消息」内边距 | padding | `AppSpacing.buttonSmallPadding` | h16 v8 |
| loading spinner | — | `CupertinoActivityIndicator`（`AsyncStateView` 内置） | — |
| empty 图标 | icon | `CupertinoIcons.person_2`（传给 `AsyncStateView.emptyIcon`） | — |
| error 图标 | icon | `CupertinoIcons.exclamationmark_circle`（`AsyncStateView` 默认） | — |

### 4.2 BotBadge

沿用项目「tint 底 + 饱和前景 pill」惯例（`recently_registered_user_page.dart` 日期徽章同款结构），双主题天然适配。

| 视觉元素 | 属性 | Token | 值 / 说明 |
|---------|------|-------|----------|
| pill 内边距 | padding | `EdgeInsets.symmetric(h: AppSpacing.small, v: AppSpacing.tiny)` | h8 v4 |
| pill 圆角 | radius | `AppRadius.borderRadiusTiny`（4）或全圆 `999` | 徽章建议 tiny=4（HIG 标签惯例） |
| 图标↔文字 间隔 | gap | `AppSpacing.horizontalTiny` | 4 |
| 徽章文字 | style | `context.textStyle(FontSizeType.caption2, fontWeight: w600)` | 11 / semiBold |
| 图标尺寸 | size | 硬编码 `12`（图标非 token 管辖；对齐 caption2 字号视觉） | 12 |
| **AI 底色** | color | `AppColors.tertiary.withValues(alpha:0.12)` | `#00ACC1`@12% |
| **AI 前景** | color | `AppColors.tertiary`（暗色可用 `onTertiaryContainer` 兜底对比） | `#00ACC1` |
| **官方 底色** | color | `AppColors.primary.withValues(alpha:0.12)` | `#2474E5`@12% |
| **官方 前景** | color | `AppColors.primary` | `#2474E5` |

> **需新增 token（建议，非阻塞）**：上表 AI/官方 直接复用了 `tertiary`/`primary`，可先落地。为语义清晰与后续维护，建议在 `app_colors.dart` 新增语义别名（值等同现有，零视觉变更）：
> - `botAiBadgeFg = tertiary` / `botAiBadgeBg = tertiary @12%`
> - `botOfficialBadgeFg = primary` / `botOfficialBadgeBg = primary @12%`
>
> 这样 BotBadge 不直接引品牌色，未来调色只改一处。**若不新增，用现有 `tertiary`/`primary` 亦合规**。
>
> 配色理由：AI=青绿(tertiary)传达「合成/非人」，与品牌蓝链接区分；官方=品牌蓝+验证印章(`checkmark_seal_fill`)复用用户对「蓝标认证」的心智。二者色相拉开，色盲用户靠**图标形状 + 文字**仍可区分（不单靠颜色，符合 WCAG 1.4.1）。

---

## 5. 交互规格 / Interaction Spec

### 5.1 分页 / 下拉刷新 / 上拉加载
- **首屏**：`initState → addPostFrameCallback → provider.initData()`（照抄 recently_registered_user）。`isLoading=true` 期间显示 spinner。
- **API 契约**：`GET /api/v1/agent/list?page=&size=&keyword=`，标准分页信封，返回 `payload['list']`（每项含 `user_id/nickname/avatar/description/account_type`）。`size` 默认 **10**（对齐项目分页默认）。
- **上拉加载更多**：`ListView`/`CustomScrollView` 底部 `NotificationListener<ScrollNotification>` 或 `ScrollController` 触底阈值（`maxScrollExtent - 200`）触发 `loadMore()`；`isLoadingMore` 期间底部渲染小 spinner footer；`hasMore=false`（返回条数 < size）后不再触发，footer 隐藏。
- **下拉刷新**：`CupertinoSliverRefreshControl`（iOS 风格，与 IosPageTemplate 一致）或 `RefreshIndicator` → `refresh()`：`page=1` 重查并替换列表。
- **不可变更新**：`state = state.copyWith(list: [...state.list, ...more])`，禁止 `list.addAll` 原地改。

### 5.2 搜索
- 输入走 `searchBar(onChanged:)` → `updateKwd(kwd)`；**debounce 300ms** 后 `page=1` 重新 `agentList(keyword:)`。
- 搜索无结果 → 复用 empty 态，文案区分「无匹配助手」（`emptyText`）。
- 清空关键词 → 回到全量首屏。

### 5.3 点击「发消息」跳转
```dart
context.push(
  '/chat/${model.userId}'
  '?type=C2C'
  '&title=${Uri.encodeComponent(model.nickname)}'
  '&avatar=${Uri.encodeComponent(model.avatar)}'
  '&sign=${Uri.encodeComponent(model.description)}',
);
```
- 复用现有 C2C 会话路由（`contact_page.dart:124` / `people_info_page.dart:324` 同款）。`type=C2C` 固定。
- **参数须 `Uri.encodeComponent`**（昵称/描述可能含 `&`、空格、emoji，否则 query 串裂）——这是安全/正确性要求，不可省。
- 整卡片可点击进资料页（可选，`context.push('/people_info/${model.userId}')`），「发消息」按钮 `onTap` 需 `stopPropagation` 语义（Flutter 中按钮在卡片 `InkWell` 之上即自然拦截）。

### 5.4 入口
- **联系人页顶部**：`contact_page.dart` 顶部 header 区加一条入口（图标 + 「AI 助手广场」）→ `context.push('/assistant_plaza')`。
- **发现页**：发现列表加一项 → 同路由。
- 路由注册：`lib/config/routes.dart` 加 `AppRoutes.assistantPlaza='/assistant_plaza'`，`app_router.dart` 挂 `GoRoute`。

### 5.5 三态与双主题差异
| 态 | 亮色 | 暗色 |
|----|------|------|
| loading | `CupertinoActivityIndicator`（系统自适应） | 同左 |
| empty | `NoDataView` 次要色文字 `#3C3C43` | 次要色 `#EBEBF5`；图标走 `onSurfaceVariant` |
| error | `NoDataView` + 重试（`onRetry → refresh()`） | 同结构，色随主题 |
| 卡片 | 白底 `#FFFFFF` + 分隔线 `#C6C6C8` | `#1C1C1E` + 分隔线 `#38383A` |
| 徽章 | tint@12% 底 + 饱和前景 | 同公式（tint 在暗底上亦够对比） |

---

## 6. 无障碍 / 可用性 / Accessibility

- **徽章语义标签**：`BotBadge` 用 `Semantics(label: accountType==1 ? t.agent.badgeAiA11y : t.agent.badgeOfficialA11y, child: pill)`，屏幕阅读器读出「AI 助手」「官方账号」，而非仅图标。**不单靠颜色传达身份**（WCAG 1.4.1）——图标形状(sparkles/seal) + 文字(AI/官方) 双通道。
- **对比度**：
  - 徽章前景色 vs tint 底：`tertiary #00ACC1` / `primary #2474E5` 在各自 12% tint 底上 ≥ 4.5:1（正常文字 AA）。`app_colors.dart` 已提供 `AppColors.getContrastRatio()` 可在 golden/单测里断言。
  - 「发消息」白字 vs `primary #2474E5` 底 ≈ 4.7:1，过 AA。
  - description 次要色文字用 `FontSizeType.footnote`(13) ≥ 12px，过可读下限。
- **触达区**：「发消息」按钮 ≥ 44×44pt（DESIGN.md 硬约束）；`buttonSmallPadding` 下须保证最小高度 44，必要时包 `ConstrainedBox(minHeight:44)`。
- **透明声明**：作为可读文本（非仅图标），确保盲用户也能获知「这些是 AI / 加密聊天无 AI」。
- **动态字号**：全部走 `context.textStyle`，跟随用户 `FontSizeOption` 缩放；卡片 description `maxLines:2 + ellipsis`，大字号下不溢出。
- **焦点/点击反馈**：卡片 `InkWell`、按钮 `ButtonWidget` 自带 ripple/highlight；徽章不可点（纯展示）。

---

## 7. 响应式 / Responsive

- 移动竖屏为主：卡片单列铺满，`AppSpacing.pageHorizontal`(20) 或卡片 `symmetric(h:16)` 左右留白。
- 大屏（平板/macOS 桌面 app）：`AppSpacing.adaptive(context)` 已提供断点（mobile/wide）；宽屏可将列表约束 `ConstrainedBox(maxWidth: 600)` 居中，避免卡片过宽。**M3 阶段可 ponytail 跳过多列网格**，单列约束宽度即可，等真机确认需求再加。
- 徽章在任意容器内 `MainAxisSize.min`，不抢昵称空间；昵称 `Flexible + ellipsis` 优先收缩。

---

## 8. 实现清单 / Implementation Checklist

### APP-1 · 助手广场页
- [ ] `PeopleModel` 增 `accountType`（int，`fromJson` 解析 `account_type`，默认 0）+ `copyWith`。
- [ ] `AgentApi.agentList({page,size,keyword})` → `GET /api/v1/agent/list`（照 `user_api.ftsRecentlyUser`，含 `resp.ok` 守卫、`iPrint` 日志）。
- [ ] `AssistantPlazaState` + `AssistantPlazaNotifier`（`initData/loadMore/refresh/updateKwd`，不可变 `copyWith`，`hasMore` 判定）。
- [ ] `AssistantPlazaPage`：`IosPageTemplate` + 声明卡 + `searchBar` + `AsyncStateView` 包列表。
- [ ] `AssistantCard`：`Avatar`(56) + 昵称 + `BotBadge` + description(2行) + 「发消息」`ButtonWidget`。
- [ ] 「发消息」跳转（`Uri.encodeComponent` 编码参数，`type=C2C`）。
- [ ] 下拉刷新 + 上拉加载 + 搜索 debounce 300ms。
- [ ] 路由常量 + `GoRoute` 注册；联系人页顶部 & 发现页入口。
- [ ] i18n：`assets/i18n/zh-CN/` 新增 `agent` namespace（plazaTitle / transparencyBanner / searchHint / emptyTitle / emptyDesc / sendMessage / badgeAiA11y / badgeOfficialA11y），`dart run slang` 同步 10 语言。
- [ ] 测试：Provider 单测（loading→success/empty/error、loadMore 追加、搜索重置 page=1）；Widget 测试（三态渲染、徽章按 accountType 渲染）。

### APP-2 · BotBadge
- [ ] `lib/component/ui/bot_badge.dart`：`accountType` switch → 0/未知不渲染，1=AI，2=官方；`compact` 参数（标题栏仅图标）。
- [ ] token 化：pill padding/radius/字号/间隔全走 `AppSpacing`/`AppRadius`/`FontSizeType`；颜色用 `tertiary`/`primary`（或新增语义别名）。
- [ ] `Semantics(label:...)` 无障碍标签。
- [ ] 挂载：会话列表 tile、聊天页标题、用户资料页、广场卡片——昵称后 `horizontalTiny`(4) 内联。
- [ ] （建议）`app_colors.dart` 新增 `botAiBadge*` / `botOfficialBadge*` 语义别名（零视觉变更）。
- [ ] 测试：Widget 测试三态（0 渲染 `SizedBox.shrink`、1/2 各自 icon+label）；golden 亮/暗各一。

### 验收门（真机）
- [ ] `flutter analyze lib` **零 issue**（项目基线）。
- [ ] 亮/暗主题各扫一遍：徽章对比度、卡片分隔线、声明卡可读。
- [ ] 大字号档（FontSizeOption huge）description 不溢出。
- [ ] 「发消息」进入的是普通 C2C 会话（**非 E2EE**），验证 AI 会话不触发加密握手。
```
