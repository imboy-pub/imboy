# 前端联动协议 / Frontend Integration Protocol

> ⚠️ **架构定位声明（2026-07 补）/ Architecture Status**
>
> 本文档为**远期设计**。当前生产交付的是**模块化功能开关**，不是动态插件平台：
> `/api/v1/app/manifest` 返回的条目源自 `imboy_plugin_registry` 的静态清单 + policy
> 特性旗标，启动期加载，**不支持运行时热加载**。§7「灰度 / 多租户」为 roadmap-only ——
> imboy 当前**无多租户实体表**（见 `docs/planning/p0-billing-multitenant-authz-2026-07.md`）。
>
> **不得据此对外宣称"插件热加载生态"或"多租户"。**

> **Last Updated**: 2026-04-29
> **Status**: 长期协议设计文档（Phase 5 前置）
> **Scope**: 后端 `/v1/app/manifest` + WS push + Flutter / Vue 双端集成
> **Source of truth**: `src/api/app_handler.erl`（待 Phase 5 扩展）、`src/lib/imboy_plugin_loader.erl`、`imboy_plugin_registry.erl#manifest_v2/1`
> **Related docs**: `docs/reference/plugin/contract.md` §3.2 entries / i18n、`.claude/plan/industrial-plugin-architecture-roadmap.md` Phase 5
> **简体中文为权威版本，本文采用 Pattern A 同节并排双语 / Chinese is authoritative; Pattern A bilingual**

---

## 0. 术语表 / Glossary

| 术语 / Term | 定义 / Definition |
|------------|-------------------|
| **manifest（清单）** | 客户端获取的当前用户可见的 plugin entries / i18n / version 元数据聚合 |
| **app entry** | Flutter UI 入口符号（如 `channel_tab`、`channel_discover_page`），与 PluginRegistry 中的 widget 工厂对应 |
| **admin entry** | Vue 后台入口符号（如 `channels_page`），与 asyncRoutes 中的路由对应 |
| **etag** | manifest 内容指纹（hex string），用于 304 缓存协商 |
| **manifest_updated** | WebSocket 服务端推送事件，提示客户端重拉 manifest |
| **plugin entry resolver** | 客户端将 entry 符号 → widget/route 的工厂查找表 |

---

## 1. 概述 / Overview

**中文**：Phase 5 实现 **manifest-driven UI**：客户端不再硬编码插件页面/菜单，而是根据后端 `/v1/app/manifest` 动态渲染。后端 disable 一个插件 → Flutter 朋友圈 tab 30 秒内消失，admin 朋友圈管理菜单同步消失，**零客户端发版**。

**English**: Phase 5 implements **manifest-driven UI**: clients no longer hardcode plugin pages/menus, but render dynamically based on backend `/v1/app/manifest`. Disabling a plugin server-side → Flutter moment tab disappears within 30s, admin moment menu hides simultaneously, **zero client release required**.

### 1.1 设计原则 / Design principles

| 原则 / Principle | 实现 / Implementation |
|----------------|----------------------|
| 单一权威 / Single source of truth | manifest 完全由后端决定，客户端只读 |
| 缓存优先 / Cache-first | etag + 304；客户端启动用本地缓存先渲染再异步刷新 |
| 推送驱动 / Push-driven | WS 推送变更事件，客户端按需重拉，避免轮询 |
| 灰度兼容 / Rollout-aware | manifest 内容已应用 features.rollout/audience，按用户而异 |
| 兼容降级 / Graceful fallback | 客户端有未知 entry 符号时安全跳过，不崩 |

### 1.2 与 contract.md 关系 / Relation to contract.md

- `entries.app` → 本协议的 Flutter app entries
- `entries.admin` → 本协议的 Vue admin entries
- `i18n.keys` → 本协议的 i18n key 占位
- features 的 rollout/audience 决定该用户是否能看到对应 entry

---

## 2. 后端 API 协议 / Backend API Protocol

### 2.1 `/v1/app/manifest` GET

**中文**：返回当前用户可见的 entries 与元数据。

**English**: Returns visible entries + metadata for current user.

**Request**:
```http
GET /v1/app/manifest HTTP/1.1
Authorization: Bearer <jwt>
If-None-Match: "<etag>"           # 可选，客户端有缓存时附带
Accept-Language: zh-CN
```

**Response（200 OK，无缓存命中）**:
```http
HTTP/1.1 200 OK
Content-Type: application/json
ETag: "a3f2d8e1c9b7"
Cache-Control: private, max-age=0
Content-Length: 1234

{
  "etag": "a3f2d8e1c9b7",
  "generated_at": 1714400000000,
  "user_id": 83540663203007943,
  "app_entries": [
    "channel_tab",
    "channel_discover_page",
    "moment_tab",
    "people_nearby_page",
    "group_vote_page",
    "group_schedule_page",
    "group_task_page"
  ],
  "admin_entries": [],
  "enabled_plugins": [
    {"name": "channel", "version": "1.0.0"},
    {"name": "moment", "version": "1.0.0"},
    {"name": "location", "version": "1.0.0"},
    {"name": "group_collab", "version": "1.0.0"}
  ],
  "i18n_keys": [
    "channel.tab.title",
    "moment.tab.title",
    "people_nearby.title"
  ],
  "feature_flags": {
    "channel_invitation": true,
    "channel_order": false,
    "channel_discover": true
  }
}
```

**Response（304 Not Modified，etag 命中）**:
```http
HTTP/1.1 304 Not Modified
ETag: "a3f2d8e1c9b7"
Cache-Control: private, max-age=0
```

### 2.2 ETag 计算 / ETag computation

```erlang
%% Backend pseudo:
EnabledPlugins = imboy_plugin_registry:enabled_for_user(Uid),
Entries = aggregate_entries(EnabledPlugins, Uid),
Payload = #{...},
Etag = base16(crypto:hash(sha256, term_to_binary(Payload))),
maps:put(etag, Etag, Payload).
```

ETag 输入应包含：
- enabled plugins 列表（按 name 排序）
- 每个 plugin 的 version
- entries（按字典序排序）
- feature_flags 用户态求值结果
- i18n_keys

**不**包含：`generated_at`（时间戳会让 etag 永不命中），`user_id`（隐含在 ETag 计算中）

### 2.3 响应字段语义 / Response field semantics

| 字段 / Field | 类型 / Type | 说明 / Note |
|-------------|------------|------------|
| `etag` | string | 内容指纹，幂等用于 304 |
| `generated_at` | int (ms) | 生成时间戳，调试用 |
| `user_id` | int (TSID) | 当前用户 |
| `app_entries` | string[] | Flutter 端可见入口符号 |
| `admin_entries` | string[] | Vue 端可见入口符号（仅管理员 jwt 返回非空） |
| `enabled_plugins` | object[] | `{name, version}` 列表，便于客户端日志/调试 |
| `i18n_keys` | string[] | 客户端可在初始化时预加载这些 key 的翻译 |
| `feature_flags` | object | per-feature 已应用 rollout/audience 的最终布尔值 |

### 2.4 错误响应 / Error responses

| HTTP | 含义 / Meaning |
|------|---------------|
| 401 | jwt 缺失或失效 |
| 503 | manifest 计算失败（loader 未就绪等），客户端应回退使用本地缓存 |

---

## 3. WebSocket 推送 / WebSocket Push

### 3.1 manifest_updated 事件

**中文**：当后端运维 disable / enable / upgrade 任一插件时，所有受影响的在线用户连接收到推送，提示重拉 manifest。

**English**: When ops disable/enable/upgrade any plugin, all affected online connections receive a push to refetch manifest.

**事件格式 / Event format**（imboy.v2 frame inner JSON payload）:
```json
{
  "type": "S2C",
  "msg_type": "manifest_updated",
  "payload": {
    "etag": "b9e3f0d1a8c2",
    "reason": "plugin_enabled",
    "plugins_changed": ["channel"]
  }
}
```

`reason` 取值：
- `plugin_installed`
- `plugin_enabled`
- `plugin_disabled`
- `plugin_upgraded`
- `plugin_uninstalled`
- `feature_toggled`（仅 features.* 灰度变更，不影响 enabled_plugins）

### 3.2 客户端订阅 / Client subscription

- 客户端**无需**主动订阅，登录建立 WS 连接后默认接收所有 S2C 推送
- 收到事件后客户端比较本地 etag 与 push 的 etag，**不一致**则重拉 `/v1/app/manifest`
- 一致则忽略（防止重复事件）

### 3.3 推送范围 / Push scope

| 变更类型 | 推送给 / Push to |
|---------|------------------|
| 全局 install / uninstall / upgrade | 所有在线连接 |
| feature_toggled 全局 rollout=always | 所有在线连接 |
| feature_toggled rollout=percentage | 命中 percentage 桶的用户 |
| feature_toggled rollout=canary + audience.uid_hash | 命中 buckets 的用户 |
| feature_toggled audience.tenant | 命中租户的用户 |

**实现注意**：后端用 `imboy_syn` 按用户 group 推送；不使用 broadcast 全连接（性能爆炸）。

---

## 4. Flutter 端集成 / Flutter Integration

### 4.1 PluginRegistry 工厂模式 / Factory pattern

```dart
// imboyapp/lib/plugin/plugin_registry.dart

class PluginRegistry {
  static final Map<String, Widget Function(BuildContext)> _factories = {
    'channel_tab': (ctx) => ChannelTabPage(),
    'channel_discover_page': (ctx) => ChannelDiscoverPage(),
    'moment_tab': (ctx) => MomentTabPage(),
    'people_nearby_page': (ctx) => PeopleNearbyPage(),
    'group_vote_page': (ctx) => GroupVotePage(),
    'group_schedule_page': (ctx) => GroupSchedulePage(),
    'group_task_page': (ctx) => GroupTaskPage(),
    // 新增 entry 时此处添加映射
    // Add mapping here when introducing new entry
  };

  static Widget? resolve(String entry, BuildContext ctx) {
    final factory = _factories[entry];
    return factory == null ? null : factory(ctx);
  }

  static bool isKnown(String entry) => _factories.containsKey(entry);
}
```

### 4.2 底部 tab 动态生成 / Dynamic bottom tab

```dart
// imboyapp/lib/main.dart

class HomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    final manifest = context.watch<ManifestProvider>().current;
    final tabs = manifest.appEntries
        .where((e) => PluginRegistry.isKnown(e))
        .map((e) => PluginRegistry.resolve(e, context)!)
        .toList();

    return Scaffold(
      body: tabs.isEmpty
          ? const FallbackEmptyView()
          : tabs[currentIndex],
      bottomNavigationBar: BottomNav(items: tabs),
    );
  }
}
```

**未知 entry 安全跳过 / Unknown entry safely skipped**：客户端版本旧、后端引入新 entry 时不崩。

### 4.3 深链处理 / Deep link handling

```dart
void handleDeepLink(String entry, BuildContext ctx) {
  if (!ManifestProvider.of(ctx).isEntryEnabled(entry)) {
    showDialog(
      context: ctx,
      builder: (_) => AlertDialog(
        title: Text('功能未启用 / Feature unavailable'),
        content: Text('此功能当前已停用 / This feature is currently disabled'),
      ),
    );
    return;
  }
  Navigator.push(ctx, MaterialPageRoute(
    builder: PluginRegistry.resolve(entry, ctx)!,
  ));
}
```

### 4.4 ManifestProvider 状态管理 / State management

```dart
class ManifestProvider extends ChangeNotifier {
  Manifest _current = Manifest.empty();
  String? _etag;

  Manifest get current => _current;

  Future<void> loadInitial() async {
    // 1. 启动用本地缓存先渲染（避免空白）
    _current = await _loadFromDisk() ?? Manifest.empty();
    notifyListeners();

    // 2. 异步刷新
    await refresh();
  }

  Future<void> refresh({String? newEtag}) async {
    if (newEtag != null && newEtag == _etag) return;  // 重复事件忽略
    final resp = await api.getManifest(ifNoneMatch: _etag);
    if (resp.statusCode == 304) return;  // 未变更
    _current = resp.body;
    _etag = resp.headers['etag'];
    await _saveToDisk(_current, _etag);
    notifyListeners();
  }

  // WS 推送回调
  void onManifestUpdated(String etag, String reason) {
    refresh(newEtag: etag);
  }
}
```

### 4.5 启动流程 / Startup flow

```
1. App 启动
2. 读本地缓存 manifest，立即渲染 UI（即使无网）
3. 后台并发：登录 + 拉 /v1/app/manifest（带 If-None-Match）
4. 304 → 沿用缓存
5. 200 → 更新内存 + 持久化 + notifyListeners
6. WS 连接建立后订阅 manifest_updated 事件
```

---

## 5. Vue 端集成 / Vue Integration

### 5.1 asyncRoutes 由 manifest 驱动 / Manifest-driven asyncRoutes

```typescript
// imboy-admin-frontend/src/router/plugin-routes.ts

import type { RouteRecordRaw } from 'vue-router'

const PLUGIN_ROUTE_FACTORIES: Record<string, () => RouteRecordRaw> = {
  channels_page: () => ({
    path: '/channels',
    name: 'AdminChannels',
    component: () => import('@/views/admin/ChannelManagePage.vue'),
    meta: { title: '频道管理', icon: 'channel', requiresAuth: true }
  }),
  moments_page: () => ({
    path: '/moments',
    name: 'AdminMoments',
    component: () => import('@/views/admin/MomentManagePage.vue'),
    meta: { title: '朋友圈管理', icon: 'moment', requiresAuth: true }
  }),
  group_vote_manage_page: () => ({
    path: '/group-vote',
    name: 'AdminGroupVote',
    component: () => import('@/views/admin/GroupVoteManagePage.vue'),
    meta: { title: '群投票管理', icon: 'vote' }
  }),
  // ... 新增 admin entry 在此注册 / register new admin entries here
}

export function buildPluginRoutes(adminEntries: string[]): RouteRecordRaw[] {
  return adminEntries
    .map((e) => PLUGIN_ROUTE_FACTORIES[e]?.())
    .filter((r): r is RouteRecordRaw => r !== undefined)
}
```

### 5.2 路由动态注入 / Dynamic route injection

```typescript
// imboy-admin-frontend/src/router/index.ts

const router = createRouter({...})

export async function refreshPluginRoutes() {
  const manifest = await fetchManifest()  // pinia store action
  const pluginRoutes = buildPluginRoutes(manifest.admin_entries)

  // 移除旧的 plugin routes
  pluginRoutes.forEach((r) => {
    if (router.hasRoute(r.name!)) router.removeRoute(r.name!)
  })

  // 注入新的
  pluginRoutes.forEach((r) => router.addRoute('AdminLayout', r))
}
```

### 5.3 菜单可见性 / Menu visibility

```vue
<template>
  <el-menu>
    <el-menu-item
      v-for="entry in visibleAdminEntries"
      :key="entry"
      :index="entryToRoutePath(entry)"
    >
      {{ t(entryToI18nKey(entry)) }}
    </el-menu-item>
  </el-menu>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { useManifestStore } from '@/stores/manifest'

const store = useManifestStore()
const visibleAdminEntries = computed(() =>
  store.admin_entries.filter((e) => e in PLUGIN_ROUTE_FACTORIES)
)
</script>
```

### 5.4 Pinia manifest store

```typescript
// imboy-admin-frontend/src/stores/manifest.ts

export const useManifestStore = defineStore('manifest', {
  state: () => ({
    etag: null as string | null,
    admin_entries: [] as string[],
    enabled_plugins: [] as Array<{name: string; version: string}>,
    feature_flags: {} as Record<string, boolean>,
  }),

  actions: {
    async refresh() {
      const resp = await api.getManifest({
        headers: this.etag ? {'If-None-Match': this.etag} : {}
      })
      if (resp.status === 304) return
      this.$patch({
        etag: resp.headers['etag'],
        admin_entries: resp.data.admin_entries,
        enabled_plugins: resp.data.enabled_plugins,
        feature_flags: resp.data.feature_flags,
      })
      await refreshPluginRoutes()
    },

    onManifestUpdated(etag: string) {
      if (etag !== this.etag) this.refresh()
    }
  }
})
```

---

## 6. 缓存与一致性 / Cache & consistency

| 层级 / Layer | 缓存策略 / Strategy |
|-------------|-------------------|
| HTTP CDN | **不**缓存（manifest 是用户态私有数据，`Cache-Control: private`） |
| 浏览器/客户端 | etag + 304 |
| Flutter 本地 | shared_preferences 持久化最近 manifest，启动先读 |
| Vue localStorage | manifest etag + admin_entries 缓存 |
| 后端 | `imboy_cache` per-user manifest（TTL 60s + 失效推送） |

**一致性保证 / Consistency guarantees**:
- 后端 admin 操作（enable / disable / upgrade）触发 `imboy_cache:flush_user_manifest/0`，清空所有缓存
- WS 推送 `manifest_updated` 是 best-effort（推送失败时客户端下次主动拉时仍能更新）
- 60s 内的最坏延迟保证（即使 WS 漏推，客户端缓存 TTL 到期会主动拉）

---

## 7. 灰度 / 多租户 / Rollout & Multi-tenant

manifest 计算时已应用 contract.md §3.2 features.rollout/audience：

```erlang
%% Backend per-user manifest computation:
compute_manifest(Uid) ->
    AllPlugins = imboy_plugin_registry:manifests_v2(),
    EnabledForUser = filter_by_audience(AllPlugins, Uid),
    EnabledFeatures = compute_features(AllPlugins, Uid),
    Entries = aggregate_entries(EnabledForUser),
    #{
        app_entries => maps:get(app, Entries, []),
        admin_entries => maps:get(admin, Entries, []),
        enabled_plugins => [...],
        feature_flags => EnabledFeatures
    }.

filter_by_audience(Plugins, Uid) ->
    UidBucket = uid_hash_bucket(Uid),
    Tenant = user_tenant(Uid),
    [P || {_, P} <- maps:to_list(Plugins),
        audience_match(maps:get(audience, P, all_audience()), UidBucket, Tenant)].

audience_match(#{kind := all}, _, _) -> true;
audience_match(#{kind := uid_hash, buckets := Buckets}, UidBucket, _) ->
    lists:member(UidBucket, Buckets);
audience_match(#{kind := tenant, tenant_ids := Ts}, _, T) ->
    lists:member(T, Ts).
```

**关键不变量**：
- 同一 user 在不同设备看到的 manifest 一致（只看 user，不看 device）
- features 灰度 toggle 后，命中桶的用户 30s 内收到 `manifest_updated` 推送

---

## 8. 错误处理与降级 / Error handling & fallback

### 8.1 客户端

| 场景 / Scenario | 行为 / Behavior |
|----------------|---------------|
| `/v1/app/manifest` 401 | jwt 失效，触发 refresh token 流程 |
| `/v1/app/manifest` 503 | 沿用本地缓存 + 60s 后重试 |
| 网络断开 | 沿用本地缓存，UI 不变 |
| WS 漏推 | 缓存 TTL 60s 到期主动拉 |
| manifest 含未知 entry | 跳过未知，渲染已知 entries（不崩） |
| 本地无缓存 + 后端不可达 | 显示空 fallback view + 重试按钮 |

### 8.2 后端

| 场景 | 行为 |
|------|------|
| `imboy_plugin_loader` 未就绪 | 503 + retry-after 头 |
| 单插件 manifest 损坏 | 跳过该插件，记录 lager 错误（不阻塞其他） |
| WS 推送失败 | log + 计入 metric，下次客户端主动拉补救 |

---

## 9. 安全考量 / Security considerations

1. **manifest 是用户态私有数据**：`Cache-Control: private`；CDN/proxy 不缓存
2. **admin_entries 鉴权**：仅 admin role 的 jwt 返回非空 admin_entries（普通用户 jwt 返回 `[]`）
3. **feature flag 不暴露未启用 plugin**：`feature_flags` 只含当前用户可见的 plugin features
4. **避免 plugin 名信息泄露**：`enabled_plugins.name` 是公开值（由 manifest 决定），不含敏感名
5. **WS 推送频率限制**：单用户每秒不超过 1 次 manifest_updated（防 admin 误操作风暴）
6. **etag 不含时间戳**：避免缓存永不命中导致 manifest 流量爆炸
7. **manifest 体积控制**：单 manifest < 32KB；entries 总数 < 200（防 DoS）

---

## 10. 实施切片建议 / Implementation slices

### Phase 5 切片 1（后端）
- 在 `src/api/app_handler.erl` 添加 `manifest/2` action
- 实现 ETag 计算与 304 返回
- per-user 缓存（imboy_cache 60s TTL）
- eunit 测试覆盖

### Phase 5 切片 2（后端 WS）
- `imboy_plugin_lifecycle` 状态变更后调 `imboy_app_manifest:invalidate_all/0`
- WebSocket S2C `manifest_updated` 事件类型
- syn 按 user group 推送

### Phase 5 切片 3（Flutter）
- `imboyapp/lib/plugin/plugin_registry.dart`（factory map）
- `ManifestProvider`（state management）
- 主页底部 tab 动态生成 + 深链处理
- shared_preferences 持久化

### Phase 5 切片 4（Vue admin）
- `imboy-admin-frontend/src/router/plugin-routes.ts`
- Pinia `manifest` store
- 路由动态注入 / 移除
- 菜单可见性

### Phase 5 切片 5（联调 + 灰度发布）
- 端到端：后端 disable moment → Flutter 朋友圈 tab 30s 内消失
- admin 朋友圈管理菜单同步消失
- 灰度按 uid_hash bucket 验证

---

## 11. 兼容性 / Compatibility

- 与 contract.md v1.0 一致（entries / i18n / features 字段不变）
- 客户端旧版本（不识别新 entry）安全跳过
- 后端 manifest 协议遵循 semver：增加新字段属 minor，删除字段属 major + ADR

---

## 12. 测试策略 / Testing strategy

- **后端 eunit**：manifest 计算 / etag 一致性 / per-user audience filter / 304 缓存协商
- **后端集成**：disable plugin → manifest 不再含该 entry + WS 推送
- **Flutter widget test**：PluginRegistry resolve / unknown entry skip / fallback view
- **Vue unit**：buildPluginRoutes / 未知 entry 过滤
- **端到端**：Detox / Playwright 验证 disable plugin 后客户端 UI 30s 内更新

---

## 13. 变更记录 / Changelog

| 日期 / Date | 变更 / Change | 作者 / Author |
|------------|---------------|---------------|
| 2026-04-29 | 文档创建（Phase 5 前置设计） | leeyi + Claude |
