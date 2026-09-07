# IMBoy L-01 Overseas Baseline Preset — Implementation Checklist

> 供实施会话直接开工的地基勘察与任务拆解。2026-09-05 由 D-04/R-02 收官会话产出。
> Phase A 剩余任务中唯一不卡 owner 决策的一项；依赖 F-07（构建矩阵 23/23 已就绪）。

## 地基现状（已勘察）

- **profile 机制**：`src/lib/imboy_profile_preset.erl`——现有两档
  community/enterprise（features 默认全 true，capabilities 差异化）。
  `current()` 读 `config_ds` 的 `product_profile`（config 表/env 双层）。
- **feature 全集**：`imboy_feature:feature_names/0` = core/e2ee + plugin registry
  的 `all_feature_keys/0`（ Ordered 固定顺序保 manifest etag 稳定）。
- **plugin manifest**：`priv/plugins/<name>/plugin.config`（features =>
  #{key => #{default => true, rollout, audience}}），由 imboy_plugin_loader
  启动期注入 persistent_term。
- **现有插件**：channel（channel/channel_discover/channel_invitation/
  channel_order）、location（附近的人）、moment、group_collab
  （group_vote/schedule/task）。
- **feature 关闭语义**：`imboy_feature:ensure_enabled(Req, Feature)` 已在
  各 API 边界守门（返回 ERR_FEATURE_DISABLED）。

## 敏感功能 → feature key 映射（L-01 默认 OFF 清单）

| plan 要求默认 OFF | 对应 feature key | 状态 |
|---|---|---|
| 附近的人 | `location` | 已有 key ✓ |
| 公共 trending/discovery | `channel_discover` | 已有 key ✓ |
| 付费 channel/wallet | `channel_order`（wallet 无独立 key，订单/支付随 channel_order 关闭） | 已确认 |
| live room | 无现成 key——预设先于实现：defaults 显式枚举压制（`e7c0b078`），未来 plugin 注册同名 key 即被置 false | 已闭环 |
| AI marketplace / Bot external webhook | bot_webhook 已新增（`9e5979ef`）；ai_marketplace 同 live_room 预设先于实现 | 已闭环 |

## 进度（2026-09-05）

- [x] 第一段：三档 profile 后端落地（imboy `e7c0b078`，preset/normalize/policy
      四模块 + policy_tests 三档化 51/51 + preset_tests 6/6）
- [x] 真库冒烟：9801 切 overseas_baseline → preset 生效；**features 的 admin
      override（config 表 features 键）优先于 preset 默认——既有面板语义**；
      去除对应 override 后 location/channel_discover=false 透出、channel=true
      基线保持
- [x] Bot webhook 守卫：feature_names 追加 Builtin bot_webhook → 生成器/
      manifest/hrl 自动纳入 → push/push_message 入口拒发（imboy `9e5979ef`，
      EUnit 3/3）
- [x] REST/WS 守卫勘察：location_handler/channel_handler 既有 ensure_enabled
      （registry 派生 feature）→ preset 生效即 REST 自动缺席；WS 不承载这些
      功能，无需守卫

## 进度（2026-09-07 第二段：三端可见性 + 构建矩阵）

- [x] 语义定案（imboy `15f5fd73`）：overseas_baseline 是 **runtime preset**——
      构建矩阵 preset manifest 与 full-selected 同编译全集（runtime can
      disable but cannot add absent），矩阵脚本的 overseas_baseline 列即可用
- [x] 三端运行时数据源（imboy `4cfdc097`）：/api/v1/init 下发 effective
      features（imboy_policy:effective_features）；/api/v1/app/features 与
      /api/adm/admin/config/features 亦走同一 imboy_feature:all() 链——三个
      端点全部 preset 感知，三端无需改数据源消费代码
- [x] Flutter 路由/API 可见性（imboyapp `c45f1eb9`）：
      **真缺口修复**——RouteFeatureGuard.featureForPath 此前把 /channel/orders
      与 /channel/order/:orderNo fallback 到父级 channel，overseas_baseline
      （channel_order=false, channel=true）下 deep link 仍可达订单页；补
      channel_order 映射。新增 overseas_baseline_visibility_test 6 例
      （敏感关/基线留/deep link 重定向/基线放行/live_room 双保险/映射断言），
      连同既有 registry 回归 11/11 绿
- [x] Admin route/menu 缺席（imboyadmin `cf78cc7`）：
      **真缺口修复**——featureKeyForAdminPath 此前把 /channels/paid（付费频道
      运营菜单）fallback 到 channel，overseas_baseline 下侧边栏仍可见；补
      channel_order 映射（路由挂载层 FeatureRoute 本就挂 channel_order，直连
      URL 无缺口）。新增 overseas_baseline_visibility.test.tsx 8 例（effective
      判定+manifest 契约+侧边栏过滤+直连 URL 兜底页），连同 features/sidebar
      既有回归 89/89 绿
- [x] F-07 构建矩阵 overseas_baseline 列：三仓纯净 worktree
      （imboy `a9ac6153` / imboyapp `c45f1eb9` / imboyadmin `cf78cc7`）跑
      run_product_feature_matrix.sh overseas_baseline，evidence 见
      feature-composition-evidence/overseas_baseline.json
- 已知边界（记录，不阻塞）：bot_webhook 在 Admin 尚无 route/menu 挂点（Bot
  webhook 是后端外呼边界，imboy `9e5979ef` 已守 push/push_message）；location/
  channel_discover 在 Admin 本无路由/菜单（仅设置页开关）；Flutter 侧
  /map_location_picker 为通用选点工具，不随 location 单独缺席

## 第二段之前的进度快照（2026-09-05 原始记录）

- [x] 第一段后端四项（见上，已于 09-07 分批转正）

## 任务拆解（建议两段）

### 第一段（后端）
1. `imboy_profile_preset`：`supported_profiles` 加 `overseas_baseline`；
   `profile_defaults(overseas_baseline)` → features 除 core/e2ee/friend/c2c/
   group/workspace/project/channel/moment 基线外全部 false（显式枚举 OFF：
   location/channel_discover/channel_order/ai_marketplace/bot_webhook…），
   capabilities 对齐 community（e2ee optional/audit metadata/retention 30d）。
   **normalize_profile 补 overseas_baseline 分支**。
2. Bot webhook 外呼边界：`imboy_feature:ensure_enabled(Req, bot_webhook)`
   （若 Bot webhook 无 feature key：plugin.config 新增 key + 边界守卫）。
3. EUnit：`overseas_baseline` 的 defaults 断言（敏感键=false、基线键=true）+
   effective-policy 断言 + normalize 断言。
4. 真库冒烟：config 表 `product_profile=overseas_baseline` → effective-policy
   端点返回断言。

### 第二段（三端可见性 + 构建矩阵）
5. Flutter：feature registry/route guards 读 manifest → overseas_baseline 下
   敏感路由缺席（deep link 404/拦截）；widget 测试。
6. Admin：route/menu/chunk 缺席测试。
7. F-07 构建矩阵加 overseas_baseline profile 列（三端 hash 匹配断言）。

## 验收口径（plan 原文）

每个禁用功能在 applicable artifacts、UI、direct route/deep link、REST 与
WebSocket action 中**缺席**；manifest hashes 匹配；核心 friend C2C/group/
workspace/project/channel 继续工作。**Stop 条款**：核心域若不能安全分离，
归类为 Base 并记录 Architecture Gap，不要为缩 artifact 重写核心。

## 环境注意

- 本地验收后端：9801（已热更 main）或重编后重启；热更新表 TSID/config/
  路由的坑见记忆「d04-e2e-macos-green-backend-hotpatch-2026-09-05」。
- F-07 矩阵证据在 `docs/compliance/feature-composition-evidence/`。
