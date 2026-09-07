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
| 付费 channel/wallet | `channel_order`（wallet 若有独立 key 待查） | 待确认 wallet 是否独立 |
| live room | **无现成 key**（live room 若未实现则为 Architecture Gap，记录即可） | 待确认 |
| AI marketplace / Bot external webhook | **无现成 key**（Bot/Agent 是 core 内建——需新增 feature key 并在 Bot webhook 外呼边界 ensure_enabled） | 需新增 |

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
- [ ] Flutter 路由/API 可见性测试
- [ ] Admin route/menu/chunk 缺席测试
- [ ] F-07 构建矩阵加 overseas_baseline 列

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
