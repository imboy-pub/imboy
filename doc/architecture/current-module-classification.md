# Imboy 当前功能分层归类表

> Last Updated: 2026-03-13
> Purpose: 把当前仓库里的主要能力按 `kernel`、`capability`、`plugin`、`profile preset` 归类，避免后续模块化时边界继续混乱
> Related docs: `module-layer-cheatsheet.md`, `overview.md`, `../guides/module-feature-flag-config.md`

## 1. 怎么看这张表

本文不是在说“代码现在已经按这个结构组织好了”，而是在说：

1. 当前仓库里已经有哪些能力；
2. 这些能力更适合放在哪一层；
3. 哪些只是“文档/配置雏形”，哪些已经有真实三端代码；
4. 后续重构时，应该优先抽哪一层，而不是先搬目录。

判断标准请优先参考：

- `module-layer-cheatsheet.md`

一句话记忆：

- `kernel`：骨架
- `capability`：规则
- `plugin`：功能
- `profile preset`：套餐

## 2. 当前总览

| 领域 | 推荐层 | 当前状态 | 备注 |
|---|---|---|---|
| 认证、用户、设备、会话、消息投递 | `kernel` | 已有真实三端实现 | 系统成立的基础，不应插件化 |
| 消息归档、E2EE、搜索、导出、审计、留存 | `capability` | 部分已实现，部分隐式存在，部分待显式化 | 企业与非企业产品分叉主要在这里 |
| 频道、朋友圈、位置、群协作 | `plugin` | 已有较明显三端代码资产 | 最适合作为可售卖模块 |
| 基础版 / 专业版 / 行业扩展模板 | `profile preset` 雏形 | 已存在文档模板，未形成一等配置对象 | 后续应升级成显式 `product_profile` |

## 3. Kernel 归类

这些能力应长期留在系统骨架内，不建议做成独立业务插件。

| 能力域 | 主要证据 | 当前成熟度 | 说明 |
|---|---|---|---|
| 认证与登录 | `passport_handler`、`auth_handler`、`qr_login_handler`、后台登录页 | 高 | 所有版本都依赖 |
| 用户与设备 | `user_handler`、`user_device_handler`、`imboyapp/lib/page/mine/user_device` | 高 | 是所有产品线的公共基础 |
| 会话管理 | `conversation_handler`、`conversation_logic`、客户端会话页 | 高 | 已闭环，属于 IM 主链路 |
| 消息投递 | `msg_handler`、`msg_c2c_logic`、`msg_c2g_logic`、`message_router_logic` | 高 | 所有聊天能力依赖 |
| WebSocket 协议 | `websocket_handler`、`websocket-api-2.md` | 高 | 不应被业务插件改写 |
| ACK / 离线同步 | `msg_ack_logic`、`msg_store_ds`、客户端 ACK/重试链路 | 高 | 属于底座可靠性，不是插件 |
| 联系人 / 好友 / 黑名单 | `friend_handler`、`friend_category_handler`、`user_denylist_handler` | 高 | 目前更接近核心社交图谱，而不是外挂功能 |
| 群聊核心与基础治理 | `group_handler`、`group_member_handler`、`group_notice_handler` | 高 | 群本身是主链路，不建议插件化 |
| 基础后台认证与 RBAC | `adm_passport_handler`、`adm_role_handler`、`RolePermissionPage.tsx` | 高 | 企业版与非企业版都需要某种后台控制能力 |
| Feature Registry | `imboy_feature.erl`、`app_feature_handler`、`adm_admin_handler` | 中 | 已有雏形，后续应从“功能开关读取”升级为“统一注册中心” |
| Policy Engine | 当前仅隐式存在 | 低 | 建议新增一等对象，用来统一解释 profile/capability/feature |

### 3.1 Kernel 内部还可以再分两类

虽然都属于 `kernel`，但建议长期再细分成两类：

1. **Core Domain**
   - auth
   - user/device
   - conversation
   - message delivery
   - group core

2. **Kernel Infrastructure**
   - websocket
   - ack/offline sync
   - feature registry
   - policy engine
   - admin auth / rbac

这样以后重构时，不会把“业务核心”与“系统基础设施”混成一团。

## 4. Capability 归类

这些能力不是独立业务模块，而是系统运行规则。它们会影响多个模块，尤其会影响企业版与非企业版的默认行为。

| 能力域 | 当前状态 | 主要证据 | 后续建议 |
|---|---|---|---|
| `storage_mode` | 缺少显式建模 | 当前只有 `payload` + `e2ee` + ACK 删除逻辑，语义混在一起 | 新增显式 `archived / secure_e2ee` |
| 消息归档 | 隐式存在但语义不稳定 | `msg_c2c/msg_c2g` 已落库，但表注释仍偏“临时存储” | 升级为正式归档能力 |
| E2EE | 已显式存在 | `e2ee_handler`、`e2ee_social_handler`、App E2EE 服务 | 应从“功能模块”升级为“平台能力” |
| 消息搜索 | 已显式存在 | `fts_handler`、`00000050_msg_fulltext_search.sql` | 搜索应依赖 capability，不应默认对密聊开放 |
| 消息导出 | 已显式存在 | `adm_message_handler` 中 `export`、后台导出能力 | 应与 `storage_mode`、`audit_mode` 联动 |
| 审计能力 | 部分已实现 | `report_ticket`、后台日志页、消息后台查询 | 需要统一到 `audit_mode` |
| 留存策略 | 隐式存在 | TimescaleDB retention policy 已存在 | 需要业务层显式化为 `retention_policy` |
| 通知模式 | 部分实现 | 会话提醒已存在，系统级通知接线未完全闭环 | 可后续补为 capability，而不是业务插件 |

### 4.1 为什么 E2EE 是 capability，不是 plugin

因为它会改变：

- 消息如何存储
- 是否支持全文搜索
- 后台能否查看正文
- 是否支持消息导出
- 审计粒度到什么程度

所以 `e2ee` 不应只等于一个普通布尔 feature；它更像消息系统的一种运行规则。

### 4.2 为什么 storage_mode 是未来最重要的 capability

后续最建议显式化的就是：

- `storage_mode = archived`
- `storage_mode = secure_e2ee`

因为这一个能力会决定企业版和非企业版的根本分叉：

- 企业版更偏 `archived`
- 非企业或密聊场景更偏 `secure_e2ee`

## 5. Plugin 归类

这些能力更像“系统在主链路之外新增的独立业务功能”，最适合作为可售卖、可关闭、可按客户裁剪的模块。

| 插件域 | 当前状态 | 主要证据 | 推荐处理 |
|---|---|---|---|
| `channel` | 高成熟度 | 后端 `channel_handler`、App `page/channel`、后台 `pages/channels` | 第一批插件化对象 |
| `moment` | 有明显三端资产，但闭环偏弱 | 后端 `moment_handler`、App `page/moment`、后台 `pages/moments` | 第二批插件化对象 |
| `location` | 有前后端入口，但不是主销售线 | `location_handler`、App `people_nearby` | 适合做可选插件 |
| `group_vote` | 三端资产存在 | `group_vote_handler`、App `group/vote`、后台 `GroupVoteManagePage` | 适合合并进群协作插件 |
| `group_schedule` | 三端资产存在 | `group_schedule_handler`、App `group/schedule`、后台 `GroupScheduleManagePage` | 适合合并进群协作插件 |
| `group_task` | 三端资产存在 | `group_task_handler`、App `group/task`、后台 `GroupTaskManagePage` | 适合合并进群协作插件 |

### 5.1 推荐的插件边界

建议不要把 `group_vote`、`group_schedule`、`group_task` 拆成 3 个完全独立插件。

更适合的方式是先做一个聚合插件：

- `group_collab`

再在插件内部定义子能力：

- `vote`
- `schedule`
- `task`

这样比“一上来 3 个插件 + 3 套菜单 + 3 套依赖”更容易维护。

### 5.2 当前不建议直接插件化的东西

虽然也有页面或接口，但当前更不适合优先做成业务插件：

- 用户标签
- 收藏
- 群文件 / 群相册 / 群标签
- 举报 / 反馈

原因不是它们不重要，而是它们更接近主包治理和群治理能力，现阶段拆出去只会增加认知负担。

## 6. Profile Preset 归类

这层当前还没有形成正式配置对象，但已经有明显雏形。

| 预设 | 当前状态 | 主要证据 | 推荐方向 |
|---|---|---|---|
| 基础版默认模板 | 已有文档雏形 | `module-feature-flag-config.md` 中基础版模板 | 可演进为 `community` 或 `basic` preset |
| 专业版默认模板 | 已有文档雏形 | `module-feature-flag-config.md` 中专业版模板 | 可演进为 `enterprise` preset |
| 行业扩展模板 | 已有文档雏形 | `module-feature-flag-config.md` 中行业扩展模板 | 后续可作为 `industry_plus` 类 preset |

### 6.1 当前最推荐先显式化的 preset

为了减少复杂度，建议先只正式支持两个一等 preset：

1. `community`
2. `enterprise`

其他模板先继续以文档形式存在，不要一开始就做成过多一等档位。

### 6.2 preset 不是什么

`profile preset`：

- 不是一个插件
- 不是一个业务模块
- 不是一个 API handler

它只是：

- `capabilities`
- `plugins/features`
- 默认配置

的组合包。

## 7. 当前建议的最小目标模型

结合当前仓库现状，最适合先落地的是下面这个结构：

### 7.1 Kernel

- auth
- user/device
- conversation
- message delivery
- websocket
- ack/offline
- admin auth/rbac
- feature registry
- policy engine

### 7.2 Capabilities

- storage_mode
- message_archive
- message_search
- message_export
- audit_mode
- retention_policy
- e2ee_mode

### 7.3 Plugins

- channel
- moment
- location
- group_collab

### 7.4 Profile Presets

- community
- enterprise

## 8. 推荐重构顺序

### 第一批：先抽象，不搬目录

先把这些概念显式化：

1. `product_profile`
2. `capabilities`
3. `features`
4. `plugin_manifest`

先形成统一注册表，而不是先物理搬文件。

### 第二批：先处理 capability，再处理 plugin

优先顺序建议：

1. `storage_mode`
2. `message_archive`
3. `message_search`
4. `message_export`
5. `audit_mode`
6. `e2ee_mode`

因为企业版与非企业版最大的真实差异在这里，不在 `moment` 或 `location`。

### 第三批：再拆第一批插件

建议顺序：

1. `channel`
2. `group_collab`
3. `moment`
4. `location`

`channel` 最先，因为它当前成熟度高、边界清楚、最适合拿来做高配模块。

## 9. 暂不纳入一等分层对象的能力

以下能力当前仓库中有页面或痕迹，但建议先放入“观察区”，不要急着纳入正式模块化主线：

| 能力 | 当前迹象 | 建议 |
|---|---|---|
| `live_room` | App 页面存在 | 暂不进入主线 |
| `wallet` | App 页面存在 | 暂不进入主线 |
| 其他一次性实验入口 | 零散页面/脚本 | 先不归入正式 layer |

原则：

- 没有稳定后端契约
- 没有清晰销售叙事
- 没有持续维护计划

的能力，不应过早进入正式模块/插件模型。

## 10. 最后只记这 4 句话

- `kernel` 是骨架
- `capability` 是规则
- `plugin` 是功能
- `profile preset` 是套餐

如果又混了，就回去看：

- `module-layer-cheatsheet.md`
