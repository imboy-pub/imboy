# Imboy 模块开关配置草案（三端统一）

> Last Updated: 2026-03-08  
> Status: 长期设计草案  
> Related docs: `doc/guides/phase2-modularization-strategy.md`, `doc/guides/deployment.md`, `doc/api/rest-api.md`

## 1. 文档目的

本文档定义 `Imboy` 的模块开关配置草案，用于后端、App 和管理后台三端统一地控制功能显隐与授权。

目标：

1. 一期主包默认稳定；
2. 二期模块默认关闭；
3. 三端能读取同一份模块定义；
4. 销售、部署、联调与验收都能围绕同一开关体系执行。

## 2. 设计原则

1. **后端为事实源**：模块最终状态以后端配置为准；
2. **客户端负责入口控制**：隐藏入口、控制路由、做关闭态兜底；
3. **后台负责菜单与权限控制**：菜单、页面、操作权限联动；
4. **开关命名稳定**：避免随版本反复改名；
5. **关闭优先安全**：模块关闭时优先返回稳定的“未启用”结果；
6. **版本与模块解耦**：版本决定默认开关组合，模块开关决定实际能力集。

## 3. 推荐配置结构

建议在后端 `sys.config` 中为 `imboy` 应用增加 `features` 配置块。

推荐结构：

```erlang
{features, #{
    core => #{enabled => true},
    e2ee => #{enabled => false},
    channel => #{enabled => false},
    location => #{enabled => false},
    moment => #{enabled => false},
    channel_discover => #{enabled => false},
    channel_invitation => #{enabled => false},
    channel_order => #{enabled => false},
    group_vote => #{enabled => false},
    group_schedule => #{enabled => false},
    group_task => #{enabled => false}
}}
```

## 4. 模块定义建议

### 4.1 核心主包

默认开启：

- `core`

其覆盖的一期主包能力包括：

1. 登录注册与鉴权；
2. 单聊 / 群聊 / 会话；
3. ACK / 离线同步；
4. 好友、黑名单、标签；
5. 搜索、`@mention`、收藏；
6. 群公告、群文件、群相册；
7. 基础后台治理。

### 4.2 高配能力

建议单独开关：

- `e2ee`
- `channel`

### 4.3 二期模块

建议单独开关：

- `location`
- `moment`
- `channel_discover`
- `channel_invitation`
- `channel_order`
- `group_vote`
- `group_schedule`
- `group_task`

## 5. 推荐环境模板

### 5.1 基础版默认模板

```erlang
{features, #{
    core => #{enabled => true},
    e2ee => #{enabled => false},
    channel => #{enabled => false},
    location => #{enabled => false},
    moment => #{enabled => false},
    channel_discover => #{enabled => false},
    channel_invitation => #{enabled => false},
    channel_order => #{enabled => false},
    group_vote => #{enabled => false},
    group_schedule => #{enabled => false},
    group_task => #{enabled => false}
}}
```

### 5.2 专业版默认模板

```erlang
{features, #{
    core => #{enabled => true},
    e2ee => #{enabled => false},
    channel => #{enabled => true},
    location => #{enabled => false},
    moment => #{enabled => false},
    channel_discover => #{enabled => false},
    channel_invitation => #{enabled => true},
    channel_order => #{enabled => false},
    group_vote => #{enabled => false},
    group_schedule => #{enabled => false},
    group_task => #{enabled => false}
}}
```

### 5.3 行业扩展模板示例

```erlang
{features, #{
    core => #{enabled => true},
    e2ee => #{enabled => true},
    channel => #{enabled => true},
    location => #{enabled => false},
    moment => #{enabled => true},
    channel_discover => #{enabled => true},
    channel_invitation => #{enabled => true},
    channel_order => #{enabled => true},
    group_vote => #{enabled => true},
    group_schedule => #{enabled => true},
    group_task => #{enabled => true}
}}
```

## 6. 后端落地建议

后端建议承担以下职责：

1. 提供统一 `feature_enabled(Module)` 读取方法；
2. 在 Router 或 Handler 层统一拦截未启用模块；
3. 对外返回稳定错误码与错误消息；
4. 为后台和 App 提供只读模块配置查询接口。

### 6.1 缺省兼容策略

当前后端实现采用“缺省兼容优先”的读取策略：

1. 整个 `features` 配置块缺失时，已登记功能按“默认开启”处理，避免存量环境升级后直接不可用；
2. 单个功能项缺失，或存在但未声明 `enabled` 时，该功能仍按“默认开启”处理；
3. 兼容读取 `true/false` 与 `#{enabled => Bool}` 两类写法，但对外接口统一返回扁平布尔矩阵；
4. `channel_discover`、`channel_invitation`、`channel_order` 属于 `channel` 的依赖功能，即使子开关显式为 `true`，只要 `channel=false`，对外返回仍为 `false`。

因此，线上环境建议在 `sys.config` 中显式声明全部已售卖功能，避免新版本新增开关后因“缺省开启”被误暴露。


### 6.2 建议错误语义

建议模块关闭时返回统一业务语义，例如：

- 错误码语义：`FEATURE_DISABLED`（后端业务码 `5190`）
- 错误文案：`功能未启用`

这样三端更容易做一致兜底。

### 6.3 当前接口约定

当前已提供两个只读功能矩阵接口：

| 接口 | 方法 | 用途 | 权限 | 备注 |
| --- | --- | --- | --- | --- |
| `/v1/app/features` | `GET` | App 启动阶段拉取功能矩阵，控制入口、路由与关闭态兜底 | 公开接口，无需登录 | 属于开放路由，仅返回当前生效功能态 |
| `/adm/admin/config/features` | `GET` | 管理后台读取当前功能矩阵，联动菜单、页面和操作权限展示 | 需要后台登录态，且具备 `settings:view` 权限 | 无权限时返回 `403` 与 `无权限操作` |

补充约定：

- 两个接口的成功 `payload` 结构完全一致；
- 返回的是“生效后的功能态”，不是原始 `sys.config` 嵌套结构；
- 对依赖型功能，返回值会自动合并父开关状态；
- 非 `GET` 请求返回 `405 Method Not Allowed`。

返回示例：

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "core": true,
    "e2ee": false,
    "channel": true,
    "location": false,
    "moment": false,
    "channel_discover": false,
    "channel_invitation": true,
    "channel_order": false,
    "group_vote": false,
    "group_schedule": false,
    "group_task": false
  }
}
```

`payload` 字段约定如下：

| 字段 | 类型 | 说明 |
| --- | --- | --- |
| `core` | boolean | 核心主包能力 |
| `e2ee` | boolean | 端到端加密能力 |
| `channel` | boolean | 频道主能力 |
| `location` | boolean | 位置能力 |
| `moment` | boolean | 朋友圈能力 |
| `channel_discover` | boolean | 频道发现能力，依赖 `channel` |
| `channel_invitation` | boolean | 频道邀请能力，依赖 `channel` |
| `channel_order` | boolean | 频道订单能力，依赖 `channel` |
| `group_vote` | boolean | 群投票能力 |
| `group_schedule` | boolean | 群日程能力 |
| `group_task` | boolean | 群任务能力 |

换言之，客户端与后台不需要解析 `#{enabled => ...}` 这样的配置细节，只需要消费统一的布尔开关矩阵即可。

## 7. App 落地建议

App 建议增加统一 `FeatureRegistry`，负责：

1. 启动时拉取模块配置；
2. 缓存本地模块配置；
3. 控制路由、按钮、菜单和 Tab；
4. 在模块关闭时统一展示不可用提示。

### 7.1 建议控制点

优先接入以下位置：

1. 个人页更多入口；
2. 联系人页入口；
3. 频道入口；
4. 群详情扩展入口；
5. 路由守卫。

### 7.2 关闭态要求

1. 不出现空白页；
2. 不出现死链接；
3. 不出现只有部分控件隐藏、但 API 仍被调用的情况；
4. 已缓存旧入口时仍能优雅降级。

### 7.3 当前 App 接入现状（2026-03-08）

当前 `imboyapp` 已按“最小接入”策略完成以下控制点：

1. 启动阶段拉取 `GET /v1/app/features` 并缓存本地布尔矩阵；
2. 统一 `FeatureRegistry` 负责父子能力合并，其中 `channel_discover`、`channel_invitation`、`channel_order` 自动受 `channel` 父开关约束；
3. `GoRouter` 已增加模块路由守卫，旧链接或手工直达关闭模块时会回落到主框架页并提示“当前功能未启用”；
4. 底部 `Tab` 已对 `channel` 做显隐，关闭后自动收敛索引，避免旧 `index` 参数导致越界；
5. 联系人页已对 `location / people_nearby` 做入口显隐；
6. 频道列表页已对 `moment`、`channel_discover`、`channel_invitation` 做 action 级入口显隐；
7. 频道详情页已对邀请中心与订单入口做功能开关显隐；
8. 群投票 / 日程 / 任务当前至少已完成路由守卫，满足关闭态不再直达。

这一实现满足“一期主包稳定、二期模块默认关闭、客户购买后按配置开启”的交付要求。

## 8. 管理后台落地建议

后台建议基于现有菜单配置和 RBAC 体系扩展模块控制。

建议做法：

1. 菜单项增加 `feature` 字段；
2. 页面路由增加模块守卫；
3. RBAC 与 Feature 共同决定是否可见、是否可操作；
4. 模块关闭时，菜单和操作按钮都不展示。

### 8.1 菜单配置草案

建议在菜单配置中增加：

```json
{
  "path": "/moments",
  "label": "朋友圈治理",
  "permission": "moments:read",
  "feature": "moment",
  "enabled": true
}
```

### 8.2 当前后台接入现状（2026-03-08）

当前 `imboy-admin-frontend` 已完成以下最小接入：

1. 新增功能矩阵读取层，优先读取 `/adm/admin/config/features`，读取成功后写入本地缓存；
2. 因后台功能矩阵接口当前要求 `settings:view`，前端采用“能读远端就读，读不到则回退本地缓存，再不行不误伤现有 RBAC”的保守策略；
3. `Sidebar` 已在远端菜单配置与 `RBAC` 过滤后，再叠加 `feature` 过滤；
4. `App.tsx` 已为 `moment`、`channel`、`channel_invitation`、`channel_order`、`group_vote`、`group_schedule`、`group_task` 增加页面级模块守卫；
5. 频道详情页已对“邀请”“订单”按钮做显隐；
6. 群详情页已对“投票管理”“日程管理”“任务管理”按钮做显隐。

这样即使销售只交付一期主包，后台也不会继续暴露未售卖模块的菜单和页面。

## 9. 推荐实施顺序

### 第一步：只做文档和配置结构

已完成。当前三端统一使用同一套布尔矩阵命名，不再要求前端解析嵌套配置。

### 第二步：后台先行

已完成最小接入。当前后台已具备菜单过滤、路由守卫和详情页按钮显隐。

### 第三步：App 入口控制

已完成最小接入。当前 App 已具备启动拉取、缓存、入口显隐和路由守卫。

### 第四步：后端统一错误语义和配置接口

已完成基础闭环。后端已提供统一功能矩阵接口与 `5190 / 功能未启用` 语义。

## 10. 与销售和验收的关系

模块开关不是纯技术设计，它直接影响：

1. 报价项；
2. 交付边界；
3. 客户验收清单；
4. 发布门禁与回归策略。

因此每次新增模块时，都应该同步更新：

1. 版本对比表；
2. 报价单模板；
3. 验收清单；
4. 模块开关配置文档。

## 11. 相关文档

- `doc/guides/phase2-modularization-strategy.md`
- `doc/guides/product-edition-comparison.md`
- `doc/guides/quotation-template.md`
- `doc/guides/customer-acceptance-checklist.md`
