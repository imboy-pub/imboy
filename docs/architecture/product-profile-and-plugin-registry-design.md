# Imboy 产品档位与插件注册设计

> Last Updated: 2026-03-13
> Status: 目标设计文档
> Purpose: 把 `product_profile`、`capabilities`、`features`、`plugin_manifest` 与 `policy_engine` 明确成可落地模型，避免后续模块化时继续把“套餐”“规则”“功能”混在一起
> Source of truth: `config/sys.config.example`, `src/lib/imboy_feature.erl`, `src/api/app_feature_handler.erl`, `src/adm/adm_admin_handler.erl`
> Related docs: `overview.md`, `module-layer-cheatsheet.md`, `current-module-classification.md`, `../guides/module-feature-flag-config.md`, `../operations/security.md`

## 1. 这份文档解决什么问题

当前仓库已经有：

- `features` 配置；
- 三端读取功能矩阵的接口；
- 一批可以按模块开关的业务能力；
- 一些已经显露出来、但还没正式建模的系统规则，例如消息搜索、导出、审计和 `E2EE`。

但当前还缺 4 个一等对象：

1. `product_profile`：这套部署到底面向谁；
2. `capabilities`：系统到底按什么规则运行；
3. `plugin_manifest`：每个插件的边界、依赖、入口和约束是什么；
4. `policy_engine`：谁来统一解释上面这些配置。

如果缺这 4 个对象，后面会持续出现这些问题：

- `enterprise` 和 `community` 只能靠口头理解，不能靠系统表达；
- `E2EE`、搜索、导出、审计这些系统规则，会被误当成普通页面功能；
- `channel`、`moment`、`group_vote` 之类的业务模块无法形成稳定可售卖边界；
- 后端、App、后台只能拿到一堆布尔开关，但不知道这些开关为什么这样生效。

## 2. 先记最短版

- `product_profile`：卖哪套套餐。
- `capabilities`：系统按什么规则运行。
- `features`：当前部署显式打开了哪些功能入口。
- `plugin_manifest`：每个插件自己的说明书。
- `policy_engine`：把上面四者合并成最终生效策略。

以后如果又忘了，就只记一句：

**套餐定默认，规则定边界，功能定入口，清单定依赖，策略引擎定最终结果。**

## 3. 核心设计决定

### 3.1 不走动态插件平台

当前最适合 `Imboy` 的不是“运行时热插拔插件平台”，而是：

- 单代码库；
- 模块化单体；
- 声明式插件注册；
- 配置驱动启停；
- 三端统一读取同一份生效策略。

原因：

- 你当前三端代码已经是强耦合演进出来的，不适合一上来做复杂动态装载；
- 真正需要的是“卖的时候能裁剪，交付时能解释，开发时边界清楚”，不是“任意下载插件包”；
- 动态插件平台会显著增加发布、兼容、测试和故障排查复杂度。

### 3.2 `enterprise` 不是插件

`enterprise` 不应该是一个叫“企业版”的大插件。

它本质上是下面三者的组合：

- 一组 `capabilities`
- 一组 `features`
- 一组默认配置

所以它应该建模为：

- `product_profile = enterprise`

而不是：

- `feature.enterprise = true`

### 3.3 `E2EE` 不只是 feature

`E2EE` 会影响：

- 消息如何存储；
- 后台能否查看正文；
- 是否支持全文搜索；
- 是否允许消息导出；
- 审计能到什么粒度。

所以它首先是 `capability`，其次才可能带出某些页面入口或客户端流程。

### 3.4 先显式化模型，再考虑物理拆分目录

后续第一步不是搬代码目录，而是把这些对象显式化：

1. `product_profile`
2. `capabilities`
3. `features`
4. `plugin_manifest`
5. `policy_engine`

只要这 5 个对象稳定了，后面你想继续保持单体、还是逐步拆模块，选择空间都会更大。

## 4. 推荐的最小目标模型

### 4.1 顶层结构

建议最终形成下面这套最小结构：

```text
product_profile
    -> preset defaults
    -> capabilities
    -> features
    -> plugin_manifest registry
    -> policy_engine
    -> effective_policy
```

其中：

- `product_profile` 决定默认套餐；
- `capabilities` 决定系统规则；
- `features` 决定当前部署打开哪些业务入口；
- `plugin_manifest` 提供插件元数据；
- `policy_engine` 负责合并并校验；
- `effective_policy` 是三端真正消费的结果。

### 4.2 生效顺序

推荐固定成下面 5 步：

1. 读取 `product_profile`
2. 装载该 profile 的默认 `capabilities` 和默认 `features`
3. 应用部署侧显式配置覆盖
4. 根据 `plugin_manifest` 解析依赖与约束
5. 输出 `effective_policy`

这样做的意义是：

- 你知道“为什么这个功能是开的”；
- 你知道“是套餐默认打开，还是项目交付时手工改开的”；
- 你知道“它开了以后是否违反系统规则”。

## 5. 配置模型建议

### 5.1 推荐配置块

建议在 `sys.config` 中最终形成 3 个顶层块：

```erlang
{product_profile, community},

{capabilities, #{
    storage_mode => archived,
    e2ee_mode => disabled,
    message_search => false,
    message_export => false,
    audit_mode => none,
    retention_policy => #{
        mode => forever
    }
}},

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

这里有两个关键点：

1. `plugin_manifest` 不建议写进部署配置，而建议留在代码注册表中；
2. `features` 先保持与当前仓库兼容，不急着一次性改名。

### 5.2 为什么 `plugin_manifest` 不放到部署配置

因为部署配置适合表达：

- 这个环境开不开；
- 这个环境默认是什么；
- 这个环境是否允许某能力。

但它不适合承载插件的结构元数据，例如：

- 插件依赖哪个父插件；
- 需要哪些 capability 才能工作；
- 哪些 API / 菜单 / 页面属于这个插件；
- 哪些 feature key 属于同一个聚合插件。

这些应该是代码级事实，而不是交付时临时填写的配置。

## 6. `product_profile` 设计

### 6.1 定义

`product_profile` 表示：

- 当前部署默认面向哪类客户；
- 默认采用哪一组系统规则；
- 默认交付哪些业务能力。

它的目标不是替代所有开关，而是提供一个稳定起点。

### 6.2 第一阶段只保留两个一等 profile

建议正式只保留两个：

- `community`
- `enterprise`

其他像：

- 基础版
- 专业版
- 行业扩展版

都可以先继续当销售或交付口径，不急着做成系统一等对象。

### 6.3 推荐默认矩阵

#### `community`

更适合：

- 私域社群；
- 小组织；
- 非强监管环境；
- 更看重轻量与私密感的交付。

**当前阶段安全默认值**建议：

```erlang
#{
    storage_mode => archived,
    e2ee_mode => optional,
    message_search => false,
    message_export => false,
    audit_mode => metadata,
    retention_policy => #{mode => rolling_days, days => 30}
}
```

这里把 `storage_mode` 先设为 `archived`，不是因为目标不是“更安全”，而是因为当前仓库还不能对外笼统承诺“所有消息默认真正端到端加密且服务端不可见”。

这与 `docs/operations/security.md` 中的口径保持一致。

#### `enterprise`

更适合：

- 企业私有部署；
- 有治理、审计、导出、留存要求的组织；
- 需要明确交付边界和后台控制能力的客户。

建议默认值：

```erlang
#{
    storage_mode => archived,
    e2ee_mode => disabled,
    message_search => true,
    message_export => true,
    audit_mode => full,
    retention_policy => #{mode => rolling_days, days => 365}
}
```

### 6.4 未来目标矩阵

当你后续真正把“密聊模式”做完整之后，再新增下面这个真实能力切换：

```erlang
storage_mode => secure_e2ee
```

那时 `community` 才更适合默认切到：

- `storage_mode = secure_e2ee`
- `message_search = false`
- `message_export = false`
- `audit_mode = metadata`

在这之前，系统和销售口径都不应过度承诺。

## 7. `capabilities` 设计

### 7.1 能力清单

建议第一批先显式支持这些：

- `storage_mode = archived | secure_e2ee`
- `e2ee_mode = disabled | optional | required`
- `message_search = true | false`
- `message_export = true | false`
- `audit_mode = none | metadata | full`
- `retention_policy`

这是最小够用集合，先不要加太多花哨能力。

### 7.2 能力约束矩阵

`capabilities` 不是简单并列开关，其中有明确约束关系。

建议至少固化下面这些规则：

| 规则 | 约束 |
|---|---|
| `storage_mode = secure_e2ee` | `message_search` 必须为 `false` |
| `storage_mode = secure_e2ee` | `message_export` 必须为 `false` 或仅元数据导出 |
| `storage_mode = secure_e2ee` | `audit_mode` 不允许是 `full` |
| `audit_mode = full` | `storage_mode` 必须为 `archived` |
| `e2ee_mode = required` | `message_search` 不应开启 |
| `e2ee_mode = required` | 后台正文查看不应开启 |

这些规则不应该散落在多个 `handler` 或页面里，而应集中交给 `policy_engine` 判断。

### 7.3 当前代码最先要接 capability 的地方

第一批最值得接入 capability 判断的不是插件页面，而是消息主链路：

- 消息存储与 ACK 语义
- 搜索入口
- 导出入口
- 后台消息查询
- E2EE 相关流程

对应当前仓库里的重点位置包括：

- `src/logic/msg_ack_logic.erl`
- `src/ds/msg_store_ds.erl`
- `src/repo/msg_store_repo.erl`
- `src/adm/adm_message_handler.erl`
- `src/api/fts_handler.erl`
- `src/api/e2ee_handler.erl`

## 8. `features` 设计

### 8.1 继续保留，但重新定位

当前 `features` 已经有真实价值，不需要推翻。

它更适合重新定义为：

- 部署层显式功能开关；
- 三端入口显隐与后端路由门禁；
- 插件启停的可交付开关面。

它不应再独自承担这些职责：

- 定义产品档位；
- 定义系统运行规则；
- 描述插件结构。

### 8.2 当前 feature 列表的推荐归属

| 当前 key | 推荐归属 |
|---|---|
| `core` | kernel 保底开关 |
| `e2ee` | 页面/流程入口 feature，但其真实行为受 capability 约束 |
| `channel` | `channel` 插件主开关 |
| `location` | `location` 插件主开关 |
| `moment` | `moment` 插件主开关 |
| `group_vote` | `group_collab` 子开关 |
| `group_schedule` | `group_collab` 子开关 |
| `group_task` | `group_collab` 子开关 |

### 8.3 为什么暂时不强改 key

例如 `group_vote`、`group_schedule`、`group_task`，长期更适合归并到聚合插件 `group_collab`。

但第一阶段不建议直接把配置 key 全部重命名，因为这会同时影响：

- 后端配置；
- App 功能矩阵消费；
- 后台菜单判断；
- 测试与部署样例。

更稳妥的方式是：

1. 保留原 key；
2. 在 `plugin_manifest` 中声明它们同属 `group_collab`；
3. 后续在稳定后再考虑统一命名。

## 9. `plugin_manifest` 设计

### 9.1 作用

`plugin_manifest` 用来描述插件本身，而不是描述某次部署要不要打开。

它至少应回答 6 个问题：

1. 这个插件叫什么；
2. 它对应哪些 feature key；
3. 它依赖哪些其他插件；
4. 它需要哪些 capability 才能正常工作；
5. 它影响哪些 App / Admin / API 入口；
6. 它属于独立插件还是聚合插件。

### 9.2 推荐结构

建议后端增加一个注册表模块，例如：

- `src/lib/imboy_plugin_registry.erl`

它返回类似下面的静态清单：

```erlang
#{
    channel => #{
        kind => plugin,
        feature_keys => [channel, channel_discover, channel_invitation, channel_order],
        requires_capabilities => [],
        depends_on_plugins => [],
        app_entries => [channel_tab, channel_discover_page],
        admin_entries => [channels_page],
        api_handlers => [channel_handler]
    },
    moment => #{
        kind => plugin,
        feature_keys => [moment],
        requires_capabilities => [],
        depends_on_plugins => [],
        app_entries => [moment_tab],
        admin_entries => [moments_page],
        api_handlers => [moment_handler]
    },
    location => #{
        kind => plugin,
        feature_keys => [location],
        requires_capabilities => [],
        depends_on_plugins => [],
        app_entries => [people_nearby_page],
        admin_entries => [],
        api_handlers => [location_handler]
    },
    group_collab => #{
        kind => aggregate_plugin,
        feature_keys => [group_vote, group_schedule, group_task],
        children => [vote, schedule, task],
        requires_capabilities => [],
        depends_on_plugins => [],
        app_entries => [group_vote_page, group_schedule_page, group_task_page],
        admin_entries => [
            group_vote_manage_page,
            group_schedule_manage_page,
            group_task_manage_page
        ],
        api_handlers => [
            group_vote_handler,
            group_schedule_handler,
            group_task_handler
        ]
    }
}
```

### 9.3 这份清单的价值

有了 `plugin_manifest`，你以后每加一个模块，就不用再靠脑子记：

- 该加几个开关；
- 需要隐藏哪些菜单；
- 依赖了哪些父能力；
- 属于哪个销售模块。

只要补一份 manifest，再让 `policy_engine` 解释即可。

## 10. `policy_engine` 设计

### 10.1 它负责什么

`policy_engine` 是整套设计里最关键的胶水层。

它的职责应该固定成 5 件事：

1. 读取 `product_profile`
2. 装载 profile 默认值
3. 读取部署侧 `capabilities` 与 `features`
4. 根据 `plugin_manifest` 解析依赖
5. 校验冲突并输出 `effective_policy`

### 10.2 推荐输出

建议最终对外形成一个统一生效结果，例如：

```erlang
#{
    profile => enterprise,
    capabilities => #{
        storage_mode => archived,
        e2ee_mode => disabled,
        message_search => true,
        message_export => true,
        audit_mode => full,
        retention_policy => #{mode => rolling_days, days => 365}
    },
    features => #{
        core => true,
        e2ee => false,
        channel => true,
        location => false,
        moment => false,
        channel_discover => false,
        channel_invitation => true,
        channel_order => false,
        group_vote => false,
        group_schedule => false,
        group_task => false
    },
    plugins => #{
        channel => #{enabled => true},
        moment => #{enabled => false},
        location => #{enabled => false},
        group_collab => #{enabled => false}
    }
}
```

### 10.3 当前仓库里的推荐承接关系

建议职责演进如下：

| 当前模块 | 后续定位 |
|---|---|
| `src/lib/imboy_feature.erl` | 继续保留，逐步退化为 feature 兼容读取层 |
| `app_feature_handler` | 未来可升级为输出 `effective_policy` 的 App 视图 |
| `adm_admin_handler` 中的功能矩阵接口 | 未来可升级为输出 `effective_policy` 的 Admin 视图 |
| 新增 `imboy_profile_preset.erl` | 提供 profile 默认值 |
| 新增 `imboy_plugin_registry.erl` | 提供插件 manifest |
| 新增 `imboy_policy.erl` | 负责合并与校验 |

### 10.4 一开始不要做的事情

第一阶段不建议：

- 做复杂数据库化配置中心；
- 做运行时热更新插件；
- 让 App 自己解释 capability 冲突；
- 让各个业务模块各写一份依赖判断。

这些都会让复杂度快速失控。

## 11. 当前与目标之间的映射关系

### 11.1 当前已有的东西

当前已经有的基础：

- `features` 配置块；
- `imboy_feature:enabled/1`；
- App 与后台读取功能矩阵接口；
- `channel` / `moment` / `location` / `group_vote` 等真实模块资产；
- `E2EE`、搜索、导出、审计相关实现痕迹。

### 11.2 当前缺的东西

当前主要缺的是：

- `product_profile` 显式配置；
- `capabilities` 显式配置；
- `plugin_manifest` 显式注册；
- `policy_engine` 统一解释；
- 能力冲突的集中校验。

### 11.3 这意味着什么

这意味着你现在不是“什么都没有”，而是：

- 已经有了 60% 的工程基础；
- 但关键的“解释层”还没建起来。

也就是说，后面工作的重点不是重写所有代码，而是先把解释层立住。

## 12. 分阶段落地顺序

### 阶段 1：只加模型，不改行为

目标：

- 新增 `product_profile`
- 新增 `capabilities`
- 新增 `imboy_profile_preset.erl`
- 新增 `imboy_plugin_registry.erl`
- 新增 `imboy_policy.erl`

要求：

- 当前线上行为不变；
- 现有 `features` 兼容；
- 三端接口暂时仍可返回原有扁平功能矩阵。

### 阶段 2：让 capability 真正生效

优先顺序建议：

1. `storage_mode`
2. `message_search`
3. `message_export`
4. `audit_mode`
5. `e2ee_mode`

要求：

- 搜索接口不再只看 feature，还要看 capability；
- 导出接口不再只看后台权限，还要看 capability；
- 后台消息正文查看能力要受 capability 约束。

### 阶段 3：把第一批插件注册起来

建议顺序：

1. `channel`
2. `group_collab`
3. `moment`
4. `location`

要求：

- 每个插件补 manifest；
- 每个插件明确 feature key；
- App / Admin / API 入口都能和 manifest 对齐。

### 阶段 4：再做销售与交付包装

这时再去沉淀：

- 社区版默认模板；
- 企业版默认模板；
- 行业交付模板；
- 客户验收 checklist；
- 售卖说明中的“可选模块清单”。

先有架构，再有售卖包装，顺序不要反。

## 13. 你现在最该怎么记住它

如果你以后又乱了，不用把整篇文档都背下来，只背这 5 句：

1. `product_profile` 是套餐。
2. `capabilities` 是规则。
3. `features` 是入口开关。
4. `plugin_manifest` 是插件说明书。
5. `policy_engine` 是总解释器。

然后再补一句最实用的判断：

**企业版和非企业版最大的真实差异，不在页面多几个入口，而在消息怎么存、能不能查、能不能导、能不能审。**

## 14. 当前推荐结论

对 `Imboy` 来说，当前最稳妥的产品化路径是：

- 继续坚持单代码库；
- 用 `product_profile + capabilities + features + plugin_manifest` 建立解释层；
- 先把企业私有部署场景需要的能力边界做实；
- 再把 `channel`、`group_collab`、`moment`、`location` 逐步做成可售卖模块；
- 在真正完成安全语义前，不夸大 `E2EE` 的默认交付承诺。

这条路的优点是：

- 不会把项目拆散；
- 能同时兼容企业需求和非企业需求；
- 还能逐步长出更清楚的销售边界。
