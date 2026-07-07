# Imboy 模块分层速查表

> Last Updated: 2026-03-13
> Purpose: 用最少概念快速判断一个能力应该放在 `kernel`、`capability`、`plugin` 还是 `profile preset`
> Related docs: `overview.md`, `../guides/module-feature-flag-config.md`

## 1. 一句话记忆

- `Kernel`：系统骨架，没它系统不成立。
- `Capability`：系统规则，决定系统怎么运行。
- `Plugin`：业务能力，决定系统多了什么功能。
- `Profile Preset`：默认套餐，决定卖哪套组合。

## 2. 只问 3 个问题

遇到一个新能力时，只问下面 3 个问题：

1. 没有它，系统主链路还成立吗？
2. 它是在增加业务功能，还是在改变系统运行规则？
3. 它是单个功能，还是面向某类客户的一整套默认组合？

判断规则：

- 如果没有它，系统主链路就不成立：`Kernel`
- 如果它主要改变系统运行规则：`Capability`
- 如果它主要增加一个独立业务功能：`Plugin`
- 如果它是面向某类客户的一组默认配置：`Profile Preset`

## 3. 最短判断版

### 3.1 Kernel

判断句：

- “所有部署都必须有”
- “所有业务都会依赖”
- “关掉以后系统就不像 IM 了”

典型例子：

- 认证与登录
- 用户与设备
- 会话
- 消息投递
- WebSocket
- ACK / 离线同步
- RBAC
- Feature Registry
- Policy Engine

### 3.2 Capability

判断句：

- “它不是页面功能，而是系统规则”
- “它会影响多个模块”
- “它会改变消息如何存、查、导、审”

典型例子：

- `storage_mode = archived | secure_e2ee`
- `message_search = true | false`
- `message_export = true | false`
- `audit_mode = none | metadata | full`
- `retention_policy`
- `e2ee_mode = disabled | optional | required | compliance`

### 3.3 Plugin

判断句：

- “它是一个可开可关的业务功能”
- “关掉以后，系统主链路还成立”
- “它通常有独立页面、菜单、路由或 API”

典型例子：

- `channel`
- `moment`
- `location`
- `group_vote`
- `group_schedule`
- `group_task`

### 3.4 Profile Preset

判断句：

- “它不是一个功能，而是一套默认组合”
- “它是拿来卖、拿来交付、拿来部署的”
- “它回答的是默认给哪类客户哪套配置”

典型例子：

- `community`
- `enterprise`

## 4. 一个最容易记住的对照表

| 层 | 回答的问题 | 例子 |
|---|---|---|
| `Kernel` | 系统没有它能不能活？ | auth / conversation / websocket / ack |
| `Capability` | 系统到底怎么运行？ | archive / e2ee / search / export / audit |
| `Plugin` | 系统多了什么业务功能？ | moment / channel / location / group_task |
| `Profile Preset` | 默认卖哪套组合？ | community / enterprise |

## 5. Imboy 当前的推荐理解

### 5.1 Kernel

- 登录注册与鉴权
- 用户、设备、会话
- 消息投递
- ACK / 离线同步
- WebSocket 协议
- 后台登录与 RBAC
- 功能开关读取
- 策略引擎

### 5.2 Capability

- 消息归档模式
- E2EE 模式
- 搜索能力
- 导出能力
- 审计能力
- 留存策略

### 5.3 Plugin

- 频道
- 朋友圈
- 位置
- 群投票
- 群日程
- 群任务

### 5.4 Profile Preset

- `community`
- `enterprise`

## 6. 最重要的纪律

### 6.1 不要把所有东西都做成插件

这些不应插件化：

- 认证
- 会话
- 消息投递
- ACK
- 离线同步
- 权限系统
- Feature Registry
- Policy Engine

### 6.2 不要把系统规则误当成业务插件

这些更像 `capability`，不应只用普通功能开关表达：

- 是否归档消息
- 是否支持全文搜索
- 是否支持消息导出
- 是否允许 E2EE
- 审计记录到什么粒度

### 6.3 不要把企业版做成一个“大插件”

`enterprise` 不是插件，而是一组：

- `capabilities`
- `plugins`
- 默认配置

的组合。

## 7. 以后忘了时，直接背这 4 句话

- `Kernel` 是骨架。
- `Capability` 是规则。
- `Plugin` 是功能。
- `Profile Preset` 是套餐。

如果还拿不准，就再问一次：

1. 没有它，系统还能不能成立？
2. 它是在加功能，还是在改规则？
3. 它是单个能力，还是一整套默认配置？
