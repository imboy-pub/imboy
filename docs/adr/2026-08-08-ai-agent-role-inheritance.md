# ADR: AI Agent 通过角色模板继承行为能力

- 状态：Accepted
- 日期：2026-08-08
- 关联：`ai_agent`、AI 角色模板、知识库按需检索、管理后台 AI 助手页

## 背景

当前系统同时存在三种容易混淆的表述：AI 助手、AI Agent 和 AI 角色。后端已经有 `ai_agent` 表，但角色仍以 `config.ai_roles` 的 KV 形式保存，只有 `role_id -> system_prompt`，无法支持分页、版本、绑定数量、能力策略和审计。`ai_agent.capabilities` 虽然已经落库，却没有作为运行时门控；知识库配置还会被直接追加到每次模型请求中，造成不必要的上下文和模型成本。

## 决策

### 1. 统一实体概念

- AI 助手是产品名称，AI Agent 是技术名称，两者指同一个可聊天账号。
- AI 角色是可复用的行为模板，不是用户账号，也不是第三种 Agent。
- 知识库是独立资源，通过角色的知识库策略被按需访问。
- 第一阶段不引入 AI Group、多 Agent 协作或通用工作流编辑器。

### 2. 配置归属

```text
平台安全与成本上限
        ↓
角色发布版本：Prompt + capabilities + knowledge_policy
        ↓
AI Agent：身份 + provider/model + role_code + status/visibility
        ↓
消息触发场景：C2C / C2G / onboarding
```

角色是 `system_prompt`、能力和知识库策略的唯一来源。Agent 只能绑定角色，不能通过新管理接口覆盖角色能力。provider、model、头像、昵称、可见性和状态继续归 Agent 所有。

### 3. 能力采用固定策略目录

第一阶段只支持：

- 基础对话：隐式开启；
- 知识库：`off`、`on_demand`、`required`；
- 群聊回复：`off`、`mention_only`；
- 主动消息：`off`、`welcome_only`。

未知能力拒绝保存。E2EE fail-closed、群聊触发规则、onboarding 总开关和限流属于平台硬规则，角色不能覆盖。

### 4. 角色使用草稿与发布版本

角色编辑先产生 draft version，经过配置校验后发布。发布前显示当前绑定 Agent 数量；published version 变更后，所有绑定 Agent 使用新版本。旧版本保留为 archived，支持审计和回滚。

### 5. 知识库使用按需检索

第一阶段不再无条件注入完整知识库。`off` 不读取；`on_demand` 先用关键词/规则判断，再读取有限上下文；`required` 才强制读取。当前 FAQ/群规配置复用为首个来源，embedding、外部 rerank 和独立知识库表留作后续扩展。

## 后果

- 管理后台概念减少为“AI 助手”和“AI 角色模板”两层。
- 多个 Agent 可以复用同一角色，角色发布可以统一改变行为。
- 角色能力真正参与群聊、主动消息和知识库执行链路。
- 角色页需要处理影响范围、发布和版本，不再只是 KV 编辑器。
- 旧 `ai_roles`、`ai_agent.system_prompt` 和 `ai_agent.capabilities` 在兼容期仍需保留回退，避免已有部署行为突然改变。

## 非目标

本 ADR 不引入多 Agent 编排、通用工具市场、外部搜索、代码执行、图片分析、按 token 计费或通用工作流画布。
