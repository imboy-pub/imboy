# 核心业务概念（Core Concepts）

> 本目录是 IMBoy 核心业务概念的权威定义层：每个领域一篇，回答「这个域是什么、现在实现到什么程度、边界在哪」。
> 术语的中英对照与精确定义见[术语表](../glossary.md)；协议细节、部署、API 清单不在本层重复维护。

## 概念地图

```
平台
├── 账号与主体（accounts-and-actors.md）
│     User（human / agent / system_bot / bot）· Device · Platform Admin
│
├── 协作层级（collaboration-hierarchy.md）
│     组织（Organization）
│       └── 工作区（Workspace）
│             ├── 项目（Project：任务/里程碑/成员/频道关联）
│             ├── 群组（Group，scope=personal|workspace）
│             └── 频道（Channel，scope=personal|workspace）
│
├── 消息模型（messaging-model.md）
│     C2C / C2G / C2S / S2C · conv_seq · 交付确认 · 离线补投
│
├── 端到端加密（e2ee.md）
│     Olm（单聊）· Megolm（群）· 设备信任 · 4S 备份
│
├── 智能体域（agent.md）
│     Grant（授权）→ Run（执行）→ Effect（工具账本）· Hirð · HITL
│
├── 客服域（customer-service.md）
│     业务身份 → 坐席 · 客服会话 · 访客令牌 · 挂件
│
└── 企业业务域（enterprise-business.md）
      企业联系人 · 企业会话/消息/资产 · 留持 · 离岗交接
```

三个业务切片（智能体 / 客服 / 企业业务）共享一个前置概念：**业务身份（Business Identity）**——组织的稳定经办主体，客服坐席与企业消息发送方都挂靠它。

## 阅读顺序建议

1. 新成员：账号与主体 → 协作层级 → 消息模型。
2. 做组织/工作区功能：协作层级 + `docs/architecture/` 下 2026-09-16 冻结契约系列。
3. 做加密：端到端加密 + [E2EE 协议规范](../reference/e2ee-protocol-specification.md)。
4. 做智能体/客服/企业业务：对应概念文档 + `docs/architecture/` 冻结契约。

## 状态标注约定

本目录文档统一使用四种状态标注，严格遵守不混写：

| 标注 | 含义 |
|---|---|
| **CURRENT** | 代码已实现且可用（给出代码/迁移证据） |
| **TARGET** | 已确定但尚未完全实现的目标设计 |
| **PLAN** | 实施计划（指向 plans/） |
| **UNKNOWN** | 尚未确认的事实 |
