# 操作指南（How-To Guides）

> **本目录定位**：任务导向。帮有明确目标的读者完成一件具体的事。

**写作要求**：
- 标题是任务：「如何备份生产数据库」，不是「数据库备份介绍」
- 开头一句话说清适用场景与前提
- 步骤可跳读，每步自包含，不写「如上所述」
- 有副作用的操作必须给回滚方案

**判断标准**：如果读者是「来学东西」而不是「来办事情」，这篇该去 `tutorials/`。

## 子目录

| 子目录 | 内容 | 规模 |
|--------|------|------|
| [operations/](./operations/) | 部署运维：备份恢复、升级、监控、集群、Garage | 17 篇 |
| [testing/](./testing/) | 测试指南：单元/集成/E2E/性能/混沌测试 | 16 篇 |
| [e2ee/](./e2ee/) | E2EE 配置与协议专题（含 v2/ 子目录，待 owner 细分） | 36 篇 |

## 单篇指南

| 文档 | 内容 |
|------|------|
| [sentry-dsn-integration-guide.md](./sentry-dsn-integration-guide.md) | Sentry DSN 接入配置 |

## 待迁入

- `migrations/`（数据库迁移操作）
- `plugin/`（插件安装与运维）

模板：见 [documentation-system/templates/HOWTO_TEMPLATE.md](../documentation-system/templates/HOWTO_TEMPLATE.md)
