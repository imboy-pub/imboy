# 操作指南（How-To Guides）

> **本目录定位**：任务导向。帮有明确目标的读者完成一件具体的事。

**写作要求**：
- 标题是任务：「如何备份生产数据库」，不是「数据库备份介绍」
- 开头一句话说清适用场景与前提
- 步骤可跳读，每步自包含，不写「如上所述」
- 有副作用的操作必须给回滚方案

**判断标准**：如果读者是「来学东西」而不是「来办事情」，这篇该去 `tutorials/`。

## 现有指南

| 文档 | 内容 |
|------|------|
| [sentry-dsn-integration-guide.md](./sentry-dsn-integration-guide.md) | Sentry DSN 接入配置 |

## 子目录规划

| 子目录 | 内容 | 迁移来源 |
|--------|------|---------|
| `operations/` | 备份恢复、节点控制、诊断、压测 | 现 `docs/operations/` |
| `release/` | 发版流程、应用商店清单 | 现 `docs/release/` |
| `migrations/` | 数据库迁移操作 | 现 `docs/migrations/` |
| `e2ee/` | E2EE 配置与轮换操作 | 现 `docs/e2ee/` |
| `plugin/` | 插件安装与运维 | 现 `docs/plugin/` |

模板：见 [documentation-system/templates/HOWTO_TEMPLATE.md](../documentation-system/templates/HOWTO_TEMPLATE.md)
