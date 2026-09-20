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
| [operations/](./operations/single-node-ops-chain-index-2026-08.md) | 部署运维：备份恢复、升级、监控、集群、Garage、benchmark | 18 篇 |
| [testing/](./testing/testing-strategy.md) | 测试指南：单元/集成/E2E/性能/混沌测试 | 16 篇 |
| [e2ee/](./e2ee/key-lifecycle.md) | E2EE 配置与协议专题（含 v2/ 子目录，待 owner 细分） | 37 篇 |
| [release/](./release/RELEASE.md) | 发版流程与应用商店上架清单 | 4 篇 |
| [payment/](./payment/payment-wallet-integration.md) | 支付集成（S4 支付宝/微信、钱包联调） | 2 篇 |
| [migrations/](./migrations/000070-user-dnd.md) | 数据库迁移操作 | 1 篇 |
| [security/](./security/security-hardening.md) | 安全加固 | 1 篇 |

## 单篇指南

| 文档 | 内容 |
|------|------|
| [sentry-dsn-integration-guide.md](./sentry-dsn-integration-guide.md) | Sentry DSN 接入配置 |
| [libraries-async.md](./libraries-async.md) | `elib_async` 使用指南 |
| [operations/erlang-otp29-installation.md](./operations/erlang-otp29-installation.md) | Ubuntu/Debian 安装 Erlang/OTP 29（源码/apt/kerl 三路径） |

模板：见 [documentation-system/templates/howto-template.md](https://github.com/imboy-pub/imboy/blob/main/docs/documentation-system/templates/howto-template.md)
