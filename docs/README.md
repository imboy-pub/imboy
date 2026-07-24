# IMBoy 文档门户

> 文档体系说明：本目录按 [Divio 四象限](./documentation-system/README.md) 组织——教程（学习）、指南（办事）、参考（查阅）、解释（理解）。过程材料（评审/审计/已完成计划）在 [archive/](./archive/)，进行中的工作在 [planning/](./planning/)。

## 我从这里开始（按角色）

| 我是谁 | 入口 |
|--------|------|
| 新加入的后端工程师 | [教程：本地跑通后端（15 分钟）](./tutorials/quickstart-backend.md) → [工程约定 CONVENTIONS](./CONVENTIONS.md) |
| 运维 / 私有化部署 | [生产部署](../deploy/README.md) → [备份与恢复](./guides/operations/deployment/BACKUP-RESTORE.md) |
| 前端 / 移动端开发 | [API 格式](./standards/api-format.md) → [REST API 目录](./reference/rest-api-v1-catalog.md) → [WebSocket 协议](./reference/ws-protocol-contract.md) |
| SDK 使用者（外部） | imboy-sdk-js 仓库 README（快速开始） |
| 插件开发者 | imboy-plugin-marketplace 仓库 README |
| 架构评审 / 技术买家 | [后端架构](./architecture/overview.md) → [ADR 决策记录](./adr/) |

## 四象限导航

| 象限 | 目录 | 用途 |
|------|------|------|
| 教程 | [tutorials/](./tutorials/) | 跟着做，做出可运行的成果 |
| 操作指南 | [guides/](./guides/) | 完成具体任务（备份、升级、配置、支付、安全加固） |
| 参考 | [reference/](./reference/) | 查事实（API、协议、错误码、工程笔记） |
| 解释 | [explanation/](./explanation/) | 理解设计理由与权衡 |

## 业务与规划

| 目录 | 内容 |
|------|------|
| [roadmap/](./roadmap/) | 产品与工程路线图（含 AI Agent 载体路线图） |
| [planning/](./planning/) | 进行中的计划：P0/P1 方案、执行清单、风险台账 |
| [business/](./business/) | 商业分析：变现策略、竞品对比 |
| [release/](./release/) | 发版流程与应用商店清单 |
| [legal/](./legal/) · [compliance/](./compliance/) | 法务（软著）与合规（等保、E2EE 披露） |
| [archive/](./archive/) | 归档：历史评审、审计、已完成计划（只进不出） |

## 常用专题

### 开发

- [后端架构](./architecture/overview.md)：系统分层和核心组件
- [模块地图](./architecture/module_map.md)：功能对应的代码位置
- [数据库访问](./architecture/database-access.md)：Repo 与 SQL 规范
- [API 格式](./standards/api-format.md)：请求、响应和分页约定
- [错误码](./standards/error-codes.md)：错误码定义与使用
- [REST API 目录](./reference/rest-api-v1-catalog.md)：`/api/v1` 接口速查
- [WebSocket 协议](./reference/ws-protocol-contract.md)：消息信封与事件约定

### 部署与运维

- [生产部署](../deploy/README.md)
- [备份与恢复](./guides/operations/deployment/BACKUP-RESTORE.md)
- [版本升级](./guides/operations/upgrade-runbook.md)
- [Garage 附件存储](./guides/operations/garage-deployment.md)
- [监控](./guides/operations/deployment/MONITORING.md)

### 安全

- [运行安全](./guides/operations/security.md)
- [安全加固](./guides/security/security-hardening.md)
- [漏洞报告](../SECURITY.md)

## 文档维护

- 写新文档前，先读 [写作规范](./documentation-system/writing-guide.md)，并从 [模板库](./documentation-system/templates/) 复制对应模板。
- 优先更新现有文档，不为一次性任务新建长期文档——一次性产物（评审/审计/计划）完成后归 [archive/](./archive/)。
- 本索引只收录稳定、仍可执行的内容；进行中的工作看 [planning/](./planning/)。
- 接口或部署方式变化时，同步更新根 README 和对应专题文档。
