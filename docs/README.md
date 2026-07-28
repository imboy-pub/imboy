# IMBoy 后端文档

> 本目录是 `imboy` 后端仓库的技术文档真源。面向开发、部署、集成、审计和维护；营销介绍请看 [imboy.pub](https://imboy.pub/)，轻量部署入口请看 [Wiki](https://github.com/imboy-pub/imboy/wiki)。

## 按目标开始

| 目标 | 入口 |
|---|---|
| 本地跑通后端 | [后端快速上手](./tutorials/quickstart-backend.md) |
| 生产部署 | [部署 README](https://github.com/imboy-pub/imboy/blob/main/deploy/README.md) → [Day-1 部署](./guides/operations/deployment/day1-quickstart.md) |
| 备份、恢复、升级 | [备份恢复](./guides/operations/deployment/backup-restore.md) · [升级手册](./guides/operations/upgrade-runbook.md) |
| 对接 REST / WebSocket | [API 格式](./reference/api-format.md) → [REST API 目录](./reference/rest-api-v1-catalog.md) → [WebSocket 协议](./reference/ws-protocol-contract.md) |
| 理解后端架构 | [架构总览](./architecture/overview.md) → [模块地图](./architecture/module-map.md) |
| 审核 E2EE / 合规 | [E2EE 协议](./reference/e2ee-protocol-specification.md) · [E2EE 策略](./compliance/e2ee-policy.md) · [等保清单](./compliance/mlps2-checklist.md) |
| 查看历史结论 | [规划中](./planning/) · [已归档](./archive/) |

## 文档分类

| 分类 | 回答的问题 | 目录 |
|---|---|---|
| 教程 | 我怎样从零做出一个可运行结果？ | [tutorials](./tutorials/) |
| 操作指南 | 我怎样完成部署、备份、测试或发布？ | [guides](./guides/) |
| 参考 | 参数、接口、协议和错误码是什么？ | [reference](./reference/) |
| 解释 | 为什么采用这种架构或安全设计？ | [explanation](./explanation/) · [architecture](./architecture/) |
| 业务与合规 | 产品边界、商业、安全披露是什么？ | [business](./business/) · [compliance](./compliance/) · [legal](./legal/) |
| 决策与过程 | 方案、审计和阶段性结论是什么？ | [adr](./adr/) · [planning](./planning/) · [archive](./archive/) |

判断规则：教技能是“教程”，办事情是“指南”，查事实是“参考”，讲原理是“解释”。一次性计划和已完成审计不进入稳定入口，完成后放入 `archive/`。

## 真源与发布关系

- **代码事实**：以 `src/`、`api/openapi.yaml`、`api/asyncapi.yaml`、`deploy/` 和可执行测试为准。
- **后端文档真源**：本目录 `imboy/docs/`；[GitHub Pages](https://imboy-pub.github.io/imboy/) 由 CI 构建发布，不在站点副本上直接改文档。
- **客户端文档**：见相邻仓库 [`imboyapp/docs`](https://github.com/imboy-pub/imboy-flutter/tree/main/docs)；管理后台、SDK 和插件分别维护自己的 README/文档。
- **Wiki**：只保留用户和运维最常用的短入口；详细协议、内部架构、审计证据不在 Wiki 复制。
- **产品官网**：只负责定位、能力和商业信息，不承担 API 或部署契约。

## 更新规则

1. API 或 WebSocket 变更，先更新机器可读契约，再更新对应参考文档和客户端说明。
2. 部署命令、环境变量或端口变更，同时更新 `deploy/` 与操作指南，并验证命令可执行。
3. 安全、合规和产品能力只写已实现或明确标注状态的事实；不要把规划写成现状。
4. 新文档先判断能否并入现有页面；阶段性产物完成后移入 `archive/`，不要继续挂在主入口。
5. 不提交生产数据、真实密钥、个人联系方式和环境专属配置。

写作规范、模板和 CI 约束见 [documentation-system](./documentation-system/README.md)。

## 常用命令

```bash
cd imboy
make compile
IMBOYENV=local make run
make eunit
make dialyze
```

完整开发与部署步骤以仓库根目录 [README](https://github.com/imboy-pub/imboy/blob/main/README.md) 和 [deploy/README](https://github.com/imboy-pub/imboy/blob/main/deploy/README.md) 为准。
