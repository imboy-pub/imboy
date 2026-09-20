# IMBoy 后端文档

> 本目录是 `imboy` 后端仓库的技术文档真源。面向开发、部署、集成、审计和维护；营销介绍请看 [imboy.pub](https://imboy.pub/)，轻量部署入口请看 [Wiki](https://github.com/imboy-pub/imboy/wiki)。

## 按目标开始

| 目标 | 入口 |
|---|---|
| 理解项目全貌（三端怎么组成） | [核心业务概念](./concepts/) · [三端 API 对齐](https://github.com/imboy-pub/imboy/blob/main/docs/api-contracts/three-platform-alignment.md) |
| 查一个术语的准确含义 | [术语表](./glossary.md) |
| 本地跑通后端 | [后端快速上手](./tutorials/quickstart-backend.md) |
| 生产部署 | [部署 README](https://github.com/imboy-pub/imboy/blob/main/deploy/README.md) → [Day-1 部署](./guides/operations/deployment/day1-quickstart.md) |
| 备份、恢复、升级 | [备份恢复](./guides/operations/deployment/backup-restore.md) · [升级手册](./guides/operations/upgrade-runbook.md) |
| 对接 REST / WebSocket | [API 格式](./reference/api-format.md) → [REST API 目录](./reference/rest-api-v1-catalog.md) → [WebSocket 协议](./reference/ws-protocol-contract.md) |
| 理解后端架构 | [架构总览](./architecture/overview.md) → [模块地图](./architecture/module-map.md) → [分层速查](./architecture/module-layer-cheatsheet.md) |
| 审核 E2EE / 合规 | [E2EE 概念](./concepts/e2ee.md) → [E2EE 协议](./reference/e2ee-protocol-specification.md) · [E2EE 政策](./compliance/e2ee-policy.md) · [等保清单](./compliance/mlps2-checklist.md) |
| 当前正在做什么 | [路线图](https://github.com/imboy-pub/imboy/blob/main/ROADMAP.md) · [CHANGELOG](https://github.com/imboy-pub/imboy/blob/main/CHANGELOG.md) |
| 查看历史结论 | [已归档](https://github.com/imboy-pub/imboy/blob/main/docs/archive/README.md) |

## 文档分类

| 分类 | 回答的问题 | 目录 |
|---|---|---|
| 概念 | 核心业务实体是什么、边界在哪？ | [concepts](./concepts/) |
| 教程 | 我怎样从零做出一个可运行结果？ | [tutorials](./tutorials/) |
| 操作指南 | 我怎样完成部署、备份、测试或发布？ | [guides](./guides/) |
| 参考 | 参数、接口、协议和错误码是什么？ | [reference](./reference/) · [术语表](./glossary.md) |
| 契约 | 某个域的 API 契约与三端对齐？ | [api-contracts](https://github.com/imboy-pub/imboy/tree/main/docs/api-contracts) |
| 解释 | 为什么采用这种架构或安全设计？ | [explanation](./explanation/) · [architecture](./architecture/overview.md) |
| 业务与合规 | 产品边界、商业、安全披露是什么？ | [business](./business/service-offering.md) · [compliance](./compliance/e2ee-policy.md) · [legal](https://github.com/imboy-pub/imboy/tree/main/docs/legal) |
| 决策与过程 | 方案、审计和阶段性结论是什么？ | [adr](https://github.com/imboy-pub/imboy/blob/main/docs/adr/README.md) · [archive](https://github.com/imboy-pub/imboy/blob/main/docs/archive/README.md) |

判断规则：教技能是「教程」，办事情是「指南」，查事实是「参考」，讲原理是「解释」。一次性计划和已完成审计不进入稳定入口，完成后放入 `archive/`。

### 目录口径备注

- 操作手册主阵地是 `guides/operations/`；`docs/runbooks/` 收运行手册；`docs/ops/support-matrix.md` 为支持矩阵（被 `scripts/check_release_consistency.sh` 引用，位置固定）。
- `adr/` 收编号 ADR；E2EE v2 体系（`guides/e2ee/v2/`）内部的编号 ADR 属于该体系，不并入。

## 内容状态标注（四态）

重要文档必须在头部或相关小节明确状态，**禁止混写**：

| 状态 | 含义 |
|---|---|
| CURRENT | 代码已实现且文档描述与代码一致 |
| TARGET | 已确定但尚未完全实现的目标设计 |
| PLAN | 实施计划（完成后转为执行记录并标注结论） |
| HISTORICAL | 历史记录（保留决策价值，标 `Superseded by:` 指向继任文档） |

计划文档执行完毕后：状态头必须更新为实际结局（不允许「READY_NOT_EXECUTED」残留于已执行计划）；被取代的计划加 `SUPERSEDED` 横幅。

## 真源与发布关系

- **代码事实**：以 `src/`、`api/openapi.yaml`、`api/asyncapi.yaml`、`deploy/` 和可执行测试为准。
- **后端文档真源**：本目录 `imboy/docs/`；[GitHub Pages](https://imboy-pub.github.io/imboy/) 由 CI 构建发布，不在站点副本上直接改文档。
- **客户端文档**：见相邻仓库 [`imboyapp/docs`](https://github.com/imboy-pub/imboy-flutter/tree/main/docs)；管理后台、SDK 和插件分别维护自己的 README/文档。三端共享的术语与 API 对齐以本仓 [术语表](./glossary.md) 与 [三端 API 对齐](https://github.com/imboy-pub/imboy/blob/main/docs/api-contracts/three-platform-alignment.md) 为准。
- **Wiki**：只保留用户和运维最常用的短入口；详细协议、内部架构、审计证据不在 Wiki 复制。
- **产品官网**：只负责定位、能力和商业信息，不承担 API 或部署契约。

## 更新规则

1. API 或 WebSocket 变更，先更新机器可读契约，再更新对应参考文档和客户端说明。
2. 部署命令、环境变量或端口变更，同时更新 `deploy/` 与操作指南，并验证命令可执行。
3. 安全、合规和产品能力只写已实现或明确标注状态的事实；不要把规划写成现状。
4. 新文档先判断能否并入现有页面；阶段性产物完成后移入 `archive/`，不要继续挂在主入口。
5. 不提交生产数据、真实密钥、个人联系方式和环境专属配置。
6. 新术语先查[术语表](./glossary.md)：有则复用，无则先在术语表立项再写正文。

写作规范、模板和 CI 约束见 [documentation-system](https://github.com/imboy-pub/imboy/blob/main/docs/documentation-system/README.md)。

## 常用命令

```bash
cd imboy
make compile
IMBOYENV=local make run
make eunit
make dialyze
```

完整开发与部署步骤以仓库根目录 [README](https://github.com/imboy-pub/imboy/blob/main/README.md) 和 [deploy/README](https://github.com/imboy-pub/imboy/blob/main/deploy/README.md) 为准。
