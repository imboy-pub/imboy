# Imboy 文档索引

> Last Updated: 2026-03-13  
> Scope: 当前仓库文档索引与维护规则  
> Related docs: `README.md`, `doc/changelog.md`

## 目标

只保留当前仍有效、可执行、可复用的文档入口；避免在这里继续挂已经删除、已过期或仅服务某个阶段的材料。

## 当前长期文档

### 1. 架构与规范

- `architecture/overview.md`
- `architecture/module_map.md`
- `architecture/database-access.md`
- `architecture/module-layer-cheatsheet.md`
- `architecture/current-module-classification.md`
- `architecture/product-profile-and-plugin-registry-design.md`
- `standards/api-format.md`
- `standards/error-codes.md`
- `standards/hashid-encoding.md`
- `standards/utf8-encoding.md`
- `standards/migration_naming.md`

### 2. 功能开关与部署指引

- `guides/module-feature-flag-config.md`
- `guides/deployment.md`

### 3. API 与三端契约

- `api/rest-api.md`
- `api/websocket-api-2.md`
- `api/channel_api_contract_v1.md`
- `api/moment_api_contract_v1.md`
- `api/e2ee_server_persisted_shard_contract_v1.md`
- `api/envelope.schema.json`

### 4. 运行与安全

- `operations/dependencies.md`
- `operations/security.md`

### 5. 组件与排障参考

以下文档允许长期保留，但默认不作为客户交付主文档：

- `libraries/async.md`

### 6. 变更记录

- `changelog.md`

## 不再建议沉淀的内容

- 一次性计划、评审、AI 产物；
- 已失效的外链、截图、环境专属样例；
- 绑定具体域名、IP、证书路径、面板路径的部署模板；
- 已被核心文档吸收结论的阶段性说明。

## 文档维护规则

1. 新增文档前先判断：能否并入现有核心文档章节。
2. 接口变更优先更新 `api/` 现有文档，不新建“临时实现说明”。
3. 阶段性推进计划默认不进入本文档主索引。
4. 发布前优先更新受影响核心文档与 `changelog.md`。
5. 出现环境绑定样例时，默认放到交付环境，不回写到通用仓库。
6. 当阶段性材料中的结论已稳定，应回写到核心文档，而不是继续保留原始过程文档。

## 相关链接

- 根说明：`../README.md`
- 研发上下文：`../CLAUDE.md`
- 模块地图：`architecture/module_map.md`
