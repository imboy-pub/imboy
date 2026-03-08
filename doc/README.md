# Imboy 文档中心（精简版）

> Last Updated: 2026-03-08  
> Scope: 长期文档索引、阶段性文档入口与归档规则  
> Related docs: `README.md`, `doc/changelog.md`

## 目标

只保留长期有效、可执行、可交付复用的文档；把当前仍有价值但带阶段属性的材料单独放在“阶段性文档”；把不应继续沉淀在通用仓库的内容明确标记为“建议归档 / 删除”。

## 三层索引规则

### A. 长期文档

适合长期保留，具备以下至少一个特征：

- 是产品、架构、接口、部署、安全的稳定基线；
- 会被多次交付、多次联调、多次复用；
- 当前仍是对外口径或内部执行的事实依据。

### B. 阶段性文档

适合暂时保留，但不进入长期主索引，通常具备以下特征：

- 明显服务于某一阶段、某一季度、某一专项推进；
- 阶段结束后价值快速下降；
- 更适合在阶段完成后归档，而不是长期挂在核心入口上。

### C. 建议归档 / 删除

不应继续沉淀在通用产品仓库，通常具备以下特征：

- 一次性计划、评审、AI 产物；
- 历史宣传材料、旧压测截图、已失效外链；
- 绑定具体域名、IP、证书路径、面板路径的环境样例；
- 与现行架构重复、冲突或已经失去事实基础的旧文档。

## A. 长期文档索引

### 1. 架构与规范

- `architecture/overview.md`
- `architecture/database-access.md`
- `standards/api-format.md`
- `standards/error-codes.md`
- `standards/hashid-encoding.md`
- `standards/utf8-encoding.md`
- `standards/migration_naming.md`

### 2. 产品与商业化

- `guides/product-target-boundary.md`
- `guides/three-end-phase1-function-matrix.md`
- `guides/phase2-modularization-strategy.md`
- `guides/product-edition-comparison.md`
- `guides/customer-acceptance-checklist.md`
- `guides/quotation-template.md`
- `guides/module-feature-flag-config-draft.md`

### 3. API 与三端契约

- `api/rest-api.md`
- `api/websocket-api-2.md`
- `api/channel_api_contract_v1.md`
- `api/moment_api_contract_v1.md`
- `api/e2ee_server_persisted_shard_contract_v1.md`
- `api/envelope.schema.json`

### 4. 运行、部署与安全

- `guides/deployment.md`
- `operations/dependencies.md`
- `operations/security.md`

### 5. 组件与排障参考

以下文档允许长期保留，但默认不作为客户交付主文档：

- `libraries/async.md`

### 6. 变更记录

- `changelog.md`

## B. 阶段性文档索引

当前仍可保留、但不纳入长期主索引的文档：

- `operations/three-end-delivery-plan-2026Q2.md`

处理规则：

1. 阶段结束后优先归档，而不是继续挂在长期主索引；
2. 如其中结论已成为稳定事实，应回写到长期文档，而不是长期依赖阶段计划本身；
3. 阶段性推进清单优先留在 issue、PR、项目管理工具，仓库内只保留必要版本。

## C. 建议归档 / 删除的内容

### 1. 一次性计划、评审、AI 产物

- `doc/plans/*`
- `doc/planning/*`
- `doc/ai/*`
- `doc/analysis/*`

### 2. 环境绑定部署样例

- 绑定具体域名、机器 IP、证书路径、面板路径的 `nginx` / 对象存储 / 文件存储配置
- 这类文件应保存在运维仓、交付项目目录或部署平台，不放在通用产品仓中

### 3. 历史宣传类材料

- 旧压测截图、旧环境基准记录、已失效外链文档

### 4. 阶段结束后的执行计划

- 已完成、已失效、已被长期文档吸收结论的季度计划、专项推进表、执行日报/周报

## 文档维护规则

1. 新增文档前先判断：能否并入现有核心文档章节。
2. 接口变更优先更新 `api/` 现有文档，不新建“临时实现说明”。
3. 阶段性推进计划默认不进入长期主索引。
4. 发布前优先更新受影响核心文档与 `changelog.md`。
5. 出现环境绑定样例时，默认放到交付环境，不回写到通用仓库。
6. 当阶段性文档中的结论已稳定，应回写到长期文档并移出阶段索引。

## 相关链接

- 根说明：`../README.md`
- 研发上下文：`../CLAUDE.md`
