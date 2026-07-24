# 架构决策记录 / Architecture Decision Records (ADR)

记录 imboy 后端重要架构决策的**背景、决定与后果**，便于后人理解"为什么这样做"。

- 命名：`NNNN-kebab-title.md`，序号递增、永不复用。
- 状态：`Proposed` → `Accepted` → 可被后续 ADR `Superseded`。
- 一项决策一篇，已接受的 ADR 不删改（要变更则新开一篇引用它）。

延伸阅读：`docs/architecture/`（架构全貌）、`docs/CONVENTIONS.md`（编码约定）。

## 索引
| # | 标题 | 状态 |
|---|------|------|
| [0001](./0001-four-layer-architecture.md) | 后端四层单向依赖架构 | Accepted |
| [0002](./0002-database-migrations.md) | 数据库迁移采用 erlang_migrate + 顺序版本号 | Accepted |
| [0003](./0003-plugin-route-namespace-api-prefix.md) | 动态插件路由命名空间对齐为 `/api/v{n}/` 前缀 | Accepted |
| [0004](./0004-tsid-origin-namespace.md) | TSID 之上预留 origin 命名空间位（不改现有数据） | Proposed |
