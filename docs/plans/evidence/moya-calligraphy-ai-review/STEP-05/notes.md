# STEP-05 备注

## 产物

- `priv/migrations/00000095_organization_foundation.up.sql`
- `priv/migrations/00000095_organization_foundation.down.sql`

## 关键设计决策

1. **id 类型**：organization.id 用 `bigint`（TSID），与 workspace.id 一致；owner_id FK 镜像 00000076 的 `fk_workspace_owner`（ON DELETE CASCADE）。
2. **workspace.organization_id ON DELETE RESTRICT**：fail-closed——物理删除机构前必须显式解除全部 Workspace 归属；机构生命周期主路径是 status=archived 软删除，不触发物理 DELETE。（RESTRICT 违反 SQLSTATE=23001，应用层如需捕获注意区分。）
3. **expand-first 严格执行**：只加可空列 + 索引，不回填、不按 owner_id 自动合并、不改历史迁移（94 个历史文件 shasum 前后一致）。
4. **约束命名**：沿用最新迁移（00000094）的 `pk_/ck_/fk_` 前缀风格 + 00000076 的 `chk_` 语义：`pk_organization`、`ck_organization_status`、`fk_organization_owner`、`fk_workspace_organization`。
5. **幂等**：CREATE TABLE IF NOT EXISTS / CREATE INDEX IF NOT EXISTS / DROP CONSTRAINT IF EXISTS + ADD CONSTRAINT，重复执行安全（已实测）。

## 留给 Step 8/9 的接口

- `organization(id, name, owner_id, status, branding, settings, created_at, updated_at)`
- `workspace.organization_id`（可空 bigint FK→organization.id，RESTRICT）
- 新建 Organization 时事务内创建默认 Workspace 的业务规则由应用层实现（计划 §6.1 迁移策略 3），不在本迁移内。
- "新增墨芽路径拒绝无 Organization 上下文" 由 Step 8 ACL 层实现（DB 层无法表达"查询语义"，DB-ORG-03 以兼容性证明覆盖）。

## 风险

- 若未来 organization 收紧为必须（NOT NULL），需另立迁移 + 数据审计（计划 §6.1 策略 4），本迁移不做。
