# 迁移命名规范

> Last Updated: 2026-06-11  
> Scope: `priv/migrations/` 新增 SQL 迁移文件命名规则  
> Related docs: `docs/architecture/database-access.md`, `docs/operations/dependencies.md`

## 1. 适用范围

适用于 `priv/migrations/` 下所有新增 SQL 迁移文件。

## 2. 文件命名规则

统一格式（成对创建）：

```text
{NNNNNNNN}_description.up.sql
{NNNNNNNN}_description.down.sql
```

约束：

1. 版本号为 **8 位零填充顺序整数**（`00000001`–`99999999`），取当前最大编号 +1，禁止跳号或复用。
2. `description` 使用小写 `snake_case`，只允许字母、数字、下划线。
3. `.up.sql` 必须存在；`.down.sql` 强烈建议提供（缺失时该版本不可回滚）。
4. 禁止提交 `.sql.bak`、临时副本、同版本号多文件。

示例：

- 正确：`00000010_group_task_index.up.sql` + 同名 `.down.sql`
- 错误：`20260610153000_group_task_index.up.sql`（时间戳格式，已废弃）
- 错误：`00000010_group-task-index.up.sql`（描述不符合 snake_case）

## 3. 版本号分配规则

新增迁移前，先确认当前最大编号：

```bash
ls priv/migrations/*.up.sql | sed -E 's|.*/([0-9]+)_.*|\1|' | sort -n | tail -1
```

取该值 +1 作为新文件的版本号。**多人并行开发时，若两人都从同一基础分支创建迁移，会产生相同编号——git 合并时文件名冲突，强制在合并阶段解决，不会遗漏到部署阶段。**

## 4. 乱序保护（strict 模式）

`imboy_migrate:migrate/0` 已启用 `strict => true`：

- 已应用迁移记录在 `schema_migrations_history` 表（每版本一行）。
- 若存在"版本号小于等于当前版本、但从未应用"的文件，启动时报
  `{error, {out_of_order, Versions}}`，**不会**被静默跳过。
- 处理方式：给迟到文件重命名为当前最大编号 +1（推荐）；或手动应用后执行
  `erlang_migrate:force/2` 重建历史。

## 5. 提交前自检

```bash
# 检查重复版本号（输出非空 = 必须修复）
ls priv/migrations/*.up.sql | sed -E 's|.*/([0-9]+)_.*|\1|' | sort | uniq -d

# 检查命名格式（输出非空 = 必须修复）
find priv/migrations -maxdepth 1 -type f -name '*.sql' -exec basename {} \; \
  | rg -v '^[0-9]{8}_[a-z0-9_]+\.(up|down)\.sql$'

# 检查孤儿 down 文件（有 down 无 up）
for f in priv/migrations/*.down.sql; do
  [ -f "${f%.down.sql}.up.sql" ] || echo "orphan: $f"
done
```

## 6. 历史说明

- 2026-06-11：18 个存量文件由时间戳格式（`20230101000000`–`20230409000000`）整体改回
  8 位顺序编号（`00000001`–`00000009`）。
- 2026-06-10：曾将顺序号改为 14 位时间戳格式，经实践评估后回退。
- 旧的时间戳格式（`YYYYMMDDHHMMSS_description.sql`）已废弃，不作为任何新增迁移的模板。
