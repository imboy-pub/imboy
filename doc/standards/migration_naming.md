# 迁移命名规范

> Last Updated: 2026-03-08  
> Scope: `priv/migrations/` 新增 SQL 迁移文件命名规则  
> Related docs: `doc/architecture/database-access.md`, `doc/operations/dependencies.md`

## 1. 适用范围

适用于 `priv/migrations/` 下所有新增 SQL 迁移文件。

## 2. 文件命名规则

统一格式：

```text
NNNNNNNN_description.sql
```

约束：

1. `NNNNNNNN` 为 8 位数字前缀，代表迁移顺序号。
2. `description` 使用小写 `snake_case`，只允许字母、数字、下划线。
3. 每个前缀只能出现一次，禁止复用。
4. 禁止提交 `.sql.bak`、临时副本、同前缀多文件。

示例：

- 正确：`00000061_group_task_index.sql`
- 错误：`00000061_group_task_index_copy.sql`（前缀重复）
- 错误：`61_group_task_index.sql`（前缀位数错误）
- 错误：`00000061_group-task-index.sql`（描述不符合 snake_case）

## 3. 前缀分配规则

1. 新迁移前缀必须大于当前最大前缀。
2. 一个需求批次内，如需多条迁移，必须按执行顺序连续分配。
3. 已合并前缀禁止重用；修复请申请新前缀追加迁移，不修改历史编号。

查询当前最大前缀：

```bash
find priv/migrations -maxdepth 1 -type f -name '*.sql' -exec basename {} \; \
  | sed -E 's/^([0-9]{8})_.*/\1/' \
  | sort \
  | tail -n 1
```

## 4. 提交前自检

优先执行以下两条本地命令：

```bash
find priv/migrations -maxdepth 1 -type f -name '*.sql' -exec basename {} \; \
  | sed -E 's/^([0-9]{8})_.*/\1/' \
  | sort \
  | uniq -d
```

若输出非空，表示存在重复前缀，必须先修复。

```bash
find priv/migrations -maxdepth 1 -type f -name '*.sql' -exec basename {} \; \
  | rg -n -v '^[0-9]{8}_[a-z0-9_]+\.sql$'
```

若输出非空，表示存在不符合 8 位前缀或 `snake_case` 规则的文件名，必须先修复。

说明：当前仓库仍保留少量历史命名例外（如 `00000042_imboy_v0.4.0.sql`）；该类历史文件不作为新增命名模板，新增迁移仍必须严格遵守本规范。
