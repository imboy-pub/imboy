# 迁移命名规范

> Last Updated: 2026-06-10  
> Scope: `priv/migrations/` 新增 SQL 迁移文件命名规则  
> Related docs: `docs/architecture/database-access.md`, `docs/operations/dependencies.md`

## 1. 适用范围

适用于 `priv/migrations/` 下所有新增 SQL 迁移文件。

## 2. 文件命名规则

统一格式（成对创建）：

```text
{YYYYMMDDHHMMSS}_description.up.sql
{YYYYMMDDHHMMSS}_description.down.sql
```

约束：

1. 版本号为 **14 位 UTC 时间戳**（`YYYYMMDDHHMMSS`），由生成工具产生，禁止手写或复用。
2. `description` 使用小写 `snake_case`，只允许字母、数字、下划线。
3. `.up.sql` 必须存在；`.down.sql` 强烈建议提供（缺失时该版本不可回滚）。
4. 禁止提交 `.sql.bak`、临时副本、同版本号多文件。

示例：

- 正确：`20260610153000_group_task_index.up.sql` + 同名 `.down.sql`
- 错误：`000067_group_task_index.up.sql`（旧顺序号格式，已废弃）
- 错误：`20260610153000_group-task-index.up.sql`（描述不符合 snake_case）

## 3. 版本号生成（必须用工具）

使用 erlang_migrate（>= 0.3.1）自带生成器，时间戳天然避免多人开发冲突，同秒冲突自动 +1：

```bash
# 方式一：escript（在 erlang_migrate 仓库构建一次即可）
cd ../erlang_migrate && rebar3 escriptize
_build/default/bin/erlang_migrate_cli new group_task_index /path/to/imboy/priv/migrations
```

```erlang
%% 方式二：Erlang shell
erlang_migrate:create("priv/migrations", "group_task_index").
```

## 4. 乱序保护（strict 模式）

`imboy_migrate:migrate/0` 已启用 `strict => true`：

- 已应用迁移记录在 `schema_migrations_history` 表（每版本一行）。
- 若存在"版本号小于等于当前版本、但从未应用"的文件（典型场景：分支上先生成、
  后合并），启动时报 `{error, {out_of_order, Versions}}`，**不会**被静默跳过。
- 处理方式：给迟到文件重新生成新时间戳（推荐）；或手动应用后执行
  `erlang_migrate:force/2` 重建历史。

## 5. 提交前自检

```bash
# 检查重复版本号（输出非空 = 必须修复）
find priv/migrations -maxdepth 1 -type f -name '*.up.sql' -exec basename {} \; \
  | sed -E 's/^([0-9]{14})_.*/\1/' | sort | uniq -d

# 检查命名格式（输出非空 = 必须修复）
find priv/migrations -maxdepth 1 -type f -name '*.sql' -exec basename {} \; \
  | rg -n -v '^[0-9]{14}_[a-z0-9_]+\.(up|down)\.sql$'

# 检查孤儿 down 文件（有 down 无 up）
for f in priv/migrations/*.down.sql; do
  [ -f "${f%.down.sql}.up.sql" ] || echo "orphan: $f"
done
```

## 6. 历史说明

- 2026-06-10：18 个存量文件由 6 位顺序号（`000001`–`000099`）整体改名为时间戳
  格式（`git mv` 保留历史）；旧环境升级前必须先同步 `schema_migrations` 表的
  版本号映射（见项目记忆 / 部署手册）。
- 旧的 8 位单文件格式（`NNNNNNNN_description.sql`）与 6 位 up/down 格式均已废弃，
  不作为任何新增迁移的模板。
