# 0002 — 数据库迁移采用 erlang_migrate + 顺序版本号

- 状态：Accepted
- 关联：`priv/migrations/`、独立库 `erlang_migrate`（gitee.com/imboy-pub/erlang_migrate）

## 背景
多环境（本地 / 预发 / 生产）的 PostgreSQL schema 需要可重放、可回滚的演进机制；并发开发会产生版本号乱序，若静默接受会导致环境间 schema 漂移。

## 决定
- 使用自研 `erlang_migrate` 管理迁移，而非手工 SQL 脚本。
- 迁移文件放 `priv/migrations/`，按**顺序版本号**命名并成对提供 up/down：
  `NNNNNNNN_<name>.up.sql` / `NNNNNNNN_<name>.down.sql`（如 `00000001_foundation`）。
- 启用 erlang_migrate（v0.3.1+）的 **strict 乱序检测**：已应用版本之前出现未应用的更低版本即报错，强制开发者显式处理。

## 后果
- ✅ schema 演进可重放、可回滚、乱序可被及早发现。
- ✅ 迁移版本与 `schema_migrations` 表一一对应，环境间可比对。
- ⚠️ 旧环境首次切到本机制前，须先 `UPDATE schema_migrations` 把历史版本号映射到新序号（迁移系统重构时涉及 9 条记录），否则 strict 检测会拒绝启动。
