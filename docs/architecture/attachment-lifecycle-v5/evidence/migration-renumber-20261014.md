# 迁移编号重排记录 / Migration Renumber Record (2026-10-14)

## 原因

V5.5 计划冻结时（2026-10-13 基线采样），下一可用迁移号为 177（M1-expand）与
178（M2-queue）。2026-10-14 集成回 main 时，main 已由其他会话引入
`00000177_enterprise_directory_sync`，与 `00000177_attachment_lifecycle_v5_expand`
撞号（git 文件名不同不会报冲突，但数据库升级编号必须唯一）。main 未占用
178/179，故本仓两个 V5.5 迁移整体后移一位。

## 映射

| 旧编号（计划文档/evidence 回执中的写法） | 新编号（当前文件） | 语义 |
|---|---|---|
| 00000177_attachment_lifecycle_v5_expand | 00000178_attachment_lifecycle_v5_expand | M1-expand（三表 V5 列扩容） |
| 00000178_attachment_lifecycle_v5_queue | 00000179_attachment_lifecycle_v5_queue | M2-queue（队列协议函数 + M3 触发器函数定义） |

## 范围与不变量

- 仅改文件名与 down 文件头注释中的编号；SQL 正文零改动（内容哈希除注释行外不变）。
- 计划冻结文档（01~06）与历史 evidence 回执保持原编号写法，不改写历史事实；
  阅读时按上表映射。
- gate/probe/strict-ledger 脚本均按 `priv/migrations/*.up.sql` 目录通配扫描，
  无硬编码编号，重排后无需修改。
- 已在真实 PostgreSQL（erlang_migrate strict 模式空库全链重放）验证重排后
  版本集合完整，无缺号、无重复。
