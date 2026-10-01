# 工作空间归档共用事务核

范围：用户端及 Admin 归档复用 workspace_ds:archive_tx/4；SQL 收入 workspace_repo:archive_tx/4。删除两份 Logic 内归档 SQL 和吞错的 organization_id_of_tx/2。新增 Internal 写端点尚未接线，当前仍为 36 条路由。

归档 UPDATE 使用 RETURNING organization_id，在同一条语句、同一 Conn 内取得实际企业归属。错误不再转为个人域 undefined；替代默认项校验失败抛 abort_tx，整个归档和默认交接回滚。NULL 企业归属仍为合法个人空间。Human actor 保留 Uid；Admin archived_by 固定 NULL，Admin handler 审计保留，未伪装 Human 或增加自动 Grant。

## 验证

- 76/76 专项 EUnit：模板、默认关系、用户归档、治理权限、Admin 归档与恢复、归档写守卫。旧 SQL mocks 改为 UPDATE RETURNING，断言继续真实执行。
- 8/8 真 PostgreSQL / Cowboy 门禁；全量真实迁移，当前全部产品源码重新编译，36 个 Internal 路由覆盖仍通过。
- 真库归档：默认工作空间未指定替代项拒绝；跨企业替代项拒绝；两次拒绝均保持当前默认项。注入 synthetic CHECK 数据库拒绝，工作空间和默认项仍不变。
- Admin ID 不在 user 表也可经已授权的管理入口完成归档，archived_by 为 NULL；合法同企业默认替代项交接成功；重复归档 409；群和频道及 owner 成员仍存在。
- erlfmt 定向检查、git diff --check 通过。独立容器已自动清理，原有数据库不变。

证据：[EUnit](evidence/workspace-archive-core-2026-10-01/unit.txt)、[真库 / HTTP](evidence/workspace-archive-core-2026-10-01/http.txt)、[源码绑定](evidence/workspace-archive-core-2026-10-01/sha256.json)。基线 f2bf91a1。

人工复查所有原归档函数调用、事务返回、SQL 参数化、默认交接失败路径、Human/Admin actor 与授权入口。没有执行子代理审查。该核只供已授权事务调用；新增应用接口还需要独立 workspaces:write Grant、业务校验、幂等和同事务 Application 审计，不能直接暴露此核。

English summary: Human and Admin workspace archive operations now share one transaction core. UPDATE RETURNING provides authoritative Organization ownership and removes the former error-to-personal fallback. Default handover failures and database rejection roll back atomically; resources remain. Seventy-six focused tests and eight real database/HTTP tests pass. New Internal write routes and the complete six-part production objective remain unfinished.
