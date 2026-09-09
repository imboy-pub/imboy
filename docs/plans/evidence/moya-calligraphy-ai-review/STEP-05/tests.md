# STEP-05 证据 — 约束行为测试

脚本：`/tmp/moya_mig/step5_behavior_test.sql`（BEGIN...ROLLBACK 包裹，不留测试数据）
执行：`psql -d moya_mig_test -v ON_ERROR_STOP=1 -f step5_behavior_test.sql` → EXIT=0

| # | 验收项 | 用例 | 期望 | 结果 |
|---|---|---|---|---|
| T1 | DB-ORG-02 | 1 个 organization(951000) 下插入 2 个 workspace(952001/952002) 挂同一 organization_id | 成功 | PASS |
| T2 | DB-ORG-03 | 插入 organization_id IS NULL 的历史形态 workspace(952003) | 成功（expand-first 不回填不强制） | PASS |
| T3 | DB-ORG-03 | `SELECT count(*) FROM workspace WHERE organization_id IS NULL` ≥ 1，旧行可读 | 成功 | PASS |
| T4 | DB-ORG-02 | workspace.organization_id=999999（不存在机构） | foreign_key_violation 拒绝 | PASS |
| T5 | DB-ORG-02 | `DELETE FROM organization` 仍挂 workspace 的机构 | restrict_violation(SQLSTATE 23001) 拒绝 | PASS |
| T6 | DB-ORG-02 | organization.owner_id=888888（不存在用户） | foreign_key_violation 拒绝 | PASS |
| T7 | — | organization.status='frozen' | check_violation 拒绝（仅 active/archived） | PASS |
| T8 | DB-ORG-02 | workspace.organization_id 为单列可空 bigint（结构断言：一 Workspace 至多一机构，由单列 FK 模型保证） | 断言成立 | PASS |

## 踩坑记录

- PG 的 `ON DELETE RESTRICT` 违反 SQLSTATE 是 **23001 (restrict_violation)**，不是 23503 (foreign_key_violation)。PL/pgSQL 测试块需 `EXCEPTION WHEN restrict_violation`。已验证（NOTICE: T5 caught sqlstate=23001）。
- `psql --single-transaction` 与脚本自带 `BEGIN` 叠加会产生 WARNING 且行为异常，行为测试改为脚本自带事务执行。

## 验收结论

- DB-ORG-01 PASS：空库（含全链 93 个历史迁移）→ 95 up → down → up 全通过；历史迁移校验和不变。
- DB-ORG-02 PASS：一机构多 Workspace（T1）；Workspace 单归属（T8 单列 FK）；孤儿 FK / 违规删除均被拒（T4/T5/T6）。
- DB-ORG-03 PASS：organization_id 可空、不回填（T2）；旧 Workspace 读取不受影响（T3）；本迁移仅新增可空列，未修改任何现有查询语义。
