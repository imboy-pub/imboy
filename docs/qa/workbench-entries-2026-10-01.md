# 企业工作台配置发现 / Organization workbench discovery

日期：2026-10-01。基线：`b6d1faf2`。

新增 `GET /api/v1/workbench/entries?organization_id=<ID>`，使用现有 Human JWT 认证路由，复用 OA handler / logic 及应用 repo。企业参数必须为十进制正数 int64；省略或非法为 400，未认证为 401。明确限定当前企业，非成员、离场、企业归档、无应用统一返回空 entries，不枚举其他企业。

查询仅包含有效 Human、企业有效成员、有效企业、有效应用、非空 redirect allowlist 与有效外部身份映射。按 application_id 排序，上限 20；仅输出 kind、organization_id、application_id、application_key、label、redirect_uri，不输出 credentials / scopes / principal 或 secret。label 最多 64 Unicode 字符。复用既有表，无迁移。

实际验证：源模块和测试编译；独立 PostgreSQL 18 合成数据库运行 `workbench_entries_pg_tests:run(SocketPath)`，1 项场景通过，0 跳过。场景覆盖同一成员两企业同 key、跨企业隔离、无成员 / 未知企业、disabled 应用、无映射、空 redirect、离场、企业归档、用户停用、列表上限、最小响应字段。测试库只监听任务 Unix socket，进程结束已停止。日志 `/tmp/gz-oa-entries-pg.log`。

这是真实 SQL 与逻辑投影验证，使用最小合成 schema，不是全量迁移、HTTP JWT 联调或真实 OA 验收。App 配置拉取和标准构建入口、企业切换恢复、同源会话隔离仍待接入。本接口取代旧工作台计划的全企业返回后取第一项假设；需要当前企业参数，响应明确包含企业与应用 ID。
