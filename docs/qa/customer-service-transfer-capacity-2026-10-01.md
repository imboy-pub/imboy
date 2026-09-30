# 客服转接容量与事务边界 / Seat transfer capacity and transaction boundary

日期：2026-10-01。后端源码基线 cdce845cdd9c79491a16b52ded709f50ae21bffb；本地局部验证通过，客服完整可投产证明仍未完成。

## 修复 / Changes

转接原先仅检查目标坐席存在，未在事务内锁定目标并检查 enabled 与 max_concurrent。现在复用接单的目标行锁与容量检查，接单和转接增加同一坐席的会话数量时在同一锁上串行。停用、满员和不存在/跨组织目标在状态推进前拒绝。会话 CAS、审计与受让人读游标继续同事务提交。

转接未读边界原先使用 query/2 另借连接，现在改用 query/3 的当前事务连接。没有增加表、迁移、消息副本或新配置。

English summary: Claims and transfers share the target-seat lock and capacity check. Transfer assignment, audit and unread boundary use one transaction and one database checkout.

## 验证 / Checks

- 修复前六项真实 PostgreSQL 测试中五项失败，复现停用/满员转接、并发超额、额外借连接及缺失目标错误。
- 修复后七项 PostgreSQL 18 测试通过（exit=0）：停用、满员、双转接竞争、接单与转接竞争、单事务借连接及读边界、审计失败回滚、失效版本/跨组织目标。
- cs_session_tests 与 cs_application_tests 共 33 项通过（exit=0）；受影响编译、格式、diff 和纵切架构检查通过。
- 全部共享调用方及现有错误映射人工复核；未使用独立审查代理。

复跑 PostgreSQL 测试需新建空隔离数据库，用户 departure_test、postgres 库、Unix socket 文件；只替换连接池、配置和 ID 生成，真实执行 elib_pg、cs_pg_session、cs_pg_seat 的事务/SQL。禁止在共享或业务数据库运行这个建表夹具。

```erlang
cs_transfer_capacity_pg_tests:run("/absolute/path/to/socket/.s.PGSQL.PORT").
eunit:test([cs_session_tests,cs_application_tests],[verbose]).
```

编译使用 +debug_info、-DTEST、-DEUNIT、-I include 和当前依赖路径；测试 beam 独立输出，未覆盖常驻服务。7 项夹具采用普通 PG 表，未代表全部迁移约束或真实 HTTP/Seat JWT 链；尚缺完整浏览器接待/附件/断流旅程。原有 fake store 编译警告未纳入本修复。

## 源码绑定 / Source hashes

| File | SHA256 |
|---|---|
| src/features/customer_service/infrastructure/cs_pg_session.erl | `03e22e7b3d4af9096da7b415b6da76c1cde6a287fdb268ae571b19ba410d1658` |
| test/features/customer_service/infrastructure/cs_transfer_capacity_pg_tests.erl | `55b1cf7b41a7e3c0611f2f4860125f01f59a663a27c3b833021bb9d60de0a31f` |
