# 客服坐席与审计原子性验收（2026-10-01）

基线：`86dc38e9dfdfcb1b9d3bdf11ff8b601ff26d83f0`。本轮修复坐席创建、停用、恢复的审计半态；完整客服投产目标仍在进行。

## 实现

- cs_seat_app 在操作前构造既有事件投影，传入 store；移除操作后独立 append_event 的三条路径和无用辅助函数。
- cs_pg_store 增加携带事件的 create_seat_limit_checked/6、set_enabled_checked/5，复用既有 cs_pg_seat:insert_event_in/3，在同一连接、同一事务内完成坐席变更和事件写入。
- 审计失败抛出 rollback，外层归一为既有 `{error, {audit_append_failed, Reason}}`；不提交坐席或版本号变更。
- 原基础 arity 保留给现有基础设施调用；正式 application 路径全部使用带事件版本。两种新 callback 同步登记 cs_store_port 与 cs_ports；内存 fake 只模拟审计成功，失败回滚由真实数据库验证。
- 不新增数据库表、依赖或 HTTP 端点；事件字段、动作名和对外错误形状保持原契约。

## 证据

1. 实际候选源码编译至 `/tmp/gz-seat-audit-beams`，只读复用主仓依赖，不修改主仓构建产物。编译日志 `/tmp/gz-seat-audit-compile.log`。
2. 新建隔离 PostgreSQL 18 实例，仅 Unix socket、合成用户与数据；测试结束已停机。运行信息 `/tmp/gz-seat-audit-runtime.json`。
3. `cs_seat_transaction_pg_tests:run(Socket)`：10/10，通过日志 `/tmp/gz-seat-audit-tests.log`。保留上一轮六项连接／回滚／限额／隔离测试；新增创建、停用、恢复三个审计失败回滚 oracle 和正常生命周期事件投影 oracle。
4. 故障来自真实 event 表 CHECK 约束拒绝写入，不 mock SQL 或事务。创建失败后同参数重试成功且仅一个 seat.created 事件；启停失败后 enabled 和 version 保持原值。成功创建／恢复／停用各写一条事件，组织、工作区、业务身份和 actor 投影正确。
5. 连接池入口由 meck 限制单进程一次持有一条实际数据库连接；TSID 来源采用合成单调 ID。最小数据库表不代替完整身份 FK、迁移、事件触发器或 REST 验收。
6. cs_application_tests 18 项、cs_closure_tests 8 项，共 26/26；日志 `/tmp/gz-seat-audit-regression.log`。端口 callback 与冻结注册表完全一致，feature 引用边界通过。
7. `env -u MANPATH bash scripts/check_feature_architecture.sh` 通过；日志 `/tmp/gz-seat-audit-architecture.log`。
8. 手工检查全部 create/enable 调用方、错误传播和事件作用域；未运行独立审查代理。

## 复现

以候选源码编译 cs_store_port、cs_ports、cs_pg_seat、cs_pg_store、cs_seat_app 和测试模块，添加依赖与候选 beam 到 code path。向 run/1 传入新建隔离数据库的完整 Unix socket 文件路径：

```erlang
case cs_seat_transaction_pg_tests:run(Socket) of
    ok -> halt(0);
    _ -> halt(1)
end.
```

## 未完成范围

应用集成的 Seat 列表／详情／开通／调整／停用接口、显式 Grant、幂等和整体旅程仍须补齐；本轮不把事务修复等同全部投产完成，也未部署。

English summary: seat creation, suspension and resumption now commit their audit events in the same transaction. Ten focused PostgreSQL tests, twenty-six application/port tests and the architecture gate passed. Application management APIs and complete production acceptance remain open.
