# 客服坐席事务连接验收（2026-10-01）

基线：`7b25391b0b9c66f75c071688bcd8e0f9bdfa177a`。本地修复通过，完整客服投产验收仍未完成。

## 根因与修复

`cs_pg_store` 已开启事务，但 `cs_pg_seat:create_seat_limit_tx/6` 和 `set_enabled_limit_tx/5` 忽略其 Conn，再申请第二个池连接并独立提交。外层回滚不能撤销该提交，小连接池还可能被嵌套申请耗尽。两个基础函数改为使用调用方连接；业务错误沿原 rollback 协议交给外层处理。组织限额的 advisory transaction lock、SQL 租户条件和返回结构保留。

当前生产调用方只有 cs_pg_store 的两个事务包装函数；已检查全部调用位置。没有增加新端口或依赖。

## 实际验证

- 新建独立 PostgreSQL 18 实例，仅 Unix socket、合成用户 departure_test；测试结束已正常停机。运行信息 `/tmp/gz-seat-tx-runtime.json`，日志 `/tmp/gz-seat-tx-tests.log`。
- `cs_seat_transaction_pg_tests:run(Socket)`：6/6。真实 SQL、真实事务和真实 advisory lock；连接池边界由 meck 模拟，每个进程只能持有一个测试连接，再次取连接即失败。
- 覆盖创建／停用／恢复在外层失败时回滚，生产 store 包装函数只需一次取连接，限额／跨企业拒绝不改数据，四个并发创建在限额 2 下恰好两个成功。
- 测试只建立坐席和限额两张最小表，不代替完整迁移、身份 FK、审计触发器或 HTTP 验收。
- 现有 `cs_application_tests`：18/18，日志 `/tmp/gz-seat-tx-regression.log`。
- `env -u MANPATH bash scripts/check_feature_architecture.sh` 通过；日志 `/tmp/gz-seat-tx-architecture.log`。
- 曾尝试额外 cs_pg_tests，但其共享数据库 harness 缺 pg_conf，在 setup 阶段失败；本轮不称该完整套件通过。随后单独运行应用回归并取得 exit 0。
- 手工审查全部调用方和错误传播；未运行独立审查代理。

## 复现

编译候选 cs_pg_seat、cs_pg_store 和 cs_seat_transaction_pg_tests，code path 添加既有依赖、主仓 ebin 与候选 beam；向 run/1 传入新建隔离数据库的完整 Unix socket 文件路径：

```erlang
case cs_seat_transaction_pg_tests:run(Socket) of
    ok -> halt(0);
    _ -> halt(1)
end.
```

## 剩余工作

坐席应用管理的列表／详情／开通／调整／停用接口仍需追加合同、显式 Grant、幂等及审计。现有 cs_seat_app 的创建和启停事件尚在操作完成后单独写入，必须继续合并成同一事务；本修复仅让底层正确参与调用方事务，不宣称已完成该原子审计闭环。

English summary: seat create/enable transaction helpers now use their caller's connection. Six isolated PostgreSQL checks, eighteen application tests and the feature architecture gate passed. Full audit atomicity, application management APIs and production readiness remain open.

后续状态更新：上述“审计尚未合并”是本报告基线时的历史状态，现已由 [坐席审计原子性验收](./customer-service-seat-audit-atomicity-2026-10-01.md) 补齐；应用管理接口及完整投产验收仍未完成。
