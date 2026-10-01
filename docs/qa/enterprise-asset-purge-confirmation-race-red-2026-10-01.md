# 定时 purge 与附件确认的真实竞态

状态：历史 FAIL。后续修复与本地验证见 [事务性删除任务](enterprise-asset-purge-outbox-2026-10-01.md)；本页保留旧逻辑失败证据。

当前 eb_pg_purge:run 在事务外选候选并删除对象，随后才进入数据库事务重新裁决。后置 SQL 状态检查只保护元数据，无法恢复已经删除的文件。此前 cleanup_pending 的持久意图修复没有覆盖这条独立路径。

隔离真实 PostgreSQL + Garage 运行 /tmp/imboy-seat-http.XwMP2w exit 1：创建合成过期 pending 文件，在实际 delete_private 调用前用 meck passthrough 暂停调度，原生 confirm_asset 成功将状态推进 active，再放行原实现删除 Garage 对象。purge 返回成功且保留 active 元数据，真实 GET 却返回 not_found。purge-confirmation-race.json 记录 status_after_confirmation=active、object_remains=false，严格字节完整性断言失败。插桩不替换数据库或对象结果；它仅固定并发交错顺序。首次测试误判 confirm_asset 返回形状的运行不是产品缺陷证据。

复现命令：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ASSET_GARAGE_PG_CHECK=1 IMBOY_ASSET_PURGE_RACE_CHECK=1 bash scripts/test/enterprise_asset_garage_gate.sh
```

待修：消息附件和孤儿附件两条回收路径都必须在外部删除前持久裁决，阻止确认、消息绑定和新增保留声明越过裁决；失败及审计回滚应有可恢复状态。不能只将对象删除移进普通数据库事务，因为外部成功后事务仍可能回滚，导致活跃元数据引用缺失对象。必须保留现有租户隔离、批量上限、保留期、hold、审计和权限约束，并取得真实交错验证。

证据在 evidence/enterprise-asset-purge-confirmation-race-red-2026-10-01。本页记录旧源失败；当前检查已随新方案更新并验证。没有启用调度、使用真实用户数据或执行生产操作。run.json 绑定变更前 HEAD 与实际测试源码；归档不包含生成凭证、配置或崩溃转储。
