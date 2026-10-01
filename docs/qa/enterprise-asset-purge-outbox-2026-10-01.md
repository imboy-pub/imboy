# 企业留存清理的事务性删除任务

状态：局部本地通过；六项目标整体仍为 PARTIAL。没有启用自动调度或运行生产迁移。

旧 purge 先在事务外删除对象，随后重新裁决元数据。真实竞态已复现：并发确认成功后，元数据 active，Garage 对象 not_found。现已删除该预删流程，消息附件和孤儿附件均在同一数据库事务里筛选并锁定候选、记录对象删除任务、删除元数据及追加审计。只有提交成功后才执行实际对象删除。审计错误现在显式抛出回滚，防止“返回失败但元数据已提交”。

迁移 163 增加 enterprise_asset_delete_queue，保存企业、工作区、附件 ID 和内部对象 key，不保存 URL、凭证或用户内容。任务与元数据删除、审计原子提交；对象删除失败、响应不确定或任务确认失败时保留任务，下批重试。删除成功或对象已不存在才移除任务。每次读取和处理任务受 batch_limit 限制，单个消息的附件任务全部在提交时捕获，未处理任务留在数据库。跨租户 key 仍由既有存储前缀校验拒绝。

事务先取得工作区锁，防止新的保留声明跨越资格裁决；消息和孤儿候选保留 FOR UPDATE SKIP LOCKED。工作区内的资格提交串行，但网络调用不持数据库锁。已提交的到期清理不能被后来的保留声明追溯恢复。原有保留期、hold、数据库角色/GUC、子表顺序、租户参数化及逐批审计约束保留。

真实 PostgreSQL + Garage 最终运行 /tmp/imboy-seat-http.41jnNR、/tmp/imboy-asset-garage.DWSbM6 exit 0：

- 清理等待工作区锁期间，原生附件确认成功；清理跳过，真实字节完整。
- 清理提交后、实际对象删除前，原生确认返回 not_found；队列任务已持久存在，不会产生 active 元数据指向缺失对象。待删除任务存在时，同 ID 的重新上传在对象写入前以 cleanup_pending 拒绝；登记先取得工作区 KEY SHARE 再读取队列，阻止旧上传凭证跨越清理提交复用 ID。
- 暂时撤去本测试 Garage 配置，已提交任务保持在队列；恢复配置再清理，真实对象消失，队列归零。
- 注入审计 append 错误，整个事务回滚，附件仍 pending、真实字节完整、队列为空。
- 真实过期消息及其 Garage 附件一起清理，元数据与对象均消失，队列归零。
- 待处理队列阻止迁移回滚，P0001 明确拒绝；任务为空后真实 down/up 成功。回滚守卫先锁表，避免检查与 DROP 之间插入新任务。

同一原生数据库中，以显式对象替身运行原有留存 11 项和孤儿附件 10 项用例全部通过；原存储合同 6 项通过。替身用例不被描述成 Garage 证据。非确定时序只使用 meck passthrough 暂停调度；实际资格更新、提交和对象删除均执行原生实现。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ASSET_GARAGE_PG_CHECK=1 bash scripts/test/enterprise_asset_garage_gate.sh
IMBOY_DEPS_ROOT=/path/to/independent/deps bash scripts/test/customer_service_internal_http_gate.sh
```

证据在 evidence/enterprise-asset-purge-outbox-2026-10-01；run.json 绑定测试源码与变更前 HEAD，sha256.json 绑定归档。仅归档合成日志，不含生成凭证、配置、上传引用或崩溃转储。人工审查共享入口、事务和 SQL 条件，不宣称独立代理审查。

剩余边界：自动调度仍关闭，生产角色对新表的授权需随真实部署验收。未迁移旧内存对象或历史无意图墓碑，也未取得全量冻结候选、真机与生产资格。队列是提交后的回收任务，不是可撤销删除或跨 S3/数据库的分布式事务。
