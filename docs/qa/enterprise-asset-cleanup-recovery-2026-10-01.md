# 企业附件清理故障恢复与并发保护

状态：局部本地验收通过；六项整体目标仍为 PARTIAL，未取得生产资格。

旧流程先把元数据改为 deleted，再删除对象。存储失败后没有持久重试依据，恢复配置仍跳过，真实 Garage 对象残留。修复给过期、未确认且未绑定消息的附件增加 pending_object_delete 意图；在事务内检查状态、TTL、未来保留期和有效 hold，提交逻辑删除后才进行对象删除。失败或响应不确定保留意图，重试成功或对象已不存在才清除。既有 deleted 默认无意图，不自动回收或迁移。

事务短暂锁定所属工作区，与保留声明 INSERT 的现有外键锁互斥；资产状态更新采用条件更新，确认与清理由数据库行锁裁决。网络调用在事务完成后发生。真实 PostgreSQL 等待观测确认：清理等待期间另一事务确认附件，清理跳过且 Garage 字节完整；另一事务新增工作区 hold，清理跳过；释放 hold 后清理成功。未来 retain_until 及无意图历史墓碑的对象仍完整。

真实隔离运行 /tmp/imboy-seat-http.fV7R8j、/tmp/imboy-asset-garage.VnvSyp exit 0。缺失存储配置后元数据为 deleted 且意图为 true；恢复配置再调用同一清理入口，实际对象消失且意图为 false。此前红检查要求保持 pending_confirm；现在以持久意图和真实重试删除为准，没有削弱“失败后可恢复”的验收条件。原六个显式替身合同用例及三个端口合同检查也实际通过。

真实客服浏览器回归 /tmp/imboy-seat-http.zAceEz exit 0：访客及两个坐席实际页面完成扫码、并发领取、双向附件收发、历史下载、断网恢复、停用拒绝、重新登录、结束与评分。新增字段为内部状态，不进入公开附件响应。浏览器复用先前归档的构建，本轮未修改 Admin 产品。

迁移 162 新增内部字段、状态约束和待清理索引。存在待清理意图时 down 明确以 P0001 拒绝；完成清理后，在独立数据库实际执行 down/up 成功。没有执行生产迁移。

默认 HTTP 回归 /tmp/imboy-seat-http.0cjnvr exit 0，26 个顶层检查通过，并验证四种身份响应 schema。此计数不是全部后端 EUnit，也不是完整 Internal API 上线资格。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ASSET_GARAGE_PG_CHECK=1 bash scripts/test/enterprise_asset_garage_gate.sh
IMBOY_DEPS_ROOT=/path/to/independent/deps bash scripts/test/customer_service_internal_http_gate.sh
```

证据在 evidence/enterprise-asset-cleanup-recovery-2026-10-01，run.json 记录变更前 HEAD 与逐文件源码哈希，sha256.json 绑定归档。采用已有 Garage v2.3.0；没有使用真实用户数据、凭证或通知第三方。人工检查共享入口和 SQL 条件，不宣称独立代理审查。

剩余边界：未启用自动清理调度；已有 purge 的事务外预删仍须单独修复。没有自动回收旧无意图墓碑。真机、全量候选冻结门禁和生产验收仍未完成。
