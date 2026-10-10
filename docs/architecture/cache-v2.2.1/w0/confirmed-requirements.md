# V2.2.1 W0 confirmed 需求结论

结论：confirmed=false。不增加 put_confirmed，不以分布式缓存 ACK 模拟业务原子操作。

| 真实路径 | 权威与要求 | 是否要求所有缓存副本先应用 | 结论 |
|---|---|---|---|
| qr_login_handler status/confirm + QR SSE | 当前 depcache 会话；status 是 get 后 delete，SSE 也能下推登录 token | ACK 无法约束两个请求重复消费 | 独立 G-QR 原子迁移，禁止 replicate |
| account_ds:get_wait → ID list set | PG sequence 申请批次，热命中列表仍读改写 | ACK 无法保证列表出队原子性 | 原子分配先修；保留 local，不加 confirmed |
| auth_session_ds / adm_session_ds / user_device_ds | PG epoch、撤销、设备状态；安全行为须核验权威 | 收到缓存 ACK 不代表 PG 已 COMMIT，也不能证明未分区 | invalidate + 权威检查/拒绝，不加 confirmed |
| 群成员/group_e2ee_mode/好友/频道/配置 | PG 或本地配置可重建；存在事务内缓存改动 | 普通缓存一致性和失效栅栏问题 | COMMIT 后失效与栅栏，不加 confirmed |
| login_attempt/IP 限流、bind_mail、管理员 Csrf | 节点本地计数或一次性状态；不可假定原子 | ACK 不修复读改写或重复消费 | 独立安全审查，不复制全值 |
| cluster_node_info | 运行时状态，当前固定 TTL 300 | 未找到远端缓存应用后才能继续业务的调用合同 | local；复制只保留非敏感字段候选 |
| APNs JWT、FCM、千帆 access token | 本地/第三方凭据，可重新取得 | 无 all-replica 成功后继续业务的需求 | local，禁止全值复制 |
| enterprise_asset_logic 的 file.confirmed | PG 资产转正与 audit/webhook 同事务；领域事件名 | 不是缓存确认协议 | 不把领域 confirmed 当作缓存 confirmed 需求 |

核验范围是本地源码，而不是向所有产品负责人询问后的需求证明。
src/lib/imboy_cache.erl、imboy_cache_sync.erl 没有 confirmed/put_confirmed 协议或调用。
不能因为“搜索没找到”宣布未来永远不需要；本次没有满足第 2.4 节启用条件的真实路径。
若后续出现明确的远端应用前置要求，须另行记录调用者、成员集、成功条件、超时、分区、
重试与幂等语义，再重新评审。W0 的测试 ACK 仅用来测量远端 depcache 写入完成延迟，
不是生产功能实现，也不是 confirmed 需求成立的证据。

2026-10-10 收尾轮复核（候选 closeout-34858496）：QR 两类键除 get→delete 非原子外，
还会经 legacy imboy_cache:set 全节点广播（含 SessionToken 映射），进一步支持
G-QR 独立 gate 结论；auth_session bump/1 COMMIT 后失效正确而 bump_in_tx/2 事务内
失效，属 W1-03 修复对象，不改变 confirmed=false。
