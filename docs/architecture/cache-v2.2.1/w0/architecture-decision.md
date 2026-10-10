# V2.2.1 W0 架构决策：方案 A

保留 depcache + syn，未来只增加版本/墓碑/失效栅栏/受管 Key 索引等轻量元数据。
不使用 Redis、Mnesia，不新增缓存中间件，不重写 depcache 的 TTL、依赖失效、memo/get_wait。

选择理由：

- 当前依赖已在真实 BEAM 中测量；GET 与 memo 热命中可以并发，SET 走 gen_server:call 串行提交。
- 三个真实 OTP 节点的 syn 逐条广播已测到所有接收者 depcache:set 完成 ACK，结果在两轮原始 JSON 中。
- 32 并发 SET 的排队是已观察事实，但没有实际业务峰值/SLO 证明它构成业务瓶颈。
- 没有“A 合理优化仍不能满足业务 SLO”的证据，也没有特定生产高频 SET Key 的必要性证据；B 的触发条件不成立。
- 当前 ACK/timer 已在 ack_retry_cache 自有 ETS 中，不应把已存在的专项优化包装成本轮新增方案 B。
- 冷 memo 合并 loader 的能力已实测，不应另写 single-flight；失效旧回填确实需要外层 generation 设计。

决策边界：A 是当前获准保留的架构，不是已经实现 V2，也不是目标硬件/生产吞吐验收通过。
W0_BLOCKED 不因默认 A 而转为 PASS；W1 必须等 W0 gate 关闭并获得下一轮授权。
confirmed=false，复制白名单为空；cluster_node_info 只保留未来字段投影候选。

2026-10-10 收尾轮更新（候选 closeout-34858496，与历史候选 578d83ac 双绑定）：
扩展自检补齐 depcache 原生行为 oracle（单飞、flush 后旧 loader 回填且等待者收 stale、
loader 异常重抛、premature_exit 重试、手工 get_wait 所有权语义），四轮数据形态一致，
不改变本决策与边界；新增 legacy 广播调查（含 apns_jwt/config5/QR 等敏感键全节点广播）
作为 G-04/W1-05 证据，不构成启用方案 B 的条件。
