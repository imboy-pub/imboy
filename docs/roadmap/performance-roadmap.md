# IMBoy 性能演进路线（Performance Roadmap）

> 基于 `docs/archive/review/performance-review.md` · 渐进升级,不破坏兼容 · 日期 2026-07-22
> 核心判断：瓶颈全在"共享单进程"而非算法；根治手段是把误用 depcache/单 gen_server 的热路径换成并发 ETS 表(write_concurrency) / 进程内 State——仓内 `agent_rate_limiter` 已是正确范本。

---

## PERF-01 · user_server 单进程卸载 【P0→P2 分期】
- **目标**：把全站上线/下线从单 gen_server 串行中拆出——离线检查改阈值查询（不拉 3×5000 行）、DB 写与好友 fanout 异步化。
- **原因**：`user_server.erl:94-127,218-223` 单进程串行处理全站上下线,含 DB 写 + 3×5000 行离线读 + 好友全量 fanout,重连风暴必积压（评审 P0-2）。
- **收益**：消除节点级上下线吞吐天花板;重连风暴不再积压。
- **风险**：中；上下线是在线态核心。缓解:分两期——Q3 先做"离线检查改阈值查询"（低风险独立收益）,Q2 再做 fanout 异步化。
- **影响范围**：`src/logic/user_server.erl`、`src/ds/message_ds.erl:358-362`、`include/chat.hrl:14`。
- **修改模块**：user_server、message_ds。
- **工作量**：L（分两期）。**PR 数**：3–4。
- **验收**：上线路径不再同步拉 5000 行；模拟 1 万并发重连,上下线队列不积压（压测）。

## PERF-02 · depcache ACK 定时器改并发 ETS 表(write_concurrency) 【P0】
- **目标**：把每消息每设备的投递定时器 `set`、每次 ACK 的 `set+flush` 从 depcache 单 gen_server 迁出,改并发 ETS 表(write_concurrency) 或存 WS 进程 State。
- **原因**：`message_ds.erl:154`、`websocket_logic.erl:58,71` 把 ACK 定时器（根本不需要依赖失效语义）压在 depcache 唯一进程上,横穿消息热路径（评审 P0-3）。
- **收益**：消除消息热路径最硬的单点串行；ACK 吞吐随核数扩展。
- **风险**：低；ACK 定时器语义简单,范本现成（`agent_rate_limiter` 并发 ETS 表(write_concurrency)）。
- **影响范围**：`message_ds`、`websocket_logic`、depcache 用法。
- **工作量**：M。**PR 数**：2。
- **验收**：ACK 路径零 depcache call；并发 ETS 表(write_concurrency) 并发压测无锁竞争热点。

## PERF-03 · C2G 扇出异步化 【P2】
- **目标**：把 C2G 在发送者 WS 进程内的同步 O(N) 扇出（撤回 O(成员×设备)）改为异步/分片投递。
- **原因**：`msg_c2g_logic.erl:386-392,611-626` 大群发送阻塞发送者进程。
- **收益**：大群发送延迟与群规模解耦;发送者进程不被扇出拖死。
- **风险**：中；改群投递路径。缓解:保持投递顺序语义,加投递确认测试。
- **影响范围**：`msg_c2g_logic`。
- **工作量**：M。**PR 数**：2–3。
- **验收**：千人群发送,发送者进程响应时间与群规模无关（压测）。

## PERF-04 · 投递管道去 JSON 中间格式 【P2】
- **目标**：消除 protobuf/v2 客户端每消息每设备 decode+re-encode。
- **原因**：`websocket_handler.erl:858-893` JSON 中间格式导致重复编解码。
- **收益**：降低每消息 CPU;高扇出场景显著。
- **风险**：中；与 ARCH-02 回执对齐协同设计。缓解:先做 ARCH-02,再评估管道格式。
- **影响范围**：`websocket_handler`、`imboy_codec`。
- **工作量**：M。**PR 数**：2。
- **验收**：v2 客户端投递路径无冗余 re-encode（profile 验证）。

## PERF-05 · 连接池真超时 + 语句缓存 【P2】
- **目标**：把 `elib_pg` 池耗尽的 `sleep(1s/2s/3s)` 盲重试改为真超时;加 prepared statement 缓存。
- **原因**：`elib_pg.erl:64,92-95,197-199,237` `?DEFAULT_TIMEOUT=1000` 实为重试延迟非超时,慢查询占死 80 连接池并被重试放大（评审 P1-D2,性能+数据库双命中）;逐次 parse 无语句缓存。
- **收益**：慢查询风暴不再放大成秒级卡顿;减少 parse 开销。
- **风险**：中；改连接池核心行为。缓解:灰度,保留旧行为 flag。
- **影响范围**：`src/lib/elib_pg.erl`。
- **工作量**：M。**PR 数**：2–3。
- **验收**：注入慢查询,池不被 sleep 阻塞;prepared statement 命中率可观测。

## PERF-06 · statement_timeout 全链路 【P2】
- **目标**：连接级设 `statement_timeout`,给所有查询兜底上界。
- **原因**：评审 P1-D2,全链路 grep 零命中。
- **收益**：任何慢查询有界,防拖垮连接池。
- **风险**：低；需为已知长查询设豁免。
- **影响范围**：`elib_pg` 连接初始化。
- **工作量**：S。**PR 数**：1。
- **验收**：连接建立即带 statement_timeout；超时查询被 PG 主动终止而非占死。

## PERF-07 · msg_store_worker 扩容 【P2】
- **目标**：把单 worker（批 100/1s）持久化改为多 worker 分片,加死信剔除。
- **原因**：`msg_store_worker.erl:44-46` 单 worker 是持久化天花板;无死信剔除有无效阻塞记录风险（评审 P1-PF3、P2-6）。
- **收益**：持久化吞吐随分片扩展;无效阻塞记录不阻塞整批。
- **风险**：中；改持久化管道。缓解:保持 SKIP LOCKED 语义。
- **影响范围**：`msg_store_worker`、`msg_store_sup`。
- **工作量**：M。**PR 数**：2。
- **验收**：持久化吞吐随 worker 数线性提升;单条无效阻塞记录不阻塞其余。

## PERF-08 · Admin view_url 批量端点 【P3】
- **目标**：媒体密集首屏的 N 次 presign 往返改批量端点。
- **原因**：评审指 Admin/Storage 唯一值得排期项。
- **收益**：媒体列表首屏往返 N→1。
- **风险**：低。
- **影响范围**：`adm_attach_handler`、admin 前端。
- **工作量**：S。**PR 数**：1–2。
- **验收**：媒体列表首屏 presign 请求数与条目数解耦。

---

## 兼容性说明

所有性能任务均为内部实现替换,**不改协议、不改 schema 语义、不改客户端可见行为**。PERF-01/02/05 走 flag 灰度,可即时回退。压测基线在改造前采集,改造后对比。

## 汇总表

| 编号 | 任务 | 优先级 | 工作量 | PR | 关键证据 |
|---|---|---|---|---|---|
| PERF-01 | user_server 卸载 | P0/P2 | L | 3–4 | user_server.erl:94-127 |
| PERF-02 | depcache→并发ETS(write_concurrency) | P0 | M | 2 | message_ds.erl:154 |
| PERF-03 | C2G 扇出异步 | P2 | M | 2–3 | msg_c2g_logic.erl:386 |
| PERF-04 | 投递去JSON中间格式 | P2 | M | 2 | websocket_handler.erl:858 |
| PERF-05 | 池真超时+语句缓存 | P2 | M | 2–3 | elib_pg.erl:64 |
| PERF-06 | statement_timeout | P2 | S | 1 | 全链路零命中 |
| PERF-07 | msg_store_worker 扩容 | P2 | M | 2 | msg_store_worker.erl:44 |
| PERF-08 | view_url 批量 | P3 | S | 1–2 | adm_attach_handler |
