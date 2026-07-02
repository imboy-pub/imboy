# 消息子系统稳定性盘点（不丢 / 不重 / 有序 / 多端一致）

> 只读代码审计，基于 2026-07-02 代码现状；行号以当日 HEAD 为准。
> 审查范围：msg_c2c/c2g/c2s/s2c_logic、msg_ack_logic、message_router_logic、
> msg_store_worker/sup/ds/repo、msg_archive_*、msg_c2g_timeline_repo、
> domain（message_id_vo/conv_key_vo/conversation_agg/message_policy）、
> msg_burn_logic、msg_read_ds、msg_rate_logic、elib_retry_config。
> 本轮不改代码，改动建议只列方向。

---

## 一、总结论：四项承诺的现状

| 承诺 | 现状 | 一句话定性 |
|---|---|---|
| 不丢 | ❌ 不成立 | 多端 ACK 按 uid 删离线行、ACK 与落库竞态，存在两条**真实永久丢消息路径** |
| 不重 | ⚠️ 半成立 | 落库层靠 DB 唯一约束闭环；但投递层无幂等短路，客户端重发会**重复推送**，去重被推给客户端 UI |
| 有序 | ❌ 生产不成立 | conv_seq 权威序代码已实现但 `msg_archive_enabled` **全环境默认关闭**，生产只有 created_at/TSID 近似序 |
| 多端一致 | ❌ 不成立 | ACK 不分设备、已读不广播给自己其他设备，B 设备状态与 A 设备系统性脱节 |

**心智模型校正**：文档写的"2/5/7/11s 共 4 次重试转离线"与代码不符。实际是
`src/lib/elib_retry_config.erl:22` 的 `[0, 3000]`（C2C 仅 2 次，C2G 仅 1 次 `[0]`），
且**不存在"转离线"这个动作**——消息发送前就同步落 staging 表，`msg_c2c`
正式表本身就是离线队列（行存在=未确认，ACK=删行）。

**三套互不联动的"序"**：

| 序 | 生成时机 | 生成方 | 用途 |
|---|---|---|---|
| `msg_id` | 请求到达 WS handler 时 | 客户端（`websocket_handler.erl:482` 取自 JSON `id`） | 幂等键、ACK 关联，非服务端可信序 |
| `msg_c2c.id` / `msg_c2g.id`（TSID） | worker 异步写正式表时 | 服务端 `elib_tsid:generate` | 离线/补拉排序键（`ORDER BY id ASC`） |
| `conv_seq` | 写完正式表后异步归档时 | `msg_archive_repo:next_conv_seq/1`（DB 原子自增） | 永久存储排序键，**默认全环境关闭** |

`domain/message_id_vo.erl`、`conversation_agg.erl` 为 DDD 试点示例，全仓无生产调用方，与实际排序机制无关。

---

## 二、根因点（一处改、全局受益，按优先级）

### 根因 A：ACK 语义混杂——`msg_ack_logic:client_ack/4` 一个入口干了两件不该由它干的事

`src/logic/msg_ack_logic.erl:20-40` 同时执行：
①删 `msg_c2c` 离线行（按 `(msg_id, to_id)`，**不分设备 DID**）；
②无条件 `msg_store_ds:unstage(MsgId)` 清 staging。
由此派生 P0-1、P0-2。改法方向一处收口：ACK 只标记"该设备已收到"，
staging 的 processed 状态只允许 worker `do_write` 成功后置位；
参照 C2G timeline 的 per-uid 标记模式（`msg_c2g_timeline_repo:client_ack`
不删消息本体，是全系统最安全的设计，可作统一范本）。

### 根因 B：`msg_store_ds:stage/10` 把"首次写入"与"重复写入"折叠成同一个 `ok`

`src/ds/msg_store_ds.erl:152-162` 对 `unique_violation` 降级返回 `ok`，
调用方 `msg_c2c_logic.erl:245-320`、`msg_c2g_logic.erl:266-341` 无法区分，
客户端重发会再走一遍完整的实时投递 + 离线推送。
改为返回 `{ok, new} | {ok, duplicate}`，调用方在 duplicate 时跳过投递管道——
一个函数签名的改动同时修复 C2C/C2G 两条重复推送路径。

### 根因 C：撤回路径把"撤回命令自身 MsgId"误当"OriginalMsgId"用（两处同源错误）

C2C（`msg_c2c_logic.erl:411-422` → `msg_c2c_ds.erl:190-219`）和
C2G（`msg_c2g_logic.erl:521-531` → `msg_c2g_ds.erl:146-176`）传参错位一致，
导致对原消息的清理/重置全是空操作。

---

## 三、P0（消息丢失 / 撤回失效，必修）

### P0-1 多端 ACK 误删：任一设备 ACK 即删全局离线行，离线的另一台设备永久收不到

- 【现象】`ack_c2c_msg`/`ack_s2c_msg` 按 `(msg_id, uid)` 删除，无 DID 维度。
- 【触发条件】用户双端登录，设备 A 在线 ACK，设备 B 离线；B 重连拉离线消息时行已被删。
- 【涉及】`src/ds/msg_operation_ds.erl:81-86, 105-110`；`src/repo/msg_c2c_repo.erl:243-246`；`src/repo/msg_s2c_repo.erl:146-150`
- 【方向】引入按设备的送达状态（根因 A），或删除前确认所有活跃设备已收。

### P0-2 ACK 与 worker 落库竞态：快速 ACK 提前 unstage，消息永不落正式表

- 【现象】`client_ack` 无条件 `unstage`（`msg_ack_logic.erl:38`）把 staging 行标 processed；
  worker 处于 `draining` 时忽略 kick（`msg_store_worker.erl:93-121`），
  `claim_pending` 的 `processed_at IS NULL` 跳过该行（`msg_store_repo.erl:147-179`），全程无报错。
- 【触发条件】接收方在线且 ACK 往返快于 worker 抢占该 staging 行（尤其 worker 忙于上一批次时）。
- 【后果】C2C 叠加 P0-1 让离线设备丢消息；**C2G 更严重**——第一个在线成员 ACK 快过 worker，
  整条群消息不落 `msg_c2g`，其他所有离线成员全部丢消息；archive 开启时该消息也永不归档。
- 【涉及】`src/logic/msg_ack_logic.erl:21-40`；`src/ds/msg_store_ds.erl:231-233, 313-319`
- 【方向】见根因 A——ACK 路径与 staging 生命周期解耦。

### P0-3 C2G 撤回是空操作：传参错位使原消息 payload 更新与 client_ack 重置全部落空

- 【现象】`revoke_offline_msg/9` 的两次 UPDATE 条件用的是撤回命令刚插入的新行 MsgId。
- 【触发条件】任意群消息撤回，尤其接收方已 ACK 过原消息（本意要求重新确认）。
- 【涉及】`src/logic/msg_c2g_logic.erl:521-531`；`src/ds/msg_c2g_ds.erl:146-176`
- 【方向】调用点改传 `OriginalMsgId`（根因 C）。

### P0-4 C2C 撤回不清理离线队列原消息：离线接收方上线仍收到完整原文

- 【现象】撤回只追加一条独立 MsgId 的 revoke 通知，原始行原样保留直到被 ACK。
- 【触发条件】撤回发生时接收方离线，或原消息仍在投递窗口内未 ACK。
- 【涉及】`src/logic/msg_c2c_logic.erl:411-422`；`src/ds/msg_c2c_ds.erl:190-219`；`src/logic/msg_ack_logic.erl:26`
- 【方向】撤回时对 `OriginalMsgId` 离线行原地更新或删除（根因 C）。

---

## 四、P1（一致性 / 重复 / 功能失效，应修）

1. **重复投递无服务端短路**：客户端超时重发 → 接收端收到两次 WS 推送 + 两次离线推送。
   见根因 B。触发：重发同 msg_id+created_at 命中 unique_violation 分支。

2. **`msg_c2g_timeline_repo:list_by_uid/3` 无 ORDER BY 就 LIMIT**：
   待确认消息堆积超 Limit（默认 1000）时选中错误子集，两次拉取子集可能不同、成员间顺序可能相反。
   `msg_c2g_timeline_repo.erl:28-38`、`msg_c2g_ds.erl:253-272`。
   对照：同模块带 LastMsgAt 分支（`msg_c2g_ds.erl:273-297`）和 C2C 侧（`msg_c2c_repo.erl:67-80`）都是先排序后截断。
   修法明确：补 `ORDER BY created_at ASC`。

3. **撤回不取消原消息的重试定时器**：3s 重试窗口内撤回，原文仍会被再投递一次。
   `message_ds.erl:74-158`（定时器缓存于 `imboy_cache` 键 `{ToUid, DID, MsgId}`，无外部取消入口）、
   `msg_c2c_logic.erl:331-444`。方向：撤回时 cancel_timer 或投递前置"是否已撤回"校验。

4. **"秒撤"竞态失败**：撤回校验查正式表 `find_msg_by_id`（`msg_c2c_logic.erl:344-345`），
   消息还在 staging 异步管道内时被误判 `msg_not_found`。方向：兜底查 staging。

5. **复合 async_retry 闭包整体重放**：入队+落库+实时投递+expire_at+推送包在同一个
   `elib_async:async_retry`（`msg_c2c_logic.erl:254-317`；`elib_retry.erl:33-77` 每次重试从头执行整个 Fun），
   后置步骤瞬时失败会把已成功的实时投递整体重放，造成重复推送。方向：拆分重试边界。

6. **已读状态不同步到阅读者自己的其他设备**：`handle_read_receipt`（`msg_c2c_logic.erl:607-675`）
   只通知消息作者，不广播给 `CurrentUid` 其他设备，B 设备未读数永久脱节。

7. **`msg_rate_logic` 是零调用死代码**：`check_and_record/1`（`msg_rate_logic.erl:24,59-91`）全仓无调用方；
   真实限流走 WS 层 `throttle:check(msg_per_user, ...)`（`websocket_handler.erl:161,196,277`）；
   管理后台"解除禁言"（`adm_admin_handler.erl:576,600`）操作的是永远为空的表。删除或接线，二选一。

8. **归档序与实时序无协调 + 多节点 conv_seq 错位窗口**（archive 开启后生效）：
   conv_seq 在异步归档阶段才分配（`msg_store_worker.erl:158-202`），
   多节点各跑一份 worker（`imboy_sup.erl:87-92`），`FOR UPDATE SKIP LOCKED` 并发抢占
   （`msg_store_repo.erl:147-161`）可造成 TSID 序与 conv_seq 序相反（`msg_archive_repo.erl:74-87`）。
   开启归档前必须先解决（按 conv_key 路由单一归档点），或在协议层明确 conv_seq 语义。

9. **阅后即焚 vs 归档冲突**（archive 开启后生效）：归档发生在 expire_at 设置之前，
   且 `msg_burn_logic` 清理只删 `msg_c2c`/`msg_c2g`（`msg_burn_logic.erl:179-186`），
   不触碰 `msg_store` 归档表——"焚毁"内容永久留存。
   同时焚毁计时以创建时间而非已读起算（`msg_burn_logic.erl:62-74`），多设备无"已烧毁"墓碑通知，产品语义需明确。

10. **文档与实现不符**：CLAUDE.md 的"2/5/7/11s 四次重试转离线"应按
    `elib_retry_config.erl:20-27` 现状重写，并写明"离线不是转存动作而是存储常态"。

11. **生产环境无服务端权威序（决策项）**：`msg_archive_enabled` 默认 false
    （`msg_store_worker.erl:186-202`）且仓内 config 无任何开启，"有序"当前完全依赖
    客户端时间戳与 TSID 近似序。要么开启归档（先修 P1-8/9、P2-3），要么对外不承诺严格顺序。

---

## 五、P2（边缘 / 观测 / 债务）

1. ACK 重复到达时 `msg_delivered_total` 指标虚高（`msg_ack_logic.erl:34-35`）——仅在影响行数>0 时自增。
2. 撤回/已读旁路 `write_msg` 绕开 staging 幂等层，重试可产生重复行
   （`msg_c2c_ds.erl:192-195, 252-265`；正式表 `(msg_id, created_at)` 约束因 NowTs 不同挡不住）。
3. 归档失败"烧号"产生 conv_seq 永久空洞且不重试（`msg_archive_repo.erl:104-129`
   next_conv_seq 在 insert 之前；`msg_store_worker.erl:163-179` 失败仅记日志），
   若客户端有按 seq 补拉缺口逻辑会误报丢消息。
4. `msg_s2c_logic` 两处丢弃 `stage/10` 返回值，DB 失败静默继续投递
   （`msg_s2c_logic.erl:46-57, 266-277`），与 c2c/c2g/c2s 错误处理不一致。
5. 离线消息转正式表前的瞬时重连窗口：接收方在 staging→正式表间隙重连，
   `check_and_notify_offline_msgs` 只查正式表，本次重连暂时看不到该消息
   （自愈于下次重连；`message_ds.erl:95-98` Filtered=[] 时重试链终止）。可补查 staging。
6. `msg_read` 唯一约束含客户端时间戳 `created_at`（`msg_read_repo.erl:32-51`），
   两次独立上报时间戳不同即产生重复已读行。改为 `(msg_id, to_uid, to_did)` 去重。
7. 单一全局 `msg_store_worker` 串行写全平台消息（`msg_store_sup.erl:41-60` 只启一个 worker）——
   当前是"简单+天然保序"的合理取舍，量级上来前不动；扩容时需按 conv_key 分片且注意保序。

---

## 六、正向确认（无需改动）

- **worker 崩溃重放有完整兜底**：staging 租约 30s + 指数退避重试（1s→60s 上限，
  `msg_store_worker.erl:158-179, 270-282`）+ 正式表 `ON CONFLICT DO NOTHING`
  （`msg_c2c_repo.erl:109-126`），重放不产生脏数据。
- **重试定时器挂在接收方 WS 进程上**（`message_ds.erl:140-156`），连接死则定时器随之消失——
  因消息已持久化，属自洽的 best-effort 设计（主动重推尽力 + 离线拉取强兜底），补注释即可。
- **C2G timeline per-uid 标记模型**（不删消息本体，`msg_c2g_timeline_repo:client_ack`）
  是全系统最安全的 ACK 设计，是修 P0-1 的现成参照。
- 同一 msg_id 重复 ACK 本身幂等（DELETE/UPDATE 0 行不报错）。

---

## 七、未核实事项

- 运行时 `IMBOY_*` 环境变量是否在生产覆盖 `msg_archive_enabled`（仅核了仓内静态配置）。
- Flutter 客户端是否有"按 seq 检测缺口补拉"逻辑（影响 P2-3 实际严重度）。
- C2G timeline ACK 未按 DID 区分是否造成多端未读数串扰（消息本体不删，影响限于未读计数）。
- 跨节点 syn 广播一致性语义。
- `msg_c2g_timeline` 表 DDL 是否有可用于排序的 id 列（P1-2 修复方案选型）。

---

## 八、建议修复顺序

1. **根因 A**（一并解决 P0-1/P0-2，ACK 语义重构，参照 C2G timeline 模式）
2. **根因 C**（P0-3/P0-4，行级传参修复，代价极小）
3. **根因 B**（P1-1，stage 返回值区分 new/duplicate）
4. **P1-2**（一行 ORDER BY）

前四项修完，"不丢、不重、多端一致"三项即可基本闭环；
"有序"取决于是否开启归档的产品决策（开启前先修 P1-8/9、P2-3）。
