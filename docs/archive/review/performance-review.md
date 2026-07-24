# IMBoy 性能评审报告 / Performance Review

> 评审日期：2026-07-22 | 评审方式：Fact-based 只读代码评审（未压测）
> 覆盖仓库：`imboy`（Erlang/OTP 后端）、`imboyapp`（Flutter）、`imboyadmin`（React）
> 所有结论均附 `文件:行号` 证据；影响量级为基于代码结构的工程估计，非实测数据。

---

## 热路径与潜在瓶颈概览

消息主热路径：

```
客户端 WS 帧
  → websocket_handler:websocket_handle (每连接一进程，Cowboy)     src/api/websocket_handler.erl:158
  → throttle:check(msg_per_user)                                  websocket_handler.erl:161
  → message_router_logic:route（同步，在发送者 WS 进程内执行）      websocket_handler.erl:510
  → msg_c2c_logic:c2c / msg_c2g_logic:c2g
      ├─ friend_ds:check_relationship（depcache 缓存）             msg_c2c_logic.erl:55
      ├─ msg_store_ds:stage —— 同步 INSERT staging 表              msg_c2c_logic.erl:196; ds/msg_store_ds.erl:146
      ├─ SERVER_ACK 回发                                          msg_c2c_logic.erl:278
      └─ message_ds:send_next 扇出（syn 查询 + 定时器 + depcache）  ds/message_ds.erl:74-160
  → 接收端 WS 进程 websocket_info → 按协议编码下发                 websocket_handler.erl:541-579
  → CLIENT_ACK → cancel_timer（depcache set/get/flush + syn 广播） logic/websocket_logic.erl:30-80
```

三个**全节点单进程串行点**横穿上述路径，是最主要的可扩展性风险：

| 单点 | 证据 | 波及 |
|------|------|------|
| `user_server`（唯一 gen_server，处理全站上线/下线） | src/logic/user_server.erl:36,94-127 | 每次 WS 连接/断开 |
| depcache 缓存写（唯一 gen_server，set/flush 都是 call） | deps/depcache/src/depcache.erl:454,574 | 每条消息每设备每次重试 + 每次 ACK |
| `msg_store_worker`（唯一 gen_statem，staging→正式表搬运） | src/ds/msg_store_worker.erl:44-46,71 | 所有消息持久化吞吐上限 |

---

## 一、后端并发（OTP 进程模型）

### 1.1 【P0】user_server 单进程串行化全站上线/下线，重连风暴必积压

- **现象**：所有用户的 WS 上线/下线后处理都投递到同一个本地注册 gen_server。
- **机理**：`websocket_init` → `user_logic:online/4` → `user_server:cast_online`（src/api/websocket_handler.erl:117; src/logic/user_logic.erl:127-131）。`user_server` 以 `{local, ?MODULE}` 注册为**单进程**（src/logic/user_server.erl:36）。每个 online cast 内部串行做 4 件重活（user_server.erl:94-127）：
  1. `user_device_ds:update_by_did` —— 1 次 DB UPDATE（:99）
  2. `message_ds:check_and_notify_offline_msgs` —— 3 类离线消息查询，每类最多 5000 行（:102；见 §2.2）
  3. `user_device_logic:device_name` + `send_next` 通知其它设备（:105-118）
  4. `notice_friend` —— 拉全量好友列表并逐个好友 publish 在线状态（user_server.erl:218-223 → `friend_ds:list_by_uid` + `msg_s2c_ds:send` 逐好友循环，ds/msg_s2c_ds.erl:37-68）
- **影响量级**：单次 online 处理含 ≥2 次 DB 往返 + O(好友数) 次 syn publish + 潜在 3×5000 行离线读。假设单次 20-200ms，节点重启/网络抖动导致 1 万连接同时重连时，队列积压可达 **分钟级**，期间离线消息通知、好友在线状态全部延迟；mailbox 无界增长还有内存风险。
- **风险级**：**P0**（自托管 IM 卖点场景——服务端升级重启后的重连风暴——必踩）
- **建议**：
  1. 把 online/offline 处理改为 per-user 旁路进程（`elib_async:async` 或 poolboy 式 worker pool），user_server 只做不可并行的少量状态；
  2. `notice_friend` 只通知**在线**好友（先 `imboy_syn:is_online` 过滤再 publish），并对好友数设上限/分批；
  3. 离线消息检查挪回接收端 WS 进程自身执行（天然 per-connection 并行）。

### 1.2 【P0】depcache 单 gen_server 是消息投递/ACK 热路径的全节点串行点

- **现象**：`imboy_cache:set/flush` 全部落到唯一的 depcache 进程的 `gen_server:call`。
- **机理**：depcache 的 `set/5`、`flush/2` 是同步 call（deps/depcache/src/depcache.erl:454,574；imboy_cache.erl:146-158,229-241 只是薄封装）。热路径上的调用点：
  - 投递侧：每条消息每台设备每个 >0 延迟档 `imboy_cache:set(TimerKey, Ref, TTL)`（ds/message_ds.erl:154）；投递前每设备 1 次 `get`（:106,132，get 是 ETS 直读、不串行——问题集中在写）；
  - ACK 侧：每个 CLIENT_ACK `imboy_cache:set(AckReceivedKey, true, 40)` + `flush(TimerKey)` = **2 次同步 call**（logic/websocket_logic.erl:58,71）；
  - 跨节点 ACK 广播的远端处理同样各 2 次 call（websocket_handler.erl:588-597 → handle_ack_cancel）。
- **影响量级**：每条 C2C 消息全生命周期 ≈ 2-4 次 depcache 同步 call。单 gen_server call 吞吐通常 5-20 万 op/s（含消息拷贝），但所有 WS 进程排队竞争同一 mailbox，在 1 万 msg/s 量级即成为首要串行瓶颈，且 depcache 进程还承担过期清理。
- **风险级**：**P0**（与 1.1 并列，是横向扩容前的节点内吞吐天花板）
- **建议**：ACK 定时器/标志这类"短 TTL + 无依赖"数据不需要 depcache 的依赖失效语义——换成分片 `ets`（`write_concurrency`，如 `agent_rate_limiter` 的做法，src/lib/agent_rate_limiter.erl:84-90）或直接把 timer Ref 存在接收端 WS 进程 State 里（定时器本来就 fire 在该进程，见 websocket_handler.erl:560-575），彻底去掉共享存储。

### 1.3 【P1】C2G 群消息扇出在发送者 WS 进程内同步 O(N) 执行

- **现象**：群消息的成员展开、逐成员在线判断、逐成员投递全部在发送者的 WS 进程内同步完成。
- **机理**：`msg_c2g_logic:do_stage_and_send_c2g`：`OnlineUids = [Uid || Uid <- MemberUids, ..., user_logic:is_online(Uid)]` 逐成员查 syn（src/logic/msg_c2g_logic.erl:386-391），再 `[message_ds:send_next(...) || Uid <- OnlineUids]`（:392），每个 send_next 又含 `imboy_syn:list_by_uid` + 每设备 ACK 缓存 get（ds/message_ds.erl:75,106）。撤回更重：`[cancel_timer(Uid,DID,...) || Uid <- MemberUids, DID <- online_dids(Uid)]` = O(成员×设备) 次「syn 广播 + depcache 双 call」（msg_c2g_logic.erl:611-618）。
- **影响量级**：500 人群单条消息 ≈ 1000+ 次 ETS/syn 查询 + 数百 depcache call，全部串行在一个进程；发送者的下一条消息被队头阻塞，客户端观感是"大群发消息卡"。撤回在 500 人群 ×2 设备 ≈ 2000 次跨进程操作。
- **风险级**：**P1**
- **建议**：`{ok,new}` 且回完 SERVER_ACK 后，把 ③ 之后的扇出整体 `elib_async:async` 旁路（消息已 staging 落库，投递本就是 best-effort + sync 兜底，见 elib_retry_config.erl:21-23 注释）；大群可按成员分片并行。

### 1.4 【P1】msg_store_worker 单 worker 是持久化吞吐上限

- **现象**：staging → 正式表的搬运由唯一 gen_statem 完成，批 100 条、空闲 1s 定时（src/ds/msg_store_worker.erl:44-46）。
- **机理**：`claim_and_process_batch` 用 `FOR UPDATE SKIP LOCKED` 抢批（文件头注释 :14-17），drain 循环在 N≥100 时续批（:107-110）。设计上支持多 worker（SKIP LOCKED），但监督树只起一个。
- **影响量级**：持久化吞吐 = 单 worker 串行批处理速度。假设每批（含 unstage）50-200ms，上限约 500-2000 msg/s；超出后 staging 积压，秒撤兜底查询（msg_c2g_logic.erl:526-528）和 duplicate 判定窗口拉长。
- **风险级**：**P1**（有明确天花板但可预估；SKIP LOCKED 已为多 worker 铺路，扩容成本低）
- **建议**：`msg_store_sup` 下按 CPU 数起 2-4 个 worker 即可线性扩展；给 staging 表深度加 Prometheus 指标做预警。

### 1.5 【P2】ETS/限流器使用总体健康（正面确认）

- `agent_rate_limiter`：`ets:update_counter` 原子自增 + 时间桶 + `write_concurrency`，无 gen_server 中介，无单点（src/lib/agent_rate_limiter.erl:72-75,84-90）。✅
- `imboy_syn`：基于 syn 的 ETS 注册，`publish/do_publish` 用 `erlang:start_timer(0,...)` 直投目标进程，无中心进程（src/lib/imboy_syn.erl:148-173）。✅
- throttle 配置存在：WS 握手 22/min+10/s，`msg_per_user` 60/min（config/sys.config:348-355）——注意 60/min 对活跃打字用户偏紧，属产品参数而非性能缺陷。
- `websocket_handler:init` 的 `Opt0` 里写了 `num_acceptors/max_connections => infinity`（websocket_handler.erl:56-58）——这两个是 ranch listener 级参数，放在 WS opts 中**不生效**，属误导性死配置（无性能危害，但会让人误以为已调优）。

---

## 二、数据库性能

### 2.1 【P1】elib_pg：池耗尽时 sleep 重试阻塞热路径；无 prepared statement 复用

- **现象 A（池行为）**：`pooler:take_member(Driver)` 无等待参数，耗尽立即返回 `error_no_members`，随后 `timer:sleep(Delay)` 重试，Delay 从 1000ms 起每次 +1000（src/lib/elib_pg.erl:92-95；`?DEFAULT_TIMEOUT=1000` 实际语义是**重试初始延迟**而非查询超时，:64,79-86 命名误导）。池上限 `max_count => 80`（config/sys.config:177-178）。
- **机理**：池打满瞬间，所有拿不到连接的调用进程（含 WS 消息路径，因 stage 是同步 DB 写，msg_c2c_logic.erl:196）直接睡 1s/2s/3s——不是排队等连接释放，而是盲睡后再抢。一次慢查询风暴会把延迟阶梯式放大成 1-6s 的用户可见卡顿。
- **现象 B（协议开销）**：`execute/3` 每次 `epgsql:parse` + `execute_batch`（elib_pg.erl:197-199），`query/3` 每次 `equery`（:237）——均为逐次 parse，无 prepared statement 缓存；高频小查询（好友关系、is_member、ACK 相关）每次多一轮 Parse/Describe 往返。
- **影响量级**：现象 A 在池饱和时把 p99 从毫秒级推到秒级；现象 B 恒定增加每查询 ~0.1-0.3ms 与 PG 端 parse CPU，10k qps 时可观。
- **风险级**：**P1**
- **建议**：① 换用 `pooler:take_member(Pool, Timeout)` 的阻塞等待语义（pooler 原生支持），删掉 sleep 重试；② 热点小查询走 statement 缓存（epgsql `prepared_query` + 每连接缓存）；③ `max_count=80` 需与 PG `max_connections` 与节点数联动核对，生产建议配 pgbouncer。

### 2.2 【P1】离线消息路径：一次上线最多拉 3×5000 行 + 每写一条 s2c 先 count 全量

- **现象**：`check_and_notify_offline_msgs` 对 C2C/C2G/S2C 各读 `?SAVE_MSG_LIMIT`=5000 行（ds/message_ds.erl:358-362; include/chat.hrl:14），而"直推 vs pull 通知"的阈值默认仅 10（message_ds.erl:558-560）。
- **机理**：判断"是否超过 10 条"根本不需要拉 5000 行完整 payload——`LIMIT 阈值+1` 即可短路。当前实现最坏情况把 15000 行消息体从 PG 拉进 BEAM 再只数个数丢弃；且该操作运行在 user_server 单进程内（§1.1），放大重连风暴伤害。
- **补充**：`msg_s2c_ds:write_msg` 每写一条先 `count_by_to_id(To)` 全量计数再判断是否清理（ds/msg_s2c_ds.erl:104-115）——写放大 1 次 count 查询/条。
- **风险级**：**P1**
- **建议**：先 `count(*) ... LIMIT Threshold+1`（或 `EXISTS` 短路）判断走向，只有 ≤阈值时才取完整行；s2c 溢出清理改为定时批处理，不放写路径。

### 2.3 【P2】elib_pg:page_with_total 每页必带 count(*)

- **现象**：`page_with_total/6` 与 `page_with_total_safe/7` 每次分页都执行 `pluck(Table, <<"count(*)">>, ...)`（src/lib/elib_pg.erl:485,522）。
- **机理**：admin 端大表（user、msg 审计、operation_log）翻页时 count(*) 在 PG 上是全可见性扫描；timescaledb hypertable 上 count 还要跨 chunk 聚合。
- **影响量级**：百万行级表每次翻页额外几十至数百 ms；管理台并发低，属体验问题非稳定性问题。
- **风险级**：**P2**
- **建议**：admin 列表用 keyset 分页或 `reltuples` 估算 total；至少给 count 结果按 where 条件加短 TTL 缓存。

### 2.4 【P2】timescaledb 与索引面（总体健康，两点注意）

- msg 系列表均已 hypertable 化，chunk 7 天/30 天，`create_default_indexes => FALSE`（priv/migrations/00000005_msg_c2c.up.sql:39; 00000002_message_aux.up.sql:22,159,574; 00000006:41; 00000007:35; 00000008:35），并手工补齐 to_id/from_id/msg_type/created_at 及唯一键索引（00000005_msg_c2c.up.sql:47-110; 00000008_msg_s2c.up.sql:43-71）。✅
- **注意 1**：`i_c2c_msgtype`、`i_s2c_action`、`i_s2c_msgtype` 这类低基数单列索引（00000005:68; 00000008:43,57）写放大大于查询收益，候选删除。
- **注意 2**：msg_c2c 上的 GIN 全文索引 `idx_msg_c2c_payload_fts`（00000005:82）对**每条**消息写入都有 jieba 分词开销；E2EE 密文消息分词毫无意义——建议改为 `WHERE e2ee IS NULL` 的部分索引。
- **N+1 检查**：好友关系判定用单条 LEFT JOIN 联合查询并带 depcache 缓存（ds/friend_ds.erl:389-400 `check_relationship`，TTL 300s :30）✅；群成员列表 `group_ds:member_uids` 走缓存 1 小时（ds/group_ds.erl:71-87）✅；群 e2ee 门走 60s 缓存（group_ds.erl:91 附近，msg_c2g_logic.erl:215 注释）✅。未发现经典"列表页逐行查库"式 N+1。

### 2.5 【P2】billing_usage 单行热点：每条 C2C 消息一次同行 UPDATE + 一个临时进程

- **现象**：`billing_meter:meter` 每条 C2C 发送成功后 spawn 一个进程执行 `billing_usage_ds:incr`（src/logic/msg_c2c_logic.erl:83; src/logic/billing_meter.erl:36,53）。
- **机理**：单租户硬编码 tenant_id=0（billing_meter.erl:28），同一订阅+同一 metric+同一月份 → **所有消息竞争同一 PG 行**的 UPDATE 行锁；虽为异步不阻塞主路径（设计正确，且刻意避开了 `report_usage` 的拒发红线，billing_meter.erl:9-12 ✅），但高吞吐时该行锁串行化会拖慢连接池占用，spawn-per-message 也有进程创建开销。
- **风险级**：**P2**
- **建议**：进程内 ETS 计数 + 每 5-10s 批量 flush 一次 DB（一个 timer 进程即可），DB 写频率与消息量解耦。

---

## 三、WebSocket 热路径

### 3.1 【P1】投递管道以 JSON 为中间格式：protobuf/v2 客户端每条消息 decode+re-encode

- **现象**：投递管道（send_next/timer）传递的是 JSON 预编码 binary；下发给 protobuf 客户端时先 `jsone:decode` 再 `imboy_codec:encode(protobuf, ...)`（src/api/websocket_handler.erl:858-873）；v2 framing 客户端也要先 decode JSON 才能取 type 定帧类型（:875-893，payload 保持 JSON 是为绕过 protobuf-dart bytes→base64 的坑，注释 :877-884 有明确记录）。protobuf 上行同样 decode 后 `jsone:encode` 回 JSON 再进路由（:776-777）。
- **机理**：每条消息在每台接收设备上多一轮完整 JSON parse（+可选 protobuf 序列化）；群消息按设备数放大。
- **影响量级**：单次 jsone decode/encode 对 1KB 消息 ≈ 10-50µs，万级 msg/s × 多设备时 CPU 占比可观，但不是首要瓶颈（排在 §1.1/1.2 之后）。
- **风险级**：**P1**（架构债：协议归一未完成，注释自认"迭代4 将适配 protobuf"，websocket_handler.erl:775）
- **建议**：投递管道改传 `{DecodedMap, EncodedJson}` 二元组或按连接协议懒编码缓存，同一消息对同协议设备只编码一次。

### 3.2 【P2】上行 JSON 消息路径存在双重 decode

- **现象**：v2 帧 payload 先 `jsone:decode` 探测是否 JSON（websocket_handler.erl:337），确认后把**原始 binary** 交给 `handle_json_message` 再完整 decode 一次（:311-312,489）。
- **机理**：注释说明是为复用完整 validate/convert/route 流水线（:331-333），代价是每条 v2 JSON 消息 parse ×2。
- **风险级**：**P2**（有意换取代码复用的取舍，量化后再决定是否值得改）
- **建议**：`try_decode_json_payload` 直接返回已解 Map，`handle_json_message` 增加接受 Map 的入口。

### 3.3 【P2】心跳/重连与空闲连接

- 心跳三层并存：RFC6455 ping（Cowboy 自动 pong，websocket_handler.erl:140-147）、文本 `ping`（:150-153）、v2 HEARTBEAT 帧（:240-243），均 O(1) 无 DB，✅；`idle_timeout` 180s 配 60s 客户端心跳（:63-65）合理。
- 连接级错误在 `websocket_init` 立即回帧关闭，不占连接位到 idle_timeout（:105-111）✅。
- 重连风暴的真正伤害不在握手（有 throttle_ws 10/s per DID/IP，:70-82）而在 §1.1 的 user_server 串行后处理。
- 已确认禁忌遵守：热路径无 `billing_logic:report_usage` 调用（全仓仅 billing_meter 软计量，见 §2.5）✅。

### 3.4 【P3】WS 进程 hibernate 策略

- 所有返回都带 `hibernate`（websocket_handler.erl 全文）。万级空闲连接省内存正确；但对高频活跃连接，每条消息后 hibernate 引发 GC + 栈重建，增加活跃会话 CPU。属权衡项：可按最近活跃度动态决定是否 hibernate。

---

## 四、Flutter（imboyapp）

### 4.1 【P3】会话列表：状态设计良好（正面确认）

- `ConversationState` 缓存排序结果，仅在 map 变更时重排（lib/page/conversation/conversation_provider.dart:54-71）；高频更新有 batch timer 合并（:114-116）。✅
- `conversation_page.dart:183` `ref.watch(conversationProvider)` 监听整个 state——任一会话变更整页 rebuild，但列表 itemBuilder 虚拟化下成本可控；若列表项复杂化，可拆 `select`。
- 聊天消息列表用 `ListView.builder`（lib/page/chat/widget/chat_message_list.dart:56-63）✅。
- **注意**：约 20 处 `shrinkWrap: true`（如 lib/component/chat/message_image_multi_builder.dart、lib/page/group/group_select/group_select_page.dart 等）——shrinkWrap 列表放弃惰性布局，嵌套于滚动容器且数据量大时会整列表实测布局；多数场景数据量小，逐个核查大数据源的即可。

### 4.2 【P2】附件 presign 无批量接口：媒体密集首屏 N 次 view_url 往返

- **现象**：`AssetUrlResolver` 按单个 object_key 调 `GET /api/v1/attachment/view_url`（lib/service/asset_url_resolver.dart:30-33; imboy/src/api/attach_handler.erl:88-98）。
- **机理**：TTL 缓存 540s + in-flight 合并已做（asset_url_resolver.dart:47-50,73-80 ✅），但首次进入含 30 张图的会话/朋友圈 = 30 次串行化 HTTP 往返（每次含服务端 authorize 六分支鉴权，attach_logic.erl:9）；蜂窝网络下首屏图片瀑布明显。
- **风险级**：**P2**
- **建议**：加 `POST /attachment/view_urls` 批量端点（一次鉴权上下文签 N 个 key），客户端按屏幕批量预取。

### 4.3 【P3】SQLite：索引面完备（正面确认）

- 会话/消息核心查询路径均有匹配复合索引：`conversation(user_id, last_time DESC)`、`msg_c2c(conversation_uk3, created_at)`、unread 专用 `(conversation_uk3, is_author, auto_id)` 等（assets/migrations/baseline_schema.sql:175-202）。✅
- sqlcipher 加密库 + 每消息落库，写路径为单条 insert；未见明显批量缺失问题。

### 4.4 【P3】客户端重试

- `MessageRetry` 单例队列 + 定时 tick + 网络恢复触发，扫描互斥防并发重入（lib/service/message_retry.dart:34,47）；队列在内存 Map 中，量级小。✅ 与服务端投递重试语义分离明确（imboy/src/lib/elib_retry_config.erl:5-11 注释）。

---

## 五、Admin（imboyadmin）

### 5.1 【P3】构建与缓存策略良好（正面确认）

- 全路由 `React.lazy` 代码分割（src/App.tsx:16-25 起）；vite `manualChunks` 把 react/recharts/radix/tanstack 等拆独立 vendor chunk（vite.config.ts:15-33），recharts 只被 3 个懒加载页引用（FinanceReportPage/DashboardPage/AnalyticsPage）不进首屏。✅
- TanStack Query 全局 `staleTime: 5min`（src/App.tsx:90-93），权限/横幅等复用同样 5min（src/hooks/useAdminPermission.ts:43,51）。✅
- 表格统一 `DataTablePagination` 服务端分页（项目规范，默认 size 10）。✅

### 5.2 【P2】大表分页的 total 成本在后端

- Admin 每次翻页触发后端 `page_with_total` 的 count(*)（见 §2.3，elib_pg.erl:485,522）。前端无需改动，修后端即可。

### 5.3 【P3】`vendor-misc` 兜底 chunk

- `manualChunks` 未命中的依赖全落 `vendor-misc`（vite.config.ts:32）——该 chunk 会随依赖增长悄悄变大且任何依赖变更都打破缓存。建议 CI 加 bundle size 预算检查（rollup-plugin-visualizer 报表）。

---

## 六、Storage（Garage S3）

### 6.1 【P3】直传架构正确（正面确认）

- 上传：服务端只签 presigned PUT（1h），客户端直传 Garage，字节流不过 Erlang 节点（imboy/src/logic/attach_logic.erl:30-43,21-22）。✅
- confirm 回调做 HEAD 核实真实 mime/size（attach_logic.erl:8,122-128）——每附件 1 次服务端→Garage 往返，量级可接受。
- 下载：public scope 直读公开 URL，受限资源签 600s GET（attach_logic.erl:9,23-24）；presign 本身是本地 HMAC 计算（elib_s3_sign）无网络往返，成本在 authorize 的 DB 鉴权。
- 主要优化空间即 §4.2 的批量 view_url。

---

## 问题汇总表

| # | 层 | 问题 | 证据 | 风险 |
|---|-----|------|------|------|
| 1 | 后端并发 | user_server 单进程串行全站上线/下线（DB 写 + 3×5000 离线读 + 好友全量 fanout），重连风暴积压 | src/logic/user_server.erl:36,94-127,218-223; user_logic.erl:127-131 | **P0** |
| 2 | 后端并发 | depcache 唯一 gen_server：投递定时器/ACK 标志的 set/flush 全部同步 call，消息热路径全节点串行点 | deps/depcache/src/depcache.erl:454,574; ds/message_ds.erl:154; logic/websocket_logic.erl:58,71 | **P0** |
| 3 | WS 热路径 | C2G 扇出在发送者 WS 进程内同步 O(成员数)；撤回 O(成员×设备) 次 syn+depcache 操作 | src/logic/msg_c2g_logic.erl:386-392,611-626 | P1 |
| 4 | 数据库 | 连接池耗尽走 sleep(1s/2s/3s) 盲重试阻塞调用进程；query 逐次 parse 无 prepared statement 缓存 | src/lib/elib_pg.erl:92-95,197-199,237; config/sys.config:177-178 | P1 |
| 5 | 数据库 | 上线离线检查最多拉 3×5000 整行只为对比阈值 10；msg_s2c 每写先全量 count | ds/message_ds.erl:358-362,558-560; include/chat.hrl:14; ds/msg_s2c_ds.erl:104-115 | P1 |
| 6 | 后端并发 | msg_store_worker 单 worker = 持久化吞吐上限（批100/1s） | src/ds/msg_store_worker.erl:44-46,107-110 | P1 |
| 7 | WS 热路径 | 投递管道 JSON 中间格式：protobuf/v2 客户端每消息每设备 decode+re-encode | src/api/websocket_handler.erl:858-893,776-777 | P1 |
| 8 | 数据库 | billing_usage 单行热点：每条 C2C 一次同行 UPDATE + spawn-per-message | src/logic/billing_meter.erl:36,53; msg_c2c_logic.erl:83 | P2 |
| 9 | 数据库 | page_with_total 每页 count(*)（admin 大表翻页） | src/lib/elib_pg.erl:485,522 | P2 |
| 10 | 数据库 | msg_c2c GIN 全文索引对 E2EE 密文也分词；低基数单列索引写放大 | priv/migrations/00000005_msg_c2c.up.sql:82,68; 00000008:43,57 | P2 |
| 11 | WS 热路径 | v2 JSON 上行双重 decode | src/api/websocket_handler.erl:337,489 | P2 |
| 12 | Flutter/Storage | view_url 无批量端点，媒体密集首屏 N 次往返 | lib/service/asset_url_resolver.dart:30-33; src/api/attach_handler.erl:88 | P2 |
| 13 | Admin | vendor-misc 兜底 chunk 无预算管控 | imboyadmin/vite.config.ts:32 | P3 |
| 14 | Flutter | 约 20 处 shrinkWrap:true 待逐个核查大数据源 | 如 lib/component/chat/message_image_multi_builder.dart | P3 |
| 15 | 后端并发 | WS opts 中 num_acceptors/max_connections 为不生效的死配置 | src/api/websocket_handler.erl:56-58 | P3 |

### 正面确认（无需处理）

- 每连接一进程 + syn 无中心注册/直投（imboy_syn.erl:148-173）
- agent_rate_limiter 原子 ETS 时间桶，无 gen_server（agent_rate_limiter.erl:72-90）
- 好友/群成员/e2ee 门均有 depcache 读缓存，无经典 N+1（friend_ds.erl:389; group_ds.erl:71-87）
- billing 热路径未调用 report_usage（红线遵守，billing_meter.erl:9-12）
- msg 表 hypertable 分区 + 手工索引齐备；Flutter SQLite 索引完备；Admin lazy 路由 + chunk 拆分 + staleTime
- Garage 直传（presigned PUT），字节流不过应用节点
