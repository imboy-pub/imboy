# imboy 三端盘点跨域汇聚与行动计划

> 汇聚日期：2026-07-02 ｜ 性质：只读汇聚与规划，不含任何代码改动
> 输入：`docs/archive/review/01_ws_protocol.md`、`docs/archive/review/02_message_stability.md`、`docs/archive/review/03_e2ee.md`
> 条目引用记法：[WS-*]=01 号文档、[MSG-*]=02 号文档、[E2EE-*]=03 号文档

> **执行进度（2026-07-02 当日实施，已提交至 imboy dev/main 本地，未 push）**：
> ✅ 已完成并过绿灯门：T00、T01、T02、T03、T04、T05、T06、T07、T08、T09、T11、T12、T16、T21（含 [WS-P1-8]）
>   提交：`3e9caabc`（代码批次）、`4732f448`（本文档）、`ce1cab1e`（T09/D3）、`78d29ccd`（T03/P0-2）
> ✅ T03/P0-1（多端 ACK 按设备送达）已实施：**V-A 拍板 = (b) per-device**，新增 msg_delivery 表
>   （ACK 标记语义，见 design-T03-P0-1 第 9 节实施记录）；⚠️跨仓跟进：imboyapp 的 REST
>   /offline 与 offline_ack 需带 did 参数才获得按设备语义；V7（C2G 多端未读）未动，可复用本表扩展
> ✅ T14（帧层健壮性包）已实施：decode 拒收 Ver≠2（unsupported_version）+ 三处静默丢弃
>   （帧解码失败/未知帧类型/payload 双路解码失败）改回 ERROR 帧(0x06,负载=UTF-8 原因文本)；
>   帧头 9 字节布局未动；[WS-P1-8] 已随 T21 完成不重复；FLAG_ACK 文档化留 T17
> ✅ T15（响应显式标注，后端先行）已实施：全部同步响应组装点（C2C/C2G/C2S_SERVER_ACK、
>   sync_resp、CLIENT_ACK_CONFIRM、ws_validation_error）加纯加性字段 **in_reply_to**=请求消息 id。
>   ⚠️字段名未按盘点原议用 reply_to——该名已被引用回复占用（上行顶层 map `#{msg_id,from_id}`，
>   三端已消费），同名不同型会撞车，改用 IETF 惯例 in_reply_to；⚠️protobuf 通路经 to_pb_map
>   只保留 schema 字段，in_reply_to 现仅 JSON 通路生效，proto 增字段为三端共享契约跨仓跟进
> ✅ T18（已读同步到自己其他设备，后端先行）已实施：C2C 已读落库后向阅读者本人
>   推送新增 S2C action **message_read_sync**（payload=msg_id/peer/read_at，save 落 msg_s2c，
>   离线设备按 T03 per-device 语义重连可拉；阅读设备收到后幂等忽略）；旧客户端按未知
>   action 忽略不 break；⚠️跨仓跟进：imboyapp 消费该 action 更新未读数 + 真机双端回归
> ✅ T17（文档收敛）已实施：websocket-api-2.md 修订 6 处过时点（Flags DIR 位/ACK 方向/
>   重试真值/RPC deprecated/FLAG_ACK 装饰性/ERROR 帧与错误处理表）+ 2026-07-02 changelog；
>   ws-protocol-contract.md 新增 §9 语义类型总表（RPC vs 推送 vs 回执 + in_reply_to 辨析 +
>   message_read_sync + REST did 参数）；CLAUDE.md 修正 QoS 神话（重试真值+离线=存储常态+
>   per-device ACK）并修复 4 处失效文档路径（websocket-api-2/tsid/rest-api-v1 均实际在
>   docs/archive/analysis/）；引用路径已 ls 全量核验可达
> ✅ P2 backlog 部分完成：[MSG-P2-6] 已修——⚠️盘点建议的三列唯一约束在 msg_read
>   （hypertable，按 created_at 分区）上不可行（唯一索引必须含分区列），改为 save_read
>   SQL 内 WHERE NOT EXISTS 三列应用层去重 + 原四列 ON CONFLICT 兜并发竞态，零 DDL。
>   [MSG-P2-1] 已修——重复 ACK（送达标记已存在 {ok,0}）不再自增 msg_delivered_total；
>   [MSG-P2-2] 已修——撤回/已读旁路写点改 write_msg_if_absent 按 (msg_id,to_id) 判重
>   （revoke_offline_msg/9 与 read_offline_msg 两个活跃点；/5 为零调用遗留未动）。
>   [E2EE-P2-17] 已修——删除 e2ee_transfer_ds:cleanup_expired_sessions DS 层双实现
>   （零生产调用+直连 SQL 违反分层+条件不等价：status != 'confirmed' 会误删终态记录且无
>   LIMIT），生产清理唯一路径 = e2ee_cleanup_worker → repo 版。
>   [E2EE-P2-15] 部分修——①worker 清理失败补 WARN 告警；②transmission_log 增保留期清理
>   （新 repo delete_older_than/1，批量 LIMIT 1000，配置 e2ee_transmission_log_retention_days
>   默认 180 天，<=0 不清理）；create_shards 幂等键需契约设计，留后续。
>   其余 P2 项处置：[MSG-P2-4] 已随 T06 完成；[WS-P1-7] 格式位——flags 位已占满
>   （bit4-3 被 DIR 占用），无安全空位，维持嗅探；[MSG-P2-5]（自愈型瞬时窗口）与
>   [E2EE-P2-16]（设备上限拒登录属产品决策）留后续批次
> ⏳ 待做：T10（D4=删孤岛，⚠️涉及 imboy_router.erl——该文件现有并发未提交改动，须等其
>   落地后再做）、T19（D6=接线，升 L 需 admin 联动且 src/adm/* 同样有并发改动）、
>   T20（D5=保持开启，前置三项属归档架构级大改）——均需独立立项/协调
> 裁决状态：D1/D2 已被 T00 核实消解；**D3/D4/D5/D6 已拍板（见第 4 节选中项）**
>
> **绿灯门结果**：`make app` 编译通过；触碰面全部测试模块单独重跑全绿（msg_c2c/c2g/c2s/s2c_logic、e2ee_social_logic/handler/shard_validator、e2ee_transfer_ds/logic/repo、e2ee_social_repo、msg_store_ds/repo、msg_c2c_ds、user_ds、user_device_logic、websocket_handler）；`make dialyze` 仅剩预存基线 warning（jsx/gen_statem 类型缺失，本会话 diff 零 jsx 调用，CI 中 dialyze 为 continue-on-error 基线 job）。全量 `make eunit` 的其余失败为 DB 未起（missing_config pg_conf / econnrefused）与若干预存测试漂移，均非本轮引入——本轮顺带修复了因签名/返回值变更连带的旧测试：ws handler 补 parse_qs/peer 桩、stage 返回值断言、transfer/social repo 的 UUID 生成迁移测试、mark_processed/1 签名、e2ee 列 JSONB 包装断言。

---

## 0. 汇聚判定

| 指标 | 值 | 依据 |
|---|---|---|
| 跨域关联数 X | **5** | 见第 1 节 R2/R3/R5/R6/R8（同文件多域报告或同根因多域出现） |
| 跨域冲突数 Y | **0** | 无"不同域给出相反改法"的硬冲突；但有 6 项单点决策需拍板（第 4 节） |
| 改动依赖数 Z | **5** | T06←T05、T12←T02、T17←T13/T15、T20 内部前置（[MSG-P1-8/9][MSG-P2-3]）、T01/T03/T13 定级依赖 T00 核实 |

**判定：X>0 且 Z>0 → 执行完整汇聚流程。**

三处跨域锚点已当场抽查确认（仅确认关联，未重新审计）：
- `src/logic/msg_s2c_logic.erl` 同时被三份盘点点名（无兜底子句 / 分片归属校验缺口 / 丢弃 stage 返回值）；
- `src/logic/msg_ack_logic.erl:21-40` 确认忽略 `_DID` 形参且无条件 `msg_store_ds:unstage(MsgId)`；
- `src/ds/msg_store_worker.erl:191` 确认 `application:get_env(imboy, msg_archive_enabled, false)` 代码默认 false（与项目记忆"实际默认 true"矛盾，列入第 5 节待核实）。

---

## 1. 根因聚类（"一处改、全局受益"优先）

### R1：`ws_reply/2` 硬编码 `framing=none`，v2 同步响应系统性丢帧头
- 衍生条目：[WS-P0-1]
- 受益面：一处收口（删 `ws_reply/2`、强制传 State framing）修复 CLIENT_ACK_CONFIRM / CLIENT_ACK_ERROR / 路由校验错误等**全部 v2 同步响应路径**，消除每条下行消息 4 轮无效 ACK 重试与 pending 堆积。
- 涉及文件：`src/api/websocket_handler.erl`（:367,376,387,433,441,451,730,747,755,770-771）

### R2：ACK 入口语义混杂——不分设备 + 与 staging 生命周期耦合
- 衍生条目：[MSG-根因A][MSG-P0-1][MSG-P0-2]；关联 [WS-P1-2]（WEBRTC 方向被同一入口拒收，staging 清理对整类消息失效）；[WS-ACK专项]（帧级 ACK 适配进同一管道且无法携带 did，多设备语义降级）
- 受益面：`msg_ack_logic:client_ack/4` 一处重构（ACK 只标记"该设备已收到"，processed 只允许 worker 落库成功后置位，参照 `msg_c2g_timeline_repo:client_ack` per-uid 范本）同时消除两条**真实永久丢消息路径**，并为多端一致打地基。
- 涉及文件：`src/logic/msg_ack_logic.erl`、`src/ds/msg_operation_ds.erl`、`src/repo/msg_c2c_repo.erl`、`src/repo/msg_s2c_repo.erl`、`src/api/websocket_handler.erl`（validate_ack_params/process_ack_type）

### R3：服务端对客户端重发无幂等短路，重试边界过粗
- 衍生条目：[MSG-根因B][MSG-P1-1][MSG-P1-5]；关联 [WS-P1-1]（webrtc 成功零响应 → 客户端按重试策略重发 offer → 重复 SDP，与历史 SDP glare 嫌疑吻合——同属"重发无短路"家族的客户端触发面）
- 受益面：`msg_store_ds:stage/10` 返回 `{ok,new}|{ok,duplicate}` 一个签名改动，同时切断 C2C/C2G 两条重复推送路径；拆分 `async_retry` 复合闭包消除"后置步骤失败整体重放"。
- 涉及文件：`src/ds/msg_store_ds.erl:152-162`、`src/logic/msg_c2c_logic.erl:245-320`、`src/logic/msg_c2g_logic.erl:266-341`、`src/lib/elib_retry.erl`（只读参照）

### R4：撤回路径把"撤回命令自身 MsgId"误当"OriginalMsgId"（两处同源）
- 衍生条目：[MSG-根因C][MSG-P0-3][MSG-P0-4]；派生 [MSG-P1-3][MSG-P1-4]（定时器不取消 / 秒撤竞态，修传参后顺路联动）
- 受益面：调用点传参修复，代价极小，C2C/C2G 撤回从"全空操作"变为生效。
- 涉及文件：`src/logic/msg_c2c_logic.erl:411-422`、`src/ds/msg_c2c_ds.erl:190-219`、`src/logic/msg_c2g_logic.erl:521-531`、`src/ds/msg_c2g_ds.erl:146-176`

### R5：`e2ee_shard_validator` 同名模块编译冲突，审计链从未写入一条正确记录
- 衍生条目：[E2EE-P0-1][E2EE-P1-10]（测试网按错误签名 meck，破洞同源）
- 受益面：合并为单一模块 + 按调用方实际语义定契约 + CI 加"同 `-module` 名禁多源文件"检查——审计链是 E2EE 其他一切监控的前提。
- 涉及文件：`src/lib/e2ee_shard_validator.erl`、`src/logic/e2ee_shard_validator.erl`、4 个调用点（`e2ee_social_logic.erl:120`、`msg_c2s_logic.erl:321`、`msg_s2c_logic.erl:161,196`）、`test/logic/e2ee_social_logic_tests.erl`

### R6：`msg_s2c_logic` 上行入口三域缺口汇聚（文件级关联，非同一根因，但必须一次改完）
- 衍生条目：[WS-P0-2]（无兜底子句 → function_clause 误报 invalid_json）+ [E2EE-P1-8]（store_shard/shard_stored/decrypt_shard 不查库核实归属直接转发）+ [MSG-P2-4]（两处丢弃 `stage/10` 返回值静默继续投递）
- 证据判定：三条问题分属协议健壮性 / 安全 / 错误处理三个根因，**不合并根因**；但同一文件同一批 action 子句，拆三个任务会互相冲突，**合并为一个执行任务**（T06）。
- 涉及文件：`src/logic/msg_s2c_logic.erl`（含 :19-237 子句区、:141-211 分片区、:46-57/266-277 stage 调用）、`src/logic/msg_c2s_logic.erl:304-346`

### R7：E2EE 恢复路径与分片生命周期缺失（域内根因，跨端影响）
- 衍生条目：[E2EE-P0-2]（分片永不失效可重放 + 撤销不级联）[E2EE-P0-3]（注销不清 e2ee_* 表）[E2EE-P0-4]（同账号传输被拒，换机主路径断）[E2EE-P0-5]（backup create 孤岛）[E2EE-P1-7]（accept 不延长 expires_at）[E2EE-P1-9]（threshold 硬编码 0）
- 受益面：分片"一次性语义 + 级联失效 + 注销级联"三件套关掉"历史代理合谋重建（含已注销用户）私钥"的真实安全敞口。
- 涉及文件：`e2ee_social_handler/ds/repo`、`user_ds.erl:225-266`、`e2ee_transfer_handler/logic/repo`、`imboy_router.erl:265-266`

### R8：重试间隔文档神话"2/5/7/11s 转离线"（双域重复报告，一次修订全消）
- 衍生条目：[WS-P2-表#3、#6][MSG-P1-10]；同批收敛 [WS-P2-表#1/#2/#4/#5]（Flags 位 / ACK 方向 / 文档路径失效）与"双 v2 命名债"
- 受益面：`imboy/CLAUDE.md` 关键特性段 + `websocket-api-2.md` 六处 + contract 补「语义类型总表」一轮文档任务全部解决；真值以 `src/lib/elib_retry_config.erl:20-27` 为准（C2C `[0,3000]`、C2G `[0]`，且"离线不是转存动作而是存储常态"）。
- 涉及文件：`imboy/CLAUDE.md`、`docs/reference/websocket-api-2.md`、`docs/reference/ws-protocol-contract.md`

### R9：协议无显式响应标记，`type` 一字段背三职（域内根因）
- 衍生条目：[WS-P1-6][WS-总论]；关联 [WS-P1-8]（e2ee_key_changed_ack 裸 map 违反信封契约——补信封时一并归位）
- 受益面：消息信封加可选 `reply_to` 字段（纯加性，旧客户端零破坏），客户端凭其存在即可区分 RPC 响应与推送，长期让 type 回归纯方向枚举。
- 涉及文件：`message_ds`/`message_policy`/`msg_c2s_logic` 等响应组装点 + 三端消费方

---

## 2. 全局分级（覆盖单域定级）

### P0 —— 阻断正确性（丢消息 / 撤回失效 / ACK 闭环断 / 私钥安全敞口）

| # | 问题 | 来源域 | 根因 |
|---|---|---|---|
| G-P0-1 | v2 同步响应丢帧头，CLIENT_ACK_CONFIRM 闭环失效 | 【WS】 | R1 |
| G-P0-2 | 多端 ACK 误删离线行 + ACK/落库竞态，两条永久丢消息路径（C2G 一人 ACK 全群离线成员丢） | 【消息】 | R2 |
| G-P0-3 | C2C/C2G 撤回传参错位，撤回实质空操作 / 离线端仍收原文 | 【消息】 | R4 |
| G-P0-4 | 同名模块冲突，分片审计日志零正确记录且失败被吞 | 【E2EE】 | R5 |
| G-P0-5 | 分片永不失效可无限重放 + 撤销不级联 + 注销不清 e2ee_* 表（合谋可重建已注销用户私钥） | 【E2EE】 | R7 |
| G-P0-6 | 同账号换机 transfer 被拒 + backup create 孤岛，三条恢复路径两条断 | 【E2EE】 | R7（需拍板 D3/D4） |
| G-P0-7 | 客户端上行 S2C 未注册 action → function_clause 误报 invalid_json | 【WS】 | R6 |

> 注：[WS-P0-1] 的最终定级依赖 T00 核实 Dart 端有无裸 protobuf 回退；核实前按 P0 对待。

### P1 —— 健壮性 / 体验 / 一致性失效

| # | 问题 | 来源域 | 根因 |
|---|---|---|---|
| G-P1-1 | 重发无幂等短路 → 重复推送；复合重试闭包整体重放 | 【消息】 | R3 |
| G-P1-2 | webrtc 成功零响应 → 客户端重发 → 重复 SDP（关联历史 SDP glare 嫌疑） | 【WS】 | R3（需拍板 D2） |
| G-P1-3 | CLIENT_ACK WEBRTC 方向黑洞，staging 清理不发生 | 【WS】 | R2（需拍板 D1） |
| G-P1-4 | WS relay 分片消息无归属校验，可伪造分片状态骚扰 | 【E2EE】 | R6 |
| G-P1-5 | timeline `list_by_uid` 无 ORDER BY 就 LIMIT，堆积时选错子集 | 【消息】 | 独立 |
| G-P1-6 | 撤回不取消重试定时器 + 秒撤竞态误判 not_found | 【消息】 | R4 派生 |
| G-P1-7 | 已读不广播给自己其他设备，B 设备未读数永久脱节 | 【消息】 | 独立（多端一致家族） |
| G-P1-8 | transfer accept 后不延长 expires_at，accepted 会话被 cleanup 误清 | 【E2EE】 | R7 |
| G-P1-9 | threshold 硬编码 0；恢复链路仅 JWT 无独立因子 | 【E2EE】 | R7 |
| G-P1-10 | 帧层健壮性缺口：版本不校验 / 未知帧静默丢 / FLAG_ACK 装饰位 / e2ee_key_changed_ack 裸 map | 【WS】 | R9 邻域 |
| G-P1-11 | 生产无服务端权威序（archive 默认关）；开启前置 [MSG-P1-8/9][MSG-P2-3] 未解 | 【消息】 | 独立（需拍板 D5） |
| G-P1-12 | 手动删设备不踢 WS 会话不 revoke token | 【E2EE】 | 独立 |
| G-P1-13 | 文档漂移：重试神话 / Flags 位 / 失效路径引用 | 【WS】【消息】 | R8 |
| G-P1-14 | `msg_rate_logic` 死代码但 admin"解除禁言"操作空表 | 【消息】 | 独立（需拍板 D6） |

### P2 —— 一致性 / 债务（不逐条立任务，随批次 4 打包）

指标虚高 [MSG-P2-1]、撤回/已读旁路 staging [MSG-P2-2]、conv_seq 烧号空洞 [MSG-P2-3]（归档前置，归入 T20）、瞬时重连窗口 [MSG-P2-5]、msg_read 唯一约束含客户端时间戳 [MSG-P2-6]、E2EE 死代码族/transmission_log 无清理/设备数无上限/DS 层直连 SQL [E2EE-P2-14~17]、JSON/protobuf 内容嗅探 [WS-P1-7 降为 P2]、双 v2 命名债。

---

## 3. 任务计划

### T00：核实四项前置假设（客户端 + 运行时）
- 来源根因：三份盘点「待核实」合集（第 5 节 V1–V4）
- 域：跨端（只读核查，不改码）
- 规模：S
- 依赖：无（**批次 0，最先做**；其结论决定 T01 定级、T03 的 WEBRTC 分支、T13 方案）
- 验证（绿灯门）：核查报告回填本文档第 5 节即为完成；不涉及编译
- 兼容性提示：无改动

### T01：修复 `ws_reply/2` 丢 framing，收口全部 v2 同步响应
- 来源根因：R1（[WS-P0-1]）
- 域：后端
- 规模：S
- 依赖：无（T00 只影响定级不阻塞修复——两种核实结果下该修法均正确）
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；随后真机验证 v2 连接下 CLIENT_ACK 一次确认不重试
- 兼容性提示：实现 bug 修复非协议变更；v1（json/none framing）路径行为不变

### T02：修复撤回 OriginalMsgId 传参错位（C2C+C2G 两处同源）
- 来源根因：R4（[MSG-P0-3][MSG-P0-4]）
- 域：后端
- 规模：S
- 依赖：无
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；补撤回场景 EUnit（离线接收方上线不再收到原文）
- 兼容性提示：消息结构不变，仅服务端行为修正

### T03：重构 ACK 语义——按设备送达标记，unstage 与 ACK 解耦
- 来源根因：R2（[MSG-P0-1][MSG-P0-2][WS-P1-2]）
- 域：后端（若 D1 裁决为"客户端停发 WEBRTC"则含移动端）
- 规模：L
- 依赖：blockedBy T00（V2 核实）+ 裁决 D1；建议 T01 先行（ACK 闭环通了再重构语义，便于真机验证）
- 验证（绿灯门）：`make compile && make eunit && make dialyze && make ctl ARGS="smoke all"`；双端登录场景真机回归（A 在线 ACK 后 B 离线重连仍能拉到）
- 兼容性提示：CLIENT_ACK 线上格式不变（`CLIENT_ACK,type,msgid,did`），仅服务端消费语义变更，不 break 旧客户端；参照 `msg_c2g_timeline_repo:client_ack` per-uid 范本

### T04：stage/10 返回 `{ok,new}|{ok,duplicate}` + 拆分复合重试闭包
- 来源根因：R3（[MSG-根因B][MSG-P1-1][MSG-P1-5]）
- 域：后端
- 规模：M
- 依赖：无（与 T03 同文件区域少量交叠，建议排在 T03 之后合并冲突成本更低，非硬依赖）
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；EUnit 覆盖"重发同 msg_id 不二次投递"
- 兼容性提示：客户端无感知，纯服务端幂等加固

### T05：合并 `e2ee_shard_validator` 同名模块 + 端到端落库单测 + CI 静态检查
- 来源根因：R5（[E2EE-P0-1][E2EE-P1-10]）
- 域：后端
- 规模：M
- 依赖：无
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；新增不 meck 的落库单测；CI 增加"同 `-module` 名禁止多源文件"检查并跑通
- 兼容性提示：以调用方实际语义 `(Event, ShardId, Meta)` 为契约基准；对外 API 无变化

### T06：整固 `msg_s2c_logic` 上行入口（兜底 + 归属校验 + stage 返回值）
- 来源根因：R6（[WS-P0-2][E2EE-P1-8][MSG-P2-4]），含 `msg_c2s_logic:304-346` 同类分片路径
- 域：后端
- 规模：M
- 依赖：blockedBy T05（shard_validator 契约定稿后再动调用点）；建议 T04 先行（stage 返回值语义定稿）
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；EUnit 覆盖未知 action 返回 `unknown_action`（非 invalid_json）、非归属 shard_id 被拒
- 兼容性提示：兜底响应语义与 `message_router_logic:147-148` 的 route_action 兜底对齐；合法客户端行为不受影响

### T07：分片一次性语义 + 撤销级联 + threshold 真值校验
- 来源根因：R7（[E2EE-P0-2][E2EE-P1-9]）
- 域：后端
- 规模：M
- 依赖：无
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；EUnit 覆盖 decrypt_shard 二次调用被拒/告警、remove_trusted_contact 级联失效分片
- 兼容性提示：客户端恢复流程单次取分片不受影响；"used 后再取"的行为变化需在 contract 文档注明

### T08：账号注销级联清理 e2ee_* 四表
- 来源根因：R7（[E2EE-P0-3]）
- 域：后端
- 规模：S
- 依赖：无
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；EUnit 覆盖注销后 `e2ee_social_shards`（含 proxy_uid 维度）/`e2ee_trusted_contacts`/`e2ee_transfer_sessions`/`e2ee_local_backups` 全清
- 兼容性提示：无协议变化；属被遗忘权合规修复

### T09：放开同账号跨设备 transfer（换机主路径）
- 来源根因：R7（[E2EE-P0-4]）
- 域：跨端（后端放开 + Flutter 端联调）
- 规模：M
- 依赖：blockedBy 裁决 D3（产品确认原语义）
- 验证（绿灯门）：后端 `make compile && make eunit && make dialyze`；移动端 `flutter analyze && flutter test` + 真机双设备换机回归
- 兼容性提示：限制改为「同 uid 不同 device_id 允许」，跨 uid 传输语义维持现状不放宽

### T10：本地备份路径二选一（补 create 端点 或 删孤岛）
- 来源根因：R7（[E2EE-P0-5]）
- 域：跨端（补）或后端（删）
- 规模：M（补）/ S（删）
- 依赖：blockedBy 裁决 D4；建议在 T09 结论之后（换机主路径通了，备份优先级可降）
- 验证（绿灯门）：补→后端三件套 + `flutter analyze && flutter test`；删→后端三件套 + 按死代码三闸标准（当场重跑 xref + 独立 grep + 路由注册表核对）
- 兼容性提示：删除方案需同步删路由 2 端点 + repo + 表迁移，Flutter 端 list/delete 调用同步下线

### T11：`msg_c2g_timeline` 拉取补 ORDER BY
- 来源根因：盘点条目 [MSG-P1-2]
- 域：后端
- 规模：S
- 依赖：无（排序键选型见第 5 节 V5，若 DDL 无 id 列则用 `created_at ASC`）
- 验证（绿灯门）：`make compile && make eunit && make dialyze`
- 兼容性提示：无

### T12：撤回联动——取消重试定时器 + 秒撤兜底查 staging
- 来源根因：R4 派生（[MSG-P1-3][MSG-P1-4]）
- 域：后端
- 规模：M
- 依赖：blockedBy T02
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；EUnit 覆盖"3s 窗口内撤回不再投递原文""staging 内消息可撤"
- 兼容性提示：无协议变化

### T13：webrtc 信令响应语义定稿并实施
- 来源根因：R3 关联（[WS-P1-1][WS-P1-2] 的 webrtc 面）
- 域：跨端
- 规模：M
- 依赖：blockedBy 裁决 D2 + T00（V3 核实 Flutter webrtc 是否挂 MessageRetry）
- 验证（绿灯门）：后端三件套；移动端 `flutter analyze && flutter test` + **真机双端通话回归**（含跨网络场景，验证 SDP 不重复）
- 兼容性提示：若选"加 SERVER_ACK"，为纯加性下行消息，旧客户端忽略即可，不 break；若选"文档定 fire-and-forget"，客户端须同步停重试

### T14：帧层健壮性包——版本校验 + 未知帧回 ERROR 帧 + e2ee_key_changed_ack 补信封
- 来源根因：盘点条目 [WS-P1-5][WS-P1-3][WS-P1-8]（[WS-P1-4] FLAG_ACK 按文档化处理归入 T17）
- 域：后端
- 规模：M
- 依赖：无
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；`test/lib/imboy_frame_tests.erl` 补版本拒收/ERROR 帧用例
- 兼容性提示：**帧头 9 字节布局不动**（三端字节级 fixture 锁死）；`decode` 拒收 Ver≠2 属守护断言，现网客户端全部发 Ver=2 不受影响；ERROR 帧为新增下行类型，旧客户端未知类型按其现有丢弃逻辑处理，V1/V2 共存不 break

### T15：消息信封增加可选 `reply_to` 字段
- 来源根因：R9（[WS-P1-6][WS-总论]）
- 域：跨端（后端先行，三端渐进消费）
- 规模：M（后端）+ 各端各 S
- 依赖：无硬依赖；建议在 T03 之后（ACK 响应组装点稳定后统一加字段）
- 验证（绿灯门）：后端三件套 + `.claude/scripts/ddd_loop_gate.sh`（跨仓契约门）；SDK `npm run build` + vitest；移动端 `flutter analyze && flutter test`
- 兼容性提示：**纯加性字段**，旧客户端忽略未知字段零破坏；`type` 现有越界值本期不动（长期回归纯方向枚举另行立项）

### T16：transfer accept 后延长 expires_at（窗口做成配置项）
- 来源根因：R7（[E2EE-P1-7]）
- 域：后端
- 规模：S
- 依赖：无
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；EUnit 覆盖 accept→超 300s→confirm 成功
- 兼容性提示：无

### T17：文档收敛——重试真值 + 语义类型总表 + 六处过时点
- 来源根因：R8（[WS-P2 全表][MSG-P1-10]）+ [WS-P1-4]（FLAG_ACK 标注"仅心跳装饰性使用"）+ RPC_REQ/RSP 标 deprecated + 双 v2 命名说明
- 域：后端（docs）
- 规模：M
- 依赖：blockedBy T13、T15（协议语义定稿后一次写清，避免二次返工）
- 验证（绿灯门）：文档评审 + `imboy/CLAUDE.md` 引用路径全部可达（`ls` 核验）
- 兼容性提示：无代码改动

### T18：已读回执广播到阅读者自己的其他设备
- 来源根因：盘点条目 [MSG-P1-6]（多端一致家族）
- 域：后端 + 移动端
- 规模：M
- 依赖：建议 T03 之后（多端送达模型定稿后统一语义）
- 验证（绿灯门）：后端三件套；移动端 `flutter analyze && flutter test` + 真机双端未读数回归
- 兼容性提示：新增一类 S2C 推送，旧客户端忽略即可

### T19：死代码清扫（E2EE 族 + msg_rate_logic 裁决后处置）
- 来源根因：盘点条目 [E2EE-P1-6][E2EE-P2-14][MSG-P1-7]
- 域：后端
- 规模：M
- 依赖：blockedBy 裁决 D6（msg_rate_logic 删除还是接线）；`create_key_shares/5` 等无需裁决可先删
- 验证（绿灯门）：按项目死代码三闸标准——当场重跑 xref diff + 独立 grep 第二源 + 注册表核对；`make compile && make eunit && make dialyze`；完成后 `git diff --stat` 逐文件校验无越界
- 兼容性提示：若 D6 选"接线"，admin 解禁言功能需补 WS 层联动，规模升 L 另拆任务

### T20：归档/权威序决策落地（含开启前置三项）
- 来源根因：盘点条目 [MSG-P1-8][MSG-P1-9][MSG-P1-11][MSG-P2-3]
- 域：后端
- 规模：L
- 依赖：blockedBy 裁决 D5 + T00（V4 核实生产 env 真值）；内部顺序：先修多节点 conv_seq 错位（按 conv_key 路由单一归档点）与焚烧/归档冲突、烧号空洞，再开 `msg_archive_enabled`
- 验证（绿灯门）：`make compile && make eunit && make dialyze && make ctl ARGS="smoke all"`；多节点环境验证 conv_seq 单调
- 兼容性提示：开启归档改变消息留存语义（与阅后即焚产品承诺冲突面必须先裁决），需在发布说明中对外声明

### T21：删设备联动踢 WS 会话 + revoke token
- 来源根因：盘点条目 [E2EE-P1-12]
- 域：后端
- 规模：S
- 依赖：无（与 logout 路径对齐即可）
- 验证（绿灯门）：`make compile && make eunit && make dialyze`；EUnit 覆盖删设备后旧 token 失效
- 兼容性提示：无

**P2 打包 backlog（不单独立任务，随批次 4 顺带或后续专项）**：[MSG-P2-1/2/5/6]、[E2EE-P2-15/16/17]、[WS-P1-7]（格式嗅探占 flag 位，纯加性）、msg_read 唯一约束改 `(msg_id,to_uid,to_did)`。

### 批次 DAG

```
批次0（串行起点，核实+裁决）
  T00（核实 V1-V5） + 用户裁决 D1-D6
      │
批次1（P0 独立修复，全部可并行）
  T01  T02  T05  T07  T08  T11  T16  T21
      │
批次2（依赖批次0/1 的正确性主修）
  T03（←T00,D1）   T04            ← T03/T04 同域建议串行，先 T03
  T06（←T05,T04）  T12（←T02）
  T09（←D3）       T10（←D4，且议程排 T09 后）
      │
批次3（协议演进 + 一致性，可并行）
  T13（←D2,T00）  T14  T15（议程排 T03 后）  T18（议程排 T03 后）
      │
批次4（收尾）
  T17（←T13,T15）  T19（←D6）  T20（←D5,T00）  P2 backlog
```

- **必须串行**：T00→(T01 定级/T03/T13/T20)；T02→T12；T05→T06；T13/T15→T17。
- **可并行**：批次 1 全部 8 个任务互不依赖；批次 3 的 T13/T14/T18 互不依赖。

---

## 4. 待裁决冲突 / 决策项（等你拍板，未擅自合并）

> 严格意义的"跨域相反改法"为 0；以下 6 项是单点二选一决策。
>
> **拍板结果（2026-07-02 用户已裁决）**：
> - **D1**：已被 T00 核实消解（ack_manager 已不发 WEBRTC ACK），无需后端加白名单
> - **D2**：已被 T00 核实消解（webrtc 信令不挂重试），按 fire-and-forget 文档化（并入 T17）
> - **D3 ✅ 选中【放开同账号跨设备】**：已实施（T09，提交 `ce1cab1e`）
> - **D4 ✅ 选中【删除 backup 孤岛】**：待做 T10 —— ⚠️破坏性 DDL（删表迁移）+ 跨仓（imboyapp 仍调 list/delete），需两仓协调后独立执行
> - **D5 ✅ 选中【保持归档开启 + 尽快修前置】**：待做 T20 前置三项（按 conv_key 单一归档点路由 / msg_burn 触及归档表 / next_conv_seq 失败不烧号）—— 归档架构级大改，需独立立项
> - **D6 ✅ 选中【接线激活 msg_rate_logic】**：待做 T19 —— 规模升 L，需与 admin 解禁言路径联动，独立立项

| # | 决策 | 选项 A | 选项 B | 推荐及理由 | 阻塞 |
|---|---|---|---|---|---|
| D1 | CLIENT_ACK 的 WEBRTC 方向 | 后端白名单加 WEBRTC（映射 no-op/s2c 清理） | 客户端停发 WEBRTC ACK | **A**：兼容存量旧客户端，改动在服务端一处；B 需发版且旧版本继续打黑洞 | T03 |
| D2 | webrtc 信令响应语义 | 成功也回 SERVER_ACK（与 C2C 对齐） | 契约明文 fire-and-forget，客户端禁止重试 | **A**：消除"有响应=失败"的反直觉语义，并直接压制 SDP 重发（历史 glare 嫌疑）；纯加性不 break | T13 |
| D3 | e2ee transfer 同账号语义 | 允许同 uid 不同 device_id | 维持拒绝（若原语义有意如此） | **A**：换机是本系统最常见恢复场景，现状主路径断裂；需产品确认当初拒绝是否有意 | T09 |
| D4 | 本地备份孤岛 | 补 create 端点激活整条路径 | 删除孤岛（表+2 API+repo+Flutter list/delete） | **视 D3 结果**：若同账号 transfer 打通，推荐 **B**（YAGNI，换机主路径已通）；若 transfer 维持拒绝，则 **A** 成为唯一自助恢复路径必须补 | T10 |
| D5 | 是否开启 msg_archive（权威序） | 修完前置后开启，对外承诺严格顺序 | 不开启，对外明示"近似序" | 产品决策：涉及留存合规、阅后即焚承诺、存储成本；技术侧仅要求"开启前必须先完成 T20 内部三项前置" | T20 |
| D6 | msg_rate_logic 死代码 | 删除（真实限流在 WS 层 throttle） | 接线（让 admin"解除禁言"生效） | **A 删除**，除非"管理端禁言"是在售功能承诺；若选 B 规模升 L 需另立任务 | T19 |

---

## 5. 未核实假设清单（合并去重）

| # | 假设/疑点 | 来源 | 核实结果（2026-07-02 T00 完成） |
|---|---|---|---|
| V1 | Dart `_handleV2Binary` 有无"裸 protobuf 回退"容错 | WS | ✅ 已核实：`lib/service/websocket.dart:537-541` 走 `ImboyFrame.tryDecode`，解不出帧直接 `return` 静默丢弃，**无裸 protobuf 回退** → [WS-P0-1] 维持 P0 |
| V2 | `ack_manager.dart` 现行代码是否仍发 WEBRTC 方向 ACK | WS | ✅ 已核实：全文无 webrtc 匹配，**客户端已不发 WEBRTC ACK** → [WS-P1-2] 降级为文档项，**D1 裁决消解**（后端无需加白名单），T03 解除 D1 阻塞 |
| V3 | Flutter webrtc 发送是否挂 MessageRetry | WS | ✅ 已核实：`signaling_v2.dart` 无 sendWithAck/MessageRetry 挂载，**webrtc 信令不重试** → [WS-P1-1] 降级，**D2 消解**：按"fire-and-forget 契约文档化"处理，T13 降级并入 T17 |
| V4 | 生产是否开启 `msg_archive_enabled`（代码默认 false，`msg_store_worker.erl:191`） | 消息 | ⚠️ 已核实：`config/sys.config:94` = **true**（非 local 环境用 sys.config → **生产归档开启**），与 02 号盘点"全环境默认关闭"相反 → [MSG-P1-8/9][MSG-P2-3] 是**活问题**，D5 语义变为"保持开启则 T20 前置三项必须尽快修" |
| V5 | `msg_c2g_timeline` DDL 是否有可排序 id 列 | 消息 | ✅ 已核实：`00000002_message_aux.up.sql:10-16` 无 id 列（msg_id/to_uid/to_gid/client_ack/created_at）→ T11 用 `ORDER BY created_at ASC` |
| V6 | Flutter 客户端是否有"按 conv_seq 检测缺口补拉"逻辑 | 消息 | 未核实，随 T20 设计阶段 |
| V7 | C2G timeline ACK 未按 DID 区分是否造成多端未读数串扰 | 消息 | 未核实，随 T03 设计阶段 |
| V8 | 跨节点 syn 广播一致性语义 | 消息 | 未核实，随 T20（多节点归档路由方案前） |
