# Binary 与 ETS / 内存审查（Phase C）

> 2026-11-23 · 审查范围：binary 引用与内存保留、iodata、拼接反模式、ETS 归属与原子性、全表扫描、persistent_term、在线数据生命周期
> 依据教程 Ch.03 Binaries and Memory / Ch.04 ETS

---

## 1. Binary 审查

### 1.1 红旗扫描结果（教程 Ch.3 §4 系列全量）

| 反模式 | 命中 | 判定 |
|---|---|---|
| §4.1 前插累积器 `<<(X)/binary, Acc/binary>>` | **0** | ✅ |
| §4.2 `++` fold/递归二次方 | 0 个热路径（命中均为启动期路由表一次拼接/日志前缀小串） | ✅ |
| §4.3 大 binary 切片长期持有 | 81 处 binary:part，未见大帧切片入长命状态的直接形态 | ⚠️ 残余观察面（§1.3） |
| §4.4 转发进程句柄堆积 | WS 进程**每帧 hibernate**（全 return 子句）= 每帧 fullsweep GC，ProcBin 句柄不堆积 | ✅（代价见 perf P2-6） |
| §4.5 输入造原子 | 6 处实质调用全白名单前置（见 checklist §1） | ✅ |
| §4.6 破坏 append 优化 | 无累积器双活版本形态 | ✅ |

### 1.2 正面确认

- **gpb 生成解码器内置防御**：`textguard_pb.erl:605/610/680/850...` 对每个匹配出的 bytes 子项显式 `binary:copy(Bytes)` —— 正是教程 Ch.3 §4.3 的推荐解药，protobuf 路径解出的 payload 不钉住父大帧。
- **跨节点大载荷**：C2G 网关批量按 `?GROUP_BATCH_MAX_BYTES=65536` 预算切块（message_ds.erl:28-31），erpc 参数 `external_size` 预估 + Deadline 窗口——避免教程 Ch.6 §4.3 的"500MB 一把梭"形态。
- **消息投递管道**：JSON 预编码 binary 直投（encode_delivery_frame），protobuf 客户端侧才 decode→re-encode——每消息一次转码而非逐跳复制。

### 1.3 残余观察面（P3，二轮动作）

- 81 处 `binary:part` 中，凡"切片 → 存 ETS / 存进程 state 超过单帧生命周期"的位点，理论上钉住父 binary（若父 > 64 字节且为 refc）。当前热路径（WS 2MB max_frame）经 hibernate 每帧释放，风险被 GC 节奏压制。二轮建议：对 ack_retry_cache / staging / moderation_shadow 等表跑 `binary:referenced_byte_size` 只读抽查（escript 诊断，不改代码）。
- `agent_run_pg.erl:103/652/654` 对 DB 枚举值 binary_to_atom（P3-4）：值域由服务端写入约束，有界；同仓 cs/eb 模块对 HTTP 输入已显式拒绝造原子（cs_http.erl:344、eb_enterprise_http.erl:177 注释）。

## 2. ETS 审查

### 2.1 表清单与归属（ets:new 全量 20 处）

| 表 | 创建者/owner | 生存期语义 | 判定 |
|---|---|---|---|
| msg_rate / agent_rate_limiter / ack_retry_cache / auth_oidc 一次性凭据 / auth_oidc_jwks / simple_captcha | **application master**（imboy_app.erl:69-95 显式 init_table，注释论证"必须长驻进程持有，否则短命 WS/请求进程持表随断线销毁"） | 随 app 生命周期 | ✅ 教程 Ch.4 §4.1 的正确解法，且注释把攻击面（反复连接断开绕限流）写透 |
| imboy_delivery_budget_ets | application master 经 `imboy_syn:init`（imboy_app.erl:69 → imboy_syn.erl:53-55），懒加载 ensure 仅兜底 | 随 app；崩溃竞态下计数归零=观测不连续，行数据无价值可丢（注释已接受） | ✅ |
| login_attempt 两表（login_attempt_ds.erl:285-288） | login_attempt_ds gen_server（imboy_sup 永久子） | 进程崩→表重建→计数清零。限流窗口重开 = 暴力破解面短暂敞开；但 one_for_one 秒级恢复 | ⚠️ 可接受（崩溃即意味着更大故障在发生）；可选 heir 提级 |
| moderation_shadow（moderation_shadow.erl:124） | moderation_shadow worker（永久子） | public + write_concurrency，多进程写 | ✅ worker 属主明确 |
| barrel_mcp registry/session/subscriptions/pending/inflight 五表 | MCP gen_server 系（transient 子） | transient 崩溃正常退出不重启——表随进程消失，MCP 域自愈靠重连重注册（注释：reg_all 幂等） | ✅ 语义自洽 |
| router/ws_action registry、cache_sync META、sms_fake OUTBOX | 各自常驻 owner / 测试 stub | 小表 | ✅ |

### 2.2 原子性与查询模式

- **读改写全部原子原语**：投递预算计数 `ets:update_counter(Tab, {deferred_total, Pid}, {2,1}, Default)`（imboy_syn.erl:399-400）、通知槽 CAS 用 `ets:select_replace`（:444-447，注释精确处理"行缺失与已置位同返 0"的区分）——教程 Ch.4 §4.2 的标准解法且更深一层。
- **无热路径全表扫描**：tab2list/match_object 全量 12 处均为小表（路由名、sync 元数据快照、测试 stub）。
- **遍历安全**：未见 set 表上 first/next 手工遍历被并发写污染的形态（教程 Ch.4 §4.4）；cache_sync 的 tab2list 用于快照/恢复路径，属冷路径。
- **bag 热键**：唯一 bag 表 sms_fake OUTBOX 为测试设施。✅
- **对象尺寸**：所有表存小行（计数/槽位/元数据）；无"2MB ConfigMap 单对象"形态（教程 Ch.4 §4.5）。✅

### 2.3 persistent_term（17 处 put 全量定性）

| 形态 | 位点 | 判定 |
|---|---|---|
| 启动期一次性 | elib_tsid:264（node_id）、imboy_license:67、elib_type:16（regex 编译缓存）、msg_rate_logic:146（timer ref） | ✅ 教科书用途 |
| 稀有事件 | 插件 manifest（loader/lifecycle）、barrel_mcp_registry:728（工具注册，reg_all 幂等低频）、adm_feedback_handler:300（管理端改配置）、imboy_hird_host_bridge:97（replay guard） | ✅ 无每请求写 |
| 惰性构建缓存 | cs_http.erl:355 / eb_enterprise_http.erl:212（known_keys 白名单缓存） | ✅ 构建后命中即不再 put；失效重建罕见 |

**结论：无教程 Ch.4 §4.6（persistent_term 当缓存=全局 GC 风暴）形态。**

## 3. 在线连接与用户状态数据生命周期

```
在线态（权威）：syn 注册表（imboy_syn:join/leave）——进程死自动摘除，崩溃自愈零残留
派生态：user_server 分片内存队列（有界+丢弃）——不落盘，丢=迟到通知，业务可忍
投递预算：DELIVERY_BUDGET_TAB（pid 键行，terminate/崩溃清理 + reset 兜底）——无价值行可丢
ACK 重试标志：ack_retry_cache（app master 持有；timer 随 WS 进程死而被 cowboy 清理链回收）
消息事实：PG（msg_store 批拉模型）——进程崩溃不丢事实，客户端离线补拉
```

数据按"事实在库、会话在进程、瞬态在 ETS"分层，每层崩溃语义都有注释论证——**生命周期设计是本项目强项**，未见"表死数据亡"或"进程携唯一副本"形态。

## 4. 内存防线现状与缺口

| 防线 | 现状 |
|---|---|
| 进程级 max_heap_size | 未设置（vm.args/代码均无）——单进程失控（如 2MB 帧 × 邮箱堆积）无熔断；由 BE-14 预算门在投递侧间接限制 MQL |
| message_queue_data | 未显式设置（默认 on_heap）；WS 每帧 hibernate 部分对冲 |
| 二进制内存观测 | metrics 无 binary 维度（并入 observability P2-2）；事后可用 `recon:binary_leak(1)` + `instrument` 诊断 |
| atom 上限 | 默认 1,048,576，无动态造原子入口 → 风险关闭 |

**建议**（并入 observability 批次，不单列新风险）：max_heap_size 作为二轮讨论项（值需按 2MB 帧上限校准，盲设会误杀合法大帧处理），先补 binary memory 指标获得事实再决策。

## 5. Phase C 结论

Binary 与 ETS 纪律**全面优于教程检查表基线**：六类反模式零热路径命中，ETS 归属全部有主且注释把攻击面/失效语义写进代码，原子计数与 select_replace 的使用深度超出教程范围。残余为三个 P3 观察项（binary:part 抽查、login_attempt 表 heir 可选、agent_run_pg 枚举原子化），无 P1/P2 级内存风险。
