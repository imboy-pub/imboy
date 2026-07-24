# Ponytail 债务账本 — imboy

> 由 `/ponytail-debt` 扫描生成 · 2026-07-23
> 每个 `ponytail:` 标记 = 一条刻意简化,记录其上限(ceiling)与重访触发条件(upgrade)。
> `no-trigger` = 注释只述取舍、未给触发条件,是最易悄悄腐化的项。
> 只读扫描产物,不改代码。

**32 markers, 14 with no trigger.**

## test/repo/msg_store_jsonb_roundtrip_tests.erl
- `:39` 密文全为数字字符的 jsonb 往返残留边界。ceiling: 极小概率全数字密文。upgrade: 命中该边界(续行条件)。
- `:50` 密文整体等于 JSON 字面量的残留边界。ceiling: 极小概率。upgrade: 续行条件。

## src/ds/
- `channel_webhook_ds.erl:27` 四步非单事务(镜像 ai_agent_ds:create 取舍)。ceiling: 中途失败留残留。upgrade: 续行(补偿路径)。
- `ai_agent_ds.erl:31` 三步非单事务(user_repo 走 auto-commit,非 with_tx Conn)。ceiling: 非原子。upgrade: 续行。
- `moderation_ds.erl:58` 逗号分隔存敏感词。ceiling: 词本身不含逗号。upgrade: 需支持含逗号词 → 改 jsonb 列。
- `channel_ds.erl:96` `[] → DB default '[]'::jsonb`。ceiling: 空转默认。upgrade: **无** · `no-trigger`
- `msg_c2c_ds.erl:336` 1/20 采样避免每写必 COUNT。ceiling: 溢出最多延迟 ~20 条被清理。upgrade: **无** · `no-trigger`

## src/logic/
- `msg_c2s_logic.erl:140` 失败重试重推 delta,前端按 index 幂等覆盖。ceiling: 可容忍重推。upgrade: **无** · `no-trigger`
- `msg_forward_logic.erl:228` c2c reply 是发给 caller 的 WS 帧,本层 treat as ok。upgrade: **无** · `no-trigger`
- `msg_forward_logic.erl:232` c2g error 走 self()!{reply,...},返回恒 ok。upgrade: **无** · `no-trigger`
- `group_task_logic.erl:545` 用 millisecond() 对齐 rfc3339_to 的 integer(ms) 类型。upgrade: **无** · `no-trigger`
- `billing_meter.erl:16` 单租户硬取 tenant_id=0。ceiling: 单租户。upgrade: 真多租户 → 换 uid→tenant→sub 映射。
- `msg_c2g_logic.erl:540` guard integer 避免 CreatedAt 空/非法时 badarith。upgrade: **无** · `no-trigger`
- `msg_pinned_logic.erl:76` 任何群成员可置顶/取消置顶。ceiling: 无权限限制。upgrade: 管理员限制留给产品层。
- `ai_agent_runtime.erl:11` 投递到 agent 的消息帧本进程丢弃(无 client ACK)。ceiling: 无 ACK。upgrade: 续行(走 QoS 重试后)。
- `ai_agent_runtime.erl:14` 靠周期 refresh 兜底新增/状态变更(守 DS→上层单向依赖)。ceiling: 周期 refresh。upgrade: 续行。
- `user_device_logic.erl:84` token 为 uid 级非设备级。ceiling: 无法按设备 revoke。upgrade: 续行。
- `ai_agent_proactive.erl:141` 与 ai_agent_reply:merge_model/2 同形(5 行)各自私有。ceiling: 5 行重复避免跨模块导出。upgrade: **无** · `no-trigger`
- `mcp_governance_logic.erl:38` 每次 find_by_owner 查 DB。ceiling: 无缓存。upgrade: 真成热点 → 加 (owner_uid→client) depcache。
- `agent_task_observer.erl:283` PoC 审批记录常驻 ETS。ceiling: 无 TTL 清扫。upgrade: 生产化 → 加 sweep 或落库。
- `msg_c2c_logic.erl:78` maybe_dispatch 对每条非 E2EE 文本 C2C 多做一次 ai_agent 主键查。ceiling: 每消息一次多查。upgrade: 续行。
- `agent_payment_logic.erl:120` try_reserve 与结算非同一 DB 事务。ceiling: 两步补偿(release/2)。upgrade: 续行(进程/节点故障)。
- `channel_logic_notify.erl:156` 阈值门控写扩散,枚举 SubscriberUids O(N)。ceiling: O(N) 枚举。upgrade: 续行。

## src/lib/
- `imboy_plugin_lifecycle.erl:326` `encode map→jsonb`。upgrade: **无** · `no-trigger`
- `llm_stream.erl:27` 只做字节阈值节流。ceiling: 无时间节流。upgrade: 真有体感问题 → 加时间节流(M ms)。
- `agent_rate_limiter.erl:21` 单节点计数(各节点独立,够金钱 DoS 兜底)。ceiling: 分钟级速率闸门。upgrade: 续行。

## src/api/
- `channel_handler.erl:489` Bin0 恒 binary,跳 normalize 避免 dead-pattern 告警。upgrade: **无** · `no-trigger`

## src/repo/
- `msg_c2c_repo.erl:541` NOW() 避免 epgsql 对 RFC3339 binary 的 timestamptz 转换。upgrade: **无** · `no-trigger`
- `channel_order_repo.erl:60` encode map/list→jsonb,guard 未来调用方。upgrade: **无** · `no-trigger`
- `mcp_client_repo.erl:120` 仅 status 等值过滤。ceiling: keyword 模糊匹配未下推。upgrade: 后续真需要 → 下推 SQL。
- `ai_agent_repo.erl:110` 内联 LIMIT/OFFSET(整数来自已校验分页)。ceiling: 内联安全。upgrade: **无** · `no-trigger`
- `ai_agent_repo.erl:145` 内联 LIMIT/OFFSET + keyword 参数化 $1 防注入。ceiling: 内联安全。upgrade: **无** · `no-trigger`

---

## 腐化风险优先级(14 个 no-trigger 中值得补触发条件的)

多数 `no-trigger` 是良性一行编码注释(`encode map→jsonb`、`[] → default`、类型对齐、dead-pattern 规避),无需处理。真正建议补"何时重访"的:

1. `msg_c2c_ds.erl:336` — 1/20 采样溢出延迟 ~20 条:写量级上涨时该重访采样率或改精确清理。
2. `msg_c2s_logic.erl:140` — 重推 delta 依赖前端 index 幂等:前端幂等契约一变即咬人,应写明该契约为前提。
3. `ai_agent_proactive.erl:141` — 5 行 DRY 债:出现第 3 处复制时应抽公共模块。
