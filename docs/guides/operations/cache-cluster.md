# 缓存集群运维指南（cache-cluster）

> 适用：IMBoy 分布式缓存 V2 同步子系统（fenced cache sync）。
> 架构与指标语义详见 `docs/architecture/imboy-distributed-cache-v2.md`；
> 本篇面向值班与运维：观测、告警处置、常见故障、诊断工具、回滚入口。

---

## 1. 集群形态与前提

- 成员管理：syn pg（scope `imboy_cache`，group `dsync_handler`）；节点加入/离开由
  syn 维护，同步进程随 `dsync_enabled=true` 启动。
- 节点互通依赖标准 Erlang 分布式（epmd + cookie）；跨节点握手在 syn 之上另有
  r2 版本握手（pid 直连），syn 视图滞后不致恢复饥饿。
- **复制默认关闭**：`cache_replication.whitelist` 为空时任何键都不会跨节点复制。
  开启白名单前必须确认目标键已在注册表登记 `{replicate, false}`（fail-closed）。

### 1.1 可重建数据的显式 SET 接口 / Opt-in reconstructible SET

`{shared_cache, Key}` 已登记为非敏感复制命名空间，默认白名单仍为空，**不会自动启用**。
只存丢失后可以重新计算或回源的数据；禁止用于权限、令牌、计数器、锁、资金、
幂等凭据或二维码一次性消费。既有 local/invalidate/QR 分类和权威选择不变。

通过已有 V2 部署前置检查（包括 QR 权威/消费门禁）后，所有参与节点必须使用
相同版本、注册表、白名单和预算。由部署负责人显式将 `shared_cache` 加入
`cache_replication.whitelist`；使用 V2 协议，不需要 `cache_registry_overrides`。
本次新增注册表条目会改变 `registry_hash`，即使白名单为空亦然；旧新节点混合时
版本化帧会拒绝，不能把它当成无中断滚动升级。协调升级/重启，确认 READY 后再放量。

```erlang
%% Configuration excerpt, only after existing deployment prerequisites are met:
{cache_sync_protocol, v2}.
{cache_replication, #{whitelist => [shared_cache], max_ttl_sec => 600}}.

%% Application usage (Value must be non-sensitive and reconstructible):
imboy_cache:put({shared_cache, <<"search-result:example">>}, Value, 60, replicate).
imboy_cache:get({shared_cache, <<"search-result:example">>}).
imboy_cache:put_delete({shared_cache, <<"search-result:example">>}).
```

写入成功仅表示本节点接受并发起复制，**不表示所有节点已确认**。并发写入按现有
revision（逻辑计数器、节点标识）全序收敛，并非按真实墙钟时间决定最后写入；
不提供事务或线性一致读。读取与版本变更相遇时可安全返回 miss。
分区重连后普通值可能继续过时，正常周期对账仍为 60 秒。READY 节点收到 nodeup
时，仅将现有对账定时器提前至约 1 秒，不延后已排队/更早的定时器；同机 2/3 节点
实测无新写入、无人工触发时从约 50 秒降到约 1 秒。syn 成员视图延迟时仍依赖
60 秒周期兜底；不能视为跨机器网络的 1 秒 SLA。持续节点抖动最多约每秒增加一次对账。
分区、恢复、过期、容量耗尽也会造成 miss 或明确写入错误，调用方应回源/重算并处理
`not_ready`、`incompatible_peers`、`overloaded`、`index_full` 等错误，不无限重试。
SET 有有限 TTL，删除保留版本墓碑；全节点重启不保留数据。接收节点保守扣除
5 秒时钟偏差余量，所以 TTL ≤ 5 秒的值可能仅本地可见；分布式用途应选择明显大于
5 秒的 TTL（还会向下取整到秒，须预留传输/恢复时间），不能将所有节点的过期时刻视为完全相同。
部署需维持节点时钟同步；超出此偏差预算时的 TTL 语义不在保证范围内。故障修复与收敛能力
仍受现有协议和已验证场景限制，不能承诺任意分区时长下的完整跨来源切点。

Only explicitly whitelisted, non-sensitive, reconstructible values belong here. A successful
write is local acceptance, not cluster-wide acknowledgement. Concurrent reads may miss;
partitions/recovery can reject writes. Never use this cache for authorization or atomic consume.
The registry change requires coordinated version/configuration rollout; no production setting
is enabled automatically. Rollback first stops new shared-cache writers, removes their whitelist
entry consistently, and restores the prior coordinated release/configuration; no data migration
is needed for these disposable values. Keep QR authority/schema rollback separate.

## 2. 日常观测

入口 `/metrics`（仅内网可达，nginx 对外 403）。核心读法：

| 现象 | 判读 |
|---|---|
| `imboy_cache_sync_status = 2` | 正常 |
| `= 1` | 恢复中（伴随恢复轮重试；持续 >5 分钟触发 StuckRecovering 告警） |
| `<= 0` | 降级（0）/ 同步进程不存在（-1），立即按 §3.2 处置 |
| `imboy_cache_sync_queue_depth` 持续增长 | 消费跟不上生产；先看 CPU/负载，再看是否有对端失联重试风暴 |
| `imboy_cache_sync_peers` 突降 | 有节点掉线；对照节点存活与 syn 日志 |
| `imboy_cache_sync_peer_counter{peer}` 停滞 | 该对端握手/快照通道异常，单对端问题优先排查网络分区 |

## 3. 告警处置（`deploy/prometheus/rules/imboy-alerts.yml`）

### 3.1 ImBoyCacheSyncSnapshotAborting（warning）

10 分钟内快照 abort>2 次。多为瞬时分页/摘要异常。
处置：看 Grafana `imboy-cache-sync` 面板 abort 曲线与当时网络事件；偶发可观察，
频发进入 §4.1。

### 3.2 ImBoyCacheSyncStuckRecovering（critical）

status==1 持续 5 分钟：恢复轮耗尽路径（`RECOVER_ROUND_MS=5000` × `RECOVER_MAX_RETRY=3`
反复失败）。
处置顺序：① 确认对端节点存活（`imboy_cache_sync_peers`）；② 若对端在但水位
（peer_counter）不推进，按 §4.2 分区排查；③ 短暂 status=1 属正常恢复，勿重启节点。

### 3.3 ImBoyCacheSyncDegraded（critical）

status<=0。0=降级，-1=进程不在。处置：先查进程是否因 `dsync_enabled` 被关或启动
崩溃（`imboy_ctl` / 日志）；未有人为变更且 -1 持续 = 同步进程崩溃，拉 crash dump
（参照 `scripts/erl_crashdump_analyzer`）并按事件流程上报。

### 3.4 ImBoyCacheSyncRejected / ImBoyCacheSyncUnknownEvents

多为集群间 **config_hash/registry_hash 不一致**（配置滚动窗口内正常，收敛后自愈）或
epoch 漂移。处置：核对各节点 `cache_fenced_namespaces`、`cache_replication`、预算
三项是否一致；滚动发布期间出现属预期，发布完成后仍持续则配置漂移，按 §5 校准。

## 4. 诊断工具（勿在生产节点常驻运行）

工具均需 `make compile` 后以独立节点名/cookie/端口租约运行，输出 JSON 证据。

```bash
# 三节点故障注入 dcheck（C9a–f：分区重连/化身变更/nmax 上限等）
erl -pa ebin -pa deps/*/ebin -noshell -eval 'cache_v21_dcheck:main(["/tmp/dcheck.json"]).' -s init stop
# DCHECK_ONLY=c9e 只跑单个场景（诊断用；证据轮禁用）

# 混合故障轮（M7–M10：页摘要不符/epoch 漂移/旧化身重放/r2 帧）
erl -pa ebin -pa deps/*/ebin -noshell -eval 'cache_v21_mixed_dcheck:main(["/tmp/mixed.json"]).' -s init stop
```

- 输出 JSON 顶层含 `candidate_sha256` 与 `utc`，可直接被
  `scripts/check_cache_closeout_v1.py` 校验。
- 43181213 之后两驱动均要求 `CANDIDATE_SHA256` 环境变量绑定候选。

## 5. 配置变更注意

1. 改 `cache_fenced_namespaces` / `cache_replication` / 预算 任一项都会改变
   config_hash：**滚动窗口内** Rejected 告警属预期；一次只滚一个节点，滚完确认
   告警自清再继续。
2. 新增复制键：先注册表 `{replicate, false}` 登记 → 再进白名单；顺序颠倒会被
   fail-closed 拒绝（这是设计行为，不是故障）。
3. 白名单只增不删时无需快照；大规模启用建议安排低峰并观察
   `imboy_cache_sync_queue_depth`。

## 6. 回滚入口

完整回滚步骤：`docs/architecture/cache-v2.2.1/closeout-v1/rollback-v1.md`。
两条硬约束：

- G-QR 单特性回滚必须**先执行 down 迁移、再回退代码**（迁移与代码序不可倒置）。
- 回退不修改 `erlang.mk`（vendored）与 `imboyapp/`、`imboyadmin/`（本子系统对
  QR 客户端 wire contract 无影响，回滚仅涉及后端）。

## 7. 红线

- 不在生产节点手工注入测试键（未登记键会被 fail-closed 拒绝；登记了又会污染
  config_hash）。诊断用键仅限 `cache_v2_test_rep` 且只在 dcheck 专属租约环境。
- 不直连原始缓存 URL 绕过授权视图（前端规则：附件必须经 `AssetsService.viewUrl`）。
- 指标/告警表达式变更须同步 `deploy/grafana/dashboards/imboy-cache-sync.json`。
