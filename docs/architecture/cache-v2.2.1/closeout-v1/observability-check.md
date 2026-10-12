# C11 — 缓存同步 V2.2.1 监控、诊断与文档一致性核查（19.8 节）

> 任务：CACHE_V221_BACKEND_CLOSEOUT_V1 收口 C11。
> 方式：文件级静态核查（未运行任何服务/测试/节点）。
> 核查日期：2026-10-11。
> 范围：src/lib/imboy_cache_sync*.erl、src/lib/elib_metric.erl、src/api/metrics_handler.erl、
> src/api/healthz_handler.erl、src/imboy_router.erl、deploy/{prometheus,grafana,helm}。

---

## 1. sync 状态可观测（epoch / incarnation / status / peer-lag）

逐项结论（✓ = 已具备且可从代码定位；△ = 有存储与 Erlang API 但无任何导出通道；✗ = 不存在）：

| 项 | 结论 | 代码位置 |
|---|---|---|
| 本节点 sync epoch | △ 存储+API，无导出 | `src/lib/imboy_cache_sync_proto.erl:290-296`（`epoch/0` 读 `sync_epoch`）、`:298-301`（`bump_epoch/0`）；init 初值写入 `src/lib/imboy_cache_sync.erl:239` |
| peer epoch（per node） | △ 存储+API，无导出 | 写入 `src/lib/imboy_cache_sync.erl:689`（`{{peer_epoch, FromNode}, Epoch}`）；读取 `src/lib/imboy_cache_sync_proto.erl:305-310`（`peer_epoch/1`）；冷恢复取 max 见 `imboy_cache_sync.erl:806-811` |
| node_boot（本节点 incarnation） | △ 存储+API，无导出 | 写入 `src/lib/imboy_cache_sync.erl:237`（`{node_boot, {node(), BootId}}`）；API `node_boot/0` 于 `:172-178` |
| peer_incarnation | △ 存储+API，无导出 | `src/lib/imboy_cache_sync_proto.erl:217-231`（`register_incarnation/2`，写 `{peer_incarnation, Origin}`）；变化检测 `:236-242`（`incarnation_changed/2`）；宽限淘汰 `:245-256`（`prune_grace/1`） |
| status（starting/discovering/recovering/ready/degraded） | △ 存储+API，无导出 | API `status/0` 于 `src/lib/imboy_cache_sync.erl:149-161`（读 ETS `status`）；状态写入点：`:238`（discovering）、`:648`（recovering）、`:656`（degraded）、`:804`（ready） |
| peer-lag（peer_epoch/watermark 派生差值） | ✗ 不存在 | 原料齐备：peer_epoch per-node（`imboy_cache_sync.erl:689`）+ 版本水位 `sync_counter`（`:240` init、`:760` 快照水位合并、`:1112/:1117` 时钟合并）；但全仓无 lag 计算点，也无导出 |
| 队列深度（19.8 "队列"项） | △ API，无导出 | `queue_depth/0` 于 `src/lib/imboy_cache_sync.erl:185-197`；`overloaded/0` 于 `:180-183` |
| config/registry hash 一致性（19.8 项） | △ 标记+API，无导出 | divergent 标记 `src/lib/imboy_cache_sync_proto.erl:266-279`（`check_hashes/2` 写 `{hash_divergent, Origin}`）；查询 `:281-284`（`hash_divergent/1`） |
| protocol_version（19.8 项） | ✗ 无指标化 | 配置存在（`imboy_cache_policy:protocol/0`，`publish_invalidate/1` 于 `imboy_cache_sync.erl:1042` 消费），但无任何可观测输出 |

**小结**：状态机五态、epoch、incarnation、hash 标记在进程内 ETS 均有真实维护，Erlang API（`status/0`、`node_boot/0`、`meta_snapshot/0`、`queue_depth/0`）齐备——但**全部止步于进程内**。`src/api/` 全目录 grep 无一处引用 `imboy_cache_sync`；`/readyz`（`src/api/healthz_handler.erl:59-99`）聚合 PG+TSID，不含 cache sync 状态；`/metrics`（见第 3 节）不含任何 cache sync gauge。即：第 1 组在"进程内可观测"层面 ✓，在"监控可观测"层面 ✗。

## 2. 新增计数器写入点（imboy_cache_sync.erl incr/1 体系）

`incr/1` 定义于 `src/lib/imboy_cache_sync.erl:1200-1203`（转发 `elib_metric:increment/1`）。六个要求计数器全部有真实写入点：

| 计数器 | 结论 | 写入点（file:line） |
|---|---|---|
| snapshot_abort_total | ✓ | `src/lib/imboy_cache_sync.erl:768`（`abort_round/2`，快照回合失败：deadline 过期/页 digest 不符/页序悬挂） |
| cold_recovery_total | ✓ | `src/lib/imboy_cache_sync.erl:930`（`controlled_cold_recovery/0`，墓碑预算满受控清理） |
| reject_total | ✓ | `src/lib/imboy_cache_sync.erl:284`、`:293`（publish 前置失败/墓碑预算满）、`:313`（publish_delete 前置失败）、`:403`（迟到快照页）、`:469`（V2 双端一致性拒绝）、`:562`（Nmax 预算满拒收） |
| degraded_total | ✓ | `src/lib/imboy_cache_sync.erl:655`（`enter_degraded/1`，Dmax 耗尽/回合超限进入 DEGRADED） |
| reconcile_total | ✓ | `src/lib/imboy_cache_sync.erl:370`（应答快照请求）、`:822`（`periodic_reconcile/1` 定期校对，60s 周期） |
| receive_total | ✓ | `src/lib/imboy_cache_sync.erl:475`、`:479`、`:485`（legacy set/flush/全量 flush 三路径）、`:524`（V2 事件通过校验后） |

体系内其余计数器（同链路，一并列出供对账）：`publish_total`（`:276`、`:304`、`:1044`）、`unknown_total`（`:491`）、`expired_total`（`:566`）、`conflict_total`（`:573`、`:600`）、`gap_detected_total`（`:950`）。快照/协议/策略模块（`imboy_cache_sync_snapshot.erl`、`imboy_cache_sync_proto.erl`、`imboy_cache_policy.erl`、`imboy_cache.erl`）grep 无任何 `incr`/`elib_metric` 调用，计数全部收敛在 `imboy_cache_sync.erl` 单点。

**小结：第 2 组 6/6 ✓，且每个写入点都在对应业务分支的真实执行路径上（非死代码）。**

## 3. 指标导出路径（/metrics 链路逐环）

链路（计数器部分）：

| 环节 | 结论 | 位置 |
|---|---|---|
| ① 计数存储 | ✓ | `incr/1` → `elib_metric:increment/1`（`src/lib/imboy_cache_sync.erl:1200-1203`）→ gen_server cast → ETS `elib_metrics_ets` 键 `{counter, Name}`（`src/lib/elib_metric.erl:232-240`） |
| ② 导出格式化 | ✓ | `src/api/metrics_handler.erl:84-94`（`fetch_metrics/0` → `elib_metric:get_all_metrics/0`，全部计数器含 cache sync 各项自动并入）；Prometheus 文本 `format_prometheus/1`（`:216-259`，atom 名原样输出，如 `reject_total`）；JSON 分支 `:37-50` |
| ③ 路由注册 | ✓ | `src/imboy_router.erl:32`（`{"/metrics", metrics_handler, #{}}`）、`:76`（`/api/v1/metrics`）；端点限内网 IP（`metrics_handler.erl:17-23` + `is_internal_ip/1` `:74-81`），Prometheus 同 compose 网络抓取不受影响 |
| ④ 抓取配置 | ✓ | `deploy/prometheus/prometheus.yml:28-36`（job `imboy_backend`，`metrics_path: /metrics`，target `imboy_backend:9800`）；`rule_files: /etc/prometheus/rules/*.yml`（`:17-18`） |

**计数器链路 ①→④ 全通，无断链**。导出名与 deploy 侧引用逐字一致（`reject_total` 等无前缀原样，与 `imboy-cache-sync.json`、`imboy-alerts.yml` 引用对账吻合）。

**断链与缺口（状态类指标）**：

- **断链点**：`metrics_handler:collect_system_metrics/0`（`src/api/metrics_handler.erl:97-167`）产出 VM/连接池/License/TSID 等 gauge，**不含任何 cache sync 项**。第 1 节全部状态（status/epoch/incarnation/peer-lag/queue_depth/hash_divergent）在 ② 环节被丢弃——即"计数可导出、状态不可导出"。
- 链路特性（非断链，须知）：计数器 ETS 惰性创建（`elib_metric.erl:232-240`），**零事件计数器不出现在 /metrics 输出**。`snapshot_abort_total`/`cold_recovery_total`/`degraded_total` 在首次事件前面板显示 No data 而非 0；告警用 `increase(...)>0` 语义不受影响（缺失序列不触发）。
- 19.8 口径细分缺口：`receive_total`/`publish_total` 未按 protocol（legacy/v2）拆标签（legacy 三路径 `:475/:479/:485` 与 V2 `:524` 混入同名计数器）；`reject_total` 无 reason 标签细分（expired 已独立为 `expired_total`，其余 hash/stale/budget 类聚合）。

## 4. deploy/ Grafana dashboard 与 alert rules

| 资产 | 存在 | 路径 |
|---|---|---|
| 缓存同步专板 | ✓ | `deploy/grafana/dashboards/imboy-cache-sync.json`（uid `imboy-cache-sync`，7 面板，provisioning 自动发现） |
| 告警规则组 | ✓ | `deploy/prometheus/rules/imboy-alerts.yml` group 19 `imboy.cache_sync`（`:1319-1378`） |
| 挂载链 | ✓ | `deploy/docker-compose.prod.yml:385-386`（prometheus rules 挂载）、`:506-507`（grafana dashboards/provisioning 挂载） |

面板覆盖（专板 expr 全集）：`publish_total`、`receive_total`、`reject_total`、`unknown_total`（4 个 stat + 2 个 timeseries）、`reconcile_total`（timeseries）。
告警覆盖：`ImBoyCacheSyncRejected`（`reject_total`，yml:1334）、`ImBoyCacheSyncUnknownEvents`（`unknown_total`，yml:1359）。告警组头注释（yml:1322-1325）与实际覆盖一致，且明确了"正常流量计数器只做面板不做告警"的设计决策。

关键信号覆盖核查（任务指定三项）：

| 关键信号 | 面板 | 告警 | 结论 |
|---|---|---|---|
| snapshot_abort | ✗ 无面板 | ✗ 无告警 | 缺口（计数器已导出，纯 deploy 配置缺口） |
| degraded | ✗ 无面板 | ✗ 无告警 | 缺口（`degraded_total`/`cold_recovery_total` 已导出无消费；status gauge 因第 3 节断链更不存在） |
| status 卡 recovering | ✗ 无面板 | ✗ 无告警 | **双重缺口**：无 status gauge 导出（前置）+ 无面板/告警（后置） |
| （补充）cold_recovery / conflict / expired / gap_detected | ✗ | ✗ | 均已计数已导出、无消费 |

次要缺陷：专板 4 个 stat 面板 `"id"` 均为 1（`imboy-cache-sync.json` 重复 panel id，Grafana 导入时会告警并可能渲染异常）。`deploy/helm/templates/` 无 Prometheus/Grafana 资源，监控栈仅随 docker compose 部署（部署形态差异，注记不列为缺口）。

---

## 缺口清单与建议动作

### A. 代码已备，只差面板/告警（仅改 deploy/ 配置即可收口）

1. **补三个关键告警**（进 `imboy.cache_sync` 组，计数器已导出）：
   - `degraded_total` increase > 0 → critical（fail-closed 已生效，读 unverified/写拒绝，退出手段=重启节点——最关键的单条）；
   - `cold_recovery_total` increase > 0 → warning（受管复制副本被丢弃，后续依赖快照回填）；
   - `snapshot_abort_total` 短窗 increase（如 30m > 3）→ warning（回合反复失败 = 逼近 Dmax/DEGRADED 的前兆）。
2. **专板补面板**：`snapshot_abort_total`、`cold_recovery_total`、`degraded_total`、`conflict_total`、`expired_total`、`gap_detected_total`（事件计数，stat/timeseries 均可）。
3. **修复专板重复 panel id**（4 个 stat 面板 id 全为 1）。

### B. 需要后续提交补（涉及 src 代码变更）

4. **status gauge 导出**（B 类中优先级最高，是"卡 recovering"告警的前置）：`metrics_handler:collect_system_metrics/0` 增加 `imboy_cache_sync_status{state=...}` 数值化 gauge（消费现成 `imboy_cache_sync:status/0`）与 `imboy_cache_sync_epoch`、`imboy_cache_sync_queue_depth`；随后补"status 卡 recovering 超 N 分钟"告警。
5. **peer-lag 派生指标**：本节点 `sync_counter` 与各 peer 上界差值的计算与导出（原料已在 ETS，缺计算与导出两环）。19.8 明确要求。
6. 19.8 口径细分：`receive_total`/`publish_total` 加 protocol（legacy/v2）标签；`reject_total` 加 reason 标签；`incarnation` 变化计数化（现仅有 `gap_detected_total` 间接近似）；`hash_divergent` 状态导出。

### C. 无需动作

7. 六个指定计数器的写入点与导出全链路（存储→格式化→路由→抓取）已通，名字与 deploy 引用逐字一致。
8. 部署挂载链（compose rules/dashboards 挂载 + provisioning 自动发现 + prometheus 抓取 job）完整。
9. `reject_total`/`unknown_total` 面板+告警已覆盖；reconcile 面板已有。
10. 零事件计数器不导出属 Prometheus 惯例（increase 语义不受影响），不建议为此在 init 预热计数器。
11. `/readyz` 不含 cache sync 状态：19.8 要求的是"可观测"而非"影响探活"，维持现状合理（DEGRADED 不应摘除流量探针——节点活着只是缓存降级）。

### 缺口计数

- A 类（只差 deploy 配置）：3 项（告警 3 条 + 面板 1 组 + panel id 修复，对应上方 1-3）。
- B 类（需代码提交）：3 项（status/epoch/queue gauge 导出、peer-lag、19.8 细分标签，对应上方 4-6）。
- C 类：无需动作 5 项。

**四组一句话结论**：① 状态可观测——进程内 ✓、监控导出 ✗（仅 ETS+API，无任何 HTTP 导出通道）；② 六计数器写入点 6/6 ✓；③ 计数器导出链路四环全通 ✓（状态类指标在 ② 环节断链）；④ 专板与告警组存在 ✓，但 snapshot_abort/degraded/卡 recovering 三关键信号面板告警双缺（共 3 项 A 类 + 3 项 B 类缺口）。
