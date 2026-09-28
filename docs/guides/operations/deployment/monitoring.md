# IMBoy 监控指南

---

## 告警处置卡片（owner / 止损 / 回滚）· W3-A04

监控/告警栈自身的运维三要素（业务告警的处置动作见各条告警规则的
`runbook` 注解，`deploy/prometheus/rules/imboy-alerts.yml`）：

| 要素 | 值 |
|---|---|
| **Owner** | IMBoy Ops（待用户指名，指名后替换本行） |
| **Escalation** | ① IMBoy Ops 值班 → ② 待指名平台负责人（IM/电话占位）→ ③ 监控栈持续不可用 > 30 分钟视为盲飞事件，升级为生产事故处理 |

### 止损（触发条件 + 止血动作）

- **告警风暴**（>10 条/分钟，常见于后端宕机引发 up/延迟/5xx 连锁）：
  用 amtool 按 alertname/instance 聚合静默非根因告警，保住 critical 通道信噪比：
  ```bash
  amtool silence add alertname=~"ImBoy.*" severity="warning" -d 30m \
    --comment "根因处置中，静默衍生告警"
  ```
- **监控栈自身故障**（Prometheus/Alertmanager 容器退出或 up 缺失）：
  立即重启对应容器恢复采集；**盲飞期间冻结一切生产变更**（部署/扩容/迁移），
  恢复后再解除。
- **误报确认**：单条规则连续误报 ≥2 次——先静默该 alertname（带注释与期限），
  再修阈值，不允许长期挂静默不修因。

### 回滚（监控配置变更失败）

改 rules/告警路由后 Prometheus 起不来或 `promtool check rules` 失败时
（`imboy_prometheus`/`imboy_alertmanager` 在 `docker-compose.community.yml`
的 `monitoring` profile 下）：

```bash
cd deploy
git checkout -- prometheus/rules/imboy-alerts.yml   # 还原规则文件
docker compose -f docker-compose.community.yml --profile monitoring \
  restart imboy_prometheus
promtool check rules prometheus/rules/imboy-alerts.yml   # 确认回绿再离场
```

Alertmanager 侧同理：还原 `alertmanager/alertmanager.yml`（或重跑
`render.sh` 用上次 env 渲染）后重启 `imboy_alertmanager`。

---

## 关键监控指标

### 应用层

| 指标 | 端点 | 警报阈值 |
|------|------|---------|
| WebSocket 在线用户 | `/metrics` → `imboy_online_users` | 突降 > 50% |
| WS 连接数 | `/metrics` → `ws_connections_current` | > 100K (单机) |
| 消息吞吐量 | `/metrics` → `msg_sent_total` | 突降 > 80% |
| HTTP 请求延迟 | `/metrics` → `http_request_duration` | P99 > 1s |

### 系统层

| 指标 | 端点 | 警报阈值 |
|------|------|---------|
| Erlang 进程数 | `/metrics` → `erlang_process_count` | > 500K |
| 内存使用 | `/metrics` → `erlang_memory_total_bytes` | > 12GB |
| ETS 内存 | `/metrics` → `erlang_memory_ets_bytes` | > 2GB |
| 连接池空闲 | `/metrics` → `db_pool_free` | = 0 持续 > 30s |
| 连接池使用 | `/metrics` → `db_pool_in_use` | > 70 (max=80) |

### 数据库层

| 指标 | 查询 | 警报阈值 |
|------|------|---------|
| 活跃连接 | `SELECT count(*) FROM pg_stat_activity` | > 100 |
| 慢查询 | `pg_stat_statements` | > 1s |
| 死锁 | `pg_stat_activity` WHERE `wait_event_type = 'Lock'` | > 0 |
| 磁盘使用 | `pg_database_size('imboy')` | > 80% 容量 |
| 复制延迟 | `pg_stat_replication` | > 1MB |

---

## Prometheus 采集

```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'imboy'
    scrape_interval: 15s
    metrics_path: '/metrics'
    static_configs:
      - targets: ['imboy-host:9800']
    headers:
      Accept: ['text/plain']
```

---

## Grafana Dashboard

### 推荐面板

1. **IMBoy Overview**
   - 在线用户趋势
   - WebSocket 连接数
   - 消息吞吐量
   - HTTP 延迟分布

2. **Erlang VM**
   - 进程数
   - 内存分布（total/processes/ets）
   - GC 统计

3. **PostgreSQL**
   - 连接池状态
   - 查询性能
   - 磁盘使用

---

## 故障排查 Checklist

### 用户无法连接

- [ ] 检查端口是否开放：`curl http://host:9800/api/v1/init`
- [ ] 检查 Erlang 节点状态：`_rel/imboy/bin/imboy ping`
- [ ] 检查连接池：`pooler:pool_stats(pgsql)`
- [ ] 检查 SSL 证书是否过期
- [ ] 检查防火墙规则

### 消息延迟

- [ ] 检查消息队列积压：`SELECT count(*) FROM msg_store_staging WHERE processed_at IS NULL`
- [ ] 检查 Worker 进程：`erlang:process_info(whereis(msg_store_worker))`
- [ ] 检查数据库慢查询
- [ ] 检查 CPU/内存使用

### 内存泄漏

- [ ] ETS 表大小：`ets:info(TableName, size)`
- [ ] 进程内存排序：`recon:proc_count(memory, 10)`
- [ ] 消息队列堆积：`recon:proc_count(message_queue_len, 10)`

### 数据库连接耗尽

- [ ] 检查 pg_stat_activity：`SELECT state, count(*) FROM pg_stat_activity GROUP BY state`
- [ ] 检查长事务：`SELECT pid, age(now(), xact_start) FROM pg_stat_activity WHERE state = 'active'`
- [ ] 调整 `max_count` 配置
