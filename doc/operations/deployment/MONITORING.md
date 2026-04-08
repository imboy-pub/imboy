# IMBoy 监控指南

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

- [ ] 检查端口是否开放：`curl http://host:9800/v1/init`
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
