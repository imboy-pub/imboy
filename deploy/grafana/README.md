# IMBoy Grafana 配置包

开箱即用的 Grafana + Prometheus 配置，配套 `deploy/docker-compose.prod.yml` 可观测性栈。

## 目录结构

```
deploy/grafana/
├── dashboards/
│   └── imboy-overview.json         # 总览 dashboard（9 个 panel）
├── provisioning/
│   ├── datasources/
│   │   └── prometheus.yml          # 自动注入 Prometheus 数据源
│   └── dashboards/
│       └── default.yml             # 自动发现 dashboards/ 目录
└── README.md                       # 本文件

deploy/prometheus/
└── prometheus.yml                  # 抓取配置（imboy_backend / pg / node / caddy）
```

## 覆盖指标

| Panel | PromQL | 来源 |
|---|---|---|
| Backend Up | `up{job="imboy_backend"}` | Prometheus 自检 |
| WebSocket 在线连接 | `imboy_ws_connections_total` | 后端业务计数器 |
| Backend Uptime | `process_uptime_seconds` | prometheus_process_collector |
| HTTP 请求速率 | `rate(imboy_http_requests_total[1m])` | 后端 cowboy middleware |
| 消息投递延迟 p50/p95/p99 | `histogram_quantile(..., imboy_msg_deliver_duration_seconds_bucket)` | 后端业务 histogram |
| 消息发送速率 | `rate(imboy_msg_sent_total[1m])` | 按 C2C/C2G/C2S 分类 |
| Erlang VM 内存 | `erlang_vm_memory_bytes_total` | prometheus_erlang_collector |
| Erlang 进程/端口计数 | `erlang_vm_process_count` / `erlang_vm_port_count` | 同上 |
| PG 事务速率 | `pg_stat_database_xact_commit/rollback` | postgres_exporter |

## 启用方式（推荐：扩展 docker-compose.prod.yml）

在 `docker-compose.prod.yml` 追加以下 service（与现有栈共享 `imboy-network`）：

```yaml
  prometheus:
    image: prom/prometheus:latest
    container_name: imboy_prometheus
    restart: unless-stopped
    volumes:
      - ./prometheus/prometheus.yml:/etc/prometheus/prometheus.yml:ro
      - ./data/prometheus:/prometheus
    command:
      - --config.file=/etc/prometheus/prometheus.yml
      - --storage.tsdb.retention.time=30d
    networks:
      - imboy-network

  grafana:
    image: grafana/grafana:latest
    container_name: imboy_grafana
    restart: unless-stopped
    environment:
      GF_SECURITY_ADMIN_PASSWORD: ${GRAFANA_ADMIN_PASSWORD:-changeme}
      GF_USERS_ALLOW_SIGN_UP: "false"
    volumes:
      - ./grafana/provisioning:/etc/grafana/provisioning:ro
      - ./grafana/dashboards:/var/lib/grafana/dashboards:ro
      - ./data/grafana:/var/lib/grafana
    networks:
      - imboy-network
    ports:
      - "3000:3000"
```

**暴露方式**：生产环境建议在 Caddyfile 增加 `grafana.example.com` 反向代理到 `grafana:3000`，而非直接映射 `3000:3000` 到公网。

## 手动导入

若不使用 provisioning，可在 Grafana UI 中：

1. 创建 Prometheus 数据源，URL `http://prometheus:9090`
2. Dashboards → Import → 上传 `dashboards/imboy-overview.json`

## 后端指标对接

IMBoy 后端需暴露 `/metrics` 端点（Prometheus 文本格式）。当前实现位置：
- `imboy/src/api/` 中的 metrics handler（如缺失，参考 `prometheus_cowboy` 集成）
- 业务指标注册：`imboy/src/lib/imboy_metrics.erl`

若指标名称与本 dashboard 不匹配，修改 `dashboards/imboy-overview.json` 中的 `expr` 字段即可。

## 告警

本 dashboard **不附带告警规则**。建议在 Prometheus `rules.yml` 中单独维护告警，例如：

- `up{job="imboy_backend"} == 0` 持续 1 分钟
- 消息投递 p95 > 500ms 持续 5 分钟
- Erlang 进程数 > 预期基线 2 倍

告警规则文件参见 1.0.0 GA 里程碑（S3 扩展项）。
