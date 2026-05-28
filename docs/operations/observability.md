# 可观测性 / Observability

> 版本：1.0.0-rc.1
> 适用范围：workspace 级（imboy 后端 / imboyapp 客户端 / imboy-admin-frontend 管理后台）

本文说明 IMBoy 在生产环境下的可观测性能力：**日志、健康检查、指标、错误追踪（Sentry）**。目标是让运维方在不阅读源码的前提下定位故障。

---

## 1. 三支柱概览 / Three Pillars

| 支柱 | 当前状态 | 后续规划 |
|---|---|---|
| **Logs** | ✅ 已具备：`imboy/log/*.log`（lager + OTP logger） | 1.1 结构化 JSON 输出 |
| **Metrics** | ⚠️ 部分：PG `pg_stat_*`、Erlang VM observer | 1.1 Prometheus exporter |
| **Traces / Errors** | ⚠️ 预留：`SENTRY_DSN` 环境变量已贯通，SDK 未默认开启 | 1.0 正式版默认启用 |
| **Healthcheck** | ✅ 已具备：`GET /healthcheck`（HTTP 200 即健康） | — |

---

## 2. 日志 / Logs

### 2.1 位置

| 组件 | 路径 | 说明 |
|---|---|---|
| imboy 后端（Docker） | 容器内 `/app/imboy/log/` → 宿主映射 `./log/imboy/` | Erlang lager 按级别分文件 |
| imboy 后端（裸机） | `$IMBOY_HOME/log/` | 同上 |
| Caddy 反代 | `docker logs imboy_caddy` | JSON 访问日志 |
| PostgreSQL | `docker logs imboy_pg18` | 慢查询与错误 |

### 2.2 级别

默认 `info`。生产排障时可临时调整：

```erlang
%% Remote shell 进入后端
logger:set_primary_config(level, debug).
```

完成后务必调回 `info`，`debug` 级别会显著拖慢系统并产生 GB 级日志。

### 2.3 轮转

裸机部署建议配合 `logrotate`：

```
/var/log/imboy/*.log {
    daily
    rotate 14
    compress
    missingok
    notifempty
    copytruncate
}
```

Docker 部署由 `docker-compose.prod.yml` 限制单文件大小：

```yaml
logging:
  driver: "json-file"
  options:
    max-size: "100m"
    max-file: "5"
```

---

## 3. 健康检查 / Healthcheck

### 3.1 后端

```bash
curl -fsS https://$API_DOMAIN/healthcheck
# → 200 OK（body 可忽略）
```

Caddy / 负载均衡探针应使用此端点，超时 3s、间隔 10s。

### 3.2 数据库

`docker-compose.prod.yml` 内置 PG healthcheck：

```yaml
healthcheck:
  test: ["CMD-SHELL", "pg_isready -U imboy"]
  interval: 10s
  timeout: 5s
  retries: 5
```

`imboy_backend` 通过 `depends_on: { imboy_pg18: { condition: service_healthy } }` 确保数据库就绪后才启动。

---

## 4. 错误追踪 / Sentry

### 4.1 设计

IMBoy 预留三端统一的 `SENTRY_DSN` 注入点：

| 组件 | 注入方式 | 代码位置（规划） |
|---|---|---|
| imboy 后端 | `SENTRY_DSN` 环境变量 → `sys.pro.config` | 计划接入 [`raven_erlang`](https://github.com/artemeff/eraven) 或 `sentry-erlang` |
| imboyapp 客户端 | `--dart-define=SENTRY_DSN=...` 构建期注入 | `sentry_flutter` |
| imboy-admin-frontend | Vite `VITE_SENTRY_DSN` | `@sentry/vue` |

### 4.2 启用步骤

**1. 申请 DSN**

登录 [sentry.io](https://sentry.io) 或私有部署 Sentry，创建三个 project：

- `imboy-backend`（Platform: Erlang / Other）
- `imboy-app`（Platform: Flutter）
- `imboy-admin`（Platform: Vue）

**2. 写入 `deploy/.env`**

```bash
# deploy/.env（不进 git）
SENTRY_DSN=https://xxxxx@o123456.ingest.sentry.io/7890
SENTRY_ENVIRONMENT=production
SENTRY_RELEASE=imboy@1.0.0-rc.1
```

**3. 重启后端**

```bash
cd deploy
docker compose -f docker-compose.prod.yml up -d imboy_backend
```

**4. 验证事件投递**

触发一次已知错误路径，在 Sentry 控制台确认事件出现。

### 4.3 脱敏规范

**绝对禁止**上送到 Sentry 的字段：

- JWT / refresh token
- `password` / `pin` / `private_key`
- 消息明文 payload（E2EE 场景下本就是密文，但仍应排除）
- 用户手机号 / 邮箱全量值（可截断为 `138****1234`）

后端接入时应在 SDK 的 `before_send` hook 中统一过滤。

### 4.4 采样率

| 环境 | error_sample_rate | traces_sample_rate |
|---|---|---|
| production | 1.0 | 0.05 |
| staging | 1.0 | 0.5 |
| dev | 关闭 | 关闭 |

`traces_sample_rate` 在 1.0.0 首版先按 5% 采样，避免配额压力。

---

## 5. 指标 / Metrics

### 5.1 当前可用

```bash
# Erlang VM 快照
docker exec -it imboy_backend ./rel/imboy/bin/imboy remote_console
> observer_cli:start().

# PG 连接数
docker exec -it imboy_pg18 psql -U imboy -c \
  "SELECT state, count(*) FROM pg_stat_activity GROUP BY state;"

# WebSocket 在线数（通过后端 API）
curl -fsS https://$API_DOMAIN/metrics/online
```

### 5.2 Prometheus 规划（1.1）

计划暴露 `/metrics` 端点（OpenMetrics 格式），核心指标：

- `imboy_ws_connections_total{state="online|idle"}`
- `imboy_msg_delivered_total{type="c2c|c2g"}`
- `imboy_msg_delivery_latency_seconds_bucket`（直方图）
- `imboy_db_pool_size{pool="default"}`

---

## 6. 运维告警矩阵 / Alerting

1.0.0 首版建议的最小告警集（不依赖 Prometheus，用 Sentry + 简单脚本即可）：

| 告警 | 触发条件 | 渠道 |
|---|---|---|
| 后端宕机 | `GET /healthcheck` 连续 3 次 5xx/超时 | 邮件 + 短信 |
| PG 连接耗尽 | `pg_stat_activity` 活跃连接 > 80% pool | Sentry |
| 错误率飙升 | Sentry event rate > 10/min | Sentry 告警规则 |
| 磁盘 > 85% | `df -h` cron 检查 | 邮件 |
| CPU > 90% 持续 5 分钟 | `top` cron 检查 | 邮件 |

---

## 7. 故障排查速查 / Quick Troubleshooting

| 症状 | 第一步检查 | 第二步 |
|---|---|---|
| 客户端连不上 | `curl /healthcheck` | 查 `log/imboy/error.log` |
| 消息发送后对方未收到 | 查 `msg_store` 表是否写入 | 确认对方 `conv_seq` 是否已推进 |
| 登录 -34018 (macOS) | entitlement 缺失 | 参见 `imboyapp/macos/` keychain 配置 |
| 内存持续增长 | `observer_cli` 查进程数 | 检查是否有泄漏的 gen_server |
| 数据库慢 | `pg_stat_statements` | `EXPLAIN ANALYZE` 慢查询 |

---

## 8. 参考 / References

- [Sentry Erlang](https://github.com/artemeff/eraven)
- [Sentry Flutter](https://docs.sentry.io/platforms/flutter/)
- [Sentry Vue](https://docs.sentry.io/platforms/javascript/guides/vue/)
- [Erlang observer_cli](https://github.com/zhongwencool/observer_cli)
- [PostgreSQL Monitoring](https://www.postgresql.org/docs/current/monitoring.html)
- [CHANGELOG.md](../../CHANGELOG.md) — 版本历史
- [SECURITY.md](../../SECURITY.md) — 安全上报渠道
