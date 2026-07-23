# 可观测性笔记（Observability Notes）

> 工程视角 · 描述现状 + 增量改进 · 三支柱(metrics/logs/traces)

## 现状

**部署资产不等于可复现栈**(`imboy/deploy/`):Git 跟踪 Alertmanager、Loki、Promtail、Grafana provisioning/dashboard 与 Prometheus 告警规则；但生产 Compose 草稿及部分运行配置被忽略，当前仓库不能独立复现“Prometheus + Grafana + Loki + Promtail + Alertmanager 全套编排”。

**Metrics**:后端有 `elib_metric`(指标封装,`imboy_sup` 挂载)+ `telemetry` 依赖 + `metrics_handler`(暴露端点);Prometheus `prometheus.yml` 定义抓取目标 + `rules/` 告警规则。

**Logs**:见 `logging-notes`(覆盖稀疏)+ promtail→loki 聚合。

**Traces**:未见分布式追踪基础设施;请求链路缺统一 request id(见 logging-notes)。

## 优点

- 三支柱已有较多配置资产，起点高；仍需补齐受版本控制的编排与缺失运行配置。
- 有指标封装层 `elib_metric` 与暴露端点,埋点有统一入口。
- Grafana dashboards + provisioning 声明式,可版本化。

## 潜在改进

1. **补业务指标埋点**(优先级中,增量):确认关键业务指标(在线连接数、消息收发量/延迟 p50/p95/p99、ACK 送达率、离线拉取量、E2EE 失败率、支付结算)是否已埋点。基础设施在,埋点密度需核实补齐——这决定能否回答"消息为什么没送达""登录为什么失败"。
2. **引入最小追踪能力**(中):关键链路 request id 贯穿(接入→logic→ds→repo→日志),即便不上完整分布式追踪,也让单请求可跨日志关联。IM 的多跳投递(WS→syn→设备)尤其需要。
3. **三支柱关联**(中):日志、指标、告警目前相对独立;建立 trace/request id 作为关联键,dashboard 能下钻到日志。
4. **可观测性覆盖盲区梳理**(中):以"能否诊断已知故障"为验收(见 `docs/testing/observability-testing` 概念),列出当前盲区(如单进程瓶颈 user_server/depcache 的队列深度是否可见)。

## 相关模块

`imboy/src/lib/elib_metric.erl`、`imboy/src/api/metrics_handler.erl`、`imboy/deploy/prometheus/`、`imboy/deploy/grafana/dashboards/`、`imboy/deploy/loki/`、`imboy/deploy/alertmanager/`

## 优先级

| 建议 | 优先级 |
|---|---|
| 核实并补业务指标埋点 | 中 |
| 关键链路 request id / 最小追踪 | 中 |
| 三支柱关联(下钻) | 中 |
| 可观测盲区梳理 | 中 |
