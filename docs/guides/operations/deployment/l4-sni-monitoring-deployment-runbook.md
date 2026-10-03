# L4 SNI 巡检与监控部署 Runbook（按需执行 / 候选状态）

> 状态：**未部署**。2026-10-03 用户决策：当前服务器（2C / 3.7G，可用 ~1.5G）资源紧张，
> 暂不部署 Prometheus / Pushgateway / Alertmanager / Grafana（uptrace 从不属于本方案，
> 服务器上既有的 /opt/uptrace 与本任务无关，不会被触碰）。本文档把完整配置与步骤
> 整理入库，资源允许或换机时按需执行。
>
> 2026-10-03 后续授权口径：本轮仅完成配置开关、步骤与本地校验，
> 不启动、不对接 Prometheus/Uptrace 等服务。巡检 cron 历史上线记录与
> 告警管路实际验收分别登记，后者仍为暂缓。

## 配置开关入口 / Deferred activation switch

独立候选为 `deploy/docker-compose.l4-sni-monitoring.yml`，仅包含
Prometheus、Pushgateway 和 Alertmanager；复用仓内已固定的镜像 digest。
不需要 Erlang、数据库、Grafana、Uptrace 或 exporter。端口仅绑定
loopback 的 19090/19091/19093，内存上限合计 736 MiB。

```bash
# 本地配置校验；不拉取镜像、不启动容器、不联络第三方
bash deploy/l4-sni-monitoring/control.sh --check
# 默认 false；--start 在关闭时以 exit 2 拒绝
L4_MONITORING_ENABLED=false bash deploy/l4-sni-monitoring/control.sh --start
```

规则从 `imboy-alerts.yml` 的 `imboy.l4_sni` group 生成到被忽略的
`deploy/l4-sni-monitoring/generated/`；只校验本任务四条规则，避免未部署的
后端/数据库抓取产生噪声。默认 Alertmanager 只在本地界面记录告警。
任何外部接收渠道需用户确认后通过受保护的 `L4_ALERT_CONFIG` 文件传入；
默认配置不能证明通知送达。

未来取得具体生产安装授权、确认资源和接收渠道后，执行：

```bash
L4_MONITORING_ENABLED=true bash deploy/l4-sni-monitoring/control.sh --start
# 验证三个 loopback 端口、targets/rules、Pushgateway /metrics。
# 巡检推送地址改为 http://127.0.0.1:19091，并保留原 cron 其他任务。
# 实际 cron 至少运行一轮，再核验新鲜度与已确认的通知路径。
```

`--start` 不改 cron，不重载 nginx/HAProxy，不配置外部联系方式。启用后
仍须独立验收 scrape、指标、告警及通知。回退使用
`bash deploy/l4-sni-monitoring/control.sh --stop`，保留数据卷，恢复本次变更前
的 cron/ops.env；不得使用 `down -v` 清除证据。
>
> 来源候选：后端 `e8aee7c4`（Wave B，巡检脚本 v3，已在真实宝塔现场只读验证通过）；
> 候选包 SHA-256 `731b927434898a4d935855204339e85ca181f4a5359c0ee5f6712998f617e185`，
> 位于服务器 `/root/l4sni-candidates/20261002T131801Z-l4-sni-hardening-v3/`。
> 现场事实见 run：`.Codex/runs/20261002T131801Z-l4-sni-hardening/agents/SERVER/`。

## 0. 「巡检 cron」是什么（大白话）

一条 `/etc/cron.d` 定时任务：**每 5 分钟用 bash 读一遍 8 个 nginx vhost 配置文件（共 60KB 纯文本），检查 listen 指令是否仍是"loopback:10443 + proxy_protocol"、有没有人把 443 直连改回来**，结果追加到日志。它：

- **不碰 nginx 进程**（绝不 reload/重启，只读文本文件）；
- **不联网、不起常驻进程**（跑完即退，无 Pushgateway 时不需要任何网络）；
- 与监控栈**完全解耦**：没有 Prometheus 也能跑，只是漂移结果只进日志、没人被通知。

**真实消耗（2026-10-03 生产机实测，GNU/bash 计时 3 次取值）**：

| 项 | 实测值 | 折算 |
|---|---|---|
| 单次耗时 | 1.07–1.14 秒 | 每 5 分钟一次 ≈ 每天 5.3 分钟 CPU ≈ 单核 0.4% |
| 峰值内存 | ~2 MB | 瞬时，跑完即释放 |
| 读取量 | 60 KB 文本 | — |
| 日志增长（健康态） | 54 字节/次 | ≈ 16 KB/天（漂移时也只有 KB 级） |

结论：巡检 cron 本身**接近零成本**，资源瓶颈只在 Prometheus 一族。

## 1. 资源预算（为什么暂缓监控栈）

整栈稳态估算（含 compose 里已声明的 limit）：

| 组件 | 稳态 RSS 估 | limit | 说明 |
|---|---|---|---|
| prometheus | 200–350 MB | 512M | 本规模（4 目标/15s/~几百序列/7d 保留） |
| imboy_pushgateway | 15–30 MB | 128M | observability overlay 已 digest 钉定 |
| node_exporter | 10–15 MB | 128M | overlay |
| postgres_exporter | 20–30 MB | 128M | overlay |
| alertmanager | 30–50 MB | — | 通知路由（email/dingtalk 模板渲染） |
| grafana | 200–300 MB | — | 面板 |
| **整栈合计** | **~0.5–0.9 GB** | | 对 1.5G 可用内存偏重 → 暂缓决策依据 |

最小可用子集（若未来只想要告警不要面板）：prometheus + pushgateway 两件 ≈ 250–400 MB。
不做子集裁剪的简化替代：整栈部署但**不装 grafana/alertmanager**，告警只在 Prometheus UI/API 看。

## 2. 方案 B：仅巡检 cron（零监控栈，现在就可执行）

> 这是当前资源约束下唯一零成本选项。代价：**漂移只进日志，没有人会被主动通知**
> （需要人 `grep` 日志或接到故障报告后查证）。

### 步骤（服务器上，全部命令已按本机路径写死）

```bash
# B-1. 安装巡检脚本到部署仓（先校验 hash，再复制）
cd /root/l4sni-candidates/20261002T131801Z-l4-sni-hardening-v3
sha256sum scripts/check_l4_sni_listen.sh
# 期望：3910bd611d939b09…（完整值以包内 SHA256SUMS 为准）
install -m 0755 scripts/check_l4_sni_listen.sh /www/wwwroot/imboy-api/scripts/check_l4_sni_listen.sh

# B-2. 手跑一次验证（只读，预期 rc=0 输出 CHECK_OK）
L4_SNI_ENV_FILE=/etc/imboy/livekit-l4-sni.env \
L4_SNI_INCLUDE_PATH=/www/server/nginx/nginx/conf \
bash /www/wwwroot/imboy-api/scripts/check_l4_sni_listen.sh --strict; echo "rc=$?"

# B-3. 安装 cron（无 --push：没有 Pushgateway 可推，退出码语义仍保留在日志）
mkdir -p /var/log/imboy
cat >/etc/cron.d/imboy-ops-l4-sni <<'CRON'
# L4 SNI nginx listen 漂移巡检（只读，不 reload nginx；无监控栈版本，不带 --push）
*/5 * * * * root L4_SNI_ENV_FILE=/etc/imboy/livekit-l4-sni.env L4_SNI_INCLUDE_PATH=/www/server/nginx/nginx/conf bash /www/wwwroot/imboy-api/scripts/check_l4_sni_listen.sh --strict >> /var/log/imboy/l4_sni_listen.log 2>&1
CRON
chmod 0644 /etc/cron.d/imboy-ops-l4-sni

# B-4. 日志轮转（16KB/天，保守配 4 份周轮转）
cat >/etc/logrotate.d/imboy-l4-sni <<'LR'
/var/log/imboy/l4_sni_listen.log {
    weekly
    rotate 4
    compress
    missingok
    notifempty
}
LR

# B-5. 验证：等 5-10 分钟后
tail /var/log/imboy/l4_sni_listen.log        # 应看到 CHECK_OK: mode=strict files=8 servers=13 violations=0
grep -c CHECK_DRIFT /var/log/imboy/l4_sni_listen.log || true   # 漂移计数（健康=0）
```

### 回滚（方案 B）

```bash
rm /etc/cron.d/imboy-ops-l4-sni /etc/logrotate.d/imboy-l4-sni
rm /www/wwwroot/imboy-api/scripts/check_l4_sni_listen.sh   # 或保留不影响任何现役行为
mv /var/log/imboy/l4_sni_listen.log{,.bak} 2>/dev/null || true
```

### 人工查证（无告警期间的代替动作）

```bash
# 最近一次巡检是否健康 + 漂移明细（诊断带 file:line）
tail -50 /var/log/imboy/l4_sni_listen.log | grep -E "CHECK_|imboy.pub.conf:"
```

## 3. 方案 A：完整监控栈（资源允许时执行）

以下为原通用部署参考，使用 9090/9091/9093；与上文独立开关栈
（19090/19091/19093）互斥选择，不得混用抓取地址和巡检推送地址。
独立开关栈生成的规则说明已同步改为 19091，规则判定表达式不变。

**复用仓库既有资产，不新造**：`deploy/docker-compose.observability.yml`（pushgateway/
node_exporter/postgres_exporter，digest 钉定）、`deploy/prometheus/prometheus.yml`
（pushgateway job 已带 `honor_labels: true`，与巡检推送 job `imboy_l4_sni_check` 匹配）、
`deploy/prometheus/rules/imboy-alerts.yml`（含 Wave A 的 imboy.l4_sni 告警组）、
`deploy/alertmanager/`（模板 + env 渲染，收件方式不入库）。

### A-1. 前置差异说明（本机与仓内假设不同处）

- 本机 imboy Erlang 后端**未运行**：prometheus.yml 的 `imboy_backend` job 会显示 down
  ——部署时把该 job 注释或接受常红（推荐注释，避免噪声）。
- 本机无 prod compose（部署仓是复制目录非 git 仓）：prometheus/alertmanager 本体
  需要单独容器（见 A-2 的最小 compose 片段），或随未来 prod compose 一起上。
- postgres_exporter / node_exporter 可选：磁盘/PG 面板需要才上，告警链路不依赖。

### A-2. 最小告警链路（prometheus + pushgateway + alertmanager，无 grafana）

```yaml
# /root/imboy-monitoring/docker-compose.yml（示例路径，本 runbook 新增文件，不入仓）
services:
  imboy_prometheus:
    image: prom/prometheus:v2.53.0          # 部署时钉 digest：docker pull 后补 @sha256:…
    container_name: imboy_prometheus
    restart: unless-stopped
    command:
      - --config.file=/etc/prometheus/prometheus.yml
      - --storage.tsdb.retention.time=7d
    volumes:
      - ./prometheus:/etc/prometheus
      - prometheus_data:/prometheus
    ports: ["127.0.0.1:9090:9090"]
    deploy: { resources: { limits: { memory: 512M } } }
  imboy_pushgateway:
    image: prom/pushgateway:v1.9.0@sha256:98a458415f8f5afcfd45622d289a0aa67063563bec0f90d598ebc76783571936
    container_name: imboy_pushgateway
    restart: unless-stopped
    command: ["--persistence.file=/data/pushgateway.store", "--persistence.interval=5m"]
    volumes: [pushgateway_data:/data]
    ports: ["127.0.0.1:9091:9091"]           # 只绑 loopback，见 overlay 注释
    deploy: { resources: { limits: { memory: 128M } } }
  imboy_alertmanager:
    image: prom/alertmanager:v0.27.0
    container_name: imboy_alertmanager
    restart: unless-stopped
    volumes: [./alertmanager:/etc/alertmanager]
    ports: ["127.0.0.1:9093:9093"]
    deploy: { resources: { limits: { memory: 96M } } }
volumes: { prometheus_data: {}, pushgateway_data: {} }
```

prometheus.yml 以 `deploy/prometheus/prometheus.yml` 为底本裁剪：注释掉
`imboy_backend` job（本机后端未运行）与 postgres/node exporter job（未部署时），
`rule_files` 指向 imboy-alerts.yml（含 imboy.l4_sni 组：L4SNIListenDrift /
L4SNICheckFailed / L4SNIStaleMetrics / L4SNIMetricsMissing）；
alertmanager 配置用 `bash deploy/alertmanager/render.sh` 渲染（收件人等值在
`alertmanager.env`，**不入库**；渲染前先与用户确认通知渠道——这属于会影响第三方的配置）。

### A-3. 巡检切换到推送模式

1. 建 `/etc/imboy/ops.env`（600 root）：`PUSHGATEWAY_URL=http://127.0.0.1:9091`
2. cron 行升级为（即仓库 `deploy/cron/imboy-ops.cron` 的新增行语义）：
   `set -a; . /etc/imboy/ops.env; set +a; … check_l4_sni_listen.sh --strict --push …`
3. （可选，规范化）把 `NGINX_INCLUDE_PATH=/www/server/nginx/nginx/conf` 写进
   `/etc/imboy/livekit-l4-sni.env`，cron 行可省去 L4_SNI_INCLUDE_PATH 前缀。

### A-4. 上线验证清单（对应计划 Task 9）

```bash
curl -s localhost:9091/-/healthy                       # pushgateway 活
curl -s localhost:9091/metrics | grep imboy_l4_sni     # 巡检已推（drift=0, ts 新鲜）
curl -s localhost:9090/api/v1/targets | jq '.data.activeTargets[]|{job:.labels.job,health}'
curl -s localhost:9090/api/v1/rules | jq '.data.groups[].name'   # 含 imboy.l4_sni
# 告警语义已由仓内 fixture 证明（promtool 22 断言）；生产不做故障注入，
# 新鲜度可自然观察：停 cron 一轮后 L4SNIStaleMetrics 15m 出现（验证后恢复）。
```

### A-5. 回滚（方案 A）

停监控 compose（保留数据卷）；cron 行改回无 `--push` 形态（即方案 B 行）；恢复
prometheus.yml/alertmanager 配置备份。巡检脚本本身保留不影响任何现役服务。

## 4. 边界与不做

- 本 runbook 仅文档；未在服务器执行任何部署动作。
- 不含 uptrace；不触碰 /opt/uptrace。
- Alertmanager 收件渠道（邮箱/钉钉 webhook）属第三方影响面，渲染与启用前必须用户确认。
- 方案 A 执行时遵循计划 Task 9 授权要求：先 diff 现役 cron/规则，仅安装本任务变化。
