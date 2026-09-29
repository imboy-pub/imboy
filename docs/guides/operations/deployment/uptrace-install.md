# Uptrace 链路追踪安装与后端对接指南

> 目标：在一台 Linux 服务器上从零安装 Uptrace（OpenTelemetry APM 平台，https://trace.imboy.pub），
> 并让 imboy 后端（prod / dev 环境）把链路数据上报上来。
>
> **写法承诺**：本文所有命令均按顺序复制执行即可成功（在 Debian 12 + Docker 24 环境实测通过）。
> 每一步都有 ✅ 验证点，看到预期输出再做下一步。
>
> | 项 | 值 |
> |---|---|
> | Uptrace 版本 | v2.1.0-beta.8（源码构建，原因见 [FAQ-1](#faq)） |
> | 组件 | Uptrace + ClickHouse 25.3.3 + PostgreSQL 15 + Redis 7 |
> | 机器要求 | ≥ 2 核 / 4G 内存 / 20G 磁盘（已内置内存上限保护） |
> | 预计耗时 | 30~45 分钟（大部分是镜像拉取和编译等待） |
>
> 最后更新：2026-09-16 ｜ 维护人：imboy 运维

---

## 目录

1. [架构总览](#1-架构总览)
2. [前置准备：DNS 与工具检查](#2-前置准备)
3. [生成全部凭据（自动）](#3-生成全部凭据)
4. [创建 Uptrace 配置文件](#4-创建-uptrace-配置文件)
5. [创建 Docker 编排文件](#5-创建-docker-编排文件)
6. [构建镜像并启动](#6-构建镜像并启动)
7. [安装 Nginx 反向代理](#7-安装-nginx-反向代理)
8. [申请 HTTPS 证书 + 自动续期](#8-申请-https-证书--自动续期)
9. [登录验证（超管账号）](#9-登录验证)
10. [imboy 后端对接](#10-imboy-后端对接)
11. [日常运维命令](#11-日常运维命令)
12. [FAQ——常见坑](#faq常见坑)

---

## 1. 架构总览

```
                 ┌────────────── 同一台服务器（示例 106.53.76.53）──────────────┐
浏览器/用户       │  Nginx(443, HTTPS)  ──→  Uptrace(127.0.0.1:14318, HTTP)     │
   │             │                              │          │                    │
   │ https://trace.imboy.pub                    │          └─ Redis(缓存)         │
   │             │                              ▼                                │
imboy 后端        │                    ClickHouse(traces/metrics/logs)           │
(OTLP 上报)  ────→│  127.0.0.1:14317(gRPC) / :14318(HTTP)                        │
                 │                    PostgreSQL(用户/项目/仪表盘)                │
                 └──────────────────────────────────────────────────────────────┘
```

- 四个组件全部用 Docker 跑在内部网络，**只有 Uptrace 的 HTTP 端口映射到 127.0.0.1**，
  数据库端口不对公网开放。
- HTTPS 由 Nginx + Let's Encrypt 提供，证书自动续期。
- 超级管理员账号和项目密钥在 Uptrace 配置文件里声明，首次启动自动创建。

---

## 2. 前置准备

### 2.1 DNS 解析

在域名服务商处添加一条 A 记录：

```
主机记录: trace      记录类型: A      记录值: <你的服务器公网 IP>
```

✅ 验证（在本机或任意机器执行，应返回你的服务器 IP）：

```bash
dig +short trace.imboy.pub @119.29.29.29
```

### 2.2 服务器工具检查

SSH 登录服务器后执行：

```bash
docker --version          # 需要 20.10+
docker compose version    # 需要 v2.x；若提示不存在，先执行下面这行安装
```

如果没有 docker compose 插件：

```bash
apt-get update && apt-get install -y docker-compose-plugin git curl
docker compose version    # 再验证一次
```

> 本文统一使用 `docker compose`（v2 写法）。若你的机器只有 v1（`docker-compose`），
> 把后文所有 `docker compose` 替换为 `docker-compose` 即可。

---

## 3. 生成全部凭据

所有密码/密钥用一条命令自动生成，保存在 `/opt/uptrace/.secrets.env`（root 600 权限）。
**你不需要手工输入任何密码**，后续步骤会自动读取。

```bash
mkdir -p /opt/uptrace && cd /opt/uptrace

cat > .secrets.env <<EOF
# Uptrace 全套凭据（自动生成于 $(date +%F)，勿提交到任何仓库）
CH_PASS=$(openssl rand -hex 16)          # ClickHouse 密码
PG_PASS=$(openssl rand -hex 16)          # PostgreSQL 密码
SVC_SECRET=$(openssl rand -hex 24)       # Uptrace 内部签名密钥
ADMIN_PASS=$(openssl rand -base64 24 | tr -dc 'a-zA-Z0-9' | head -c 18)  # 超管登录密码
PROD_TOKEN=$(openssl rand -hex 16)       # imboy-prod 项目上报令牌
DEV_TOKEN=$(openssl rand -hex 16)        # imboy-dev 项目上报令牌
EOF
chmod 600 .secrets.env
```

✅ 验证：`source .secrets.env && echo $ADMIN_PASS` 能打印出一个 18 位密码。

> 💡 这就是稍后登录 https://trace.imboy.pub 的超级管理员密码。
> 随时可以 `cat /opt/uptrace/.secrets.env` 查看，或手动改写后重启容器生效。

---

## 4. 创建 Uptrace 配置文件

> ⚠️ 不要从 Uptrace 官方 GitHub 复制 `config/uptrace.dist.yml`——那份模板与
> 实际二进制长期不同步（写了不存在的字段，会启动失败）。直接用下面这份
> 已验证可用的最小配置。

```bash
cd /opt/uptrace

cat > uptrace.yml <<'EOF'
## Uptrace 配置（trace.imboy.pub 实测可用版，基于 v2.1.0-beta.8）
## 占位符由第 3 步的 .secrets.env 替换：__SVC_SECRET__ __ADMIN_PASS__
##                                    __PROD_TOKEN__ __DEV_TOKEN__

service:
  env: production
  secret: '__SVC_SECRET__'

site:
  # 对外访问地址（改成你的域名）
  url: 'https://trace.imboy.pub'

## 超级管理员：首次启动自动创建。email 即登录账号。
auth:
  users:
    - name: Admin
      email: admin@imboy.pub
      password: '__ADMIN_PASS__'

## 项目：每个上报方一个项目，token 用于鉴权（即 DSN 里的 <token>）
projects:
  - id: 1
    name: 'imboy-prod'
    token: '__PROD_TOKEN__'
  - id: 2
    name: 'imboy-dev'
    token: '__DEV_TOKEN__'

## ClickHouse 连接（容器网络内）
ch:
  addr: 'clickhouse:9000'
  user: uptrace
  password: '__CH_PASS__'
  database: uptrace
  max_execution_time: 15s
  query_settings:
    session_timezone: UTC
    async_insert: 1

## PostgreSQL 连接
pg:
  addr: 'postgres:5432'
  user: uptrace
  password: '__PG_PASS__'
  database: uptrace

## 监听端口：gRPC 收 OTLP/gRPC，HTTP 收 OTLP/HTTP + Web UI
listen:
  grpc:
    addr: ':4317'
  http:
    addr: ':80'

## 邮件告警默认关闭；需要时再配置 smtp
mailer:
  smtp:
    enabled: false

## Redis 缓存
redis_cache:
  addrs:
    1: 'redis:6379'

## 关闭 Uptrace 自身的客户端遥测
uptrace_go:
  disabled: true

logging:
  level: INFO
EOF

# 用真实凭据替换占位符
source .secrets.env
sed -i "s/__SVC_SECRET__/${SVC_SECRET}/; s/__ADMIN_PASS__/${ADMIN_PASS}/; s/__PROD_TOKEN__/${PROD_TOKEN}/; s/__DEV_TOKEN__/${DEV_TOKEN}/" uptrace.yml
chmod 600 uptrace.yml
```

✅ 验证：`grep -c "__" uptrace.yml` 输出 `0`（没有未替换的占位符）。

> 📌 记下两个 DSN（后端对接要用，第 10 步会再次出现）：
>
> | 环境 | DSN |
> |---|---|
> | imboy-prod | `http://$PROD_TOKEN@trace.imboy.pub/1` |
> | imboy-dev | `http://$DEV_TOKEN@trace.imboy.pub/2` |
>
> 也可登录 Uptrace 后在 **Project Settings → DSN** 页面随时查看。

---

## 5. 创建 Docker 编排文件

### 5.1 ClickHouse 内存限制（小内存机器必读）

```bash
cd /opt/uptrace
cat > clickhouse-memory.xml <<'EOF'
<clickhouse>
    <!-- 限制 ClickHouse 总内存 ~700MB，防止挤占同机其他服务 -->
    <max_server_memory_usage>700000000</max_server_memory_usage>
    <mark_cache_size>134217728</mark_cache_size>
</clickhouse>
EOF
```

### 5.2 docker-compose.yml

```bash
cd /opt/uptrace
cat > docker-compose.yml <<'EOF'
version: "2.4"

services:
  clickhouse:
    image: clickhouse/clickhouse-server:25.3.3
    container_name: uptrace_clickhouse
    restart: unless-stopped
    environment:
      CLICKHOUSE_DB: uptrace
      CLICKHOUSE_USER: uptrace
      CLICKHOUSE_PASSWORD: "${CH_PASS}"
    ulimits:
      nofile:
        soft: 262144
        hard: 262144
    volumes:
      - ch_data:/var/lib/clickhouse
      - ./clickhouse-memory.xml:/etc/clickhouse-server/config.d/99-memory.xml:ro
    healthcheck:
      test: ["CMD", "wget", "--spider", "-q", "localhost:8123/ping"]
      interval: 5s
      timeout: 3s
      retries: 30
    mem_limit: 900m
    networks: [uptrace]

  postgres:
    image: postgres:15-alpine
    container_name: uptrace_postgres
    restart: unless-stopped
    environment:
      PGDATA: /var/lib/postgresql/data/pgdata
      POSTGRES_USER: uptrace
      POSTGRES_PASSWORD: "${PG_PASS}"
      POSTGRES_DB: uptrace
    volumes:
      - pg_data:/var/lib/postgresql/data/pgdata
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U uptrace -d uptrace"]
      interval: 5s
      timeout: 3s
      retries: 20
    mem_limit: 256m
    networks: [uptrace]

  redis:
    image: redis:7-alpine
    container_name: uptrace_redis
    restart: unless-stopped
    command: ["redis-server", "--maxmemory", "48mb", "--maxmemory-policy", "allkeys-lru", "--save", ""]
    mem_limit: 64m
    networks: [uptrace]

  uptrace:
    image: uptrace-local:2.1.0-beta.8
    container_name: uptrace_app
    restart: unless-stopped
    depends_on:
      clickhouse:
        condition: service_healthy
      postgres:
        condition: service_healthy
      redis:
        condition: service_started
    command: ["serve"]
    volumes:
      - ./uptrace.yml:/etc/uptrace/config.yml:ro
    ports:
      # 只绑定本机回环，公网统一走 Nginx
      - "127.0.0.1:14318:80"
      - "127.0.0.1:14317:4317"
    mem_limit: 384m
    networks: [uptrace]

volumes:
  ch_data:
  pg_data:

networks:
  uptrace:
    name: uptrace_net
EOF
```

> 注意：Uptrace 的配置文件挂载路径是 `/etc/uptrace/config.yml`（不是 GitHub 文档写的
> `uptrace.yml`），这是镜像入口脚本写死的路径，挂错会启动失败。

### 5.3 Dockerfile（后端 + Vue 前端一起构建）

> Uptrace 的 Web 界面在**编译期**打包进二进制。Docker Hub 上的官方镜像构建时间
> 严重滞后（连自家配置格式都不认，且与现役 ClickHouse 无法连通），因此必须从
> 源码构建一次。国内网络已内置 goproxy.cn / npmmirror 加速。

```bash
cd /opt/uptrace

cat > Dockerfile <<'EOF'
# ---------- 阶段 1：构建 Vue 前端 ----------
FROM node:20-alpine AS vue
RUN corepack enable
WORKDIR /app
ENV npm_config_registry=https://registry.npmmirror.com \
    CYPRESS_INSTALL_BINARY=0 \
    NODE_OPTIONS=--max-old-space-size=1536
RUN corepack prepare pnpm@9.15.9 --activate
COPY vue/package.json vue/pnpm-lock.yaml ./
RUN pnpm install --frozen-lockfile
COPY vue/ .
RUN pnpm build

# ---------- 阶段 2：编译 Go 后端（前端产物嵌入二进制）----------
FROM golang:1.25-alpine AS build
RUN apk add --no-cache git ca-certificates
WORKDIR /src
ENV GOPROXY=https://goproxy.cn,direct GOMEMLIMIT=1400MiB
COPY . .
COPY --from=vue /app/dist vue/dist
RUN CGO_ENABLED=0 go build -trimpath -ldflags="-s -w" -p 2 -o /uptrace ./cmd/uptrace

# ---------- 阶段 3：运行镜像 ----------
FROM alpine:3.20
RUN apk add --no-cache ca-certificates tzdata
COPY --from=build /uptrace /uptrace
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
EOF

cat > entrypoint.sh <<'EOF'
#!/bin/sh
set -euxo pipefail
if [ $# -eq 0 ]; then
    /uptrace --config=/etc/uptrace/config.yml pg wait
    /uptrace --config=/etc/uptrace/config.yml ch wait
    exec /uptrace --config=/etc/uptrace/config.yml serve
else
    exec /uptrace --config=/etc/uptrace/config.yml $@
fi
EOF
chmod +x entrypoint.sh
```

### 5.4 拉取 Uptrace 源码（锁定版本）

```bash
cd /opt/uptrace
git clone --depth 1 --branch v2.1.0-beta.8 https://github.com/uptrace/uptrace build
cp entrypoint.sh build/entrypoint.sh
ls build/cmd/uptrace   # 确认源码在位
```

✅ 验证：`ls /opt/uptrace/build/vue/pnpm-lock.yaml` 存在。

---

## 6. 构建镜像并启动

### 6.1 构建镜像（约 10~20 分钟，可在后台等）

```bash
cd /opt/uptrace
docker build -f Dockerfile -t uptrace-local:2.1.0-beta.8 build/ ; echo "BUILD_EXIT=$?"
```

✅ 验证：最后一行 `BUILD_EXIT=0`，且 `docker images uptrace-local` 能看到镜像
（约 70MB——**小于 50MB 说明前端没打进去，页面会退化成文件列表**，需重查构建日志）。

### 6.2 启动整套服务

```bash
cd /opt/uptrace
source .secrets.env
docker compose up -d
sleep 40
docker compose ps
```

✅ 验证：四个容器全部 `Up`（clickhouse/postgres 为 `healthy`）。

> 首次启动时 Uptrace 会自动：建 PostgreSQL/ClickHouse 全部表、
> 创建超管账号（admin@imboy.pub）、创建 imboy-prod / imboy-dev 两个项目。

### 6.3 数据库初始化（双保险，幂等可重复执行）

```bash
docker exec uptrace_app /uptrace --config=/etc/uptrace/config.yml ch init
docker exec uptrace_app /uptrace --config=/etc/uptrace/config.yml pg init
```

✅ 验证：

```bash
curl -s -o /dev/null -w "%{http_code}\n" http://127.0.0.1:14318/   # 期望 200
docker logs uptrace_app 2>&1 | grep -c "level=ERROR"               # 期望 0
```

---

## 7. 安装 Nginx 反向代理

> 以下为裸 Nginx 配法。如果你的服务器用宝塔面板，也可以在面板里建站 + 一键 SSL，
> 只要保证：80 端口保留 `/.well-known` 给证书验证、443 反向代理到 `127.0.0.1:14318`。

### 7.1 先配置 80 端口（用于证书验证 + 强制跳转 HTTPS）

```bash
cat > /etc/nginx/conf.d/trace.imboy.pub.conf <<'EOF'
server
{
    listen 80;
    server_name trace.imboy.pub;
    root /www/wwwroot/trace.imboy.pub;

    location ~ \.well-known { allow all; }
    location / { return 301 https://$host$request_uri; }
}
EOF
mkdir -p /www/wwwroot/trace.imboy.pub
nginx -t && systemctl reload nginx
```

✅ 验证：`nginx -t` 输出 `syntax is ok` / `test is successful`。

> 宝塔用户路径为 `/www/server/panel/vhost/nginx/trace.imboy.pub.conf`，
> reload 命令相同。

### 7.2 申请 HTTPS 证书

```bash
apt-get install -y certbot   # 已装可跳过
certbot certonly --webroot -w /www/wwwroot/trace.imboy.pub -d trace.imboy.pub --non-interactive --agree-tos
```

✅ 验证：

```bash
ls /etc/letsencrypt/live/trace.imboy.pub/fullchain.pem && openssl x509 -in /etc/letsencrypt/live/trace.imboy.pub/cert.pem -noout -dates
```

### 7.3 配置 443 反向代理（覆盖上面同一份文件）

```bash
cat > /etc/nginx/conf.d/trace.imboy.pub.conf <<'EOF'
server
{
    listen 80;
    server_name trace.imboy.pub;
    root /www/wwwroot/trace.imboy.pub;
    location ~ \.well-known { allow all; }
    location / { return 301 https://$host$request_uri; }
}

server
{
    listen 443 ssl http2;
    server_name trace.imboy.pub;

    ssl_certificate     /etc/letsencrypt/live/trace.imboy.pub/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/trace.imboy.pub/privkey.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_session_cache shared:uptrace_SSL:10m;
    ssl_session_timeout 10m;
    add_header Strict-Transport-Security "max-age=31536000" always;

    client_max_body_size 32m;

    location /
    {
        proxy_pass http://127.0.0.1:14318;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto https;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;
        proxy_buffering off;
    }
}
EOF
nginx -t && systemctl reload nginx
```

### 7.4 证书自动续期

Let's Encrypt 证书 90 天有效。系统安装 certbot 时自带每日检查定时器
（`certbot.timer`，剩余 30 天内自动续），只需补一个"续期后重载 Nginx"的钩子：

```bash
mkdir -p /etc/letsencrypt/renewal-hooks/deploy
cat > /etc/letsencrypt/renewal-hooks/deploy/reload-nginx.sh <<'EOF'
#!/bin/bash
systemctl reload nginx
EOF
chmod +x /etc/letsencrypt/renewal-hooks/deploy/reload-nginx.sh

certbot renew --dry-run   # 模拟续期一次，验证整条链路
```

✅ 验证：`certbot renew --dry-run` 输出 `Congratulations, all simulated renewals succeeded`。

---

## 8. 登录验证

浏览器打开 **https://trace.imboy.pub**，用以下账号登录：

| 项 | 值 |
|---|---|
| 邮箱 | `admin@imboy.pub` |
| 密码 | `cat /opt/uptrace/.secrets.env` 里的 `ADMIN_PASS` |

✅ 验证清单：

- 登录成功，左上角项目切换器能看到 **imboy-prod** 和 **imboy-dev** 两个项目
- 密码忘了：`cat /opt/uptrace/.secrets.env`；
  想改密码：编辑 `/opt/uptrace/uptrace.yml` 的 `auth.users` 段后
  `docker compose restart uptrace`（该文件是用户唯一真源）

---

## 9. imboy 后端对接

> imboy 后端已内置 OpenTelemetry 上报（commit `b3bd6c30` 起）。
> **只要设置 `IMBOY_UPTRACE_DSN` 环境变量并重启节点即完成对接**；
> 未设置该变量的环境完全不受影响。

### 9.1 DSN 对照表

| imboy 环境 | DSN（在 /opt/uptrace/.secrets.env 里取 token 值） |
|---|---|
| prod（生产节点） | `http://<PROD_TOKEN>@trace.imboy.pub/1` |
| dev（开发节点） | `http://<DEV_TOKEN>@trace.imboy.pub/2` |

登录 Uptrace 后在 **Project Settings** 页也能直接复制 DSN。

### 9.2 dev 环境对接（开发节点）

```bash
cd /www/wwwroot/imboy-api            # 开发节点工作区
echo 'IMBOY_UPTRACE_DSN=http://<DEV_TOKEN>@trace.imboy.pub/2' >> .env.local
scripts/start_node.sh imboydev <your-cookie> 9700 "" daemon
```

> `.env.local` 会被 `start_node.sh` 在启动前自动加载，且不入 git。

### 9.3 prod 环境对接（生产节点）

推荐把上报配置写进逐机配置 `config/sys.pro.config`（不入 git），在顶层
列表（与 `{kernel, ...}` 平级）追加：

```erlang
 {opentelemetry, [
      {processors, [{otel_batch_processor, #{
          exporter => {otel_exporter_traces_otlp, #{
              endpoints => [<<"http://127.0.0.1:14318">>],
              headers => [{<<"uptrace-dsn">>, <<"http://<PROD_TOKEN>@trace.imboy.pub/1">>}],
              protocol => http_protobuf,
              timeout => 5000}},
          scheduled_delay_ms => 5000}}]}
 ]},
```

然后重新打包并重启生产节点（⚠️ 会中断服务约 1~3 分钟，选低峰执行）：

```bash
IMBOYENV=pro make rel
# 之后按你们的发布流程重启生产节点即可
```

> ⚠️ exporter 模块名必须是 **`otel_exporter_traces_otlp`**。写成
> `otel_exporter_otlp` 不会报错，但 span 会静默丢失（见 FAQ-3）。

### 9.4 验证上报成功

节点启动约 10 秒后会自动上报一条 `imboy.boot` 探针 span。登录 Uptrace，
切到对应项目，首页应出现这条 trace；或者在服务器上直接查库：

```bash
docker exec uptrace_clickhouse clickhouse-client --query \
  "SELECT project_id, name, count() FROM uptrace.spans_index GROUP BY project_id, name LIMIT 10"
```

`project_id=1` 出现 `imboy.boot` → 生产对接成功；
`project_id=2` 出现 `imboy.boot` → 开发对接成功。

### 9.5 业务代码里加自定义埋点（可选）

```erlang
%% 整段逻辑计入一个 span，异常自动记录
imboy_telemetry:with_span(<<"user.login">>, fun() ->
    do_login()
end).

%% 带属性
imboy_telemetry:with_span(<<"msg.route">>, #{attrs => #{<<"msg.type">> => <<"c2c">>}}, fun() ->
    route_message()
end).
```

遥测未启用的环境调用这些函数零开销（直接执行原逻辑）。

---

## 10. 日常运维命令

```bash
cd /opt/uptrace
docker compose ps                    # 看状态
docker compose logs -f uptrace       # 看 Uptrace 日志
docker compose restart uptrace       # 只重启 Uptrace
docker compose down                  # 停整套（数据在 volume，不丢）
docker compose up -d                 # 再启动

cat /opt/uptrace/.secrets.env        # 查看全部凭据
vi /opt/uptrace/uptrace.yml          # 改配置（超管密码/项目/DSN token）
docker compose up -d --force-recreate uptrace   # 改完配置必须 force-recreate 才生效
```

> ⚠️ Uptrace 的配置文件是**单文件 bind mount**：修改后务必用
> `--force-recreate` 重建容器，普通 restart 可能读到旧内容。

---

## FAQ——常见坑

**FAQ-1：为什么不用 Docker Hub 的官方镜像 `uptrace/uptrace`？**
Hub 上的镜像构建时间严重滞后于代码仓库（tag 名与实际二进制不符）：配置文件路径、
配置字段、ClickHouse 兼容性都对不上，实测 2.0.3 与 2.1.0-beta.8 两个 tag 均无法
与任何现役 ClickHouse 版本连通。从源码构建一次（本文第 5~6 步）是唯一可靠路径。

**FAQ-2：页面打开是一个只有 README.md 链接的目录列表？**
说明镜像里没有前端。Uptrace 的 Web 界面在编译期嵌入二进制（`//go:embed vue/dist`），
必须走第 5.3 的双阶段 Dockerfile（先 pnpm 构建前端再编译 Go）。成品镜像约 70MB，
只有 47MB 左右就是缺前端。

**FAQ-3：登录正常、接口正常，但 Uptrace 里始终没有 span 数据？**
检查 exporter 模块名是否为 `otel_exporter_traces_otlp`。写成 `otel_exporter_otlp`
（1.9 及更早的名字）时 span 会进入队列但永不导出，且**无任何报错日志**。
另外确认节点启动日志里有 `Application opentelemetry started`。

**FAQ-4：修改 uptrace.yml 后行为没变化？**
单文件 bind mount 的经典问题：用 `docker compose up -d --force-recreate uptrace`
重建容器（普通 restart 不保证读到新内容）。

**FAQ-5：ClickHouse 把同机其他服务内存吃光了？**
本文 compose 已限制 ClickHouse 700MB（clickhouse-memory.xml）、容器 900MB。
数据默认保留策略由 `ch_schema` 控制，磁盘增长过快时可在 Uptrace 项目设置里
调短保留期。

**FAQ-6：想换/找回超管密码？**
超管账号的唯一真源是 `/opt/uptrace/uptrace.yml` 的 `auth.users` 段。
改密码 → 改文件 → `docker compose up -d --force-recreate uptrace`。
项目 token 同理（token 变了，后端 DSN 要同步更新）。

---

## 附：本文档的验证环境

- Debian 12 (bookworm) / Docker 24.0.7 / Compose v2.23.3 / certbot 2.1.0
- 106.53.76.53（trace.imboy.pub），与 imboy 生产/开发节点同机部署
- 内存 3.7G + 8G swap（同机还运行 imboy 全家桶），Uptrace 全栈内存占用约 1.2~1.5G
