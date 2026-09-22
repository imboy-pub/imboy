# 客服托管 Widget 部署与运维 / Hosted Customer Service Widget Deployment

> 适用范围：标准部署（Docker Compose + Nginx，社区版 `docker-compose.community.yml` /
> 商务版 `docker-compose.prod.yml`）。本组件由任务卡 CSD-DEP-01 引入，接入合同见
> run 冻结合同 hosted-widget-contract-v1（S1-S6）。
>
> 本文所有域名一律以 `example.com` 占位示例，不含任何真实凭据、联系方式或生产域名。

---

## 1. 概述 / Overview

客服托管 Widget 是嵌入第三方网站的一行代码客服入口。第三方页面只需嵌入
`public_widget_id`（安装级公开 ID，非 secret）+ snippet：

```html
<script async src="https://cs.example.com/v1/loader.js" data-widget-id="1234567890123456789"></script>
```

标准部署因此从两域升级为**三域**：

| 域名 / Variable | 承载 | 上游 |
|---|---|---|
| `API_DOMAIN`（api.example.com） | 后端 API + WebSocket + LiveKit + S3 | `imboy_backend:9800` 等 |
| `ADMIN_DOMAIN`（admin.example.com） | 管理后台 | `imboy_admin:80` |
| `CS_WIDGET_DOMAIN`（cs.example.com） | 客服 Widget 网关（第三域） | 见下 |

`CS_WIDGET_DOMAIN` vhost 的路由拓扑（`deploy/nginx/templates/cs-widget.conf.template`）：

```text
https://cs.example.com/v1/loader.js        ┐
https://cs.example.com/assets/*            ├-> imboy_widget:8080（静态产物容器，仅 compose 内网）
https://cs.example.com/widget/*            ┘
https://cs.example.com/w/<public_widget_id>          -> imboy_backend:9800（动态 frame HTML，
                                                        CSP frame-ancestors 由 backend 按
                                                        installation allowlist 逐请求下发）
https://cs.example.com/api/v1/cs/widget/*            -> imboy_backend:9800（同源 Widget API；
                                                        .../sessions/:id/events 为 SSE，
                                                        网关已关闭缓冲、读超时 3600s）
```

`imboy_widget` 是纯静态产物容器：零业务配置、零数据库、零队列、零 secret，
不发布任何公网端口（仅 compose 内网 `expose 8080`），只读根文件系统运行。

---

## 2. DNS 前置 / DNS Prerequisites

- `cs.example.com` 的 A/AAAA 记录必须指向本机（与 API/Admin 同一公网入口）。
- 与 API/Admin 不同域、且三者**两两不同**；三域重复会被 preflight 直接拒绝
  （重复会使 Nginx `server_name` 冲突、证书签发对象错乱）。
- 80/443 端口公网可达（Let's Encrypt HTTP-01 校验用，与 API/Admin 共用同一机制）。
- **fail-closed 口径**：标准部署三域必填。`CS_WIDGET_DOMAIN` 留空、占位符
  （`cs.example.com`）或与另两域任一相同，`preflight.sh` / `install.sh` 都会
  以非零退出拒绝部署 —— 不存在"没填也能装、Widget 静默半可用"的状态。

---

## 3. 配置项 / Configuration

`deploy/.env`（由 `.env.example` 复制）新增/相关项：

| 变量 | 必填 | 说明 |
|---|---|---|
| `CS_WIDGET_DOMAIN` | 必填 | 客服 Widget 第三域。格式必须为纯域名（不带 `https://` 与路径），且与 `API_DOMAIN`/`ADMIN_DOMAIN` 两两不同 |
| `IMBOY_WIDGET_IMAGE` | 可选 | Widget 静态镜像，默认 `ghcr.io/imboy-pub/imboy-widget:${IMBOY_VERSION}`，与 backend/admin 同一发布流水线构建（源码与镜像定义在 imboyadmin 仓 `Dockerfile.widget`） |

无其他配置项：Widget 静态容器不读取任何业务配置；第三方来源白名单
（`allowed_origins`）等安装级数据全部在 backend 侧管理，不经 `.env` 下发。

---

## 4. 首次安装 / First Install

```bash
cd /path/to/imboy/deploy
cp .env.example .env
$EDITOR .env        # 填写 API_DOMAIN / ADMIN_DOMAIN / CS_WIDGET_DOMAIN / CERTBOT_EMAIL 等
bash install.sh --edition community
```

`install.sh` 会依次完成：内部密钥生成 → preflight（含三域必填/两两不同校验，
任一失败非零退出）→ 镜像拉取 → 启动（含 `imboy_widget`）→ **三域证书一次性签发**
（`nginx/init-letsencrypt.sh` 的域清单已含 CS 域；任一域签发失败即整体中止，
不会在 TLS 未就绪时宣称部署完成）→ 健康等待 → 打印访问地址（含
`客服 Widget : https://cs.example.com`）。

手工等价路径（逐步排查时）：

```bash
cd /path/to/imboy/deploy
bash preflight.sh --docker --edition community
docker network create imboy-network 2>/dev/null || true
docker compose -f docker-compose.community.yml up -d
bash nginx/init-letsencrypt.sh     # 三域证书签发（幂等：已签发的域自动跳过）
```

验证嵌入：在允许的第三方页面（origin 需在该 installation 的 `allowed_origins`
内）粘贴第 1 节 snippet，刷新后右下角应出现客服入口。

---

## 5. 一键事务部署 `cs` / One-command Transactional Deploy

除标准 Compose 部署外，仓库还提供面向既有蓝绿生产环境的 CS 组件一键事务入口：

```bash
bash scripts/imboy-deploy.sh cs [--env-file PATH]
```

该命令由 `scripts/imboy-deploy.sh`（统一部署入口）提供，固定顺序：

```text
PRECHECK → BUILD_AND_VERIFY_WIDGET → STAGE_WIDGET_RELEASE → VALIDATE_CS_VHOST_AND_TLS
  → DEPLOY_BACKEND_BLUE_GREEN → ATOMIC_ACTIVATE_WIDGET_AND_VHOST → REAL_SMOKE
  → FINALIZE_OR_ROLLBACK_WIDGET_GATEWAY
```

关键不变量：Backend 先成功再激活新 Widget；升级失败只回滚 Widget symlink/vhost，
已成功且向后兼容的 Backend 不回滚；首次安装失败不遗留半激活 vhost；verbose 输出
零 secret/token。专属配置键（`CS_WIDGET_DOMAIN`、`CS_BUILD_DIR`、`CS_REMOTE_ROOT`、
`CS_NGINX_CONF`、`CS_CERT_FULLCHAIN`、`CS_CERT_KEY`、`CS_SMOKE_SHOP_ORIGIN`）与
命令细节见 [deploy-script.md](./deploy-script.md) 的「客服 Widget 部署（cs）」章节。

> 两种形态的关系：本文第 4 节是标准 Compose 部署（`imboy_widget` 容器承载静态产物）；
> `cs` 面向蓝绿多节点生产（不可变 release 目录 + `current` symlink 承载静态产物）；
> 它始终从 clean Git HEAD 构建 Widget。`-v` 仅增加日志，`-l` 仅让 Backend 改用本地 rsync。
> 二者共用同一份 `.env`/env-file 的 `CS_WIDGET_DOMAIN` 与同一套 backend 路由。

---

## 6. 升级 / Upgrade

Widget 静态容器跟随 `IMBOY_VERSION` 单一版本来源升级：

```bash
cd /path/to/imboy/deploy
# 1) .env 中把 IMBOY_VERSION 改为目标版本（widget 镜像默认同版本联动）
# 2) 拉新镜像并只滚动 Widget 静态容器（PG/backend/admin/nginx 均不动）
docker compose -f docker-compose.community.yml pull imboy_widget
docker compose -f docker-compose.community.yml up -d imboy_widget
# 3) 确认健康
docker compose -f docker-compose.community.yml ps imboy_widget
curl -fsS https://cs.example.com/health.txt
```

升级不动 vhost、证书与 `.env`；`/v1/loader.js` 为 `no-cache` 稳定入口，第三方页面
下次加载即取到新版（内容 hash 资产 `immutable` 缓存不受影响）。

## 7. 回滚 / Rollback

```bash
# 方式 1：镜像 tag 回退（推荐）—— 显式指回上一个已知良好版本
#   .env: IMBOY_WIDGET_IMAGE=ghcr.io/imboy-pub/imboy-widget:<上一版本>
docker compose -f docker-compose.community.yml up -d imboy_widget

# 方式 2：仅停止 Widget（vhost 保留，第三方嵌入表现为 loader 可达、frame 不可用）
docker compose -f docker-compose.community.yml stop imboy_widget
```

回滚只影响 `imboy_widget` 容器与 Widget 产物；API/Admin/backend 数据、证书与
vhost 不受影响。`cs` 形态的回滚由脚本自动完成（恢复先前 symlink/vhost），
见 deploy-script.md。

---

## 8. Health / Smoke 检查 / Health & Smoke Checks

部署与每次升级后执行（全部应通过）：

```bash
# 1) 静态容器健康（compose 内网）
docker compose -f docker-compose.community.yml ps imboy_widget   # STATUS 含 (healthy)

# 2) 网关静态面：loader 稳定入口（期望 200 + Cache-Control: no-cache）
curl -fsSI https://cs.example.com/v1/loader.js | grep -iE 'HTTP/|cache-control'

# 3) 内容 hash 资产 immutable（任取一个 assets 文件，期望 max-age=31536000, immutable）
curl -fsSI https://cs.example.com/assets/<hash>.js | grep -i cache-control

# 4) 动态 frame：未知 ID 期望 404（installation_unavailable）；已知 ID 期望 200、
#    Cache-Control: no-store、CSP frame-ancestors 为该 installation 的 allowlist
curl -fsSI https://cs.example.com/w/<public_widget_id> | grep -iE 'HTTP/|cache-control|content-security-policy'

# 5) Widget API 同源反代：无 token 的 bootstrap 期望 4xx JSON（而非 404/502）
curl -sS -o /dev/null -w '%{http_code}\n' -X POST https://cs.example.com/api/v1/cs/widget/bootstrap

# 6) SSE 路径反代连通（期望非 502/504；未带合法 visit token 会被 backend 401）
curl -sS -o /dev/null -w '%{http_code}\n' -N --max-time 5 \
  https://cs.example.com/api/v1/cs/widget/sessions/1/events
```

## 9. 证书续期检查 / Certificate Renewal

CS 域证书与 API/Admin 走同一 certbot 机制（`imboy_certbot` 每 12h `certbot renew`，
`imboy_nginx` 每 6h reload 加载新证书），签发成功后自动包含在续期清单内：

```bash
# 确认 CS 域 renewal 配置存在
ls data/certbot/conf/renewal/            # 应看到 cs.example.com.conf（连同 api/admin 域）

# 到期时间
docker compose -f docker-compose.community.yml exec imboy_certbot \
  certbot certificates | grep -A3 'cs.example.com'

# 续期监控：prometheus 规则 IMBoyTLSCertExpiringSoon / IMBoyTLSCertExpired 覆盖全部证书
```

人工续期演练（非破坏，`--dry-run` 使用测试环境）：

```bash
docker compose -f docker-compose.community.yml run --rm --entrypoint certbot imboy_certbot \
  renew --dry-run --cert-name cs.example.com
```

---

## 10. 故障排查 / Troubleshooting

| 现象 / Symptom | 排查 / Troubleshooting |
|---|---|
| `preflight.sh` 报 `CS_WIDGET_DOMAIN 与 ... 不能相同` | 三域必须两两不同；修改 `.env` 后重跑 |
| preflight 报 `CS_WIDGET_DOMAIN 不是有效的纯域名` | 值里带了 `https://`、路径或结尾 `.`；只填 `cs.example.com` 形式的裸域名 |
| `install.sh` 提示 `CS_WIDGET_DOMAIN 尚未填写真实值` | 占位符 `cs.example.com` 未替换；编辑 `.env` 后重跑 |
| `install.sh` 证书签发失败且提到 CS 域 | 该域 A 记录未指向本机 / 80 端口不通 / Let's Encrypt 限流；与 API/Admin 域签发失败的排查方式一致 |
| 第三方页面嵌入后无客服入口 | 浏览器 console 是否有 `data-widget-id` 告警；该页 origin 是否在该 installation 的 `allowed_origins` 内 |
| loader 200 但 frame 打不开 | `curl -I https://cs.example.com/w/<id>` 看 CSP `frame-ancestors` 是否包含宿主 origin；404 表示 installation 不存在/已停用/已吊销 |
| `/w/` 或 `/api/v1/cs/widget/` 返回 502 | `docker compose logs imboy_backend`；backend 未健康时网关反代必然 502 |
| SSE 无推送但消息正常 | 确认线上 vhost 是本仓模板渲染产物（SSE location 需 `proxy_buffering off`）；中间若有额外代理层需同样关闭缓冲 |
| `imboy_widget` 容器反复重启 | `docker compose logs imboy_widget`；确认镜像为本仓发布产物（只读根文件系统 + tmpfs /tmp 为镜像运行时合同） |

## 11. 卸载边界 / Uninstall Boundary

仅卸载客服 Widget、不影响 API/Admin 的完整边界：

```bash
cd /path/to/imboy/deploy
# 1) 停止并移除静态容器（不动 backend/admin/pg/nginx/certbot）
docker compose -f docker-compose.community.yml rm -sf imboy_widget
# 2) 移除 CS vhost 模板并 reload nginx（移除后 nginx 不再监听该域）
mv nginx/templates/cs-widget.conf.template /tmp/cs-widget.conf.template.removed
docker compose -f docker-compose.community.yml exec imboy_nginx nginx -s reload
# 3) 移除该域证书目录（可选；只删 CS 域，勿动 api/admin 域目录）
rm -rf data/certbot/conf/live/cs.example.com data/certbot/conf/archive/cs.example.com \
       data/certbot/conf/renewal/cs.example.com.conf
# 4) .env 中 CS_WIDGET_DOMAIN 可保留（不再被任何组件消费）
```

明确**不属于**卸载范围：API/Admin 的 vhost（`imboy.conf.template`）、backend 的
`/api/v1/cs/widget/*` 与旧 frame 路由、PostgreSQL/Garage 及其数据。若要连客服
业务数据一并清理，属于独立的 backend 数据治理操作，不在本组件卸载边界内。
