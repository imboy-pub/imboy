# 宿主机生产 vhost 快照 / Host-level Production Vhost Snapshots

> 快照时间 / Snapshot date: **2026-09-21**（同日续期统一迁移后回填：pro/s3/www/dev/trace
> 五域 vhost 已改为 certbot 证书路径 + ACME webroot，宝塔面板续期已对这四域停用）
> 来源主机 / Source host: `pro-imboy`（蓝绿生产机，宝塔面板布局）
> 服务器路径 / Server path: `/www/server/panel/vhost/nginx/<domain>.conf`
>
> 本目录是**生产宿主机全部活跃域名 vhost 的入库快照**（蓝绿单机形态，区别于
> `templates/` 下的 compose envsubst 模板）。快照不自动同步：**改服务器文件后须
> 回填本目录**；从本目录恢复时须 `nginx -t && nginx -s reload`。
> 各文件与服务器逐字节一致（仅统一补了行尾换行）。

## 文件清单 / Files（10 个域；8 个生产活跃 + 2 个 LK-01 新增目标形态）

| 文件 | 角色 / Role | 证书来源 / Cert | 续期 / Renewal | 管理方式 |
|---|---|---|---|---|
| `pro.imboy.pub.conf` | API 主域（蓝绿 upstream、WS、LiveKit 旧 /livekit location） | `/etc/letsencrypt/live/pro.imboy.pub/` | certbot webroot `/var/www/certbot`（cs 模式） | 手工 |
| `prodadm.imboy.pub.conf` | 管理后台（静态 + `:9800` admin API；含 IPv6 listener） | 宝塔 `.../cert/prodadm.imboy.pub/` | 宝塔面板（未迁移，仍走宝塔续期） | 由 `admin` 组件部署静态产物，vhost 手工 |
| `cs.imboy.pub.conf` | 客服 Widget 网关（CSD-CLI-01 工具托管，**禁止手改线上**，改 `scripts/lib/cs_deploy.sh` 后重跑 `cs -v -l`） | `/etc/letsencrypt/live/cs.imboy.pub/` | certbot webroot `/var/www/certbot` + `reload-openresty.sh` 钩子 | **工具托管** |
| `www.imboy.pub.conf` | 官网静态（`server_name` 含裸域 `imboy.pub`） | `/etc/letsencrypt/live/www.imboy.pub/`（SAN 含裸域） | certbot webroot `/var/www/certbot`（cs 模式） | 手工 |
| `s3.imboy.pub.conf` | Garage S3（`$s3_pass` 3900/3902 自包含） | `/etc/letsencrypt/live/s3.imboy.pub/` | certbot webroot `/var/www/certbot`（cs 模式） | 手工 |
| `trace.imboy.pub.conf` | Uptrace UI（`127.0.0.1:14318`） | `/etc/letsencrypt/live/trace.imboy.pub/` | certbot webroot `/var/www/certbot`（cs 模式；原站点根 webroot 已停用） | 手工 |
| `dev.imboy.pub.conf` | 9700 开发节点 | `/etc/letsencrypt/live/dev.imboy.pub/` | certbot webroot `/var/www/certbot`（cs 模式） | 手工 |
| `turn.imboy.pub.conf` | 纯 80 ACME webroot（**仅**承载本域证书签发/续期；TURN/TLS 5349 由 LiveKit embedded TURN 直接承载，无 443 —— TURN_443=BLOCKED，LK-01 §3） | `/etc/letsencrypt/live/turn.imboy.pub/` | certbot webroot + `../livekit-turn-cert-deploy-hook.sh`（原子分发到 `/etc/imboy/livekit-certs/` 并安全重启 LiveKit） | 手工 |
| `rtc.imboy.pub.conf` | **LK-01 新增目标形态（尚未部署到服务器）**：LiveKit 信令域 WSS 反代 `127.0.0.1:7880`（Upgrade 头 + 3600s + buffering off，写法对齐 pro 的 /livekit） | `/etc/letsencrypt/live/rtc.imboy.pub/`（待签发） | certbot webroot `/var/www/certbot` + nginx reload 钩子 | 手工（部署顺序见文件头：先纯 80 签发，再放开 443 块） |

## 服务器端依赖（快照不含）/ Server-side includes NOT in snapshots

`dev / pro / www / turn` 四个 vhost 含宝塔 `include`，依赖服务器上的这些文件
（本目录未快照，恢复到新机时须一并迁移）：

- `/www/server/panel/vhost/nginx/extension/<domain>/*.conf`
- `/www/server/panel/vhost/nginx/well-known/<domain>.conf`（证书申请验证用）
- `/www/server/panel/vhost/rewrite/<domain>.conf`（turn/www）

## 未部署 / 未入库的域 / Absent domains

- **`rtc.imboy.pub.conf` / `turn.imboy.pub.conf` 的差异**：两文件是 LK-01（2026-09-21）
  的**目标形态**（rtc 为全新 vhost；turn 更新为纯 ACME webroot + LiveKit 分发链路），
  服务器现场尚未同步 —— 部署时按文件头注释顺序操作，同步后本目录恢复
  「与服务器逐字节一致」约定。
- 共享依赖（两域 vhost 同 pro 域）：`$connection_upgrade` 全局 map（rtc 的 WSS
  Upgrade 依赖）、`/var/www/certbot` webroot、`../livekit-turn-cert-deploy-hook.sh`
  （turn 证书续期分发 + LiveKit 重启，安装到服务器
  `/etc/letsencrypt/renewal-hooks/deploy/`）。

- **`a.imboy.pub`**：遗留 301 → `s3.imboy.pub` 重定向域（HTTP only），按 2026-09-21
  拍板**不做快照**（服务器上有活跃 vhost，恢复新机时需手工重建）。

- **`sc.imboy.pub`、`admdev.imboy.pub`**：DNS A 记录指向本机，但服务器上**没有
  vhost、没有证书**（请求落入 `0.default.conf` 兜底：https 404）。属未部署域；
  未来上线时把 conf 加入本目录。
- `i.imboy.pub`：已下线（服务器仅剩 `.bak` 文件，无活跃 vhost）。
- `0.default.conf`：默认兜底 server（80，宝塔欢迎页），基础设施文件，不入本目录。

## 回填快照 / Re-snapshot

```bash
ssh -p 32 root@<host> 'cat /www/server/panel/vhost/nginx/<domain>.conf' \
  > deploy/nginx/prod-vhosts/<domain>.conf
```

> ⚠️ 服务器端文件可能缺行尾换行；回填后可在服务器上 `nginx -t` 校验。
